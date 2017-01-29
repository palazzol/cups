{Unit of routines for 3-D potentials.                                }
{Used by Scattr3D.pas.                                               }
{                                                                    }
{Author: John R. Hiller                                              }
{Version: 1.0             7/24/94                                    }
{Copyright (c) 1995 John Wiley & Sons, Inc.                          }
{                                                                    }
{How to add a potential:                                             }
{     The addition of a potential requires a new procedure to define }
{text screens and default values for parameters, and the addition of }
{entries in four procedures, SetPotlValues, SetPotlUnits,            }
{SetPotlScales, and ValidatePotl.  The first computes the potential's}
{values, the second sets units for parameters, the third checks      }
{parameter values and sets values for scales, and the fourth checks  }
{input from a file for a valid potential.  Templates that can be used}
{are already included as the potential 'UserDefined.'  These entries }
{can be modified to include the desired form and other specifications}
{of a potential.  Menu access is also already included. The number of}
{parameters in the potential is limited by the constant MaxNumParams.}
{a new procedure to set units for parameters, }
{                                                                    }
{   The default algorithm code number, DefAlg, should not be set to  }
{zero except for piecewise constant potentials.  If it is set to    }
{zero, the parameters must fit the following pattern:                }
{Params[2*n-1]=r_n and Params[2*n]=V_n, where r_n is the location of }
{the nth discontinuity and V_n is the scaled value of the potential  }
{to the right of r_n.  The point r_0=r_min and the last point, r_max,}
{are understood and not included in the array Params.  The value to  }
{the right of r_min is V_0, to the left is infinity.                 }
{                                                                    }
UNIT Sc3DPotl;

INTERFACE

USES Graph,CUPS,CUPSmupp,CUPSfunc,CUPSproc,CUPSgui,CUPSgrph,
     Sc3DMach,Sc3DCmn;

CONST
   alfa=0.007297351;         {fine structure constant}
   MeVtonm=0.19732893e-3;    {conversion factor; assumes hbar=c=1}
   MaxNumParams=5;
   uInit=1.0e-7;             {starting value for wave function}

TYPE
   ParamNameType=array [1..MaxNumParams] OF string[12];
   ParamsType=array [1..MaxNumParams] OF real;
   PotlType=record
      IDcode       :string;
      Saved,
      Zoom         :boolean;
      V,                        {values of the potential}
      u,uFree,A    :DVector;    {temporary storage during solution
                                 of Schrodinger equation}
      DefAlg,Alg,               {code number for algorithm, and default}
      Invert,                   {sign for inversion of potential}
      NumParams    :integer;
      DefParams,                {default parameters}
      Params       :ParamsType;
      ParamNames,
      ParamUnits   :ParamNameType;
      ParamList    :array [1..12] OF string[25];
      V0,DefV0,                 {energy scale and default}
      L0,DefL0,                 {length scale and default}
      Mass,                     {particle reduced mass, m*c^2}
      zeta,                     {dimensionless scale parameter}
      Efac,Lfac,Mfac,
      HorizMin,HorizMax,        {horizontal range of plot}
      VertMin,VertMax,          {vertical range of plot}
      DXSscale,TXSscale,        {default max for cross-section plot}
      DefrMin,
      DefrMax,
      rMin,                     {outer radius of hard core}
      rMax,                     {edge of potential}
      rStop,                    {range of wave function computation}
      r1,r2,                    {match points for phase shift calculation}
      dr,                       {Numerov step size}
      u1           :real;       {initial wave function value for Numerov}
      Eunit,                    {energy unit prefix}
      Lunit,                    {length unit prefix}
      Munit        :char;       {mass unit prefix}
      Particle     :ParticleType;
      nRSteps,            {number of Numerov steps from r_max to r_stop}
      nSteps,             {number of Numerov steps from r_min to r_max}
      DefnSteps,
      NumLines     :integer;     {number of lines in InfoScr}
      InfoScr,                   {text output for description of potential}
      PlotInfoScr  :InfoScrType  {text output for potential parameters}
      END; {PotlType}

VAR
   Potl          :PotlType;
   MachEpsY12    :real;  {12th root of MachEps}

{--------Routines for definition, modification, and display of potentials--}

PROCEDURE SetPotlScales(IDcode :string; VAR V0,L0 :real; VAR Invert :integer;
                        rMin,rMax,Mass,Efac,Lfac,Mfac :real;
                        VAR VertMin,VertMax :real;
                        VAR Params,DefParams :ParamsType;
                        VAR ParamNames :ParamNameType);
PROCEDURE ValidatePotl(VAR IDcode :string; VAR ValidPotl :boolean);
PROCEDURE InitializeScales(VAR Potl :PotlType);
PROCEDURE PrepPotl(var Potl :PotlType);
PROCEDURE SetPotlToPowerLaw(VAR Potl :PotlType);
PROCEDURE SetPotlToYukawa(VAR Potl :PotlType);
PROCEDURE SetPotlToGaussian(VAR Potl :PotlType);
PROCEDURE SetPotlToLennardJones(VAR Potl :PotlType);
PROCEDURE SetPotlToWoodsSaxon(VAR Potl :PotlType);
PROCEDURE SetPotlToSphere(VAR Potl :PotlType);
PROCEDURE SetPotlToHardSphere(VAR Potl :PotlType);
PROCEDURE SetPotlToUserDefined(VAR Potl :PotlType);
PROCEDURE DrawPotential(VAR Potl :PotlType; hMin,hMax,vMin,vMax,scale :real);
PROCEDURE PlotPotential(VAR Potl :PotlType);
PROCEDURE DisplayPotential(VAR Potl :PotlType; VAR MainMenu :TMenu;
                           VAR MenuClicked,Modified :boolean);

IMPLEMENTATION

{--------Routines for definition, modification, and display of potentials--}

PROCEDURE SetPotlValues(VAR Potl :PotlType);
VAR
   i       :integer;
   r,Vtemp :real;
BEGIN
   WITH Potl DO
      BEGIN
         IF IDcode='PowerLaw' THEN
            FOR i:=0 TO nSteps+nRSteps DO
               BEGIN
                  r:=i*dr+rMin;
                  IF r<MachEps THEN r:=MachEps;
                  IF r<rMax THEN Vtemp:=Invert*Pwr(r,-Params[3])
                  ELSE Vtemp:=0.0;
                  V.Put(i+1,Vtemp);
               END
         ELSE IF IDcode='Yukawa' THEN
            FOR i:=0 TO nSteps+nRSteps DO
               BEGIN
                  r:=i*dr+rMin;
                  IF r<MachEps THEN r:=MachEps;
                  IF r<rMax THEN Vtemp:=Invert*ExpSafe(-r)/r
                  ELSE Vtemp:=0.0;
                  V.Put(i+1,Vtemp);
               END
         else if IDcode='Gaussian' then
            FOR i:=0 TO nSteps+nRSteps DO
               BEGIN
                  r:=i*dr+rMin;
                  IF r<rMax THEN Vtemp:=Invert*ExpSafe(-sqr(r))
                  ELSE Vtemp:=0.0;
                  V.Put(i+1,Vtemp);
               END
         ELSE IF IDcode='LennardJones' THEN
            FOR i:=0 TO nSteps+nRSteps DO
               BEGIN
                  r:=i*dr+rMin;
                  IF r<MachEpsY12 THEN r:=MachEpsY12;
                  IF r<rMax THEN Vtemp:=Invert*(Pwr(r,-12)-2*Pwr(r,-6))
                  ELSE Vtemp:=0.0;
                  V.Put(i+1,Vtemp);
               END
         ELSE IF IDcode='WoodsSaxon' THEN
            FOR i:=0 TO nSteps+nRSteps DO
               BEGIN
                  r:=i*dr+rMin;
                  IF r<rMax THEN
                     Vtemp:=Invert/(1+ExpSafe((r-1.0)/(Params[3]/L0)))
                  ELSE Vtemp:=0.0;
                  V.Put(i+1,Vtemp);
               END
         ELSE IF IDcode='Sphere' THEN
            FOR i:=0 TO nSteps+nRSteps DO
               BEGIN
                  r:=i*dr+rMin;
                  IF r<rMax THEN Vtemp:=Invert
                  ELSE Vtemp:=0.0;
                  V.Put(i+1,Vtemp);
               END
         ELSE IF IDcode='HardSphere' THEN
            FOR i:=0 TO nSteps+nRSteps DO
               BEGIN
                  r:=i*dr+rMin;
                  V.Put(i+1,0.0);
               END
         ELSE IF IDcode='UserDefined' THEN
            FOR i:=0 TO nSteps+nRSteps DO
               BEGIN
                  r:=i*dr+rMin;
                  IF r<rMax THEN Vtemp:=Invert{*expression}
                  ELSE Vtemp:=0.0;
                  V.Put(i+1,Vtemp);
               END;
      END;
END; {SetPotlValues}

PROCEDURE SetPotlUnits(IDcode :string; Eunit,Lunit,Munit :char;
                       VAR ParamUnits :ParamNameType);
BEGIN
   IF IDcode='PowerLaw' THEN
      BEGIN
         ParamUnits[1]:=ConCat(Eunit,'eV');
         ParamUnits[2]:=ConCat(Lunit,'m');
         ParamUnits[3]:='';
      END
   ELSE IF IDcode='Yukawa' THEN
      BEGIN
         ParamUnits[1]:=ConCat(Eunit,'eV');
         ParamUnits[2]:=ConCat(Lunit,'m');
      END
   ELSE IF IDcode='Gaussian' THEN
      BEGIN
         ParamUnits[1]:=ConCat(Eunit,'eV');
         ParamUnits[2]:=ConCat(Lunit,'m');
      END
   ELSE IF IDcode='LennardJones' THEN
      BEGIN
         ParamUnits[1]:=ConCat(Eunit,'eV');
         ParamUnits[2]:=ConCat(Lunit,'m');
      END
   ELSE IF IDcode='WoodsSaxon' THEN
      BEGIN
         ParamUnits[1]:=ConCat(Eunit,'eV');
         ParamUnits[2]:=ConCat(Lunit,'m');
         ParamUnits[3]:=ConCat(Lunit,'m');
      END
   ELSE IF IDcode='Sphere' THEN
      BEGIN
         ParamUnits[1]:=ConCat(Eunit,'eV');
      END
   ELSE IF IDcode='HardSphere' THEN
      BEGIN
         {No parameters.}
      END
   ELSE IF IDcode='UserDefined' THEN
      BEGIN
         ParamUnits[1]:=ConCat(Eunit,'eV');
         ParamUnits[2]:=ConCat(Lunit,'m');
         ParamUnits[3]:='';
         ParamUnits[4]:='';
      END;
END; {SetPotlUnits}

PROCEDURE SetV0(VAR V0 :real; VAR Invert :integer;
                Mass,Efac,Mfac :real; VAR VertMin,VertMax :real;
                pNum :integer; VAR Params,DefParams :ParamsType;
                VAR ParamNames :ParamNameType);
VAR
   V0Old :real;
BEGIN
   V0Old:=V0;
   IF Params[pNum]=0.0 THEN
      RealInputWarn(ParamNames[pNum],Params[pNum],DefParams[pNum],8);
   V0:=Params[pNum];
   IF V0<0.0 THEN
      BEGIN
         Invert:=-1; V0:=-V0
      END
   ELSE Invert:=1;
   VertMin:=VertMin*V0/V0old; VertMax:=VertMax*V0/V0old;
END; {SetV0}

PROCEDURE SetPotlScales(IDcode :string; VAR V0,L0 :real; VAR Invert :integer;
                        rMin,rMax,Mass,Efac,Lfac,Mfac :real;
                        VAR VertMin,VertMax :real;
                        VAR Params,DefParams :ParamsType;
                        VAR ParamNames :ParamNameType);
VAR
   V0Old :real;
BEGIN
   IF IDcode='PowerLaw' THEN
      BEGIN
         SetV0(V0,Invert,Mass,Efac,Mfac,VertMin,VertMax,
               1,Params,DefParams,ParamNames);
         IF Params[2]<=0.0 THEN
            RealInputWarn(ParamNames[2],Params[2],DefParams[2],8);
         L0:=Params[2];
      END
   ELSE IF IDcode='Yukawa' THEN
      BEGIN
         SetV0(V0,Invert,Mass,Efac,Mfac,VertMin,VertMax,
               1,Params,DefParams,ParamNames);
         IF Params[2]<=0.0 THEN
            RealInputWarn(ParamNames[2],Params[2],DefParams[2],8);
         L0:=Params[2];
      END
   ELSE IF IDcode='Gaussian' THEN
      BEGIN
         SetV0(V0,Invert,Mass,Efac,Mfac,VertMin,VertMax,
               1,Params,DefParams,ParamNames);
         IF Params[2]<=0.0 THEN
            RealInputWarn(ParamNames[2],Params[2],DefParams[2],8);
         L0:=Params[2];
      END
   ELSE IF IDcode='LennardJones' THEN
      BEGIN
         SetV0(V0,Invert,Mass,Efac,Mfac,VertMin,VertMax,
               1,Params,DefParams,ParamNames);
         IF Params[2]<=0.0 THEN
            RealInputWarn(ParamNames[2],Params[2],DefParams[2],8);
         L0:=Params[2];
      END
   ELSE IF IDcode='WoodsSaxon' THEN
      BEGIN
         SetV0(V0,Invert,Mass,Efac,Mfac,VertMin,VertMax,
               1,Params,DefParams,ParamNames);
         IF Params[2]<=0.0 THEN
            RealInputWarn(ParamNames[2],Params[2],DefParams[2],8);
         L0:=Params[2];
      END
   ELSE IF IDcode='Sphere' THEN
      BEGIN
         SetV0(V0,Invert,Mass,Efac,Mfac,VertMin,VertMax,
               1,Params,DefParams,ParamNames);
         L0:=1.0;
      END
   ELSE IF IDcode='HardSphere' THEN
      BEGIN
         V0Old:=V0;
         V0:=1.0;
         VertMin:=VertMin*V0/V0old; VertMax:=VertMax*V0/V0old;
         L0:=1.0;
      END
   ELSE IF IDcode='UserDefined' THEN
      BEGIN
         SetV0(V0,Invert,Mass,Efac,Mfac,VertMin,VertMax,
               1,Params,DefParams,ParamNames);
         IF Params[2]<=0.0 THEN
            RealInputWarn(ParamNames[2],Params[2],DefParams[2],8);
         L0:=Params[2];
      END;
END; {SetPotlScales}

PROCEDURE ValidatePotl(VAR IDcode :string; VAR ValidPotl :boolean);
BEGIN
   IF IDcode='PowerLaw' THEN ValidPotl:=True
   ELSE IF IDcode='Yukawa' THEN ValidPotl:=True
   ELSE IF IDcode='Gaussian' THEN ValidPotl:=True
   ELSE IF IDcode='LennardJones' THEN ValidPotl:=True
   ELSE IF IDcode='WoodsSaxon' THEN ValidPotl:=True
   ELSE IF IDcode='Sphere' THEN ValidPotl:=True
   ELSE IF IDcode='HardSphere' THEN ValidPotl:=True
   ELSE IF IDcode='UserDefined' THEN ValidPotl:=True
   ELSE ValidPotl:=False;
END; {ValidatePotl}

PROCEDURE PrepVParamList(VAR Potl :PotlType);
{Prepare parameter list for input and Info screens.}
VAR
   n :integer;
BEGIN
   WITH Potl DO
      BEGIN
         ParamList[1]:=' Potential parameters';
         FOR n:=1 TO NumParams DO
            ParamList[n+1]:=ConCat(' ',ParamNames[n],Num2Str(Params[n],6),
                                   ' ',ParamUnits[n]);
         ParamList[NumParams+2]:=ConCat('  r_min: ',Num2Str(L0*rMin,6),
                                        ' ',Lunit,'m');
         ParamList[NumParams+3]:=ConCat('  r_max: ',Num2Str(L0*rMax,6),
                                        ' ',Lunit,'m');
         ParamList[NumParams+4]:=ConCat('  delta r: ',Num2Str(L0*dr,6),
                                        ' ',Lunit,'m');
         FOR n:=NumParams+5 TO 12 DO ParamList[n]:=''
      END
END; {PrepVParamList}

PROCEDURE PrepPotlPlotScr(VAR Potl :PotlType);
{Prepare Info screen for plot of potential.}
VAR
   n,i :integer;
BEGIN
   WITH Potl DO
      BEGIN
         FOR n:=1 TO NumParams+4 DO PlotInfoScr[n]:=ParamList[n];
         FOR n:=NumParams+5 TO 25 DO PlotInfoScr[n]:=''
      END
END; {PrepPotlPlotScr}

Procedure PrepPotl(var Potl :PotlType);
BEGIN
   SetPotlValues(Potl);
   PrepVParamList(Potl);
   PrepPotlPlotScr(Potl);
END; {PrepPotl}

PROCEDURE InitializeScales(VAR Potl :PotlType);
BEGIN
   WITH Potl DO
      BEGIN
         SetPotlUnits(IDcode,Eunit,Lunit,Munit,ParamUnits);
         CASE Eunit OF
            ' ' :Efac:=1.0;
            'k' :Efac:=1.0e+3;
            'M' :Efac:=1.0e+6;
            'G' :Efac:=1.0e+9;
            END; {case}
         CASE Lunit OF
            'n' :Lfac:=1.0;
            'p' :Lfac:=1.0e-3;
            'f' :Lfac:=1.0e-6;
            END; {case}
         CASE Munit OF
            ' ' :Mfac:=1.0-6;
            'k' :Mfac:=1.0e-3;
            'M' :Mfac:=1.0;
            'G' :Mfac:=1.0e+3;
            END; {case}
      END;
END; {InitializeScales}

PROCEDURE SetPotlToDefaults(VAR Potl :PotlType);
{Set potential parameters to default values.}
VAR
   n :integer;
BEGIN
   WITH Potl DO
      BEGIN
         Saved:=True; Zoom:=False;
         FOR n:=1 TO NumParams DO Params[n]:=DefParams[n];
         rMin:=DefrMin; rMax:=DefrMax;
         InitializeScales(Potl);
         V0:=DefV0;
         VertMin:=-1.5*V0; VertMax:=1.5*V0;
         SetPotlScales(IDcode,V0,L0,Invert,rMin,rMax,Mass,Efac,Lfac,Mfac,
                       VertMin,VertMax,Params,DefParams,ParamNames);
         ComputeZeta(V0,L0,Mass,Eunit,Lunit,Munit,zeta);
         dr:=(rMax-rMin)/nSteps;
         rStop:=rMax+dr*nRSteps;
         u1:=-uInit*sqr(dr)/6.0;
         HorizMin:=0.0; HorizMax:=L0*rStop;
         IF (DefAlg<>0) AND (Alg=0) THEN Alg:=DefAlg;
      END;
END; {SetPotlToDefaults}

PROCEDURE PotlLegend;
BEGIN
   OpenViewPort(1);
   ClearViewPort;
   SelectScale(1);
   LegendEntry(3,LightCyan,'potential');
   SetColor(Whitte)
END; {PotlLegend}

PROCEDURE SetPotlToPowerLaw(VAR Potl :PotlType);
{Prepare I/O screen InfoScr for power-law potential and set parameters.}
VAR
   n :integer;
BEGIN
   WITH Potl DO
      BEGIN
         IDcode:='PowerLaw';
         NumParams:=3;
         ParamNames[1]:=' V_0: ';
         ParamNames[2]:=' L_0: ';
         ParamNames[3]:='  mu: ';
         Invert:=1;
         NumLines:=7;
         InfoScr[1]:='      Power-law potential     ';
         InfoScr[2]:='                              ';
         InfoScr[3]:=' V(r)=infinity,  0 < r < r_min';
         InfoScr[4]:=' V(r)=V_0*(r/L_0)^{-mu},      ';
         InfoScr[5]:='             r_min < r < r_max';
         InfoScr[6]:=' V(r)=0,  r > r_max           ';
         InfoScr[7]:='                              ';
         FOR n:=NumLines+1 TO 25 DO InfoScr[n]:='';
         DXSscale:=0.05; TXSscale:=0.03;
         DefrMin:=0.0; DefrMax:=10.0;
         DefV0:=1.0; DefL0:=1.0;
         DefParams[1]:=DefV0; DefParams[2]:=DefL0;
         DefParams[3]:=2.0;
         DefAlg:=2
      END;
   SetPotlToDefaults(Potl);
   PrepPotl(Potl)
END; {SetPotlToPowerLaw}

PROCEDURE SetPotlToYukawa(VAR Potl :PotlType);
{Prepare I/O screen InfoScr for Yukawa potential and set parameters.}
VAR
   n :integer;
BEGIN
   WITH Potl DO
      BEGIN
         IDcode:='Yukawa';
         NumParams:=2;
         ParamNames[1]:=' V_0: ';
         ParamNames[2]:=' L_0: ';
         Invert:=1;
         NumLines:=7;
         InfoScr[1]:='       Yukawa potential       ';
         InfoScr[2]:='                              ';
         InfoScr[3]:=' V(r)=infinity, 0 < r < r_min ';
         InfoScr[4]:=' V(r)=V_0*exp(-r/L_0)/(r/L_0),';
         InfoScr[5]:='             r_min < r < r_max';
         InfoScr[6]:=' V(r)=0, r > r_max            ';
         InfoScr[7]:='                              ';
         FOR n:=NumLines+1 TO 25 DO InfoScr[n]:='';
         DXSscale:=0.0015; TXSscale:=0.002;
         DefrMin:=0.0; DefrMax:=3.0;
         DefV0:=-100.0; DefL0:=1.2;
         DefParams[1]:=DefV0; DefParams[2]:=DefL0;
         DefAlg:=2
      END;
   SetPotlToDefaults(Potl);
   PrepPotl(Potl)
END; {SetPotlToYukawa}

PROCEDURE SetPotlToGaussian(VAR Potl :PotlType);
{Prepare I/O screen InfoScr for Gaussian potential and set parameters.}
VAR
   n :integer;
BEGIN
   WITH Potl DO
      BEGIN
         IDcode:='Gaussian';
         NumParams:=2;
         ParamNames[1]:=' V_0: ';
         ParamNames[2]:=' L_0: ';
         Invert:=1;
         NumLines:=7;
         InfoScr[1]:='      Gaussian potential      ';
         InfoScr[2]:='                              ';
         InfoScr[3]:=' V(r)=infinity, 0 < r < r_min ';
         InfoScr[4]:=' V(r)=V_0*exp[-(r/L_0)^2],    ';
         InfoScr[5]:='             r_min < r < r_max';
         InfoScr[6]:=' V(r)=0, r > r_max            ';
         InfoScr[7]:='                              ';
         FOR n:=NumLines+1 TO 25 DO InfoScr[n]:='';
         DXSscale:=1.0; TXSscale:=0.3;
         DefrMin:=0.0; DefrMax:=2.0;
         DefV0:=1.0; DefL0:=1.0;
         DefParams[1]:=DefV0; DefParams[2]:=DefL0;
         DefAlg:=2                           
      END;
   SetPotlToDefaults(Potl);
   PrepPotl(Potl)
END; {SetPotlToGaussian}

PROCEDURE SetPotlToLennardJones(VAR Potl :PotlType);
{Prepare I/O screen InfoScr for Lennard-Jones potential}
{and set parameters.}
VAR
   n :integer;
BEGIN
   WITH Potl DO
      BEGIN
         IDcode:='LennardJones';
         NumParams:=2;
         ParamNames[1]:=' V_0: ';
         ParamNames[2]:=' L_0: ';
         Invert:=1;
         NumLines:=7;
         InfoScr[1]:='    Lennard-Jones potential   ';
         InfoScr[2]:='                              ';
         InfoScr[3]:=' V(r)=infinity, 0 < r < r_min ';
         InfoScr[4]:=' V(r)=V_0*[(L_0/r)^12         ';
         InfoScr[5]:='             -2(L_0/r)^6],    ';
         InfoScr[6]:='             r_min < r < r_max';
         InfoScr[7]:=' V(r)=0, r > r_max            ';
         FOR n:=NumLines+1 TO 25 DO InfoScr[n]:='';
         DXSscale:=0.2; TXSscale:=0.2;
         DefrMin:=0.0; DefrMax:=3.0;
         DefV0:=1.0; DefL0:=1.0;
         DefParams[1]:=DefV0; DefParams[2]:=DefL0;
         DefAlg:=2
      END;
   SetPotlToDefaults(Potl);
   PrepPotl(Potl)
END; {SetPotlToLennardJones}

PROCEDURE SetPotlToWoodsSaxon(VAR Potl :PotlType);
{Prepare I/O screen InfoScr for Woods-Saxon potential}
{and set parameters.}
VAR
   n :integer;
BEGIN
   WITH Potl DO
      BEGIN
         IDcode:='WoodsSaxon';
         NumParams:=3;
         ParamNames[1]:=' V_0: ';
         ParamNames[2]:=' L_0: ';
         ParamNames[3]:='   a: ';
         Invert:=1;
         NumLines:=7;
         InfoScr[1]:='    Woods-Saxon potential     ';
         InfoScr[2]:='                              ';
         InfoScr[3]:=' V(r)=infinity, 0 < r < r_min ';
         InfoScr[4]:=' V(r)=V_0*/[1+exp((r-L_0)/a)],';
         InfoScr[5]:='             r_min < r < r_max';
         InfoScr[6]:=' V(r)=0, r > r_max            ';
         InfoScr[7]:='                              ';
         FOR n:=NumLines+1 TO 25 DO InfoScr[n]:='';
         DXSscale:=0.001; TXSscale:=0.002;
         DefrMin:=0.0; DefrMax:=5.0;
         DefV0:=50.0; DefL0:=1.0;
         DefParams[1]:=DefV0; DefParams[2]:=DefL0;
         DefParams[3]:=0.3*DefL0;
         DefAlg:=2
      END;
   SetPotlToDefaults(Potl);
   PrepPotl(Potl)
END; {SetPotlToWoodsSaxon}

PROCEDURE SetPotlToSphere(VAR Potl :PotlType);
{Prepare I/O screen InfoScr for spherical potential and set parameters.}
VAR
   n :integer;
BEGIN
   WITH Potl DO
      BEGIN
         IDcode:='Sphere';
         NumParams:=1;
         ParamNames[1]:=' V_0: ';
         Invert:=1;
         NumLines:=5;
         InfoScr[1]:='      Spherical shells        ';
         InfoScr[2]:='                              ';
         InfoScr[3]:=' V(r)= infinity, 0 < r < r_min';
         InfoScr[4]:=' V(r)= V_0,  r_min < r < r_max';
         InfoScr[5]:=' V(r)= 0,    r > r_max        ';
         FOR n:=NumLines+1 TO 25 DO InfoScr[n]:='';
         DXSscale:=0.3; TXSscale:=0.7;
         DefrMin:=0.0; DefrMax:=1.0;
         DefV0:=3.0; DefL0:=1.0;
         DefParams[1]:=DefV0;
         DefAlg:=0
      END;
   SetPotlToDefaults(Potl);
   PrepPotl(Potl)
END; {SetPotlToSphere}

PROCEDURE SetPotlToHardSphere(VAR Potl :PotlType);
{Prepare I/O screen InfoScr for hard sphere potential}
{and set parameters.}
VAR
   n :integer;
BEGIN
   WITH Potl DO
      BEGIN
         IDcode:='HardSphere';
         NumParams:=0;
         Invert:=1;
         NumLines:=4;
         InfoScr[1]:='          Hard sphere         ';
         InfoScr[2]:='                              ';
         InfoScr[3]:=' V(r)= infinity, 0 < r < r_min';
         InfoScr[4]:=' V(r)= 0,  r > r_min          ';
         FOR n:=NumLines+1 TO 25 DO InfoScr[n]:='';
         DXSscale:=1.0; TXSscale:=0.2;
         DefrMin:=1.0; DefrMax:=2.0;
         DefV0:=1.0; DefL0:=1.0;
         DefAlg:=0
      END;
   SetPotlToDefaults(Potl);
   PrepPotl(Potl)
END; {SetPotlToHardSphere}

PROCEDURE SetPotlToUserDefined(VAR Potl :PotlType);
{Prepare I/O screen InfoScr for user-defined potential}
{and set parameters.}
VAR
   n :integer;
BEGIN
   Announce('Modify code to insert your potential.');
{   WITH Potl DO                                               }
{     BEGIN                                                    }
{        IDcode:='UserDefined';                                }
{        NumParams:=4;                                         }
{        ParamNames[1]:=' V_0: ';                              }
{        ParamNames[2]:=' L_0: ';                              }
{        ParamNames[3]:='   c: ';                              }
{        ParamNames[4]:='   a: ';                              }
{        Invert:=1;                                            }
{        NumLines:=6;                                          }
{        InfoScr[1]:='    User-defined potential    ';         }
{        InfoScr[2]:='                              ';         }
{        InfoScr[3]:=' V(r)=infinity, 0 < r < r_min ';         }
{        InfoScr[4]:=' V(r)=V_0,  r_min < r < r_max ';         }
{        InfoScr[5]:=' V(r)=0,    r > r_max         ';         }
{        InfoScr[6]:='                              ';         }
{        FOR n:=NumLines+1 TO 25 DO InfoScr[n]:='';            }
{        DXSscale:=1.0; TXSscale:=1.0;                         }
{        DefrMin:=0.0; DefrMax:=5.0;                           }
{        DefV0:=1.0; DefL0:=1.0;                               }
{        DefParams[1]:=DefV0; DefParams[2]:=DefL0;             }
{        DefParams[3]:=0.75; DefParams[4]:=1.0;                }
{        DefAlg:=2   Use 0 if piecewise constant.              }
{      END;                                                    }
{  SetPotlToDefaults(Potl);                                    }
{  PrepPotl(Potl)                                              }
END; {SetPotlToUserDefined}

PROCEDURE DrawPotential(VAR Potl :PotlType; hMin,hMax,vMin,vMax,scale :real);
VAR
   n,NumSteps,nMid     :integer;
   x1,x2,y1,y2,rMid,vR :real;
BEGIN
   vR:=vMax-vMin;
   WITH Potl DO
      BEGIN
         SetColor(LightCyan);
         IF DefAlg=0 THEN
            BEGIN
               NumSteps:=1+NumParams DIV 2;
               x2:=rMin;
               IF rMin>0.0 THEN y2:=vMax ELSE y2:=scale*V.Value(1);
               IF NumSteps=1 THEN rMid:=(rMax+rMin)/2
               ELSE rMid:=(Params[1]+rMin)/2;
               nMid:=round((rMid-rMin)/dr);
               FOR n:=1 TO NumSteps DO
                  BEGIN
                     x1:=x2; y1:=y2;
                     IF n=1 THEN y2:=scale*V.Value(nMid+1)
                     ELSE y2:=Invert*scale*Params[2*n-2];
                     IF n=NumSteps THEN x2:=rMax
                     ELSE x2:=Params[2*n-1];
                     IF y2<vMin-vR THEN y2:=vMin-vR
                     ELSE IF y2>vMax+vR THEN y2:=vMax+vR;
                     PlotLine(L0*x1,y1,L0*x1,y2);
                     PlotLine(L0*x1,y2,L0*x2,y2)
                  END;
               x2:=rMax;
            END
         ELSE
            BEGIN
               x2:=rMin; y2:=scale*V.Value(1);
               IF y2<0.0 THEN y1:=vMin ELSE y1:=vMax;
               IF y2<vMin-vR THEN y2:=vMin-vR
               ELSE IF y2>vMax+vR THEN y2:=vMax+vR;
               IF (rMin>0.0) AND (abs(y2)<abs(y1)) THEN
                  PlotLine(L0*x2,y1,L0*x2,y2);
               FOR n:=1 TO nSteps DO
                  BEGIN
                     x1:=x2; y1:=y2;
                     x2:=x2+dr; y2:=scale*V.Value(n+1);
                     IF y2<vMin-vR THEN y2:=vMin-vR
                     ELSE IF y2>vMax+vR THEN y2:=vMax+vR;
                     PlotLine(L0*x1,y1,L0*x2,y2);
                  END
            END;
         PlotLine(L0*x2,y2,L0*rMax,0.0);
         IF L0*rMax<hMax THEN PlotLine(L0*rMax,0.0,hMax,0.0)
      END
END; {DrawPotential}

PROCEDURE PlotPotential(VAR Potl :PotlType);
VAR
   vp :integer;
BEGIN
   WITH Potl DO
      BEGIN
         IF Zoom THEN vp:=3
         ELSE
            BEGIN
               vp:=2;
               ClearOversizedViewPort(3);
               Info(InfoScr,NumLines,4);
               Info(PlotInfoScr,25,5);
               PotlLegend
            END;
         PrepViewPort(vp);
         SpecialAxis(2,HorizMin,HorizMax,5,VertMin,VertMax,10);
         PutLabel(Top,'Potential');
         PutLabel(Left,ConCat('V(x) (',Eunit,'eV)'));
         PutLabel(Bottom,ConCat('r (',Lunit,'m)'));
         DrawPotential(Potl,HorizMin,HorizMax,VertMin,VertMax,V0);
         SetColor(Whitte);
      END;
END; {PlotPotential}

PROCEDURE ModifyPotential(VAR Potl :PotlType; VAR Modified :boolean);
VAR
   n        :integer;
   rMaxOld  :real;
   InputScr :TInputScreen;
BEGIN
   {New(InputScr); {Mac only}
   WITH Potl,InputScr DO
      BEGIN
         Init;
         DefineInputPort(0.63,0.93,0.0,0.92);
         LoadLine(' Potential parameters');
         FOR n:=1 TO NumParams DO
            LoadLine(ConCat(' ',ParamNames[n],' {      }',ParamUnits[n]));
         LoadLine(ConCat('  r_min: {      }',Lunit,'m'));
         LoadLine(ConCat('  r_max: {      }',Lunit,'m'));
         LoadLine('');
         LoadLine(' Restore defaults? #F    ');
         LoadLine('');
         LoadLine(' [  OK  ]    [Cancel]');
         FOR n:=1 TO NumParams DO SetNumber(n,Params[n]);
         SetNumber(NumParams+1,L0*rMin);
         SetNumber(NumParams+2,L0*rMax);
         AcceptScreen;
         IF NOT Canceled THEN
            BEGIN
               IF GetBoolean(NumParams+3) THEN SetPotlToDefaults(Potl)
               ELSE
                  BEGIN
                     rMaxOld:=rMax;
                     FOR n:=1 TO NumParams DO Params[n]:=GetNumber(n);
                     rMin:=GetNumber(NumParams+1);
                     IF rMin<0.0 THEN
                        RealInputWarn('r_min',rMin,0.0,6);
                     rMax:=GetNumber(NumParams+2);
                     IF rMax<=rMin THEN
                        RealInputWarn('r_max',rMax,rMin+DefrMax-DefrMin,6);
                     SetPotlScales(IDcode,V0,L0,Invert,rMin,rMax,
                                   Mass,Efac,Lfac,Mfac,
                                   VertMin,VertMax,
                                   Params,DefParams,ParamNames);
                     ComputeZeta(V0,L0,Mass,Eunit,Lunit,Munit,zeta);
                     rMin:=rMin/L0; rMax:=rMax/L0;
                     dr:=(rMax-rMin)/nSteps; u1:=-uInit*sqr(dr)/6.0;
                     rStop:=rStop*rMax/rMaxOld;
                     nRSteps:=round((rStop-rMax)/dr);
                     A.Free; uFree.Free; u.Free; V.Free;  {resize storage}
                     V.Init(nSteps+nRSteps+1);
                     u.Init(nSteps+nRSteps+1);
                     uFree.Init(nSteps+nRSteps+1);
                     A.Init(nSteps+nRSteps+1);
                     rStop:=rMax+dr*nRSteps;
                     IF HorizMax>L0*rStop THEN HorizMax:=L0*rStop;
                     Saved:=False;
                  END;
               PrepPotl(Potl);
               Modified:=True;
               PlotPotential(Potl)
            END;
         Done;
      END;
   {Dispose(InputScr); {Mac only}
END; {ModifyPotential}

PROCEDURE ChangePotlScales(VAR Potl :PotlType);
VAR
   Changed,AutoSet :boolean;
BEGIN
   WITH Potl DO
      BEGIN
         AutoSet:=False;
         ChangePlotScales(HorizMin,HorizMax,ConCat(Lunit,'m '),0.0,rStop,
                          VertMin,VertMax,ConCat(Eunit,'eV'),-1.0,3.0*V0,
                          Changed,AutoSet);
         IF Changed THEN PlotPotential(Potl);
      END
END; {ChangePotlScales}

PROCEDURE DisplayPotential(VAR Potl :PotlType; VAR MainMenu :TMenu;
                           VAR MenuClicked,Modified :boolean);
{Display potential and allow modifications.}
VAR
   DPHelpScr    :HelpScrType;
   DPkeys       :THotKeys;
   Dkey         :byte;
   n            :integer;
   WishToReturn :boolean;
BEGIN
   DPHelpScr[01]:='';
   DPHelpScr[02]:=' <F1>-show this help screen.';
   DPHelpScr[03]:='';
   DPHelpScr[04]:=' <F2>-modify potential. An input screen will';
   DPHelpScr[05]:='    appear to allow changes to parameters.';
   DPHelpScr[06]:='';
   DPHelpScr[07]:=' <F3>-toggle size of plot.  When small, text';
   DPHelpScr[08]:='    information and a legend are shown.';
   DPHelpScr[09]:='';
   DPHelpScr[10]:=' <F4>-change horizontal and vertical scales';
   DPHelpScr[11]:='    of plot.';
   DPHelpScr[12]:='';
   DPHelpScr[13]:=' <F10>-accept potential and activate menu.';
   DPHelpScr[14]:='     The parameterization of the potential';
   DPHelpScr[15]:='     is then set.';
   for n:=16 to 25 do DPHelpScr[n]:='';

   WITH Potl DO
      BEGIN
         PlotPotential(Potl);
         ModifyPotential(Potl,Modified);
         {New(DPkeys); {Mac only}
         WITH DPkeys DO
            BEGIN
               Init(5);
               key[1]:='F1-Help';
               key[2]:='F2-Input';
               IF Zoom THEN key[3]:='F3-Contract'
               ELSE key[3]:='F3-Expand';
               key[4]:='F4-Scale';
               key[5]:='F10-Menu';
               Display
            END;
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
                        2 :ModifyPotential(Potl,Modified);
                        3 :BEGIN
                              Zoom:=not Zoom;
                              PlotPotential(Potl);
                              IF Zoom THEN DPkeys.key[3]:='F3-Contract'
                              ELSE DPkeys.key[3]:='F3-Expand'
                           END;
                        4 :ChangePotlScales(Potl);
                        5 :BEGIN
                              WishToReturn:=True;
                              IF MainMenu.Chosen THEN
                                 MenuClicked:=True;
                           END;
                        END; {case}
                     DPkeys.Display;
                  END
               ELSE IF MainMenu.Activated THEN MenuClicked:=True;
            END;
         DPkeys.Clear;
         {Dispose(DPkeys); {Mac only}
      END;
END; {DisplayPotential}

END. {Sc3DPotl}
