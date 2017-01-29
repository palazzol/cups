{Unit of routines for 1-D potentials.                                 }
{Used by Scattr1D.pas.                                                }
{                                                                     }
{Author: John R. Hiller                                               }
{Version: 1.0               11/20/94                                  }
{Copyright (c) 1995 John Wiley & Sons, Inc.                           }
{                                                                     }
{How to add a potential:                                              }
{     The addition of a potential requires a new procedure to define  }
{text screens and default values for parameters and the addition of   }
{entries in two procedures, ValidatePotl and SetPotlValues, and in one}
{function, Vfunc.  The first procedure verifies that a given ID code  }
{is valid; the second computes Vinfty, the value of the potential at  }
{x_max and beyond, and fills the DVector V with equally spaced values }
{in the interior.  These interior values are computed by the function }
{Vfunc, which can also be called directly when a value at a particular}
{x is needed.  Templates that can be used to add a new potential are  }
{already included as the procedure SetPotlToUserDefined and as the    }
{entries in ValidatePotl, SetPotlValues and Vfunc with the ID code of }
{UserDefined.  Menu access is also already included.  The number of   }
{parameters in the potential is limited by the constant MaxNumParams. }
{This does not include the scale V_0 for the magnitude which sets the }
{scale for all energies that appear.                                  }
{   The default algorithm code number, DefAlg, should not be set to   }
{zero except for piecewise constant potentials.  If it is set to zero,}
{the parameters in the array Params must fit the following pattern:   }
{Params[2*n-1]=x_n and Params[2*n]=V_n/V_0, where x_n is the location }
{of the nth discontinuity and V_n is the value of the potential to the}
{right of x_n.  The point x_0=x_min and the last point, x_max, are not}
{included in the array Params.  The value of the potential immediately}
{to the right of x_min is V_0.                                        }
{                                                                     }
UNIT Sc1DPotl;

INTERFACE

USES Graph,CUPS,CUPSmupp,CUPSfunc,CUPSproc,CUPSgui,
     Sc1DMach,Sc1DCmn;

CONST
   MaxNumParams=5;

TYPE
   PotlType=record
      IDcode        :string;
      V             :DVector;
      Saved,
      Zoom          :boolean;
      Invert,                    {sign for inversion of potential}
      NumParams     :integer;
      DefParams,                 {default parameters}
      Params        :array [1..MaxNumParams] OF real;
      ParamNames,
      ParamUnits    :array [1..MaxNumParams] OF string[12];
      ParamList     :array [1..12] OF string[25]; {parameter list}
      Vinfty,                    {value of potential beyond xMax}
      Esmall,Elarge,             {WKB estimates of allowed dynamic range}
      V0,DefV0,                  {energy scale and default}
      L0,                        {length scale}
      Mass,                      {mass*c^2}
      zeta          :real;       {dimensionless scale parameter}
      Eunit,                     {energy scale unit prefix}
      Lunit,                     {length scale unit prefix}
      Munit         :char;       {mass unit prefix}
      DefHorizMin,DefHorizMax,
      HorizMin,HorizMax,         {horizontal range for plot}
      VertMin,VertMax,           {vertical range for plot}
      DefxMin,DefxMax,
      xMin,xMax,                 {range where potential may vary}
      dx            :real;
      Alg,DefAlg,                {code numbers for algorithm chosen & default}
      nRSteps,                   {number of steps beyond x_max}
      nLSteps,                   {number of steps before x_min}
      nSteps,
      DefnSteps,
      NumLines      :integer;    {number of lines in InfoScr}
      InfoScr,                   {text output for potential}
      PlotInfoScr   :InfoScrType
      END; {PotlType}

VAR
   Potl :PotlType;
   cPi  :real;                   {global constant set to Turbo Pi function}

PROCEDURE ValidatePotl(VAR IDcode :string; VAR ValidPotl :boolean);
FUNCTION Vfunc(x :real; VAR Potl :PotlType) :real;
PROCEDURE PrepPotl(VAR Potl :PotlType; VAR Failed :boolean);
PROCEDURE SetPotlToSquare(VAR Potl :PotlType);
PROCEDURE SetPotlToStep(VAR Potl :PotlType);
PROCEDURE SetPotlToDbleSquare(VAR Potl :PotlType);
PROCEDURE SetPotlToRamp(VAR Potl :PotlType);
PROCEDURE SetPotlToSmoothStep(VAR Potl :PotlType);
PROCEDURE SetPotlToGaussian(VAR Potl :PotlType);
PROCEDURE SetPotlToWoodsSaxon(VAR Potl :PotlType);
PROCEDURE SetPotlToUserDefined(VAR Potl :PotlType);
PROCEDURE DrawPotential(VAR Potl :PotlType; hMin,hMax,vMin,vMax,scale :real);
PROCEDURE PlotPotential(VAR Potl :PotlType);
PROCEDURE DisplayPotential(VAR Potl :PotlType; VAR MainMenu :TMenu;
                           VAR MenuClicked,WFcomputed,TRcomputed :boolean);

IMPLEMENTATION

PROCEDURE ValidatePotl(VAR IDcode :string; VAR ValidPotl :boolean);
BEGIN
   IF IDcode='Square' THEN ValidPotl:=True
   ELSE IF IDcode='DbleSquare' THEN ValidPotl:=True
   ELSE IF IDcode='Step' THEN ValidPotl:=True
   ELSE IF IDcode='Ramp' THEN ValidPotl:=True
   ELSE IF IDcode='SmoothStep' THEN ValidPotl:=True
   ELSE IF IDcode='Gaussian' THEN ValidPotl:=True
   ELSE IF IDcode='WoodsSaxon' THEN ValidPotl:=True
   ELSE IF Potl.IDcode='UserDefined' THEN ValidPotl:=True
   ELSE ValidPotl:=False;
END; {ValidatePotl}

FUNCTION Vfunc(x :real; VAR Potl :PotlType) :real;
BEGIN
   WITH Potl DO
      IF x<=xMin THEN Vfunc:=0.0
      ELSE IF x>=xMax THEN Vfunc:=Vinfty
      ELSE
         BEGIN
            IF IDcode='Square' THEN Vfunc:=Invert
            ELSE IF IDcode='DbleSquare' THEN
               BEGIN
                  IF x<Params[1]/L0 THEN Vfunc:=Invert
                  ELSE IF x<Params[3]/L0 THEN Vfunc:=Invert*Params[2]
                  ELSE Vfunc:=Invert*Params[4];
               END
            ELSE IF IDcode='Step' THEN Vfunc:=Invert*Params[1]
            ELSE IF IDcode='Ramp' THEN Vfunc:=Invert*(x-xMin)/(xMax-xMin)
            ELSE IF IDcode='SmoothStep' THEN
               Vfunc:=Invert/(1+ExpSafe(-L0*x/Params[1]))
            ELSE IF IDcode='Gaussian' THEN
               Vfunc:=Invert*ExpSafe(-Params[1]*sqr(L0*x))
            ELSE IF IDcode='WoodsSaxon' THEN
               Vfunc:=Invert/(1+ExpSafe((abs(L0*x)-Params[1])/Params[2]))
            ELSE IF Potl.IDcode='UserDefined' THEN
               Vfunc:=Invert{*expression}
         END;
END; {Vfunc}

PROCEDURE SetPotlValues(VAR Potl :PotlType; VAR Failed :boolean);
CONST
   DynLim=300.0;
VAR
   i,n,imin,imax :integer;
   x,Vtemp,Vmax,
   E1,E2,F1  :real;

   FUNCTION Fsmall(E :real) :real;
   VAR
      Ftemp :real;
      nn    :integer;
   BEGIN
      Ftemp:=0.0;
      FOR nn:=0 TO Potl.nSteps+1 DO
         IF E<Potl.V.Value(nn+1) THEN
            Ftemp:=Ftemp+sqrt(Potl.V.Value(nn+1)-E);
      Fsmall:=sqrt(Potl.zeta)*Potl.dx*Ftemp-DynLim;
   END; {Fsmall}

   FUNCTION Flarge(E :real) :real;
   VAR
      Ftemp :real;
      nn    :integer;
   BEGIN
      Ftemp:=0.0;
      FOR nn:=1 TO Potl.nSteps DO
         IF E>Potl.V.Value(nn+1) THEN
            Ftemp:=Ftemp+sqrt(E-Potl.V.Value(nn+1));
      Flarge:=sqrt(Potl.zeta)*Potl.dx*Ftemp/Potl.nSteps-2*cPi/3;
   END; {Flarge}

BEGIN {SetPotlValues}
   WITH Potl DO
      BEGIN
         IF IDcode='Square' THEN Vinfty:=0.0
         ELSE IF IDcode='DbleSquare' THEN Vinfty:=0.0
         ELSE IF IDcode='Step' THEN Vinfty:=Invert
         ELSE IF IDcode='Ramp' THEN Vinfty:=Invert
         ELSE IF IDcode='SmoothStep' THEN
            Vinfty:=Invert/(1+ExpSafe(-L0*xMax/Params[1]))
         ELSE IF IDcode='Gaussian' THEN Vinfty:=0.0
         ELSE IF IDcode='WoodsSaxon' THEN Vinfty:=0.0
         ELSE IF Potl.IDcode='UserDefined' THEN
            Vinfty:=Invert;  {or other value}
         V.Free; V.Init(nSteps+2);
         V.Put(1,0.0);
         FOR i:=1 TO nSteps-1 DO
            BEGIN
               x:=i*dx+xMin;
               V.Put(i+1,Vfunc(x,Potl));
            END;
         V.Put(nSteps+1,Vinfty);
         V.Put(nSteps+2,Vinfty);

         {Estimate allowed dynamic range.}
         V.MinMax(imin,Vtemp,imax,Vmax);
         E1:=0.0; E2:=Vmax; Esmall:=0.0;             {Estimate lower limit.}
         IF Fsmall(E1)>0.0 THEN
            BEGIN
               FOR n:=1 TO 10 DO
                  BEGIN
                     Esmall:=(E1+E2)/2;
                     IF Fsmall(Esmall)>0.0 THEN E1:=Esmall ELSE E2:=Esmall;
                  END;
               Esmall:=E2;
            END;
         IF Esmall<1.0e-7 THEN Esmall:=1.0e-7;
         Esmall:=V0*Esmall;
         IF Esmall>=0.1 THEN
            BEGIN
               IF Esmall<1.0 THEN Esmall:=(trunc(1000*Esmall)+1)/1000.0
               ELSE IF Esmall<10.0 THEN Esmall:=(trunc(100*Esmall)+1)/100.0
               ELSE IF Esmall<100.0 THEN Esmall:=(trunc(10*Esmall)+1)/10.0
               ELSE IF Esmall<1.0e6 THEN Esmall:=trunc(Esmall)+1
            END;

         IF Flarge(Vmax)<0.0 THEN                    {Estimate upper limit.}
            BEGIN
               E1:=Vmax; E2:=Vmax+2*sqr(2*cPi/(3*dx))/zeta; F1:=-1.0;
            END
         ELSE
            BEGIN
               E1:=0.0; E2:=Vmax; F1:=Flarge(E1)
            END;
         IF F1<0.0 THEN
            FOR n:=1 TO 11 DO
               BEGIN
                  Elarge:=(E1+E2)/2;
                  IF Flarge(Elarge)<0.0 THEN E1:=Elarge ELSE E2:=Elarge;
               END;
         Elarge:=V0*E1;
         IF Elarge>=0.1 THEN
            BEGIN
               IF Elarge<1.0 THEN Elarge:=trunc(1000*Elarge)/1000.0
               ELSE IF Elarge<10.0 THEN Elarge:=trunc(100*Elarge)/100.0
               ELSE IF Elarge<100.0 THEN Elarge:=trunc(10*Elarge)/10.0
               ELSE IF Elarge<1.0e6 THEN Elarge:=trunc(Elarge)
            END;
         IF Esmall>Elarge THEN Failed:=True;
      END;
END; {SetPotlValues}

PROCEDURE SetPotlToDefaults(VAR Potl :PotlType);
{Set potential parameters to default values.}
VAR
   n :integer;
BEGIN
   WITH Potl DO
      BEGIN
         Saved:=True; Zoom:=False;
         FOR n:=1 TO NumParams DO Params[n]:=DefParams[n];
         xMin:=DefxMin; xMax:=DefxMax;
         V0:=DefV0; L0:=1.0;
         Mass:=0.511; Munit:='M';         {electron mass m*c^2 in Mev}
         dx:=(xMax-xMin)/nSteps;
         nRSteps:=round((DefHorizMax-xMax)/dx);
         HorizMax:=(xMax+dx*nRSteps)*L0;
         nLSteps:=round((xMin-DefHorizMin)/dx);
         HorizMin:=(xMin-dx*nLSteps)*L0;
         VertMin:=-1.5*V0; VertMax:=1.5*V0;
         IF (DefAlg<>0) AND (Alg=0) THEN Alg:=DefAlg;
      END;
END; {SetPotlToDefaults}

PROCEDURE PrepVParamList(VAR Potl :PotlType);
{Prepare parameter list for input and Info screens.}
VAR
   n :integer;
BEGIN
   WITH Potl DO
      BEGIN
         ParamList[1]:=' Potential parameters';
         ParamList[2]:=ConCat('     V_0: ',Num2Str(Invert*V0,6)
                       ,' ',Eunit,'eV');
         FOR n:=1 TO NumParams DO
            ParamList[n+2]:=ConCat(ParamNames[n],
                                   Num2Str(Params[n],6),' ',
                                   ParamUnits[n]);
         ParamList[NumParams+3]:=ConCat('   x_min: ',Num2Str(L0*xMin,6),
                                 ' ',Lunit,'m');
         ParamList[NumParams+4]:=ConCat('   x_max: ',Num2Str(L0*xMax,6),
                                 ' ',Lunit,'m');
         ParamList[NumParams+5]:=ConCat(' delta x: ',Num2Str(L0*dx,6),
                                 ' ',Lunit,'m');
         FOR n:=NumParams+6 TO 12 DO ParamList[n]:=''
      END
END; {PrepVParamList}

PROCEDURE PrepPotlPlotScr(VAR Potl :PotlType);
{Prepare Info screen for plot of potential.}
VAR
   n,i :integer;
BEGIN
   WITH Potl DO
      BEGIN
         FOR n:=1 TO NumParams+5 DO PlotInfoScr[n]:=ParamList[n];
         FOR n:=NumParams+6 TO 25 DO PlotInfoScr[n]:=''
      END
END; {PrepPotlPlotScr}

PROCEDURE PrepPotl(VAR Potl :PotlType; VAR Failed :boolean);
BEGIN
   WITH Potl DO ComputeZeta(V0,L0,Mass,Eunit,Lunit,Munit,zeta);
   SetPotlValues(Potl,Failed);
   PrepVParamList(Potl);
   PrepPotlPlotScr(Potl);
END; {PrepPotl}

PROCEDURE PotlLegend;
{Display legend for plot of potential.}
BEGIN
   OpenViewPort(1);
   ClearViewPort;
   SelectScale(1);
   LegendEntry(3,LightCyan,'potential');
   LegendEntry(4,LightBlue,'x_min, x_max');
   SetColor(Whitte)
END; {PotlLegend}

PROCEDURE SetPotlToSquare(VAR Potl :PotlType);
{Prepare I/O screen InfoScr for square potential and set parameters.}
VAR
   n :integer;
   TempBool :boolean;
BEGIN
   WITH Potl DO
      BEGIN
         IDcode:='Square';
         NumParams:=0; Invert:=1;
         NumLines:=5;
         InfoScr[1]:='     Square barrier           ';
         InfoScr[2]:='                              ';
         InfoScr[3]:=' V(x)=0, x < x_min            ';
         InfoScr[4]:=' V(x)=V_0, x_min < x < x_max  ';
         InfoScr[5]:=' V(x)=0, x > x_max            ';
         FOR n:=NumLines+1 TO 25 DO InfoScr[n]:='';
         DefV0:=2.0;
         DefxMin:=-1.4; DefxMax:=1.4;
         DefHorizMin:=-2.8; DefHorizMax:=2.8;
         DefnSteps:=100;
         DefAlg:=0
      END;
   SetPotlToDefaults(Potl);
   PrepPotl(Potl,TempBool)
END; {SetPotlToSquare}

PROCEDURE SetPotlToDbleSquare(VAR Potl :PotlType);
{Prepare I/O screen InfoScr for square potential and set parameters.}
VAR
   n :integer;
   TempBool :boolean;
BEGIN
   WITH Potl DO
      BEGIN
         IDcode:='DbleSquare';
         NumParams:=4;
         ParamNames[1]:='     x_1: '; ParamUnits[1]:=ConCat(Lunit,'m');
         ParamNames[2]:=' V_1/V_0: '; ParamUnits[2]:='  ';
         ParamNames[3]:='     x_2: '; ParamUnits[3]:=ConCat(Lunit,'m');
         ParamNames[4]:=' V_2/V_0: '; ParamUnits[4]:='  ';
         Invert:=1;
         NumLines:=6;
         InfoScr[1]:='    Double-square barrier     ';
         InfoScr[2]:=' V(x)=0,   x < x_min          ';
         InfoScr[3]:=' V(x)=V_0, x_min < x < x_1    ';
         InfoScr[4]:=' V(x)=V_1, x_1 < x < x_2      ';
         InfoScr[5]:=' V(x)=V_2, x_2 < x < x_max    ';
         InfoScr[6]:=' V(x)=0,   x > x_max          ';
         FOR n:=NumLines+1 TO 25 DO InfoScr[n]:='';
         DefV0:=2.0; 
         DefParams[1]:=-1.35; DefParams[2]:=0.0;
         DefParams[3]:=1.35; DefParams[4]:=1.0;
         DefxMin:=-1.4; DefxMax:=1.4;
         DefHorizMin:=-2.8; DefHorizMax:=2.8;
         DefnSteps:=200;
         DefAlg:=0
      END;
   SetPotlToDefaults(Potl);
   PrepPotl(Potl,TempBool)
END; {SetPotlToDbleSquare}

PROCEDURE SetPotlToStep(VAR Potl :PotlType);
{Prepare I/O screen InfoScr for step potential and set parameters.}
VAR
   n :integer;
   TempBool :boolean;
BEGIN
   WITH Potl DO
      BEGIN
         IDcode:='Step';
         NumParams:=1;
         ParamNames[1]:=' V_1/V_0: '; ParamUnits[1]:='';
         Invert:=1;
         NumLines:=5;
         InfoScr[1]:='     Step potential           ';
         InfoScr[2]:='                              ';
         InfoScr[3]:=' V(x)=0,   x < x_min          ';
         InfoScr[4]:=' V(x)=V_1, x_min < x < x_max  ';
         InfoScr[5]:=' V(x)=V_0, x > x_max          ';
         FOR n:=NumLines+1 TO 25 DO InfoScr[n]:='';
         DefV0:=1.0; 
         DefxMin:=0.0; DefxMax:=1.0;
         DefHorizMin:=-2.0; DefHorizMax:=2.0;
         DefParams[1]:=1.0;
         DefnSteps:=100;
         DefAlg:=0
      END;
   SetPotlToDefaults(Potl);
   PrepPotl(Potl,TempBool)
END; {SetPotlToStep}

PROCEDURE SetPotlToRamp(VAR Potl :PotlType);
{Prepare I/O screen InfoScr for ramp potential and set parameters.}
VAR
   n :integer;
   TempBool :boolean;
BEGIN
   WITH Potl DO
      BEGIN
         IDcode:='Ramp';
         NumParams:=0;
         Invert:=1;
         NumLines:=7;
         InfoScr[1]:='       Ramp potential         ';
         InfoScr[2]:='                              ';
         InfoScr[3]:=' V(x)=0,    x < x_min         ';
         InfoScr[4]:=' V(x)=V_0*(x-x_min)           ';
         InfoScr[5]:='          /(x_max-x_min),     ';
         InfoScr[6]:='            x_min < x < x_max ';
         InfoScr[7]:=' V(x)=V_0,  x > x_max         ';
         FOR n:=NumLines+1 TO 25 DO InfoScr[n]:='';
         DefV0:=1.0; 
         DefxMin:=-1.0; DefxMax:=3.0;
         DefHorizMin:=-5.0; DefHorizMax:=5.0;
         DefnSteps:=100;
         DefAlg:=2
      END;
   SetPotlToDefaults(Potl);
   PrepPotl(Potl,TempBool)
END; {SetPotlToRamp}

PROCEDURE SetPotlToSmoothStep(VAR Potl :PotlType);
{Prepare I/O screen InfoScr for Smooth-step potential}
{and set parameters.}
VAR
   n :integer;
   TempBool :boolean;
BEGIN
   WITH Potl DO
      BEGIN
         IDcode:='SmoothStep';
         NumParams:=1;
         ParamNames[1]:='       a: '; ParamUnits[1]:=ConCat(Lunit,'m');
         Invert:=1;
         NumLines:=7;
         InfoScr[1]:='   Smooth-step potential      ';
         InfoScr[2]:='                              ';
         InfoScr[3]:=' V(x)=0,   x < x_min          ';
         InfoScr[4]:=' V(x)=V_0/[1+exp(-x/a)],      ';
         InfoScr[5]:='           x_min < x < x_max  ';
         InfoScr[6]:=' V(x)=V_0/[1+exp(-x_max/a)],  ';
         InfoScr[7]:='           x > x_max          ';
         FOR n:=NumLines+1 TO 25 DO InfoScr[n]:='';
         DefV0:=1.0; 
         DefParams[1]:=0.1;
         DefxMin:=-1.0; DefxMax:=1.0;
         DefHorizMin:=-2.0; DefHorizMax:=2.0;
         DefnSteps:=100;
         DefAlg:=2
      END;
   SetPotlToDefaults(Potl);
   PrepPotl(Potl,TempBool)
END; {SetPotlToSmoothStep}

PROCEDURE SetPotlToGaussian(VAR Potl :PotlType);
{Prepare I/O screen InfoScr for Gaussian potential and set parameters.}
VAR
   n :integer;
   TempBool :boolean;
BEGIN
   WITH Potl DO
      BEGIN
         IDcode:='Gaussian';
         NumParams:=1;
         ParamNames[1]:='     mu: '; ParamUnits[1]:=ConCat(Lunit,'m^-2');
         Invert:=1;
         NumLines:=6;
         InfoScr[1]:='   Gaussian potential         ';
         InfoScr[2]:='                              ';
         InfoScr[3]:=' V(x)=0,    x < x_min         ';
         InfoScr[4]:=' V(x)=V_0*exp(-mu*x^2),       ';
         InfoScr[5]:='            x_min < x < x_max ';
         InfoScr[6]:=' V(x)=0,    x > x_max         ';
         FOR n:=NumLines+1 TO 25 DO InfoScr[n]:='';
         DefV0:=1.0; 
         DefParams[1]:=10.0;
         DefxMin:=-1.0; DefxMax:=1.0;
         DefHorizMin:=-2.0; DefHorizMax:=2.0;
         DefnSteps:=100;
         DefAlg:=2
      END;
   SetPotlToDefaults(Potl);
   PrepPotl(Potl,TempBool)
END; {SetPotlToGaussian}

PROCEDURE SetPotlToWoodsSaxon(VAR Potl :PotlType);
{Prepare I/O screen InfoScr for Woods-Saxon potential}
{and set parameters.}
VAR
   n :integer;
   TempBool :boolean;
BEGIN
   WITH Potl DO
      BEGIN
         IDcode:='WoodsSaxon';
         NumParams:=2;
         ParamNames[1]:='       c: '; ParamUnits[1]:=ConCat(Lunit,'m');
         ParamNames[2]:='       a: '; ParamUnits[2]:=ConCat(Lunit,'m');
         Invert:=1;
         NumLines:=6;
         InfoScr[1]:='   Woods-Saxon potential      ';
         InfoScr[2]:='                              ';
         InfoScr[3]:=' V(x)=0,   x < x_min          ';
         InfoScr[4]:=' V(x)=V_0/[1+exp((|x|-c)/a)], ';
         InfoScr[5]:='           x_min < x < x_max  ';
         InfoScr[6]:=' V(x)=0,   x > x_max          ';
         FOR n:=NumLines+1 TO 25 DO InfoScr[n]:='';
         DefV0:=1.0; 
         DefParams[1]:=0.5; DefParams[2]:=0.05;
         DefxMin:=-1.0; DefxMax:=1.0;
         DefHorizMin:=-2.0; DefHorizMax:=2.0;
         DefnSteps:=100;
         DefAlg:=2
      END;
   SetPotlToDefaults(Potl);
   PrepPotl(Potl,TempBool)
END; {SetPotlToWoodsSaxon}

PROCEDURE SetPotlToUserDefined(VAR Potl :PotlType);
{Prepare I/O screen InfoScr for user-defined potential}
{and set parameters.}
VAR
   n :integer;
   TempBool :boolean;
BEGIN
   Announce('Modify code to insert your potential.');
{   with Potl do                                                }
{     BEGIN                                                     }
{        IDcode:='UserDefined';                                 }
{        NumParams:=2;                                          }
{        ParamNames[1]:='       a: '; ParamUnits[1]:='';            }
{        ParamNames[2]:='       b: '; ParamUnits[2]:='';            }
{        Invert:=1;                                             }
{        NumLines:=5;                                           }
{        InfoScr[1]:='   User-defined potential     ';          }
{        InfoScr[2]:='                              ';          }
{        InfoScr[3]:=' V(x)=0,   x < x_min          ';          }
{        InfoScr[4]:=' V(x)=V_0, x_min < x < x_max  ';          }
{        InfoScr[5]:=' V(x)=0,   x > x_max          ';          }
{        FOR n:=NumLines+1 TO 25 DO InfoScr[n]:='';             }
{        DefV0:=1.0;                                            }
{        DefParams[1]:=1.0; DefParams[2]:=1.0;                  }
{        DefxMin:=-1.0; DefxMax:=1.0;                           }
{        DefHorizMin:=-2.0; DefHorizMax:=2.0;                   }
{        DefnSteps:=100;                                        }
{        DefAlg:=2   Use 0 if piecewise constant.               }
{      end;                                                     }
{  SetPotlToDefaults(Potl);                                     }
{  PrepPotl(Potl,TempBool)                                      }
END; {SetPotlToUserDefined}

PROCEDURE DrawPotential(VAR Potl :PotlType; hMin,hMax,vMin,vMax,scale :real);
VAR
   n,NumSteps :integer;
   x1,x2,y1,y2 :real;
BEGIN
   WITH Potl DO
      BEGIN
         SetColor(LightCyan);
         IF hMin/L0<xMin THEN PlotLine(hMin,0.0,L0*xMin,0.0);
         IF DefAlg=0 THEN
            BEGIN
               NumSteps:=1+NumParams DIV 2; x2:=xMin; y2:=0.0;
               FOR n:=1 TO NumSteps DO
                  BEGIN
                     x1:=x2; y1:=y2;
                     IF n=1 THEN
                        BEGIN
                           IF IDcode='Step' THEN y2:=Invert*scale*Params[1]
                           ELSE y2:=Invert*scale
                        END
                     ELSE IF (n=NumSteps) AND (IDcode='Step') THEN
                        y2:=Invert*scale
                     ELSE
                        BEGIN
                           IF (IDcode='Step') THEN
                              y2:=Invert*scale*Params[2*n]
                           ELSE y2:=Invert*scale*Params[2*n-2];
                        END;
                     IF n=NumSteps THEN x2:=xMax
                     ELSE x2:=Params[2*n-1];
                     IF y2<vMin-(vMax-vMin) THEN y2:=vMin-(vMax-vMin)
                     ELSE IF y2>vMax+(vMax-vMin) THEN y2:=vMax+(vMax-vMin);
                     PlotLine(L0*x1,y1,L0*x1,y2);
                     PlotLine(L0*x1,y2,L0*x2,y2)
                  END
            END
         ELSE
            BEGIN
               x2:=xMin; y2:=0.0;
               FOR n:=1 TO nSteps DO
                  BEGIN
                     x1:=x2; y1:=y2; x2:=x2+dx; y2:=scale*V.Value(n+1);
                     PlotLine(L0*x1,y1,L0*x2,y2)
                  END
            END;
         PlotLine(L0*x2,y2,L0*xMax,scale*Vinfty);
         IF xMax<hMax/L0 THEN
            PlotLine(L0*xMax,scale*Vinfty,hMax,scale*Vinfty)
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
               vp:=2; ClearOversizedViewPort(3);
               Info(InfoScr,NumLines,4);
               Info(PlotInfoScr,25,5);
               PotlLegend
            END;
         PrepViewPort(vp);
         SpecialAxis(2,HorizMin,HorizMax,5,VertMin,VertMax,10);
         PutLabel(Top,'Potential');
         PutLabel(Left,ConCat('V(x) (',Eunit,'eV)'));
         PutLabel(Bottom,ConCat('x (',Lunit,'m)'));
         SetColor(LightBlue);
         PlotLine(L0*xMin,VertMin,L0*xMin,VertMax);
         PlotLine(L0*xMax,VertMin,L0*xMax,VertMax);
         DrawPotential(Potl,HorizMin,HorizMax,VertMin,VertMax,V0);
         SetColor(Whitte);
      END;
END; {PlotPotential}

PROCEDURE ModifyPotential(VAR Potl :PotlType;
                          VAR WFcomputed,TRcomputed :boolean);
VAR
   n,InvertOld,
   nRStepsOld,nLStepsOld   :integer;
   HorizMaxOld,HorizMinOld,
   V0old,xMinOld,xMaxOld   :real;
   Failed,TempBool         :boolean;
   ParamsOld :array [1..MaxNumParams] OF real;
   InputScr  :TInputScreen;
BEGIN
   {New(InputScr); {Mac only}
   WITH Potl,InputScr DO
      REPEAT
         Failed:=False;
         Init;
         DefineInputPort(0.63,0.93,0.1,0.9);
         LoadLine(       'Potential parameters');
         LoadLine(ConCat('     V_0: {      }',Eunit,'eV'));
         FOR n:=1 TO NumParams DO
            LoadLine(ConCat(ParamNames[n],'{      }',ParamUnits[n]));
         LoadLine('');
         LoadLine(ConCat('   x_min: {      }',Lunit,'m'));
         LoadLine(ConCat('   x_max: {      }',Lunit,'m'));
         LoadLine('');
         LoadLine(' Restore defaults? #F    ');
         LoadLine('');
         LoadLine(' [  OK  ]    [Cancel]');
         SetNumber(1,Invert*V0);
         FOR n:=1 TO NumParams DO SetNumber(n+1,Params[n]);
         SetNumber(NumParams+2,xMin);
         SetNumber(NumParams+3,xMax);
         AcceptScreen;
         IF NOT Canceled THEN
            BEGIN
               IF GetBoolean(NumParams+4) THEN
                  BEGIN
                     SetPotlToDefaults(Potl);
                     Potl.Alg:=Potl.DefAlg;
                     Potl.nSteps:=Potl.DefnSteps;
                  END
               ELSE
                  BEGIN
                     V0old:=V0; InvertOld:=Invert;
                     FOR n:=1 TO NumParams DO ParamsOld[n]:=Params[n];
                     xMinOld:=xMin; xMaxOld:=xMax;
                     nRStepsOld:=nRSteps; nLStepsOld:=nLSteps;
                     HorizMaxOld:=HorizMax; HorizMinOld:=HorizMin;
                     V0:=GetNumber(1);
                     IF V0=0.0 THEN
                        RealInputWarn('V_0',V0,InvertOld*V0old,6);
                     IF V0>0.0 THEN Invert:=1
                     ELSE
                        BEGIN
                           V0:=-V0; Invert:=-1
                        END;
                     FOR n:=1 TO NumParams DO Params[n]:=GetNumber(n+1);
                     xMin:=GetNumber(NumParams+2);
                     xMax:=GetNumber(NumParams+3);
                     IF xMax<=xMin THEN
                        RealInputWarn('x_max',xMax,
                                      xMin+L0*(xMaxOld-xMinOld),6);
                     xMin:=xMin/L0; xMax:=xMax/L0;
                     dx:=(xMax-xMin)/nSteps;
                     nRSteps:=round((HorizMax/L0-xMax)/dx);
                     HorizMax:=(xMax+dx*nRSteps)*L0;
                     nLSteps:=round((xMin-HorizMin/L0)/dx);
                     HorizMin:=(xMin-dx*nLSteps)*L0;
                     VertMin:=VertMin*V0/V0old; VertMax:=VertMax*V0/V0old;
                  END;
               PrepPotl(Potl,Failed);
               IF Failed THEN
                  BEGIN
                     VertMin:=VertMin*V0old/V0; VertMax:=VertMax*V0old/V0;
                     V0:=V0old; Invert:=InvertOld;
                     FOR n:=1 TO NumParams DO Params[n]:=ParamsOld[n];
                     xMin:=xMinOld; xMax:=xMaxOld;
                     dx:=(xMax-xMin)/nSteps;
                     nRSteps:=nRStepsOld; nLSteps:=nLStepsOld;
                     HorizMax:=HorizMaxOld; HorizMin:=HorizMinOld;
                     PrepPotl(Potl,TempBool);
                     Announce(ConCat('Insufficient dynamic range for chosen',
                                     ' parameters.'));
                  END
               ELSE
                  BEGIN
                     WFcomputed:=False; TRcomputed:=False; Saved:=False;
                     PlotPotential(Potl);
                  END;
            END;
         Done;
      UNTIL NOT Failed;
   {Dispose(InputScr); {Mac only}
END; {ModifyPotential}

PROCEDURE ChangePotlScales(VAR Potl :PotlType);
VAR
   Changed,AutoSet :boolean;
BEGIN
   WITH Potl DO
      BEGIN
         AutoSet:=False;
         ChangePlotScales(HorizMin,HorizMax,ConCat('(',Lunit,'m)'),-1.0,L0,
                          VertMin,VertMax,ConCat('(',Eunit,'eV)'),-1.0,3.0*V0,
                          Changed,AutoSet);
         IF Changed THEN
            BEGIN
               if HorizMax/L0>xMax then
                  BEGIN
                     nRSteps:=round((HorizMax/L0-xMax)/dx);
                     HorizMax:=(xMax+dx*nRSteps)*L0
                  END
               ELSE nRSteps:=0;
               if HorizMin/L0<xMin then
                  BEGIN
                     nLSteps:=round((xMin-HorizMin/L0)/dx);
                     HorizMin:=(xMin-dx*nLSteps)*L0
                  END 
               ELSE nLSteps:=0;
               PlotPotential(Potl)
            END;
      END;
END; {ChangePotlScales}

PROCEDURE DisplayPotential(VAR Potl :PotlType; VAR MainMenu :TMenu;
                           VAR MenuClicked,WFcomputed,TRcomputed :boolean);
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
   DPHelpScr[11]:='    of the plot.';
   DPHelpScr[12]:='';
   DPHelpScr[13]:=' <F10>-accept potential and activate menu.';
   DPHelpScr[14]:='     The parameterization of the potential';
   DPHelpScr[15]:='     is then set.';
   FOR n:=16 TO 25 DO DPHelpScr[n]:='';

   WITH Potl DO
      BEGIN
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
         PlotPotential(Potl);
         ModifyPotential(Potl,WFcomputed,TRcomputed);
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
                        2 :ModifyPotential(Potl,WFcomputed,TRcomputed);
                        3 :BEGIN
                              Zoom:=not Zoom;
                              PlotPotential(Potl);
                              IF Zoom THEN DPkeys.key[3]:='F3-Contract'
                              ELSE DPkeys.key[3]:='F3-Expand';
                           END;
                        4 :ChangePotlScales(Potl);
                        5 :BEGIN
                              WishToReturn:=True;
                              IF MainMenu.Chosen THEN MenuClicked:=True;
                           END;
                        END; {case}
                     DPkeys.Display
                  END
               ELSE IF MainMenu.Activated THEN MenuClicked:=True;
            END;
         DPkeys.Clear;
         {Dispose(DPkeys); {Mac only}
      END;
END; {DisplayPotential}

END. {Sc1DPotl}
