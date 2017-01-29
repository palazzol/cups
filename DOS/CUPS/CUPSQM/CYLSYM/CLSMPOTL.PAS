{Unit of routines for plots and cylindrically symmetric potentials.  }
{Used by CylSym.pas.                                                 }
{                                                                    }
{Author: John R. Hiller                                              }
{Version: 1.0             11/15/94                                   }
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
{                                                                    }
UNIT ClSmPotl;

INTERFACE

USES Graph,CUPS,CUPSmupp,CUPSfunc,CUPSproc,CUPSgui,CUPSgrph,
     ClSmMach,ClSmCmn;

CONST
   alfa=0.007297351;         {fine structure constant}
   eTeslaToMeV2=59.16e-12;   {conversion factor; assumes hbar=c=1}
   MeVtonm=0.19732893e-3;       {conversion factor; assumes hbar=c=1}
   MaxNumParams=5;
   MaxNumE=200;

TYPE
   ParamNameType=array [1..MaxNumParams] OF string[12];
   ParamsType=array [1..MaxNumParams] OF real;
   PotlType=record
      IDcode        :string;
      Vmat          :DMatrixExt;
      NumParams     :integer;
      DefParams,                 {default parameters}
      Params        :ParamsType;
      ParamNames,
      ParamUnits    :ParamNameType;
      ParamList     :array [1..25] OF string[25];
      V0,                        {energy scale}
      L0,                        {length scale}
      Shift,                     {shift in potential; used by ADI}
      Mass,                      {mass*c^2 for particle}
      zeta,                      {dimensionless scale parameter}
      Efac,Lfac,Mfac,
      DefrhoMax,DefzMax,
      DefVertMin,DefVertMax,
      SingVal       :real;       {averaged value used for singular points}
      Eunit,                     {energy unit prefix}
      Lunit,                     {length unit prefix}
      Munit         :char;       {prefix for unit of mass}
      Particle      :ParticleType;
      Plotted,
      Saved         :boolean;
      NumLines      :integer;
      InfoScr,
      PlotInfoScr  :InfoScrType
      END; {PotlType}
   GridType=record
      PlotType,
      NrhoSteps,
      DefNrhoSteps,
      NzSteps,
      DefNzSteps    :integer;
      rhoMax,zMax,
      drho,dz,
      VertMin,VertMax,
      Size,Distance,
      alpha,beta,gamma    :real;
      Zoom                :boolean;
      END; {GridType}

VAR
   Potl          :PotlType;
   Grid          :GridType;
   cPi           :real;  {store value of pi given by Turbo function Pi}

{--------Routines for surface & contour plots-------------------------}

PROCEDURE SetDefaultSurfaceView(VAR alpha,beta,gamma,Size,Distance :real);
PROCEDURE PlotSurface(VAR S :DMatrixExt; VAR Grid :GridType;
                      VAR PlotInfoScr,InfoScr :InfoScrType;
                      NumLines,zParity :integer;
                      Post,UseVertScale :boolean; VAR Plotted :boolean;
                      PlotLabel :string; Color :word);
PROCEDURE RotateSurface(VAR S :DMatrixExt; VAR Grid :GridType;
                        VAR PlotInfoScr,InfoScr :InfoScrType;
                        NumLines,zParity :integer;
                        UseVertScale :boolean; VAR Plotted :boolean;
                        PlotLabel :string; Color :word);
PROCEDURE PlotContour(VAR S :DMatrixExt; VAR Grid :GridType;
                      VAR PlotInfoScr,InfoScr :InfoScrType;
                      NumLines,zParity :integer;
                      Post :boolean; VAR Plotted :boolean;
                      PlotLabel :string);

{--------Routines for defining potentials and changing parameters------}

PROCEDURE SetPotlValues(VAR Potl :PotlType; VAR Grid :GridType);
PROCEDURE ValidatePotl(VAR IDcode :string; VAR ValidPotl :boolean);
PROCEDURE PrepPotl(VAR Potl :PotlType; VAR Grid :GridType);
PROCEDURE InitializeScales(VAR Potl :PotlType);
PROCEDURE SetPotlToDefaults(VAR Potl :PotlType; VAR Grid :GridType);
PROCEDURE SetPotlToZeeman(VAR Potl :PotlType; VAR Grid :GridType);
PROCEDURE SetPotlToH2Ion(VAR Potl :PotlType; VAR Grid :GridType);
PROCEDURE SetPotlToWoodsSaxon(VAR Potl :PotlType; VAR Grid :GridType);
PROCEDURE SetPotlToOscillator(VAR Potl :PotlType; VAR Grid :GridType);
PROCEDURE SetPotlToCylinder(VAR Potl :PotlType; VAR Grid :GridType);
PROCEDURE SetPotlToUserDefined(VAR Potl :PotlType; VAR Grid :GridType);
PROCEDURE ModifyPotential(VAR Potl :PotlType; VAR Grid :GridType);
PROCEDURE PlotPotential(VAR Potl :PotlType;
                        VAR Grid :GridType; PotlPlotType :integer);

IMPLEMENTATION

{--------Routines for surface & contour plots-------------------------}

PROCEDURE InterpDMat(VAR DMat :DMatrixExt; VAR PlotMat :DMatrix;
                     zParity :integer);
VAR
   i,j,ii,jj,MaxRow,MaxCol :integer;
   val,x,y :real;
BEGIN
   DMat.GetSize(MaxRow,MaxCol);
   FOR i:=1 TO 11 DO FOR j:=1 TO 11 DO
      BEGIN
         x:=i*(MaxCol-1)/12; y:=(j-0.5)*(MaxRow-1)/11;
         ii:=trunc(x); jj:=trunc(y+0.5);
         IF jj>0 THEN
            BEGIN
               val:=(x-ii)*(y+0.5-jj)*DMat.Value(jj+1,ii+2);
               val:=val+(ii+1-x)*(y+0.5-jj)*DMat.Value(jj+1,ii+1);
               val:=val+(x-ii)*(jj+0.5-y)*DMat.Value(jj,ii+2);
               val:=val+(ii+1-x)*(jj+0.5-y)*DMat.Value(jj,ii+1);
            END
         ELSE {jj=0}
            BEGIN
               val:=(x-ii)*(y+0.5)*DMat.Value(1,ii+2);
               val:=val+(ii+1-x)*(y+0.5)*DMat.Value(1,ii+1);
               val:=val+(x-ii)*(0.5-y)*DMat.Value(1,ii+2);
               val:=val+(ii+1-x)*(0.5-y)*DMat.Value(1,ii+1);
            END;
         PlotMat.Put(j+12,i+13,val);
      END;
   FOR i:=1 TO 11 DO
      BEGIN
         x:=i*(MaxCol-1)/12; ii:=trunc(x);
         PlotMat.Put(24,i+13,(x-ii)*DMat.Value(MaxRow,ii+2)
                             +(ii+1-x)*DMat.Value(MaxRow,ii+1))
      END;
   FOR j:=1 TO 11 DO
      BEGIN
         y:=(j-0.5)*(MaxRow-1)/11; jj:=trunc(y+0.5);
         IF jj>0 THEN
            BEGIN
               PlotMat.Put(j+12,13,(y+0.5-jj)*DMat.Value(jj+1,1)
                                   +(jj+0.5-y)*DMat.Value(jj,1));
               PlotMat.Put(j+12,25,(y+0.5-jj)*DMat.Value(jj+1,MaxCol)
                                   +(jj+0.5-y)*DMat.Value(jj,MaxCol))
            END
         ELSE
            BEGIN
               PlotMat.Put(j+12,13,(y+0.5)*DMat.Value(1,1)
                                   +(0.5-y)*DMat.Value(1,1));
               PlotMat.Put(j+12,25,(y+0.5)*DMat.Value(1,MaxCol)
                                    +(0.5-y)*DMat.Value(1,MaxCol))
            END
      END;
   PlotMat.Put(24,13,DMat.Value(MaxRow,1));
   PlotMat.Put(24,25,DMat.Value(MaxRow,MaxCol));
   for i:=13 to 25 do for j:=1 to 12 do
      PlotMat.Put(j,i,PlotMat.Value(25-j,i));
   FOR i:=1 TO 12 DO FOR j:=1 TO 24 DO
      PlotMat.Put(j,i,zParity*PlotMat.Value(j,26-i));
END; {InterpDMat}

PROCEDURE SetDefaultSurfaceView(VAR alpha,beta,gamma,Size,Distance :real);
BEGIN
   alpha:=330; beta:=20; gamma:=0;
   Size:=1.5; Distance:=3
END; {SetDefaultSurfaceView}

PROCEDURE PlotSurface(VAR S :DMatrixExt; VAR Grid :GridType;
                      VAR PlotInfoScr,InfoScr :InfoScrType;
                      NumLines,zParity :integer;
                      Post,UseVertScale :boolean; VAR Plotted :boolean;
                      PlotLabel :string; Color :word);
{Draw surface described by S(+/-j,i+1) for i>=0 }
{and by zParity*S(+/-j,|i|+1) for i<0. Use 24x25 grid}
{and linear interpolation.  Box side ratio set by xW and yW.}
VAR
   PlotMat   :DMatrix;
   Surface   :TGraph3D;
   vMin,vMax :real;
   vp        :integer;
   sp        :PointType;
BEGIN
   IF NOT Plotted THEN WITH Grid DO
      BEGIN
         IF UseVertScale THEN
            BEGIN
               vMin:=VertMin; vMax:=VertMax
            END
         ELSE
            BEGIN
               vMin:=-1.0; vMax:=1.0
            END;
         HideMouse;
         IF Zoom THEN vp:=3
         ELSE
            BEGIN
               vp:=2;
               IF Post THEN
                  BEGIN
                     ClearOversizedViewPort(3);
                     Info(Potl.InfoScr,Potl.NumLines,4);
                     PostEulerAngles(alpha,beta,gamma);
                  END;
               Info(PlotInfoScr,25,5);
            END;
         PrepViewPort(vp);
         {New(PlotMat); {Mac only}
         PlotMat.Init(24,25);
         InterpDMat(S,PlotMat,zParity);
         {New(Surface); {Mac only}
         Surface.Init;
         Surface.BoxDrawn:=True;
         Surface.Set3DScales(-zMax,zMax,-rhoMax,rhoMax,vMin,vMax);
         Surface.SetDistance(Size,Distance);
         Surface.SetViewAngles(alpha,beta,gamma);
         IF rhoMax>zMax THEN Surface.SetBoxRatios(zMax/rhoMax,1.0,0.6)
         ELSE {rhoMax<=zMax} Surface.SetBoxRatios(1.0,rhoMax/zMax,0.6);
         Surface.SurfaceColor:=Color;
         Surface.DrawSurface(PlotMat);
         SetColor(Whitte);
         Surface.Vector3D(-1.5*zMax,0.0,0.0,1.5*zMax,0.0,0.0);
         Surface.GetPoint(1.5*zMax,0.0,0.0,sp);
         OutTextXY(sp.x+3,sp.y,'z');
         PutLabel(Top,PlotLabel);
         PlotMat.Free;
         Event.KeyPressed:=False;
         Event.MouseClicked:=False;
         Plotted:=True;
         {Dispose(Surface); Dispose(PlotMat); {Mac only}
         ShowMouse;
      END;
END; {PlotSurface}

PROCEDURE RotateSurface(VAR S :DMatrixExt; VAR Grid :GridType;
                        VAR PlotInfoScr,InfoScr :InfoScrType;
                        NumLines,zParity :integer;
                        UseVertScale :boolean; VAR Plotted :boolean;
                        PlotLabel :string; Color :word);
{Rotate view of surface S and replot.}
VAR
   RVHelpScr :HelpScrType;
   RVkeys    :THotKeys;
   View      :TView3D;
   Rkey      :byte;
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
   RVHelpScr[18]:='    a block or click on arrows.  Use arrow keys';
   RVHelpScr[19]:='    to select and move block.';
   RVHelpScr[20]:='';
   RVHelpScr[21]:=' The changes made are immediately visible in the';
   RVHelpScr[22]:=' appearance of the small box in the lower left.';
   RVHelpScr[23]:='';
   RVHelpScr[24]:=' <Enter>-accept view and return';
   RVHelpScr[25]:='    to previous screen.';

   WITH Grid DO
      BEGIN
         {New(RVkeys); New(View); {Mac only}
         WITH RVkeys DO
            BEGIN
               Init(6);
               key[1]:='F1-Help';
               key[2]:='F2-Plot';
               key[3]:='F3-Size';
               key[4]:='F4-Distance';
               key[5]:='F5-Reset';
               key[6]:='RETURN-Accept';
               Display
            END;
         SaveSize:=Size; SaveDistance:=Distance;
         SaveAlpha:=alpha; SaveBeta:=beta; SaveGamma:=gamma;
         SaveZoom:=Zoom; Zoom:=False;
         Plotted:=False;
         ClearOversizedViewPort(3);
         PlotSurface(S,Grid,PlotInfoScr,InfoScr,NumLines,zParity,
                     False,UseVertScale,Plotted,PlotLabel,Color);
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
                           PlotSurface(S,Grid,PlotInfoScr,InfoScr,
                                       NumLines,zParity,False,
                                       UseVertScale,Plotted,
                                       PlotLabel,Color);
                        END;
                     3 :View.Resize(Size,Distance);
                     4 :View.Persp(Size,Distance);
                     5 :BEGIN
                           View.Done;
                           SetDefaultSurfaceView(alpha,beta,gamma,
                                                 Size,Distance);
                           View.Init;
                           View.Draw(alpha,beta,gamma,Size,Distance);
                           Plotted:=False
                        END;
                     6 :WishToAccept:=True
                     END; {case}
               IF View.Changed THEN
                  BEGIN
                     View.Rotate(alpha,beta,gamma);
                     Plotted:=False
                  END;
            END;
         RVkeys.Clear;
         View.Done;
         Zoom:=SaveZoom; IF Zoom THEN Plotted:=False;
         IF NOT Plotted THEN
            PlotSurface(S,Grid,PlotInfoScr,InfoScr,NumLines,zParity,
                        True,UseVertScale,Plotted,PlotLabel,Color)
         ELSE IF NOT Zoom THEN
            BEGIN
               Info(Potl.InfoScr,Potl.NumLines,4);
               PostEulerAngles(alpha,beta,gamma);
            END;
         {Dispose(View); Dispose(RVkeys); {Mac only}
      END;
END; {RotateSurface}

PROCEDURE PlotContour(VAR S :DMatrixExt; VAR Grid :GridType;
                      VAR PlotInfoScr,InfoScr :InfoScrType;
                      NumLines,zParity :integer;
                      Post :boolean; VAR Plotted :boolean;
                      PlotLabel :string);
{Draw contours described by S(+/-j,i+1) for i>=0 }
{and by zParity*S(+/-j,|i|+1) for i<0. Use 24x25 grid}
{and linear interpolation.}
CONST
   AspectRatio=0.75;
VAR
   vp        :integer;
   Side,Vpx1,Vpx2,Vpy1,Vpy2   :real;
   PlotMat   :DMatrix;
   Contour   :TContour;
BEGIN
   IF NOT Plotted THEN WITH Grid DO
      BEGIN
         HideMouse;
         ClearOversizedViewPort(3);
         IF Zoom THEN vp:=3
         ELSE
            BEGIN
               vp:=2;
               IF Post THEN Info(InfoScr,NumLines,4);
               Info(PlotInfoScr,25,5);
            END;
         WITH Views[vp] DO
            BEGIN
               Vpx1:=Vx1/GetMaxX; Vpx2:=Vx2/GetMaxX;
               Vpy1:=1.0-Vy2/GetMaxY; Vpy2:=1.0-Vy1/GetMaxY
            END;
         Side:=Vpx2-Vpx1;
         IF Side>Vpy2-Vpy1 THEN Side:=Vpy2-Vpy1;
         IF AspectRatio*Grid.rhoMax/zMax<1.0 THEN
            DefineViewPort(vp,Vpx1,
                           Vpx1+Side*AspectRatio*Grid.zMax/Grid.rhoMax,
                           Vpy1,Vpy1+Side)
         ELSE
            DefineViewPort(vp,Vpx1,Vpx1+Side,Vpy1,
                           Vpy1+Side*Grid.rhoMax/Grid.zMax/AspectRatio);
         SelectViewPort(vp);
         {New(PlotMat); {Mac only}
         PlotMat.Init(24,25);
         InterpDMat(S,PlotMat,zParity);
         {New(Contour); {Mac only}
         Contour.Init;
         Contour.DrawMatrix(PlotMat);
         PlotMat.Free;
         Event.KeyPressed:=False;
         Event.MouseClicked:=False;
         IF Zoom THEN Contour.PrintLegend(4)
         ELSE IF Post THEN
            BEGIN
               SelectViewPort(1); Contour.PrintLegend(4)
            END;
         SelectViewPort(vp);
         PutLabel(Top,PlotLabel);
         DefineViewPort(vp,Vpx1,Vpx2,Vpy1,Vpy2);
         Plotted:=True;
         {Dispose(Contour); Dispose(PlotMat); {Mac only}
         ShowMouse;
      END;
END; {PlotContour}

{--------Routines for defining potentials and changing parameters------}

PROCEDURE SetPotlValues(VAR Potl :PotlType; VAR Grid :GridType);
VAR
   i,j       :integer;
   rho,z,
   V,theta,r :real;
BEGIN
   WITH Potl,Grid DO
      BEGIN
         SingVal:=-2/sqrt(sqr(drho)+sqr(dz));
         Plotted:=False; Saved:=False;
         Vmat.Free; Vmat.Init(NrhoSteps,NzSteps+1);
         IF IDcode='Zeeman' THEN
            FOR i:=0 TO NzSteps DO FOR j:=1 TO NrhoSteps DO
               BEGIN
                  rho:=(j-0.5)*drho; z:=i*dz;
                  IF (-MachEps<=rho) AND (rho<=MachEps)
                     AND (-MachEps<=z) AND (z<=MachEps) THEN V:=SingVal
                  ELSE
                     V:=-Params[1]/sqrt(sqr(rho)+sqr(z))
                           +0.125*sqr(Params[2]*eTeslaToMeV2
                                  /sqr(Mass*Mfac*alfa))*sqr(rho);
                  Vmat.Put(j,i+1,V);
               END
         ELSE IF IDcode='H2Ion' THEN
            FOR i:=0 TO NzSteps DO FOR j:=1 TO NrhoSteps DO
               BEGIN
                  rho:=(j-0.5)*drho; z:=i*dz;
                  IF (-MachEps<=rho) AND (rho<=MachEps) THEN
                     IF (-Params[2]/L0-MachEps<=z)
                        AND (z<=-Params[2]/L0+MachEps) THEN V:=SingVal
                     ELSE IF (Params[2]/L0-MachEps<=z)
                        AND (z<=Params[2]/L0+MachEps) THEN V:=SingVal
                     ELSE
                        V:=-Params[1]*(1/abs(z-Params[2]/L0)
                                       +1/abs(z+Params[2]/L0))
                  ELSE
                     V:=-Params[1]*(1/sqrt(sqr(rho)+sqr(z-Params[2]/L0))
                                 +1/sqrt(sqr(rho)+sqr(z+Params[2]/L0)));
                  Vmat.Put(j,i+1,V);
               END
         ELSE IF IDcode='WoodsSaxon' THEN
            FOR i:=0 TO NzSteps DO FOR j:=1 TO NrhoSteps DO
               BEGIN
                  rho:=(j-0.5)*drho; z:=i*dz;
                  r:=L0*sqrt(sqr(rho)+sqr(z));
                  theta:=ArcTan2(z,rho);
                  V:=-(1-Params[4]*(3*sqr(cos(theta))-1)/2)
                       /(1+ExpSafe((r-Params[2])/Params[3]));
                  Vmat.Put(j,i+1,V);
               END
         ELSE IF IDcode='Oscillator' THEN
            FOR i:=0 TO NzSteps DO FOR j:=1 TO NrhoSteps DO
               BEGIN
                  rho:=(j-0.5)*drho; z:=i*dz;
                  V:=0.5*(sqr(rho)+sqr(Potl.Params[2]*z/Potl.Params[1]));
                  Vmat.Put(j,i+1,V);
               END
         else if IDcode='Cylinder' then
            FOR i:=0 TO NzSteps DO FOR j:=1 TO NrhoSteps DO
               BEGIN
                  rho:=(j-0.5)*drho; z:=i*dz;
                  IF (rho<Params[2]/L0) AND (-Params[3]/L0<z)
                     AND (z<Params[3]/L0) THEN V:=-1.0
                  ELSE V:=0.0;
                  Vmat.Put(j,i+1,V);
               END
         ELSE IF IDcode='UserDefined' THEN
            FOR i:=0 TO NzSteps DO FOR j:=1 TO NrhoSteps DO
               BEGIN
                  rho:=(j-0.5)*drho; z:=i*dz;
                  r:=sqrt(sqr(rho)+sqr(z));
                  theta:=ArcTan2(z,rho);
                  V:=0.0; {Replace with expression that depends
                           on rho AND z, OR r AND theta}
                  {Remember that rho and z are scaled by L0.}
                  Vmat.Put(j,i+1,V);
               END;
      END;
END; {SetPotlValues}

PROCEDURE SetPotlUnits(IDcode :string; Eunit,Lunit,Munit :char;
                       VAR ParamUnits :ParamNameType);
BEGIN
   IF IDcode='Zeeman' THEN
      BEGIN
         ParamUnits[1]:='';
         ParamUnits[2]:='Tesla';
      END
   ELSE IF IDcode='H2Ion' THEN
      BEGIN
         ParamUnits[1]:='';
         ParamUnits[2]:=ConCat(Lunit,'m');
      END
   ELSE IF IDcode='WoodsSaxon' THEN
      BEGIN
         ParamUnits[1]:=ConCat(Eunit,'eV');
         ParamUnits[2]:=ConCat(Lunit,'m');
         ParamUnits[3]:=ConCat(Lunit,'m');
         ParamUnits[4]:='';
      END
   ELSE IF IDcode='Oscillator' THEN
      BEGIN
         ParamUnits[1]:=ConCat(Eunit,'eV/hbar');
         ParamUnits[2]:=ConCat(Eunit,'eV/hbar');
      END
   ELSE IF IDcode='Cylinder' THEN
      BEGIN
         ParamUnits[1]:=ConCat(Eunit,'eV');
         ParamUnits[2]:=ConCat(Lunit,'m');
         ParamUnits[3]:=ConCat(Lunit,'m');
      END
   ELSE IF IDcode='UserDefined' THEN
      BEGIN
         ParamUnits[1]:=ConCat(Eunit,'eV');
         ParamUnits[2]:=ConCat(Lunit,'m');
         ParamUnits[3]:='';
      END;
END; {SetPotlUnits}

PROCEDURE SetPotlScales(IDcode :string; VAR V0,L0,Shift :real;
                        Mass,Efac,Lfac,Mfac :real;
                        VAR Params,DefParams :ParamsType;
                        VAR ParamNames :ParamNameType);
VAR
   V0Old :real;
BEGIN
   V0Old:=V0;
   IF IDcode='Zeeman' THEN
      BEGIN
         IF Params[1]<0.0 THEN
            RealInputWarn(ParamNames[1],Params[1],DefParams[1],8);
         V0:=Mass*sqr(alfa)*Mfac*1.0e6/Efac;
         L0:=MeVtonm/(Mass*alfa*Mfac*Lfac);
      END
   ELSE IF IDcode='H2Ion' THEN
      BEGIN
         IF Params[1]<0.0 THEN
            RealInputWarn(ParamNames[1],Params[1],DefParams[1],8);
         V0:=Mass*sqr(alfa)*Mfac*1.0e6/Efac;
         L0:=MeVtonm/(Mass*alfa*Mfac*Lfac);
      END
   ELSE IF IDcode='WoodsSaxon' THEN
      BEGIN
         IF Params[1]<=0.0 THEN
            RealInputWarn(ParamNames[1],Params[1],DefParams[1],8);
         IF Params[3]<=0.0 THEN
            RealInputWarn(ParamNames[3],Params[3],DefParams[3],8);
         V0:=Params[1];
         L0:=Params[2];
      END
   ELSE IF IDcode='Oscillator' THEN
      BEGIN
         IF Params[1]<=0.0 THEN
            RealInputWarn(ParamNames[1],Params[1],DefParams[1],8);
         V0:=Params[1];
         L0:=MeVtonm/(sqrt(Mass*Mfac*Params[1]*Efac/1.0e6)*Lfac);
      END
   ELSE IF IDcode='Cylinder' THEN
      BEGIN
         IF Params[1]<=0.0 THEN
            RealInputWarn(ParamNames[1],Params[1],DefParams[1],8);
         V0:=Params[1];
         L0:=sqrt(sqr(Params[2])+sqr(Params[3]));
         IF L0=0.0 THEN L0:=1.0;
      END
   ELSE IF IDcode='UserDefined' THEN
      BEGIN
         IF Params[1]<=0.0 THEN
            RealInputWarn(ParamNames[1],Params[1],DefParams[1],8);
         V0:=Params[1];
         L0:=Params[2];
      END;
   Shift:=V0Old*Shift/V0;
END; {SetPotlScales}

PROCEDURE ValidatePotl(VAR IDcode :string; VAR ValidPotl :boolean);
BEGIN
   IF IDcode='Zeeman' THEN ValidPotl:=True
   ELSE IF IDcode='H2Ion' THEN ValidPotl:=True
   ELSE IF IDcode='WoodsSaxon' THEN  ValidPotl:=True
   ELSE IF IDcode='Oscillator' THEN ValidPotl:=True
   ELSE IF IDcode='Cylinder' THEN ValidPotl:=True
   ELSE IF IDcode='UserDefined' THEN  ValidPotl:=True
   ELSE ValidPotl:=False;
END; {ValidatePotl}
                                        
PROCEDURE PrepVParamList(VAR Potl :PotlType; VAR Grid :GridType);
VAR
   n :integer;
BEGIN
   WITH Potl,Grid DO
      BEGIN
         ParamList[1]:=' Potential parameters';
         FOR n:=1 TO NumParams DO
            ParamList[n+1]:=ConCat(' ',ParamNames[n],' ',
                            Num2Str(Params[n],6),' ',ParamUnits[n]);
         ParamList[NumParams+2]:=ConCat('     z_max: ',Num2Str(L0*zMax,6),
                                        ' ',Lunit,'m');
         ParamList[NumParams+3]:=ConCat('   delta z: ',Num2Str(L0*dz,6),
                                        ' ',Lunit,'m');
         ParamList[NumParams+4]:=ConCat('   rho_max: ',Num2Str(L0*rhoMax,6),
                                        ' ',Lunit,'m');
         ParamList[NumParams+5]:=ConCat(' delta rho: ',Num2Str(L0*drho,6),
                                        ' ',Lunit,'m');
         ParamList[NumParams+6]:=ConCat('  Vertical scale (',Eunit,'eV)');
         ParamList[NumParams+7]:=ConCat('   ',Num2Str(V0*VertMin,6),
                                        ' to ',Num2Str(V0*VertMax,6),' ');
         FOR n:=NumParams+8 TO 25 DO ParamList[n]:='';
      END;
END; {PrepVParamList}

PROCEDURE PrepPotlPlotScr(VAR Potl :PotlType);
VAR
   n,i :integer;
BEGIN
   WITH Potl DO
      BEGIN
         FOR n:=1 TO NumParams+7 DO PlotInfoScr[n]:=ParamList[n];
         FOR n:=NumParams+8 TO 25 DO PlotInfoScr[n]:=''
      END;
END; {PrepPotlPlotScr}

PROCEDURE PrepPotl(VAR Potl :PotlType; VAR Grid :GridType);
BEGIN
   SetPotlValues(Potl,Grid);
   PrepVParamList(Potl,Grid);
   PrepPotlPlotScr(Potl);
   Potl.Plotted:=False;
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

PROCEDURE SetPotlToDefaults(VAR Potl :PotlType; VAR Grid :GridType);
{Set potential and grid parameters to default values.}
VAR
   n :integer;
BEGIN
   WITH Potl,Grid DO
      BEGIN
         FOR n:=1 TO NumParams DO Params[n]:=DefParams[n];
         PlotType:=1;
         InitializeScales(Potl);
         SetPotlScales(IDcode,V0,L0,Shift,Mass,Efac,Lfac,Mfac,
                       Params,DefParams,ParamNames);
         ComputeZeta(V0,L0,Mass,Eunit,Lunit,Munit,zeta);
         zMax:=DefzMax/L0; rhoMax:=DefrhoMax/L0;
         dz:=zMax/NzSteps; drho:=rhoMax/(NrhoSteps-0.5);
         VertMin:=DefVertMin/V0; VertMax:=DefVertMax/V0;
      END
END; {SetPotlToDefaults}

PROCEDURE SetPotlToZeeman(VAR Potl :PotlType; VAR Grid :GridType);
{Prepare I/O screen InfoScr for Zeeman potential and set parameters.}
VAR
   n :integer;
BEGIN
   WITH Potl DO
      BEGIN
         IDcode:='Zeeman';
         NumParams:=2;
         ParamNames[1]:=' Z:';
         ParamNames[2]:=' B:';
         NumLines:=6;
         InfoScr[1]:=' quadratic Zeeman potential   ';
         InfoScr[2]:='                              ';
         InfoScr[3]:='V(rho,z)=-Ze^2/sqrt(rho^2+z^2)';
         InfoScr[4]:='          +(e*B*rho)^2/(8*mu) ';
         InfoScr[5]:='                              ';
         InfoScr[6]:='where mu is the particle mass.';
         FOR n:=NumLines+1 TO 25 DO InfoScr[n]:='';
         DefParams[1]:=1; DefParams[2]:=1e6;
         DefzMax:=0.3; DefrhoMax:=0.3;
         DefVertMin:=-100.0; DefVertMax:=50.0
      END;
   SetPotlToDefaults(Potl,Grid);
   PrepPotl(Potl,Grid)
END; {SetPotlToZeeman}

PROCEDURE SetPotlToH2Ion(VAR Potl :PotlType; VAR Grid :GridType);
{Prepare I/O screen InfoScr for H2+ ion potential and set parameters.}
VAR
   n :integer;
BEGIN
   WITH Potl DO
      BEGIN
         IDcode:='H2Ion';
         NumParams:=2;
         ParamNames[1]:=' Z:';
         ParamNames[2]:=' a:';
         NumLines:=5;
         InfoScr[1]:='       H2+ ion potential      ';
         InfoScr[2]:='                              ';
         InfoScr[3]:='V(rho,z)=-Ze^2*               ';
         InfoScr[4]:='      [1/(sqrt(rho^2+(z-a)^2) ';
         InfoScr[5]:='      +1/(sqrt(rho^2+(z+a)^2)]';
         FOR n:=NumLines+1 TO 25 DO InfoScr[n]:='';
         DefParams[1]:=1; DefParams[2]:=0.1;
         DefzMax:=0.4; DefrhoMax:=0.3;
         DefVertMin:=-50.0; DefVertMax:=5.0
      END;
   SetPotlToDefaults(Potl,Grid);
   PrepPotl(Potl,Grid);
END; {SetPotlToH2Ion}

PROCEDURE SetPotlToWoodsSaxon(VAR Potl :PotlType; VAR Grid :GridType);
{Prepare I/O screen InfoScr for Woods-Saxon potential}
{and set parameters.}
VAR
   n :integer;
BEGIN
   WITH Potl DO
      BEGIN
         IDcode:='WoodsSaxon';
         NumParams:=4;
         ParamNames[1]:=' V_0:';
         ParamNames[2]:='   c:';
         ParamNames[3]:='   a:';
         ParamNames[4]:='   b:';
         NumLines:=7;
         InfoScr[1]:='    Woods-Saxon potential     ';
         InfoScr[2]:='                 1-b*P_20     ';
         InfoScr[3]:='V(rho,z)=-V_0 ----------------';
         InfoScr[4]:='               1+exp[(r-c)/a] ';
         InfoScr[5]:='   P_20=[3cos^2(theta)-1]/2   ';
         InfoScr[6]:='     theta=atan(rho/z)        ';
         InfoScr[7]:='     r=sqrt(rho^2+z^2)        ';
         FOR n:=NumLines+1 TO 25 DO InfoScr[n]:='';
         DefParams[1]:=50.0; DefParams[2]:=5.0;
         DefParams[3]:=0.5; DefParams[4]:=0.1;
         DefzMax:=15.0; DefrhoMax:=15.0;
         DefVertMin:=-60.0; DefVertMax:=10.0
      END;
   SetPotlToDefaults(Potl,Grid);
   PrepPotl(Potl,Grid)
END; {SetPotlToWoodsSaxon}

PROCEDURE SetPotlToOscillator(VAR Potl :PotlType; VAR Grid :GridType);
{Prepare I/O screen InfoScr for oscillator potential}
{and set parameters.}
VAR
   n :integer;
BEGIN
   WITH Potl DO
      BEGIN
         IDcode:='Oscillator';
         NumParams:=2;
         ParamNames[1]:=' w_1:';
         ParamNames[2]:=' w_2:';
         NumLines:=7;
         InfoScr[1]:='     Oscillator potential     ';
         InfoScr[2]:='                              ';
         InfoScr[3]:='V(rho,z)=0.5*mu*              ';
         InfoScr[4]:='           [(w_1*rho)^2       ';
         InfoScr[5]:='                 +(w_2*z)^2]  ';
         InfoScr[6]:='                              ';
         InfoScr[7]:='where mu is the particle mass.';
         FOR n:=NumLines+1 TO 25 DO InfoScr[n]:='';
         DefParams[1]:=1.0; DefParams[2]:=1.0;
         DefzMax:=1.0; DefrhoMax:=1.0;
         DefVertMin:=0.0; DefVertMax:=20.0
      END;
   SetPotlToDefaults(Potl,Grid);
   PrepPotl(Potl,Grid)
END; {SetPotlToOscillator}

PROCEDURE SetPotlToCylinder(VAR Potl :PotlType; VAR Grid :GridType);
{Prepare I/O screen InfoScr for cylindrical well and set parameters.}
VAR
   n :integer;
BEGIN
   WITH Potl DO
      BEGIN
         IDcode:='Cylinder';
         NumParams:=3;
         ParamNames[1]:='   V_0:';
         ParamNames[2]:=' rho_1:';
         ParamNames[3]:='   z_1:';
         NumLines:=5;
         InfoScr[1]:='       Cylindrical well       ';
         InfoScr[2]:='                              ';
         InfoScr[3]:=' V(rho,z)=-V_0, rho<rho_1,    ';
         InfoScr[4]:='                |z|<z_1       ';
         InfoScr[5]:=' V(rho,z)= 0,   otherwise     ';
         FOR n:=NumLines+1 TO 25 DO InfoScr[n]:='';
         DefParams[1]:=1.0; DefParams[2]:=0.5; DefParams[3]:=0.5;
         DefzMax:=2.0; DefrhoMax:=2.0;
         DefVertMin:=-2.0; DefVertMax:=1.0
      END;
   SetPotlToDefaults(Potl,Grid);
   PrepPotl(Potl,Grid)
END; {SetPotlToCylinder}

PROCEDURE SetPotlToUserDefined(VAR Potl :PotlType; VAR Grid :GridType);
{Prepare I/O screen InfoScr for user-defined potential}
{and set parameters.}
VAR
   n :integer;
BEGIN
   Announce('Modify code to insert your potential');
{   with Potl do                                                       }
{     BEGIN                                                            }
{        IDcode:='UserDefined';                                        }
{        NumParams:=3;                                                 }
{        ParamNames[1]:=' V0:';                                        }
{        ParamNames[2]:='  a:';                                        }
{        ParamNames[3]:='  b:';                                        }
{        NumLines:=4;                                                  }
{        InfoScr[1]:='    User-defined potential    ';                 }
{        InfoScr[2]:='                              ';                 }
{        InfoScr[3]:='V(rho,z)=-V_0                 ';                 }
{        InfoScr[4]:='                              ';                 }
{        FOR n:=NumLines+1 TO 25 DO InfoScr[n]:='';                    }
{        DefParams[1]:=1.0; DefParams[2]:=0.2; DefParams[3]:=0.2;      }
{        DefzMax:=5.0; DefrhoMax:=5.0;                                 }
{        DefVertMin:=-1.0; DefVertMax:=0.5                             }
{     END;                                                             }
{  SetPotlToDefaults(Potl,Grid);                                       }
{  PrepPotl(Potl,Grid)                                                 }
END; {SetPotlToUserDefined}

PROCEDURE ModifyPotential(VAR Potl :PotlType; VAR Grid :GridType);
VAR
   n,i,j    :integer;
   InputScr :TInputScreen;
BEGIN
   {New(InputScr);  {Mac only}
   WITH Potl,Grid,InputScr DO
      BEGIN
         Init;
         DefineInputPort(0.64,0.94,0.1,0.9);
         LoadLine('Potential parameters');
         FOR n:=1 TO NumParams DO
            LoadLine(ConCat(ParamNames[n],' {      }',
                            ParamUnits[n]));
         LoadLine('');
         LoadLine(ConCat(' z_max: ',Num2Str(L0*zMax,6),' ',Lunit,'m'));
         LoadLine(ConCat('  no. of steps: ',NumStr(NzSteps,4,0)));
         LoadLine(ConCat(' rho_max: ',Num2Str(L0*rhoMax,6),' ',Lunit,'m'));
         LoadLine(ConCat('  no. of steps: ',NumStr(Nrhosteps,4,0)));
         LoadLine('');
         LoadLine(ConCat(' Vertical scale (',Eunit,'eV)'));
         LoadLine('  {      } to {      }');
         LoadLine('');
         LoadLine(' Restore defaults? #F    ');
         LoadLine('');
         LoadLine('  [  OK  ]    [Cancel]');
         FOR n:=1 TO NumParams DO SetNumber(n,Params[n]);
         SetNumber(NumParams+1,V0*VertMin);
         SetNumber(NumParams+2,V0*VertMax);
         AcceptScreen;
         IF NOT Canceled THEN
            BEGIN
               IF GetBoolean(NumParams+3) THEN SetPotlToDefaults(Potl,Grid)
               ELSE
                  BEGIN
                     FOR n:=1 TO NumParams DO Params[n]:=GetNumber(n);
                     SetPotlScales(IDcode,V0,L0,Shift,Mass,Efac,Lfac,Mfac,
                                   Params,DefParams,ParamNames);
                     ComputeZeta(V0,L0,Mass,Eunit,Lunit,Munit,zeta);
                     VertMin:=GetNumber(NumParams+1)/V0;
                     VertMax:=GetNumber(NumParams+2)/V0;
                  END;
               PrepPotl(Potl,Grid);
            END;
         Done;
      END;
   {Dispose(InputScr); {Mac only}
END; {ModifyPotential}

PROCEDURE PlotPotential(VAR Potl :PotlType;
                        VAR Grid :GridType; PotlPlotType :integer);
BEGIN
   WITH Potl DO
      BEGIN
         IF PotlPlotType=1 THEN
            PlotSurface(Vmat,Grid,PlotInfoScr,InfoScr,NumLines,1,
                        True,True,Plotted,'Potential',LightCyan)
         ELSE
            PlotContour(Vmat,Grid,PlotInfoScr,InfoScr,NumLines,1,
                        True,Plotted,'Potential');
         Plotted:=True;
      END
END; {PlotPotential}

END. {ClSmPotl}
