{Unit of routines for the calculation of phase shifts, differential  }
{cross sections, and total cross sections.                           }
{Used by Scattr3D.pas.                                               }
{                                                                    }
{Author: John R. Hiller                                              }
{Version: 1.0                  11/13/94                              }
{Copyright (c) 1995 John Wiley & Sons, Inc.                          }
{                                                                    }
UNIT Sc3DPDT;

INTERFACE

USES Graph,CUPS,CUPSmupp,CUPSfunc,CUPSproc,CUPSgui,CUPSgrph,
     Sc3DMach,Sc3DCmn,Sc3DPotl,Sc3DWF;

CONST
   MaxNumE=502;    {maximum number of energy values}

TYPE
   PStype=record
      PShift,
      Energy        :DVector;
      AngMom        :integer;
      Emin,
      Emax,
      deltaE,
      PSmin,
      PSmax         :real;
      NumE,
      PlotType      :integer;
      Plotted,
      Zoom,
      Computed      :boolean;
      PlotInfoScr   :InfoScrType
      END; {PStype}
   DXStype=record
      DiffXS,
      Angle         :DVector; 
      AngMomMin,
      AngMomMax     :integer;
      ThetaMin,ThetaMax,        {horizontal range for plot}
      VertMin,VertMax,          {vertical range for plot}
      Energy,
      dTheta,
      Min,Max       :real;
      nAngles,
      PlotType      :integer;
      Plotted,
      Zoom,
      Computed      :boolean;
      PlotInfoScr   :InfoScrType
      END; {DXStype}
   TXStype=record
      TotalXS,
      Energy        :DVector;
      AngMomMin,
      AngMomMax     :integer;
      Emin,Emax,
      deltaE,
      VertMin,VertMax,          {vertical range for plot}
      Min,Max       :real;
      NumE,
      PlotType      :integer;
      Plotted,
      Zoom,
      Computed      :boolean;
      PlotInfoScr   :InfoScrType
      END; {TXStype}

VAR
   PS            :PStype;
   DXS           :DXStype;
   TXS           :TXStype;

{--------Routines for phase shifts------------------------------------}

PROCEDURE SetPSDefaults(VAR PS :PStype; VAR Potl :PotlType);
PROCEDURE PrepPSPlotScr(VAR PS :PStype; VAR Potl :PotlType);
PROCEDURE PlotPhaseShift(VAR PS :PStype; VAR Potl :PotlType);
PROCEDURE ComputePS(VAR PS :PStype; VAR Potl :PotlType;
                    VAR MainMenu :TMenu; VAR MenuClicked :boolean);

{--------Routines for differential cross sections---------------------}

PROCEDURE SetDXSDefaults(VAR DXS :DXStype; VAR Potl :PotlType);
PROCEDURE PrepDXSPlotScr(VAR DXS :DXStype; VAR Potl :PotlType);
PROCEDURE PlotDiffXSect(VAR DXS :DXStype; VAR Potl :PotlType);
PROCEDURE ComputeDXS(VAR DXS :DXStype; VAR Potl :PotlType;
                     VAR MainMenu :TMenu; VAR MenuClicked :boolean);

{--------Routines for total cross sections----------------------------}

PROCEDURE SetTXSDefaults(VAR TXS :TXStype; VAR Potl :PotlType);
PROCEDURE PrepTXSPlotScr(VAR TXS :TXStype; VAR Potl :PotlType);
PROCEDURE PlotTotalXSect(VAR TXS :TXStype; VAR Potl :PotlType);
PROCEDURE ComputeTXS(VAR TXS :TXStype; VAR Potl :PotlType;
                     VAR MainMenu :TMenu; VAR MenuClicked :boolean);

IMPLEMENTATION

{--------Routines for phase shifts------------------------------------}

PROCEDURE SetPSDefaults(VAR PS :PStype; VAR Potl :PotlType);
BEGIN
   WITH PS DO
      BEGIN
         Emin:=0.1; Emax:=2.0; deltaE:=0.02;
         PSmin:=-3*cPi; PSmax:=3*cPi;
         AngMom:=0;
         PlotType:=1;      {[-pi/2,pi/2]}
         Computed:=False;
         Plotted:=False;
         Zoom:=False
      END
END; {SetPSDefaults}

PROCEDURE PrepPSPlotScr(VAR PS :PStype; VAR Potl :PotlType);
{Prepare Info screen to go with plot of phase shifts.}
VAR
   n,i :integer;
BEGIN
   WITH PS,Potl DO
      BEGIN
         FOR n:=1 TO NumParams+5 DO PS.PlotInfoScr[n]:=ParamList[n];
         PS.PlotInfoScr[NumParams+6]:=' Phase shift parameters';
         PS.PlotInfoScr[NumParams+7]:=
            ConCat('  Ang. momentum: ',NumStr(AngMom,2,0));
         PS.PlotInfoScr[NumParams+8]:=
            ConCat('  Energy range (',Eunit,'eV)');
         PS.PlotInfoScr[NumParams+9]:=
            ConCat('   ',Num2Str(V0*Emin,6),' to ',Num2Str(V0*Emax,6));
         PS.PlotInfoScr[NumParams+10]:=
            ConCat('  delta E: ',Num2Str(V0*deltaE,6),' ',Eunit,'eV');
         PS.PlotInfoScr[NumParams+11]:=
            ConCat('  Mass: ',Num2Str(Mass,6),' ',Munit,'eV/c^2');
         PS.PlotInfoScr[NumParams+12]:='';
         PS.PlotInfoScr[NumParams+13]:=' Algorithm: ';
         CASE Alg OF
            0 :PS.PlotInfoScr[NumParams+14]:='  piecewise constant';
            1 :PS.PlotInfoScr[NumParams+14]:='  transfer matrix';
            2 :PS.PlotInfoScr[NumParams+14]:='  direct integration';
            END; {case}
         FOR n:=NumParams+15 TO 25 DO PS.PlotInfoScr[n]:=''
      END
END; {PrepPSPlotScr}

PROCEDURE PSLegend;
{Display legend for plot of phase shifts.}
BEGIN
   OpenViewPort(1);
   ClearViewPort;
   SelectScale(1);
   LegendEntry(3,LightCyan,'new ph shift');
   LegendEntry(4,LightRed,'old ph shift');
   SetColor(Whitte)
END; {PSLegend}

PROCEDURE PrepPSPlot(VAR PS :PStype; VAR Potl :PotlType);
{Prepare viewport for plot of phase shifts.}
VAR
   vp :integer;
BEGIN
   WITH PS DO
      BEGIN
         IF Zoom THEN vp:=3
         ELSE
            BEGIN
               vp:=2;
               ClearOversizedViewPort(3);
               IF Computed THEN Info(PlotInfoScr,25,5)
               ELSE Info(Potl.PlotInfoScr,25,5);
               Info(Potl.InfoScr,Potl.NumLines,4);
               PSLegend
            END;
         PrepViewPort(vp);
         IF PSmax<=PSmin THEN PSmax:=PSmin+cPi;
         CASE PlotType OF
            1 :BEGIN
                  SpecialAxis(4,Potl.V0*Emin,Potl.V0*Emax,5,
                                -cPi/2,cPi/2,10);
                  PutLabel(Top,'Phase shifts - [-pi/2, pi/2]')
               END;
            2 :BEGIN
                  SpecialAxis(4,Potl.V0*Emin,Potl.V0*Emax,5,0.0,cPi,10);
                  PutLabel(Top,'Phase shifts - [0, pi]')
               END;
            3 :BEGIN
                  SpecialAxis(4,Potl.V0*Emin,Potl.V0*Emax,5,
                                PSmin,PSmax,10);
                  PutLabel(Top,'Phase shifts - [min, max]')
               END
            END; {case}
         PutLabel(Bottom,ConCat('Energy (',Potl.Eunit,'eV)'));
      END;
END; {PrepPSPlot}

PROCEDURE PlotPSvalues(VAR PS :PStype; V0 :real; nE :integer);
{Plot pair of phase shift values.}
VAR
   PSval1,PSval2,PSval1M,PSval2M,E1,E2,Etemp :real;
   Sign,count1,count2 :integer;

   FUNCTION ModRange(x,start,stop :real; VAR count :integer) :real;
   VAR range :real;
   BEGIN
      range:=stop-start;
      count:=0;
      IF x<start THEN
         REPEAT
            x:=x+range; count:=count-1;
         UNTIL x>=start
      ELSE IF x>=stop THEN
         REPEAT
            x:=x-range; count:=count+1;
         UNTIL x<stop;
      ModRange:=x;
   END; {ModRange}
         
BEGIN
   WITH PS DO
      BEGIN
      IF Zoom THEN SelectViewPort(3) ELSE SelectViewPort(2);
      SelectScale(4);
      E1:=Energy.Value(nE-1); E2:=Energy.Value(nE);
      PSval1:=PShift.Value(nE-1); PSval2:=PShift.Value(nE);
      CASE PlotType OF
         1 :BEGIN
               PSval1M:=ModRange(PSval1,-cPi/2,cPi/2,count1);
               PSval2M:=ModRange(PSval2,-cPi/2,cPi/2,count2);
               IF count2=count1 THEN
                  PlotLine(V0*E1,PSval1M,V0*E2,PSval2M)
               ELSE
                  BEGIN
                     IF count2>count1 THEN Sign:=1 ELSE Sign:=-1;
                     Etemp:=E1+(E2-E1)*(Sign*cPi/2-PSval1M)/(PSval2-PSval1);
                     PlotLine(V0*E1,PSval1M,V0*Etemp,Sign*cPi/2);
                     Etemp:=E2-(E2-E1)*(PSval2M+Sign*cPi/2)/(PSval2-PSval1);
                     PlotLine(V0*Etemp,-Sign*cPi/2,V0*E2,PSval2M)
                  END
            END;
         2 :BEGIN
               PSval1M:=ModRange(PSval1,0.0,cPi,count1);
               PSval2M:=ModRange(PSval2,0.0,cPi,count2);
               IF count2=count1 THEN
                  PlotLine(V0*E1,PSval1M,V0*E2,PSval2M)
               ELSE
                  BEGIN
                     IF count2>count1 THEN Sign:=1 ELSE Sign:=0;
                     Etemp:=E1+(E2-E1)*(Sign*cPi-PSval1M)/(PSval2-PSval1);
                     PlotLine(V0*E1,PSval1M,V0*Etemp,Sign*cPi);
                     Etemp:=E2-(E2-E1)*(PSval2M-(1-Sign)*cPi)
                                    /(PSval2-PSval1);
                     PlotLine(V0*Etemp,(1-Sign)*cPi,V0*E2,PSval2M)
                  END
               END;
         3 :PlotLine(V0*E1,PSval1,V0*E2,PSval2)
         END {case PlotType}
      END
END; {PlotPSvalues}

PROCEDURE ReplotOldPS(VAR PS :PStype; VAR Potl :PotlType);
VAR
   nE :integer;
BEGIN
   WITH PS DO
      BEGIN
         SetColor(LightRed);
         FOR nE:=2 TO NumE DO PlotPSvalues(PS,Potl.V0,nE);
         SetColor(Whitte);
      END;
END; {ReplotOldPS}

PROCEDURE PlotPhaseShift(VAR PS :PStype; VAR Potl :PotlType);
VAR
   nE :integer;
BEGIN
   WITH PS DO
      BEGIN
         PrepPSPlot(PS,Potl);
         SetColor(LightCyan);
         FOR nE:=2 TO NumE DO PlotPSvalues(PS,Potl.V0,nE);
         Plotted:=True;
         SetColor(Whitte);
      END;
END; {PlotPhaseShift}

PROCEDURE ComputePhaseShift(VAR PS :PStype; VAR Potl :PotlType);
{Compute phase shifts for range of energy}
{and add to plot previous results.}
VAR
   PSHelpScr      :HelpScrType;
   CalcIncomplete :string;
   PSkeys         :THotKeys;
   Pkey           :byte;
   n,nE           :integer;
   WishToAbort    :boolean;
   E,Elarge,Etemp,
   deltaEnew,
   PSval,uMax     :real;
   ComputeScr     :TInputScreen;
BEGIN
   PSHelpScr[01]:='';
   PSHelpScr[02]:=' <F1>-show this help screen.';
   PSHelpScr[03]:='';
   PSHelpScr[04]:=' <F2>-stop the calculation.';
   PSHelpScr[05]:='';
   PSHelpScr[06]:=' <F5>-halve the energy step deltaE.';
   PSHelpScr[07]:='';
   PSHelpScr[08]:=' <F6>-double the energy step deltaE.';
   FOR n:=9 TO 25 DO PSHelpScr[n]:='';
   CalcIncomplete:=
      'Calculation incomplete due to limit on storage of energy values.';

   {New(ComputeScr); {Mac only}
   WITH Potl,ComputeScr DO
      BEGIN
         Elarge:=V0*sqr(2*cPi/(3*dr))/zeta;
         nE:=round((PS.Emax-PS.Emin)/PS.deltaE);
         Init;
         DefineInputPort(0.63,0.93,0.0,0.92);
         FOR n:=1 TO NumParams+5 DO LoadLine(ParamList[n]);
         LoadLine(       'Phase shift parameters');
         LoadLine(       ' Ang. momentum: {   }');
         LoadLine(ConCat(' Energy range: (',Eunit,'eV)'));
         LoadLine(       '  {      } to {      }');
         LoadLine(ConCat(' delta E: {      }',Eunit,'eV'));
         LoadLine(ConCat(' Mass: ',Num2Str(Mass,6),' ',Munit,'eV/c^2'));
         LoadLine('');
         IF PS.Plotted THEN LoadLine('  Clear plot? #F');
         LoadLine('');
         LoadLine(' [  OK  ]    [Cancel]');
         SetNumber(1,PS.AngMom);
         SetNumber(2,V0*PS.Emin);
         IF V0*PS.Emax>Elarge THEN
            BEGIN
               SetNumber(3,Elarge);
               deltaEnew:=(Elarge-V0*PS.Emin)/nE;
            END
         ELSE
            BEGIN
               SetNumber(3,V0*PS.Emax);
               deltaEnew:=V0*PS.deltaE;
            END;
         SetNumber(4,deltaEnew);
         AcceptScreen;
         IF NOT Canceled THEN WITH PS DO
            BEGIN
               IF Plotted AND NOT GetBoolean(5) THEN ReplotOldPS(PS,Potl);
               AngMom:=round(GetNumber(1));
               IF (AngMom<0) OR (AngMom>MaxRange) THEN
                  IntInputWarn('angular momentum',AngMom,0,2);
               Emin:=GetNumber(2); Emax:=GetNumber(3);
               IF Emin<=0.0 THEN
                  RealInputWarn('E_min',Emin,1.0e-7,6)
               ELSE IF Emin>Elarge THEN
                  BEGIN
                     IF Emax<=Elarge THEN
                        Announce(ConCat('Wave number too large for numerical',
                                        ' calculation to proceed.'));
                     RealInputWarn('E_min',Emin,Elarge/2,6);
                  END;
               IF Emax>Elarge THEN
                  BEGIN
                     Announce(ConCat('Wave number too large for numerical ',
                                     'calculation to proceed.'));
                     RealInputWarn('E_max',Emax,Elarge,6);
                  END
               ELSE IF Emax<=Emin THEN
                  BEGIN
                     Etemp:=Emin+2.0*V0;
                     IF Etemp>Elarge THEN Etemp:=Elarge;
                     RealInputWarn('E_max',Emax,Etemp,6);
                  END;
               deltaE:=GetNumber(4);
               IF (deltaE<=0.0) OR (Emin+deltaE>Elarge) THEN
                  RealInputWarn('delta E',deltaE,(Emax-Emin)/nE,7)
               ELSE IF (Emax-Emin)/deltaE>MaxNumE-2 THEN
                  RealInputWarn('delta E',deltaE,(Emax-Emin)/(MaxNumE-2),7);
               Emin:=Emin/V0; Emax:=Emax/V0;
               deltaE:=deltaE/V0;
               PrepPSPlotScr(PS,Potl);
               IF NOT Plotted THEN PrepPSPlot(PS,Potl)
               ELSE IF GetBoolean(5) THEN PrepPSPlot(PS,Potl)
               ELSE IF NOT Zoom THEN Info(PlotInfoScr,25,5);
               {New(PSkeys); {Mac only}
               WITH PSkeys DO
                  BEGIN
                     Init(4);
                     key[1]:='F1-Help';
                     key[2]:='F2-Stop';
                     key[3]:='F5-Halve';
                     key[4]:='F6-Double';
                     Display
                  END;
               E:=Emin; NumE:=0;
               PSmax:=0.0; PSmin:=0.0;
               IF Zoom THEN SelectViewPort(3) ELSE SelectViewPort(2);
               Print(25,2,ConCat('E:          ',Potl.Eunit,'eV'));
               Print(25,3,'delta E: ');
               WishToAbort:=False;
               WHILE (E<=Emax) AND (NumE<MaxNumE) AND (NOT WishToAbort) DO
                  BEGIN
                     RubOut(28,2,8,DarkGray); RubOut(34,3,8,DarkGray);
                     SetColor(Whitte);
                     Print(28,2,Num2Str(Potl.V0*E,8));
                     Print(34,3,Num2Str(Potl.V0*deltaE,8));
                     SolveSchrodinger(E,AngMom,Potl,uMax,PSval);
                     NumE:=NumE+1; Energy.Put(NumE,E);
                     PShift.Put(NumE,PSval);
                     IF PSval>PSmax THEN PSmax:=PSval
                     ELSE IF PSval<PSmin THEN PSmin:=PSval;
                     IF NumE>1 THEN
                        BEGIN
                           SetColor(LightCyan);
                           PlotPSvalues(PS,Potl.V0,NumE)
                        END;
                     CheckForEvents;
                     IF Event.ReadKey=Escape THEN WishToAbort:=True
                     ELSE IF PSkeys.Pressed(Pkey) THEN
                        CASE Pkey OF
                           1 :Help(PSHelpScr);
                           2 :WishToAbort:=True;
                           3 :deltaE:=deltaE/2;
                           4 :deltaE:=2*deltaE;
                           END; {case Tkey}
                     E:=E+deltaE;
                  END; {while E<=Emax}
               IF NumE>0 THEN 
                  BEGIN
                     IF (NumE=MaxNumE) AND (E<=Emax) THEN
                        BEGIN
                           Announce(CalcIncomplete);
                           Emax:=E-deltaE;
                        END;
                     Computed:=True; Plotted:=True; Potl.Saved:=False;
                     PrepPSPlotScr(PS,Potl);
                     IF NOT Zoom THEN Info(PlotInfoScr,25,5);
                  END
               ELSE Computed:=False;
               PSkeys.Clear;
               {Dispose(PSkeys); {Mac only}
            END;
         Done;
      END;
   {Dispose(ComputeScr); {Mac only}
END; {ComputePhaseShift}

PROCEDURE ChangePSPlotScales(VAR PS :PStype; VAR Potl :PotlType);
VAR
   Changed,AutoSet :boolean;
BEGIN
   WITH PS DO
      BEGIN
         AutoSet:=False;
         Emin:=Potl.V0*Emin; Emax:=Potl.V0*Emax;
         ChangePlotScales(Emin,Emax,ConCat('(',Potl.Eunit,'eV)'),
                          -1.0,(NumE-1)*Potl.V0*deltaE,
                          PSmin,PSmax,'',-1.0,6*cPi,Changed,AutoSet);
         Emin:=Emin/Potl.V0; Emax:=Emax/Potl.V0;
         IF Changed THEN
            BEGIN
               PrepPSPlotScr(PS,Potl);
               PlotPhaseShift(PS,Potl);
            END;
      END;
END; {ChangePSPlotScales}

PROCEDURE ComputePS(VAR PS :PStype; VAR Potl :PotlType;
                    VAR MainMenu :TMenu; VAR MenuClicked :boolean);
{Compute and plot phase shifts.}
VAR
   PSHelpScr :HelpScrType;
   PSkeys    :THotKeys;
   n         :integer;
   Pkey      :byte;
   WishToReturn,SaveZoom :boolean;
BEGIN
   PSHelpScr[01]:='';
   PSHelpScr[02]:=' <F1>-show this help screen.';
   PSHelpScr[03]:='';
   PSHelpScr[04]:=' <F2>-compute new phase shift with new choice';
   PSHelpScr[05]:='    of angular momentum or energy step,';
   PSHelpScr[06]:='    or with new plot range.  The input';
   PSHelpScr[07]:='    screen will include an option to erase';
   PSHelpScr[08]:='    previous results, if any.';
   PSHelpScr[09]:='';
   PSHelpScr[10]:=' <F3>-toggle size of plot.  When small, text';
   PSHelpScr[11]:='    information and a legend are shown.';
   PSHelpScr[12]:='';
   PSHelpScr[13]:=' <F4>-change horizontal and vertical scales';
   PSHelpScr[14]:='    of plot.';
   PSHelpScr[15]:='';
   PSHelpScr[16]:=' <F5>-change plot type.  The types currently';
   PSHelpScr[17]:='    installed are:';
   PSHelpScr[18]:='              [-pi/2,pi/2]';
   PSHelpScr[19]:='              [0,pi]';
   PSHelpScr[20]:='              [min,max]';
   PSHelpScr[21]:='';
   PSHelpScr[22]:=' <F10>-activate menu.';
   for n:=23 to 25 do PSHelpScr[n]:='';

   WITH PS DO
      BEGIN
         IF Computed THEN PlotPhaseShift(PS,Potl)
         ELSE
            BEGIN
               Zoom:=False; Plotted:=False;
               SaveZoom:=Potl.Zoom; Potl.Zoom:=False;
               PlotPotential(Potl);
               Potl.Zoom:=SaveZoom;
            END;
         ComputePhaseShift(PS,Potl);
         IF Computed THEN
            BEGIN
               {New(PSkeys); {Mac only}
               WITH PSkeys DO
                  BEGIN
                     Init(6);
                     key[1]:='F1-Help';
                     key[2]:='F2-Input';
                     IF Zoom THEN key[3]:='F3-Contract'
                     ELSE key[3]:='F3-Expand';
                     key[4]:='F4-Scale';
                     key[5]:='F5-PlotType';
                     key[6]:='F10-Menu';
                     Display
                  END;
               WishToReturn:=False;
               WHILE (NOT WishToReturn) AND (NOT MenuClicked) DO
                  BEGIN
                     CheckForEvents;
                     IF Event.ReadKey=Escape THEN WishToReturn:=True
                     ELSE IF PSkeys.Pressed(Pkey) THEN
                        BEGIN
                           PSkeys.Clear;
                           case Pkey of
                              1 :Help(PSHelpScr);
                              2 :ComputePhaseShift(PS,Potl);
                              3 :BEGIN
                                    Zoom:=not Zoom;
                                    PlotPhaseShift(PS,Potl);
                                    IF Zoom THEN
                                       PSkeys.key[3]:='F3-Contract'
                                    ELSE PSkeys.key[3]:='F3-Expand';
                                 END;
                              4 :ChangePSPlotScales(PS,Potl);
                              5 :BEGIN
                                    PlotType:=(PlotType MOD 3)+1;
                                    PlotPhaseShift(PS,Potl)
                                 END;
                              6 :BEGIN
                                    WishToReturn:=True;
                                    IF MainMenu.Chosen THEN
                                       MenuClicked:=True;
                                 END;
                              END; {case}
                           PSkeys.Display;
                        END
                     ELSE IF MainMenu.Activated THEN MenuClicked:=True;
                  END;
               PSkeys.Clear;
               {Dispose(PSkeys); {Mac only}
            END;
      END;
END; {ComputePS}

{--------Routines for differential cross sections---------------------}

PROCEDURE SetDXSDefaults(VAR DXS :DXStype; VAR Potl :PotlType);
BEGIN
   WITH DXS DO
      BEGIN
         Energy:=0.5;
         AngMomMin:=0; AngMomMax:=2;
         nAngles:=30; dTheta:=180/nAngles;
         PlotType:=1;         {linear plot}
         Computed:=False;
         Plotted:=False;
         Zoom:=False
      END
END; {SetDXSDefaults}

PROCEDURE PrepDXSPlotScr(VAR DXS :DXStype; VAR Potl :PotlType);
{Prepare Info screen to go with plot of differential cross section.}
VAR
   n,i :integer;
BEGIN
   WITH DXS,Potl DO
      BEGIN
         FOR n:=1 TO NumParams+5 DO DXS.PlotInfoScr[n]:=ParamList[n];
         DXS.PlotInfoScr[NumParams+6]:=' Diff Xsect parameters';
         DXS.PlotInfoScr[NumParams+7]:='  Angular momentum';
         DXS.PlotInfoScr[NumParams+8]:=
            ConCat('   ',NumStr(AngMomMin,2,0),
                   ' to ',NumStr(AngMomMax,2,0));
         DXS.PlotInfoScr[NumParams+9]:=
            ConCat('  Energy: ',Num2Str(V0*Energy,6),' ',Eunit,'eV');
         DXS.PlotInfoScr[NumParams+10]:=
            ConCat('  Mass: ',Num2Str(Mass,6),' ',Munit,'eV/c^2');
         DXS.PlotInfoScr[NumParams+11]:=
            ConCat('  Del theta: ',Num2Str(dTheta,6),' deg');
         DXS.PlotInfoScr[NumParams+12]:='';
         DXS.PlotInfoScr[NumParams+13]:=' Algorithm: ';
         CASE Alg OF
            0 :DXS.PlotInfoScr[NumParams+14]:='  piecewise constant';
            1 :DXS.PlotInfoScr[NumParams+14]:='  transfer matrix';
            2 :DXS.PlotInfoScr[NumParams+14]:='  direct integration';
            END; {case}
         FOR n:=NumParams+15 TO 25 DO DXS.PlotInfoScr[n]:=''
      END
END; {PrepDXSPlotScr}

PROCEDURE DXSLegend;
{Display legend for plot of wave function.}
BEGIN
   OpenViewPort(1);
   ClearViewPort;
   SelectScale(1);
   LegendEntry(3,LightMagenta,'new Xsect');
   LegendEntry(4,LightRed,'old Xsect');
   SetColor(Whitte)
END; {DXSPlotLegend}

PROCEDURE PrepDXSPlot(VAR DXS :DXStype; Potl :PotlType);
{Prepare viewport for wave function plot.}
VAR
   vp :integer;
BEGIN
   WITH DXS DO
      BEGIN
         IF Zoom THEN vp:=3
         ELSE
            BEGIN
               vp:=2;
               ClearOversizedViewPort(3);
               Info(Potl.InfoScr,Potl.NumLines,4);
               IF Computed THEN Info(PlotInfoScr,25,5)
               ELSE Info(Potl.PlotInfoScr,25,5);
               DXSLegend
            END;
         PrepViewPort(vp);
         CASE PlotType OF
            1 :SpecialAxis(5,ThetaMin,ThetaMax,6,VertMin,VertMax,10);
            2 :if VertMin<=0.0 THEN
                  SpecialAxis(5,ThetaMin,ThetaMax,6,
                                -5.0,Log10(VertMax),10)
               ELSE
                  SpecialAxis(5,ThetaMin,ThetaMax,6,
                                Log10(VertMin),Log10(VertMax),10);
            END; {case}
         PutLabel(Top,'Differential cross section');
         CASE PlotType OF
            1 :PutLabel(Left,ConCat('dSigma/dOmega (',
                                    Potl.Lunit,'m^2)'));
            2 :PutLabel(Left,ConCat('log[(dSigma/dOmega)/(',
                                    Potl.Lunit,'m^2)]'))
            END; {case}
         PutLabel(Bottom,'theta (deg)');
      END;
END; {PrepDXSPlot}

PROCEDURE PlotDXSvalues(VAR DXS :DXStype; L0 :real; nA :integer);
VAR
   DXSval1,DXSval2,L02 :real;
BEGIN
   WITH DXS DO
      BEGIN
         L02:=sqr(L0);
         IF Zoom THEN SelectViewPort(3) ELSE SelectViewPort(2);
         SelectScale(5);
         CASE PlotType OF
            1 :PlotLine(Angle.Value(nA-1),L02*DiffXS.Value(nA-1),
                        Angle.Value(nA),L02*DiffXS.Value(nA));
            2 :BEGIN
                  DXSval1:=L02*DiffXS.Value(nA-1);
                  IF DXSval1>0.0 THEN DXSval1:=Log10(DXSval1)
                  ELSE DXSval1:=GraphScales[5].yMin;
                  DXSval2:=L02*DiffXS.Value(nA);
                  IF DXSval2>0.0 THEN DXSval2:=Log10(DXSval2)
                  ELSE DXSval2:=GraphScales[5].yMin;
                  PlotLine(Angle.Value(nA-1),DXSval1,
                           Angle.Value(nA),DXSval2);
               END
            END {case PlotType}
      END
END; {PlotDXSvalues}

PROCEDURE AddDXSToPlot(VAR DXS :DXStype;
                       VAR Potl :PotlType; Color :word);
VAR
   nA :integer;
BEGIN
   WITH DXS DO
      BEGIN
         if Zoom then SelectViewPort(3)
         ELSE
            BEGIN
               SelectViewPort(2);
               Info(PlotInfoScr,25,5)
            END;
         SelectScale(3);
         SetColor(Color);
         FOR nA:=2 TO nAngles DO PlotDXSvalues(DXS,Potl.L0,nA);
         SetColor(Whitte);
      END;
END; {AddDXSToPlot}

PROCEDURE PlotDiffXSect(VAR DXS :DXStype; VAR Potl :PotlType);
BEGIN
   PrepDXSPlot(DXS,Potl);
   AddDXSToPlot(DXS,Potl,LightMagenta);
   DXS.Plotted:=True
END; {PlotDiffXSect}

PROCEDURE ComputeDiffXSect(VAR DXS :DXStype; VAR Potl :PotlType);
VAR
   n,AngMom            :integer;
   nAnglesMax          :longint;
   k,dsigma,Theta,
   PSval,uMax          :real;
   f                   :complex;
   PShift,Pl           :DVector;
   ComputeScr          :TInputScreen;
   Propeller           :TWaitMessage;
BEGIN
   {New(ComputeScr); {Mac only}
   WITH Potl,ComputeScr DO
      BEGIN
         Init;
         DefineInputPort(0.63,0.93,0.0,0.92);
         FOR n:=1 TO NumParams+5 DO LoadLine(ParamList[n]);
         LoadLine(       'Diff Xsect parameters');
         LoadLine(       ' Angular momentum');
         LoadLine(       '  min {   } max {   }');
         LoadLine(ConCat(' Energy: {      }',Eunit,'eV'));
         LoadLine(ConCat(' Mass: ',Num2Str(Mass,6),' ',Munit,'eV/c^2'));
         LoadLine(       ' Theta range (deg):');
         LoadLine(       '  {      } to {      }');
         LoadLine(       '  incr: {      } deg');
         LoadLine('');
         IF DXS.Plotted THEN LoadLine('     Clear plot? #F');
         LoadLine('');
         LoadLine(' [  OK  ]    [Cancel]');
         SetNumber(1,DXS.AngMomMin);
         SetNumber(2,DXS.AngMomMax);
         SetNumber(3,V0*DXS.Energy);
         SetNumber(4,DXS.ThetaMin);
         SetNumber(5,DXS.ThetaMax);
         SetNumber(6,DXS.dTheta);
         AcceptScreen;
         IF NOT Canceled THEN WITH DXS DO
            BEGIN
               IF Plotted AND NOT GetBoolean(7) THEN
                  AddDXSToPlot(DXS,Potl,LightRed);
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
               Energy:=Energy/V0;
               ThetaMin:=GetNumber(4);
               IF (ThetaMin<0.0) OR (ThetaMin>=180.0) THEN
                  RealInputWarn('theta_min',ThetaMin,0.0,6);
               ThetaMax:=GetNumber(5);
               IF (ThetaMax<=ThetaMin) OR (ThetaMax>180.0) THEN
                  RealInputWarn('theta_max',ThetaMax,180.0,6);
               dTheta:=GetNumber(6);
               IF dTheta<=0.0 THEN
                  RealInputWarn('delta theta',dTheta,6.0,6);
               nAnglesMax:=2+trunc((ThetaMax-ThetaMin)/dTheta);
               IF nAnglesMax>MaxRange THEN
                  BEGIN
                     RealInputWarn('delta theta',dTheta,
                                   (ThetaMax-ThetaMin)/(MaxRange-2),6);
                     nAnglesMax:=MaxRange;
                  END;
               IF Plotted AND GetBoolean(7) THEN
                  PrepDXSPlot(DXS,Potl);
               Angle.Free; DiffXS.Free;
               DiffXS.Init(nAnglesMax);
               Angle.Init(nAnglesMax);
               {New(PShift); {Mac only}
               PShift.Init(AngMomMax+1);
               {New(Propeller); {Mac only}
               Propeller.Show;
               FOR AngMom:=AngMomMin TO AngMomMax DO
                  BEGIN
                     Propeller.Update;
                     SolveSchrodinger(Energy,AngMom,Potl,uMax,PSval);
                     PShift.Put(AngMom+1,PSval)
                  END;
               k:=sqrt(Potl.zeta*Energy);
               nAngles:=0;
               Theta:=ThetaMin;
               {New(Pl); {Mac only}
               Pl.Init(AngMomMax+1);
               REPEAT
                  Propeller.Update;
                  nAngles:=nAngles+1;
                  Legendre(Pl,AngMomMax,cos(cPi*Theta/180.0));
                  f.Re:=0.0; f.Im:=0.0;
                  FOR AngMom:=AngMomMin TO AngMomMax DO
                     BEGIN
                        f.Re:=f.Re+(2*AngMom+1)*0.5
                                    *sin(2*PShift.Value(AngMom+1))
                                    *Pl.Value(AngMom+1);
                        f.Im:=f.Im+(2*AngMom+1)
                                    *sqr(sin(PShift.Value(AngMom+1)))
                                    *Pl.Value(AngMom+1);
                     END;
                  dsigma:=sqr(CABS(f)/k);
                  DiffXS.Put(nAngles,dsigma);
                  Angle.Put(nAngles,Theta);
                  IF nAngles>1 THEN
                     BEGIN
                        IF Min>dsigma THEN Min:=dsigma
                        ELSE IF Max<dsigma THEN Max:=dsigma;
                     END
                  ELSE
                     BEGIN
                        Min:=dsigma; Max:=dsigma;
                     END;
                  Theta:=Theta+dTheta;
               UNTIL (Theta>ThetaMax) OR (nAngles>=nAnglesMax);
               Propeller.Hide;
               {Dispose(Propeller); {Mac only}
               Pl.Free; PShift.Free;
               {Dispose(Pl); Dispose(PShift); {Mac only}
               PrepDXSPlotScr(DXS,Potl);
               Computed:=True; Potl.Saved:=False;
               IF Plotted THEN AddDXSToPlot(DXS,Potl,LightMagenta)
               ELSE PlotDiffXSect(DXS,Potl);
            END;
         Done;
      END;
   {Dispose(ComputeScr); {Mac only}
END; {ComputeDiffXSect}

PROCEDURE ChangeDXSPlotScales(VAR DXS :DXStype; VAR Potl :PotlType);
VAR
   Changed,AutoSet :boolean;
BEGIN
   WITH DXS DO
      BEGIN
         AutoSet:=True;
         ChangePlotScales(ThetaMin,ThetaMax,'(deg)',0.0,ThetaMax-ThetaMin,
                          VertMin,VertMax,ConCat('(',Potl.Lunit,'m^2)'),
                          -1.0,Potl.DXSscale*sqr(Potl.L0*Potl.rMax),
                          Changed,AutoSet);
         IF AutoSet THEN
            BEGIN
               IF Min<Max then
                  BEGIN
                     VertMin:=sqr(Potl.L0)*Min;
                     VertMax:=sqr(Potl.L0)*Max;
                  END
               ELSE IF Max>0.0 THEN
                  BEGIN
                     VertMin:=0.0;
                     VertMax:=sqr(Potl.L0)*Max;
                  END;
            END;
         IF Changed THEN PlotDiffXSect(DXS,Potl);
      END;
END; {ChangeDXSPlotScales}

PROCEDURE ComputeDXS(VAR DXS :DXStype; VAR Potl :PotlType;
                     VAR MainMenu :TMenu; VAR MenuClicked :boolean);
{Compute and plot differential cross section.}
VAR
   DXSHelpScr :HelpScrType;
   DXSkeys    :THotKeys;
   Wkey       :byte;
   n          :integer;
   WishToReturn,SaveZoom :boolean;
BEGIN
   DXSHelpScr[01]:='';
   DXSHelpScr[02]:=' <F1>-show this help screen.';
   DXSHelpScr[03]:='';
   DXSHelpScr[04]:=' <F2>-compute differential cross section';
   DXSHelpScr[05]:='    with new choice of partial waves or';
   DXSHelpScr[06]:='    energy, or with new plot range.';
   DXSHelpScr[07]:='    The input screen will include an option';
   DXSHelpScr[08]:='    to erase previous results, if any.';
   DXSHelpScr[09]:='';
   DXSHelpScr[10]:=' <F3>-toggle size of plot.  When small, text';
   DXSHelpScr[11]:='    information and a legend are shown.';
   DXSHelpScr[12]:='';
   DXSHelpScr[13]:=' <F4>-change horizontal and vertical scales';
   DXSHelpScr[14]:='    of plot.';
   DXSHelpScr[15]:='';
   DXSHelpScr[16]:=' <F5>-toggle between linear and semilog plot.';
   DXSHelpScr[17]:='';
   DXSHelpScr[18]:=' <F10>-activate menu.';
   FOR n:=19 TO 25 DO DXSHelpScr[n]:='';

   WITH DXS DO
      BEGIN
         IF Computed THEN PlotDiffXSect(DXS,Potl)
         ELSE
            BEGIN
               Zoom:=False; Plotted:=False;
               ThetaMin:=0.0; ThetaMax:=180.0;
               VertMin:=0.0; VertMax:=Potl.DXSscale*sqr(Potl.L0*Potl.rMax);
               SaveZoom:=Potl.Zoom; Potl.Zoom:=False;
               PlotPotential(Potl);
               Potl.Zoom:=SaveZoom;
            END;
         ComputeDiffXSect(DXS,Potl);
         IF Computed THEN
            BEGIN
               {New(DXSkeys); {Mac only}
               WITH DXSkeys DO
                  BEGIN
                     Init(6);
                     key[1]:='F1-Help';
                     key[2]:='F2-Input';
                     IF Zoom THEN key[3]:='F3-Contract'
                     ELSE key[3]:='F3-Expand';
                     key[4]:='F4-Scale';
                     IF PlotType=1 THEN key[5]:='F5-Semilog'
                     ELSE key[5]:='F5-Linear';
                     key[6]:='F10-Menu';
                     Display
                  END;
               WishToReturn:=False;
               WHILE (NOT WishToReturn) AND (NOT MenuClicked) DO
                  BEGIN
                     CheckForEvents;
                     IF Event.ReadKey=Escape THEN WishToReturn:=True
                     ELSE IF DXSkeys.Pressed(Wkey) THEN
                        BEGIN
                           DXSkeys.Clear;
                           case Wkey of
                              1 :Help(DXSHelpScr);
                              2 :ComputeDiffXSect(DXS,Potl);
                              3 :BEGIN
                                    Zoom:=not Zoom;
                                    PlotDiffXSect(DXS,Potl);
                                    IF Zoom THEN
                                       DXSkeys.key[3]:='F3-Contract'
                                    ELSE DXSkeys.key[3]:='F3-Expand'
                                 END;
                              4 :ChangeDXSPlotScales(DXS,Potl);
                              5 :BEGIN
                                    PlotType:=(PlotType MOD 2)+1;
                                    PlotDiffXSect(DXS,Potl);
                                    IF PlotType=1 THEN
                                       DXSkeys.key[5]:='F5-Semilog'
                                    ELSE DXSkeys.key[5]:='F5-Linear';
                                 END;
                              6 :BEGIN
                                    WishToReturn:=True;
                                    IF MainMenu.Chosen THEN
                                       MenuClicked:=True;
                                 END;
                              END; {case}
                           DXSkeys.Display;
                        END
                     ELSE IF MainMenu.Activated THEN MenuClicked:=True;
                  END;
               DXSkeys.Clear;
               {Dispose(DXSkeys); {Mac only}
            END;
      END;
END; {ComputeDXS}

{--------Routines for total cross sections----------------------------}

PROCEDURE SetTXSDefaults(VAR TXS :TXStype; VAR Potl :PotlType);
BEGIN
   WITH TXS DO
      BEGIN
         Emin:=0.1; Emax:=2.0; deltaE:=0.02;
         AngMomMin:=0; AngMomMax:=2;
         PlotType:=1;      {linear}
         Computed:=False;
         Plotted:=False;
         Zoom:=False
      END
END; {SetTXSDefaults}

PROCEDURE PrepTXSPlotScr(VAR TXS :TXStype; VAR Potl :PotlType);
{Prepare Info screen to go with plot of total cross section.}
VAR
   n,i :integer;
BEGIN
   WITH TXS,Potl DO
      BEGIN
         FOR n:=1 TO NumParams+5 DO TXS.PlotInfoScr[n]:=ParamList[n];
         TXS.PlotInfoScr[NumParams+6]:=' Total Xsect parameters';
         TXS.PlotInfoScr[NumParams+7]:='  Angular momentum';
         TXS.PlotInfoScr[NumParams+8]:=
            ConCat('   ',NumStr(AngMomMin,2,0),
                   ' to ',NumStr(AngMomMax,2,0));
         TXS.PlotInfoScr[NumParams+9]:=
            ConCat('  Energy range (',Eunit,'eV)');
         TXS.PlotInfoScr[NumParams+10]:=
            ConCat('   ',Num2Str(V0*Emin,6),' to ',Num2Str(V0*Emax,6));
         TXS.PlotInfoScr[NumParams+11]:=
            ConCat('  delta E: ',Num2Str(V0*deltaE,6),' ',Eunit,'eV');
         TXS.PlotInfoScr[NumParams+12]:=
            ConCat('  Mass: ',Num2Str(Mass,6),' ',Munit,'eV/c^2');
         TXS.PlotInfoScr[NumParams+13]:='';
         TXS.PlotInfoScr[NumParams+14]:=' Algorithm: ';
         CASE Alg OF
            0 :TXS.PlotInfoScr[NumParams+15]:='  piecewise constant';
            1 :TXS.PlotInfoScr[NumParams+15]:='  transfer matrix';
            2 :TXS.PlotInfoScr[NumParams+15]:='  direct integration';
            END; {case}
         FOR n:=NumParams+16 TO 25 DO TXS.PlotInfoScr[n]:=''
      END
END; {PrepTXSPlotScr}

PROCEDURE TXSLegend;
{Display legend for plot of total cross section.}
BEGIN
   OpenViewPort(1);
   ClearViewPort;
   SelectScale(1);
   LegendEntry(3,LightCyan,'new Xsect');
   LegendEntry(4,LightRed,'old Xsect');
   SetColor(Whitte)
END; {TXSLegend}

PROCEDURE PrepTXSPlot(VAR TXS :TXStype; VAR Potl :PotlType);
{Prepare viewport for plot of total cross section.}
VAR
   vp :integer;
BEGIN
   WITH TXS DO
      BEGIN
         IF Zoom THEN vp:=3
         ELSE
            BEGIN
               vp:=2;
               ClearOversizedViewPort(3);
               IF Computed THEN Info(PlotInfoScr,25,5)
               ELSE Info(Potl.PlotInfoScr,25,5);
               Info(Potl.InfoScr,Potl.NumLines,4);
               TXSLegend;
            END;
         PrepViewPort(vp);
         CASE PlotType OF
            1 :SpecialAxis(6,Potl.V0*Emin,Potl.V0*Emax,5,
                             VertMin,VertMax,10);
            2 :if VertMin<=0.0 THEN
                  SpecialAxis(6,Potl.V0*Emin,Potl.V0*Emax,5,
                                -5.0,Log10(VertMax),10)
               ELSE
                  SpecialAxis(6,Potl.V0*Emin,Potl.V0*Emax,6,
                                Log10(VertMin),Log10(VertMax),10);
            END; {case}
         PutLabel(Top,'Total cross section');
         CASE PlotType OF
            1 :PutLabel(Left,ConCat('Sigma (',Potl.Lunit,'m^2)'));
            2 :PutLabel(Left,ConCat('log[Sigma/(',Potl.Lunit,'m^2)]'))
            END; {case}
         PutLabel(Bottom,ConCat('Energy (',Potl.Eunit,'eV)'));
      END;
END; {PrepTXSPlot}

PROCEDURE PlotTXSvalues(VAR TXS :TXStype; V0,L0 :real; nE :integer);
{Plot pair of cross section values.}
VAR
   E1,E2,TXSval1,TXSval2,L02 :real;
BEGIN
   WITH TXS DO
      BEGIN
         L02:=sqr(L0);
         IF Zoom THEN SelectViewPort(3) ELSE SelectViewPort(2);
         SelectScale(6);
         E1:=V0*Energy.Value(nE-1); E2:=V0*Energy.Value(nE);
         TXSval1:=L02*TotalXS.Value(nE-1); TXSval2:=L02*TotalXS.Value(nE);
         CASE PlotType OF
            1 :PlotLine(E1,TXSval1,E2,TXSval2);
            2 :BEGIN
                  IF TXSval1>0.0 THEN TXSval1:=Log10(TXSval1)
                  ELSE TXSval1:=GraphScales[6].yMin;
                  IF TXSval2>0.0 THEN TXSval2:=Log10(TXSval2)
                  ELSE TXSval2:=GraphScales[6].yMin;
                  PlotLine(E1,TXSval1,E2,TXSval2);
               END
            END {case PlotType}
      END
END; {PlotTXSvalues}

PROCEDURE ReplotOldTXS(VAR TXS :TXStype; VAR Potl :PotlType);
VAR
   nE :integer;
BEGIN
   WITH TXS DO
      BEGIN
         SetColor(LightRed);
         FOR nE:=2 TO NumE DO PlotTXSvalues(TXS,Potl.V0,Potl.L0,nE);
         SetColor(Whitte);
      END;
END; {ReplotOldTXS}

PROCEDURE PlotTotalXSect(VAR TXS :TXStype; VAR Potl :PotlType);
VAR
   nE :integer;
BEGIN
   WITH TXS DO
      BEGIN
          PrepTXSPlot(TXS,Potl);
          SetColor(LightCyan);
          FOR nE:=2 TO NumE DO PlotTXSvalues(TXS,Potl.V0,Potl.L0,nE);
          Plotted:=True;
          SetColor(Whitte);
      END;
END; {PlotTotalXSect}

PROCEDURE ComputeTotalXSect(VAR TXS :TXStype; VAR Potl :PotlType);
{Compute total cross section for range of energy and add to plot}
{of previous results.}
VAR
   TXSHelpScr     :HelpScrType;
   CalcIncomplete :string;
   TXSkeys        :THotKeys;
   Tkey           :byte;
   AngMom,n,nE    :integer;
   WishToAbort    :boolean;
   E,Elarge,Etemp,
   deltaEnew,
   sigma,uMax,
   PhaseShift     :real;
   ComputeScr     :TInputScreen;
   Propeller      :TWaitMessage;
BEGIN
   TXSHelpScr[01]:='';
   TXSHelpScr[02]:=' <F1>-show this help screen.';
   TXSHelpScr[03]:='';
   TXSHelpScr[04]:=' <F2>-stop the calculation.';
   TXSHelpScr[05]:='';
   TXSHelpScr[06]:=' <F5>-halve the energy step deltaE.';
   TXSHelpScr[07]:='';
   TXSHelpScr[08]:=' <F6>-double the energy step deltaE.';
   for n:=9 to 25 do TXSHelpScr[n]:='';
   CalcIncomplete:=
      'Calculation incomplete due to limit on storage of energy values.';

   {New(ComputeScr); {Mac only}
   WITH Potl,ComputeScr DO
      BEGIN
         Elarge:=V0*sqr(2*cPi/(3*dr))/zeta;
         nE:=round((PS.Emax-PS.Emin)/PS.deltaE);
         Init;
         DefineInputPort(0.63,0.93,0.0,0.92);
         FOR n:=1 TO NumParams+5 DO LoadLine(ParamList[n]);
         LoadLine(       'Total Xsect parameters');
         LoadLine(       ' Angular momentum');
         LoadLine(       '  min {   } max {   }');
         LoadLine(ConCat(' Energy range: (',Eunit,'eV) '));
         LoadLine(       '  {      } to {      }');
         LoadLine(ConCat(' delta E: {      }',Eunit,'eV'));
         LoadLine(ConCat(' Mass: ',Num2Str(Mass,6),' ',Munit,'eV/c^2'));
         LoadLine('');
         IF TXS.Plotted THEN LoadLine('    Clear plot? #F');
         LoadLine('');
         LoadLine(' [  OK  ]    [Cancel]');
         SetNumber(1,TXS.AngMomMin);
         SetNumber(2,TXS.AngMomMax);
         SetNumber(3,V0*TXS.Emin);
         IF V0*TXS.Emax>Elarge THEN
            BEGIN
               SetNumber(4,Elarge);
               deltaEnew:=(Elarge-V0*TXS.Emin)/nE;
            END
         ELSE
            BEGIN
               SetNumber(4,V0*TXS.Emax);
               deltaEnew:=V0*TXS.deltaE;
            END;
         SetNumber(5,deltaEnew);
         AcceptScreen;
         IF NOT Canceled THEN WITH TXS DO
            BEGIN
               IF Plotted AND NOT GetBoolean(6) THEN ReplotOldTXS(TXS,Potl);
               AngMomMin:=round(GetNumber(1));
               IF (AngMomMin<0) OR (AngMomMin>MaxRange) THEN
                  IntInputWarn('minimum angular momentun',AngMomMin,0,2);
               AngMomMax:=round(GetNumber(2));
               IF (AngMomMax<AngMomMin) OR (AngMomMax>MaxRange) THEN
                  IntInputWarn('maximum angular momentun',
                               AngMomMax,AngMomMin,2);
               Emin:=GetNumber(3); Emax:=GetNumber(4);
               IF Emin<=0.0 THEN
                  RealInputWarn('E_min',Emin,1.0e-7,6)
               ELSE IF Emin>Elarge THEN
                  BEGIN
                     IF Emax<=Elarge THEN
                        Announce(ConCat('Wave number too large for numerical',
                                        ' calculation to proceed.'));
                     RealInputWarn('E_min',Emin,Elarge/2,6);
                  END;
               IF Emax>Elarge THEN
                  BEGIN
                     Announce(ConCat('Wave number too large for numerical ',
                                     'calculation to proceed.'));
                     RealInputWarn('E_max',Emax,Elarge,6);
                  END
               ELSE IF Emax<=Emin THEN
                  BEGIN
                     Etemp:=Emin+2.0*V0;
                     IF Etemp>Elarge THEN Etemp:=Elarge;
                     RealInputWarn('E_max',Emax,Etemp,6);
                  END;
               deltaE:=GetNumber(5);
               IF (deltaE<=0.0) OR (Emin+deltaE>Elarge) THEN
                  RealInputWarn('delta E',deltaE,(Emax-Emin)/nE,7)
               ELSE IF (Emax-Emin)/deltaE>MaxNumE-2 THEN
                  RealInputWarn('delta E',deltaE,(Emax-Emin)/(MaxNumE-2),7);
               Emin:=Emin/V0; Emax:=Emax/V0;
               deltaE:=deltaE/V0;
               PrepTXSPlotScr(TXS,Potl);
               IF NOT Plotted THEN PrepTXSPlot(TXS,Potl)
               ELSE IF GetBoolean(6) THEN PrepTXSPlot(TXS,Potl)
               ELSE IF NOT Zoom THEN Info(PlotInfoScr,25,5);
               {New(TXSkeys); {Mac only}
               WITH TXSkeys DO
                  BEGIN
                     Init(4);
                     key[1]:='F1-Help';
                     key[2]:='F2-Stop';
                     key[3]:='F5-Halve';
                     key[4]:='F6-Double';
                     Display;
                  END;
               E:=Emin; NumE:=0;
               WishToAbort:=False;
               IF Zoom THEN SelectViewPort(3) ELSE SelectViewPort(2);
               Print(25,2,ConCat('E:          ',Potl.Eunit,'eV'));
               Print(25,3,'delta E: ');
               {New(Propeller); {Mac only}
               WHILE (E<=Emax) AND (NumE<MaxNumE) AND (NOT WishToAbort) DO
                  BEGIN
                     NumE:=NumE+1;
                     Energy.Put(NumE,E);
                     RubOut(28,2,8,DarkGray); RubOut(34,3,8,DarkGray);
                     SetColor(Whitte);
                     Print(28,2,Num2Str(Potl.V0*E,8));
                     Print(34,3,Num2Str(Potl.V0*deltaE,8));
                     sigma:=0.0;
                     Propeller.Show;
                     FOR AngMom:=AngMomMin TO AngMomMax DO
                        BEGIN
                           Propeller.Update;
                           SolveSchrodinger(E,AngMom,Potl,uMax,PhaseShift);
                           sigma:=sigma+(2*AngMom+1)*sqr(sin(PhaseShift))
                        END;
                     Propeller.Hide;
                     sigma:=4*cPi*sigma/(zeta*E);
                     TotalXS.Put(NumE,sigma);
                     IF NumE>1 THEN
                        BEGIN
                           IF Min>sigma THEN Min:=sigma
                           ELSE IF Max<sigma THEN Max:=sigma;
                           SetColor(LightCyan);
                           PlotTXSvalues(TXS,Potl.V0,Potl.L0,NumE);
                        END
                     ELSE
                        BEGIN
                           Min:=sigma; Max:=sigma;
                        END;
                     CheckForEvents;
                     IF Event.ReadKey=Escape THEN WishToAbort:=True
                     ELSE IF TXSkeys.Pressed(Tkey) THEN
                        CASE Tkey OF
                           1 :Help(TXSHelpScr);
                           2 :WishToAbort:=True;
                           3 :deltaE:=deltaE/2;
                           4 :deltaE:=2*deltaE;
                           END; {case Tkey}
                     E:=E+deltaE;
                  END; {while E<=Emax}
               {Dispose(Propeller); {Mac only}
               IF NumE>0 THEN 
                  BEGIN
                     IF (NumE=MaxNumE) AND (E<=Emax) THEN
                        BEGIN
                           Announce(CalcIncomplete);
                           Emax:=E-deltaE;
                        END;
                     Computed:=True; Plotted:=True; Potl.Saved:=False;
                     PrepTXSPlotScr(TXS,Potl);
                     IF NOT Zoom THEN Info(PlotInfoScr,25,5);
                  END
               ELSE Computed:=False;
               TXSkeys.Clear;
               {Dispose(TXSkeys); {Mac only}
            END;
         Done;
      END;
   {Dispose(ComputeScr); {Mac only}
END; {ComputeTotalXSect}

PROCEDURE ChangeTXSPlotScales(VAR TXS :TXStype; VAR Potl :PotlType);
VAR
   Changed,AutoSet :boolean;
BEGIN
   WITH TXS DO
      BEGIN
         AutoSet:=True;
         Emin:=Potl.V0*Emin; Emax:=Potl.V0*Emax;
         ChangePlotScales(Emin,Emax,ConCat('(',Potl.Eunit,'eV)'),
                          -1.0,Potl.V0*(Emin+(NumE-1)*deltaE),
                          VertMin,VertMax,ConCat('(',Potl.Lunit,'m^2)'),
                          -1.0,Potl.TXSscale*cPi*sqr(Potl.L0*Potl.rMax),
                          Changed,AutoSet);
         Emin:=Emin/Potl.V0; Emax:=Emax/Potl.V0;
         IF AutoSet THEN
            BEGIN
               IF Min<Max then
                  BEGIN
                     VertMin:=sqr(Potl.L0)*Min;
                     VertMax:=sqr(Potl.L0)*Max;
                  END
               ELSE IF Max>0.0 THEN
                  BEGIN
                     VertMin:=0.0;
                     VertMax:=sqr(Potl.L0)*Max;
                  END;
            END;
         IF Changed THEN
            BEGIN
               PrepTXSPlotScr(TXS,Potl);
               PlotTotalXSect(TXS,Potl)
            END;
      END;
END; {ChangeTXSPlotScales}

PROCEDURE ComputeTXS(VAR TXS :TXStype; VAR Potl :PotlType;
                     VAR MainMenu :TMenu; VAR MenuClicked :boolean);
{Compute and plot total cross section.}
VAR
   TXSHelpScr :HelpScrType;
   TXSkeys    :THotKeys;
   Tkey       :byte;
   n          :integer;
   WishToReturn,SaveZoom :boolean;
BEGIN
   TXSHelpScr[01]:='';
   TXSHelpScr[02]:=' <F1>-show this help screen.';
   TXSHelpScr[03]:='';
   TXSHelpScr[04]:=' <F2>-compute total cross section with new';
   TXSHelpScr[05]:='    choice of partial waves or energy step,';
   TXSHelpScr[06]:='    or with new plot range.  The input';
   TXSHelpScr[07]:='    screen will include an option to erase';
   TXSHelpScr[08]:='    previous results, if any.';
   TXSHelpScr[09]:='';
   TXSHelpScr[10]:=' <F3>-toggle size of plot.  When small, text';
   TXSHelpScr[11]:='    information and a legend are shown.';
   TXSHelpScr[12]:='';
   TXSHelpScr[13]:=' <F4>-change horizontal and vertical scales';
   TXSHelpScr[14]:='    of plot.';
   TXSHelpScr[15]:='';
   TXSHelpScr[16]:=' <F5>-toggle between linear and semilog plot.';
   TXSHelpScr[17]:='';
   TXSHelpScr[18]:=' <F10>-activate menu.';
   FOR n:=19 TO 25 DO TXSHelpScr[n]:='';

   WITH TXS DO
      BEGIN
         IF Computed THEN PlotTotalXSect(TXS,Potl)
         ELSE
            BEGIN
               Zoom:=False; Plotted:=False;
               VertMin:=0.0;
               VertMax:=Potl.TXSscale*4*cPi*sqr(Potl.L0*Potl.rMax);
               SaveZoom:=Potl.Zoom; Potl.Zoom:=False;
               PlotPotential(Potl);
               Potl.Zoom:=SaveZoom;
            END;
         ComputeTotalXSect(TXS,Potl);
         IF Computed THEN
            BEGIN
               {New(TXSkeys); {Mac only}
               WITH TXSkeys DO
                  BEGIN
                     Init(6);
                     key[1]:='F1-Help';
                     key[2]:='F2-Input';
                     IF Zoom THEN key[3]:='F3-Contract'
                     ELSE key[3]:='F3-Expand';
                     key[4]:='F4-Scale';
                     IF PlotType=1 THEN key[5]:='F5-Semilog'
                     ELSE key[5]:='F5-Linear';
                     key[6]:='F10-Menu';
                     Display
                  END;
               WishToReturn:=False;
               WHILE (NOT WishToReturn) AND (NOT MenuClicked) DO
                  BEGIN
                     CheckForEvents;
                     IF Event.ReadKey=Escape THEN WishToReturn:=True
                     ELSE IF TXSkeys.Pressed(Tkey) THEN
                        BEGIN
                           TXSkeys.Clear;
                           case Tkey of
                              1 :Help(TXSHelpScr);
                              2 :ComputeTotalXSect(TXS,Potl);
                              3 :BEGIN
                                    Zoom:=not Zoom;
                                    PlotTotalXSect(TXS,Potl);
                                    IF Zoom THEN
                                       TXSkeys.key[3]:='F3-Contract'
                                    ELSE TXSkeys.key[3]:='F3-Expand';
                                 END;
                              4 :ChangeTXSPlotScales(TXS,Potl);
                              5 :BEGIN
                                    PlotType:=(PlotType MOD 2)+1;
                                    PlotTotalXSect(TXS,Potl);
                                    IF PlotType=1 THEN
                                       TXSkeys.key[5]:='F5-Semilog'
                                    ELSE TXSkeys.key[5]:='F5-Linear';
                                 END;
                              6 :BEGIN
                                    WishToReturn:=True;
                                    IF MainMenu.Chosen THEN
                                       MenuClicked:=True;
                                 END;
                              END; {case}
                           TXSkeys.Display;
                        END
                     ELSE IF MainMenu.Activated THEN MenuClicked:=True;
                  END;
               TXSkeys.Clear;
               {Dispose(TXSkeys); {Mac only}
            END;
      END;
END; {ComputeTXS}

END. {Sc3DPDT}


