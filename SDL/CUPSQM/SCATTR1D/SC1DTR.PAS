{Unit of routines for 1-D transmission and reflection probabilities.  }
{Used by Scattr1D.pas.                                                }
{                                                                     }
{Author: John R. Hiller                                               }
{Version: 1.0                11/15/94                                 }
{Copyright (c) 1995 John Wiley & Sons, Inc.                           }
{                                                                     }
UNIT Sc1DTR;

INTERFACE

USES Graph,CUPS,CUPSmupp,CUPSfunc,CUPSproc,CUPSgui,
     Sc1DMach,Sc1DCmn,Sc1DPotl,Sc1DWF;

CONST
   MaxNumE=502;    {maximum allowed number of energy values}

TYPE
   TRtype=record
      T,
      LogT,
      Energy        :DVector;
      Emin,
      Emax,
      deltaE,
      VertMin,VertMax,           {vertical range for plot}
      LogTRmin      :real;       {minimum value for plot scale}
      NumE,
      PlotType      :integer;
      Zoom,
      Computed      :boolean;
      PlotInfoScr   :InfoScrType
      END; {TRtype}

VAR
   TR            :TRtype;

PROCEDURE SetTRDefaults(VAR TR :TRtype; VAR Potl :PotlType);
PROCEDURE PrepTRPlotScr(VAR TR :TRtype; VAR Potl :PotlType);
PROCEDURE PlotTransRefl(VAR TR :TRtype; VAR Potl :PotlType);
PROCEDURE ComputeTR(VAR TR :TRtype; VAR Potl :PotlType;
                    VAR MainMenu :TMenu; VAR MenuClicked :boolean);

IMPLEMENTATION

{--------Routines for trans/refl probability computations and plots----}

PROCEDURE SetTRDefaults(VAR TR :TRtype; VAR Potl :PotlType);
BEGIN
   WITH TR DO
      BEGIN
         Emin:=0.95; Emax:=1.45; deltaE:=0.005;
         PlotType:=1;          {linear}
         Computed:=False;
         Zoom:=False
      END
END; {SetTRDefaults}

PROCEDURE PrepTRPlotScr(VAR TR :TRtype; VAR Potl :PotlType);
{Prepare Info screen to go with plot of transmission/reflection}
{probabilities.}
VAR
   n :integer;
BEGIN
   WITH TR,Potl DO
      BEGIN
         FOR n:=1 TO NumParams+7 DO TR.PlotInfoScr[n]:=ParamList[n];
         TR.PlotInfoScr[NumParams+8]:= ' T&R parameters';
         TR.PlotInfoScr[NumParams+9]:=ConCat('  Energy range (',Eunit,'eV)');
         TR.PlotInfoScr[NumParams+10]:=ConCat('   ',Num2Str(V0*Emin,6),
                                              ' to ',Num2Str(V0*Emax,6));
         TR.PlotInfoScr[NumParams+11]:=ConCat('  Delta E: ',
                                              Num2Str(V0*deltaE,6),
                                              ' ',Eunit,'eV');
         TR.PlotInfoScr[NumParams+12]:='';
         TR.PlotInfoScr[NumParams+13]:=' Algorithm: ';
         CASE Alg OF
            0 :TR.PlotInfoScr[NumParams+14]:='  piecewise constant';
            1 :TR.PlotInfoScr[NumParams+14]:='  transfer matrix';
            2 :TR.PlotInfoScr[NumParams+14]:='  direct integration';
            3 :TR.PlotInfoScr[NumParams+14]:='  integration of log';
            END; {case}
         FOR n:=NumParams+15 TO 25 DO TR.PlotInfoScr[n]:='';
      END
END; {PrepTRPlotScr}

PROCEDURE TRLegend;
{Display legend for plot of phase shifts.}
BEGIN
   OpenViewPort(1);
   ClearViewPort;
   SelectScale(1);
   LegendEntry(3,LightCyan,'transmission');
   LegendEntry(4,LightRed,'reflection');
   SetColor(Whitte)
END; {TRLegend}

PROCEDURE PrepTRPlot(VAR TR :TRtype; VAR Potl :PotlType);
{Prepare to plot transmission/reflection probabilities.}
VAR
   vp :integer;
BEGIN
   WITH TR DO
      BEGIN
         IF Zoom THEN vp:=3
         ELSE
            BEGIN
               vp:=2;
               ClearOversizedViewPort(3);
               IF Computed THEN Info(PlotInfoScr,25,5)
               ELSE Info(Potl.PlotInfoScr,25,5);
               Info(Potl.InfoScr,Potl.NumLines,4);
               TRLegend
            END;
         PrepViewPort(vp);
         CASE PlotType OF
            1 :SpecialAxis(4,Potl.V0*Emin,Potl.V0*Emax,5,
                           VertMin,VertMax,10);
            2 :IF VertMin<=0.0 THEN
                  SpecialAxis(4,Potl.V0*Emin,Potl.V0*Emax,5,
                              LogTRmin,Log10(VertMax),10)
               ELSE
                  SpecialAxis(4,Potl.V0*Emin,Potl.V0*Emax,5,
                              Log10(VertMin),Log10(VertMax),10)
               END; {case}
         PutLabel(Top,'Transmission & reflection probabilities');
         CASE PlotType OF
            1 :PutLabel(Left,'T, R');
            2 :PutLabel(Left,'log(T), log(R)')
            END; {case}
         PutLabel(Bottom,ConCat('Energy (',Potl.Eunit,'eV)'));
      END;
END; {PrepTRPlot}

PROCEDURE PrepTRPlotValues(VAR LogTval,LogRval :real);
BEGIN
   IF LogTval>0.5 THEN {value of 1.0 is flag for -infinity}
      BEGIN
         LogTval:=GraphScales[4].Ymin; LogRval:=0.0
      END
   ELSE IF LogTval>=0.0 THEN
      BEGIN
         LogTval:=0.0; LogRval:=GraphScales[4].Ymin
      END
   ELSE LogRval:=Log10(1-Pwr(10.0,LogTval))
END; {PrepTRPlotValues}

PROCEDURE PlotTRvalues(VAR TR :TRtype; nE :integer; V0 :real);
VAR
   LogTval1,LogTval2,LogRval1,LogRval2 :real;
BEGIN
   WITH TR DO
      BEGIN
         IF Zoom THEN SelectViewPort(3) ELSE SelectViewPort(2);
         SelectScale(4);
         CASE PlotType OF
            1 :BEGIN
                  SetColor(LightCyan);
                  PlotLine(V0*Energy.Value(nE-1),T.Value(nE-1),
                           V0*Energy.Value(nE),T.Value(nE));
                  SetColor(LightRed);
                  PlotLine(V0*Energy.Value(nE-1),1-T.Value(nE-1),
                           V0*Energy.Value(nE),1-T.Value(nE))
               END;
            2 :BEGIN
                  LogTval1:=LogT.Value(nE-1);
                  PrepTRPlotValues(LogTval1,LogRval1);
                  LogTval2:=LogT.Value(nE);
                  PrepTRPlotValues(LogTval2,LogRval2);
                  SetColor(LightCyan);
                  PlotLine(V0*Energy.Value(nE-1),LogTval1,
                           V0*Energy.Value(nE),LogTval2);
                  SetColor(LightRed);
                  PlotLine(V0*Energy.Value(nE-1),LogRval1,
                           V0*Energy.Value(nE),LogRval2)
               END
            END {case PlotType}
      END
END; {PlotTRvalues}

PROCEDURE PlotTransRefl(VAR TR :TRtype; VAR Potl :PotlType);
VAR
   nE :integer;
BEGIN
   PrepTRPlot(TR,Potl);
   FOR nE:=2 TO TR.NumE DO PlotTRvalues(TR,nE,Potl.V0);
   SetColor(Whitte);
END; {PlotTransRefl}

PROCEDURE ComputeTransRefl(VAR TR :TRtype; VAR Potl :PotlType);
VAR
   TRHelpScr   :HelpScrType;
   TRkeys      :THotKeys;
   Tkey        :byte;
   n,nE        :integer;
   Failed,
   WishToAbort :boolean;
   E,Etemp,deltaEnew,RAmp,RPhase,
   TAmp,TPhase,LogRval,LogTval :real;
   Mag,Phase   :DVector;
   ComputeScr  :TInputScreen;
BEGIN
   TRHelpScr[01]:='';
   TRHelpScr[02]:='';
   TRHelpScr[03]:=' <F1>-show this help screen.';
   TRHelpScr[04]:='';
   TRHelpScr[05]:=' <F2>-stop the calculation.';
   TRHelpScr[06]:='';
   TRHelpScr[07]:=' <F5>-halve the energy step deltaE.';
   TRHelpScr[08]:='';
   TRHelpScr[09]:=' <F6>-double the energy step deltaE.';
   for n:=10 to 25 do TRHelpScr[n]:='';

   {New(ComputeScr); {Mac only}
   WITH Potl,TR,ComputeScr DO
      BEGIN
         Init;
         DefineInputPort(0.63,0.93,0.0,0.9);
         FOR n:=1 TO NumParams+7 DO LoadLine(ParamList[n]);
         LoadLine(       ' T&R parameters');
         LoadLine(ConCat('  Energy range (',Eunit,'eV)'));
         LoadLine(       '   {      } to {      }');
         LoadLine(ConCat('   in (',Num2Str(Esmall,6),', ',
                         Num2Str(Elarge,6),')'));
         LoadLine('');
         LoadLine(ConCat('  Delta E: {      }',Eunit,'eV'));
         LoadLine('');
         LoadLine(' [  OK  ]    [Cancel]');
         nE:=round((Emax-Emin)/deltaE);
         IF (V0*Emin<Esmall) OR (V0*Emin>=Elarge) THEN
            BEGIN
               SetNumber(1,Esmall);
               deltaEnew:=Esmall;
            END
         ELSE
            BEGIN
               SetNumber(1,V0*Emin);
               deltaEnew:=V0*Emin;
            END;
         IF (V0*Emax>Elarge) OR (V0*Emax<=Esmall) THEN
            BEGIN
               SetNumber(2,Elarge);
               deltaEnew:=(Elarge-deltaEnew)/nE;
            END
         ELSE
            BEGIN
               SetNumber(2,V0*Emax);
               deltaEnew:=(V0*Emax-deltaEnew)/nE;
            END;
         SetNumber(3,deltaEnew);
         AcceptScreen;
         IF NOT Canceled THEN
            BEGIN
               Emin:=GetNumber(1);
               IF Emin<Esmall THEN
                  RealInputWarn('E_min',Emin,Esmall,6)
               ELSE IF Emin>Elarge THEN
                  RealInputWarn('E_min',Emin,(Elarge+Esmall)/2,6);
               Emax:=GetNumber(2);
               IF Emax>Elarge THEN
                  RealInputWarn('E_max',Emax,Elarge,6)
               ELSE IF Emax<=Emin THEN
                  BEGIN
                     Etemp:=Emin+2.0*V0;
                     IF Etemp>Elarge THEN Etemp:=Elarge;
                     RealInputWarn('E_max',Emax,Etemp,6);
                  END;
               deltaE:=GetNumber(3);
               IF (deltaE<=0.0) OR (Emin+deltaE>Elarge) THEN
                  RealInputWarn('delta E',deltaE,(Emax-Emin)/nE,7)
               ELSE IF (Emax-Emin)/deltaE>MaxNumE-2 THEN
                  RealInputWarn('delta E',deltaE,(Emax-Emin)/(MaxNumE-2),7);
               Emin:=Emin/V0; Emax:=Emax/V0;
               deltaE:=deltaE/V0;
               PrepTRPlotScr(TR,Potl);
               {New(TRkeys); {Mac only}
               WITH TRkeys DO
                  BEGIN
                     Init(4);
                     key[1]:='F1-Help';
                     key[2]:='F2-Stop';
                     key[3]:='F5-Halve';
                     key[4]:='F6-Double';
                     Display
                  END;
               PrepTRPlot(TR,Potl);
               IF NOT Zoom THEN Info(PlotInfoScr,25,5);
               {New(Mag); New(Phase); {Mac only}
               Mag.Init(1); Phase.Init(1);
               E:=Emin; NumE:=0;
               LogTRmin:=0.0;
               Failed:=False;
               IF Zoom THEN SelectViewPort(3) ELSE SelectViewPort(2);
               Print(25,2,ConCat('E:          ',Eunit,'eV'));
               Print(25,3,'delta E: ');
               WishToAbort:=False;
               WHILE (E<=Emax) AND (NumE<MaxNumE) AND (NOT WishToAbort) DO
                     BEGIN
                        IF Zoom THEN SelectViewPort(3)
                        ELSE SelectViewPort(2);
                        RubOut(28,2,8,DarkGray); RubOut(34,3,8,DarkGray);
                        SetColor(Whitte);
                        Print(28,2,Num2Str(V0*E,8));
                        Print(34,3,Num2Str(V0*deltaE,8));
                        SolveSchrodinger(Mag,Phase,E,Potl,Failed,
                                         RAmp,RPhase,TAmp,TPhase,LogTval);
                        IF NOT Failed THEN
                           BEGIN
                              NumE:=NumE+1; Energy.Put(NumE,E);
                              T.Put(NumE,1-sqr(RAmp));
                              LogT.Put(NumE,LogTval);
                              IF LogTval<LogTRmin THEN LogTRmin:=LogTval;
                              IF LogTval>0.0 THEN LogRval:=0.0
                              ELSE LogRval:=Log10(1-Pwr(10.0,LogTval));
                              IF LogRval<LogTRmin THEN LogTRmin:=LogRval;
                              IF NumE>1 THEN PlotTRvalues(TR,NumE,V0);
                              CheckForEvents;
                              IF Event.ReadKey=Escape THEN WishToAbort:=True
                              ELSE IF TRkeys.Pressed(Tkey) THEN
                                 CASE Tkey OF
                                    1 :Help(TRHelpScr);
                                    2 :WishToAbort:=True;
                                    3 :deltaE:=deltaE/2;
                                    4 :deltaE:=2*deltaE;
                                    END; {case Tkey}
                              E:=E+deltaE;
                           END
                        ELSE WishToAbort:=True;
                     END; {while E<Emax}
               IF NumE>0 THEN 
                  BEGIN
                     IF ((NumE=MaxNumE) OR Failed) AND (E<=Emax) THEN
                        BEGIN
                           Announce('Energy range less than requested.');
                           Emax:=E-deltaE;
                        END;
                     IF LogTRMin=0.0 THEN LogTRMin:=-5.0;
                     PrepTRPlotScr(TR,Potl);
                     IF NOT Zoom THEN Info(PlotInfoScr,25,5);
                     Computed:=True; Saved:=False
                  END
               ELSE Computed:=False;
               Phase.Free; Mag.Free;
               {Dispose(Phase); Dispose(Mag); {Mac only}
               TRkeys.Clear;
               {Dispose(TRkeys); {Mac only}
            END;
         Done;
      END;
   {Dispose(ComputeScr); {Mac only}
END; {ComputeTransRefl}

PROCEDURE ChangeTRPlotScales(VAR TR :TRtype; VAR Potl :PotlType);
VAR
   Changed,AutoSet :boolean;
   HorizMin,HorizMax :real;
BEGIN
   WITH TR DO
      BEGIN
         AutoSet:=False;
         HorizMax:=Potl.V0*Emax; HorizMin:=Potl.V0*Emin;
         ChangePlotScales(HorizMin,HorizMax,ConCat('(',Potl.Eunit,'eV)'),
                          0.0,Potl.V0*(NumE-1)*deltaE,
                          VertMin,VertMax,'',0.0,1.0,Changed,AutoSet);
         IF Changed THEN 
            BEGIN
               Emax:=HorizMax/Potl.V0; Emin:=HorizMin/Potl.V0;
               PrepTRPlotScr(TR,Potl);
               PlotTransRefl(TR,Potl)
            END;
      END;
END; {ChangeTRPlotScales}

PROCEDURE ComputeTR(VAR TR :TRtype; VAR Potl :PotlType;
                    VAR MainMenu :TMenu; VAR MenuClicked :boolean);
{Compute and plot transmission/reflection probabilities.}
VAR
   TRHelpScr :HelpScrType;
   TRkeys    :THotKeys;
   Tkey      :byte;
   n         :integer;
   WishToReturn,SaveZoom :boolean;
BEGIN
   TRHelpScr[01]:='';
   TRHelpScr[02]:=' <F1>-show this help screen.';
   TRHelpScr[03]:='';
   TRHelpScr[04]:=' <F2>-compute and plot transmission and';
   TRHelpScr[05]:='    reflection probabilities for new';
   TRHelpScr[06]:='    energy range.';
   TRHelpScr[07]:='';
   TRHelpScr[08]:=' <F3>-toggle size of plot.  When small, text';
   TRHelpScr[09]:='    information and a legend are shown.';
   TRHelpScr[10]:='';
   TRHelpScr[11]:=' <F4>-change horizontal and vertical scales';
   TRHelpScr[12]:='    of the plot.';
   TRHelpScr[13]:='';
   TRHelpScr[14]:=' <F5>-toggle between linear and semilog plot.';
   TRHelpScr[15]:='';
   TRHelpScr[16]:=' <F10>-activate menu.';
   for n:=17 to 25 do TRHelpScr[n]:='';

   WITH TR DO
      BEGIN
         IF Computed THEN PlotTransRefl(TR,Potl)
         ELSE
            BEGIN
               Zoom:=False;
               VertMin:=0.0; VertMax:=1.0;
               LogTRmin:=-5.0;
               SaveZoom:=Potl.Zoom; Potl.Zoom:=False;
               PlotPotential(Potl); Potl.Zoom:=SaveZoom;
            END;
         ComputeTransRefl(TR,Potl);
         IF Computed THEN
            BEGIN
               {New(TRkeys); {Mac only}
               WITH TRkeys DO
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
               WHILE (NOT WishToReturn) AND (NOT MenuClicked)
                AND Computed DO
                  BEGIN
                     CheckForEvents;
                     IF Event.ReadKey=Escape THEN WishToReturn:=True
                     ELSE IF TRkeys.Pressed(Tkey) THEN
                        BEGIN
                           TRkeys.Clear;
                           case Tkey of
                              1 :Help(TRHelpScr);
                              2 :ComputeTransRefl(TR,Potl);
                              3 :BEGIN
                                    Zoom:=not Zoom;
                                    PlotTransRefl(TR,Potl);
                                    IF Zoom THEN
                                       TRkeys.key[3]:='F3-Contract'
                                    ELSE TRkeys.key[3]:='F3-Expand';
                                 END;
                              4 :ChangeTRPlotScales(TR,Potl);
                              5 :BEGIN
                                    PlotType:=(PlotType MOD 2)+1;
                                    PlotTransRefl(TR,Potl);
                                    IF PlotType=1 THEN
                                       TRkeys.key[5]:='F5-Semilog'
                                    ELSE TRkeys.key[5]:='F5-Linear';
                                 END;
                              6 :BEGIN
                                    WishToReturn:=True;
                                    IF MainMenu.Chosen THEN
                                       MenuClicked:=True;
                                 END;
                              END; {case}
                           TRkeys.Display;
                        END
                     ELSE IF MainMenu.Activated THEN MenuClicked:=True;
                  END;
               TRkeys.Clear;
               {Dispose(TRkeys); {Mac only}
            END; {if Computed}
      END; {with TR}
END; {ComputeTR}

END. {Sc1DTR}
