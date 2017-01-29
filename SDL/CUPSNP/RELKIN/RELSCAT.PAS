{###}
{$D+,L+}
UNIT Relscat;
          {**********************************************}
          {**********************************************}
          {**           Program RELSCAT.Pas            **}
          {**                                          **}
          {**                        J. Rothberg       **}
          {**   Relativistic Scattering and Decay      **}
          {**                                          **}
          {**               May 16 1993                **}
          {**               July 21 1993  MAC version  **}
          {**               Dec. 12 1994  minor fix    **}
          {**********************************************}
          {**********************************************}

Interface

USES Crt, Graph, cups, cupsMUPP, Cupsfunc,Cupsproc, CupsGrph, Cupsgui,
                        Pscat12, plot2, relutil;

VAR { startup defaults}
   maxevt     : Integer ;      { number of events to generate }
   evdelay    : Integer ;     { delay between events; milliseconds }
   evdelayf   : Integer ;     { delay, fast MC; milliseconds }
   evdelays   : Integer ;     { delay, slow MC; milliseconds }
   enabledraw : Boolean ;     { enable event processing }
   eventgen   : Boolean ;    { enable Monte Carlo event generation}
   tflag,fflag: Boolean ;    { true, flase flags for MC delay}
   QuitFlag   : Boolean ;

   decmass    : Real    ;      { decaying particle mass default; nyi}
   p1mass     : Real    ;      { secondary particle 1 mass default; nyi}
   p2mass     : Real    ;      { secondary particle 2 mass default; nyi}
   Hmass      : String[1] ;    { hide decay mass value}
   cEunit     : String[4] ;  { current unit for mass, energy, momemtum}
   MEunit     : String[4] ;  { unit for mass, energy, momemtum}
   GEunit     : String[4] ;  { unit for mass, energy, momemtum}
   inpx1      : Real ;          {input port locations}
   inpx2      : Real ;
   inpy1      : Real ;
   inpy2      : Real ;

{ scatterplot variable selection defaults }
   framex : Integer ;         { CM = 1   Lab = 2 }
   framey : Integer ;
   varnox : Integer ;
   varnoy : Integer ;
   parnox : Integer ;
   parnoy : Integer ;
   xlow:  Real ;
   xhigh: Real ;
   ylow:  Real ;
   yhigh: Real ;     { current plot scale }

   a    : Real;         { default CM angle, degrees  }
   b    : Real;         { default beam momentum, MeV }
   c    : Real;
     { viewports }
   vpCM       : Integer ;
   vpLab      : Integer ;
   vpCMlist   : Integer ;
   vpLablist  : Integer ;
   vptopplot  : Integer ;
   vpbotplot  : Integer ;
   vpAngIn    : Integer ;
   vpmessage  : Integer ;
   vpmupp     : Integer ;

   color      : integer ;
   FullScreen : boolean ;
   FindZExt   : boolean ;

   colwidth : Integer;
   rowht    : Integer;
   cpi      : Real;

VAR
   MyMenu       : TMenu;
   HotKeys      : THotKeys;
   slider : TSliders;

   f            : FunctionXY;
   partic       : pardec;        { particle}
   splott       : splot;         { scatter plot}
   hplott       : hplot;         { histogram }

    axvar  : Integer;                   { choose plot variables }
    inpvar  : array[1..2] of Integer;
    inframe : array[1..2] of Integer;

   inCMang    : Real;      {CM angle input}
   inmoment   : Real;      {decaying particle momentum input}

   initpart :  Integer;   {initial particles chosen in menu}

   eventno   : Integer;       { number of generated events}

{ _________________________________________________________________}
PROCEDURE Startup;

Implementation

PROCEDURE SetUpMyMenu;
VAR k: Integer;
BEGIN
   MyMenu.Init;
   WITH MyMenu DO
   BEGIN
      column(1, 'File');
        row(1,1, 'About CUPS');
        row(1,2, 'About Program');
        row(1,3, 'Configuration');
        row(1,4, '------------------');
        row(1,5, 'Enable Output File');
        row(1,6, 'Close/Disable File');
        row(1,7, '------------------');
        row(1,8, '------------------');
        row(1,9, 'Exit Program');

      column(2, ' Initial');
       FOR k:=1 to nreact DO
       BEGIN
        row(2,k, CONCAT(
          pcons[reac[k].in1].name,'  ',pcons[reac[k].in2].name));
       END;

      column(3, ' Final');
       row(3,1, '                     ');
       row(3,2, '                     ');
       row(3,3, '                     ');
       row(3,4, '                     ');
      column(4, 'Control');
       row(4,1, 'Redraw');
       row(4,2, 'Rescale');
       row(4,3, 'Choose Input');
       row(4,4, 'Choose Scale' );
       row(4,5, 'Clear Plots');
       row(4,6, 'Plot Variables');
       row(4,7, 'Particle data');

      column(5, 'MonteCarlo');
       row(5,1, 'Fast MC ');
       row(5,2, 'Slow MC ');
       row(5,3, 'Parameters');
       row(5,4, 'HELP');
      column(6, 'Help');
       row(6,1, 'Reactions');
       row(6,2, 'Display');
       row(6,3, 'Plots');
       row(6,4, 'Input');
   END;
   mymenu.rowactivate(1,4,FALSE);
   mymenu.rowactivate(1,6,FALSE);
   mymenu.rowactivate(1,7,FALSE);
   mymenu.rowactivate(1,8,FALSE);
   mymenu.rowchecked(5,1,tflag); mymenu.rowchecked(5,2,fflag);
   MyMenu.Display;
END;


PROCEDURE SetUpHotKeys;
BEGIN
   WITH HotKeys DO BEGIN
      init(5);
      key[1] := 'F1-Help';
      key[2] := 'F5-Draw Evt';
      key[3] := 'F6-Run MC';
      key[4] := 'F8-Clr/Rescl';
      key[5] := 'F10-Menu';

   END;
   HotKeys.Display;
END;

PROCEDURE ShoWarnScreen;
VAR
   WarnScreen  : TInputScreen;
BEGIN
   WITH WarnScreen DO
   BEGIN
     init;
     ShowCursor;
     DefineInputPort (inpx1, inpx2, inpy1, inpy2);
     LoadLine(' ');
     LoadLine(' Below Threshold ');
     LoadLine(' ');
     LoadLine('enter new momentum ');
     LoadLine('F-5 to continue ');
     LoadLine('p beam {     } MeV');
     LoadLine(' [ Help ] ');
     LoadLine(' [  Ok  ]  [Cancel] ');
     SetNumber(1,inmoment);
     SetHelpFile('relkin.hlp','Threshold');
     AcceptScreen;
     inmoment := GetNumber(1);
     IF NOT (Canceled) THEN
     BEGIN
       b:= inmoment;
     END;
     done;
     partic.getmom(inmoment);
     partic.rescale;
   END;
   OpenViewPort(vplab);
   SelectViewPort(2);
   ShowCursor;
END;


 
PROCEDURE ProcEvt;                     { Process and draw event }
VAR
   x,y,radius  : Integer;
   x1,y1,x2,y2 : Integer;
   ang,leng:          Real;
BEGIN
   HideCursor;
   eventno := eventno + 1;
   partic.dmass;                       {choose mass if non zero width}
   partic.getmom(inmoment);
   partic.getang(inCMang);
   partic.calc_s;                       { CM variables }
   IF(partic.qvalue > macheps) THEN     { check if above threshold }
   begin
    partic.decay;                { CM angle}
    partic.CMvari;               { calculate initial state CM variables }
    partic.CMvarf;               { final state variables}
    partic.CMtoLab;              { transform to Lab system }
    partic.calc_t;               { momentum transfer }

    SetColor(whitte);
    OpenViewport(vpCM);
    partic.showcm;               { draw CM vectors }
    OpenViewport(vpLab);
    partic.showlab;              { draw Lab vectors }

    partic.fillntuple;           { save event variables for plots }

    IF(OutFileEnable = 1) THEN   { output data file }
      BEGIN
         partic.filloutdat;      { fill output ntuple }
         partic.WriteData;       { write data file }
      END;

    splott.plot;                 { draw scatter plot }
    hplott.fill;                 { fill histogram }
    hplott.plot(eventno);        { draw histogram }

    SetColor(whitte);
    IF (new) THEN OpenViewport(vpCMlist);
    SelectViewport(vpCMlist);
    partic.listgCM;              { list CM values }

    IF (new) THEN  OpenViewport(vpLablist);
    SelectViewport(vpLablist);
    partic.listgLab;             { List Lab values }
    new := FALSE;
  end
  ELSE                           { below threshold }
  begin
    ShoWarnScreen;               { ask for new beam momentum }
  end;                           { check q value }
   ShowCursor;
   SelectViewPort(2);
END;
 
PROCEDURE doeventgen;          { generate random events      }
                               { uniform in cos(theta) in CM }
VAR
   inCMcos : Real;
   idum    : Integer;
BEGIN
   idum:=1;
   IF (eventno < maxevt) THEN
   BEGIN
 {    eventno := eventno + 1;}
     inCMcos := 2*Urand(idum) -1;
     IF (inCMcos < -1) THEN inCMcos :=-1;
     IF (inCMcos >  1) THEN inCMcos :=1;
     inCMang := arccos(inCMcos)*180/cpi;
     ProcEvt;
     delay(evdelay);
   END;

END;

    Procedure ClearPlot;
begin
    eventno := 0;
    eventgen:= false;
    splott.clearplot;
    hplott.clearplot;
end;

    Procedure ClearPlotWindows;
BEGIN
       OpenViewport(8);
       SelectViewPort(8);ClearViewPort;
       OpenViewport(9);
       SelectViewPort(9);ClearViewPort;
END;


PROCEDURE ReadScaleScreen;
VAR
   ScaleScreen  : TInputScreen;
BEGIN
   WITH ScaleScreen DO
   BEGIN
     init;
     DefineInputPort (inpx1, inpx2, inpy1, inpy2);
     LoadLine(' ');
     LoadLine(' Scale ');
     LoadLine(' Vector{     } MeV');
     LoadLine(' ');
     LoadLine(' ');
     LoadLine(' ');
     LoadLine(' [ Help ] ');
     LoadLine(' [  Ok  ]  [Cancel] ');
     SetNumber(1,LabVec);  
     SetHelpFile('relkin.hlp','Scale');
     AcceptScreen;
     IF NOT (canceled) THEN  BEGIN
        LabVec := GetNumber(1);
     END;
     done;
   END;
   OpenViewPort(vplab);
   SelectViewPort(2);
   ShowCursor;
END;

PROCEDURE ReadEvtScreen;     { input max number of events to generate}
VAR
   EvtScreen : TInputScreen;
   n : byte;
BEGIN
   WITH EvtScreen DO
   BEGIN
     init;
     DefineInputPort (inpx1, inpx2, inpy1, inpy2);
     LoadLine(' ');
     LoadLine(' Monte Carlo        ');
     LoadLine('                    ');
     LoadLine(' no.of evts  {    } ');      { number of events}
     LoadLine(' pause(msec) {    } ');      { delay between events }
     LoadLine('                    ');
     LoadLine(' [  Ok  ]  [Cancel] ');
     SetNumber(1,Maxevt);
     SetNumber(2,evdelay);
     AcceptScreen;
     IF NOT (canceled) THEN
     BEGIN
        Maxevt  := trunc(GetNumber(1));
        evdelay := trunc(GetNumber(2));
     END;
     done;
   END;
     IF (evdelay < 10) THEN evdelay := 10;
   OpenViewPort(vpLab);
   SelectViewPort(2);
   ShowCursor;
END;


PROCEDURE PlotVarScreen;     { input plot variables }
VAR
   PlotVScreen : TInputScreen;
   n : byte;
BEGIN

   WITH PlotVScreen DO
   BEGIN
     init;
     DefineInputPort (0.05, 0.55, 0.10, 0.90);
     LoadLine('         Scatter Plot Variables     ');
     LoadLine('                 x-axis       y-axis   ');
     LoadLine(' Particle (1/2)    { }          { }  ');
     LoadLine(' Frame:      CM    Lab     CM    Lab ');
     Loadline('             #1    #1      #2    #2  ');
     LoadLine(' energy           #3           #4   ');
     LoadLine(' momentum         #3           #4   ');
     LoadLine(' beta             #3           #4   ');
     LoadLine(' theta            #3           #4   ');
     LoadLine(' cos theta        #3           #4   ');
     LoadLine(' pT               #3           #4   ');
     LoadLine(' KE               #3           #4   ');
     LoadLine(' theta diff       #3           #4   ');
     LoadLine(' inv mass         #3           #4   ');
     LoadLine(' ');
     LoadLine(' scale: low  {      }    low  {      } ');
     LoadLine('        high {      }    high {      } ');
     LoadLine(' [  Ok  ]    [Cancel]  [ Help ]  ');
     SetNumber(1,parnox);
     SetNumber(2,parnoy);

     SetRadioButton('1',framex);
     SetRadioButton('2',framey);
     SetRadioButton('3',varnox);
     SetRadioButton('4',varnoy);
     SetNumber(25,xlow);
     SetNumber(26,ylow);
     SetNumber(27,xhigh);
     SetNumber(28,yhigh);
     SetHelpFile('relkin.hlp','Plotvar');
     AcceptScreen;
     IF NOT (canceled) THEN
     BEGIN
       parnox  := trunc(GetNumber(1));
       parnoy  := trunc(GetNumber(2));

       framex  := GetRadioButton('1');
       framey  := GetRadioButton('2');
       varnox  := GetRadioButton('3');
       varnoy  := GetRadioButton('4');
       xlow    := GetNumber(25);
       ylow    := GetNumber(26);
       xhigh   := GetNumber(27);
       yhigh   := GetNumber(28);
     END;
     done;
   END;
 {   splott.getscl(xlow,xhigh,ylow,yhigh);    rem 4 Feb. 1993 }
   IF (parnox < 1) or (parnox >2) THEN parnox := 1;
   IF (parnoy < 1) or (parnoy >2) THEN parnoy := 1;
   IF (framex < 1) or (framex >2) THEN framex := 1;
   IF (framey < 1) or (framey >2) THEN framey := 1;
   IF (varnox < 1) or (varnox >9) THEN varnox := 4;
   IF (varnoy < 1) or (varnoy >9) THEN varnoy := 4;

   axvar := 1;
   inpvar[axvar]  := varnox;
   inframe[axvar] := (framex-1)*2 + parnox;
   axvar := 2;
   inpvar[axvar]  := varnoy;
   inframe[axvar] := (framey-1)*2 + parnoy;

   IF (xlow < xhigh) and (ylow < yhigh) THEN
        splott.rescl(xlow,xhigh,ylow,yhigh);
   ClearPlot;
   ClearPlotWindows;
   splott.init(inpvar[1],inframe[1],inpvar[2],inframe[2]);
   hplott.init(inpvar[1],inframe[1]);
   OpenViewPort(vpmupp);
   SelectViewPort(vpmupp);ClearViewPort;
   SetColor(whitte);
   OpenViewPort(vpCM);OpenViewPort(vpLab);
   OpenViewPort(vpCMlist);OpenViewPort(vpLablist);
   {ClearPlot;}
   SelectViewPort(2);
   ShowCursor;

END;

PROCEDURE EnableFile;     { enable output file }
VAR
   FileScreen : TInputScreen;
   n : byte;
BEGIN
   WITH FileScreen DO
   BEGIN
     init;
     DefineInputPort (inpx1, inpx2, inpy1, inpy2);
     LoadLine(' ');
     LoadLine('   Enter File name ');
     LoadLine('                ');
     LoadLine(' "           " ');      { file name  }
     LoadLine(' ');
     LoadLine(' ');
     LoadLine(' [ Help ] ');
     LoadLine(' [  Ok  ]  [Cancel] ');
     SetString(1,OutFilename);
     SetHelpFile('relkin.hlp','Infile');
     AcceptScreen;
     IF NOT (canceled) THEN
     BEGIN
        OutFilename:= GetString(1);
     END;
     done;
   END;
     OutFileEnable:= 1;
     Assign(outfile,OutFilename);
     Rewrite(outfile);
   OpenViewPort(vpLab);
   SelectViewPort(2);
   ShowCursor;
END;

PROCEDURE DisableFile;   {disable output file}
BEGIN
   Close(outfile);
   OutFileEnable:= 0;
END;

PROCEDURE ReadMassScreen;     { input variables }
VAR
   MassScreen : TInputScreen;
   n : byte;
BEGIN
   WITH MassScreen DO
   BEGIN
     init;
     DefineInputPort (inpx1, inpx2, inpy1, inpy2);
     LoadLine(' ');
     LoadLine('   Choose Inputs   ');
     LoadLine('                   ');
     LoadLine(' CM angle  {    }  deg ');     { CM angle of particle 1 }
     LoadLine(' p_beam  {      }  MeV ');     { beam momentum }
     LoadLine(' ');                          {  }
     LoadLine(' [ Help ] ');
     LoadLine(' [  Ok  ]   [Cancel]  ');
     SetNumber(1,inCMang);
     SetNumber(2,inmoment);
     SetHelpFile('relkin.hlp','Inputvar');
     AcceptScreen;
     IF NOT (canceled) THEN
     BEGIN
        inCMang  := GetNumber(1);
        inmoment := GetNumber(2);
     END;
     done;
   END;
   a:= inCMang;
   b:= inmoment;
   OpenViewPort(vpLab);
   SelectViewPort(2);
   ShowCursor;
END;

PROCEDURE PartScrn;
VAR
    i,j      : Integer;
    parthelp : HelpScrType;
    hname    : String[8];
    lstring  : String[12];
BEGIN
       FOR i:=1 TO 25 DO
       BEGIN
         parthelp[i]:=' ';
       END;
       j:=3;
    parthelp[2] := ' name       mass     lifetime(nsec)  width(MeV)';
       FOR i:= 1 TO numpart DO
       BEGIN
         WITH pcons[i] DO
         BEGIN
         hname:='        ';
         hname:= copy(name,1,8);
         hname[0]:=#8;
         IF(hname<>'        ') THEN
         BEGIN
            lstring:= decstr(lifet,12);
            IF (lifet = -1) THEN
            BEGIN
               lstring:='            ';
               lstring[0]:=#12;
            END;
            j:=j+1;
            parthelp[j]:= CONCAT(hname ,'  ' ,
            decstr(mass,8) , '  ', lstring , '  ' , decStr(width,8));
         END;
         END;
      END;
      Help(parthelp);
END;


PROCEDURE HandleMenu;
VAR i,f :Integer;
    n : byte;
    temp : real;
    min,max : point3D;

BEGIN
   WITH MyMenu DO
   CASE colChosen OF
    1 : BEGIN
        CASE rowchosen OF
         1 : AboutCUPS;
         2 : ShowHelp('relkin.hlp','About Program');
         3 : Configuration;         
         5 : BEGIN
              EnableFile;
              myMenu.rowactivate(1,6,TRUE);
              myMenu.rowactivate(1,5,FALSE);
            END;
         6 : BEGIN
              DisableFile;
              myMenu.rowactivate(1,5,TRUE);
              myMenu.rowactivate(1,6,FALSE);
            END;

         9 : QuitFlag:= true;

        END;
        END;

    2 : BEGIN           { initial state }
           MyMenu.row(3,1,' '); MyMenu.row(3,2,' ');
           MyMenu.row(3,3,' '); MyMenu.row(3,4,' ');
           MyMenu.rowactivate(3,1,FALSE);  MyMenu.rowactivate(3,2,FALSE);
           MyMenu.rowactivate(3,3,FALSE);  MyMenu.rowactivate(3,4,FALSE);
           initpart := rowchosen;
           i:= rowchosen;

           FOR f:=1 TO reac[i].nf DO     { loop over final states }
           BEGIN
              MyMenu.row(3,f, CONCAT('-> ',
                 pcons[reac[i].fin1[f]].name,' + ',
                 pcons[reac[i].fin2[f]].name ));

              MyMenu.rowactivate(3,f,TRUE);
           END;
           partic.istate(reac[i].in1,reac[i].in2);

               { default final state}
           partic.fstate(reac[initpart].fin1[1],
                          reac[initpart].fin2[1]);
           partic.getmom(inmoment);
           partic.rescale;
           ClearPlot;
           new := TRUE;     { enable screen refresh }
           ProcEvt;         {do one event}

        END;


    3 : BEGIN   { final state }
            partic.fstate(reac[initpart].fin1[rowchosen],
                          reac[initpart].fin2[rowchosen]);


           partic.getmom(inmoment);
           partic.rescale;
           ClearPlot;
           new := TRUE;     { enable screen refresh }
           ProcEvt;         {do one event}
         END;
    4 :  IF rowChosen<=7 THEN
          BEGIN
           CASE rowChosen OF
            1 : ProcEvt;
            2 : BEGIN partic.getmom(inmoment);partic.rescale; ProcEvt; END;
            3 : BEGIN ReadMassScreen;
                 partic.getmom(inmoment);partic.rescale; ProcEvt; END;
            4 : ReadScaleScreen;
            5 : BEGIN ClearPlot;  END;
            6 : PlotVarScreen;
            7 : PartScrn;
           END;
           ShowCursor;
           SelectViewPort(2);
        END;
    5 : BEGIN
         CASE rowChosen OF
          1 : BEGIN evdelay:=  evdelayf;
              rowchecked(5,1,tflag); rowchecked(5,2,fflag); END;  {fast}
          2 : BEGIN evdelay:=  evdelays;     {slow}
              rowchecked(5,2,tflag); rowchecked(5,1,fflag); END;  {fast}
          3 : ReadEvtScreen;
          4 : ShowHelp('relkin.hlp','MCparam');
         END;
       END;

    6 : BEGIN
         CASE rowChosen OF
          1 :  ShowHelp('relkin.hlp','Reactions');
          2 :  ShowHelp('relkin.hlp','Display');
          3 :  ShowHelp('relkin.hlp','Plots');
          4 :  ShowHelp('relkin.hlp','Input');
         END;
        END;
   END; {case}
   new:= TRUE;
END;

PROCEDURE INTERRUPT (key : byte);
BEGIN
   enabledraw:= False;
   CASE key OF
    1 : ShowHelp('relkin.hlp','General Help');
    2 : BEGIN                                   {F-5}
          doEventGen;
          SetWriteMode (CopyPut);
          Showcursor;
          SelectViewport(2);
        END;
    3 : BEGIN        {Random events}            {F-6}

           IF eventgen=False THEN
           BEGIN
             eventgen := True;
             hotkeys.key[3] := 'F6-Stop MC';
             hotkeys.display;
           END
           ELSE
           BEGIN
             eventgen := False;
             hotkeys.key[3] := 'F6-Run MC';
             hotkeys.display;
           END;
        END;
    4 : BEGIN        {Clear Plots/Rescale vectors  }    {F-8}
           eventno:= 0;
           eventgen:= False;
           hotkeys.key[3] := 'F6-Run MC';
           hotkeys.display;
           ClearPlot ;
           partic.getmom(inmoment);
           partic.rescale;

        END;
    5 : BEGIN CheckForEvents; IF MyMenu.Chosen THEN HandleMenu END;  {F-10}

   END; {case}
END;

PROCEDURE displaySlider;
VAR
   h : integer;
BEGIN
   slider.init;
   SelectViewPort(4);
   clearViewPort;

  {              low hi deflt    x    y  siz dec                 }
 slider.create(1,0.,180.,  30, 0.66,0.52,0.99,0,'0','180','CM angle',false);
 slider.create(2,0.,1000.,100, 0.66,0.46,0.99,0,'0','1000','Momentum',false);

   slider.drawAll;

   SelectViewPort(2);
END;

PROCEDURE HandleSlider;
VAR
    iang : REAL;
    cang : REAL;
BEGIN
    inCMang:= slider.value(1);
    inmoment:= slider.value(2);
    cang:= inCMang/5.0;
    iang := INT(cang);
    inCMang:=iang*5;
    slider.reset(1,inCMang);

    partic.getmom(inmoment);
    partic.getang(inCMang);

    Delay(20);
    ProcEvt;

END;

{ *****************************************************}
     PROCEDURE Startup;
VAR
   key   : Byte;
   dy    : Integer;
   idum  : Integer;
   rdum  : Real;

BEGIN

{ constant definitions}
   maxevt     := 100;      { number of events to generate }
   evdelay    := 100;     { delay between events; milliseconds }
   evdelayf   := 100;     { delay, fast MC; milliseconds }
   evdelays   := 1000;     { delay, slow MC; milliseconds }
   enabledraw := True;     { enable event processing }
   eventgen   := False;    { enable Monte Carlo event generation}

   decmass    := 140;      { decaying particle mass default; nyi}
   p1mass     := 100;      { secondary particle 1 mass default; nyi}
   p2mass     :=   0;      { secondary particle 2 mass default; nyi}
   Hmass      := 'N';    { hide decay mass value}
   cEunit     := ' MeV';  { current unit for mass, energy, momemtum}
   MEunit     := ' MeV';  { unit for mass, energy, momemtum}
   GEunit     := ' GeV';  { unit for mass, energy, momemtum}
   inpx1      := 0.36;          {input port locations}
   inpx2      := 0.62;
   inpy1      := 0.54;
   inpy2      := 0.88;

{ scatterplot variable selection defaults }
   framex := 1;         { CM = 1   Lab = 2 }
   framey := 2;
   varnox := 4;
   varnoy := 4;
   parnox := 1;
   parnoy := 2;
   xlow   := 0.0;
   xhigh  := 180.0;
   ylow   := 0.0;
   yhigh  := 180.0;     { current plot scale }

   a      := 30;         { default CM angle, degrees  }
   b      := 100;        { default beam momentum, MeV }
   c      := 00;
     { viewports }
   vpCM       := 6;
   vpLab      := 7;
   vpCMlist   := 1;
   vpLablist  := 5;
   vptopplot  := 3;
   vpbotplot  := 2;
   vpAngIn    := 4;
   vpmessage  := 10;
   vpmupp     := 11;

   color      := green;
   FullScreen := false;
   FindZExt   := true;
   tflag := True;
   fflag := False;

   colwidth := 8;
   rowht    := 20;
   cpi := pi;

 {end of constant definitons }
   QuitFlag:= false;

   DefineViewPort (1, 0.0,  0.32, 0.08, 0.48);      {List CM}
   DefineViewPort (5, 0.33, 0.65, 0.08, 0.48);      {List Lab}

   DefineViewPort (6, 0.0,  0.32, 0.52, 0.92);      {  CM   }
   DefineViewPort (7, 0.33, 0.65, 0.52, 0.92);      {  Lab  }

   DefineViewPort (2, 0.71, 1.0, 0.08, 0.43);       { bottom Plot }
   DefineViewPort (3, 0.71, 1.0, 0.58, 0.92);       { top Plot    }
   DefineViewPort (4, 0.7, 1.0, 0.45, 0.55);        { Display Angles }
   DefineViewPort (8, 0.65, 1.0, 0.05, 0.44);       { clear bot Window }
   DefineViewPort (9, 0.65, 1.0, 0.555, 0.93);       { clear top Window }
   DefineViewPort (10, 0.36, 0.62, 0.54, 0.88);     { message window }
   DefineViewPort (11, 0.00, 0.65, 0.08, 0.92);     { clear mupp screen }

   partic.firstinit;      { read particle and reaction data file}

   SetUpMyMenu;
   SetUpHotKeys;
   HideCursor;
   OpenViewPort (1);
   OpenViewPort (5);
   OpenViewPort (6);
   OpenViewPort (7);
   OpenViewPort (3);

{   OpenViewPort(10);}

   AboutCUPS;
{   ShowHelp('relkin.hlp','HelpScr');}

   Eunit := cEunit;                         { Default energy units}

    inCMang := a;                     {initialize angle, momentum}
    inmoment:= b;

    partic.getmom(inmoment);
    partic.getang(inCMang);
    partic.istate(pion,proton);
    partic.fstate(pion,proton);

    partic.init;
    partic.rescale;

   {Default no event generation}
    eventgen := False;
    eventno := 0;
    idum:= -1;                           {Random Number Initialization}
    rdum:= Urand(idum);

    OpenViewPort (vpbotplot);
    splott.firstinit(4,3,4,4);          { initialize default scatter plot}

    OpenViewPort(vptopplot);
    hplott.firstinit(4,3);              { initialize default histogram}
    MyMenu.Display;

   SetWriteMode (CopyPut);
   ShowCursor;

   DisplaySlider;

   REPEAT
      enabledraw:= True;
      IF (eventgen) THEN doeventgen;      {generate events}

      CheckForEvents;
      IF HotKeys.Pressed(key) THEN INTERRUPT(key);
      IF MyMenu.Activated THEN HandleMenu;
      IF Slider.Changed THEN HandleSlider;
   UNTIL QuitFlag;
      MyMenu.Done;
      CupsDone;
END;

END.
 
 
