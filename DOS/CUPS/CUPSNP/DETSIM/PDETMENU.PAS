{###}
{$D+,L+}
Unit Pdetmenu;
          {**********************************************}
          {**********************************************}
          {**           Unit  PDETMENU.Pas             **}
          {**                                          **}
          {**                        J. Rothberg       **}
          {**                                          **}
          {**                                          **}
          {**                  June 1 1992             **}
          {**                  15 Feb. 1993            **}
          {**                  23 May. 1993            **}
          {**                  16 Oct. 1993            **}
          {**                  12 Dec. 1994            **}
          {**********************************************}
          {**********************************************}
Interface

USES Crt, Graph, cups, cupsMUPP, Cupsfunc,Cupsproc,Cupsgrph,Cupsgui,
                           Pscat12, Pdetec;

VAR
   maxevt     : Integer ;      { number of events to generate }
   evdelay    : Integer ;     { delay between events; milliseconds }
   enabledraw : Boolean ;     { enable event processing }
   eventgen   : Boolean ;    { enable Monte Carlo event generation}

   decmass    : Real    ;      { decaying particle mass default; nyi}
   p1mass     : Real    ;      { secondary particle 1 mass default; nyi}
   p2mass     : Real    ;      { secondary particle 2 mass default; nyi}
   Hmass      : String[1] ;    { hide decay mass value}
   cEunit     : String[4] ;  { current unit for mass,energy,momemtum}
   MEunit     : String[4] ;  { unit for mass, energy, momemtum}
   GEunit     : String[4] ;  { unit for mass, energy, momemtum}
   inpx1      : Real ;        { input port locations}
   inpx2      : Real ;
   inpy1      : Real ;
   inpy2      : Real ;

   a          : real ;          { default CM angle, degrees  }
   b          : real ;        { default beam momentum, MeV }
   c          : real ;
     { viewports }
   vpCM       : Integer ;
   vpLab      : Integer ;
   vpLablist  : Integer ;
   vpAngIn    : Integer ;
   vpmessage  : Integer ;
   vpmupp     : Integer ;

   color      : integer ;
   FullScreen : boolean ;
   FindZExt   : boolean ;
   QuitFlag   : boolean ;
VAR
   MyMenu       : TMenu;
   HotKeys      : THotKeys;
   f            : FunctionXY;
   partic       : pardec;        { particle}
   detector     : detect;
   evnthits     : evnt;

   inCMang    : Real;      {CM angle input}
   inmoment   : Real;      {decaying particle momentum input}

   initpart :  Integer;   {initial particles chosen in menu}

   eventno   : Integer;       { number of generated events}

{ _________________________________________________________________}

Procedure Startup;


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

      column(2, 'Initial');
       FOR k:=1 to nreact DO
       BEGIN
         row(2,k, CONCAT(
            pcons[reac[k].in1].name,'  ',pcons[reac[k].in2].name));
       END;

      column(3, ' Final ');
       row(3,1, '                     ');
       row(3,2, '                     ');
       row(3,3, '                     ');
       row(3,4, '                     ');
      column(4, 'Control');
       row(4,1, 'Redraw');
       row(4,2, 'Choose Input');
       row(4,3, 'Detector Material');
       row(4,4, 'Detector Geometry' );
       row(4,5, 'Detector Response' );
       row(4,6, 'Text on/off ');
       row(4,7, '  ');
       row(4,8, '      ');
      column(5, 'MonteCarlo');
       row(5,1, 'Fast MC ');
       row(5,2, 'Slow MC ');
       row(5,3, 'Params ');


      column(6, 'Help');
       row(6,1, 'Reactions');
       row(6,2, 'General Help');
       row(6,3, 'Output');
       row(6,4, 'Input');
       row(6,5, 'How to Use Program');
   END;
   mymenu.rowactivate(1,4,FALSE);
   mymenu.rowactivate(1,6,FALSE);
   mymenu.rowactivate(1,7,FALSE);
   mymenu.rowactivate(1,8,FALSE);
   MyMenu.Display;
END;
 
 
PROCEDURE SetUpHotKeys;
BEGIN
   WITH HotKeys DO BEGIN
      init(5);
      key[1] := 'F1-Help';
      key[2] := 'F5-Draw Evt';
      key[3] := 'F6-Run MC';
      key[4] := 'F8-Clear ';
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
     DefineInputPort (inpx1,inpx2,inpy1,inpy2);
     LoadLine('');
     LoadLine('  Below Threshold  ');
     LoadLine(' ');
     LoadLine(' enter momentum  ');
     LoadLine(' F-5 to continue  ');
     LoadLine(' p beam  {     }  MeV ');
     LoadLine(' ');
     LoadLine(' [  Ok  ]  [Cancel] ');
     SetNumber(1,inmoment);
     AcceptScreen;
     IF NOT(canceled) THEN
      BEGIN
        inmoment := GetNumber(1);
        b:= inmoment;
      END;
     done;
   END;  
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
   partic.getmom(inmoment);
   partic.getang(inCMang);
   partic.calc_s;                       { CM variables }
   IF(partic.qvalue > macheps) THEN     { check if above threshold }
   BEGIN
    partic.decay;                { CM angle}
    partic.CMvari;               { calculate initial state CM variables }
    partic.CMvarf;               { final state variables}
    partic.CMtoLab;              { transform to Lab system }
    partic.calc_t;               { momentum transfer }

    SetColor(white);
  {  OpenViewport(vpCM); }
  {  partic.showYZ;      }         { draw YZ view }
  {  OpenViewport(vpLab); }
  {  partic.showXY;       }        { draw XY view }

{    evnthits.detdraw;}
    evnthits.fill(partic);
    evnthits.draw;
    evnthits.list(partic);


    IF(OutFileEnable = 1) THEN   { output data file }
      BEGIN
         evnthits.filloutdat;      { fill output ntuple }
         evnthits.WriteData;       { write data file }
      END;


    SetColor(white);

 {   OpenViewport(vpLablist);
    SelectViewport(vpLablist);
    partic.listgLab;    }         { List Lab values }
  END
  ELSE                           { below threshold }
  BEGIN
    ShoWarnScreen;               { ask for new beam momentum }
  END;                           { check q value }
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
     inCMang := arccos(inCMcos)*180/pi;
     ProcEvt;
     delay(evdelay);
   END;

END;

PROCEDURE ReadMatrlScrn;
VAR
   MatrlScrn  : TInputScreen;
   Amat, Pmat  : Real;
BEGIN
   WITH MatrlScrn DO
   BEGIN
     init;
     DefineInputPort (inpx1,inpx2,inpy1,inpy2);
     LoadLine('');
     LoadLine('  Detector Material ');
     LoadLine(' Active    {      }  gm/cm^2  ');
     LoadLine(' Passive   {      }   gm/cm^2 ');
     LoadLine(' ');
     LoadLine(' ');
     LoadLine(' ');
     LoadLine(' [  Ok  ]  [Cancel] ');
     { temporary}
     Amat:= 100.0;
     Pmat:= 1000;
     SetNumber(1,Amat);
     SetNumber(2,Pmat);
     AcceptScreen;
     IF NOT(canceled) THEN
       BEGIN
         Amat   := GetNumber(1);
         Pmat   := GetNumber(2);
       END;
     done;
    END;
   SelectViewPort(2);
   ShowCursor;
END;


PROCEDURE ReadRespScrn;
VAR
   RespScrn  : TInputScreen;
   ERes, xyRes  : Real;
BEGIN
   WITH RespScrn DO
   BEGIN
     init;
     DefineInputPort (inpx1,inpx2,inpy1,inpy2);
     LoadLine('');
     LoadLine('  Detector Response ');
     LoadLine(' E Resolution    {     }  %  ');
     LoadLine(' x,y Resolution  {     }  cm  ');
     LoadLine(' ');
     LoadLine(' ');
     LoadLine(' ');
     LoadLine(' [  Ok  ]  [Cancel] ');
     evnthits.getResp(Eres,xyRes);
     SetNumber(1,ERes);
     SetNumber(2,xyres);
     AcceptScreen;
     IF NOT(canceled) THEN
     BEGIN
       ERes   := GetNumber(1);
       xyRes  := GetNumber(2);
       evnthits.setResp(Eres,xyRes);
     END;
     done;
   END;
   SelectViewPort(2);
   ShowCursor;
END;


PROCEDURE ReadDetGeomScrn;
VAR
   DetGeomScrn  : TInputScreen;
   irad, zpos, ienscl    : Real;
   zstart, zend, decleng : Real;
BEGIN
   WITH DetGeomScrn DO
   BEGIN
     init;
     DefineInputPort (inpx1,inpx2,inpy1,inpy2);
     LoadLine('');
     LoadLine('  Detector Geometry  ');
     LoadLine(' Radius  {    }  cm  ');
     LoadLine(' Z position  {    }  cm  ');
     LoadLine(' Energy scale  {     }  cm/MeV  ');
     LoadLine('    Decay region  ');
     LoadLine(' Z start  {    }  cm  ');
     LoadLine(' Z end    {    }  cm  ');
     LoadLine(' Decay Length  {     }  cm  ');
     LoadLine(' [  Ok  ]  [Cancel] ');
     evnthits.getdetect(irad,zpos,ienscl,zstart,zend,decleng);
     SetNumber(1,irad);
     SetNumber(2,zpos);
     SetNumber(3,ienscl);
     SetNumber(4,zstart);
     SetNumber(5,zend);
     SetNumber(6,decleng);
     AcceptScreen;
     IF NOT(canceled) THEN
     BEGIN
       irad   := GetNumber(1);
       zpos   := GetNumber(2);
       ienscl := GetNumber(3);
       zstart := GetNumber(4);
       zend   := GetNumber(5);
       decleng:= GetNumber(6);
     END;
     done;
   END;  
      evnthits.setdetect(irad,zpos,ienscl,zstart,zend,decleng);
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
     DefineInputPort (inpx1,inpx2,inpy1,inpy2);
     LoadLine('');
     LoadLine('  Monte Carlo      ');
     LoadLine('                   ');
     LoadLine(' no.of evts   {    } ');      { number of events}
     LoadLine(' pause(msec)  {    } ');      { delay between events }
     LoadLine('                   ');
     LoadLine(' [  Ok  ]  [Cancel]   ');
     SetNumber(1,Maxevt); 
     SetNumber(2,evdelay); 
     AcceptScreen;
     IF NOT(canceled) THEN
     BEGIN
       Maxevt  := trunc(GetNumber(1));
       evdelay := trunc(GetNumber(2));
       IF (evdelay < 10) THEN evdelay := 10;
     END;
     done;
   END;
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
     DefineInputPort (inpx1,inpx2,inpy1,inpy2);
     LoadLine('');
     LoadLine('   Enter File name ');
     LoadLine('                ');
     LoadLine(' "           "');      { file name  }
     LoadLine(' ');
     LoadLine(' ');
     LoadLine(' ');
     LoadLine(' [  Ok  ]  [Cancel]   ');
     SetString(1,OutFileName);
     AcceptScreen;
     IF NOT(canceled) THEN
     BEGIN
       OutFilename:= GetString(1);
       OutFileEnable:= 1;
       Assign(outfile,OutFilename);
       Rewrite(outfile);
     END;
     done;
   END;
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
     DefineInputPort (inpx1,inpx2,inpy1,inpy2);
     LoadLine('');
     LoadLine('   Choose Inputs ');
     LoadLine('                ');
     LoadLine(' CM angle   {   }  deg ');     { CM angle of particle 1 }
     LoadLine(' p_beam   {     }  MeV ');     { beam momentum }
     LoadLine('');                         {  }
     LoadLine('');
     LoadLine(' [  Ok  ]  [Cancel]   ');
     SetNumber(1,inCMang); 
     SetNumber(2,inmoment); 
     AcceptScreen;
     IF NOT(canceled) THEN
     BEGIN
       inCMang  := GetNumber(1);
       inmoment := GetNumber(2);
       a:= inCMang;
       b:= inmoment;
     END;
     done;
   END;
   SelectViewPort(2);
   ShowCursor;
END;
 

 
PROCEDURE HandleMenu;
VAR i, f : Integer;
    n : byte;
    temp : real;
    min,max : point3D;

BEGIN
   WITH MyMenu DO
   CASE colChosen OF
    1 : BEGIN
        CASE rowchosen OF
         1 : AboutCUPS;
         2 : ShowHelp('detsim.hlp','About Program');
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
         9 : QuitFlag:=true;
        END;
        END;

    2 : BEGIN
           MyMenu.row(3,1,' '); MyMenu.row(3,2,' ');
           MyMenu.row(3,3,' '); MyMenu.row(3,4,' ');
           MyMenu.rowactivate(3,1,FALSE);  MyMenu.rowactivate(3,2,FALSE);
           MyMenu.rowactivate(3,3,FALSE);  MyMenu.rowactivate(3,4,FALSE);
           initpart := rowchosen;
           i:=rowchosen;
           FOR f:= 1 TO reac[i].nf DO    {loop over final states}
           BEGIN
             MyMenu.row(3,f,CONCAT('-> ',
               pcons[reac[i].fin1[f]].name,' + ',
               pcons[reac[i].fin2[f]].name));
             MyMenu.rowactivate(3,f,TRUE);
           END;
           partic.istate(reac[i].in1,reac[i].in2);

                          { default final state}
           partic.fstate(reac[initpart].fin1[1],
                          reac[initpart].fin2[1]);
           partic.getmom(inmoment);
           partic.rescale;
           new := TRUE;     { enable screen refresh }
           ProcEvt;         {do one event}

        END;


    3 : BEGIN    { final state}
            partic.fstate(reac[initpart].fin1[rowchosen],
                          reac[initpart].fin2[rowchosen]);

           partic.getmom(inmoment);
           partic.rescale;
           ProcEvt;         {do one event}
         END;
    4 :  IF rowChosen<7 THEN
          BEGIN
           CASE rowChosen OF
            1 : evnthits.detdraw;
            2 : BEGIN ReadMassScreen;
                 partic.getmom(inmoment);partic.rescale; ProcEvt; END;
            3 : ReadMatrlScrn;
            4 : ReadDetGeomScrn;
            5 : ReadRespScrn;
            6 : BEGIN   evnthits.debugflag:= not evnthits.debugflag; END;
           END;
           ShowCursor;
           SelectViewPort(2);
        END;
    5 : BEGIN
        CASE rowChosen OF
         1 : evdelay:=  100;     {fast}
         2 : evdelay:= 1000;     {slow}
         3 : ReadEvtScreen;
        END;
        END;

    6 : BEGIN
         CASE rowChosen OF
          1: ShowHelp('detsim.hlp','Reactions');
          2: ShowHelp('detsim.hlp','General Help');
          3: ShowHelp('detsim.hlp','Output');
          4: ShowHelp('detsim.hlp','Input');
          5: ShowHelp('detsim.hlp','How to use');
         END;
        END;

   END; {case}
END;

PROCEDURE INTERRUPT (key : byte);
BEGIN
   enabledraw:= False;
   CASE key OF
    1 : ShowHelp ('detsim.hlp','About Program');
    2 : BEGIN                                   {F-5}
          doeventgen;
          SetWriteMode (CopyPut);
          Showcursor;
          SelectViewport(2);
        END;
    3 : BEGIN        {Random events}            {F-6}
          IF eventgen=False THEN
          BEGIN
             eventgen := True;
             hotkeys.key[3]:= 'F6-Stop MC';
             hotkeys.display;
          END
          ELSE
          BEGIN
             eventgen := False;
             hotkeys.key[3] := 'F6-Run MC';
             hotkeys.display
          END;
        END;
    4 : BEGIN   {clear hits}
          eventno:=0;
          evnthits.hitclear;
          evnthits.detdraw;
        END;
    5 : BEGIN CheckForEvents; IF MyMenu.Chosen THEN HandleMenu END;  {F-10}
   END; {case}
END;


{ *****************************************************}
Procedure Startup;
VAR
   key   : Byte;
   dy    : Integer;
   idum  : Integer;
   rdum  : Real;
BEGIN
   maxevt     := 100;      { number of events to generate }
   evdelay    := 1000;     { delay between events; milliseconds }
   enabledraw := True;     { enable event processing }
   eventgen   := False;    { enable Monte Carlo event generation}

   decmass    := 140;      { decaying particle mass default; nyi}
   p1mass     := 100;      { secondary particle 1 mass default; nyi}
   p2mass     :=   0;      { secondary particle 2 mass default; nyi}
   Hmass      := 'N';    { hide decay mass value}
   cEunit     := ' MeV';  { current unit for mass,energy,momemtum}
   MEunit     := ' MeV';  { unit for mass, energy, momemtum}
   GEunit     := ' GeV';  { unit for mass, energy, momemtum}
   inpx1      := 0.06;        { input port locations}
   inpx2      := 0.42;
   inpy1      := 0.36;
   inpy2      := 0.86;

   a          := 30;          { default CM angle, degrees  }
   b          := 4000;        { default beam momentum, MeV }
   c          := 00;
     { viewports }
   vpCM       := 6;
   vpLab      := 7;
   vpLablist  := 5;
   vpAngIn    := 4;
   vpmessage  := 10;
   vpmupp     := 11;

   color      := green;
   FullScreen := false;
   FindZExt   := true;
   QuitFlag   := false;
 {--------------------------------}

                   {   x1    x2    y1    y2 }
   DefineViewPort (5, 0.03, 0.98, 0.08, 0.30);      {List Lab}

   DefineViewPort (6, 0.0,  0.50, 0.32, 0.92);      {  YZ view }
   DefineViewPort (7, 0.52, 0.98, 0.32, 0.92);      {  XY view }

   DefineViewPort (4, 0.7, 1.0, 0.45, 0.55);        { Display Angles }
   DefineViewPort (8, 0.65, 1.0, 0.05, 0.44);       { clear bot Window }
   DefineViewPort (9, 0.65, 1.0, 0.56, 0.93);       { clear top Window }
   DefineViewPort (10, 0.36, 0.62, 0.54, 0.88);     { message window }
   DefineViewPort (11, 0.00, 0.65, 0.08, 0.92);     { clear mupp screen }

   partic.firstinit;                 {read particle data file}

   SetUpMyMenu;
   SetUpHotKeys;
   HideCursor;
   OpenViewPort (5);
   OpenViewPort (6);
   OpenViewPort (7);
   
    Showhelp('detsim.hlp','About Program');

    MyMenu.display;

    Eunit := cEunit;                         { Default energy units}

    inCMang := a;                     {initialize angle, momentum}
    inmoment:= b;

    partic.getmom(inmoment);
    partic.getang(inCMang);
    partic.istate(pizero,null);
    partic.fstate(photon,photon);

    partic.init;
    partic.rescale;

    evnthits.init;

    evnthits.detdraw;

   {Default no event generation}
    eventgen := False;
    eventno := 0;
    idum:= -1;                           {Random Number Initialization}
    rdum:= Urand(idum); rdum:=Urand(idum);


   SetWriteMode (CopyPut);
   ShowCursor;
   REPEAT
      WITH event DO WITH views[4] DO
      IF ExtendedKey  OR mouseClicked  THEN
      BEGIN
         IF mouseClicked THEN evnthits.getmouse;

         partic.getmom(inmoment);
         partic.getang(inCMang);


         SetWriteMode (CopyPut);
 {        IF (enabledraw) THEN  ProcEvt; }  { skip if function key}
         SetWriteMode (CopyPut);
      END;

      enabledraw:= True;
      IF (eventgen) THEN doeventgen;      {generate events}

      Selectviewport(7);SelectScale(7);

      CheckForEvents;
      IF HotKeys.Pressed(key) THEN INTERRUPT(key);
      IF MyMenu.Activated THEN HandleMenu;
   UNTIL QuitFlag;
   Mymenu.done;
   CupsDone;

END;
END.
 
 
