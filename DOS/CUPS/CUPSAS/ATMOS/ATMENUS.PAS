unit Atmenus;
 {sets up menus and help screens; handles them; calls physics routines}
  {modified 6/06/94}
interface

USES  crt, dos, graph, printer, CUPSMUPP,CUPS, CUPSfunc,CUPSproc,cupsgui, CUPSGrph, AtCommon,AToutput,ATInput,Atphys,AtSky;
 PROCEDURE SetUpInterface;
 PROCEDURE CREATEBUTTONS(var filename : string);
 PROCEDURE ResetGenHotKeys;
 PROCEDURE ResetMoveHotKeys;
 PROCEDURE MFile;
 PROCEDURE MseeStars;
 PROCEDURE buildAModel(M:integer; Var Ok:boolean); {carries out the model building}
 PROCEDURE findModelRadiation(m:integer;var ok: boolean); {evaluates radiation field}
 PROCEDURE MPlotWhat;
 PROCEDURE MHelp;
 PROCEDURE MDoModel(M: integer); {builds requested model}
 {--------------------------------------------}
IMPLEMENTATION

PROCEDURE SetUpInterface;
BEGIN
    MyMenu.Init;
   WITH MyMenu DO
   BEGIN
      
       column(1,'File');
       Row(1,1, 'About Cups');
       Row(1,2, 'About this program');
       Row(1,3, 'Configuration');
     {  Row(1,3, 'Read Model 1...'); }
     {  Row(1,4, 'Read Model 2...'); }
       Row(1,4, 'Save Model 1...');
       Row(1,5, 'Save Model 2...');
       Row(1,6, '------------------');
       Row(1,7, 'Exit program');

      Column(2, 'See Stars');
       row(2,1, 'On Sky');
       row(2,2, 'Spectrograph');

      column(3, 'Model 1');
       row(3,1,  'Specify Model 1');
       row(3,2,  'Build Model 1');
       row(3,3,  'List gas data');
       row(3,4,  'List radiation data');
       row(3,5,  'List parameters');
       row(3,6,  'Adjust Temperature');


      column(4, 'Model 2');
       row(4,1,  'Specify Model 2');
       row(4,2,  'Build Model 2');
       row(4,3,  'List gas data');
       row(4,4,  'List radiation data');
       row(4,5,  'List parameters');
       row(4,6,  'Adjust Temperature');


      column(5, 'Plot-what');
       row(5,1,  'Gas Structure');
       row(5,2,  'More Gas Structure');
       row(5,3,  'Radiation Field');
       row(5,4,  'More Radiation Field');
       row(5,5,  'Opacity');
       row(5,6,  'Plot Options');

       column(6, 'Help');
       row(6,1,   'Get Started');
       row(6,2,   'Select Stars');
       row(6,3,   'Specify Models');
       row(6,4,    'Plots');
       row(6,5,   'Adjust Temperature');
       row(6,6,   'Spectrum & Colors');
       row(6,7,   'Methods');
       row(6,8,   'Opacity');

       rowactivate(1,6,false);
       END; {with mymenu}

   WITH GenHotKeys DO BEGIN
      init(2);
      key[1] := 'F1-Help';
      key[2] := 'F10-Menu';
   END;

   WITH MoveHotKeys DO BEGIN
      init(6);
      KEY[1] := 'F1-Help';
      key[2] := 'LEFT-Left';
      key[3] := 'RIGHT-Right';
      key[4] := 'DOWN-Decrease';
      KEY[5] := 'UP-Increase';
      KEY[6] := 'F10-Menu';
   END;
   MoveHotkeys.backcolor :=GREEN;
END;  {Set up interface}

PROCEDURE CREATEBUTTONS(var filename : string);
VAR X,Y:REAL;
  C:INTEGER;
BEGIN
C:=FINDNUMCONST(filename);
ConstBut.init;
ShowMod1But.init;
ShowMod2But.init;
ShowbbBut.init;
LockScaleBut.init;

 WITH VIEWS[SETUP] DO
 BEGIN                {this needs to be rewritten to use numconstellatioins}
constBut.create(1,0.12,0.43,consName[1]);
constBut.create(2,0.12,0.38,consName[2]);
constBut.create(3,0.12,0.33,consName[3]);
constBut.create(4,0.12,0.28,consName[4]);
constBut.create(5,0.12,0.23,consName[5]);
  END;
with views[SpectOpt] do
     BEGIN
     showMod1but.create(1,0.25,0.90,'Show ');
     showMod1but.create(2,0.35,0.90,'Dont show MODEL 1');
     showMod2but.create(1,0.25,0.85,'Show ');
     showMod2but.create(2,0.35,0.85,'Dont show MODEL 2');
     showBBbut.create(1,0.25,0.80,'Show ');
     showBBbut.create(2,0.35,0.80,'Dont show BLACK BODY');
     lockScaleBut.create(1, 0.45,0.75,'Lock Scales');
     lockScaleBut.create(2,0.25,0.75,'Auto Scale');
     ClearBut.create(1,0.25,0.70,'Auto Clear');
     ClearBut.create(2,0.45,0.70,'Keep Graphs');

     END;

tempSlider.init;
TempSlider.create(1,3000,50000,ESBBTemp,0.22,0.65,0.7,0,'3,000K','50,000K','Black Body Temp.',false);

END;
  {=============Utilities======================}
PROCEDURE ResetGenHotKeys;

BEGIN
movehotkeys.clear;
genHotKeys.display;
END;
   {---------------------------------------------------------------------------}

PROCEDURE ResetMoveHotKeys;
var oldcolor:word;
BEGIN
     genhotkeys.clear;
     MoveHotKeys.display;
END;


{=======================Control Procedures=================================}
PROCEDURE BuildAModel(M:integer; var ok:boolean);
BEGIN
modsel:=m;
ok:=false;
buildmodel;
if models[m].gasbuilt = true then
     ok:=TRUE else beep;

END; {buildAModel}

{--------------------------------------------------------------}
 PROCEDURE FindModelRadiation(M:integer; var ok:boolean);
BEGIN
modsel:=m;
ok:=false;
If models[m].gasbuilt = true then
     Findradiation;

If models[m].radfound then
   okay :=true
   else beep;
   
END; {buildAModel}

{--------------------------------------------------------------}

{=================  Menu Response handling Procedures ================}

PROCEDURE MFILE;
   {------------------------------}
   PROCEDURE WriteToDisc(myModel:integer);
    VAR
   fname,firststr, secstr:string;
   datafile: text;
   i:integer;

 
   BEGIN
   with models[myModel] do begin
        fname := concat(name,'.dat');
      
       ASSIGN(datafile,fname);
       REWRITE(datafile);

       WRITELN(datafile,concat('Data for stellar atmosphere model in file: ', fname));
       WRITELN(datafile,'Model ' + ScNumStr(Modsel,0) +'  Parameters');
       WRITELN(datafile,'Model name: '+ name);
       WRITELN(datafile,'Surface Temp.(Te) = '+ numstr(teff,7,1)+'K; Surface gravity = '+num2str(dyngrav,5)+' cm/sec^2');
       WRITELN(datafile,'Solar units: M = '+ ScNumStr(mass,1)+ '; R = '+ ScNumstr(rad,1)+ '; Luminosity = '+scNumStr(lum,1));
       WRITELN(datafile,'Distance = '+ scNumstr( Ly_distance,1) + ' light years');
       WRITELN(datafile,'mag(app. visual) = '+ScNumStr(MV_App,2) + ';  B-V = '+ ScNumStr(B_V,2));
       WRITELN(datafile,'Mag(Abs. visual) = '+ ScNumStr(MVIS,2)+'; Mag(bolometric) = '+scNumStr(MBOL,2));
       WRITELN(datafile,'Abundances by number:');
       WRITELN(datafile, 'H(X) = '+ scNumstr(habund,4)+ '; He(Y) = '+ scNumStr(heabund,4)+
             '; Met(Z) = ' + ScNumStr(metabund,4));
	if inclradpress then
		WRITELN(datafile,'Includes Rad. Pressure')
		   else WRITELN(datafile,'Radiation pressure not included');
        case modtype of
		      TCOnst:
	              firststr := 'T = Const.';
	              radEq:
		      firststr := 'Radiative Eqm.';
	              IncCon:
		      firststr := 'Incl. Conv.';
                           end;
        case absorber of
			gray:
	             secStr := 'Gray Opacity';
			HydHe:
				    secStr := 'H + He opacity';
			hyd:
			       secStr := 'Hydrogen opacity';

			end;
	WRITELN(datafile, firststr + '   '+ secstr);

	if (absorber = gray) then
	  WRITELN(datafile,'Epsilon = '+ numstr(GrEps,6,4));
        end;{with model}

         WRITELN(datafile, ' ');
         WRITELN(datafile, 'Gas data(cgs)');
 	 WRITELN(datafile, '  s    Tau       T       P      V     kappa  mmwt ConFlux/Tot  MachNo');
	 WRITELN(datafile, '       Depth(cm) adgrad radgrad       Hion   Heion   He+ion ');

         i:=1;

         repeat
	    with models[MyModel].shell[i] do
	    begin
            WRITELN(datafile,'  '+ScNumstr(i,0)+'  '+ scnumstr(vari.value(tau),1) + '  '+scNumstr(vari.value(temp),3) + '  '+
            scNumstr(vari.value(pres),2)+'  '+scNumstr(vari.value(volu),2)+'  '+scNumStr(vari.value(kapp),2)+'  '
                 + scNumStr(vari.value(mmwt),2) +'  '+
                 scNumStr(vari.value(conFlux),2)+'  '+scNumStr(vari.value(MachNo),2));

            WRITELN(datafile,'     ' + scNumstr(-vari.value(height),1)+'  '+scNumStr(vari.value(adgrad),2)+'  '+
            scNumStr(vari.value(radgrad),2)+'  '+
            ScNumStr(vari.value(Hion),1)+'  '+scNumStr(vari.value(Heion),1)+'  '+scNumStr(vari.value(HePion),1));
            i:= i + 1;  end;
         until (i > models[modsel].numshells);
       {  end;}{with shell}

       WRITELN(DATAFILE,' ');
       WRITELN(DATAFILE,'Radiation data');
       WRITELN(DATAFILE,'Surface fluxes at each wave number:');

       WRITELN(DATAFILE, ' Wave numbers(inverse micrometers) | Surface Fluxes(erg/cm^2/sec):');
                i := 1;
                writeln(datafile);
                WRITE(DATAFILE,'WN ');
        	repeat
		i := i + 1;
	        WRITE(DATAFILE,'    ', scNumStr(WNLIst[modsel,i],2));
		until (((i mod 9) = 0) or (i = numfreqs));
                writeln(datafile);
        {newline}
        	i := 1;
                WRITE(DATAFILE,'F  ');
		repeat
		i := i + 1;
              	WRITE(DATAFILE,'  ',ScNumStr(models[modsel].emflux[i],2));
		until (((i mod 9) = 0) or (i = numfreqs));
                writeln(datafile);
		if i < numfreqs then {we have not finished list yet}
		begin
        	i := 9;
                WRITE(DATAFILE,'WN ');
		repeat
                i := i + 1;
		WRITE(DATAFILE,'    ',ScNUmStr(WNLIst[modsel,i],2));
		until (((i mod 18) = 0) or (i = numfreqs));
                writeln(datafile);
		i := 9;
                WRITE(DATAFILE,'F   ');
		repeat
                i := i + 1;
               	WRITE(DATAFILE,'  ',scNumStr(models[modsel].emflux[i],2));

		until (((i mod 18) = 0) or (i = numfreqs));
		end;
                i:=1;
                writeln(datafile);
               WRITELN(DATAFILE,' ');
               WRITELN(DATAFILE, 'Shell    Tau      H        B         S        J       I+      I- ');
               writeln(datafile);
               	repeat
		with models[myModel].shell[i] do
		begin
                WRITELN(DATAFILE,' ',ScNumstr(i,0),'   ',scnumstr(vari.value(tau),2),'   ',scNumstr(radn.value(mean,flux),2)+
                '   ',scNumstr(radn.value(mean,Planck),2),'   ',scNumstr(radn.value(mean,sfn),2)
                  ,'   ',scNumStr(radn.value(mean,jay),2) +
                  '   ',scNumStr(radn.value(mean,iup),2),'   ',scNumStr(radn.value(mean,idn),2));

                end; {with shell}
		i := i + 1;

                until (i > models[myModel].numshells);

      CLOSE(datafile);
       Announce('The data are now in file: '+fname+
                '     <anykey> to continue');


  END  ;
    {--------------------------------}

BEGIN
 gMode:=Menumode;
  resetGenHotkeys;
 WITH MyMenu DO CASE rowChosen OF
  1:   AboutCups;

  2:  ShowHelp('Atmoshlp.hlp','AboutThisProgramHelp');

  3: configuration;

  4: BEGIN {Save Model 1 }
      Modsel:=1;
      WriteToDisc(Modsel);
     { Pause; }
  {    mymenu.display;}
      END;

  5: BEGIN {Save Model 2 }
       Modsel :=2;
       WriteToDisc(Modsel);
       { Pause;}
        mymenu.display;
      END;

  7:   QuitNow := true;

 END; {CASE}

END;{MfILE}

  {-------------------------------------------------}
PROCEDURE MSeeStars;    {puts current number of model into menu}
 var oldcolor:word;
BEGIN
openviewport(9); {to erase scale}
CloseViewPort(9); {whole screen}
 WITH MyMenu DO CASE rowChosen OF
  1: BEGIN {OnSky}
     gMode := SelectStarsMode;
     ResetMoveHotkeys;
     DoShowStars;
     END;

  2: BEGIN {Spectrum}
     gMode:=SpectrumMode;
     firstplot:=true;
     OpenSpectrumPlot;
     DoSpectrumPlot;
     ResetGenHotkeys;
    
     END;

 END; {CASE}

END;   {MSeeStars}

{---------------------------------------------------------------------------}
PROCEDURE MDoModel(M:integer);
BEGIN
CloseViewPort(9); {whole screen}
 ResetGenHotkeys;
  CASE myMenu.rowChosen of
            1: BEGIN

               DoSpecifyAModel(M,Changed);
               If changed then ModelBuilt[M] :=false;        {Specify}
               gMode:=MenuMode;
               BuildAModel(M, okay);
               If Okay then
                  begin
                       ModelBuilt[M]:=true;
                       FindModelRadiation(m,okay);
                       PlotModel[M]:=true; {enable plotting}
                       mymenu.rowchecked(M+2,7,true);
                       showParameterlist(m);
                  end;

               END; {ITEM1}
            2: BEGIN   {Build}
               gMode:=BuildMode;
               BuildAModel(M, okay);
               If Okay then
                  begin
                       ModelBuilt[M]:=true;
                       FindModelRadiation(m,okay);
                       PlotModel[M]:=true; {enable plotting}
                       mymenu.rowchecked(M+2,7,true);
                       ShowParameterlist(m);
                  end;
               END;{ITEM 2}

            3: BEGIN {LIST GAS}
           
               showGasList(M);
               END;

            4: BEGIN {LIST RADIATION}
              
               ShowRadiationList(M);
               END;

            5: BEGIN {LIST PARAMETERS}
              
               ShowParameterList(M);
               END;


            6: BEGIN
               gMode:=adjustTMode;      {AdjustT}
               genhotkeys.clear;
               ResetMoveHotKeys;
               ShowTempProfile(M,Changed);

               END;


       END; {CASE}
end; {mdoModel}

{---------------------------------------------------------------------------}
 
PROCEDURE MPlotWhat;
VAR
fname:string;
datafile: text;
i,tempButton:integer;
getAxesScrn:TInputScreen;
BEGIN
 CloseViewPort(9); {whole screen}
 resetGenHotkeys;

  WITH MyMenu DO CASE rowChosen OF

   1: BEGIN  { Plot gas structure}
      gdoingMore:=false;
      DoStructurePlot(gdoingMore);
      gmode:=PlotMode;
      END;

   2: BEGIN  {More Plot gas}
      gdoingMore:=true;
      DoStructurePlot(gdoingMore);
      gmode:=PlotMode;
      END;

  3: BEGIN  {Plot Radiation Field}
      gdoingMore:=false;
      DoRadBeamsPlot(gdoingMore);
      gmode:=PlotMode;
      END;

  4: BEGIN  {Plot More Radiation}
     gdoingMore:=true;
     DoRadBeamsPlot(gdoingMore);
     gmode:=PlotMode;
     END;

  5: BEGIN  {Plot monochromatic opacity at various depths }
     gMode:=OpacityMode;
     DoOpacityPlot;
     END;

  6: BEGIN {CHANGE AXES}
     with getAxesScrn do begin
      init;
      defineInputPort(0.0,0.6,0.3,0.9);
      loadLine(' ');
      loadline('Select X-coordinate:');
      LoadLine('  #1 Optical Depth  #1 Geometrical Depth');
      LoadLine('Y axis of Structure Plot:');
      loadLine('  #2 Logarithmic  #2 Linear');
      LoadLine('Y Scaling for different models:');
      loadLine('  #3 Both models scaled to fill graph');
      LoadLIne('  #3 Both have same scale');
      loadLIne(' ');
      LoadLIne('Select models to show in plots');
      LoadLine(' #T  Model 1    #T  Model 2 ');
      loadLine(' ');
      Loadline(' [OK]        [Cancel]');

      setBoolean(7,PlotModel[1]);
      Setboolean(8,plotModel[2]);
      If structureRec.tauscale then setRadioButton('1',1)
         else setRadioButton('1',2);
      if structureRec.logarithmic then  setradiobutton('2',1)
        else  setradiobutton('2',2);
      if SameScale then  setradiobutton('3',2)
        else  setradiobutton('3',1);
      AcceptScreen;
      if ModelBuilt[1] then  PlotModel[1]:=getBoolean(7)
         else PlotModel[1]:=false;
      if modelBuilt[2] then  PlotModel[2]:=getboolean(8)
         else plotModel[2]:=false;
      structurerec.tauScale:=(getRadioButton('1')=1);
      RadRec.tauScale:=structureRec.tauscale;
      TempButton:=getradioButton('2');
      structureRec.logarithmic :=(tempButton=1);
      TempButton:=getradioButton('3');
      SameScale :=(TempButton<>1);
      Done;
     end; {with}

  END;

  END; {CASE menu}
  ResetgenHotkeys;

END; {mPlotWhat}

{---------------------------------------------------------------------------}


PROCEDURE MHelp;
BEGIN
   gmode:=menumode;
   case MyMenu.rowChosen OF
    1: ShowHelp('atmoshlp.hlp','GetStartedHelp');
    2: ShowHelp('atmoshlp.hlp','SelectStarsHelp');
    3: ShowHelp('atmoshlp.hlp','SpecifyModHelp');
    4: ShowHelp('atmoshlp.hlp','PlotsHelp');
    5: ShowHelp('atmoshlp.hlp','AdjustTHelp');
    6: ShowHelp('atmoshlp.hlp','SpectrumHelp');
    7: ShowHelp('atmoshlp.hlp','MethodsHelp');
    8: ShowHelp('atmoshlp.hlp','OpacityHelp');
   END; {CASE}
END;   {MHelp}
{------------------------------------------------------------------------}


END.