
PROGRAM AtmosPC;

{******************************************************************}
{*    Computes the Structure of a Stellar Atmosphere              *}
{*    in Thermal and Hydrostatic Equilibrium and                  *}
{*    synthesizes spectrum.                                       *}
{*                                                                *}
{*     C.A. Whitney    Copyright @ 1994                           *}
{*        CUPS Project. PCAtmos version 078; June 20, 1994        *}
{******************************************************************}



USES  crt, dos, graph, printer, CUPSMUPP,CUPS, CUPSfunc,CUPSproc,cupsgui, CUPSGrph, AtCommon, Atmenus, AtOutPut, AtInPut,AtSky;
{*-----               Initialization                                  -----*}
{---------------------------------------------------------------------------}


{--------------------------------------------------------------------  }
 function ClickedInViewPort(vp : integer) : boolean;
   begin
       with views[vp] do
       ClickedInViewPort := (event.x>vx1) and (event.x<vx2) and
                    (event.y>vy1) and (event.y<vy2);
    end;

 {======================================}
PROCEDURE HandleMenu;
 BEGIN

  WITH mymenu DO CASE colChosen OF

    1: MFile;
    2: MseeStars;
    3: BEGIN
       Modsel:= 1;
       MDoModel(modsel);
       END;
     4:BEGIN
       Modsel:=2;
       MDoModel(modsel);
       END;
     5: MPLotWhat;
    6: MHelp;

    END; {CASE}

END;  {HandleMenu}


{========================================}

PROCEDURE InitModels;

VAR
 i :Integer;

BEGIN
showshell:=[2,5, 8, 10,12,15];
wn0:=2.0; {normalizing wave number for opacity}
Samescale:=true;
Modsel:=1;
gMode:=MenuMode;
QuitNow:=false;
rebuildModel:=false;
PlotModel[1]:=false;
Plotmodel[2]:=false;
isPrinting:=false;
for i:=0 to maxshells do
    begin;
       models[1].shell[i].init;
       models[2].shell[i].init;
    end;
 with models[1] do begin
       name:='Sun';
       	Taubot := 5.0;
	Tautop := 0.01;
        muZero := 1 / sqrt(3);
       mass:=1;
       rad:=1;
       lum:=1;
       modtype:=RadEq; {Options: TConst, RadEq, IncCon}
       absorber:=Hyd; {Options: gray, StepF, sHyd, HydHe}
       wstep:=2.74;
       stepFact:=5;
       Numshells:=15;
       habund:=0.70;
       Heabund:=0.28;
       Metabund:=0.02;
       fluxlocked:=false;
       gasbuilt:=false;
       radfound:=false;
       Ly_Distance:=32;
       Greps:=1.0;
       plotcolor:=yellow;
      end;

with models[2] do begin
      name:='Vega';
      Taubot := 5.0;
      Tautop := 0.01;
      muZero := 1 / sqrt(3);
       mass:=2;
       rad:=3;
       lum:=10;
       modtype:=radEq;
       absorber:=GRAY;
        wstep:=2.74;
        stepFact:=5;
       Numshells:=15;
       habund:=0.70;
       Heabund:=0.28;
       Metabund:=0.02;
       fluxlocked:=false;
       gasbuilt:=false;
       radfound:=false;
       Ly_Distance:=32;
       Greps:=1.0;
       plotcolor:=lightmagenta;
      end;

    for i:=1 to 3 do showSpectrum[i]:=true;
  autoClear:=true; {spectrum plots}
  firstPlot:=true;
  ESBBTemp := 5000;
  ESUpperWN := 5.00; {11 = Lyman limit for Hydrogen}
  ESLowerWN := 0.01;
  {Set pass bands}
  PassBand[highband].lowerWN := 2.15;
  PassBand[highband].CenterWN := 2.27;
  PassBand[highband].UpperWN := 2.39;
  PassBand[highband].response := 1;
  PassBand[Lowband].lowerWN := 1.70;
  PassBand[Lowband].CenterWN := 1.82;
  PassBand[Lowband].UpperWN := 1.94;
  PassBand[Lowband].response := 1.0;
  solarFlux := SolarLum / solarrad / solarrad / fourpi / fourpi; {Eddington flux, H}
{These additive constants are set to  force agreement between  solar model produced by this}
{program and  observations of the sun.  }
{For example, to find constant for B_V using another solar value, add B_V(new)-0.65}
 B_Vconst := -0.64;{solar = 0.65}
 MBOLconst := 29.00;{solar = 4.75}
 MVISconst := 31.39;{solar = 4.83}


structureRec.tauscale:=true; {for plotting}
radrec.tauscale:=true;
structureRec.logarithmic:=true;
radRec.logarithmic:=false;
ModelBuilt[1]:=false;
ModelBuilt[2]:=false;
ShowSpectrum[3]:=true; {black body}
conSelected:=1;
shellsel:=models[1].numshells div 2;   {Default for AdjustTemp. window}

StarFile := 'Stardata.rec';
ReadConsData(StarFile,ConSelected); {conSelected set by radio buttons}

END;{INit}
{-----------------------------------------------}
PROCEDURE BuildDefaultModel(m:integer);
BEGIN
Modsel:=m;
BuildAModel(modsel,okay); {build the model and find its radiation}
Modelbuilt[m]:=true; {tell menu okay to plot this model}
 If Okay then
                  begin
                       ModelBuilt[Modsel]:=true;
                       FindModelRadiation(modsel,okay);
                       PlotModel[modsel]:=true; {enable plotting}
                       mymenu.rowchecked(Modsel+2,7,true);
                      { ShowParameterlist(modsel); }
                  end;
END; {BuildFirstModel}
{--------------------------------------------}
PROCEDURE HandleGenHotKeys(key:byte);
     BEGIN
        CASE key OF
        1 :Case gMode OF
                MenuMode: showHelp('atmoshlp.hlp','GetStartedHelp');
                SelectStarsMode: showHelp('atmoshlp.hlp','SelectStarsHelp');
                specifyModMode: showHelp('atmoshlp.hlp','SpecifyModHelp');
                SpectrumMode: showHelp('atmoshlp.hlp','SpectrumHelp');
                AdjustTMode: showHelp('atmoshlp.hlp','AdjustTHelp');
                PlotMode:showHelp('atmoshlp.hlp','PlotsHelp');
                opacitymode:showHelp('atmoshlp.hlp','OpacityHelp');
         end; {case}

       2:   IF MyMenu.chosen THEN HandleMenu;
       END; {case}

     END; {HandleGenHotKeys}
{===============================================}

 PROCEDURE HandleMoveHotKeys(key:byte);
          VAR TTEMP:REAL;
          BEGIN
               IF KEY = 1 THEN  {help}
                  CASE gmode of
                  selectstarsmode:  showHELP('atmoshlp.hlp','SelectStarsHelp');
                  AdjustTmode:     showHelp('atmoshlp.hlp','AdjustTHelp');
                  END; {Case}

               if gmode=adjustTmode then
                  BEGIN

                     CASE KEY OF
                     2: BEGIN    {select another shell}
                        ShellSel:=ShellSel-1;
                        if shellSel<1
                           then ShellSel:=models[modsel].Numshells;
                        END;
                     3: BEGIN
                        ShellSel:=ShellSel+1;
                        if shellSel>models[modsel].numshells
                           then ShellSel:=1;
                       END;
                     4: BEGIN    {decrease temp. at the shell}
                        TempArray[modsel,ShellSel]:=0.98*TempArray[modsel,ShellSel];
                        END;
                     5: BEGIN {increase temp at the shell}
                        TempArray[modsel,ShellSel]:=TempArray[modsel,ShellSel]/0.98;
                        END;
                     END; {CASE}

                     ShowTempProfile(modsel,changed); {refresh screen; in AtInput}
                     print(2,18,concat('Shell No.: '+numstr(ShellSel,3,0)));
                     print(18,18,concat('Temp. = '+numstr(tempArray[modsel,ShellSel],7,0)));
                  END;

          END; {HandleMoveHotKeys}
{======================}

PROCEDURE HANDLEMOUSECLICKED;
VAR frame,s:integer;
inSky,InCM,DoEsc:boolean;
xr,yr:real;

BEGIN
     if (( gMode=selectstarsMode)) then
     BEGIN
     {FIND WINDOW AND THEN POSTION OF NEAREST STAR;
     Reset StarSelected}
     insky:=false;
     inCM:=false;
            mousePosn(xr,yr,Sky,sky,InSky);
            if not inSky then
               MousePosn(xr,yr,CM,CM,inCM);
            frame:=0;
            if InCM then  frame:=CM else if InSky then frame:=sky;

            selectscale(frame);
            if frame<>0 then
            if  FindSelectedStar(frame,xr,yr,s)=true then
               BEGIN
               starSelected:=s;
               doShowStars;
               END;
     END;

     if gMOde=adjustTMode then
     BEGIN
          RebuildModel:=False;
          Doesc:=false;
          mouseposn(xr,yr,escBut,escBut,doEsc);
          if doEsc then
             BEGIN
             CLOSEVIEWPORT(TAdjust);
             closeviewport(EscBut);
             closeviewport(AcceptBut);
             openviewport(9);
             closeviewport(9);
             END;
          mousePosn(xr,yr,acceptBut,acceptBut,RebuildModel);
           IF RebuildModel then
              BEGIN
              closeviewport(acceptBut);
              graphbackcolor:=black;  {invert button}
              openviewport(acceptBut);

              for s:=1 to models[modsel].numshells do  {load new temperatures}
                   begin
                        models[modsel].shell[s].vari.put(temp,TempArray[modsel,s]);
                   end;
              BuildAModel(modsel,okay); {build the model and find its radiation}
              for s:=1 to models[modsel].numshells do  {load new temperatures}
                   begin
                        temparray[modsel,s]:=models[modsel].shell[s].vari.value(temp);
                   end;

              If Okay then
                  begin
                       ReBuildModel:=False;
                       FindModelRadiation(modsel,okay);
                       PlotModel[modsel]:=true; {enable plotting}
                       ShowTempProfile(modsel,changed);  {reOpen and plot}
                  end;
              END
              Else {reload old model into TempArray}
               for s:=1 to models[modsel].numshells do
                   tempArray[modsel,s]:=  models[modsel].shell[s].vari.value(temp)
     END; {adjustTMode}

END;
{===========================================================================}
(*-----              ****    MAIN PROGRAM    ****                     -----*)
{---------------------------------------------------------------------------}

{This program controls the flow of the main menu}
VAR
 Error :Integer;
 key   :Byte;
 keychar:char;
 i,s,frame:integer;
 InCM,InSky,IsChanged,doEsc,DoRebuild:BOOLEAN;
 x,y:integer;
 xr,yr:real;

BEGIN {Main}
 CUPSinit;
 InitModels;
 Showhelp('Atmoshlp.hlp','AboutThisProgramHelp');
 SetUpInterface;
 CREATEBUTTONS(StarFile);  {CREATES BUTTONS FOR SELECTING CONSTELLATIONS
                    IT READS 'stardata.REC' FOR FIND ARRAY OF NAMES
                    FOR BUTTON LABELS.}
 setUpViewPorts; {see Output.pas}
 message(' I am building first model.');
 BuildDefaultModel(1);
 message(' I am building second model.');
 BuildDefaultModel(2);
 gMode := SelectStarsMode;

 DoShowStars;
 MyMenu.display;
 GenHotKeys.display;
 REPEAT

     CheckForEvents;
     if MoveHotKeys.pressed(key)
             then handleMoveHotKeys(key);

     if GenHotKeys.pressed(key) then
        HandleGenHotKeys(key);

     if event.mouseclicked then
        handleMouseClicked;

     if GenHotKeys.pressed(key)
        Then HandleGenHotKeys(key);

     if gmode=selectstarsMode then
       if constbut.changed then
         begin
              starSelected:=0; {reset selection}
              conSelected := constbut.number;
              ReadConsData(StarFile,ConSelected); {conSelected set by radio buttons}
              DOSHowStars;
         end;

     if gMode=spectrumMode then
        BEGIN
             isChanged:=false;{initialize}
             IF ShowMod1But.changed then
                BEGIN
                ISCHANGED:=TRUE;
                    if showMod1But.number=2
                    then showSpectrum[1]:=false
                    else showSpectrum[1]:=true;
                END
                ELSE
                if showMod2But.changed then
                BEGIN
                ISCHANGED:=TRUE;
                     if showMod2But.number=2
                     then showSpectrum[2]:=false
                     else showSpectrum[2]:=true;
                END
                ELSE
                if ShowBBBut.changed then
                BEGIN
                     ISCHANGED:=TRUE;
                     if showBBBut.number=2
                     then showSpectrum[BBody]:=false
                     else showSpectrum[BBody]:=true;
                END
                ELSE
                IF TempSlider.changed then
                   BEGIN
                        ISCHANGED:=TRUE;
                        esbbtemp:=tempslider.value(1);
                   END
                ELSE
                IF CLEARBUT.CHANGED THEN
                   BEGIN
                        ISCHANGED:=TRUE;
                        IF clearBut.number=2 then
                           autoclear:=false
                           else
                           autoclear:=true;
                   END;

         If lockScaleBut.changed then
            BEGIN
            IsChanged:=true; firstplot:=true;
            if lockScaleBut.number= 2
               then lockScale:=false
               else lockscale:=true;
            END;

         if IsChanged then
            BEGIN
                 if autoclear then openSpectrumPlot; {ERASE THE OLD GRAPHS}
                 DoSpectrumPlot;
            END;
        END;

     IF MyMenu.activated THEN HandleMenu;
 UNTIL quitNow; {repeat forever}
 for i:=0 to maxshells do
     begin
     models[1].shell[i].done;
     models[2].shell[i].done;
     end;
 CONSTBUT.DONE;
 tempslider.done;
 showmod1but.done;
 showMod2but.done;
 ShowBBBut.done;
 ClearBut.done;
 CUPSdone;
END.
