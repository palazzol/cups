
unit AtOutput;
{---------------------------------------------------------------------------}
{----            Output Graph Procedures                      -----*)
{------------------- June 14, 1994   ----------------------------}
{---------------------------------------------------------------------------}


interface

USES  crt, dos, graph, printer, CUPsMupp, CUPS, CUPSfunc,
 CUPSProc,cupsgui, CUPSGrph, AtCommon,AtPhys;


PROCEDURE SetUpViewPorts;
PROCEDURE OpenStructurePlot(doingMore:boolean);
PROCEDURE OpenSpectrumPlot;
PROCEDURE OpenSpectrumOptions;
PROCEDURE OpenRadBeamsPlot(doingMore:boolean);
PROCEDURE OpenOpacityPlot;

PROCEDURE DoStructurePlot(doingMore:boolean);
PROCEDURE DoSpectrumPlot;
PROCEDURE DoRadBeamsPlot(doingMore:boolean);
PROCEDURE DoOpacityPlot;
PROCEDURE ShowParameterList(M:integer);
PROCEDURE ShowRadiationList(M:integer);
PROCEDURE ShowGasList(M:integer);
implementation
   
{---------------------------------------------------------------------------}
{===================  Open Screen Procedures  ======================}
{---------------------------------------------------------------------------}


{---------------------------------------------------------------------------}
PROCEDURE SetUpViewPorts;
var i:integer;
BEGIN               {l,r,b,t}
 DefineViewport(1, 0.1, 0.49, 0.15, 0.50);  {bottom left}
 DefineViewport(2, 0.1, 0.49, 0.58, 0.93);  {top left}
 DefineViewport(3, 0.61, 1.00, 0.15, 0.50);  {bottom right}
 DefineViewport(4, 0.61, 1.00, 0.58, 0.93);  {top right}

 DefineViewport(5, 0.1, 0.49, 0.15, 0.50);  {bottom left}
 DefineViewport(6, 0.1, 0.49, 0.58, 0.93);  {top left}
 DefineViewport(7, 0.61, 1.00, 0.15, 0.50);  {bottom right}
 DefineViewport(8, 0.61, 1.00, 0.58, 0.93);  {top right}
 DefineViewport(9, 0.0, 1.0, 0.07, 0.95);  {whole screen}
 DefineViewport(10, 0.0, 1.0, 0.35, 0.93);  {Upper portion of screen}
 for i := 1 to 10 do DefineScale(i, 0, 1, 0, 1);
END;
{=====================================}

FUNCTION MAXOF(A,B:REAL):REAL; {returns the greater}
BEGIN
  if A>=B then
  maxof:=a
  else
  maxof:=b;
END;

FUNCTION MinOF(A,B:REAL):REAL; {returns the greater}
BEGIN
  if A<B then
  minof:=a
  else
  minof:=b;
END;
{================================}

PROCEDURE SetMyScales(f:integer); {Called by OpenRadBeams to initialize
possible scales to be selected later}
BEGIN
 if PLotmodel[1] then  {set xr[f] to max relevant value; st corresp. to model 1}
    if not PlotModel[2] then
       DefineScale(f,0,xr[1],sb[1,f],st[1,f])
       else
       if abs(xr[2])>abs(xr[1]) then
          DefineScale(f,0,xr[2],sb[1,f],st[1,f])
          else
          DefineScale(f,0,xr[1],sb[1,f],st[1,f]);

 if PlotModel[2] then  {define f+8 with
    same choice of xr[f] but st corresp. to model 2}
     if not PlotModel[1] then
       DefineScale(f+8,0,xr[2],sb[2,f],st[2,f])
       else
       if abs(xr[2])>abs(xr[1]) then
          DefineScale(f+8,0,xr[2],sb[2,f],st[2,f])
          else
          DefineScale(f+8,0,xr[1],sb[2,f],st[2,f]);

END;
{====================================}

PROCEDURE SetMyAxes(s:integer);
var ystep:real;
    scale:integer;
BEGIN
with GraphScales[s] do
   begin
      scale := ScaleNumber;
      ystep := tickSpace(yMax-yMin);
      if ystep>1e9 then
        begin
           DefineScale(20,xmin,xmax,ymin/1e9,ymax/1e9);
           with GraphScales[20] do ystep := tickSpace(yMax-yMin);
        end;
         Axis(0.0,0.0,TickSpace(abs(xmax-xmin)),ystep);
        scaleNumber := Scale;
     end;

END;
{==================================}
PROCEDURE PutLabels(f:integer);
BEGIN
    PutLabel(left,Labelstr[1,f]);
    putLabel(bottom,LabelStr[2,f]);
END;
{=================================}
PROCEDURE GetXScale(m:integer;myRec:windowRec);
BEGIN
{get limits of each model}
with models[m] do begin 
 xl[m]:=0;
 if MyRec.tauscale then
 xr[m]:=models[m].shell[numshells].vari.value(tau)
 else
  xr[m]:=models[m].shell[numshells].vari.value(height)/1e5; end; {with models}
END;
{==================================}
PROCEDURE GetYScale(m,f:integer;myData:integer);
var temp1,temp2:real;
i:integer;
BEGIN
with models[m] do
BEGIN

if myData=StructData then
with StructureRec do begin

temp1:=0;
for i:=1 to numshells do        {scan for largest value}
if shell[i].vari.value(gVarId[f])> temp1
   then temp1:=shell[i].vari.value(gVarId[f]);

                     {scan for smallest value}
temp2:=1e25;

for i:=1 to numshells do
if shell[i].vari.value(gVarId[f])< temp2
   then temp2:=shell[i].vari.value(gVarId[f]);

if (logarithmic and (not gdoingMore)) then
     BEGIN
     if temp1 >0 then
       st[m,f]:=  Log10(temp1) {find upper scale}
      else st[m,f]:=0;
      if temp2>0 then
       sb[m,f]:= Log10(temp2)
      else sb[m,f]:=0;
      END ;
 If ((not logarithmic) or (logarithmic and gdoingMore)) then {Natural values}
      BEGIN
       st[m,f] := temp1; {find upper scale}
       sb[m,f]:= temp2;
      END;
if ((gVarid[f]=Hion) or ( gVarid[f]=Heion))
then begin st[m,f]:=1.0; sb[m,f]:=0;end;
if ((gvarId[f]=adgrad) or (gvarId[f]=radgrad))
then begin sb[m,f]:=0; end;

if gvarId[f]=radgrad then
st[m,f]:=trunc(st[m,f]+1);
END {with StructureRec}

ELSE IF  Mydata=RadnData then

with RadRec do begin
{scan for largest value}
temp1:=0;
for i:=1 to numshells do
if shell[i].radn.value(mean,gVarId[f])> temp1
   then temp1:=shell[i].radn.value(mean,gVarId[f]);
if temp1<=0 then temp1:=1;
{scan for smallest value}
temp2:=1e25;

for i:=1 to numshells do
if shell[i].radn.value(mean,gVarId[f])< temp2
   then temp2:=shell[i].radn.value(mean,gVarId[f]);

if logarithmic then
     BEGIN
     if temp1 >0 then
       st[m,f]:=  Log10(temp1) {find upper scale}
      else st[m,f]:=0;
      if temp2>0 then
       sb[m,f]:= Log10(temp2)
      else sb[m,f]:=0;
      END
    ELSE {Natural values}
      BEGIN
       st[m,f] := temp1; {find upper scale}
       sb[m,f]:= 0;    {set bottoms all to zero}
      END;

end;{if radn data}
END;{with models}
END;
{=======================================}
PROCEDURE  OpenStructurePlot(doingMore:boolean);
var
m,f,fs,fLower,fUpper:integer;
stmax: real;
{-----------------}
PROCEDURE InitArrays;
VAR I,J:INTEGER;
BEGIN

gVarId[1]:=pres;  {tell program which data to plot in each frame}
gvarId[2]:=temp;
gVarId[3]:=dens;
gVarId[4]:=kapp;
gVarId[5]:=adGrad;
gVarId[6]:=radgrad;
gVarId[7]:=Hion;
gVarId[8]:=Heion;
for i:=1 to 2 do
    for j:=1 to 8  do
BEGIN
 st[I,J]:=0;
 sb[I,J]:=0;
END;
if structureRec.logarithmic
then begin
LabelStr[1,1]:='Log(Pressure-dyn/cm^2) ';    {for frame scales}
LabelStr[1,2]:='Log(Temperature-K)';
LabelStr[1,3]:='Log(Density-gm/cm^3)';
LabelStr[1,4]:='Log(Opacity-cm^2/gm)';
LabelStr[1,5]:='Ad. gradient ';    
LabelStr[1,6]:='Rad. gradient';
LabelStr[1,7]:='H ionization';
LabelStr[1,8]:='He ionization';
end
else
begin
LabelStr[1,1]:='Pressure-dyn/cm^2 ';    {for frame scales}
LabelStr[1,2]:='Temperature-K';
LabelStr[1,3]:='Density-gm/cm^3';
LabelStr[1,4]:='Opacity-cm^2/gm';
LabelStr[1,5]:='Ad. gradient ';
LabelStr[1,6]:='Rad. gradient';
LabelStr[1,7]:='H ionization';
LabelStr[1,8]:='He ionization';
end;
LabelStr[2,2]:=' ';
LabelStr[2,4]:=' ';
LabelStr[2,6]:=' ';
LabelStr[2,8]:=' ';

if structureRec.tauscale  then
begin
LabelStr[2,1]:='Optical depth';
LabelStr[2,3]:='Optical depth';
LabelStr[2,5]:='Optical depth';
LabelStr[2,7]:='Optical depth';

end
else
begin 
LabelStr[2,1]:='Height (Km)';
LabelStr[2,3]:='Height (Km)';
LabelStr[2,5]:='Height (Km)';
LabelStr[2,7]:='Height (Km)';
end;

END;
{-----------------}


BEGIN      {main}
if doingMore then  {select data and frames to be plotted}
begin fLower:=5; fUpper:=8; end
else
begin fLower:=1;fUpper:=4; end;
 GraphBackColor := DarkGray;
 SetLineStyle(0,0,1);
 SetColor(LightGreen);
INITARRAYS;
for m:=1 to 2 do
BEGIN
   IF plotModel[m] then
   begin
        for f:=fLower to fUpper do
        begin 
            GetYScale(m,f,StructData);
        if st[m,f]=sb[m,f] then   {variable is constant across plot}
         begin
              if structurerec.logarithmic then
              begin
                   with models[m] do   {use value at bottom}
                       st[m,f]:=int(log10(shell[numshells].vari.value(gvarid[f]))+1);
                   sb[m,f]:=st[m,f]-1;
              end
              else
              begin
              sb[m,f]:=0;
              with models[m] do   {use value at bottom}
                   st[m,f]:=shell[numshells].vari.value(gvarid[f]);
              end;
          end;
        end;
        if doingMore then
        BEGIN st[m,5]:=st[m,6];
        END;{use same upper limits for adgrad and radgrad}
        GetXScale(m,Structurerec); {same x-scale for all frames, depends only on model}
   end; {if plotmodel}
END; {for m}

{now find largest and use for all  plots}
stmax:=0;
FOR m:=1 to 2 do
    BEGIN
        for f:=fLower to fUpper do  
             if st[m,f]>stmax then stmax:=st[m,f];
        st[m,f]:=stmax;
    END; {FOR M}

for f:=fLower to fUpper do
BEGIN
    OpenViewPort(f);
    SetMyScales(f);
    PutLabels(f);

END; {for f}

setcolor(white);

END; {OPENSTRUCTUREPLOT}

{---------------------------------------------------------------------------}
PROCEDURE OPENSPECTRUMOPTIONS;

BEGIN
 GraphBackColor := blue;

 DefineViewport(spectOpt, 0.20, 0.80, 0.61, 0.92);  {whole screen}

 SetLineStyle(0,0,1);
 SetColor(LightGreen);
 Openviewport(spectOpt);
 setcolor(white);
 {print(15,1,'Set Options');}
 if showSpectrum[1] then
    ShowMod1But.number:=1
    else
    ShowMod1But.number:=2;
 if showSpectrum[2] then
    ShowMod2But.number:=1
    else
    ShowMod2But.number:=2;
 if showSpectrum[BBody] then
    ShowBBBut.number:=1
    else
    ShowBBBut.number:=2;

 if autoClear then
    clearBut.number:=1
    else
    clearBut.number:=2;

 ShowMod1but.drawall;
 ShowMod2But.drawall;
 ShowBBBut.Drawall;
 LockScaleBut.drawall;
 clearBut.drawall;
 tempslider.drawall;


END;

PROCEDURE OpenSpectrumPlot;

BEGIN
 openviewport(9); {to erase scale}
 CloseViewPort(9); {whole screen}
 OpenSpectrumOptions;
 GraphBackColor := DarkGray;
 DefineViewport(spectrum, 0.10, 0.90, 0.12, 0.60);  {lower portion of screen}
 selectviewport(spectrum);
 SetLineStyle(0,0,1);
 SetColor(LightGreen);
 Openviewport(spectrum);
 putlabel(left,'Intensity');
 putlabel(bottom, 'Wave Number');
 setcolor(white);
 Print(27,1,'Spectrum');

END;

{===========================================-}

PROCEDURE OpenRadBeamsPlot;

VAR
 stmax:real;
 fLower,fUpper,m, f,fs:integer;
 {--------------------------------------}
 PROCEDURE INITARRAYS;
 VAR I,J:INTEGER;
BEGIN

gVarId[1]:=Iup;  {tell program which data to plot in each frame}
gvarId[2]:=Idn;
gVarId[3]:=Jay;
gVarId[4]:=Flux;
gVarId[5]:=sfn;
gVarId[6]:=Planck;

for i:=1 to 2 do
    for j:=1 to 6  do
BEGIN
 st[I,J]:=0;
 sb[I,J]:=0;
END;
LabelStr[1,1]:='I upward (*10^9) ';    {for frame scales}
LabelStr[1,2]:='I downward (*10^9)';
LabelStr[1,3]:='J, mean int. (*10^9)';
LabelStr[1,4]:='F, Rad flux (*10^8)';
LabelStr[1,5]:='S, Source fn (*10^9) ';
LabelStr[1,6]:='B, Planck fn (*10^9)';

LabelStr[2,2]:=' ';
LabelStr[2,4]:=' ';
LabelStr[2,7]:=' ';
LabelStr[2,8]:=' ';

if RadRec.tauscale  then
begin
LabelStr[2,1]:='Optical depth';
LabelStr[2,3]:='Optical depth';
LabelStr[2,5]:='Optical depth';
LabelStr[2,6]:='Optical depth';

end
else
begin
LabelStr[2,1]:='Height (Km)';
LabelStr[2,3]:='Height (Km)';
LabelStr[2,5]:='Height (Km)';
LabelStr[2,6]:='Height (Km)';
end;

 END;
 {------------------------------------}
BEGIN     {main}
if doingMore then  {select data and frames to be plotted}
begin fLower:=5; fUpper:=6; end
else
begin fLower:=1;fUpper:=4; end;

 GraphBackColor := DarkGray;
 SetLineStyle(0,0,1);
 SetColor(LightGreen);
INITARRAYS;
for m:=1 to 2 do
BEGIN
   IF plotModel[m] then
   begin
        for f:=fLower to fUpper do
            GetYScale(m,f,RadnData);  {find st[m,f]}
        GetXScale(m,Radrec); {same x-scale for all frames, depends only on model}
   end; {if plotmodel}
END; {for m}

{now find largest and use for all radiation plots}
FOR m:=1 to 2 do
  BEGIN
  STMAX:=0;
    if plotmodel[m] then
     BEGIN
        for f:=fLower to fUpper do
             if st[m,f]>stmax then stmax:=st[m,f];
        for f:= fLower to fUpper
            do st[m,f]:=stmax;

     END; {FOR M}
  END;
for f:=fLower to fUpper do
BEGIN
    OpenViewPort(f);
    setMyScales(f);
    PutLabels(f);

 END; {for f}
setcolor(white);
END;

{==================================================-}

PROCEDURE OpenOpacityPlot;
BEGIN
     openviewport(9);
     closeviewport(9);
     defineViewPort(OpacPlot,0.1,0.9,0.15,0.9);
     selectviewPort(OpacPlot);
     SetLinestyle(0,0,1);
     setcolor(lightgreen);
     openviewport(opacplot);
     putlabel(left, 'ln(Opacity (cm^2/g))');
     putlabel(bottom, 'Wave Number');
END;


{=========================Output Handling Procedures =======================}
FUNCTION ChooseMyScale(m,f:integer):Integer;
BEGIN
  IF not SameScale then
     ChooseMyScale:=f+8*(m-1)
  else
      BEGIN
       If PlotModel[1] then
          if not Plotmodel[2]
             then
              chooseMyScale:=f
             else  if plotmodel[2] then
              BEGIN
              If (graphScales[f].ymax-graphScales[f].ymin)
              <  (graphScales[f+8].ymax-graphScales[f+8].ymin)
                 then
                 ChooseMyScale:=f
                 else
                 ChooseMyScale:=f+8;
              END;

       if not Plotmodel[1] then
                 ChooseMyScale:=f+8;
       END;
end;

{========================================}
FUNCTION ChooseMyRadScale(m,f:integer):Integer;
BEGIN
  IF not SameScale then
     ChooseMyRadScale:=f+8*(m-1)
  else
      BEGIN
       If PlotModel[1] then
          if not Plotmodel[2]
             then
              chooseMyRadScale:=f
             else  if plotmodel[2] then
              BEGIN
              If (graphScales[f].ymax-graphScales[f].ymin)
              >  (graphScales[f+8].ymax-graphScales[f+8].ymin)
                 then
                 ChooseMyRadScale:=f
                 else
                 ChooseMyRadScale:=f+8;
              END;

       if not Plotmodel[1] then
                 ChooseMyradScale:=f+8;
       END;
end;
{=============================================}
PROCEDURE PlotGasdata(model,frame,DataId:integer);
var i,s,c:integer;
xb,xe,yb,ye:real;
BEGIN
selectViewPort(frame);
selectScale(ChooseMyScale(model,frame));
if sameScale then SetMyaxes(chooseMyScale(model,frame));
c:=getcolor;
with models[model] do
     BEGIN
     {Set color}
     setcolor(plotcolor);

        if  (frame=2) or (frame=6) then
         Print(1,model,'Model ' + ScNumStr(Model,0));
       
           FOR s:=1 to numshells do
               BEGIN
               if structurerec.tauscale then
               BEGIN  
                      xb:=Shell[s-1].vari.value(tau);
                      xe:=Shell[s].vari.value(tau);
               END
               ELSE
               BEGIN
                    xb:=shell[s-1].vari.value(height)/1e5;
                    xe:=shell[s].vari.value(height)/1e5;
               END; {if tauscale}
              
               if (Structurerec.logarithmic and (not gdoingmore)) then
               BEGIN
                 yb:=log10(shell[s-1].vari.value(dataId));
                 ye:=log10(shell[s].vari.value(dataId));
               END;
             
               if ((not structurerec.logarithmic) or  (structurerec.logarithmic and gdoingmore))  then
               BEGIN   {natural values}
                yb:=shell[s-1].vari.value(dataId);
                ye:=shell[s].vari.value(dataId);
                END;
               plotline(xb,yb,xe,ye);{does mapping to screen}
               END;{FOR}
     END; {WITH}
setcolor(c);{reset}
END;{PlotGasdata}
{============================================-}

PROCEDURE Plotraddata(model,frame,DataId:integer);
var i,s,c:integer;
xb,xe,yb,ye:real;
BEGIN
selectViewPort(frame);
selectScale(ChooseMyRadScale(model,frame));
if sameScale then setMyAxes(chooseMyRadScale(model,frame));
  {Only show both scales when they are equal.}
c:=getcolor;
with models[model] do
     BEGIN
     setcolor(plotcolor);
     if  (frame=2) or (frame=6) then
     Print(1,model,'Model ' + ScNumStr(Model,0));

           FOR s:=2 to numshells do   {trouble if start from 1}
               BEGIN
               if Radrec.tauscale then
               BEGIN
                      xb:=Shell[s-1].vari.value(tau);
                      xe:=Shell[s].vari.value(tau);
               END
               ELSE
               BEGIN
                    xb:=shell[s-1].vari.value(height)/1e5;
                    xe:=shell[s].vari.value(height)/1e5;
               END; {if tauscale}

               if Radrec.logarithmic = true then
               BEGIN
                 yb:=log10(shell[s-1].radn.value(mean,dataId));
                 ye:=log10(shell[s].radn.value(mean,dataId));
               END
               ELSE  {natural values}
               BEGIN
                yb:=shell[s-1].radn.value(mean,dataId);
                ye:=shell[s].radn.value(mean,dataId);
                if dataId=flux then
                   begin
                   yb:=10*yb;
                   ye:=10*ye;
                   end;
                END;
               plotline(xb,yb,xe,ye);{does mapping to screen}
               END;{FOR}
     END; {WITH}
setcolor(c);{reset}
END;{PlotGasdata}
{============================================-}
PROCEDURE DoStructurePlot;  {P, T, rho, kappa}
var
i,m,fLower,fUpper:integer;


BEGIN
if not gDoingMOre then
begin fLower:=1;fUpper:=4; end
else
begin fLower:=5;fUpper:=8;end;

OpenStructurePlot(gDoingMore); {get scales}

for m:=1 to 2 do
if plotmodel[m] then   {model is to be plotted; selected with menu}
for i:=fLower to fUpper do
    begin
    PlotGasData(m,i,gvarId[i]);   {put data variid[i] for  model m into widow i}
    end;
END;{DoStructurePlot}
{-------------------------------------------------}

PROCEDURE FindSpScale(m,s:integer);
var i:integer;
BEGIN
 WITH GRAPHSCALES[s] DO
      BEGIN
      XMin:=WNList[m,1];
      xMax:=wnlist[m,numFreqs];
      yMin:=0;
      {now find ymax}
      yMax:=0;
      if (m<3) and ModelBuilt[m] then
       BEGIN
         for i:=1 to numFreqs do
           if models[m].emFlux[i]>yMax then
           ymax:=models[m].emFlux[i];
       END
       ELSE
       BEGIN
         for i:=1 to numFreqs do
           if BB(ESBBTemp,WNList[m,i])/4>yMax then
           ymax:=BB(ESBBTemp,WNList[m,i])/4;

       END;


      END;
END;

{=====================================}

PROCEDURE PlotASpectrum(m,Scale:integer);
var w:integer;
xb,xe,yb,ye:real;

BEGIN
SelectScale(scale);

for w:=1 to numFreqs-1 do
    BEGIN
       xb:=Wnlist[m,w];
       xe:=WNList[m,w+1];
       if (m<3) and modelbuilt[m] then
          BEGIN
          yb:=models[m].EMFlux[w];
          ye:=models[m].EMFlux[w+1];
          END
          ELSE
          BEGIN
          yb:=BB(ESBBTemp,WNList[m,w])/4;
          ye:=BB(ESBBTemp,WNList[m,w+1])/4;
          END;
          PlotLine(xb,yb,xe,ye);
    END;

END;

{===============================}
PROCEDURE DoSpectrumPlot;
const oldspectrum=20;
var i,band,myScale,scaleNo,temp:integer;
    xl,xr,y:real;
    ym:array[1..3] of real;
    myRect:array[1..4] of pointtype;
    holdscale:boolean;
BEGIN
temp := 5;
Selectviewport(spectrum);
{plot regions of passbands, before models so it will be in
background}
findSpScale(1,spectrum); selectscale(spectrum);
for band:=lowband to highband do
    with passband[band] do
     begin
     xl:= lowerWN;
     xr:=upperWN;
     y:=graphscales[spectrum].ymax*response*0.1;
     Myrect[1].x:=mapX(xl);
     myRect[1].y:=mapY(0);
     myRect[2].x:=mapX(xl);
     MyRect[2].y:=mapY(y);
     MyRect[3].x:=mapX(xr);
     MyRect[3].y:=mapY(y);
     MyRect[4].x:=mapX(xr);
     MyRect[4].y:=mapY(0);
     setFillstyle(xHatchFill,green);
     if band=2 then
        setFillstyle(xHatchfill,lightblue);

     fillPoly(4,MyRect);   {bracket out 6 lines to hide photometric bands}
     setcolor(white);
     if band = lowBand
        then printxy( xl/ESUpperWN,0.2,'    V')
        else
        printxy(xl/ESUpperWn,0.2,'   B');    {down to here}

     end;
{now start with spectra themselves}
 ym[1]:=0;ym[2]:=0;ym[3]:=0;

 holdscale:=true;
 for i:=1 to 3 do
 IF showSpectrum[I] THEN
  BEGIN   {find highest point in each spectrum}
  FindSpScale(i,i);
  ym[i]:= graphscales[i].ymax;
  if i<3 then holdscale:=false;   {only if no models are plotted}
  END;

 myscale:=1;    {this makes sure the scale is set to the tallest graph}
 if ym[2]>ym[1] then myScale:=2;
 if (ym[3]>ym[1]) and (ym[3]>ym[2]) then myscale:=3;

if not lockscale  then
begin
           findSpScale(myscale,oldspectrum); {set to tallest graph}
        with graphscales[oldspectrum] do
        begin
                 defineScale(temp,xmin,xmax,ymin/1e9,ymax/1e9);
                 axis(0,0,tickspace(xmax-xmin),tickspace(ymax-ymin)/1e9);
{                 defineScale(oldspectrum,xmin,xmax,ymin,ymax);}
        end;
        SelectScale(oldSpectrum);

end;

if lockscale  then
  begin
   if  (showSpectrum[1] or showSpectrum[2]) then
     begin
       findSpScale(myscale,spectrum); {set to tallest graph}
      with graphscales[spectrum] do
       begin
                 defineScale(temp,xmin,xmax,ymin/1e9,ymax/1e9);
                 axis(0,0,tickspace(xmax-xmin),tickspace(ymax-ymin)/1e9);
         end;
        SelectScale(Spectrum);
                  findSpScale(myscale,oldspectrum); {set to tallest graph}
        with graphscales[oldspectrum] do
        begin
                 defineScale(temp,xmin,xmax,ymin/1e9,ymax/1e9);
                 axis(0,0,tickspace(xmax-xmin),tickspace(ymax-ymin)/1e9);
        end;
        SelectScale(oldSpectrum);
        end;
    end
    else
    begin
     findSpScale(myscale,spectrum); {set to tallest graph}
      with graphscales[spectrum] do
       begin
                 defineScale(temp,xmin,xmax,ymin/1e9,ymax/1e9);
                 axis(0,0,tickspace(xmax-xmin),tickspace(ymax-ymin)/1e9);
       end;
       SelectScale(Spectrum);
  end;

for i:=1 to 3 do
IF showSpectrum[I] THEN
  BEGIN
     if (not lockscale) then
        begin
        scaleNo:=spectrum;
        FindSpScale(i,spectrum);
        end;
     if i<3 then setcolor(models[i].plotcolor)
        else setcolor(lightcyan);  {for blackbody}
     if lockscale then
           if (showspectrum[1] or showspectrum[2]) then scaleNo:=spectrum
           else scaleno:=oldspectrum;
     if i<3 then print(40,2+i,concat('Model',Numstr(i,2,0)))
         else print(40,2+i,'Blackbody');
     PlotASpectrum(i,scaleNo);
  END;{IF showSpectrum}
firstplot:=false;
setwritemode(copyput);
setcolor(white);
END;   {DoSpectrumPlot}
{-------------------------------------------------}
PROCEDURE DoRadBeamsPlot(doingMore:boolean);      {Iup, Idn, J, F} {S,B}
var
i,m,fLower,fUpper:integer;

BEGIN
if not gDoingMOre then
begin fLower:=1;fUpper:=4; end
else
begin fLower:=5;fUpper:=6;end;


Openradbeamsplot(gDoingMOre);
for m:=1 to 2 do
if plotmodel[m] then   {model is to be plotted; selected with plotOptions}
for i:=fLower to fUpper do
    begin
    PlotRadData(m,i,gvarId[i]);   {put data variid[i] for  model m into window i}
    end;
END;
{-------------------------------------------------}

{-------------------------------------------------}
PROCEDURE DoOpacityPlot;
var
oxl,oxr,oyt,oyb,xb,xe,yb,ye:real; {endpoints of scale}
st,sb:array[1..2] of real; {yscale limits}
mm,ss,ww,c,j:integer;
{------------------------}
procedure FindOYScale(var yt,yb:real);
{scan opacity values for extremes; work with
natural values and convert later}
var
s,m,w:integer;
tempval:real;
begin
 yt:=0;
 yb:=1;
 for m:=1 to 2 do
     if plotmodel[m] then
        with models[m] do
             for s:=1 to numshells do
                 for w:=1 to numfreqs do
                     begin
                      tempval:= shell[s].opacity.value(w);
                      if (tempval > yt) then yt:=tempval;
                      if (tempval < yb) then yb:=tempval;
                      end;
yt:=ln(yt);
yb:=ln(yb);
end;
{-------------------------}
BEGIN
OpenOpacityPlot;
oxl:=0;
oxr:=wnList[1,numfreqs];
if oxr=0 then oxr:=wnlist[2,numfreqs];
findOYScale(oyt,oyb);
definescale(opacPlot,oxl,oxr,0.9*oyb,1.1*oyt);
{set axes}
axis(0,0,tickspace(oxl-oxr),tickspace(1.1*oyt-0.9*oyb));
for mm:=1 to 2 do
   if plotmodel[mm] then
     with models[mm] do
        begin
        {setcolor(plotcolor); }
        j:=0;
        for ss:=1 to numshells do
             if ss in showshell then
             begin
                j:=j+1;
                setcolor(plotcolor+j);
                for ww:=1 to  numfreqs-1 do
                begin
                xb:=wnlist[mm,ww];
                xe:=wnlist[mm,ww+1];
                yb:=ln(shell[ss].opacity.value(ww));
                ye:=ln(shell[ss].opacity.value(ww));
                plotline(xb,yb,xe,ye);
                end;

             print(20+(mm-1)*25,j,concat('Shell ',Numstr(ss,2,0)));
             end; {if}
        setcolor(plotcolor);
        print(10+(mm-1)*25,1,concat('Model',Numstr(mm,2,0)))
        end;
setcolor(white);
END;

{==================================}
PROCEDURE ShowparameterList(M:integer);

			var
				firststr, secstr, thirdstr, fourthstr: str30;
				fontno: integer;
		begin
                        graphbackcolor:=DarkGray;
                        openviewport(10);
                        
			with models[m] do
				begin
				Print(25,2,'Model ' + ScNumStr(M,0) +'  Parameters');
				Print(3,4,'Model name: '+ name);
				Print(3,5,'Effective temperature(Te) = '+ numstr(teff,7,1)+'K; Surface gravity = '+ num2str(dyngrav,5) + ' cm/sec^2');
				Print(3,6,'Solar units: Mass = '+ ScNumStr(mass,1)+ '; Radius = '+ ScNumstr(rad,1)+ '; Luminosity = '+scNumStr(lum,1));
				Print(3,7,'Distance = '+ scNumstr( Ly_distance,1) + ' light years');
				Print(3,8,'mag(app. visual) = '+ScNumStr(MV_App,2) + ';  B-V = '+ ScNumStr(B_V,2));
                                Print(3,9,'Mag(Abs. visual) = '+ ScNumStr(MVIS,2)+'; Mag(bolometric) = '+scNumStr(MBOL,2));
				Print(3,10,'Abundances by number:');
                                print(8,11, 'H(X) = '+ scNumstr(habund,4)+ '; He(Y) = '+ scNumStr(heabund,4));
                                Print(40,11,' Met(Z) = ' + ScNumStr(metabund,4));
				if inclradpress then
				   print(3 ,12,'Includes Rad. Pressure')
				   else print(3 ,12,'Radiation pressure not included');
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
		              secStr := 'Gray Opacity  ';
					StepF:
					     secStr := 'Step function opacity  ';
					hyd:
					    secStr := 'Hydrogen opacity ';

					end;
				Print(3,13, firststr + '   '+ secstr);

				if (absorber = gray) then
				print(34 , 13,' Epsilon = '+ numstr(GrEps,6,4));

				end;{with models[m]}
END;

{==============================================}
PROCEDURE ShowGasList(M:integer);

var
i,myx,MyY,topLine:integer;
secondpage:boolean;

PROCEDURE DOHEADER;

BEGIN
  myX:=1;
  topline:=2;
  myY:=topline;
  Print(25,topline,'Model ' + ScNumStr(M,0) +'  Gas data (cgs)');
  myY:=myY+1;
  Print(myx,myY, '  s    Tau       T       P      V     kappa  mmwt ConFlux/Tot  MachNo');
  myY:=myY+1;
  Print(myx,myY, '       Depth(cm) adgrad radgrad       Hion   Heion   He+ion ');
  topline:=myY;
 
END;
{-----------------------------------}
BEGIN
 graphbackcolor:=DarkGray;
 Openviewport(9);
 DoHeader;
 for i:= 1 to models[m]. numshells do
     begin
          with models[m].shell[i] do
		begin
	        myx:=2;
               { myY:=(2*(i mod 7) +topline); }
                myY:=myY+1;
	      	Print(myx,myY,Numstr(i,2,0));
                Myx:=8;
                Print(myX,MyY,scnumstr(vari.value(tau),1));
                Myx:=16;
                Print(myX,MyY,scNumstr(vari.value(temp),3));
                Myx:=24;
                Print(myX,MyY,scNumstr(vari.value(pres),2));
                Myx:=32;
                Print(myX,MyY,scNumstr(vari.value(volu),2));
                Myx:=40;
                Print(myX,MyY,scNumStr(vari.value(kapp),2));
                Myx:=48;
                Print(myX,MyY,scNumStr(vari.value(mmwt),2));
                Myx:=56;
                Print(myX,MyY,scNumStr(vari.value(conFlux),2));
                Myx:=64;
                Print(myX,MyY,scNumStr(vari.value(MachNo),2));


                 myY:=myY+1;
                 MyX:=8;
                 Print(myx,myY,scNumstr(-vari.value(height),1));
                 MyX:=16;
                 Print(myx,myY,scNumStr(vari.value(adgrad),2));
                 MyX:=24;
                 Print(myx,myY,scNumStr(vari.value(radgrad),2));
                 MyX:=40;
                 Print(myx,myY,ScNumStr(vari.value(Hion),2));
                  { Print(myx,myY,ScNumStr(vari.value(tgrad),2));}
                 MyX:=48;
                 Print(myx,myY,scNumStr(vari.value(Heion),2));
                 MyX:=56;
                 Print(myx,myY,scNumStr(vari.value(HePion),2));

		end;
                 if (i mod 8) = 0 then
                      begin
                      message('     Press any key or click the mouse for more...');
                      StaticPause;
                      openviewport(9);
                      doheader;
                      myY:=topline;
                      end;
                 end;{with i}
   mymenu.display;
END; {ShowGasList}
{==========================================}

PROCEDURE ShowRadiationList(M:integer);
var
i,myx,MyY,topLine:integer;
{------------------------------------}
Procedure DoHeader;

BEGIN
  myX:=1;
  topline:=2;
  myY:=topline;
  Print(25,topline,'Model ' + ScNumStr(M,0) +' Radiation data (cgs)');

END;
{------------------------}

BEGIN   {main}
  graphbackcolor:=DarkGray;
  Openviewport(9);
  DoHeader;
  myY:=myY+1;
  Print(myx,myY, ' Wave numbers(inverse microns)/Surface Fluxes(erg/cm^2/sec):');
  i := 0;
  MyY:=myY+1;
	repeat
	i := i + 1;
        myX:=(i-1)*9+2;
	Print(myX,myY,scNumStr(WNLIst[m,i],2));
	until (((i mod 9) = 0) or (i = numfreqs));
  MyY:=MyY+1;
  i := 0;
	repeat
	i := i + 1;
         myX:=(i-1)*9+2;
	Print(myx,MyY,ScNumStr(models[m].emflux[i],2));
	until (((i mod 9) = 0) or (i = numfreqs));

	if i < numfreqs then {we have not finished list yet}
	begin
		MyY:=MyY+1;
		i := 9;
		repeat
                   i := i + 1;
                   myX:=(i-1-9)*9+2;
	           Print(myX,MyY,ScNUmStr(WNLIst[m,i],2));
               	until (((i mod 9) = 0) or (i = numfreqs));

                 MyY:=MyY+1;
                 i:= 9;
	         repeat
                     i := i + 1;
                     myX:=(i-1-9)*9+2;
	             Print(myX,MyY,scNumStr(models[m].emflux[i],2));
                  until (((i mod 18) = 0) or (i = numfreqs));
       end;
   MyY:=MyY+1;
   myX:=1;
   Print(myx,myY, 'Shell   Tau     H        B         S       J      I+      I- ');

   topline:=myY; {get ready for next data}
    for i:=1 to models[m].numshells do begin

			with models[m].shell[i] do
			begin
	                      myx:=2;
                              {myY:=((i mod 20) +topline);}
                              myy:=myY+1;
			      Print(myx,myY,Numstr(i,2,0));
                              Myx:=8;
                              Print(myX,MyY,scnumstr(vari.value(tau),1));
                              Myx:=16;
                              Print(myX,MyY,scNumstr(radn.value(mean,flux),2));
                              Myx:=24;
                              Print(myX,MyY,scNumstr(radn.value(mean,Planck),2));
                              Myx:=32;
                              Print(myX,MyY,scNumstr(radn.value(mean,sfn),2));
                              Myx:=40;
                              Print(myX,MyY,scNumStr(radn.value(mean,jay),2));
                              Myx:=48;
                              Print(myX,MyY,scNumStr(radn.value(mean,iup),2));
                              Myx:=56;
                              Print(myX,MyY,scNumStr(radn.value(mean,idn),2));

			end;
                         if (i mod 12) = 0 then
                      begin
                      message('     Press any key or click the mouse for more...');
                      Staticpause;
                      openviewport(9);
                      doheader;
                      MyY:=MyY+2;
                      myX:=1;
                      Print(myx,myY, 'Shell   Tau     H        B         S       J      I+      I- ');

                      myY:=topline+3;
                      end;
                        end; {for i}
    mymenu.display;


END;{ShowRadiationLIst}

end.
