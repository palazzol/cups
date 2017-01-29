
unit AtSky;
{---------------------------------------------------------------------------}
{----            sky and Spectrum Procedures-----*}
{-------------------June 20, 1994   ----------------------------}
{-----------------C. A. Whitney --------------------------------}
{---------------------------------------------------------------------------}


interface

USES  crt, dos, graph, printer, CUPsMupp, CUPS, CUPSfunc, CUPSProc,cupsgui,
 CUPSGrph, AtCommon;
PROCEDURE DoShowStars;
FUNCTION FINDNUMCONST(var FILENAME:string):INTEGER;
FUNCTION FindSelectedStar(f:integer;xr,yr:real;var s:integer):boolean;
PROCEDURE ReadConsData(var FILENAME:string; C:integer);
{----------------------------------------------------------------}
implementation

FUNCTION FINDNUMCONST(var FILENAME:string):INTEGER;
VAR InFile:reco;
dummyc:constellationType;
i:integer;
BEGIN
assign(infile,filename);
reset(infile);
i:=0;
while (not eof(infile)) do
      begin
      i:=i+1;
      read(infile,dummyC);
      consName[i]:=dummyC.name;
      end;
FINDNUMCONST:=i;
END;
{====================================}
PROCEDURE ReadConsData(var FILENAME:string; C:integer); {conSelected set by radio buttons}
VAR InFile:reco;
i:integer;
BEGIN
assign(infile,filename);
{$i-} reset(infile); {$i+}
   if IOResult<>0 then
   begin
        announce('Please find StarData.rec file');
        filename:=OpenFile('Stardata.rec');
        if filename='' then Error('I cannot continue without Stardata.rec file.');
        assign(infile,filename);
        reset(infile);
   end;

i:=0;
while (not eof(infile)) do
      begin
      i:=i+1;
      read(infile,myconstellation);
      consName[i]:=myconstellation.name;
      end;

numConstellations:=i;     {This is needed for setting up window}
reset(infile);
for i:=1 to c do
read(infile, myconstellation); {reads single record}
close(infile);
END;
{====================================================}
PROCEDURE getSkyScales;
	const
			degrad = 0.0174533;
		var
			MeanDec,MeanRa,size,decmin, decmax, ramin, ramax: real;
			i: integer;
	begin
        if not ( myconstellation.name = 'Some nearby stars') then
        BEGIN
                selectViewport(sky);
		decmin := 90;
		decmax := -90;
		ramin := 360;
		ramax := 0;
		with myconstellation do
			for i := 1 to numStars do
				begin
					if star[i].dec < decmin then
						decmin := star[i].dec;
					if star[i].dec > decmax then
						decmax := star[i].dec;
					if star[i].ra < ramin then
						ramin := star[i].ra;
					if star[i].ra > raMax then
						raMax := star[i].ra;
				end;{with, for}
        if Abs(decMax-DecMin) < abs(raMax-raMin) then
           size:=1.0*abs(ramax-ramin)
           else
           size :=1.0*abs(decMax-decMin);
        MeanDec:=(decMax+decMin)/2;
        MeanRA:= (raMax+RaMin)/2;
        DefineScale(Sky,MeanRA+size/0.7*cos(meanDec*degrad),
            MeanRA-size/0.7*cos(meanDec*degrad), MeanDec-size,MeanDec+size);

with graphScales[sky] do
     axis(0,0,tickspace(xmIN-xmAX),tickspace(ymax-ymin));
setcolor(lightgreen);
putLabel(left,'Declination (deg)');
putLabel(bottom, 'Right Ascension (deg)');
setcolor(yellow);
PutLabel(inside,'       Sky Map');
 setcolor(white);
 END;
END;
{====================================================}
PROCEDURE getCMScales;
var MinMag,MaxMag:real;
    s:integer;
BEGIN
selectViewPort(CM);
setcolor(lightgreen);
putlabel(left,'M(vis)');
putLabel(bottom,'(B-V) color');
MinMag:= 10;
maxMag:=-10;
for s:=1 to myconstellation.numstars do
    with myconstellation do
    begin
       if star[s].MVis<minMag then minMag:=star[s].Mvis;
       if star[s].MVis>maxMag then maxMag:=star[s].Mvis;
    end;
for s:=1 to 2 do
    if plotmodel[s] then
     begin
       if models[s].MVis<minMag then minMag:=models[s].Mvis;
       if models[s].MVis>maxMag then maxMag:=models[s].Mvis;
     end;

defineScale(CM,-1,3,round(maxMag+1),trunc(minMag-1));
with GraphScales[CM] do
     axis(0,0,tickspace(xmax-xmin),tickspace(2*abs(-ymax-ymin)));
setcolor(yellow);
putLabel(inside,'Color-magnitude diagram');
setcolor(white);
END;
{====================================================}
PROCEDURE SetupSelection; {RAdio buttons in vieport 1}
BEGIN
SELECTVIEWPORT(SETUP);
setcolor(yellow);
print(2,1,'Select constellation ');
CONSTBUT.NUMBER:=conSelected;
CONSTBUT.DRAWALL;
END;
{====================================================}

PROCEDURE PlotStarOnCM(mystar:startype;hi:boolean);

var
	starloc: pointtype;
	starcolor, starsize: integer;
   pattern:fillpatterntype;
begin
         getfillpattern(pattern);
        selectSCALE(cm);
        SELECTVIEWPORT(cm);
        hidecursor;
		with myStar do
			begin
                        {Use absolute magnitude}
					starsize := trunc(10 - MVis) div 2 + 3;
                        map(B_V,MVis,starloc.x,starloc.y);
				if hi then  {highlight the star}
					begin
                                        setcolor(white);
                                        circle(starloc.x,starloc.y,starsize+3);

					end;


				if B_V > 1.0 then
					starcolor := red
				else if B_V > 0.5 then
					starcolor := yellow
				else if B_V > -0.1 then
					starcolor := white
				else
					starcolor := cyan;
				setcolor(starcolor);
            setcolor(black);
            setfillpattern(pattern,starcolor);
				fillellipse(starloc.x,starloc.y,starsize,starsize);
		end;
        showcursor;
END;
{====================================}
PROCEDURE PlotStarOnsky( mystar: startype; hi: boolean);
var
	starloc: pointtype;
	starcolor, starsize: integer;
   pattern:fillpatterntype;
begin
     getfillpattern(pattern);
     hidecursor;
        selectSCALE(sky);
        SELECTVIEWPORT(SKY);
		with myStar do
			begin
               		{Use apparent magnitude}
					starsize := trunc(10 - MV_app) div 2 + 1;
                        map(ra,dec,starloc.x,starloc.y);
				if hi then  {highlight the star}
					begin
                                        setcolor(white);
                                        circle(starloc.x,starloc.y,starsize+3);
					end;
				if B_V > 1.0 then
					starcolor := red
				else if B_V > 0.5 then
					starcolor := yellow
				else if B_V > -0.1 then
					starcolor := white
				else
					starcolor := cyan;
				setcolor(starcolor);
              setcolor(black);
            setfillpattern( pattern, starcolor);
				fillellipse(starloc.x,starloc.y,starsize,starsize);
				end;
                showcursor;
end;

{=============================================================}
PROCEDURE PlotSky ( s: integer);      {S = selected star}
		var
			i: integer;
			hi: boolean;
	begin
		if not (myconstellation.name = 'Some nearby stars') then
			begin
				with myconstellation do
					for i := 1 to numStars do
						begin
							hi := (i=s);
							PlotStarOnsky( star[i], hi);
						end;
			end;
		if myconstellation.name = 'Some nearby stars' then
			begin
                               SELECTVIEWPORT(SKY);
				PRINT(1,2,'    Not in one constellation');
			end;
	end;
{====================================================}
PROCEDURE PlotModelsOnCM;

		var
                   myX,MyY:integer;
			starloc: pointtype;
			starcolor, starsize,m: integer;
         pattern:fillpatterntype;
      begin
            getfillpattern(pattern);
            selectViewport(CM);
            selectScale(CM);
            hidecursor;
        for m:=1 to 2 do
                if plotmodel[m] then
		     with models[m] do
           begin

				starsize := trunc(10 - MVis) div 2 + 1;
            map(B_V,Mvis,starloc.x,starloc.y);

            if B_V > 1.0 then
					starcolor := red
				else if B_V > 0.5 then
					starcolor := yellow
				else if B_V > -0.1 then
					starcolor := white
				else
					starcolor := cyan;
				   setcolor(starcolor);
                 setcolor(black);
              setfillpattern(pattern,starcolor);
              fillellipse(starloc.x,starloc.y,starsize,starsize);
              setcolor(white);
            if (starSelected-100)=m then
              circle(starloc.x,starloc.y,starsize+3);
              outtextxy(starloc.x+starsize+4,starloc.y-3,concat('Model of '+ models[m].name));

			end;
        showcursor;

END;
{====================================================}

{=================================================}
PROCEDURE  PlotCMCons ( s: integer);
	var
	i: integer;
	hi: boolean;
	begin
		with myconstellation do
			begin
                        	for i := 1 to numstars do
					begin
                                                hi:= (i=starselected);
						PlotStaronCM( star[i], hi);
					end{for i}
	end;{with}
END;
{====================================================}

FUNCTION FindSelectedStar(f:integer;xr,yr:real;var s:integer):boolean;

VAR oldS,i:integer;
    LL,UR:pointtype;
    xrr,yrr,xrl,yrl:real;
    delta:real;
BEGIN
oldS:=starSelected;
findselectedStar:=false;
s:=0;
with myconstellation do
     for i:= 1 to numstars do
         with star[i] do
         BEGIN
        { starsize := trunc(10 - MVis) div 2 + 1;}
              IF F=sky then
              BEGIN
              delta:=1;
              if name ='Pleiades' then delta:=0.1
                 else if name= 'Hyades' then delta :=0.5;
              xrr:=ra-delta;
              xrl:=ra+delta;
              yrl:=dec-delta;
              yrr:=dec+delta;
               if ((Xr>=xrr) AND (Xr<=xrl)) then
                 if  ((Yr>=yrl) AND (Yr<=yrr)) THEN
              s:=i;
              END
              ELSE IF f=CM then
              BEGIN
              xrr:=B_V+0.1;
              xrl:=B_V-0.1;
              yrr:=Mvis-0.3;
              yrl:=Mvis+0.3;
              if ((Xr<=xrr) AND (Xr>=xrl)) then
                 if  ((Yr<=yrl) AND (Yr>=yrr)) THEN
              s:=i;
              END;
         END;{for i}
{now find if model was clicked in CM diagram}
if F=CM then
  for i:= 1 to 2 do
      if plotmodel[i] then
         with models[i] do
         BEGIN
              xrr:=B_V+0.1;
              xrl:=B_V-0.1;
              yrr:=Mvis-0.3;
              yrl:=Mvis+0.3;
              if ((Xr<=xrr) AND (Xr>=xrl)) then
                 if  ((Yr<=yrl) AND (Yr>=yrr)) THEN
              s:=i+100;
         END;
IF s>0 then findSelectedStar:=true;
IF s=oldS then s:=0; {toggle off if reselected}
END;
{===============================}
PROCEDURE showStarData(s:integer);
BEGIN
selectViewport(data); setcolor(yellow);
print(6,1,'Data for selected star');
setcolor(white);

if s<1
   then BEGIN
             print(3,3,'Select a star');
             print(5,4,'by clicking on it');
             print(5,5,'on map or c-M diagram');
        END
   else if s<99 then
       WITH myConstellation.star[s] do
        BEGIN
        print(3,2,concat('Name: ' + starname));
        print(3,3,concat('Desig: ' + designation));
        print(3,4,concat('M(vis): ' + NumStr( MVis,2,2)));
        print(3,5,concat('B_V: ' + Numstr(b_v,2,2)));
        print(3,6,concat('sp. type: ' + SpectralType));
        print(3,7,concat('Distance: '+Numstr(ly_dist,2,0)+' light years'));
        print(3,8,concat('m(vis): ' + NumStr(mv_app,2,2)));

        END
   ELSE IF S>99 THEN
        with models[s-100] do
        BEGIN {PRINT MODEL}
         print(3,2,concat('Model Name: ' + name));
        print(3,3,concat('Desig: --'));
        print(3,4,concat('M(vis): ' + NumStr( MVis,2,2)));
        print(3,5,concat('B_V: ' + Numstr(b_v,2,2)));
        print(3,6,concat('Assumed Dist.: '+Numstr(ly_distance,2,0)+' light years'));
        print(3,7,concat('m(vis): ' + NumStr(mv_app,2,2)));

        END;
END;
{====================================================}
PROCEDURE DoShowStars;
var f:integer;
BEGIN
readConsData(StarFile,conSelected);
  {erase old labels}
  openviewport(9);closeviewport(9);
 for f:=1 to 4 do
    OpenViewPort(f);
getSkyScales;
getCMScales;
SetupSelection; {RAdio buttons in vieport 1}
PlotSky(StarSelected); {puts selected constellation in viewport 2}
PlotModelsOnCM;
PlotCMCons(conSelected);
showStarData(StarSelected);

END;
end.