
PROGRAM creFile;

{******************************************************************}
{*    Creates data file in record format for reading              *}
{*    constellation data for Stellar Atmosphere program           *}
{*     C.A. Whitney    Copyright @ 1993                           *}
{*        CUPS Project. creFile version 0.1 Aug 11, 1993    *}
{******************************************************************}



USES  crt, dos, graph, printer, CUPSMUPP,CUPS, CUPSfunc,CUPSproc,cupsgui, CUPSGrph;
{*-----               Initialization                                  -----*}
{---------------------------------------------------------------------------}
CONST
maxstars=10;
Recofile = 1;
textfile = 2;
		
TYPE
str20=string[20];
str6=string[6];
spData = record
       lam,mag:real;
       end;
starType = record
         starname: str20;
	designation: str20;
	RA, DEC: real;
	MVIS, mv_app, b_v, Ly_dist: real;
	spectralType: str6;
	spectrum: array[1..20] of SpData;
	end;

constellationType = record
	name: str20;
	numStars: integer;
	star: array[1..10] of startype;
	end;

RECO = file of constellationType;

VAR
Filetype: integer;

RECOFILEIsNamed: boolean;
RECOFILEname, TEXTfileName: str20;
currConstellation, numConstellations,gC: integer;
StarTextFile: text;
starRECOFile: RECO;
myConstellation: array[1..10] of constellationType;
MyMenu:Tmenu;
quitnow:boolean;

PROCEDURE INITPARAMETERS;
BEGIN
QUITNOW:=FALSE;
numConstellations:=0;
gc:=0;
END;
{============================}

PROCEDURE SETUPINTERFACE;

BEGIN
MyMenu.init;
with MyMenu do
BEGIN
COLUMN(1, 'FILE');
ROW(1,1, 'About This Program');
ROW(1,2,'Configuration');
ROW(1,3,'Open Text ..');
ROW(1,4,'Open Rec ..');
ROW(1,5,'Save As Text ..');
ROW(1,6,'Save As Rec ..');
ROW(1,7,'------------');
ROW(1,8,'Exit Program');

column(2,'See Data');
Row(2,1,'First Constellation');
Row(2,2,'Next');
COLUMN(3, 'Help');
ROW(3,1, 'How to Prepare File');
RowActivate(1,7,false);
END;  {with myMenu}
defineViewPort(1,0.05,0.95,0.05,0.95);
END;
{==================================}
PROCEDURE READTEXT;
var textinfile:text;
    textfileName:str20;
    c,s:integer;
BEGIN
textfilename:='STARDATA.PAS';
assign(textinfile, TEXTFILEname);
reset(textinfile);
readln(textInFIle);{skip headers}
readln(textInFIle);{skip headers}
readln(textInFIle);{skip headers}
readln(textInFIle);{skip headers}
readln(textinfile, Numconstellations);
while (not eof(textinfile)) do
	begin
	for c := 1 to numConstellations do
        	begin
		with myconstellation[c] do
			begin
			readln(TextInFile, name);
			readln(TextInFile, numstars);
			for s := 1 to numstars do
	                	with star[s] do
					begin
					readln(TextInFile, starname);
					readln(TextInFile, designation);
					readln(TextInFile, RA, Dec, Mvis, Mv_App, b_v, LY_DIst);
					readln(textinfile, spectraltype);
					end; {with star}
		if not eof(textinfile) then
		readln(TextInFile); {blank line after each constellation}
		end;{with constellation}
        end;
     end;
END;
{=====================================}
PROCEDURE READRECORD;
var c:integer;
recoInFile:reco;
recoFileName:str20;
BEGIN
recoFileName:='STARDATA.rec';
currconstellation := 0;{reset display counter}
assign(recoInFile,RecoFileName);
reset(RECOinfile);
c := 0;
	while (not eof(recoinfile)) do
	begin
	c := c + 1;
	read(recoinfile, myconstellation[c]);
	end;
	numconstellations := c;
END;
{=====================================}
PROCEDURE SAVETEXT;
var textOutFile:text;
    outputfilename:str20;
    c,s:integer;
BEGIN
outputfileName:='STARDATA.pas';
assign(textoutfile,outputfilename);
rewrite(TextOutfile);
				writeln(textOutfile, 'Star data from file:  ', outputfilename); {header line}
				writeln(textoutfile, 'Constellation data format: NumConstellations, Myconstellation.name, Myconstellation.numstars');
				writeln(textOutfile, 'Star data format: StarName, designation, RA, dec, MVis, Mv_app, B-V, Ly_Dist, Sp Type');
				Writeln(textoutfile, ' Pairs of Lamda, Mag');
				writeln(textoutfile, numconstellations : 2); {write number of constellations first}
				for c := 1 to numconstellations do {followed by constellations}
					with myconstellation[c] do
						begin
							writeln(textOutfile, name);
							writeln(textOutfile, numstars : 2);
							for s := 1 to numstars do
								with star[s] do
									begin
										writeln(textOutfile, starname);
										writeln(textOutfile, designation);
										writeln(textOutfile, RA : 7 : 2, Dec : 7 : 2, Mvis : 7 : 2, Mv_App : 7 : 2, b_v : 7 : 2, LY_DIst);
										writeln(textoutfile, spectraltype);
									end; {with star}
							writeln(textoutfile); {blank line after each constellation}
						end;{with constellation}
                                close(textoutfile);
END;
{=====================================}
PROCEDURE SAVERECORD;
var
recoOutFile:reco;
outfileName:str20;
c:integer;
BEGIN
outfileName:='STARDATA.rec';
assign(recoOutfile,outfileName);
rewrite(recoOutFile);
for c := 1 to numConstellations do
write(recoOutFile,myCOnstellation[c]);
close(recoOutFile);
END;
{=====================================}

PROCEDURE SHOWCONSTELLATION(C:INTEGER);
VAR myX,myY,Topline,s,p,offset:integer;
BEGIN
IF numConstellations=0 then
   BEGIN
   message('No data have been read in yet. (Hit any key.)');
   PAUSE
   END
   ELSE
   BEGIN
        topline:=1;
        selectviewport(1);
        openviewport(1);
     with myconstellation[c] do
	begin
        myX:=2;
        myY:=topLine;
        OffSet:=0;
	Print(myX,myY,concat(numStr(c,3,0)+'   '+ name));
	for s := 1 to numstars do
		BEGIN
                myX:=2;
                myY:=myY+1;
		Print(myX,myy,concat(numStr(s,2,0)+ ' ' + star[s].starname + ' '+ star[s].designation +numStr(star[s].ra,7,2)+
                numStr(star[s].dec,6,2) + numStr( star[s].mv_app,6, 2) + numstr(star[s].mvis,6, 2) +
                numStr( star[s].b_v , 6 , 2) + numStr( star[s].Ly_dist , 7 , 1) +'  '+ star[s].spectraltype));
                MyY:=myY+1;
		      for p := 1 to 10 do
                      BEGIN
		      PRINT(3+P*7-7,MyY,numstr(star[s].spectrum[p].mag,6, 3));
                      END;
                MyY:=myY+1;
		      for p := 11 to 20 do
                      BEGIN
		      PRINT(3+(P-10)*7-7,MyY,numstr(star[s].spectrum[p].mag,6, 3));
                      END;
                if ((s mod 6) = 0) then
                       BEGIN MESSAGE('Hit any key for more');
                       pause;
                       myY:=0;
                       openviewport(1);
                       mymenu.display;
                       END;
                END; {FOR S}
        end; {WITH MYCONSTELLATION}
END;
END;
{====================================}


PROCEDURE MFILE;
BEGIN
       with mymenu do Case rowchosen of
       1: BEGIN
       SHOWhelp('Create.Hlp','ABOUTTHISPROGRAM');
       END;

       2: configuration;
       3: ReadText;
       4: ReadRecord;
       5: SaveText;
       6: SaveRecord;

       8: quitNow:=true;

       END; {CASE}


END;  {HandleMenu}
{========================================}

PROCEDURE MSHOW;

BEGIN
with MyMenu do case rowchosen of
1: BEGIN
   if numConstellations>0 then
   BEGIN
   gc:=1;
   ShowConstellation(gc);
   END
   ELSE
   BEGIN
   MESSAGE('No data have been read yet. (Hit any key.)');
   PAUSE;
   END;
   END;
2: BEGIN
   if numConstellations>0 then
   BEGIN
   gc:=gc+1;
   if gc>numconstellations
       then gc:=1;
   ShowConstellation(gc);
   END
   ELSE
   BEGIN
   MESSAGE('NO DATA HAVE BEEN READ YET');
   PAUSE;
   END;
   END;
   END;{CASE}

END;  {MSHOW}
{=======================================}
PROCEDURE MHELP;

BEGIN

END;
{========================================}
PROCEDURE HandleMenu;
 BEGIN

  WITH mymenu DO CASE colChosen OF
    1:  mFILE;
    2: mShow;
    3: mHELP;
    END;
END;

PROCEDURE CreateAFile;
var
			p, c, s: integer;
begin


{clear all spectrum data}
		for c := 1 to 10 do {10 constellations}
			for s := 1 to 10 do {10 stars per constellation}
				for p := 1 to 20 do {20 spectrum points per star}
					begin
						myconstellation[c].star[s].Spectrum[p].mag := 0.0;
						myconstellation[c].star[s].Spectrum[p].lam := 0.5;
					end;

		myconstellation[1].name := 'Orion';
		myconstellation[1].numstars := 8;

		myconstellation[1].star[1].starname := 'Betelgeuse';
		myconstellation[1].star[1].designation := 'Yale 2061';
		myconstellation[1].star[1].RA := 87.5;
		myconstellation[1].star[1].Dec := 7.4;
		myconstellation[1].star[1].mv_app := 0.8;
		myconstellation[1].star[1].B_V := 1.86;
		myconstellation[1].star[1].ly_dist := 1410;
		myconstellation[1].star[1].spectralType := 'M2_Ia ';

		myconstellation[1].star[2].starname := 'Alnilam';
		myconstellation[1].star[2].designation := 'Yale 1903';
		myconstellation[1].star[2].RA := 84.05;
		myconstellation[1].star[2].Dec := -1.20;
		myconstellation[1].star[2].mv_app := 1.7;
		myconstellation[1].star[2].B_V := -0.19;
		myconstellation[1].star[2].ly_dist := 1300;
		myconstellation[1].star[2].spectralType := 'B0_Ia ';

		myconstellation[1].star[3].starname := 'Rigel';
		myconstellation[1].star[3].designation := 'Yale 1713';
		myconstellation[1].star[3].RA := 77.50;
		myconstellation[1].star[3].Dec := -8.32;
		myconstellation[1].star[3].mv_app := 0.11;
		myconstellation[1].star[3].B_V := -0.03;
		myconstellation[1].star[3].ly_dist := 250;
		myconstellation[1].star[3].spectralType := 'B8_Ia';


		myconstellation[1].star[4].starname := 'Bellatrix';
		myconstellation[1].star[4].designation := 'Yale 1790';
		myconstellation[1].star[4].RA := 80.00;
		myconstellation[1].star[4].Dec := 6.27;
		myconstellation[1].star[4].mv_app := 1.63;
		myconstellation[1].star[4].B_V := -0.22;
		myconstellation[1].star[4].ly_dist := 112;
		myconstellation[1].star[4].spectralType := 'B2_III';


		myconstellation[1].star[5].starname := 'Mintaka';
		myconstellation[1].star[5].designation := 'Yale 1852';
		myconstellation[1].star[5].RA := 83.00;
		myconstellation[1].star[5].Dec := -0.3;
		myconstellation[1].star[5].mv_app := 2.23;
		myconstellation[1].star[5].B_V := -0.14;
		myconstellation[1].star[5].ly_dist := 232;
		myconstellation[1].star[5].spectralType := 'O9_II';


		myconstellation[1].star[6].starname := 'Alnitak';
		myconstellation[1].star[6].designation := 'Yale 1949';
		myconstellation[1].star[6].RA := 85.33;
		myconstellation[1].star[6].Dec := -1.95;
		myconstellation[1].star[6].mv_app := 2.05;
		myconstellation[1].star[6].B_V := -0.14;
		myconstellation[1].star[6].ly_dist := 135;
		myconstellation[1].star[6].spectralType := 'O9_I';


		myconstellation[1].star[7].starname := 'Saiph';
		myconstellation[1].star[7].designation := 'Yale 2004';
		myconstellation[1].star[7].RA := 86.92;
		myconstellation[1].star[7].Dec := -9.67;
		myconstellation[1].star[7].mv_app := 2.06;
		myconstellation[1].star[7].B_V := -0.18;
		myconstellation[1].star[7].ly_dist := 217;
		myconstellation[1].star[7].spectralType := 'B0_I';


		myconstellation[1].star[8].starname := 'Nair al Saif';
		myconstellation[1].star[8].designation := 'Yale 1899';
		myconstellation[1].star[8].RA := 83.850;
		myconstellation[1].star[8].Dec := -5.92;
		myconstellation[1].star[8].mv_app := 2.76;
		myconstellation[1].star[8].B_V := -0.23;
		myconstellation[1].star[8].ly_dist := 130;
		myconstellation[1].star[8].spectralType := 'O9_III';

{---------------------------}

		myconstellation[2].name := 'Scorpio';
		myconstellation[2].numstars := 10;

		myconstellation[2].star[1].starname := 'Graffias';
		myconstellation[2].star[1].designation := 'Yale 5985';
		myconstellation[2].star[1].RA := 241.35;
		myconstellation[2].star[1].Dec := -19.6;
		myconstellation[2].star[1].mv_app := 2.62;
		myconstellation[2].star[1].B_V := -0.07;
		myconstellation[2].star[1].ly_dist := 520;
		myconstellation[2].star[1].spectralType := 'B0_V';


		myconstellation[2].star[2].starname := 'Dschubba';
		myconstellation[2].star[2].designation := 'Yale 5953';
		myconstellation[2].star[2].RA := 240.0;
		myconstellation[2].star[2].Dec := -22.61;
		myconstellation[2].star[2].mv_app := 2.30;
		myconstellation[2].star[2].B_V := -0.11;
		myconstellation[2].star[2].ly_dist := 520;
		myconstellation[2].star[2].spectralType := 'B0_V';


		myconstellation[2].star[3].starname := 'Pi Scorpii';
		myconstellation[2].star[3].designation := 'Yale 5944';
		myconstellation[2].star[3].RA := 239.75;
		myconstellation[2].star[3].Dec := -26.1;
		myconstellation[2].star[3].mv_app := 2.90;
		myconstellation[2].star[3].B_V := -0.19;
		myconstellation[2].star[3].ly_dist := 570;
		myconstellation[2].star[3].spectralType := 'B1_V';


		myconstellation[2].star[4].starname := 'Sigma Scorpii';
		myconstellation[2].star[4].designation := 'Yale 6084';
		myconstellation[2].star[4].RA := 245.25;
		myconstellation[2].star[4].Dec := -25.35;
		myconstellation[2].star[4].mv_app := 2.88;
		myconstellation[2].star[4].B_V := -0.14;
		myconstellation[2].star[4].ly_dist := 550;
		myconstellation[2].star[4].spectralType := 'B1_III';


		myconstellation[2].star[5].starname := 'Antares';
		myconstellation[2].star[5].designation := 'Yale 6134';
		myconstellation[2].star[5].RA := 247.35;
		myconstellation[2].star[5].Dec := -26.43;
		myconstellation[2].star[5].mv_app := 0.96;
		myconstellation[2].star[5].B_V := 1.80;
		myconstellation[2].star[5].ly_dist := 520;
		myconstellation[2].star[5].spectralType := 'M1_I';


		myconstellation[2].star[6].starname := 'Tau Scorpio';
		myconstellation[2].star[6].designation := 'Yale 6165';
		myconstellation[2].star[6].RA := 248.97;
		myconstellation[2].star[6].Dec := -28.22;
		myconstellation[2].star[6].mv_app := 2.83;
		myconstellation[2].star[6].B_V := -0.25;
		myconstellation[2].star[6].ly_dist := 520;
		myconstellation[2].star[6].spectralType := 'B0_V';


		myconstellation[2].star[7].starname := 'Epsilon Scorpio';
		myconstellation[2].star[7].designation := 'Yale 6241';
		myconstellation[2].star[7].RA := 252.55;
		myconstellation[2].star[7].Dec := -34.3;
		myconstellation[2].star[7].mv_app := 2.30;
		myconstellation[2].star[7].B_V := 1.15;
		myconstellation[2].star[7].ly_dist := 89;
		myconstellation[2].star[7].spectralType := 'K2_III';


		myconstellation[2].star[8].starname := 'Eta Scorpio';
		myconstellation[2].star[8].designation := 'Yale 6380';
		myconstellation[2].star[8].RA := 258.00;
		myconstellation[2].star[8].Dec := -43.24;
		myconstellation[2].star[8].mv_app := 3.33;
		myconstellation[2].star[8].B_V := 0.41;
		myconstellation[2].star[8].ly_dist := 42;
		myconstellation[2].star[8].spectralType := 'FO_IV';


		myconstellation[2].star[9].starname := 'Sargas';
		myconstellation[2].star[9].designation := 'Yale 6553';
		myconstellation[2].star[9].RA := 264.32;
		myconstellation[2].star[9].Dec := -43.00;
		myconstellation[2].star[9].mv_app := 1.86;
		myconstellation[2].star[9].B_V := 0.40;
		myconstellation[2].star[9].ly_dist := 197;
		myconstellation[2].star[9].spectralType := 'FO_I';


		myconstellation[2].star[10].starname := 'Shaula';
		myconstellation[2].star[10].designation := 'Yale 6527';
		myconstellation[2].star[10].RA := 263.4;
		myconstellation[2].star[10].Dec := -37.1;
		myconstellation[2].star[10].mv_app := 1.62;
		myconstellation[2].star[10].B_V := -0.21;
		myconstellation[2].star[10].ly_dist := 320;
		myconstellation[2].star[10].spectralType := 'B1_V';



{---------------------------}

		myconstellation[3].name := 'Pleiades';
		myconstellation[3].numstars := 9;

		myconstellation[3].star[1].starname := 'Taygeta';
		myconstellation[3].star[1].designation := 'Yale 1145';
		myconstellation[3].star[1].RA := 56.30;
		myconstellation[3].star[1].Dec := 24.47;
		myconstellation[3].star[1].mv_app := 4.30;
		myconstellation[3].star[1].B_V := -0.11;
		myconstellation[3].star[1].ly_dist := 400;
		myconstellation[3].star[1].spectralType := 'B6_V';


		myconstellation[3].star[2].starname := 'Maia';
		myconstellation[3].star[2].designation := 'Yale 1149';
		myconstellation[3].star[2].RA := 56.45;
		myconstellation[3].star[2].Dec := 24.37;
		myconstellation[3].star[2].mv_app := 3.88;
		myconstellation[3].star[2].B_V := -0.07;
		myconstellation[3].star[2].ly_dist := 400;
		myconstellation[3].star[2].spectralType := 'B7_III';


		myconstellation[3].star[3].starname := 'Electra';
		myconstellation[3].star[3].designation := 'Yale 1142';
		myconstellation[3].star[3].RA := 56.22;
		myconstellation[3].star[3].Dec := 24.01;
		myconstellation[3].star[3].mv_app := 3.70;
		myconstellation[3].star[3].B_V := -0.11;
		myconstellation[3].star[3].ly_dist := 400;
		myconstellation[3].star[3].spectralType := 'B6_III';


		myconstellation[3].star[4].starname := 'Merope';
		myconstellation[3].star[4].designation := 'Yale 1156';
		myconstellation[3].star[4].RA := 56.57;
		myconstellation[3].star[4].Dec := 23.95;
		myconstellation[3].star[4].mv_app := 4.18;
		myconstellation[3].star[4].B_V := -0.06;
		myconstellation[3].star[4].ly_dist := 400;
		myconstellation[3].star[4].spectralType := 'B6_IV';


		myconstellation[3].star[5].starname := 'Alcyone';
		myconstellation[3].star[5].designation := 'Yale 1165';
		myconstellation[3].star[5].RA := 56.87;
		myconstellation[3].star[5].Dec := 24.12;
		myconstellation[3].star[5].mv_app := 2.87;
		myconstellation[3].star[5].B_V := -0.09;
		myconstellation[3].star[5].ly_dist := 400;
		myconstellation[3].star[5].spectralType := 'B7_III';


		myconstellation[3].star[6].starname := 'Atlas';
		myconstellation[3].star[6].designation := 'Yale 1178';
		myconstellation[3].star[6].RA := 57.30;
		myconstellation[3].star[6].Dec := 24.05;
		myconstellation[3].star[6].mv_app := 3.63;
		myconstellation[3].star[6].B_V := -0.08;
		myconstellation[3].star[6].ly_dist := 400;
		myconstellation[3].star[6].spectralType := 'B8_III';


		myconstellation[3].star[7].starname := 'Pleione';
		myconstellation[3].star[7].designation := 'Yale 1180';
		myconstellation[3].star[7].RA := 57.30;
		myconstellation[3].star[7].Dec := 24.13;
		myconstellation[3].star[7].mv_app := 5.05;
		myconstellation[3].star[7].B_V := -0.08;
		myconstellation[3].star[7].ly_dist := 232;
		myconstellation[3].star[7].spectralType := 'B8';


		myconstellation[3].star[8].starname := 'Asterope';
		myconstellation[3].star[8].designation := 'Yale 1151';
		myconstellation[3].star[8].RA := 56.47;
		myconstellation[3].star[8].Dec := 24.56;
		myconstellation[3].star[8].mv_app := 5.76;
		myconstellation[3].star[8].B_V := -0.04;
		myconstellation[3].star[8].ly_dist := 230;
		myconstellation[3].star[8].spectralType := 'B8V';

		myconstellation[3].star[9].starname := 'Celaeno';
		myconstellation[3].star[9].designation := 'Yale 1140';
		myconstellation[3].star[9].RA := 56.20;
		myconstellation[3].star[9].Dec := 24.29;
		myconstellation[3].star[9].mv_app := 5.45;
		myconstellation[3].star[9].B_V := -0.04;
		myconstellation[3].star[9].ly_dist := 320;
		myconstellation[3].star[9].spectralType := 'B8_III';


		numconstellations := 3;
{---------------------------}

		myconstellation[4].name := 'Hyades';
		myconstellation[4].numstars := 10;

		myconstellation[4].star[1].starname := 'Epsilon Tauri';
		myconstellation[4].star[1].designation := 'Yale 1409';
		myconstellation[4].star[1].RA := 67.15;
		myconstellation[4].star[1].Dec := 19.18;
		myconstellation[4].star[1].mv_app := 3.53;
		myconstellation[4].star[1].B_V := 1.01;
		myconstellation[4].star[1].ly_dist := 163;
		myconstellation[4].star[1].spectralType := 'K0_III';


		myconstellation[4].star[2].starname := '-';
		myconstellation[4].star[2].designation := 'Yale 1389';
		myconstellation[4].star[2].RA := 66.37;
		myconstellation[4].star[2].Dec := 17.93;
		myconstellation[4].star[2].mv_app := 4.30;
		myconstellation[4].star[2].B_V := 0.05;
		myconstellation[4].star[2].ly_dist := 171;
		myconstellation[4].star[2].spectralType := 'A2_IV';


		myconstellation[4].star[3].starname := '-';
		myconstellation[4].star[3].designation := 'Yale 1373';
		myconstellation[4].star[3].RA := 65.72;
		myconstellation[4].star[3].Dec := 17.53;
		myconstellation[4].star[3].mv_app := 3.76;
		myconstellation[4].star[3].B_V := 0.98;
		myconstellation[4].star[3].ly_dist := 163;
		myconstellation[4].star[3].spectralType := 'K0_III';


		myconstellation[4].star[4].starname := '-';
		myconstellation[4].star[4].designation := 'Yale 1346';
		myconstellation[4].star[4].RA := 64.95;
		myconstellation[4].star[4].Dec := 15.62;
		myconstellation[4].star[4].mv_app := 3.63;
		myconstellation[4].star[4].B_V := 0.99;
		myconstellation[4].star[4].ly_dist := 163;
		myconstellation[4].star[4].spectralType := 'K0_III';


		myconstellation[4].star[5].starname := 'Pi Tauri';
		myconstellation[4].star[5].designation := 'Yale 1396';
		myconstellation[4].star[5].RA := 66.65;
		myconstellation[4].star[5].Dec := 14.72;
		myconstellation[4].star[5].mv_app := 4.69;
		myconstellation[4].star[5].B_V := 0.98;
		myconstellation[4].star[5].ly_dist := 250;
		myconstellation[4].star[5].spectralType := 'G8_III';


		myconstellation[4].star[6].starname := 'Theta 2 Tauri';
		myconstellation[4].star[6].designation := 'Yale 1412';
		myconstellation[4].star[6].RA := 67.15;
		myconstellation[4].star[6].Dec := 15.86;
		myconstellation[4].star[6].mv_app := 3.41;
		myconstellation[4].star[6].B_V := 0.18;
		myconstellation[4].star[6].ly_dist := 112;
		myconstellation[4].star[6].spectralType := 'A7_III';


		myconstellation[4].star[7].starname := '-';
		myconstellation[4].star[7].designation := 'Yale 1473';
		myconstellation[4].star[7].RA := 69.55;
		myconstellation[4].star[7].Dec := 12.51;
		myconstellation[4].star[7].mv_app := 4.27;
		myconstellation[4].star[7].B_V := 0.13;
		myconstellation[4].star[7].ly_dist := 181;
		myconstellation[4].star[7].spectralType := 'A5_V';


		myconstellation[4].star[8].starname := 'Aldebaran';
		myconstellation[4].star[8].designation := 'Yale 1157';
		myconstellation[4].star[8].RA := 68.97;
		myconstellation[4].star[8].Dec := 16.50;
		myconstellation[4].star[8].mv_app := 0.86;
		myconstellation[4].star[8].B_V := 1.53;
		myconstellation[4].star[8].ly_dist := 60;
		myconstellation[4].star[8].spectralType := 'K5_III';


		myconstellation[4].star[9].starname := 'Sigma 2 Tauri';
		myconstellation[4].star[9].designation := 'Yale 1479';
		myconstellation[4].star[9].RA := 69.82;
		myconstellation[4].star[9].Dec := 15.92;
		myconstellation[4].star[9].mv_app := 4.68;
		myconstellation[4].star[9].B_V := 0.16;
		myconstellation[4].star[9].ly_dist := 155;
		myconstellation[4].star[9].spectralType := 'A5_V';


		myconstellation[4].star[10].starname := 'Rho Tauri';
		myconstellation[4].star[10].designation := 'Yale 1444';
		myconstellation[4].star[10].RA := 68.45;
		myconstellation[4].star[10].Dec := 14.85;
		myconstellation[4].star[10].mv_app := 4.65;
		myconstellation[4].star[10].B_V := 0.24;
		myconstellation[4].star[10].ly_dist := 148;
		myconstellation[4].star[10].spectralType := 'F0_V';

		numconstellations := numconstellations + 1;
{---------------------------}
		myconstellation[5].name := 'Some nearby stars';
		myconstellation[5].numstars := 10;

		myconstellation[5].star[1].starname := '-';
		myconstellation[5].star[1].designation := 'HD225213';
		myconstellation[5].star[1].RA := 0;
		myconstellation[5].star[1].Dec := 0;
		myconstellation[5].star[1].mv_app := 8.63;
		myconstellation[5].star[1].B_V := 1.45;
		myconstellation[5].star[1].ly_dist := 14.48;
		myconstellation[5].star[1].spectralType := 'M4V';


		myconstellation[5].star[2].starname := 'Eta Cas';
		myconstellation[5].star[2].designation := 'Yale 219';
		myconstellation[5].star[2].RA := 0;
		myconstellation[5].star[2].Dec := 0;
		myconstellation[5].star[2].mv_app := 3.45;
		myconstellation[5].star[2].B_V := 0.57;
		myconstellation[5].star[2].ly_dist := 19.2;
		myconstellation[5].star[2].spectralType := 'G0V';

		myconstellation[5].star[3].starname := '61 Cyg';
		myconstellation[5].star[3].designation := 'Yale 8085/6';
		myconstellation[5].star[3].RA := 0;
		myconstellation[5].star[3].Dec := 0;
		myconstellation[5].star[3].mv_app := 5.22;
		myconstellation[5].star[3].B_V := 1.17;
		myconstellation[5].star[3].ly_dist := 11.1;
		myconstellation[5].star[3].spectralType := 'K5_V';



		myconstellation[5].star[4].starname := 'Sirius';
		myconstellation[5].star[4].designation := 'Yale 2491';
		myconstellation[5].star[4].RA := 0;
		myconstellation[5].star[4].Dec := 0;
		myconstellation[5].star[4].mv_app := -1.46;
		myconstellation[5].star[4].B_V := 0.00;
		myconstellation[5].star[4].ly_dist := 19.4;
		myconstellation[5].star[4].spectralType := 'A1_V';


		myconstellation[5].star[5].starname := 'Sirius B';
		myconstellation[5].star[5].designation := 'ADS 5423B ';
		myconstellation[5].star[5].RA := 0;
		myconstellation[5].star[5].Dec := 0;
		myconstellation[5].star[5].mv_app := 8.68;
		myconstellation[5].star[5].B_V := 0.00;
		myconstellation[5].star[5].ly_dist := 19.4;
		myconstellation[5].star[5].spectralType := 'WDwrf';


		myconstellation[5].star[6].starname := 'Procyon';
		myconstellation[5].star[6].designation := 'Yale 2943';
		myconstellation[5].star[6].RA := 0;
		myconstellation[5].star[6].Dec := 0;
		myconstellation[5].star[6].mv_app := 0.37;
		myconstellation[5].star[6].B_V := 0.42;
		myconstellation[5].star[6].ly_dist := 11.4;
		myconstellation[5].star[6].spectralType := 'F5_V';


		myconstellation[5].star[7].starname := 'Alpha Cen B';
		myconstellation[5].star[7].designation := 'Yale 5460';
		myconstellation[5].star[7].RA := 0;
		myconstellation[5].star[7].Dec := 0;
		myconstellation[5].star[7].mv_app := 1.33;
		myconstellation[5].star[7].B_V := 0.88;
		myconstellation[5].star[7].ly_dist := 4.37;
		myconstellation[5].star[7].spectralType := 'K5_V';



		myconstellation[5].star[8].starname := 'Altair';
		myconstellation[5].star[8].designation := 'Yale 7557';
		myconstellation[5].star[8].RA := 0;
		myconstellation[5].star[8].Dec := 0;
		myconstellation[5].star[8].mv_app := 0.76;
		myconstellation[5].star[8].B_V := 0.22;
		myconstellation[5].star[8].ly_dist := 16.5;
		myconstellation[5].star[8].spectralType := 'A7_V';





		myconstellation[5].star[9].starname := 'Kruger 60';
		myconstellation[5].star[9].designation := '-';
		myconstellation[5].star[9].RA := 0;
		myconstellation[5].star[9].Dec := 0;
		myconstellation[5].star[9].mv_app := 9.85;
		myconstellation[5].star[9].B_V := 1.62;
		myconstellation[5].star[9].ly_dist := 12.88;
		myconstellation[5].star[9].spectralType := 'M3 ';


		myconstellation[5].star[10].starname := 'The Sun';
		myconstellation[5].star[10].designation := '-';
		myconstellation[5].star[10].RA := 0;
		myconstellation[5].star[10].Dec := 0;
		myconstellation[5].star[10].mv_app := -26.74;
		myconstellation[5].star[10].B_V := 0.65;
		myconstellation[5].star[10].ly_dist := 1.5776e-05;
		myconstellation[5].star[10].spectralType := 'G2_V ';

		numconstellations := numconstellations + 1;
{---------------------------}
		for c := 1 to numconstellations do
			with myconstellation[c] do
				for s := 1 to numStars do
					star[s].MVIS := star[s].mv_app + 5 * 0.4343 * ln(1 / star[s].Ly_Dist) + 7.56;
{MV_App := MVIS + 5 * 0.4343 * ln(Ly_Distance) - 7.56;}
END;
{===========================================================================}
(*-----              ****    MAIN PROGRAM    ****                     -----*)
{---------------------------------------------------------------------------}



BEGIN {Main}
 CUPSinit;
 INITParameters;
 {Help(aboutCUPShelp); }
 SetUpInterface;
 OpenViewPort(1);
 MyMenu.display;
 createAFile;

 REPEAT

     CheckForEvents;

     IF MyMenu.activated THEN HandleMenu;

 UNTIL quitNow; {repeat forever}

 CUPSdone;
END.
