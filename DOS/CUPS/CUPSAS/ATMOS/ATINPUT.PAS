unit AtInPut;

{-------  INTPUT SCREENS ------}
{------------Modified June 21, 1994 ------------------}
interface

USES  crt, dos, graph, printer, CUPS, cupsMUPP, CUPSfunc,CupsProc,cupsgui, CUPSGrph, ATCommon;
 {----------------------------------------------}

  PROCEDURE DoStarSelect;
  PROCEDURE DoSpecifyAModel(M:integer; var c:boolean);
  PROCEDURE ShowTempProfile(M:integer; var c:boolean);

 {------------------------------------------------}
implementation
{======================UTILITIES====================}

FUNCTION NumStrExp(Num: Double; width : Integer): string;
VAR
 temp  :string;
BEGIN
 str(Num:width, temp);
 NumStrExp := temp;
END;


{======================WINDOWS===================}


PROCEDURE OpenScAdjustT;
BEGIN
 GraphBackColor := Blue;
 DefineViewport(TAdjust, 0.10, 0.90, 0.15, 0.90);  {whole screen}
 SetLineStyle(0,0,1);
 SetColor(LightGreen);
 Openviewport(TAdjust);
END;
{===========================================}
PROCEDURE OpenScShowSky;

BEGIN
 GraphBackColor := Blue;
 SetLineStyle(0,0,1);
 SetColor(LightGreen);
 Openviewport(5);   {whole screen}
END;
{============================================}
PROCEDURE OpenSpecifyModScrn(m:integer;c:boolean);
var tempstr:str30;
BEGIN
     c:=false;
     with SpecifyScrn Do begin
     Init;

     DefineInputPort(0.05,0.5,0.05,0.95);  {whole screen}
     LoadLine('      STELLAR ATMOSPHERES ');
     LoadLine('    Set parameters for one model');
     Loadline(' ');
     str(modsel,tempstr);
     tempstr:=concat('Enter data for Model No. '+ tempstr);
     LoadLine(tempstr);   {Print MOdsel}
     LoadLine('Name of Model: "               " ');       {get string}
     LoadLine('Mass of star/Sun:   {  } ');
     LoadLine('Radius of star/Sun:     {     } ');
     LoadLine('Luminosity of star/Sun: {     } ');
     LoadLine('Distance (light years): {     } ');
     LoadLine('Helium abundance (by mass) {    } ');
     LoadLine('Metal abundance (by mass) {    } ');
     LoadLine('Type of structure:');
     LoadLIne(' #1   T = Constant');
     LoadLine(' #1   Pure Radiative');
     LoadLine(' #1   Incl. Convection');
      {TConst = 1; RadEq = 2;IncCon = 3;}
     setRadiobutton('1', models[modsel].modtype);

     LoadLine('Select opacity:');
     LoadLine(' #2  Gray  #2  Step   #2  Hydr.');
    { LoadLine(' #2  Hydrogen'); }
    { LoadLine(' #2  Hydrogen + Helium');  }
     Setradiobutton('2', models[modsel].absorber);
      LoadLine(' ');
      LoadLine('   [ OK ]     [Cancel ] ');  {add help button}
      loadline(' OK builds model.');
      with models[modsel] do begin
   {  setNumber(1,M);}
     SetString(1,name);
     SetNumber(2,mass); {put current values}
     setNumberLimits(2, 0.1, 9.0);
     SetNumber(3,rad);
     setNumberLimits(3, 0.01, 100);
     SetNumber(4,lum);
     setNumberLimits(4, 0.01, 1000);
     SetNumber(5,Ly_Distance);
     setNumberLimits(5, 1.0, 1000);
     SetNumber(6,Heabund);
     setNumberLimits(6, 0.0, 0.99);
     SetNumber(7,MetAbund);
     setNumberLimits(7, 0.001, 0.20);
           end;

    AcceptScreen;


    end;{with screen, models}
   { c:=true; }

END;
{===================================}
PROCEDURE DoStarSelect;
BEGIN
OpenScShowSky;
END;

{======================================}
procedure DoSpecifyAModel;
begin
OpenSpecifyModScrn(modsel,changed);
{then get the data}
      changed:=false;
with specifyScrn do begin
   if not canceled then   with models[modsel] do
   begin
     name:= getstring(1);
     mass:=getnumber(2);
     rad:=getnumber(3);
     lum:=getnumber(4);
     Ly_distance:=getnumber(5);
     Heabund:=getnumber(6);
     MetAbund:=getnumber(7);
     Habund:=1-metAbund-HeAbund;
     Modtype:=getradioButton('1');
     absorber:=getradioButton('2');
     Changed:=true;
   end;
   done;
end;

{then close the screen}
end;
{=========================================}


PROCEDURE ShowTempProfile(M:integer; var c:boolean);
{Input handler for adjusting temperature distribution}
var  mystring:str20;
ymax,ymin,xmax,xmin,MyXb,myXe,MyYb,myYe:real;
mysymbol:char;
s:integer;
BEGIN
  {wipe scales from left}
 DefineViewport(spectOpt, 0.02, 0.15, 0.05, 0.95);
 graphbackcolor:=black;
 openviewport(spectOpt);
 closeviewport(spectOPt);

 OpenScAdjustT;
  {TempArray is loaded when model is built}
  setcolor(white);
 If modsel=1 then Mystring:='MODEL 1' else mystring:= 'MODEL 2';
Print(10,1,'Adjust temperature with Arrow Keys');
Print(20,2,mystring);
graphbackcolor:=gray;
DefineViewport(escBut,0.28,0.38,0.72,0.77);
openviewport(escBut);
DefineViewport(Acceptbut,0.48,0.68,0.72,0.77);
openviewport(AcceptBut);
selectviewport(TAdjust);
if structureRec.tauscale then
   putlabel(Bottom,'Optical Depth')
   else
   putlabel(Bottom,'Geometric Depth (cm)');
 putlabel(Left,'Temperature (K)');
 print(15,4,' Escape          Accept/Rebuild');
 c:=false;  {changed, do rebuild}
 {find scales and plot the data in a series of symbols with special one
 for the selected shell}
 with models[modsel] do
   begin
        ymax:=1.5*shell[numshells].vari.value(temp);
        ymin:= shell[1].vari.value(temp)-1000;
        xmin:=0;
        if structureRec.tauscale=true then
           xmax:=taubot+1
           else
           xmax:=shell[numshells].vari.value(height);
        definescale(TAdjust,xmin,xmax,ymin,ymax);
        selectscale(tAdjust);
        if   structureRec.tauscale then
             axis(0.0,0.0,tickspace(xmax-xmin),tickspace(ymax-ymin))
             else
             axis(0.0,0.0,tickspace(-xmax-xmin),tickspace(ymax-ymin));
        for s:=1 to numshells do
         begin
              {myYb:= shell[s].vari.value(temp);
              myYe:= shell[s+1].vari.value(temp); }
              myYb:= tempArray[modsel,s];
              myYe:= TempArray[modsel, s+1];
             if structureRec.tauscale
                 then
                     BEGIN myXb:=shell[s].vari.value(tau);
                           myXe:=shell[s+1].vari.value(tau)
                     END
                  else
                      BEGIN myXb:=shell[s].vari.value(height);
                            myXe:=shell[s+1].vari.value(height);
                      END;
               if not (s=shellSel) then mysymbol:='o'
                  else
                    begin
                         mySymbol:='0';
                         plotline(myXb, myYb-500,myXb,myYb+500);
                    end;
                   plotsymbol(myXb,myYb,mysymbol);
              if s<numshells then
                     plotline(myXb,myYb,myXe,myYe);
         end;{s=1 to numshells}
   end;
 END;

end.