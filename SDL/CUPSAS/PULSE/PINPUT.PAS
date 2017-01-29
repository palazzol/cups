unit PInPut;
  {Unit for Stellar Atmosphere; C. A. Whitney}
 {-------  INTPUT SCREENS ------}
 {------------Modified April 1994------------------}
interface

	uses
		crt, dos, graph, printer, CUPS, cupsMUPP, CUPSfunc, CupsProc, cupsgui, CUPSGrph, PCommon, pBuild;
  {----------------------------------------------}
	function NumStrExp (Num: Double; width: Integer): string;
	procedure GetVal (var localcase: Integer);  {Read menu data; only ESC return}
	procedure OpenScSpecifyMod;
	procedure DoSpecifyAModel;
	procedure doSetModes;
   procedure showDisplacements;
   procedure findshowshells;

  {------------------------------------------------}


implementation
 {======================UTILITIES====================}

	function NumStrExp (Num: Double; width: Integer): string;
		var
			temp: string;
	begin
		str(Num : width, temp);
		NumStrExp := temp;
	end;

 {---------------------------------------------------------------------------}


	procedure GetVal (var localcase: Integer);  {Read menu data; only ESC return}
	begin



	end;

 {======================WINDOWS===================}

	procedure OpenScSpecifyMod;
		var
			tempstr: string; {for display}
			tempnum: integer;
	begin
		with specifyscrn do
			begin
				init;
				defineInputPort(0.05, 0.6, 0.25, 0.95);
				LoadLine('         SPECIFY MODEL PARAMETERS   ');
				LoadLine('Solar units');
				Loadline('Name of model: "           " ');
				LoadLine('Mass:       {    } ');
				LoadLine('Radius:     {    } ');
				LoadLine('Luminosity: {    } ');
				LoadLine('Helium mass fraction:    {     }  ');
				LoadLine('Metals mass fraction:    {     }  ');
				LoadLine('Number of shells (2<n<21):  {    }  ');
				LoadLine('Fractional envelope mass {    } (<=1.0)');
				LoadLine('Type of model:  ');
				LoadLine('  #1 Constant density ');
				LoadLine('  #1 Radiative/convective ');
				LoadLine('  #1 Radiative ');
				LoadLine('  #1 Convective ');
				LoadLine('           [ Ok  ]         [ Cancel ]   ');
       {enter Default values}
				setstring(1, modelName);
				setnumber(2, starmass);
				SetNumberLimits(2, 0.1, 50);
				setnumber(3, starRad);
				setnumberLimits(3, 0.1, 50);
				setnumber(4, starLum);
				setnumberLimits(4, 0.1, 10000);
				setnumber(5, heAbund);
				setNumberLimits(5, 0.01, 0.98);
				setnumber(6, MetAbund);
				setNumberLimits(6, 0.005, 0.1);
				setnumber(7, numshells);
				setNumberLimits(7, 3, maxnumshells);
				setnumber(8, EnvelMass);
				setNumberLimits(8, 0.01, 1.0);

				Setradiobutton('1', star.densitylaw);

				acceptScreen;
			end;{with specifyscrn}
	end;

PROCEDURE OpenDispSetScrn;
 BEGIN
      GraphBackColor := Blue;
      Defineviewport(DispSetScrn, 0.10, 0.90, 0.15, 0.90);  {whole screen}
      SetLineStyle(0,0,1);
      SetColor(LightGreen);
      Openviewport(DispSetSCrn);
 END;
 {===============================INPUT handling procedures=======}


	procedure DoSpecifyAModel;
 {Input handler for setting model paramters}
		var
			tempnum: real; {for parsing input}
			okay, c: boolean;
	begin
		OpenScSpecifyMod;
		c := false;
		with specifyscrn do
			begin
				if not canceled then
					begin        {get new data}
						c := true;
						modelName := getstring(1);
						StarMass := getNumber(2);
						Starrad := getNumber(3);
						StarLum := GetNumber(4);
						HeAbund := GetNumber(5);
						MetAbund := getNumber(6);
						NumShells := trunc(getNumber(7));
                  shellsel:=numshells div 2;
						EnvelMass := getnumber(8);
						coremass := 1 - envelmass;
						HAbund := 1 - MetAbund - HeAbund;
						Star.densityLaw := getradiobutton('1');
					end;
				done;
			end; {with screen}
         findShowShells;
		if c then
			buildmodel(okay);
		if okay then
			begin
				modelbuilt := true;

              {if buildtridiagmat then}
{
                 begin}
{
                 FindLinearModes;}
{
                 end;}
			end
		else
			begin
				modelbuilt := false;
				beep;
			end;
	end;
 {--------------------------------------------------------}
	procedure MakeSetModes;
		var
			m: integer;
	begin
		with ModeSetScrn do
			begin
				init;
				defineinputPort(0.05, 0.7, 0.5, 0.9);
				LoadLine('SPECIFY MODES TO BE ACTIVE');
				LoadLine('             Fund. 1st Harm. 2nd Harm. 3rd Harm.');
				LoadLine('Active        #1      #2        #3        #4');
				LoadLine('Inactive      #1      #2        #3        #4');
				LoadLIne(' ');
				LoadLine('Amp. (0-1)   {   }   {   }    {   }    {   }');
				LoadLine('Phase (deg) {     } {     }  {     }  {     }');
				loadLine(' ');
				LoadLine('       [OK]     [Cancel]');

				if Mode[0].active then
					SetradioButton('1', 1)
				else
					setradiobutton('1', 2);
				if Mode[1].active then
					SetradioButton('2', 1)
				else
					setradiobutton('2', 2);
				if Mode[2].active then
					SetradioButton('3', 1)
				else
					setradiobutton('3', 2);
				if Mode[3].active then
					SetradioButton('4', 1)
				else
					setradiobutton('4', 2);
				SetNumber(9, mode[0].amp);
				SetNumber(10, mode[1].amp);
				SetNumber(11, mode[2].amp);
				SetNumber(12, mode[3].amp);
				SetNumber(13, mode[0].phase * radToDeg);
				SetNumber(14, mode[1].phase * radToDeg);
				SetNumber(15, mode[2].phase * radToDeg);
				SetNumber(16, mode[3].phase * radToDeg);
				for m := 9 to 12 do
					begin
						setnumberLimits(m, 0, 1);
						setNumberLImits(m + 4, 0, 360);
					end;
				Acceptscreen;
   		end;{WITH MODESCRN}
	end;

 {=======================================}
	procedure DoSetModes;
		var
			m: integer;
	begin
		MakeSetModes;
      for m:=3 to numshells do
              mode[m].active:=false;  {de-activate higher modes}
		with modeSetScrn do
			if not canceled then
				begin
					if GetRadioButton('1') = 1 then
						Mode[0].active := true
					else
						mode[0].active := false;
					if GetRadioButton('2') = 1 then
						Mode[1].active := true
					else
						mode[1].active := false;
					if GetRadioButton('3') = 1 then
						Mode[2].active := true
					else
						mode[2].active := false;
					if GetRadioButton('4') = 1 then
						Mode[3].active := true
					else
						mode[3].active := false;
					for m := 0 to 3 do
						begin
							mode[m].amp := getnumber(m + 9);
							mode[m].phase := getnumber(m + 13) / RadToDeg;
						end;
            ManualDisp:=false; {initial displacements will be computed from modes}
				end;
	end;

PROCEDURE ShowDisplacements;
{Input handler for adjusting initial displacements}
var
ymax,ymin,xmax,xmin,MyXb,myXe,MyYb,myYe,ydum:real;
mysymbol:char;
s:integer;
BEGIN
 OpenDispSetScrn;
  {TempArray is loaded when model is built}
  setcolor(white);

Print(8,1,'Adjust displacements with Arrow Keys ');
{graphbackcolor:=gray; }
DefineViewport(escBut,0.18,0.28,0.72,0.77);
openviewport(escBut);
DefineViewport(Acceptbut,0.28,0.38,0.72,0.77);
openviewport(AcceptBut);
selectviewport(DispSetScrn);
if MassPointScale then
   putlabel(Bottom,'Shell Index')
   else
   putlabel(Bottom,'Geometric Depth (cm)');
putlabel(Left,'Displacement');
print(7,4,' Escape  Accept');

{scan to find y extremes}
ymax:= 0;
ymin:= 0;
with star do
for s:=1 to numshells do
    begin
    ydum:= dyn[s].vari[disp];
      if ydum>=ymax then
         ymax:= ydum;
      if ydum<ymin then
         ymin:=ydum;
    end;
ymin:=ymin*1.05;
ymax:=ymax*1.05;

{find x extremes}
if massPointScale=true then
   xmax:=numshells +1
   else
   xmax:=star.eq[numshells].vari[radi];

definescale(DispSetScrn,xmin,xmax,ymin,ymax);
selectscale(DispSetSCrn);
axis(0.0,0.0,tickspace(xmax-xmin),tickspace(ymax-ymin));
with star do
for s:=1 to numshells do
         begin
              if massPointScale
                 then
                     BEGIN myXb:=s;
                           myXe:=s+1
                     END
                  else
                      BEGIN myXb:=eq[s].vari[radi];
                            myXe:=eq[s+1].vari[radi];
                      END;
              { myYb:=dyn[s].vari[disp];  }
              { myYe:=dyn[s+1].vari[disp]; }
                myYb:=tempVector[s];
                myYe:=tempVector[s+1];

               if not (s=shellSel) then mysymbol:='o'
                  else
                    begin
                         mySymbol:='0';
                         plotline(myXb, ymax,myXb,ymin);
                    end;
                   plotsymbol(myXb,myYb,mysymbol);
              if s<numshells then
                     plotline(myXb,myYb,myXe,myYe);
         end;{s=1 to numshells}

   end;
{------------------------------------------------------}
 procedure FindShowShells;
 var s,increment:integer;
 begin
 showshell:=[];
 s:=0;
 increment:=((numshells-1) div 5) + 1;
 repeat
  s:=s+increment;
  showshell:=showshell+[s];
 until s>=numshells;

 end;

end.