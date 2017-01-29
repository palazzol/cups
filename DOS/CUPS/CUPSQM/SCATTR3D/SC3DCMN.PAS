{Unit of routines common to quantum mechanics simulations for:       }
{    One-dimensional scattering states (Scattr1D).                   }
{    Three-dimensional scattering states (Scattr3D).                 }
{    Cylindrically symmetric bound states (CylSym).                  }
{                                                                    }
{Author: John R. Hiller                                              }
{Version: 1.0               7/21/94                                  }
{Copyright (c) 1995 John Wiley & Sons, Inc.                          }
{                                                                    }
UNIT Sc3DCmn;

INTERFACE

USES Graph,CUPS,CUPSmupp,CUPSfunc,CUPSproc,CUPSgui,CUPSgrph,Sc3DMach;

CONST
   NumPerRow=7;
   MaxRange=8000;

TYPE
   ParticleType=(electron,muon,proton,custom);
   InfoScrType=array [1..25] OF string[30];
   TView3D=object(TGraph3D)   {input of 3D viewing parameters}
         Sliders  :TSliders;
         Vsave    :integer;
         OldColor :word;
         PROCEDURE Init;
         {override; {Mac only}
         PROCEDURE Draw(a,b,c,BoxSize,BoxDist :real);
         PROCEDURE Resize(VAR BoxSize :real; BoxDist :real);
         PROCEDURE Persp(BoxSize :real; VAR BoxDist :real);
         FUNCTION Changed :Boolean;
         PROCEDURE Rotate(VAR a,b,c :real);
         PROCEDURE Done;
      END; {TView3D}

VAR
   AboutFile :HelpScrType;

PROCEDURE CommInit(FileName :string);

{----------Routines for general graphics output-------------------------}

PROCEDURE ClearOversizedViewPort(Vnum :integer);
PROCEDURE PrepViewPort(Vnum :integer);
PROCEDURE PrepAxisInput(VAR Min,Max,Tic :real; nDiv,decimal :integer);
PROCEDURE SpecialAxis(Snum :integer; hMin,hMax :real; nTicX :integer;
                      vMin,vMax :real; nTicY :integer);
PROCEDURE ChangePlotScales(VAR hMin,hMax :real; hUnit :string;
                           hMinDef,hDifDef :real;
                           VAR vMin,vMax :real; vUnit :string;
                           vMinDef,vDifDef :real;
                           VAR Changed,AutoSet :boolean);
PROCEDURE LegendEntry(Position :integer; Color :word; Name :string);
PROCEDURE PostEulerAngles(a,b,c :real);

{--------Routines for text input and output----------------------------}

PROCEDURE Info(VAR InfoScr :InfoScrType; NumLines,Vnum :integer);
PROCEDURE IntInputWarn(Name :string; VAR InpValue :integer;
                       DefValue :integer; field :integer);
PROCEDURE RealInputWarn(Name :string; VAR InpValue :real;
                        DefValue :real; field :integer);
PROCEDURE SelectMassAndUnits(VAR Particle :ParticleType;
                             VAR Mass,Lfac :real;
                             VAR Munit,Eunit,Lunit :char;
                             VAR Modified :boolean);

{--------Routines for computation-------------------------------------}

FUNCTION ExpSafe(arg :real) :real;
PROCEDURE ComputeZeta(V0,L0,Mass :real; Eunit,Lunit,Munit :char;
                      VAR zeta :real);

{--------Routines for file management---------------------------------}

PROCEDURE GetFileToOpen(VAR FileName :string; VAR WishToAbort :boolean);
PROCEDURE AskOverWrite(FileName :string; VAR WishToAbort :boolean);
PROCEDURE ReadDVector(VAR DataFile :text;
                      Num :integer; VAR Vec :DVector; divisor :real);
PROCEDURE WriteDVector(VAR DataFile :text;
                       Num :integer; VAR Vec :DVector; multiplier :real);
PROCEDURE ReadParticle(VAR DataFile :text; VAR Particle :ParticleType);
PROCEDURE WriteParticle(VAR DataFile :text; Particle :ParticleType);
PROCEDURE VerifyNew(Saved :boolean; VAR Verified :boolean);
PROCEDURE VerifyQuit(VAR DataFile :text; VAR FileName :string;
                     VAR WishToSave,WishToQuit :boolean);

IMPLEMENTATION

PROCEDURE CommInit(FileName :string);
{Initialize variables used for common help screens and others.}
VAR
   n :integer;
BEGIN
   AboutFile[01]:=' File: use files to obtain or save starting';
   AboutFile[02]:='       information.';
   AboutFile[03]:='';
   AboutFile[04]:='    * About CUPS - show description';
   AboutFile[05]:='                   of software consortium.';
   AboutFile[06]:='';
   AboutFile[07]:='    * About Program - show credits and';
   AboutFile[08]:='                      a brief description.';
   AboutFile[09]:='';
   AboutFile[10]:='    * Configuration - verify and/or change';
   AboutFile[11]:='                      program configuration.';
   AboutFile[12]:='';
   AboutFile[13]:=
           ConCat('    * New - set file name to default ',FileName);
   AboutFile[14]:='            and start new calculation.';
   AboutFile[15]:='';
   AboutFile[16]:='    * Open - open file and read contents.';
   AboutFile[17]:='';
   AboutFile[18]:='    * Save - save current state to file.';
   AboutFile[19]:='';
   AboutFile[20]:='    * Save As - save current state';
   AboutFile[21]:='                to file with chosen name and';
   AboutFile[22]:='                set file name to this choice.';
   AboutFile[23]:='';
   AboutFile[24]:='    * Exit Program.';
   for n:=25 to 25 do AboutFile[n]:='';
END; {CommInit}

{--------Methods for TView3D object------------------------------------}

PROCEDURE TView3D.Init;
BEGIN
   Vsave:=ViewPortNumber;
   DefineViewPort(9,0.0,0.25,0.08,0.38); SelectViewPort(9);
   {inherited Init; {Mac only}
   TGraph3D.Init;   {PC only}
   BoxDrawn:=True; BoxColor:=bxor(Whitte,DarkGray);
   DefineViewPort(10,0.25,0.65,0.08,0.38);
   {New(Sliders); {Mac only}
   Sliders.Init;
   SelectViewPort(Vsave);
END; {TView3D.Init}

PROCEDURE TView3D.Draw(a,b,c,BoxSize,BoxDist :real);
{Draw small box and create sliders.}
BEGIN
   Vsave:=ViewPortnumber;
   OldColor:=GetColor;
   HideMouse;
   SelectViewPort(10);
   ClearViewPort;
   OpenViewPort(9);
   SetViewAngles(a,b,c);
   SetDistance(BoxSize,BoxDist);
   SetWriteMode(XorPut);
   DrawBox;
   SetWriteMode(CopyPut);
   WITH Sliders DO
      BEGIN
         Create(1,   0,360,a,0.28,0.3,  0.63,0,   '0','360','alpha',False);
         Create(2,-180,180,b,0.28,0.225,0.64,0,'-180','180', 'beta',False);
         Create(3,   0,360,c,0.28,0.15, 0.63,0,   '0','360','gamma',False);
         DrawAll
      END;
   SetColor(OldColor);
   SelectViewPort(Vsave);
   ShowMouse;
END; {TView3D.Draw}

PROCEDURE TView3D.Resize(VAR BoxSize :real; BoxDist :real);
{Change size parameters.}
BEGIN
   SelectViewPort(9);
   SetWriteMode(XorPut);
   DrawBox;
   IF BoxSize<2 THEN BoxSize:=BoxSize+0.1 ELSE BoxSize:=0.5;
   SetDistance(BoxSize,BoxDist);
   DrawBox;
   SetWriteMode(CopyPut)
END;

PROCEDURE TView3D.Persp(BoxSize :real; VAR BoxDist :real);
{Change distance parameter.}
BEGIN
   SelectViewPort(9);
   SetWriteMode(XorPut);
   DrawBox;
   IF BoxDist<1.3 THEN BoxDist:=10 ELSE BoxDist:=BoxDist/1.5;
   SetDistance(BoxSize,BoxDist);
   DrawBox;
   SetWriteMode(CopyPut)
END;

FUNCTION TView3D.Changed :boolean;
{Detect use of sliders.}
VAR
   TextInfo :TextSettingsType;
BEGIN
   GetTextSettings(TextInfo);
   Vsave:=ViewPortNumber;
   OldColor:=GetColor;
   Changed:=Sliders.Changed;
   SetColor(OldColor);
   SelectViewPort(Vsave);
   SetTextJustify(TextInfo.Horiz,TextInfo.Vert);
END;

PROCEDURE TView3D.Rotate(VAR a,b,c :real);
{Change Euler angles.}
BEGIN
   Vsave:=ViewPortNumber;
   OldColor:=GetColor;
   HideMouse;
   SelectViewPort(9);
   SetWriteMode(XorPut);
   DrawBox;
   WITH Sliders DO
      BEGIN
         a:=Value(1); b:=Value(2); c:=Value(3)
      END;
   SetViewAngles(a,b,c);
   DrawBox;
   SetWriteMode(CopyPut);
   SetColor(OldColor);
   SelectViewPort(Vsave);
   ShowMouse;
END; {TView3D.Rotate}

PROCEDURE TView3D.Done;
BEGIN
   Vsave:=ViewPortNumber;
   HideMouse;
   CloseViewPort(9);
   CloseViewPort(10);
   Sliders.Done;
   {Dispose(Sliders); {Mac only}
   SelectViewPort(Vsave);
   ShowMouse;
END;

{----------Routines for general graphics output-------------------------}

PROCEDURE ClearOversizedViewPort(Vnum :integer);
{Clear viewport and space for labels.}
CONST
   LabelSpace=0.04;
VAR
   xLabelSpace,yLabelSpace,Vx1Ext,Vy1Ext,Vx2Ext,Vy2Ext :integer;
   x1,y1,x2,y2 :real;
BEGIN
   xLabelSpace:=round(LabelSpace*GetMaxX);
   yLabelSpace:=round(1.5*LabelSpace*GetMaxY);
   ViewPortNumber:=Vnum;
   WITH Views[Vnum] DO
      BEGIN
         Vx1Ext:=Vx1-2*xLabelSpace;
         IF Vx1Ext<0 THEN Vx1Ext:=0;
         Vy1Ext:=Vy1-yLabelSpace;
         IF Vy1Ext<0 THEN Vy1Ext:=0;
         Vx2Ext:=Vx2+xLabelSpace;
         IF Vx2Ext>GetMaxX THEN Vx2Ext:=GetMaxX;
         Vy2Ext:=Vy2+yLabelSpace;
         IF Vy2Ext>GetMaxY THEN Vy2Ext:=GetMaxY;
      END;
   x1:=Vx1Ext/GetMaxX; x2:=Vx2Ext/GetMaxX;
   y1:=1-Vy2Ext/GetMaxY; y2:=1-Vy1Ext/GetMaxY;
   DefineViewPort(Vnum+10,x1,x2,y1,y2);
   CloseViewPort(Vnum+10);
END; {ClearOversizedViewPort}

PROCEDURE PrepViewPort(Vnum :integer);
{Open viewport with space cleared for labels.}
BEGIN
   ClearOversizedViewPort(Vnum);
   OpenViewPort(Vnum)
END; {PrepViewPort}

PROCEDURE PrepAxisInput(VAR Min,Max,Tic :real; nDiv,decimal :integer);
VAR
   dx,fac :real;
   b,nTic :integer;
BEGIN
   dx:=Max-Min;
   b:=Trunc(Log10(dx))+1;
   fac:=Pwr(10.0,decimal-b);
   IF Min>0.0 THEN Min:=Trunc(fac*Min)/fac
   ELSE IF Min<0.0 THEN Min:=-(1+Trunc(-fac*Min))/fac;
   Tic:=Trunc(10*fac*dx/nDiv)/(10*fac);
   nTic:=Trunc(dx/Tic) div (nDiv+1);
   Tic:=(nTic+1)*Tic;
END; {PrepAxisInput}

PROCEDURE SpecialAxis(Snum :integer; hMin,hMax :real; nTicX :integer;
                      vMin,vMax :real; nTicY :integer);
{Define scale Snum and draw axis with origin at (0.0,0.0)}
{if this is within range of plot.}
VAR
   TicX,TicY :real;
BEGIN
   PrepAxisInput(hMin,hMax,TicX,nTicX,2);
   PrepAxisInput(vMin,vMax,TicY,nTicY,2);
   DefineScale(Snum,hMin,hMax,vMin,vMax);
   SetColor(Whitte);
   IF (hMin<=0.0) AND (hMax>=0.0) THEN
      BEGIN
         IF (vMin<=0.0) AND (vMax>=0.0) THEN Axis(0.0,0.0,TicX,TicY)
         ELSE Axis(0.0,vMin,TicX,TicY)
      END
   ELSE
      BEGIN
         IF (vMin<=0.0) AND (vMax>=0.0) THEN Axis(hMin,0.0,TicX,TicY)
         ELSE Axis(hMin,vMin,TicX,TicY)
      END
END; {SpecialAxis}

PROCEDURE ChangePlotScales(VAR hMin,hMax :real; hUnit :string;
                           hMinDef,hDifDef :real;
                           VAR vMin,vMax :real; vUnit :string;
                           vMinDef,vDifDef :real;
                           VAR Changed,AutoSet :boolean);
VAR
   InputScr :TInputScreen;
BEGIN
   {New(InputScr); {Mac only}
   WITH InputScr DO
      BEGIN
         Init;
         DefineInputPort(0.32,0.68,0.3,0.8);
         LoadLine('');
         LoadLine(       '         Plot Scales');
         LoadLine('');
         LoadLine(ConCat(' horizontal range ',hUnit));
         LoadLine(       '    {      } to {      }');
         LoadLine(ConCat(' vertical range ',vUnit));
         LoadLine(       '    {      } to {      }');
         IF AutoSet THEN
            LoadLine(    '    or auto scale? #F');
         LoadLine('');
         LoadLine(       '   [  OK  ]    [Cancel]');
         SetNumber(1,hMin); SetNumber(2,hMax);
         SetNumber(3,vMin); SetNumber(4,vMax);
         AcceptScreen;
         IF Canceled THEN Changed:=False
         ELSE
            BEGIN
               hMin:=GetNumber(1);
               IF (hMinDef>=0.0) AND (hMin<0.0) THEN
                  RealInputWarn('horizontal min',hMin,hMinDef,6);
               hMax:=GetNumber(2);
               IF hMax<=hMin THEN
                  RealInputWarn('horizontal max',hMax,hMin+hDifDef,6);
               IF AutoSet THEN AutoSet:=GetBoolean(5);
               IF NOT AutoSet THEN
                  BEGIN
                     vMin:=GetNumber(3);
                     IF (vMinDef>=0.0) AND (vMin<0.0) THEN
                        RealInputWarn('vertical min',vMin,vMinDef,6);
                     vMax:=GetNumber(4);
                     IF vMax<=vMin THEN
                        RealInputWarn('vertical max',vMax,vMin+vDifDef,6);
                  END;
               Changed:=True;
            END;
         Done;
      END;
   {Dispose(InputScr); {Mac only}
END; {ChangePlotScales}

PROCEDURE LegendEntry(Position :integer; Color :word; Name :string);
{Post entry in legend.}
VAR
   pos,height :word;
BEGIN
   SetColor(Color);
   height:=TextHeight('O');
   pos:=HeightFactor*Position*height;
   LineTP(2,pos+height div 2,25,pos+height div 2);
   SetColor(Whitte);
   OutTextXY(30,pos,Name)
END; {LegendEntry}

PROCEDURE PostEulerAngles(a,b,c :real);
BEGIN
   CloseViewPort(1);
   SelectViewPort(1);
   Print(1,2,'Euler angles:');
   Print(4,3,ConCat('alpha: ',NumStr(a,3,0),' deg'));
   Print(4,4,ConCat(' beta: ',NumStr(b,3,0),' deg'));
   Print(4,5,ConCat('gamma: ',NumStr(c,3,0),' deg'));
END; {PostEulerAngles}

{--------Routines for text input and output----------------------------}

PROCEDURE Info(VAR InfoScr :InfoScrType; NumLines,Vnum :integer);
{Display text window for information.}
VAR
   i           :integer;
   Environment :TEnvironment;
BEGIN
   {New(Environment); {Mac only}
   Environment.Save;
   Environment.Standardize;
   OpenViewPort(Vnum);
   HideMouse;
   FOR i:=1 TO NumLines DO Print(1,i,InfoScr[i]);
   Environment.Reset;
   {Dispose(Environment); {Mac only}
   ShowMouse;
END; {Info}

PROCEDURE IntInputWarn(Name :string; VAR InpValue :integer;
                       DefValue :integer; field :integer);
{Warn of problem with integer input value.}
BEGIN
   Announce(ConCat('Value of ',NumStr(InpValue,field,0),
                   ' unacceptable for ',Name,'.  Will use ',
                   NumStr(DefValue,field,0),' instead.'));
   InpValue:=DefValue
END; {IntInputWarn}

PROCEDURE RealInputWarn(Name :string; VAR InpValue :real;
                        DefValue :real; field :integer);
{Warn of problem with real input value.}
BEGIN
   Announce(ConCat('Value of ',Num2Str(InpValue,field),
                   ' unacceptable for ',Name,'.  Will use ',
                   Num2Str(DefValue,field),' instead.'));
   InpValue:=DefValue
END; {RealInputWarn}

PROCEDURE SelectMassAndUnits(VAR Particle :ParticleType;
                             VAR Mass,Lfac :real;
                             VAR Munit,Eunit,Lunit :char;
                             VAR Modified :boolean);
VAR
   MassScr  :TInputScreen;
   SaveMass :real;
BEGIN
   Modified:=False;
   SaveMass:=Mass;
   {New(MassScr);  {Mac only}
   WITH MassScr DO
      BEGIN
         Init;
         DefineInputPort(0.15,0.85,0.18,0.83);
         LoadLine('');
         LoadLine('     Particle type/mass; energy & length units');
         LoadLine('');
         LoadLine('   #1  electron,  mass: 0.511 MeV/c^2');
         LoadLine('          energy unit: eV     length unit: nm');
         LoadLine('   #1  muon,  mass: 0.106 GeV/c^2');
         LoadLine('          energy unit: keV    length unit: pm');
         LoadLine('   #1  proton,  mass: 0.938 GeV/c^2');
         LoadLine('          energy unit: MeV    length unit: fm');
         LoadLine('   #1  custom, mass: {      } #2 MeV/c^2   #2 Gev/c^2');
         LoadLine('          energy unit: #3 eV  #3 keV  #3 MeV  #3 GeV');
         LoadLine('          length unit: #4 nm  #4 pm   #4 fm');
         LoadLine('');
         LoadLine('   [  OK  ]    [Cancel]');
         LoadLine('');
         CASE Particle OF
            electron :SetRadioButton('1',1);
            muon     :SetRadioButton('1',2);
            proton   :SetRadioButton('1',3);
            custom   :SetRadioButton('1',4);
            END; {case}
         SetNumber(5,Mass);
         CASE Munit OF
            'M' :SetRadioButton('2',1);
            'G' :SetRadioButton('2',2);
            END; {case}
         CASE Eunit OF
            ' ' :SetRadioButton('3',1);
            'k' :SetRadioButton('3',2);
            'M' :SetRadioButton('3',3);
            'G' :SetRadioButton('3',4);
            END; {case}
         CASE Lunit OF
            'n' :SetRadioButton('4',1);
            'p' :SetRadioButton('4',2);
            'f' :SetRadioButton('4',3);
            END; {case}
         AcceptScreen;
         IF NOT Canceled THEN
            BEGIN
               Modified:=True;
               CASE GetRadioButton('1') OF
                  1 :BEGIN
                        Particle:=electron;
                        Mass:=0.511; Munit:='M'; Eunit:=' '; Lunit:='n';
                     END;
                  2 :BEGIN
                        Particle:=muon;
                        Mass:=0.106; Munit:='G'; Eunit:='k'; Lunit:='p';
                     END;
                  3 :BEGIN
                        Particle:=proton;
                        Mass:=0.938; Munit:='G'; Eunit:='M'; Lunit:='f';
                     END;
                  4 :BEGIN
                        Particle:=custom;
                        Mass:=GetNumber(5);
                        IF Mass<=0.0 THEN
                           RealInputWarn('mass',Mass,SaveMass,6);
                        CASE GetRadioButton('2') OF
                           1 :Munit:='M';
                           2 :Munit:='G';
                           END; {case}
                        CASE GetRadioButton('3') OF
                           1 :Eunit:=' ';
                           2 :Eunit:='k';
                           3 :Eunit:='M';
                           4 :Eunit:='G';
                           END; {case}
                        CASE GetRadioButton('4') OF
                           1 :Lunit:='n';
                           2 :Lunit:='p';
                           3 :Lunit:='f';
                           END; {case}
                     END;
                  END; {case}
               CASE Lunit OF
                  'n' :Lfac:=1.0;
                  'p' :Lfac:=1.0e-3;
                  'f' :Lfac:=1.0e-6;
                  END; {case}
            END;
         Done;
      END;
   {Dispose(MassScr);  {Mac only}
END; {SelectMassAndUnits}

{--------Routines for computation-------------------------------------}

FUNCTION ExpSafe(arg :real) :real;
{Compute exp(arg) safely.  Protected against overflows and underflows.}
BEGIN
   IF arg>80.0 THEN ExpSafe:=exp(80.0)
   ELSE IF arg<-80.0 THEN ExpSafe:=0.0
   ELSE ExpSafe:=exp(arg);
END; {ExpSafe}

PROCEDURE ComputeZeta(V0,L0,Mass :real; Eunit,Lunit,Munit :char;
                      VAR zeta :real);
{Compute dimensionless parameter of Schrodinger equation.}
CONST
   hbarc=197.327; {MeV-fm}
VAR
   fac :integer;
BEGIN
   fac:=18;
   CASE Eunit OF
      ' ' :fac:=fac+0;
      'k' :fac:=fac+3;
      'M' :fac:=fac+6;
      'G' :fac:=fac+9
      END; {case}
   CASE Lunit OF
      'n' :fac:=fac-18;
      'p' :fac:=fac-24;
      'f' :fac:=fac-30
      END; {case}
   CASE Munit OF
      ' ' :fac:=fac+0;
      'k' :fac:=fac+3;
      'M' :fac:=fac+6;
      'G' :fac:=fac+9
      END; {case}
   zeta:=2*Mass*sqr(L0)*V0*Pwr(10,fac)/sqr(hbarc);
END; {ComputeZeta}

{--------Routines for file management---------------------------------}

PROCEDURE GetFileToOpen(VAR FileName :string; VAR WishToAbort :boolean);
VAR
   tempFN :string;
BEGIN
   tempFN:=OpenFile('*.dat');
   SetColor(Whitte);
   IF length(tempFN)=0 THEN WishToAbort:=True
   ELSE
      BEGIN
         WishToAbort:=False;
         FileName:=tempFN
      END
END; {GetFileToOpen}

PROCEDURE AskOverWrite(FileName :string; VAR WishToAbort :boolean);
VAR
   OverWriteScr :TInputScreen;
BEGIN
   {New(OverWriteScr); {Mac only}
   WITH OverWriteScr DO
      BEGIN
         Init;
         DefineInputPort(0.12,0.82,0.4,0.65);
         LoadLine('');
         LoadLine(
            ConCat('   File ',FileName,' already exists.'));
         LoadLine( '   Overwrite?');
         LoadLine('');
         LoadLine( '        [  OK  ]      [Cancel]');
         LoadLine('');
         AcceptScreen;
         IF Canceled THEN WishToAbort:=True
         ELSE WishToAbort:=False;
         Done;
      END;
   {Dispose(OverWriteScr); {Mac only}
END; {AskOverWrite}

PROCEDURE ReadDVector(VAR DataFile :text;
                      Num :integer; VAR Vec :DVector; divisor :real);
VAR
   nExtra,nRow,i,iRow :integer;
   temp :real;
BEGIN
   nExtra:=Num MOD NumPerRow; nRow:=Num DIV NumPerRow;
   FOR iRow:=1 TO nRow DO
      BEGIN
         FOR i:=1 TO NumPerRow DO
            BEGIN
               read(DataFile,temp);
               Vec.Put(i+NumPerRow*(iRow-1),temp/divisor);
            END;
         readln(DataFile);
      END;
   FOR i:=1 TO nExtra DO
      BEGIN
         read(DataFile,temp);
         Vec.Put(i+NumPerRow*nRow,temp/divisor);
      END;
   IF nExtra>0 THEN readln(DataFile);
END; {ReadDVector}

PROCEDURE WriteDVector(VAR DataFile :text;
                       Num :integer; VAR Vec :DVector; multiplier :real);
VAR
   nExtra,nRow,i,iRow :integer;
BEGIN
   nExtra:=Num MOD NumPerRow; nRow:=Num DIV NumPerRow;
   FOR iRow:=1 TO nRow DO
      BEGIN
         FOR i:=1 TO NumPerRow DO
            write(DataFile,
                  multiplier*Vec.Value(i+NumPerRow*(iRow-1)) :9:6,'  ');
         writeln(DataFile);
      END;
   FOR i:=1 TO nExtra DO
      write(DataFile,multiplier*Vec.Value(i+NumPerRow*nRow) :9:6,'  ');
   IF nExtra>0 THEN writeln(DataFile);
END; {WriteDVector}

PROCEDURE ReadParticle(VAR DataFile :text; VAR Particle :ParticleType);
VAR
   name :string;
BEGIN
   readln(DataFile,name);
   IF name='electron' THEN Particle:=electron
   ELSE IF name='muon' THEN Particle:=muon
   ELSE IF name='proton' THEN Particle:=proton
   ELSE Particle:=custom;
END; {ReadParticle}

PROCEDURE WriteParticle(VAR DataFile :text; Particle :ParticleType);
BEGIN
   CASE Particle OF
      electron :writeln(DataFile,'electron');
      muon     :writeln(DataFile,'muon');
      proton   :writeln(DataFile,'proton');
      custom   :writeln(DataFile,'custom');
      END; {case}
END; {WriteParticle}

PROCEDURE VerifyNew(Saved :boolean; VAR Verified :boolean);
{Verify decision to start new calculation.}
VAR
   NewScr :TInputScreen;
BEGIN
   {New(NewScr); {Mac only}
   IF Saved THEN Verified:=True
   ELSE WITH NewScr DO
      BEGIN
         Init;
         DefineInputPort(0.12,0.82,0.35,0.65);
         LoadLine('');
         LoadLine('  Current results not saved.');
         LoadLine('  Should they be overwritten and lost?');
         LoadLine('');
         LoadLine('    [  OK  ]    [Cancel]');
         AcceptScreen;
         IF Canceled THEN Verified:=False
         ELSE Verified:=True;
         Done;
      END;
   {Dispose(NewScr); {Mac only}
END; {VerifyNew}

PROCEDURE VerifyQuit(VAR DataFile :text; VAR FileName :string;
                     VAR WishToSave,WishToQuit :boolean);
VAR
   QuitScr :TInputScreen;
BEGIN
   {New(QuitScr);  {Mac only}
   WITH QuitScr DO
      BEGIN
         Init;
         DefineInputPort(0.12,0.82,0.35,0.6);
         LoadLine('');
         LoadLine('  Current results not saved.');
         LoadLine('');
         LoadLine('    [  OK  ]  [ Save ]  [Cancel]');
         AcceptScreen;
         WishToSave:=False;
         IF Canceled THEN
            WishToQuit:=False
         ELSE IF GetBoolean(2) THEN
            BEGIN
               Done;
               Init;
               DefineInputPort(0.12,0.82,0.35,0.6);
               LoadLine('');
               LoadLine(ConCat('  Save results in file ',FileName,'?'));
               LoadLine('');
               LoadLine('     [  OK  ]    [Cancel]');
               AcceptScreen;
               IF NOT Canceled THEN WishToSave:=True
               ELSE WishToQuit:=False;
            END;
         Done;
      END;
   {Dispose(QuitScr); {Mac only}
END; {VerifyQuit}

END. {Sc3DCmn}
