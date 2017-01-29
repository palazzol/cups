{Unit of routines for plots of Hatom wave functions.                 }
{Used by Hatom.pas.                                                  }
{                                                                    }
{Author: John R. Hiller                                              }
{Version: 1.0                9/12/95                                 }
{Copyright (c) 1995  John Wiley & Sons, Inc.                         }
{                                                                    }
UNIT HatmPlot;

INTERFACE

USES Graph,CUPS,CUPSmupp,CUPSfunc,CUPSproc,CUPSgui,CUPSgrph,
     HatmMach,HatmCalc;

TYPE
   Str3=string[3];
   TView3D=object(TGraph3D)   {input of 3D viewing parameters}
         Sliders  :TSliders;
         Vsave    :integer;
         OldColor :word;
         PROCEDURE Init;      {override; {Mac only}
         PROCEDURE Draw(a,b,c,BoxSize,BoxDist :real);
         PROCEDURE Resize(VAR BoxSize :real; BoxDist :real);
         PROCEDURE Persp(BoxSize :real; VAR BoxDist :real);
         FUNCTION  Changed :Boolean;
         PROCEDURE Rotate(VAR a,b,c :real);
         PROCEDURE Done;
      END; {TView3D}

   FileDescType=record
      DataFile    :text;
      RecFile     :RfileType;
      FileName,
      RecFileName,
      DefFileName :string;
      END; {FileDescType}

{----------Routines for general graphics output-------------------------}

PROCEDURE DefineAllViewPorts;
PROCEDURE ClearOversizedViewPort(Vnum :integer);
PROCEDURE PrepViewPort(Vnum :integer);
PROCEDURE PrepAxisInput(VAR Min,Max,Tic :real; nDiv,decimal :integer);
PROCEDURE SpecialAxis(Snum :integer; hMin,hMax :real; nTicX :integer;
                      vMin,vMax :real; nTicY :integer);
PROCEDURE PostEulerAngles(a,b,c :real);

{--------Routines for wave function plots---------------------------------}

PROCEDURE PlotSurface(VAR WF :WFtype; HideCurs :boolean);
PROCEDURE PrepContour(s1Max,s2Max :real);
PROCEDURE PlotContour(VAR WF :WFtype; Post,HideCurs :boolean);
PROCEDURE PlotSmall(VAR WF :WFtype; VAR Potl :PotlType);
PROCEDURE PrepPlot(VAR WF :WFtype; VAR Potl :PotlType;
                   Post :boolean; VAR vp :integer);
PROCEDURE PlotWF(VAR WF :WFtype; Potl :PotlType; Post :boolean);
PROCEDURE RotateView(VAR WF :WFtype; VAR Potl :PotlType);
PROCEDURE ModViewWF(VAR FileDesc :FileDescType;
                    VAR WF :WFtype; VAR Potl :PotlType);

{-------Routines for construction of plot information screens---------}

PROCEDURE PrepUWFPlotScr(VAR WF :WFtype; VAR Potl :PotlType);
PROCEDURE PrepDWFPlotScr(VAR FileDesc :FileDescType;
                         VAR WF :WFtype; VAR Potl :PotlType);
PROCEDURE PrepSWFPlotScr(VAR FileDesc :FileDescType;
                         VAR WF :WFtype; VAR Potl :PotlType);
PROCEDURE PrepBWFPlotScr(VAR WF :WFtype; VAR Potl :PotlType);

IMPLEMENTATION

{--------Methods for TView3D object------------------------------------}

PROCEDURE TView3D.Init;
BEGIN
   Vsave:=ViewPortNumber;
   DefineViewPort(9,0.0,0.25,0.08,0.38);
   SelectViewPort(9);
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
END; {TView3D.Resize}

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
END; {TView3D.Persp}

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
END; {TView3D.Changed}

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
END; {TView3D.Done}

{----------Routines for general graphics output-------------------------}

PROCEDURE DefineAllViewPorts;
BEGIN
   DefineViewPort(1,0.0,0.25,0.06,0.43);   {viewport for legends}
                                           {and Euler angles}
   DefineScale(1,0.0,1.0,0.0,1.0);         {scale for legends}
   DefineViewPort(2,0.1,0.65,0.47,0.87);   {viewport for plots}
   DefineViewPort(3,0.08,0.96,0.12,0.87);  {viewport for large plots}
   DefineViewPort(4,0.325,0.65,0.13,0.43); {viewport for small plots}
   DefineViewPort(5,0.7,1.0,0.08,0.92);    {viewport for parameter list}
   DefineViewPort(11,0.0,0.5,0.53,1.00);
   DefineViewPort(12,0.5,1.0,0.53,1.00);
   DefineViewPort(13,0.5,1.0,0.06,0.53);
   DefineViewPort(14,0.0,0.5,0.06,0.53);
END; {DefineAllViewPorts}

PROCEDURE ClearOversizedViewPort(Vnum :integer);
{Clear viewport and space for labels.}
CONST
   LabelSpace=0.04;
VAR
   ViewSave :View;
   xLabelSpace,yLabelSpace,Vx1Ext,Vy1Ext,Vx2Ext,Vy2Ext :integer;
   x1,y1,x2,y2 :real;
BEGIN
   xLabelSpace:=round(LabelSpace*GetMaxX);
   yLabelSpace:=round(1.5*LabelSpace*GetMaxY);
   ViewPortNumber:=Vnum;
   ViewSave:=Views[Vnum];
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
   DefineViewPort(Vnum,x1,x2,y1,y2);
   CloseViewPort(Vnum);
   ViewPortNumber:=Vnum;
   Views[Vnum]:=ViewSave;
   WITH Views[Vnum] DO
      BEGIN
         x1:=Vx1/GetMaxX; x2:=Vx2/GetMaxX;
         y1:=1-Vy2/GetMaxY; y2:=1-Vy1/GetMaxY;
      END;
   DefineViewPort(Vnum,x1,x2,y1,y2);
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
   nTic:=trunc(dx/Tic) div (nDiv+1);
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

PROCEDURE PostEulerAngles(a,b,c :real);
BEGIN
   CloseViewPort(1);
   SelectViewPort(1);
   Print(1,2,'Euler angles:');
   Print(4,3,ConCat('alpha: ',NumStr(a,3,0),' deg'));
   Print(4,4,ConCat(' beta: ',NumStr(b,3,0),' deg'));
   Print(4,5,ConCat('gamma: ',NumStr(c,3,0),' deg'))
END; {PostEulerAngles}

{--------Routines for wave function plots---------------------------------}

PROCEDURE PlotSurface(VAR WF :WFtype; HideCurs :boolean);
VAR
   s :PointType;
   Surface  :TGraph3D;
BEGIN
   HideMouse;
   WITH WF DO
      BEGIN
         {New(Surface); {Mac only}
         Surface.Init;
         Surface.Set3DScales(-s1Max,s1Max,-s2Max,s2Max,VertMin,VertMax);
         Surface.SetDistance(Size,Distance);
         Surface.SetViewAngles(alfa,beta,gamma);
         IF s2Max>s1Max THEN Surface.SetBoxRatios(s1Max/s2Max,1.0,0.6)
         ELSE Surface.SetBoxRatios(1.0,s2Max/s1Max,0.6);
         Surface.SurfaceColor:=LightMagenta;
         Surface.DrawSurface(PsiSlice);
         SetColor(Whitte);
         Surface.Vector3D(1.1*s1Max,0.0,0.0,1.5*s1Max,0.0,0.0);
         Surface.GetPoint(1.5*s1Max,0.0,0.0,s);
         OutTextXY(s.x+3,s.y,s1Label);
         CASE sPlane OF
            1,2 :BEGIN
                    Surface.Vector3D(0.0,-1.1*s2Max,0.0,0.0,-1.5*s2Max,0.0);
                    Surface.GetPoint(0.0,-1.5*s2Max,0.0,s);
                    OutTextXY(s.x+3,s.y,s2Label);
                 END;
            3   :BEGIN
                    Surface.Vector3D(0.0,1.1*s2Max,0.0,0.0,1.5*s2Max,0.0);
                    Surface.GetPoint(0.0,1.5*s2Max,0.0,s);
                    OutTextXY(s.x-9,s.y,s2Label);
                 END;
            END; {case}
         Plotted:=True;
         SetColor(Whitte);
         {Dispose(Surface); {Mac only}
      END;
   IF NOT HideCurs THEN ShowMouse;
END; {PlotSurface}

PROCEDURE PrepContour(s1Max,s2Max :real);
CONST
   AspectRatio=0.75;
VAR
   Side,
   Vpx1,Vpx2,Vpy1,Vpy2   :real;
   Vnum                  :integer;
BEGIN
   Vnum:=ViewPortNumber;
   WITH Views[Vnum] DO
      BEGIN
         Vpx1:=Vx1/GetMaxX; Vpx2:=Vx2/GetMaxX;
         Vpy1:=1.0-Vy2/GetMaxY; Vpy2:=1.0-Vy1/GetMaxY
      END;
   Side:=Vpx2-Vpx1;
   IF Side>Vpy2-Vpy1 THEN Side:=Vpy2-Vpy1;
   IF AspectRatio*s1Max/s2Max<1.0 THEN
      DefineViewPort(Vnum,Vpx1,Vpx1+Side*AspectRatio*s1Max/s2Max,
                     Vpy1,Vpy1+Side)
   ELSE
      DefineViewPort(Vnum,Vpx1,Vpx1+Side,
                     Vpy1,Vpy1+Side*s2Max/s1Max/AspectRatio);
END; {PrepContour}

PROCEDURE PlotContour(VAR WF :WFtype; Post,HideCurs :boolean);
VAR
   Contour     :TContour;
BEGIN
   HideMouse;
   WITH WF DO
      BEGIN
         {New(Contour); {Mac only}
         Contour.Init;
         Contour.DrawMatrix(PsiSlice);
         IF Post THEN
            BEGIN
               IF Zoom THEN Contour.PrintLegend(4)
               ELSE
                  BEGIN
                     SelectViewPort(1); Contour.PrintLegend(4)
                  END;
            END;
         Plotted:=True;
         {Dispose(Contour); {Mac only}
      END;
   IF NOT HideCurs THEN ShowMouse;
END; {PlotContour}

PROCEDURE PlotSmall(VAR WF :WFtype; VAR Potl :PotlType);
VAR
   x1,x2,y1,y2,
   yMax,Vscale  :real;
   nS           :integer;
BEGIN
   WITH WF,Potl DO
      BEGIN
         CASE Computed OF
            Undistorted :BEGIN
                            PrepViewPort(4);
                            SpecialAxis(2,0.0,L0*rMax,3,-1.0,1.0,4);
                            PutLabel(Left,'arbitrary scale');
                            PutLabel(Bottom,ConCat('r (',Lunit,'m)'));
                            PlotLine(0.0,0.0,L0*rMax,0.0);
                            SetColor(LightMagenta);
                            Print(6,1,'radial wave function');
                            x2:=0.0; y2:=Rnl.Value(1);
                            FOR nS:=1 TO nSteps DO
                               BEGIN
                                  x1:=x2; y1:=y2;
                                  x2:=x2+dr; y2:=Rnl.Value(nS+1);
                                  PlotLine(L0*x1,y1,L0*x2,y2);
                               END;
                            SetColor(LightGreen);
                            Print(6,2,'radial prob density');
                            x2:=0.0; yMax:=0.0;
                            FOR nS:=1 TO nSteps DO
                               BEGIN
                                  x2:=x2+dr; y2:=sqr(x2*Rnl.Value(nS+1));
                                  IF y2>yMax THEN yMax:=y2;
                               END;
                            IF yMax=0.0 THEN yMax:=1.0;
                            x2:=0.0; y2:=0.0;
                            FOR nS:=1 TO nSteps DO
                               BEGIN
                                  x1:=x2; y1:=y2;
                                  x2:=x2+dr; y2:=sqr(x2*Rnl.Value(nS+1))/yMax;
                                  PlotLine(L0*x1,y1,L0*x2,y2);
                               END;
                         END;
            Distorted :if Mode=Auto THEN
                          BEGIN
                             PrepViewPort(4);
                             SpecialAxis(2,FieldMin,FieldMax,3,
                                           DipoleMin,DipoleMax,4);
                             PutLabel(Left,ConCat('p_z (e-',pzUnit,'m)'));
                             PutLabel(Bottom,FieldLabel);
                             SetColor(LightGreen);
                             PutLabel(Inside,'dipole moment');
                             x2:=FieldMin;
                             y2:=pz0*Dipole.Value(NautoSteps+1);
                             FOR nS:=NautoSteps DOWNTO NstepsRemain+1 DO
                                BEGIN
                                   x1:=x2; y1:=y2;
                                   x2:=x2+dField; y2:=pz0*Dipole.Value(nS);
                                   PlotLine(x1,y1,x2,y2);
                                END;
                          END;
            Shared :if (Mode=Auto) AND RecordVeff THEN
                       BEGIN
                          PrepViewPort(4);
                          SpecialAxis(2,0.0,L0*MaxSep,3,VeffMin,VeffMax,4);
                          PutLabel(Left,ConCat('V_eff (',Eunit,'eV)'));
                          PutLabel(Bottom,ConCat('r (',Lunit,'m)'));
                          SetColor(LightCyan);
                          PutLabel(Inside,'V_eff');
                          x2:=MaxSep;
                          y2:=V0*Veff.Value(NautoSteps);
                          FOR nS:=NautoSteps-1 DOWNTO NstepsRemain+1 DO
                             BEGIN
                                x1:=x2; y1:=y2;
                                x2:=x2+dSep; y2:=V0*Veff.Value(nS);
                                PlotLine(L0*x1,y1,L0*x2,y2);
                             END;
                       END;
            Binding :BEGIN
                        PrepViewPort(4);
                        SpecialAxis(2,0.0,L0*rMax,3,-1.0,1.0,4);
                        PutLabel(Left,'R_nl (arb. scale)');
                        PutLabel(Bottom,ConCat('r (',Lunit,'m)'));
                        PlotLine(0.0,0.0,L0*rMax,0.0);
                        SetColor(LightMagenta);
                        Print(6,1,'radial wave function');
                        x2:=0.0; y2:=Rnl.Value(1);
                        FOR nS:=1 TO nSteps DO
                           BEGIN
                              x1:=x2; y1:=y2;
                              x2:=x2+dr; y2:=Rnl.Value(nS+1);
                              PlotLine(L0*x1,y1,L0*x2,y2);
                           END;
                        SetColor(LightCyan);
                        Print(6,2,'V_eff');
                        Vscale:=abs(VeffMin);
                        IF Vscale<VeffMax THEN Vscale:=VeffMax;
                        IF Vscale=0.0 THEN Vscale:=1.0;
                        x2:=MaxSep;
                        y2:=V0*Veff.Value(NautoSteps)/Vscale;
                        FOR nS:=NautoSteps-1 DOWNTO 1 DO
                           BEGIN
                              x1:=x2; y1:=y2;
                              x2:=x2+dSep; y2:=V0*Veff.Value(nS)/Vscale;
                              PlotLine(L0*x1,y1,L0*x2,y2);
                           END;
                     END;
            END; {case}
         SetColor(Whitte);
      END;
END; {PlotSmall}

PROCEDURE PrepPlot(VAR WF :WFtype; VAR Potl :PotlType;
                   Post :boolean; VAR vp :integer);
BEGIN
   IF WF.Zoom THEN
      BEGIN
         vp:=3;
         ClearOversizedViewPort(3);
      END
   ELSE
      BEGIN
         IF Post THEN
            BEGIN
               ClearOversizedViewPort(3);
               PlotSmall(WF,Potl);
               IF WF.Surf THEN PostEulerAngles(WF.alfa,WF.beta,WF.gamma);
            END;
         Info(WF.PlotInfoScr,25,5);
         vp:=2;
      END;
   SelectViewPort(vp);
   IF NOT WF.Surf THEN PrepContour(WF.s1Max,WF.s2Max);
   SelectViewPort(vp);
END; {PrepPlot}

PROCEDURE PlotWF(VAR WF :WFtype; Potl :PotlType; Post :boolean);
{Plot wave function.}
VAR
   vp :integer;
BEGIN
   IF NOT WF.Plotted THEN WITH WF DO
      BEGIN
         PrepPlot(WF,Potl,Post,vp);
         OpenViewPort(vp);
         IF Surf THEN
            BEGIN
               PutLabel(Top,ConCat('Probability density (',
                                   s2Label,'-',s1Label,' plane)'));
               PlotSurface(WF,False);
            END
         ELSE
            BEGIN
               PlotContour(WF,Post,False);
               SelectViewPort(vp);
               PutLabel(Top,ConCat('Probability density (',
                                   s2Label,'-',s1Label,' plane)'));
               DefineAllViewPorts;
            END;
      END;
END; {PlotWF}

PROCEDURE RotateView(VAR WF :WFtype; VAR Potl :PotlType);
{Rotate view of surface and replot.}
VAR
   RVHelpScr :HelpScrType;
   RVkeys    :THotKeys;
   Rkey      :byte;
   View      :TView3D;
   SaveSize,SaveDistance,SaveAlfa,SaveBeta,SaveGamma,BoxSize :real;
   WishToAccept,SaveZoom :boolean;
BEGIN
   RVHelpScr[01]:='';
   RVHelpScr[02]:=' <F1>-show this help screen.   ';
   RVHelpScr[03]:='';
   RVHelpScr[04]:=' <F2>-plot surface. This regenerates the';
   RVHelpScr[05]:='    surface plot to illustrate the effects';
   RVHelpScr[06]:='    of changes made.';
   RVHelpScr[07]:='';
   RVHelpScr[08]:=' <F3>-change size of plot. Each press of the key';
   RVHelpScr[09]:='    changes the size by one fixed step in a cycle.';
   RVHelpScr[10]:='';
   RVHelpScr[11]:=' <F4>-change perspective of plot. Each press';
   RVHelpScr[12]:='    of the key changes the distance of the plot';
   RVHelpScr[13]:='    by one fixed step in a cycle.';
   RVHelpScr[14]:='';
   RVHelpScr[15]:=' <F5>-reset to default view.';
   RVHelpScr[16]:='';
   RVHelpScr[17]:=' Sliders - change Euler angles. Use mouse to drag';
   RVHelpScr[18]:='    a block, or click on arrows.  Use arrow keys';
   RVHelpScr[19]:='    to select and move block.';
   RVHelpScr[20]:='';
   RVHelpScr[21]:=' The changes made are immediately visible in the';
   RVHelpScr[22]:=' appearance of the small box in the lower left.';
   RVHelpScr[23]:='';
   RVHelpScr[24]:=' <Enter>-accept view and return';
   RVHelpScr[25]:='    to previous screen.';

   WITH WF DO
      BEGIN
         {New(RVkeys); {Mac only}
         WITH RVkeys DO
            BEGIN
               Init(6);
               key[1]:='F1-Help';
               key[2]:='F2-Plot';
               key[3]:='F3-Size';
               key[4]:='F4-Distance';
               key[5]:='F5-Reset';
               key[6]:='RETURN-Accept';
               Display
            END;
         SaveSize:=Size; SaveDistance:=Distance;
         SaveAlfa:=alfa; SaveBeta:=beta; SaveGamma:=gamma;
         SaveZoom:=Zoom; Zoom:=False;
         Surf:=True;
         Plotted:=False;
         ClearOversizedViewPort(3);
         PlotWF(WF,Potl,False);
         {New(View); {Mac only}
         BoxSize:=0.6*Size;
         View.Init;
         View.Draw(alfa,beta,gamma,BoxSize,Distance);
         WishToAccept:=False;
         WHILE NOT WishToAccept DO
            BEGIN
               CheckForEvents;
               IF Event.ReadKey=Escape THEN
                  BEGIN
                     WishToAccept:=True; Plotted:=False;
                     Size:=SaveSize; Distance:=SaveDistance;
                     alfa:=SaveAlfa; beta:=SaveBeta; gamma:=SaveGamma;
                  END
               ELSE IF RVkeys.Pressed(Rkey) THEN
                  CASE Rkey OF
                     1 :Help(RVHelpScr);
                     2 :BEGIN
                           Plotted:=False;
                           PlotWF(WF,potl,False)
                        END;
                     3 :View.Resize(BoxSize,Distance);
                     4 :View.Persp (BoxSize,Distance);
                     5 :BEGIN
                           View.Done;
                           SetSurfaceView(alfa,beta,gamma,
                                          Size,Distance);
                           BoxSize:=0.6*Size;
                           View.Init;
                           View.Draw(alfa,beta,gamma,BoxSize,Distance);
                           Plotted:=False;
                        END;
                     6 :WishToAccept:=True
                     END {case}
               ELSE IF View.Changed THEN
                  BEGIN
                     View.Rotate(alfa,beta,gamma);
                     Plotted:=False
                  END;
               Size:=BoxSize/0.6;
            END;
         RVkeys.Clear;
         View.Done;
         {Dispose(View); Dispose(RVkeys); {Mac only}
         Zoom:=SaveZoom;
         Plotted:=False;
         PlotWF(WF,Potl,True)
      END;
END; {RotateView}

PROCEDURE ModViewWF(VAR FileDesc :FileDescType;
                    VAR WF :WFtype; VAR Potl :PotlType);
VAR
   InputScr                :TInputScreen;
   Modify,
   EnoughMemory,Enough     :boolean;
   sPlaneOld,
   NrhoStepsOld,NzStepsOld :integer;
BEGIN
   {New(InputScr); {Mac only}
   WITH InputScr DO
      REPEAT
         EnoughMemory:=True;
         Init;
         DefineInputPort(0.15,0.85,0.125,0.9);
         LoadLine('');
         LoadLine('                   Modify view');
         LoadLine('');
         LoadLine(' Plot type:');
         LoadLine('    #1  x-y plane');
         LoadLine('    #1  y-z plane');
         LoadLine('    #1  x-z plane');
         LoadLine('');
         LoadLine('    #2  contour plot');
         LoadLine('    #2  surface plot (Modify viewpoint? #F)');
         LoadLine('');
         LoadLine( ' Value range (arb. scale):  {      } to {      }');
         LoadLine('');
         LoadLine(
            ConCat(' Plot range:  rho_max: {      }',Potl.Lunit,'m  ',
                   'z_max: {      }',Potl.Lunit,'m'));
         LoadLine('');
         IF WF.RecordVeff THEN
            BEGIN
               LoadLine(ConCat(' V_eff range (',Potl.Eunit,
                                               'eV): {      } to {      }'));
               LoadLine('');
            END
         ELSE IF WF.Computed=Undistorted THEN
            BEGIN
               LoadLine( ' Grid spacings:  rho: {   }    z: {   }');
               LoadLine('');
            END
         ELSE IF (WF.Computed=Distorted) AND (WF.Mode=Auto) THEN
            BEGIN
               LoadLine(ConCat(' p_z range (e-',Potl.pzUnit,
                                                'm): {      } to {      }'));
               LoadLine('');
            END;
         LoadLine(       '   [  OK  ]    [Cancel]');
         SetRadioButton('1',WF.sPlane);
         sPlaneOld:=WF.sPlane;
         IF WF.Surf THEN SetRadioButton('2',2)
         ELSE SetRadioButton('2',1);
         SetNumber(7,WF.VertMin); SetNumber(8,WF.VertMax);
         SetNumber(9,Potl.L0*WF.PrhoMax); SetNumber(10,Potl.L0*WF.PzMax);
         IF WF.RecordVeff THEN
            BEGIN
               SetNumber(11,WF.VeffMin); SetNumber(12,WF.VeffMax);
            END
         ELSE IF WF.Computed=Undistorted THEN
            BEGIN
               SetNumber(11,Potl.NrhoSteps); SetNumber(12,Potl.NzSteps);
            END
         ELSE IF (WF.Computed=Distorted) AND (WF.Mode=Auto) THEN
            BEGIN
               SetNumber(11,WF.DipoleMin); SetNumber(12,WF.DipoleMax);
            END;
         AcceptScreen;
         IF NOT Canceled THEN WITH WF,Potl DO
            BEGIN
               sPlane:=GetRadioButton('1');
               IF GetRadioButton('2')=2 THEN
                  BEGIN
                     Surf:=True;
                     Modify:=GetBoolean(6);
                  END
               ELSE
                  BEGIN
                     Surf:=False;
                     Modify:=False;
                  END;
               VertMin:=GetNumber(7);
               IF VertMin>0.0 THEN
                  RealInputWarn('vertical min',VertMin,-1.0,6);
               VertMax:=GetNumber(8);
               IF VertMax<=VertMin THEN
                  RealInputWarn('vertical max',VertMax,VertMin+1.0,6);
               PrhoMax:=GetNumber(9);
               IF (PrhoMax<=L0*drho) THEN
                  RealInputWarn('rho_max',PrhoMax,L0*drho,6);
               PzMax:=GetNumber(10);
               IF (PzMax<=L0*dz) THEN
                  RealInputWarn('z_max',PzMax,L0*dz,6);
               PrhoMax:=PrhoMax/L0; PzMax:=PzMax/L0;
               IF RecordVeff THEN
                  BEGIN
                     VeffMin:=GetNumber(11);
                     VeffMax:=GetNumber(12);
                     IF VeffMax<=VeffMin THEN
                        RealInputWarn('V_eff max',VeffMax,VeffMin+10.0,6);
                  END
               ELSE IF WF.Computed=Undistorted THEN
                  BEGIN
                     NrhoStepsOld:=NrhoSteps;
                     NrhoSteps:=round(GetNumber(11));
                     IF NrhoSteps<1 THEN
                        IntInputWarn('no. of rho grid steps',
                                     NrhoSteps,NrhoStepsOld,3);
                     NzStepsOld:=NzSteps;
                     NzSteps:=round(GetNumber(12));
                     IF NzSteps<1 THEN
                        IntInputWarn('no. of z grid steps',
                                     NzSteps,NzStepsOld,3);
                     drho:=rhoMax/(NrhoSteps-0.5); dz:=zMax/NzSteps;
                     PrepStorage(WF,Potl,EnoughMemory);
                     IF NOT EnoughMemory THEN
                        BEGIN
                           Announce(ConCat('Memory capacity insufficient for',
                                           ' requested array size.'));
                           NrhoSteps:=NrhoStepsOld; NzSteps:=NzStepsOld;
                           drho:=rhoMax/(NrhoSteps-0.5); dz:=zMax/NzSteps;
                           PrepStorage(WF,Potl,Enough);
                           PrepPsiSlice(WF,Potl);
                        END;
                  END
               ELSE IF (Computed=Distorted) AND (Mode=Auto) THEN
                  BEGIN
                     DipoleMin:=GetNumber(11);
                     DipoleMax:=GetNumber(12);
                     IF DipoleMax<=DipoleMin THEN
                        RealInputWarn('p_z max',DipoleMax,DipoleMin+2.0,6);
                  END;
               IF EnoughMemory THEN
                  BEGIN
                     Plotted:=False;
                     CASE Computed OF
                        NotDone     :{no-op};
                        Undistorted :PrepUWFPlotScr(WF,Potl);
                        Distorted   :PrepDWFPlotScr(FileDesc,WF,Potl);
                        Shared      :PrepSWFPlotScr(FileDesc,WF,Potl);
                        Binding     :PrepBWFPlotScr(WF,Potl);
                        END; {case}
                     PrepPsiSlice(WF,Potl);
                     IF Modify THEN RotateView(WF,Potl)
                     ELSE PlotWF(WF,Potl,True);
                  END;
            END;
         Done;
      UNTIL EnoughMemory;
   {Dispose(InputScr); {Mac only}
END; {ModViewWF}

{-------Routines for construction of plot information screens---------}

PROCEDURE PrepUWFPlotScr(VAR WF :WFtype; VAR Potl :PotlType);
{Prepare Info screen for plot of undistorted wave function.}
VAR
   nn :integer;
BEGIN
   WITH WF,Potl DO
      BEGIN
         PlotInfoScr[1]:='     Undistorted';
         PlotInfoScr[2]:='    eigenfunction';
         PlotInfoScr[3]:='';
         PlotInfoScr[4]:=' Quantum numbers:';
         PlotInfoScr[5]:=ConCat('     n: ',NumStr(n,2,0));
         PlotInfoScr[6]:=ConCat('     l: ',NumStr(l,2,0));
         PlotInfoScr[7]:=ConCat('     m: ',NumStr(m,2,0));
         PlotInfoScr[8]:='';
         PlotInfoScr[9]:=ConCat(' Z: ',NumStr(Z1,5,2));
         PlotInfoScr[10]:=
            ConCat(' Energy: ',Num2Str(V0*En,6),' ',Eunit,'eV');
         PlotInfoScr[11]:=' Reduced mass: ';
         PlotInfoScr[12]:=
            ConCat('    ',Num2Str(mu,6),' ',Munit,'eV/c^2');
         PlotInfoScr[13]:='';
         PlotInfoScr[14]:=
            ConCat(' rho_max: ',Num2Str(L0*PrhoMax,6),' ',Lunit,'m');
         PlotInfoScr[15]:=
            ConCat('   z_max: ',Num2Str(L0*PzMax,6),' ',Lunit,'m');
         FOR nn:=16 TO 25 DO PlotInfoScr[nn]:='';
      END;
END; {PrepUWFPlotScr}

PROCEDURE PrepDWFPlotScr(VAR FileDesc :FileDescType;
                         VAR WF :WFtype; VAR Potl :PotlType);
{Prepare Info screen for plot of distorted wave function.}
VAR
   nn,line,len :integer;
BEGIN
   WITH WF,Potl DO
      BEGIN
         PlotInfoScr[1]:='Distorted eigenfunction';
         PlotInfoScr[2]:='';
         PlotInfoScr[3]:=ConCat(' E-field: ',Num2Str(Efield,6),' TV/m');
         PlotInfoScr[4]:=ConCat(' B-field: ',Num2Str(Bfield,6),' MT');
         PlotInfoScr[5]:=ConCat(' m: ',NumStr(m,2,0),'   Z: ',NumStr(Z1,5,2));
         PlotInfoScr[6]:=
            ConCat(' Energy: ',Num2Str(V0*En,6),' ',Eunit,'eV');
         PlotInfoScr[7]:=
            ConCat(' p_z: ',Num2Str(pz0*DipoleVal,6),' e-',pzUnit,'m');
         PlotInfoScr[8]:=' Reduced mass: ';
         PlotInfoScr[9]:=
            ConCat('   ',Num2Str(mu,6),' ',Munit,'eV/c^2');
         PlotInfoScr[10]:='';
         PlotInfoScr[11]:=
            ConCat(' rho_max: ',Num2Str(L0*PrhoMax,6),' ',Lunit,'m');
         PlotInfoScr[12]:=
            ConCat('   z_max: ',Num2Str(L0*PzMax,6),' ',Lunit,'m');
         PlotInfoScr[13]:='';
         line:=13;
         IF (Mode=Auto) AND (NautoRemain>0) THEN
            BEGIN
               line:=line+1;
               PlotInfoScr[line]:=
                  ConCat(' Steps remaining: ',NumStr(NautoRemain,3,0));
               IF WF.RecordToFile THEN
                  BEGIN
                     line:=line+1;
                     len:=Length(FileDesc.FileName);
                     WHILE (len>1) AND (FileDesc.FileName[len-1]<>'\') DO
                        len:=len-1;
                     PlotInfoScr[line]:=
                        ConCat(' Filename: ',Copy(FileDesc.FileName,len,12));
                  END;
            END;
         IF Algorithm=Lanczos THEN
            BEGIN
               line:=line+1;
               PlotInfoScr[line]:=
                  ConCat(' No. of iterations:',NumStr(NumIter,3,0));
               IF Cwarn THEN
                  BEGIN
                     line:=line+1;
                     PlotInfoScr[line]:='    Did not converge.';
                  END;
            END;
         FOR nn:=line+1 TO 25 DO PlotInfoScr[nn]:='';
      END;
END; {PrepDWFPlotScr}

PROCEDURE PrepSWFPlotScr(VAR FileDesc :FileDescType;
                         VAR WF :WFtype; VAR Potl :PotlType);
{Prepare Info screen for plot of shared wave function.}
VAR
   nn,line,len :integer;
BEGIN
   WITH WF,Potl DO
      BEGIN
         PlotInfoScr[1]:=' Shared eigenfunction';
         PlotInfoScr[2]:='';
         PlotInfoScr[3]:=ConCat(' 1st charge: ',Num2Str(Z1,4),' e');
         PlotInfoScr[4]:=ConCat(' 2nd charge: ',Num2Str(ExtCharge,4),' e');
         PlotInfoScr[5]:=ConCat(' & distance: ',Num2Str(L0*Separation,6),
                                ' ',Lunit,'m');
         PlotInfoScr[6]:=ConCat(' m: ',NumStr(m,2,0));
         PlotInfoScr[7]:=
            ConCat(' Energy: ',Num2Str(V0*En,6),' ',Eunit,'eV');
         IF Separation=0.0 THEN PlotInfoScr[8]:=' V_eff: infinity'
         ELSE PlotInfoScr[8]:=
                 ConCat(' V_eff: ',
                        Num2Str(V0*(En+2*ExtCharge/Separation-EnAsymp),6),
                        ' ',Eunit,'eV');
         PlotInfoScr[9]:=
            ConCat(' Mass: ',Num2Str(Mass,6),' ',Munit,'eV/c^2');
         PlotInfoScr[10]:='';
         PlotInfoScr[11]:=
            ConCat(' rho_max: ',Num2Str(L0*PrhoMax,6),' ',Lunit,'m');
         PlotInfoScr[12]:=
            ConCat('   z_max: ',Num2Str(L0*PzMax,6),' ',Lunit,'m');
         PlotInfoScr[13]:='';
         line:=13;
         IF (Mode=Auto) AND (NautoRemain>0) THEN
            BEGIN
               line:=line+1;
               PlotInfoScr[line]:=
                  ConCat(' Steps remaining: ',NumStr(NautoRemain,3,0));
               line:=line+1;
               PlotInfoScr[line]:=
                  ConCat(' Step size: ',Num2Str(L0*dSep,6),' ',Lunit,'m');
               IF WF.RecordToFile THEN
                  BEGIN
                     line:=line+1;
                     len:=Length(FileDesc.FileName);
                     WHILE (len>1) AND (FileDesc.FileName[len-1]<>'\') DO
                        len:=len-1;
                     PlotInfoScr[line]:=
                        ConCat(' Filename: ',Copy(FileDesc.FileName,len,12));
                  END;
            END;
         line:=line+1;
         PlotInfoScr[line]:=
            ConCat(' No. of iterations:',NumStr(NumIter,3,0));
         IF Cwarn THEN
            BEGIN
               line:=line+1;
               PlotInfoScr[line]:='    Did not converge.';
            END;
         FOR nn:=line+1 TO 25 DO PlotInfoScr[nn]:='';
      END;
END; {PrepSWFPlotScr}

PROCEDURE PrepBWFPlotScr(VAR WF :WFtype; VAR Potl :PotlType);
{Prepare Info screen for plot of molecular wave function.}
VAR
   nn :integer;
BEGIN
   WITH WF,Potl DO
      BEGIN
         PlotInfoScr[1]:=' Binding eigenfunction';
         PlotInfoScr[2]:='';
         PlotInfoScr[3]:=' Quantum numbers:';
         PlotInfoScr[4]:=ConCat('   n_r: ',NumStr(nr,2,0));
         PlotInfoScr[5]:=ConCat('     l: ',NumStr(l,2,0));
         PlotInfoScr[6]:=ConCat('     m: ',NumStr(m,2,0));
         PlotInfoScr[7]:='';
         PlotInfoScr[8]:=
            ConCat(' Energy: ',Num2Str(V0*En,6),' ',Eunit,'eV');
         PlotInfoScr[9]:=
            ConCat(' m_1: ',Num2Str(m1,6),' GeV/c^2');
         PlotInfoScr[10]:=
            ConCat(' m_2: ',Num2Str(m2,6),' GeV/c^2');
         PlotInfoScr[11]:=
            ConCat(' Z_1: ',NumStr(Z1,5,2),'   Z_2',NumStr(ExtCharge,5,2));
         PlotInfoScr[12]:='';
         PlotInfoScr[13]:=
            ConCat(' rho_max: ',Num2Str(L0*PrhoMax,6),' ',Lunit,'m');
         PlotInfoScr[14]:=
            ConCat('   z_max: ',Num2Str(L0*PzMax,6),' ',Lunit,'m');
         FOR nn:=15 TO 25 DO PlotInfoScr[nn]:='';
      END;
END; {PrepBWFPlotScr}

END. {HatmPlot}
