          (*************************************************)
          (*************************************************)
          (**                                             **)
          (** Consortium of Upper-level Physics Software  **)
          (**             ( CUPS Project )                **)
          (**     (c) 1994 by John Wiley & Sons           **)
          (** Compiled with Utilities Ver. 1.8 (95/08/08) **)
          (**                                             **)
          (*************************************************)
          (*************************************************)

          {**********************************************}
          {**********************************************}
          {**          Program RayTrace.Pas            **}
          {**    Ray tracing simulations in optics     **}
          {**        Written by Brian W James          **}
          {**      Last change: 17 January 1995        **}
          {**********************************************}
          {**********************************************}

PROGRAM RayTrace;
USES crt,Graph,DOS,CUPSMUPP,CUPS,CupsFunc,CupsProc,Cupsgui,CupsGrph,
     CUPSPars,LensMatS;

{$M 32768,0,655360} {memory control for Stack and Heap }

CONST
   lightvel = 3E+8;

TYPE
   Toption = (Nopt,FmOpt,MrOpt,FbOpt,RbOpt,LnOpt);
   Tobject = (None,Infinite,nearby);
   Tdata = RECORD
      Fiber : Array [0..7] of Real;{diam,n1,n2,length,pulse,crit,na,lambda}
      maxmode,mode : INTEGER;
      FiberDelay : Array [0..100] of REAL;
      ImpactDistance,ObjectDist,RadialDist,Center,H2Oindex : REAL;
      LensFile, FuncString : STRING;
      RToption : Toption;
      RTobject : Tobject;
      RTaction, NoSpotData, SpotViewport, ZoomIn, clrindex, second,
         LensLoaded : BOOLEAN;
      newa,lasta : array[1..4] of real;
      SpotCounter : INTEGER;
      spot : array [1..150] of rays; {storage for spot diagram}
      SpotPlane,OldPlane,sides,Angle : REAL;
      AberrData : array[0..5,1..20] of REAL;
   END;

   Fermat = object
      PROCEDURE SetUpFm(sXpos,sYpos,sindex1,sindex2,sxl,syl : real;
                NumFm : integer);
      PROCEDURE Done;
      PROCEDURE CalcFm;
      PROCEDURE ReCalcFm(VAR RTdata : Tdata);
      PROCEDURE RayCalcFm(VAR RTdata : Tdata);
      PROCEDURE DrawFm(aposn : real; Dcolor1,Dcolor2,Bcolor,Mcolor : word );
      PROCEDURE PlotFm(Pcolor1,Pcolor2,Scolor : word);
      PROCEDURE MoveRayFm(a : real; Mcolor : word);
      PROCEDURE ArrowFm;
      PROCEDURE TextFm;
      PROCEDURE Mirinit(dist,height, index : REAL; NumPt : INTEGER);
      PROCEDURE MirageFm(angle : real; Mircolor : word);
      PROCEDURE IndexVar(Indexnum : integer);

      PRIVATE
         index1,index2,Xposn,Yposn,i1,i2,nsini1,nsini2,xl,yl,dummy : real;
         midx,midy,lowx,lowy,hix,hiy :word;
         x,y1,y2,ysum,theta : DVector;
         NumFmPt : integer;
   END;

VAR
   RayMenu      : TMenu;
   HotKeys      : THotKeys;
   sliders      : TSliders;
   RTdata : Tdata;
   RayFm  : Fermat;
   IndexF : TParser;
   NumChar, mcolor : integer;
   ExitFlag : boolean;

PROCEDURE ShowInitialScreen;
var
   helpScr0    : HelpScrType;
BEGIN
   helpscr0[01]:= '                                                 ';
   helpscr0[02]:= '        RAY TRACING IN GEOMETRICAL OPTICS        ';
   helpscr0[03]:= '                                                 ';
   helpscr0[04]:= '                       by                        ';
   helpscr0[05]:= '                                                 ';
   helpscr0[06]:= '                Brian W James                    ';
   helpscr0[07]:= '   University of Salford, Salford, M5 4WT, UK    ';
   helpscr0[08]:= '                                                 ';
   helpscr0[09]:= '                 Version 1.0                     ';
   helpscr0[10]:= '              Copyright (c) 1995                 ';
   helpscr0[11]:= '                                                 ';
   helpscr0[12]:= '   FERMAT - Plots the notional path of a ray in  ';
   helpscr0[13]:= '            two materials between fixed points.  ';
   helpscr0[14]:= '   MIRAGE - Shows the variation of path of a ray ';
   helpscr0[15]:= '            through a region of varying index.   ';
   helpscr0[16]:= '   FIBER  - Shows the variation of pulse delay   ';
   helpscr0[17]:= '            as a function of fiber parameters.   ';
   helpscr0[18]:= '   RAINBOW - Shows the formation of a rainbow    ';
   helpscr0[19]:= '             due to dispersion in refraction.    ';
   helpscr0[20]:= '   LENSES - An investigation into images formed  ';
   helpscr0[21]:= '            by lenses and their aberrations.     ';
   helpscr0[22]:= '                                                 ';
   helpscr0[23]:= '   Press <enter> or click the mouse to continue. ';
   helpscr0[24]:= '            F10  will select the menu.           ';
   helpscr0[25]:='                                                 ';
   help(helpscr0);
END;

PROCEDURE DrawCircle (r : REAL);
VAR
    x1,y1,x2,y2,theta,step : REAL;
    i : INTEGER;
BEGIN
    x1:= r; y1:= 0.0;
    step:= pi/50;
    for i:= 1 to 100 do
    begin
        theta:= i * step;
        x2:= r * cos(theta);
        y2:= r * sin(theta);
        PlotLine(x1,y1,x2,y2);
        x1:= x2; y1:= y2;
    end;{of circle}
END;{DrawCircle}


PROCEDURE Rainbow(VAR RTdata : Tdata);
{Traces one ray incident at a distance b below the axis, through a raindrop.}
VAR r,theta,alpha,beta,phi : real;
    i,point,colour, NumBow : integer;
    n   :array[1..7] of real;
    col :array[1..7] of integer;
    x,y : array[1..4] of real;
    x1,y1,x2,y2,step,bid : real;
BEGIN
   bid:= RTdata.ImpactDistance;
   col[1]:=red; col[2]:=6; col[3]:=green; col[4]:=blue;
    col[5]:=magenta; col[6]:=red; col[7]:=white;
   r:=1.0;
   n[1]:=1.33;
      SelectViewport (11);
      SelectScale(11);
      SetColor(lightgray);
      alpha:= arcsin(bid/r);
      theta := pi + alpha;          {point 1}
      phi:= pi + 2 * alpha;
      x[1]:= r * cos(theta);
      y[1]:= r * sin(theta);
      PlotLine(-1.4, -bid, x[1],y[1]);{incident ray}
      PlotLine(-1.2, -bid+0.05, -1.1,y[1]);{arrow head}
      PlotLine(-1.2, -bid-0.05, -1.1,y[1]);{arrow head}
      x2:= x[1] + 2*cos(phi);
      y2:= y[1] + 2*sin(phi);
      SetColor(white);
      PlotLine(x[1],y[1],x2,y2);    {reflected ray}
      FOR colour:=1 TO 5 DO BEGIN
         n[colour]:=1.33+0.01*colour;
         SetColor(col[colour]);
         beta := arcsin(bid/r/n[colour]);
         IF RTdata.second THEN NumBow:=4 ELSE NumBow:=3;
         FOR i:= 2 TO NumBow DO BEGIN
            theta:= pi + alpha + (i-1)*(pi - 2* beta);
            phi  := theta + alpha;
            x[i] := r * cos(theta);  {Next point}
            y[i] := r * sin(theta);
            PlotLine(x[i-1],y[i-1],x[i],y[i]);
            x2:= x[i] + 1.8*cos(phi);
            y2:= y[i] + 1.8*sin(phi);
            PlotLine(x[i],y[i],x2,y2);  {Refracted ray}
         END;
      END;
   IF bid < 0 THEN SetColor(lightmagenta) ELSE SetColor(magenta);
   PutLabel(Bottom,'Primary Rainbow                     ');
   IF bid > 0 THEN SetColor(lightcyan) ELSE SetColor(cyan);
   IF RTdata.second THEN PutLabel(Bottom,'                   Secondary Rainbow');
      SelectViewport (12);
      SelectScale(12);
      SetColor(lightgray);
      PlotLine(0, -bid, 90, -bid);{incident ray}

END; { of Rainbow}

PROCEDURE RainbowGraph(VAR RTdata : Tdata);
{Draws graph of deviation angles for primary and secondary rainbows
 using refractive index n}
VAR i,j,numpts : integer;
    alpha,beta,step,dev3,dev4,b,old3,old4,oldb : real;
    bi,devfreq3,devfreq4 :array [0..180] of integer;
BEGIN
     numpts:= 100;
     step :=1/numpts;
     oldb:= 0;
     old3:= 0;
     old4:= pi;
     for i:= 1 to numpts do
     begin b:=(i) * step;           {calculate points}
           alpha := arcsin(b);
           beta  := arcsin(b/RTdata.H2Oindex);
           dev3 := pi - (pi + 2 * alpha - 4 * beta);
           dev4 := pi -(6 * beta - 2 * alpha);
           SetColor(lightmagenta);
           plotline(old3,oldb,dev3,b);          {primary rainbow}
           SetColor(lightcyan);
           IF RTdata.second THEN plotline(old4,- oldb,dev4,- b); {secondary rainbow}
           oldb:= b;
           old3:= dev3;
           old4:= dev4;
    end;
    FOR i:= 0 TO 180 DO BEGIN
       bi[i]:= i;
       devfreq3[i]:=0;
       devfreq4[i]:=0;
    END;
    step:=step/10;
    FOR i:= 1 TO 1000 DO BEGIN
       oldb:= i * step;           {calculate points}
       alpha := arcsin(oldb);
       beta  := arcsin(oldb/RTdata.H2Oindex);
       old3 := pi - (pi + 2 * alpha - 4 * beta);
       old4 := pi -(6 * beta - 2 * alpha);
       j:= TRUNC(old3/pi*180);
       devfreq3[j]:= devfreq3[j]+1;
       j:= TRUNC(old4/pi*180);
       devfreq4[j]:= devfreq4[j]+1;
    END;
    SelectViewPort(13);
    SelectScale(13);
    old3:= devfreq3[0]/10;
    old4:= devfreq4[0]/10;
    oldb:= 0;
    FOR i:= 1 TO 180 DO BEGIN
       SetColor(lightmagenta);
       plotline(oldb,old3,i,devfreq3[i]/10);
       old3:= devfreq3[i]/10;
       SetColor(lightcyan);
       IF RTdata.second THEN plotline(oldb,old4,i,devfreq4[i]/10);
       old4:= devfreq4[i]/10;
       oldb:= i;
    END;
END; {of RainbowGraph}


PROCEDURE InitRainBow(VAR RTdata : Tdata);
VAR
   side : REAL;
BEGIN
   side:= RTdata.sides;
   DefineViewPort (11, 0.06, 0.56, 0.28, 0.93);
   DefineViewPort (12, 0.62, 1.0, 0.37, 0.83);
   DefineViewPort (13, 0.62, 1.0, 0.11, 0.33);
   DefineScale (11,-side,side,-side,side);
   DefineScale (12,0,90,-1,1);
   DefineScale (13,0,90,0,10);
   SelectMuppetPort;
   sliders.create(11,-1.0,+1.0,0.7, 0.04,0.15,0.55,3, '-1.0','+1.0','Impact parameter',false);
   sliders.drawAll;
   OpenViewPort (11);
   SetColor( lightmagenta);
   PutLabel(Bottom,'Primary Rainbow                     ');
   SetColor(cyan);
   IF RTdata.second THEN PutLabel(Bottom,'                   Secondary Rainbow');
   OpenViewPort (13);
   SetColor(white);
   PutLabel(bottom,'^ Intensity distribution');
   PutLabel(bottom,'| Intensity distribution');
   Axis(0,0,30,5);
   OpenViewPort (12);
   PutLabel(top,'Elevation of Ray ->');
   SelectScale(12);
   Axis(0,-1.4,30,side/2);
   DefineScale (12,0,pi/2,-1,1);
   RainbowGraph(RTdata);
   SelectViewport (11);
   SelectScale(11);
   SetColor(white);
   PutLabel(inside,'Ray paths');
   DrawCircle(1.0);
   Axis(-1.4,-1.4,side/2,side/2);
   SetWriteMode (XorPut);

   RTdata.ImpactDistance:= - 0.7{sliders.value(11)};
   Rainbow(RTdata);
   SetWriteMode (CopyPut);
   RTdata.RTaction:= True;
END;

PROCEDURE Fermat.CalcFm;
VAR
   im : integer;
   xstep,xim,ysqd1,ysqd2,yl1,yl2 : real;

BEGIN
   ysqd1:= Yposn*Yposn;
   ysqd2:= sqr(yl-Yposn);
   xstep:=xl/(NumFmPt-1);
   FOR im:= 1 TO NumFmPt DO
   BEGIN
      xim:= xstep*(im-1);
      x.put(im, xim);
      yl1:= sqrt(xim*xim+ysqd1) * index1;
      yl2:= sqrt(sqr(xl-xim)+ysqd2) * index2;
      y1.put(im, yl1);
      y2.put(im, yl2);
      ysum.put(im, yl1 + yl2);
   END;

END;


PROCEDURE Fermat.SetUpFm(sXpos,sYpos,sindex1,sindex2,sxl,syl : real;
          NumFm : integer);
BEGIN
   {DVectors initialize}
   x.init(NumFm*2);
   y1.init(NumFm*2);
   y2.init(NumFm);
   ysum.init(NumFm);
   theta.init(NumFm*2);
   Xposn:= sXpos;
   Yposn:= sYpos;
   index1:= sindex1;
   index2:= sindex2;
   xl:= sxl;
   yl:= syl;
   NumFmPt:= NumFm;
   RayFm.CalcFm;
END;

PROCEDURE Fermat.Done;
BEGIN
   {DVectors free}
   x.free;
   y1.free;
   y2.free;
   ysum.free;
   theta.free;
END;

PROCEDURE Fermat.ReCalcFm(VAR RTdata : Tdata);
BEGIN
   Yposn:= RTdata.lasta[2];
   index1:=RTdata.lasta[3];
   index2:=RTdata.lasta[4];
   CalcFm;
END;

PROCEDURE Fermat.RayCalcFm(VAR RTdata : Tdata);
BEGIN
      i1:=arctan(RTdata.newa[1]/Yposn);
      nsini1:= index1*sin(i1);
   dummy:= nsini1/index2;
      RubOut(8,12,25,GraphBackColor);
   IF dummy >= 1.0 THEN BEGIN
      {Announce('Total Internal Reflection');}
      SelectViewPort (1);
      SetColor(White);
      GraphBackColor:= RED;
      RubOut(8,12,25,GraphBackColor);
      Print(8,12,'Total Internal Reflection');
      GraphBackColor:= 8;
      END
   ELSE BEGIN
      dummy:= arcsin(dummy);
      END;
END;

PROCEDURE Fermat.DrawFm(aposn : real;Dcolor1,Dcolor2,Bcolor,Mcolor : word);
VAR
   ydash : real;

BEGIN
   HideMouse ;
   SelectViewPort(1);
   SelectScale(1);
   SetColor(Dcolor1);                                      {extra}
   line(MapX(0),MapY(0),MapX(aposn),MapY(Yposn));
   SetLineStyle(3,0,Normwidth);
   SetColor(Dcolor2);
   line(MapX(aposn),MapY(Yposn),MapX(xl),MapY(yl));
   SetLineStyle(0,0,NormWidth);
   SetColor(Bcolor);
   line(MapX(0),MapY(Yposn),MapX(xl),MapY(Yposn));
   RayFm.MoveRayFm(aposn, Mcolor);
   xposn:=aposn;
   RayFm.TextFm;
   RayFm.ArrowFm;
   SetColor(green);
   dummy:= nsini1/index2;
   IF dummy >= 1.0 THEN line(MapX(aposn),MapY(Yposn),MapX(aposn*2),MapY(0))
   ELSE BEGIN
      dummy:= pi/2 - arcsin(dummy);
      ydash:= ((1 - aposn)*tan(dummy) + Yposn);
      IF ydash < 1 THEN line(MapX(aposn),MapY(Yposn),MapX(1),MapY(ydash))
      ELSE  line(MapX(aposn),MapY(Yposn),MapX(aposn+(1-yposn)/tan(dummy)),MapY(1));
   END;
   SetColor(white);
   ShowMouse;
END;

PROCEDURE Fermat.ArrowFm;
var
   xyint : integer;
BEGIN
   SelectViewPort(1);
   SelectScale(1);
   midx:= MapX(xposn/2);
   midy:= MapY(yposn/2);
   lowx:= MapX(xposn/2 - sin(i1-0.3)*0.05);
   xyint:= MapY(yposn/2 - cos(i1-0.3)*0.05);
   IF xyint < 0 THEN lowy:= 0 ELSE lowy:= xyint;
   xyint:= MapX(xposn/2 - sin(i1+0.3)*0.05);
   IF xyint < 0 THEN hix:= 0 ELSE hix:= xyint;
   hiy:= MapY(yposn/2 - cos(i1+0.3)*0.05);
   SetColor(cyan);
   line(lowx,lowy,midx,midy);
   line(hix,hiy,midx,midy);

END;

PROCEDURE Fermat.TextFm;
VAR
   tStr : string;
   ysuml : real;
BEGIN
   SetColor(white);
   SelectViewPort(4);
   IF (xposn>0.005) AND ((xl - xposn)>0.005) THEN BEGIN
      i1:=arctan(xposn/Yposn);
      i2:=arctan((xl-xposn)/(yl - Yposn));
      nsini1:= index1*sin(i1);
      nsini2:= index2*sin(i2);
      RubOut(1,1,25,GraphBackColor);
      tStr := NumStr(nsini1,1,3);
      Print(2,1,'n*sin(i) = '+tStr+' ');
      RubOut(1,2,25,GraphBackColor);
      tStr := NumStr(nsini2,1,3);
      Print(2,2,'n''*sin(i'') = '+tStr+'');
      RubOut(1,3,29,GraphBackColor);
      ysuml:= ysum.value(round(xposn*xl*100));
      tStr:= NumStr(ysuml,1,5);
      Print(2,3,'Total path = '+tStr+' meter');
   END
   ELSE BEGIN
      RubOut(1,1,25,GraphBackColor);
      RubOut(1,2,25,GraphBackColor);
      RubOut(1,3,30,GraphBackColor);
      PRINT(2,2,'Out of range');
   END;
END;

PROCEDURE Fermat.PlotFm(Pcolor1, Pcolor2, Scolor : word);
BEGIN
   SelectViewPort(2);
   SetColor(pcolor1);
   SelectScale(2);
   PlotDVectors(x, y1,1,numfmpt);
   SetColor(pcolor2);
   PlotDVectors(x,y2,1,NumFmPt);
   SelectScale(3);
   SelectViewPort(3);
   SetColor(scolor);
   PlotDVectors(x,ysum,1,NumFmPt);
END;

PROCEDURE Fermat.MoveRayFm(a : real; Mcolor : word);
BEGIN
   HideMouse ;
   SelectViewPort(1);
   SelectScale(1);
   SetColor(Mcolor);
   line(MapX(a),MapY(0),MapX(a),MapY(yl));
   SelectViewPort(2);
   SelectScale(2);
   line(MapX(a),MapY(0),MapX(a),MapY(4));
   SelectViewPort(3);
   SelectScale(3);
   line(MapX(a),MapY(0),MapX(a),MapY(4));
   ShowMouse;
END;

PROCEDURE Fermat.MirageFm(angle : real; Mircolor : word);
VAR
   xstep,ystep,step,t90,deltay,argument : real;
   im,id,count,r : integer;
   tstr : string;
   mirage_ray : boolean;

BEGIN
   mirage_ray:= true;
   t90:= pi/2;
   theta.put(1, t90 - angle*PI/180);  {convert to radian}
   x.put(1, 0.0);
   y1.put(1, 1.6){yl/2};  {set eye level}
   IF angle > 0.05 Then step:= xl/NumFmPt ELSE step:=xl/(NumFmpt*5);
   ystep:= yl/NumFmPt;
   im:= 1;
   y1.put(2, y1.value(1) - step*cos(theta.value(1)));
   WHILE ((IndexF.f(x.value(1),y1.value(im),0,0)*sin(theta.value(im))
          /IndexF.f(x.value(1),y1.value(im + 1),0,0)< 1.0) AND (im< NumFmPt)
           AND mirage_ray) DO BEGIN
      im:=im + 1;
      x.put(im, x.value(im - 1) + step*sin(theta.value(im - 1)));
      argument:=IndexF.f(x.value(1),y1.value(im - 1),0,0);
      theta.put(im, arcsin(argument*sin(theta.value(im - 1))
        /IndexF.f(x.value(1),y1.value(im),0,0)));
      y1.put(im + 1, y1.value(im) - step*cos(theta.value(im)));
      IF y1.value(im+1) < 0 THEN mirage_ray:= false;
   end;
   SelectViewPort(5);
   tstr:=NumStr(im,5,0);
   rubout(68,2,5,graphbackcolor);
   Print(68,2,tstr);
   {announce('before reflection');}
   deltay:= step*cos(theta.value(im))/20;
   y1.put(im+1, y1.value(im));
   count:= 0;
   REPEAT
      count:= count + 1;
      y1.put(im+1, y1.value(im+1) - deltay);
   UNTIL (IndexF.f(x.value(1),y1.value(im),0,0)*sin(theta.value(im))/
            IndexF.f(x.value(1),y1.value(im+1),0,0)> 1.0) OR (count > 19);
   IF y1.value(im+1) < deltay THEN mirage_ray:= false ELSE mirage_ray:= true;
   IF mirage_ray THEN BEGIN
      step:= xl/NumFmPt;
      im:= im + 1;
      y1.put(im + 1, y1.value(im - 1));
      deltay:= (y1.value(im)-y1.value(im-1))*tan(theta.value(im-1));
      x.put(im, x.value(im-1)-deltay);
      x.put(im+1, x.value(im)-deltay);
      count:=2;
      r:=im;
      repeat
         im:= r + count;
         y1.put(im, y1.value(r - count));
         x.put(im, x.value(r)*2 - x.value(r-count));
         count:=count + 1;
      until y1.value(im) >= 1.6;
      y1.put(im+1, y1.value(im) + step * cos(theta.value(1)));{initialise for y1[]>y'eye'}
      theta.put(im, theta.value(1));
      WHILE ((IndexF.f(x.value(1),y1.value(im),0,0)*sin(theta.value(im))
               /IndexF.f(x.value(1),y1.value(im + 1),0,0)< 1.0) AND (im< NumFmPt)
                AND (x.value(im)<500.0) AND (y1.value(im)<yl) AND mirage_ray) DO BEGIN
         im:=im + 1;
         x.put(im, x.value(im - 1) + step*sin(theta.value(im - 1)));
         argument:= IndexF.f(x.value(1),y1.value(im - 1),0,0);
         theta.put(im, arcsin(argument*sin(theta.value(im - 1))
            /IndexF.f(x.value(1),y1.value(im),0,0)));
         y1.put(im + 1, y1.value(im) + step*cos(theta.value(im)));
      END;
   END;
   SelectViewPort(5);
   IF mirage_ray THEN SetColor(Mircolor) ELSE BEGIN
      SetLineStyle(3,0,Normwidth);
      SetColor(lightgreen);
   END;
   SelectScale(5);
   PlotDVectors (x,y1,1,im);
   SetLineStyle(0,0,NormWidth);
   SetColor(Mircolor);
   tstr:=NumStr(im,5,0);
   rubout(68,2,5,graphbackcolor);
   Print(68,2,tstr);
END;

{*************************************************}

procedure DrawSpot(ExitPoint:rays;distance:real;spotcolor:integer);
{ draw points in image window for spot diagrams }
var
   yy,xx,centreX,centreY,width,u,v,del : real;
begin
   del := exitpoint.z - spos(last);
   with ExitPoint do
   begin
      yy := y + (distance - del)*m/n;
      xx := x + (distance - del)*l/n;
   end; {with}
   SetColor(spotcolor);
   PlotLine(xx,yy,xx,yy);
end; {drawSpot}

procedure arrows(d:real;colour:integer);
{ position arrows}
begin
     setcolor(colour);
{     drawArrow(0,-1.1,d,0.5,up);
     drawArrow(0,1.1,d,0.5,down);    }
end; { proc arrows}

procedure animate( VAR lz,z : REAL);
{ animate spots }
var
   i:integer;
   width : real;
begin
   width:= 0;
   FOR i:= 1 to last-1 DO width:= width + lens.t[i];
   SelectViewport (15);
   SelectScale(15);
   SetWriteMode (XorPut);
   SetColor(white);
   PlotLine(lz+width,1.0,lz+width,-1.0);
   PlotLine(z+width,1.0,z+width,-1.0);
   SetWriteMode (CopyPut);

   SelectViewport (16);
   SelectScale(16);
   for i := 1 to RTdata.spotCounter do
   begin
      drawSpot(RTdata.spot[i],lz,0);  {0 - spots on a black background}
      drawSpot(RTdata.spot[i],z,RTdata.spot[i].color);
   end;
   lz := z;
end; {animate}


procedure traceSpots(zobj,yobj : REAL);
{traces points for spot diagram, for object at (0,yobj,zobj) or at infinity
and puts the resulting exit rays into spot[]. }
var i,j,k : integer;
    phi,r,x,y,z,u,v,mag,yconst,zfactor : real ;
BEGIN
   k:=0;
   zfactor:= 1/zobj; {rays start 1 (cm) to left of first surface}
 {  IF RTdata.RTobject = OnAxis THEN yobj:= 0;  }
   {Special case of central spot}
   IF RTdata.RTobject = infinite THEN trace(0,0,-1,RTdata.Angle*pi/180,pi/2)
       ELSE trace(0,-zfactor*yobj,-1,ArcCos(-zobj/sqrt(yobj*yobj + zobj*zobj)),-pi/2);
   IF rayPath[0].failed = false THEN BEGIN
         k:=k+1;
         RTdata.spot[k]:= rayPath[last];
         RTdata.spot[k].color:= 15;
   END;
   yconst:= zfactor * yobj;
   zfactor:= zfactor + 1;       {initial zfactor value + 1 to speed calculation}
   FOR i:= 1 TO 6 DO BEGIN {6 rings of spots}
         r:= i/7;
         FOR j:=-10 TO 9 DO BEGIN  {20 spots per ring}
            phi:=j*(2*pi/20);
            x:= r*cos(phi); y:=r*sin(phi);
            mag:= sqrt(x*x + (y-yobj)*(y-yobj) + zobj*zobj);
            If RTdata.RTobject = infinite THEN trace(x,y,-1,RTdata.Angle*pi/180,pi/2)
            ELSE trace(zfactor*x,zfactor*y-yconst,-1,ArcCos(-zobj/mag),ArcTan2(x,y-yobj));
    {        If RTdata.RTobject = infinite THEN trace(x,y,-1,0,pi/2)
                    ELSE trace(zfactor*x,zfactor*y-yconst,-1,ArcCos(-zobj/mag),ArcTan2(x,y-yobj));
     }       IF rayPath[0].failed = false THEN BEGIN
               k:=k+1;
               RTdata.spot[k]:= rayPath[last];
               RTdata.spot[k].color:= 9+abs(i);
            END
         END {j}
    END {i};
   RTdata.SpotCounter:=k;
   RTdata.NoSpotData := false;
END;

PROCEDURE DrawRaysInit( NumRay : INTEGER);  { draw spot diagram }
var
   width : real;
   i : integer;
   tStr : string;
BEGIN
   SelectViewport (15);
   focus(lens.fl,lens.bfl,lens.gip);
   if RTdata.NoSpotData then traceSpots(RTdata.ObjectDist,RTdata.Radialdist);     {???????????????????}
   SelectViewport (15);
   SelectScale(15);
   DrawLens(white);
   tStr:= NumStr(lens.fl,6,3);
   Print(2,7,'Focal Length '+tStr+' ');
   tStr:= NumStr(lens.bfl,6,3);
   Print(24,7,'Back Focal Distance '+tStr+' ');
   tStr:= NumStr(lens.gip,6,3);
 {  Print(51,7,'Gaussian Image Pt '+tStr+' ');  }
   SelectViewport (16);
   SelectScale(16);
   tStr:= NumStr(RTdata.SpotCounter,6,1);
   Print(3,12,'Number of rays traced = '+tStr+' ');


   RTdata.OldPlane:=  RTdata.SpotPlane;

   for i := 1 to RTdata.spotCounter do
      drawSpot(RTdata.spot[i],RTdata.SpotPlane{10.0},RTdata.spot[i].color);
   SelectViewport (15);
   SelectScale(15);
   for i := 1 to RTdata.spotCounter do
      drawExitRay(RTdata.spot[i], lens.bfl*3.5,RTdata.spot[i].color{white});{length of ray drawn}
   width:= 0;
   FOR i:= 1 to last-1 DO width:= width + lens.t[i];
   width:= width + RTdata.SpotPlane;
   SetWriteMode (XorPut);
   SetColor(white);
   PlotLine(width,1.0,width,-1.0);
   SetWriteMode (CopyPut);
   Sliders.Done;
   SelectMuppetPort;
   OpenViewport(18);
   sliders.create(14,0,20,RTdata.SpotPlane, 0.02,0.56,0.45,3, '0','20.0','distance',false);
   sliders.drawAll;
   SelectViewport(16);
end; { spots }

{*************************************************}

PROCEDURE SetUpMyMenu;
BEGIN
   RayMenu.Init;
   WITH RayMenu DO
   BEGIN
      column(1, 'File');
       row(1,1, 'About Program');
       row(1,2, 'About CUPS');
       row(1,3, 'Configuration');
       row(1,4, '-------------');
       rowactivate(1,4,false);
       row(1,5, 'Load Lens ');
       rowactivate(1,5,false);
       row(1,6, 'Save Lens ');
       rowactivate(1,6,false);
       row(1,7, '-------------');
       rowactivate(1,7,false);
       row(1,8, 'Exit   ');
      column(2, 'Fermat');
       row(2,1, 'Fermat'+chr(39)+'s Principle and Refraction');
      column(3, 'Mirage');
       row(3,1, 'Ray Tracing in Inhomogeneous Medium  ');
      column(4, 'Fiber');
       row(4,1, 'Behaviour of Pulses in Long Fibers  ');
      column(5, 'Rainbow');
       row(5,1, 'Primary Rainbow  ');
       row(5,2, 'Primary and Secondary Rainbow');
      column(6, 'Lenses');
       row(6,1, ' Object at infinity');
       row(6,2, ' Object near lens ');
       row(6,3, ' Edit lens design');
       rowactivate(6,4,false);
   END;
   RayMenu.Display;
END;


PROCEDURE SetUpHotKeys;
BEGIN
   CASE RTdata.RToption OF
   FmOpt: WITH HotKeys DO BEGIN
             HotKeys.Init(2);
             key[1] := 'F1-Help';
             key[2] := 'F10-Menu';
          END;
   MrOpt: WITH HotKeys DO BEGIN
             HotKeys.Init(4);
             key[1] := 'F1-Help';
             key[2] := 'F5-Index Var';
             key[3] := 'F6-Clear Scr';
             key[4] := 'F10-Menu';
          END;
   FbOpt: WITH HotKeys DO BEGIN
             HotKeys.Init(2);
             key[1] := 'F1-Help';
             key[2] := 'F10-Menu';
          END;
   RbOpt: WITH HotKeys DO BEGIN
             HotKeys.Init(2);
             key[1] := 'F1-Help';
             key[2] := 'F10-Menu';
          END;
   LnOpt: WITH HotKeys DO BEGIN
             HotKeys.Init(6);
             key[1] := 'F1-Help';
             key[2] := 'F4-Center';
             active[2] := RTdata.ZoomIn;
             IF RTdata.ZoomIn THEN key[3] := 'F5-Zoom-out'
                ELSE key[3] := 'F5-Zoom-in';
             key[4] := 'F6-Seidel';
             key[5] := 'F8-Object';
             key[6] := 'F10-Menu';
          END;
   END; {case}
   HotKeys.Display;
END;

PROCEDURE NewOption;
BEGIN
   sliders.Done;
   ClearMUPPETport;
   RayMenu.Display;
   SetUpHotKeys;
   HotKeys.Display;
END;

PROCEDURE FInput(VAR fString : string; fvalid : boolean );
VAR
   RadioChoice : integer;
   FScreen : TInputscreen;
   MyParserHelp : HelpScrType;
   n : byte;
BEGIN
   with FScreen do
   BEGIN
      init;
      AboutParser  (MyParserHelp);
      SetHelpScreen(MyParserHelp);
      DefineInputPort (0.1, 0.9, 0.42, 0.90);
      LoadLine('Choose the y (height) dependence of the refractive index from ');
      LoadLine('the given functions, or by editing the expression in the box. ');
      LoadLine('  ');
      LoadLine('  #A  refractive index n = "                                " ');
      LoadLine('  #A  refractive index n = 1.0003 - exp(-y) * 0.0001');
      LoadLine('  #A  refractive index n = 1.0003 - 0.00001/y');
      LoadLine('  #A  refractive index n = 1.0003 - 0.0001/((y+1)^2)');
      LoadLine('  #A  refractive index n = 1.0002 + y * 0.000025');
      IF RTdata.clrindex THEN LoadLine('  #T  Clear refractive index graph.')
      ELSE LoadLine('  #F  Clear refractive index graph.');
      LoadLine('  ');
      LoadLine('          [  Ok  ]     [Cancel]     [ Help ]');
      SetString(2,fstring);
      repeat
         AcceptScreen;
         fString:= '1.0003 - exp(-y) * 0.0001';
         mcolor:= lightcyan;
         IF Canceled THEN Announce('The default dependence has been implemented.')
         ELSE BEGIN
            RadioChoice := GetRadioButton('A');
            CASE RadioChoice OF
             1: BEGIN fString:= GetString(2); mcolor:= lightred;  END;
             2: BEGIN fString:= '1.0003 - exp(-y) * 0.0001'; mcolor:= lightcyan; END;
             3: BEGIN fString:= '1.0003 - 0.00001/y'; mcolor:= lightmagenta; END;
             4: BEGIN fString:= '1.0003 - 0.0001/((y+1)^2)'; mcolor:= white; END;
             5: BEGIN fString:= '1.0002 + y * 0.000025'; mcolor:= lightblue; END;
            END; {CASE}
            RTdata.clrindex:= GetBoolean(7);
         END;
         fvalid := IndexF.parse('x','y',' ',' ',fString);
         if not fvalid then
            announce('Syntax Error in Function Equation - Please Correct');
      until fvalid;
      IF IndexF.f(0,0,0,0) > IndexF.f(0,4,0,0) THEN BEGIN
         Announce(' Refractive index MUST increase with height - default function used. ' );
         fString:= '1.0003 - exp(-y) * 0.0001';
         fvalid := IndexF.parse('x','y',' ',' ',fString);
      END;
      IF RTdata.clrindex THEN BEGIN
         SelectScale(6);
         OpenViewPort (6);
         Axis(0.0,0.0,0.0001,1);
         SetColor(cyan);
         PutLabel(inside,'Refractive Index         ');
      END;
      SelectViewport(7);
      Rubout(2,5,35,GraphBackColor);
      SetColor(mcolor);
      Print(2,2,'Mirage rays shown in this color');
      Print(2,5,'n = ' + fstring);
      done;
   END;
END;

FUNCTION DataInput (VAR LensDfile : string) : string;
VAR
   DataInScreen : TInputscreen;
   n : byte;
   MyFile : string;
BEGIN
   MyFile:= '*.LEN';
   MyFile:= OpenFile(MyFile);
   BEGIN
      with DataInScreen do
      BEGIN
         init;
         DefineInputPort (0.12, 0.88, 0.39, 0.61);
         LoadLine('      ');
         LoadLine('      Give the full name of the data file  ');
         LoadLine('      from which the data is to be read.  ');
         LoadLine('filename = "                                         "');
         LoadLine('   [  Ok  ]    [Cancel]              ');
         SetString(1,MyFile);
         AcceptScreen;
         DataInput:= LensDfile;
         IF Canceled OR (GetString(1) = '') THEN
            Announce('The old data has been kept.')
         ELSE DataInput:= GetString(1);
         done;
      END;
   END;
END;

FUNCTION DataOutput (VAR LensSfile : string) : boolean;
VAR
   DataOutScreen : TInputscreen;
   n : byte;
BEGIN
   with DataOutScreen do
   BEGIN
      init;
      DefineInputPort (0.12, 0.88, 0.39, 0.61);
      LoadLine('     ');
      LoadLine('      Give the full name of the data file  ');
      LoadLine('      in which the data is to be stored.  ');
      LoadLine('filename = "                                         "');
      LoadLine('   [  Ok  ]     [Cancel]              ');
      AcceptScreen;
      IF Canceled OR (GetString(1) = '') THEN DataOutput:= false
      ELSE BEGIN
         LensSfile:= GetString(1);
         DataOutput:= true;
      END;
      done;
   END;
END;

PROCEDURE DrawPulse(position,height,width : REAL; Pcolor : integer);

var rhsx : real;

BEGIN
   rhsx:= position + width;
   SetColor(Pcolor);
   PlotLine(position,0.0,position,height);
   PlotLine(position,height,rhsx,height);
   PlotLine(rhsx,height,rhsx,0.0);
END;


PROCEDURE FiberCalc(var RTdata : Tdata);

var
   lhsdelay,rhsdelay,thetamax,theta,phi,oldth,pulsedelay,indexratiosqd : REAL;
   Lhsgraph,Rhsgraph,modenum,i : INTEGER;
   tstr,tstr2 : STRING;
   lastmode : BOOLEAN;

BEGIN
   lastmode:= false;
   with RTdata DO BEGIN
   lhsdelay:= FiberDelay[1]; {store old delays to use to delete scale}
   rhsdelay:= FiberDelay[maxmode];
   Fiber[5]{crit}:= arcsin(Fiber[2]{n2}/Fiber[1]{n1});
   Fiber[6]{na}:=  Fiber[1]*cos(Fiber[5]);
   indexratiosqd:=(Fiber[2]*Fiber[2]{n2*n2}/Fiber[1]/Fiber[1]{n1/n1});
   modenum:= 1;
   theta:= pi/2;
   REPEAT  {for modes}
      i:= 1;
      REPEAT  {to find theta for n th mode}
         phi:=sin(theta)*sin(theta) - indexratiosqd;
         IF phi < 0 THEN lastmode:= true
         ELSE BEGIN
            oldth:= theta;
            phi:= arctan(sqrt(phi)/cos(theta));
            theta:= arccos((pi*modenum + phi)*Fiber[7]{lambda}/(2*pi*Fiber[1]{n1}*Fiber[0]{diam}));
            i:=i+1;
          END;
      UNTIL (abs(theta - oldth) < 0.0000001) OR lastmode;
      IF NOT(lastmode) and (modenum<=100) THEN BEGIN
         pulsedelay:=Fiber[1]*fiber[3]{length}/lightvel/sin(theta)*1E+6; {microsec}
         FiberDelay[modenum]:= pulsedelay;
         modenum:=modenum + 1;
         phi:=sin(theta)*sin(theta) - indexratiosqd;  {for next mode}
      END;
      IF modenum >99 THEN announce('More than 100 modes - only 100 modes are displayed.');
   UNTIL lastmode or (modenum > 99);
   IF modenum = 1 THEN Announce('Old data retained, no propogating mode for given conditions.')
   ELSE BEGIN
      tstr:=NumStr(modenum-1,3,0);
      tstr2:= NumStr(Fiber[6],9,3);
      Selectviewport(8);
      Lhsgraph:= trunc(lhsdelay)-1;
      Rhsgraph:= trunc(rhsdelay)+2;
      SelectScale(8);
      DrawAxis(Lhsgraph,-0.2,0,0.4,black);
      Openviewport(8);
      Lhsgraph:= trunc(FiberDelay[1])-1;
      Rhsgraph:= trunc(FiberDelay[modenum-1])+2;
      DefineScale (8,Lhsgraph,Rhsgraph,-0.2,1.2);
      SelectScale(8);
      SetColor(white);
      PRINT(2,9,'Maximum mode number = '+tstr+'  Numerical aperture '+tstr2);
      FOR i:=1 TO modenum-1  DO DrawPulse(FiberDelay[i],1.0{height},Fiber[4]*1E+6{width},white);
      maxmode:= modenum-1;
      DrawAxis(Lhsgraph,-0.2,0,0.4,white);
      Print(22,1,'Pulse Delay (in microsecond)');
      END;
   END;
END;

PROCEDURE FiberSetup(var RTdata : Tdata);
BEGIN
   SelectMuppetPort;
   sliders.create(6,1.0,90.0,RTdata.Fiber[0]*1E6,  0.5,0.46,1.0,3, '1.0','90.0','Fiber Core Diameter um',false);
   sliders.create(7,1.0,2.0,RTdata.Fiber[1], 0.5,0.37,1.0,4, '1.0','2.0','Fiber Index',false);
   sliders.create(8,1.0,2.0,RTdata.Fiber[2], 0.5,0.28,1.0,4, '1.0','2.0','Cladding Index',false);
   sliders.create(9,1.0,90.0,RTdata.Fiber[3]*1E-3, 0.5,0.19,1.0,3, '1.0','90.0','Fiber Length km',false);
   sliders.create(10,1.0,90.0,RTdata.Fiber[4]*1E+9, 0.5,0.10,1.0,3, '1.0','90.0','Input Pulse Width nS',false);
   sliders.drawAll;
   OpenViewPort (8);
   OpenViewPort (9);
   SetColor(white);
   Print(4,1,'Pulse Width (nano second)');
   SelectScale(9);
   Axis(-20,-0.2,20,0.4);
   DrawPulse(0.0{position},1.0{height},RTdata.Fiber[4]*1E+9{width},white{color});
   SelectViewport(8);
   SelectScale(8);
   FiberCalc(RTdata);

END;

PROCEDURE FiberUpdate(var RTdata : Tdata);

BEGIN
   SelectViewport(8);
   SelectScale(8);
   FiberCalc(RTdata);
END;

PROCEDURE HandleSliders(VAR RTdata : Tdata);
VAR
   amir : real;
   is : word;
begin
   CASE RTdata.RToption OF
   FmOPT : BEGIN
              SetWriteMode (XorPut);
              FOR is:=1 TO 4 DO RTdata.newa[is]:=sliders.value(is);
              IF RTdata.lasta[1] <> RTdata.newa[1] THEN
              BEGIN
                 RayFm.DrawFm(RTdata.lasta[1],cyan,brown,lightgray,blue);
            RayFm.RayCalcFm(RTdata);
                 RayFm.DrawFm(RTdata.newa[1],cyan,brown,lightgray,blue);
                 RTdata.lasta[1]:=RTdata.newa[1];
              END
              ELSE BEGIN
                 RTdata.newa[2]:= 1.0 - RTdata.newa[2];
                 FOR is:=1 TO 4 DO RTdata.lasta[is]:=RTdata.newa[is];
                 RayFm.MoveRayFm(RTdata.lasta[1], blue);
                 RayFm.PlotFm(cyan,brown,lightgray);
                 RayFm.DrawFm(RTdata.lasta[1],cyan,brown,lightgray,blue);
                 RayFm.ReCalcFm(RTdata);
            RayFm.RayCalcFm(RTdata);
                 RayFm.DrawFm(RTdata.lasta[1],cyan,brown,lightgray,blue);
                 RayFm.PlotFm(cyan,brown,lightgray);
                 RayFm.MoveRayFm(RTdata.lasta[1], blue);
              END;
              SetWriteMode (CopyPut);
            END;
   MrOpt:  BEGIN
               amir:= sliders.value(5);
               RayFm.MirageFm(amir, mcolor);
            END;
   FbOpt :  BEGIN
               SelectViewport(9);
               SelectScale(9);
               DrawPulse(0.0{position},1.0{height},RTdata.Fiber[4]*1E+9{width},GraphBackcolor);
               RTdata.Fiber[0]:=sliders.value(6)*1E-6;
               RTdata.Fiber[1]:=sliders.value(7);
               RTdata.Fiber[2]:=sliders.value(8);
               RTdata.Fiber[3]:=sliders.value(9)*1E+3;
               RTdata.Fiber[4]:=sliders.value(10)*1E-9;
               DrawPulse(0.0{position},1.0{height},RTdata.Fiber[4]*1E+9{width},white);
               IF RTdata.Fiber[1] <= RTdata.Fiber[2] THEN
               BEGIN
                  IF sliders.LastAltered=7 then
                       sliders.reset(7,1.03*RTdata.Fiber[2])
                  ELSE sliders.reset(8,0.97*RTdata.Fiber[1]);
                  RTdata.Fiber[1]:=sliders.value(7);
                  RTdata.Fiber[2]:=sliders.value(8);
                  Announce('Core index must be greater than cladding index');
               END
               ELSE FiberUpdate (RTdata);
            END;
   RbOpt :  BEGIN
   SetWriteMode (XorPut);
               Rainbow(RTdata);
               RTdata.ImpactDistance:= - sliders.value(11);
               Rainbow(RTdata);
   SetWriteMode (CopyPut);
            END;
   LnOpt :  BEGIN
               IF NOT RTdata.SpotViewport THEN
                  Announce('Use F5 to select the spot diagram.')
               ELSE BEGIN
                  RTdata.SpotPlane:= sliders.value(14);
                  Animate(RTdata.OldPlane,RTdata.SpotPlane);
                  RTdata.OldPlane:= RTdata.SpotPlane;
               END;
            END;
   END; {case }
Delay(20);
END;

PROCEDURE Fermat.IndexVar(Indexnum : integer);
VAR
   iy : integer;
   fstring : string;
BEGIN
   repeat
      HaltIfError := false;
      ErrorFound  := false;
      iy := 1;
      while (iy<=40) and (not ErrorFound) DO BEGIN
        y1.put(iy, (yl/40)*iy);
        x.put(iy, IndexF.f(0{x},y1.value(iy),0,0));
        inc(iy);
      END;
      if ErrorFound then begin
        Announce(' Parser Error - default function used. ' );
        fString:= '1.0003 - exp(-y) * 0.0001';
        IndexF.parse('x','y',' ',' ',fString);
      end;
   until not ErrorFound;
   HaltIfError := true;
   ErrorFound  := false;
   SelectViewPort(6);
   SelectScale(6);
   SetColor(indexnum);
   PlotDVectors (x,y1,1,40);
END;

PROCEDURE Fermat.Mirinit(dist,height, index : REAL; NumPt : INTEGER);
BEGIN
   xl:= dist;
   yl:= height;
   index1:= index;
   NumFmPt:= NumPt;
   DefineViewPort (5, 0.07, 0.99, 0.42, 0.94);
   DefineScale (5,0.0,xl,0.0,yl); {set origin at road level}
   OpenViewPort (5);
   DefineViewPort (6, 0.52, 1.0,0.08,0.38);
   DefineScale (6,1.0001,1.0004,0,4);
   OpenViewPort (6);
   Axis(0.0,0.0,0.0001,1);
   SetColor(lightcyan);
   PutLabel(inside,'Refractive Index         ');
{  PutLabel(left,'Height m');  }
   RayFm.IndexVar(mcolor{num});
   DefineViewPort (7, 0.0, 0.46,0.08,0.29);
   OpenViewPort (7);
   SetColor(white);
   Print(2,1,'The slider sets initial ray angle');
   SetColor(lightcyan);
   Print(2,2,'Mirage rays shown in this color');
   SetColor(lightgreen);
   Print(2,3,'Non-mirage rays dotted in green');
   SetColor(white);
   Print(2,4,'Index variation with height ->');
   SetColor(lightcyan);
   Print(2,5,'n = 1.0003 - exp(-y) * 0.0001');
   SelectViewport (5);
   SelectScale(5);
   SetColor(white);
   Print(48,2,'Number of segements');
   PutLabel(inside,'Ray path');
   PutLabel(bottom,'Distance (m)                          ');
   PutLabel(left,'Height (meter)');
   Axis(0.0,0.0,xl/10,yl/5);
   SelectMuppetPort;
   sliders.create(5,0.1,7.5,0.66, 0.0,0.325,0.45,2, '0.1','7.5','angle in degree',false);
   sliders.drawAll;
   RayFm.MirageFm(0.66, lightcyan);
   RTdata.RTaction:= True;
end;

PROCEDURE LensInit( NumRay : INTEGER);
BEGIN
   DefineViewPort (14, 0.0, 0.465, 0.08, 0.52);
   DefineScale (14,-0.5,3.5,-2.0,2.0);
   OpenViewPort (14);
   DefineViewPort (15, 0.0, 1.0,0.64,0.94);
   DefineScale (15,-0.5,25,-2.0,2.0);
   DefineViewPort (16, 0.55, 1.0,0.1,0.61);
{   DefineViewPort (17, 0.47, 1.0,0.06,0.62);  }
   DefineScale (16,-2,2,-2,2);
         DefineScale (17,-0.005,0.005,-2.0,2.0);
   DefineViewPort (18, 0.0, 0.465,0.53,0.61);
   OpenViewPort (18);
   OpenViewPort (15);
   SetColor(white);
   CASE RTdata.RTobject OF
      infinite : PutLabel(inside,'Ray Diagram - object at infinity');
      nearby   : PutLabel(inside,'Ray Diagram - object near lens');
   END; {case of RTobject}
   GraphBackColor:=0;
 {  OpenViewPort (17); }
   SetColor (black);
 {  WITH Views[ViewPortNumber] DO rectangle (0,0,Vx2-Vx1,Vy2-Vy1);
                     DefineScale (16,-2,2,-2,2);  }
   OpenViewPort (16);
   GraphBackColor:=8;
   SelectViewport (16);
   SelectScale(16);
   DrawAxis(-2.0,-2.0,0.5,0.5,white);
   SetColor (white);
   PutLabel(inside,'Spot Diagram');
   SelectViewport (14);
   PutLabel(inside,'Lens Design ');
   Print(1,10,'Data file:- '+RTdata.LensFile+'      ');
   SelectViewport (14);
   SelectScale (14);
   DrawLens(white);
end;


PROCEDURE LensInput;
VAR
   LensScreen  : TInputScreen;
   i : integer;
   tstr : string;
BEGIN
   with LensScreen DO BEGIN
      init;
      DefineInputPort(0.14,0.86,0.05,0.75);
      LoadLine('Lens data:    ');
      LoadLine('Number of refracting surfaces and stops {   } (2 to 10)');
      LoadLine('Surface  Radius of   Axial       Refractive  Stop     ');
      LoadLine('number   curvature   distance    index       radius   ');
      FOR i:= 1 TO 10 DO BEGIN
         tstr:= NumStr(i,2,0);
         LoadLine('  '+tstr+'     {        }  {        }  {        }  {        }');
      END;
      LoadLine('');
      LoadLine('                   [  Ok  ]  [Cancel]  ');
      SetNumber(1,last);
      SetNumberLimits(1,2,10);
      FOR i:= 1 TO last DO
      BEGIN
         SetNumber(i*4-2,lens.c[i]);
         SetNumber(i*4-1,lens.t[i]);
         SetNumber(i*4,lens.n[i]);
         SetNumber(i*4+1,lens.r[i]);
      END;
      FOR I:= 1 TO 10 DO BEGIN
         SetNumberLimits(i * 4 - 1,0.0,5.0);
         SetNumberLimits(i * 4,1.0,2.0);
      END;
      AcceptScreen;
      IF Canceled THEN Announce('The old values have been retained.')
      ELSE BEGIN
      last:= round(GetNumber(1));
      FOR i:= 1 TO last DO
      BEGIN
         lens.c[i] := GetNumber(i*4-2);
         lens.t[i] := GetNumber(i*4-1);
         lens.n[i] := GetNumber(i*4);
         lens.r[i] := GetNumber(i*4+1);
      END;
      END;
   done;
   END;
END;

PROCEDURE SeidelChoice(VAR NumSeidel : INTEGER);
VAR
   RadioChoice : Integer;
   SeidelScreen : TInputScreen;

BEGIN
   WITH SeidelScreen DO BEGIN
      Init;
      DefineInputPort(0.6,0.95,0.15,0.58);
      LoadLine(' Select Seidel Aberration  ');
      LoadLine(' to be shown ');
      LoadLine('');
      LoadLine('   #A - Spherical');
      LoadLine('   #A - Coma');
      LoadLine('   #A - Astigmatism');
      LoadLine('   #A - Petzval');
      LoadLine('   #A - Distortion');
      LoadLine('');
      LoadLine('    [  Ok  ]   [Cancel] ');   {[ Help ] [ CUPS ]}
         {SetHelpFile('RayTrace.HLP','General Help');}
      AcceptScreen;
         { IF GetBoolean(7)
               THEN aboutCUPS
               ELSE }
      NumSeidel:= 0;
      IF NOT Canceled THEN BEGIN
         RadioChoice := GetRadioButton('A');
         CASE RadioChoice OF
            1: NumSeidel:= 1;
            2: NumSeidel:= 2;
            3: NumSeidel:= 3;
            4: NumSeidel:= 4;
            5: NumSeidel:= 5;
         END; {CASE}
      END;
      done;
   END;
END;

PROCEDURE ObjectChoice(VAR RTdata : Tdata; VAR NewObject : BOOLEAN);
VAR
   ObjectScreen : TInputScreen;

BEGIN
   WITH ObjectScreen DO BEGIN
         Init;
         DefineInputPort(0.6,0.95,0.15,0.58);
   REPEAT
         LoadLine(' Select Object Distance  ');
         LoadLine('  and position  (in cm)');
         LoadLine('');
         LoadLine(' Axial distance of ');
         LoadLine(' object from lens {     } ');
         LoadLine('');
         LoadLine(' Radial distance of ');
         LoadLine(' object from axis {     } ');
         LoadLine('');
         LoadLine('    [  Ok  ]   [Cancel] ');
         IF 4*lens.fl < RTdata.ObjectDist THEN SetNumber(1, 4* lens.fl)
         ELSE SetNumber(1, RTdata.ObjectDist);
         SetNumberLimits(1,- 10000,- 1 * lens.fl);
         SetNumber(2,RTdata.Radialdist);
         SetNumberLimits(2,0.0,10000);
         AcceptScreen;
         IF Canceled THEN BEGIN
               Announce('The old values have been retained.');
               NewObject:= false;
            END
         ELSE BEGIN
            NewObject:= true;
            RTdata.ObjectDist:= (GetNumber(1));
            RTdata.Radialdist:= (GetNumber(2));
         END;
         IF abs(RTdata.Radialdist/RTdata.ObjectDist) >= 1.0 THEN
            Announce(' Object more than 45 degree off-axis ');
      UNTIL (RTdata.Radialdist/RTdata.ObjectDist) < 1.0 ;
         done;
      END;
END;

PROCEDURE AngleChoice(VAR RTdata : Tdata; VAR NewAngle : BOOLEAN);
VAR
   AngleScreen : TInputScreen;

BEGIN
   WITH AngleScreen DO BEGIN
         Init;
         DefineInputPort(0.6,0.95,0.15,0.58);
         LoadLine(' Select Object Direction  ');
         LoadLine('');
         LoadLine(' Angle of object rays to');
         LoadLine(' the axis {     } < 45 degree');
         LoadLine('');
         LoadLine('');
         LoadLine('    [  Ok  ]   [Cancel] ');
         SetNumber(1, RTdata.Angle);
      SetNumberLimits(1,0,45);
         AcceptScreen;
         IF Canceled THEN BEGIN
            NewAngle:= false;
            Announce('The old values have been retained.');
         END
         ELSE BEGIN
            NewAngle:= true;
            RTdata.Angle:= (GetNumber(1));
         END;
         done;
      END;
END;

PROCEDURE GetAberrData (VAR RTdata : Tdata);
{ gets data for aberration graphs }
var
	i,n,s:integer;	{ loop counters }
	seidel:Vector;
begin
	if NoAberrData then
	begin
		n := 0;
     	for i:= 9 downto -9 do
		begin
			n := n + 1;
	          getSeidel(i/10,seidel);  { values for x axis }
			for s := 1 to 5 do
			begin
			   RTdata.AberrData[s,n] := seidel[s];
			end;
			RTdata.AberrData[0,n]:= i/10; { values for y axis }
	end;
	AberrCount := n;
	NoAberrData := false;  { set global variable to prevent
					  unecessary re-calculation }
	end; {if NoAberrData }
end; { GetAberrData }


PROCEDURE DrawSeidel(VAR RTdata : Tdata; DSn : integer);
{ draws graph of given aberration }
VAR
   DSi : integer;
   x,y : DataVector;
BEGIN
	getAberrData(RTdata);	{ figues for graphs }
        SelectViewport(16);
        SelectScale(17);
	SetColor(white);
	for DSi:=1 to 19 do
 	BEGIN
	   x[DSi] := RTdata.AberrData[DSn,DSi];
	   y[DSi] := RTdata.AberrData[0,DSi];
 	END; {for}
        PlotData(x,y,19);
        CASE DSn OF
           1 : PutLabel(inside,'Spherical aberration');
           2 : PutLabel(inside,'Coma');
           3 : PutLabel(inside,'Astigmatism');
           4 : PutLabel(inside,'Petzval aberration');
           5 : PutLabel(inside,'Distortion');
        END;{case}
END; { drawSeidel }

PROCEDURE HandleMenu(VAR RTdata : Tdata);
VAR n : byte;
    c : TContour;
    temp : real;
    min,max : point3D;
    found,ParsedOK : boolean;
    lensfile : string;
BEGIN
   RayMenu.Display;
   WITH RayMenu DO BEGIN
      IF (colChosen in [2..5]) OR ((colChosen = 1) AND (RTdata.RToption <> LnOpt ))
      THEN BEGIN
         rowactivate(1,5,false);
         rowactivate(1,6,false);
      END;
      CASE colChosen OF
         1 : CASE rowChosen OF
              1 : BEGIN
                 {    Datarecord.mode:= dataIO;
                     SetUpHotKeys(HotKeys,Datarecord); }
                     ShowHelp('RAYTRACE.HLP','Help 1' );
                  END;
              2 : BEGIN
                  {   Datarecord.mode:= dataIO;
                     SetUpHotKeys(HotKeys,Datarecord);   }
                     aboutCUPS;
                  END;
              3 : Configuration;
              5 : BEGIN {Load lens data}
                     IF RTdata.RToption = LnOpt THEN BEGIN
                        LensFile:= RTdata.LensFile;
                        LensFile:= DataInput(LensFile);
                        IF LensFile <> RTdata.LensFile THEN BEGIN
                           RTdata.LensFile:= Lensfile;
                           InitTrace; { initialize ray trace routines }
                           TraceMode := finite;
                           lens.scale:=1;{scale lens}
                           lens.y:=0.0;
                           found:= false;
                           LoadLens(found,Lensfile);
                           NoAberrData:= true;
                           LensInit( 100);
                           RTdata.NoSpotData:= true;
                           DrawRaysInit(100);
                        END;
                     END
                     ELSE Announce('Lens option not selected - Press F10 to continue ');
                  END;
              6 : BEGIN {Save lens data}
                     IF RTdata.RToption = LnOpt THEN BEGIN
                        IF DataOutput(LensFile) THEN saveLens(LensFile)
                        ELSE  Announce('The data has not been saved.');
                     END
                     ELSE Announce('Lens Option not selected - Press F10 to continue ');
                  END;
              8 : ExitFlag := true;{BEGIN CupsDone; Halt END;}
            END;{case of row}
         2 : CASE rowChosen OF
              1 : BEGIN
                     RTdata.RToption := FmOpt;
                     RTdata.RTaction := false;
                     RTdata.lasta[1]:= 0.5;
                     NewOption;
                  END;
             END;{case of row}
         3 : CASE rowChosen OF
              1 : BEGIN
                     RTdata.RToption := MrOpt;
                     RTdata.RTaction := false;
                     NewOption;
                     ParsedOK:= IndexF.parse('x','y',' ',' ',RTdata.FuncString);
                     mcolor:= lightcyan;
                  END;
             END;{case of row}
         4 : CASE rowChosen OF
              1 : BEGIN
                     RTdata.RToption := FbOpt;
                     RTdata.RTaction := false;
                     NewOption;
                     FiberSetup(RTdata);
                  END;
              END;{case of row}
         5 : CASE rowChosen OF
              1 : BEGIN
                     RTdata.RToption := RbOpt;
                     RTdata.RTaction := false;
                     NewOption;
                     RTdata.second:= false;
                     InitRainBow(RTdata);
                  END;
              2 : BEGIN
                     RTdata.RToption := RbOpt;
                     RTdata.RTaction := false;
                     NewOption;
                     RTdata.second:= true;
                     InitRainBow(RTdata);
                  END;
             END;{case of row}
         6 : BEGIN
               IF RTdata.RToption <> LnOpt THEN RTdata.Center:= 0.0;
               RTdata.ZoomIn:= false;
               RTdata.SpotViewport:= true;
               CASE rowChosen OF
                1 : RTdata.RTobject:= infinite;
                2 : RTdata.RTobject:= nearby;
                3 : BEGIN {Edit lens}
                       IF  RTdata.RToption <> LnOpt THEN
                        Announce(' No Lens loaded - Press <Enter> to continue ')
                       ELSE BEGIN
                          LensInput;
                          DrawAxis(-0.1,RTdata.Center - 0.1,0.05,0.05,Black);
                          LensInit( 100);
                          RTdata.NoSpotData:= true;
                          DrawRaysInit(100);
                          SetUpHotKeys;
                       END;
                    END;
               END; {case of rowChosen}
               IF rowChosen <> 3 THEN BEGIN
                  RTdata.RToption:= LnOpt;
                  rowactivate(1,5,true);
                  rowactivate(1,6,true);
                  NewOption;
                  TraceMode := finite;
                  lens.scale:=1;{scale lens}
                  lens.y:=0.0;
                  IF NOT RTdata.LensLoaded THEN BEGIN
                     InitTrace; { initialize ray trace routines }
                     found:= false;
                     lensfile:= RTdata.LensFile;
                     LoadLens(found,lensfile);
                     IF NOT(found) THEN BEGIN
                        announce(RTdata.Lensfile+' NOT found, default data used.');
                        RTdata.LensFile:='default.len';
                        last:= 2;
                        Lens.c[1]:= 0.1;
                        Lens.t[1]:= 0.3;
                        Lens.n[1]:= 1.5;
                        Lens.r[1]:= 0.0;
                        Lens.c[2]:= -0.1;
                        Lens.t[2]:= 0.0;
                        Lens.n[2]:= 1.0;
                        Lens.r[2]:= 0.0;
                        Lens.fl:= 0.0;
                        Lens.bfl:= 0.0;
                        found:= true;
                     END;
                     RTdata.LensLoaded:= true;
                  END;
                  NoAberrData:= true;
                  LensInit( 100);
                  RTdata.NoSpotData:= true;
                  DrawRaysInit(100);
             END;
         END;
      END; {case}
   END;
END;

PROCEDURE UpDateSlider;
var
   xhot: real;
   tstr,tstr1 : string;

BEGIN
   xhot:= sliders.value(14);
   RTdata.SpotPlane:=xhot;
   sliders.Done;
   SelectMuppetPort;
   OpenViewport(18);
   tstr:= NumStr(0.9*xhot,6,3);
   tStr1:= NumStr(1.1*xhot,6,3);
   sliders.create(14,0.9*xhot,1.1*xhot,xhot, 0.02,0.56,0.45,3, tstr,tstr1,'distance',false);
   sliders.drawAll;
   RTdata.SpotPlane:= sliders.value(14);
END;

PROCEDURE HandleHotKeys (key : byte);
VAR
   NumAberration,hki : INTEGER;
   xhot,side,yy : real;
   found,FuncValid,NewData : boolean;
   lensfile,tstr,tstr1 : string;

BEGIN
   CASE RTdata.RToption OF
   FmOpt : CASE key OF
              1 : ShowHelp  ('RAYTRACE.HLP','Help 2' );
              2 : IF RayMenu.Chosen THEN HandleMenu(RTdata);
           END;
   MrOpt: CASE key OF
              1 : ShowHelp  ('RAYTRACE.HLP','Help 3' );
              2 : BEGIN
                     Finput(RTdata.FuncString,FuncValid);
                     RayFm.IndexVar(mcolor{num});
                  END;
              3 : BEGIN
                     OpenViewPort (5);
                     SelectViewport (5);
                     Setcolor(white);
                     Print(48,2,'Number of segements');
                     PutLabel(inside,'Ray path');
                     Setcolor(cyan);
                  END;
              4 : IF RayMenu.Chosen THEN HandleMenu(RTdata);
           END;
   FbOpt : CASE key OF
              1 : ShowHelp  ('RAYTRACE.HLP','Help 4' );
              2 : IF RayMenu.Chosen THEN HandleMenu(RTdata);
           END;
   RbOpt : CASE key OF
              1 : ShowHelp  ('RAYTRACE.HLP','Help 5' );
          {    2 : BEGIN
                     side:=RTdata.sides;
                     Setcolor(white);
                     OpenViewPort (11);
                     SelectScale(11);
                     Axis(-1.4,-1.4,side/2,side/2);
                     PutLabel(inside,'Ray paths');
                     Setcolor(cyan);
                  END;              }
              2 : IF RayMenu.Chosen THEN HandleMenu(RTdata);
           END;
   LnOpt : CASE key OF
              1 : ShowHelp  ('RAYTRACE.HLP','Help 6' );
              2 : BEGIN {center image}
                     IF RTdata.ZoomIn THEN BEGIN
                        UpDateSlider;
                        SelectViewport (16);
                        WITH RTdata.Spot[1] DO
                           yy:= y + (RTdata.SpotPlane - (z - Spos(last))) * m / n;
                        IF abs(RTdata.center - yy) > 0.05 THEN BEGIN
                           DrawAxis(-0.1,RTdata.Center - 0.1,0.05,0.05,Black);
                           FOR hki:= 1 TO RTdata.spotCounter DO
                              DrawSpot(RTdata.spot[hki],RTdata.OldPlane,Black);
                           RTdata.Center:= round(yy*20)/20;;
                           DefineScale (16,-0.1,0.1,RTdata.Center - 0.1,RTdata.Center + 0.1);
                           FOR hki:= 1 TO RTdata.spotCounter DO
                              DrawSpot(RTdata.spot[hki],RTdata.OldPlane,RTdata.spot[hki].color);
                           DrawAxis(-0.1,RTdata.Center - 0.1,0.05,0.05,White);
                        END;
                     END
                     ELSE Announce(' image will not be centered  Zoom-out');
                  END;
              3 : BEGIN {Zoom in}
                     IF NOT RTdata.SpotViewport THEN BEGIN
                        RTdata.ZoomIn := NOT RTdata.ZoomIn;
                        RTdata.SpotViewport:= true;
                     END;
                     {IF used to recover from seidel aberrations}
                     IF NOT RTdata.ZoomIn THEN BEGIN
                        RTdata.ZoomIn:= true;
                        RTdata.SpotViewport:= true;
                     GraphBackcolor:= 0;
                     SelectViewport (16);
                     Selectscale (16);
                     DrawAxis(-2.0,-2.0,0.5,0.5,black);
                     SetColor (black);
                     OpenViewport (16);
                     GraphBackcolor:= 8;
                     DefineScale (16,-0.1,0.1,RTdata.Center - 0.1,RTdata.Center + 0.1);
                     DrawAxis(-0.1,RTdata.Center - 0.1,0.05,0.05,White);
                     SetColor (white);
                     PutLabel(inside,'Spot Diagram');
                     tStr:= NumStr(RTdata.SpotCounter,6,1);
                     Print(3,12,'Number of rays traced = '+tStr+' ');
                     UpDateSlider;
                     Animate(RTdata.OldPlane,RTdata.SpotPlane);
                     RTdata.OldPlane:= RTdata.SpotPlane;
                  END

                  ELSE BEGIN {Zoom out}
                     Selectscale (16);
                     IF RTdata.ZoomIn THEN  DrawAxis(-0.1,RTdata.Center - 0.1,0.05,0.05,Black);
                     RTdata.ZoomIn:= false;
                     RTdata.SpotViewport:= true;
                     GraphBackcolor:= 0;
                     SetColor (black);
                     OpenViewport (16);
                     GraphBackcolor:= 8;
                     DefineScale (16,-2,2,-2,2);
                     DrawAxis(-2.0,-2.0,0.5,0.5,White);
                     SetColor (white);
                     PutLabel(inside,'Spot Diagram');
                     tStr:= NumStr(RTdata.SpotCounter,6,1);
                     Print(3,12,'Number of rays traced = '+tStr+' ');
                     xhot:= sliders.value(14);
                     RTdata.SpotPlane:=xhot;
                     sliders.Done;
                     SelectMuppetPort;
                     OpenViewport(18);
                     sliders.create(14,0,20,xhot,0.02,0.56,0.45,2, '0',
                        '20.0','distance',false);
                     sliders.drawAll;
                     RTdata.SpotPlane:= sliders.value(14);
                     Animate(RTdata.OldPlane,RTdata.SpotPlane);
                     RTdata.OldPlane:= RTdata.SpotPlane;
                     END;
                     SetUpHotKeys;
                  END;
              4 : BEGIN {Seidel aberrations}
                     RTdata.SpotViewport:= false;
                     NumAberration := 0;
                     SeidelChoice(NumAberration {NumSeidel} );
                     IF NumAberration > 0 THEN BEGIN
                        GraphBackColor:= black;
                        SelectScale (16);
                        IF RTdata.Zoomin THEN
                           DrawAxis(-0.1,RTdata.Center - 0.1,0.05,0.05,Black)
                           ELSE DrawAxis(-2.0,-2.0,0.5,0.5,Black);
                        SetColor (black);
                        OpenViewPort (16);
                        GraphBackColor:=8;
                        SelectViewport (16);
                        DrawSeidel(RTdata, NumAberration)   {DrawSeidel}
                     END
                     ELSE BEGIN
                     RTdata.SpotViewport:= true;
                     END;
                  END;
              5 : BEGIN {Object data}
                     NumAberration := 0;
                     RTdata.SpotViewport:= true;
                     IF RTdata.RTobject = infinite THEN AngleChoice(RTdata,NewData)
                        ELSE ObjectChoice(RTdata,NewData);
                     IF NewData THEN BEGIN
                        SelectViewPort(16);
                        IF RTdata.ZoomIn THEN  DrawAxis(-0.1,RTdata.Center - 0.1,0.05,0.05,Black);
                        RTdata.ZoomIn:= false;
                        LensInit( 100);
                        RTdata.NoSpotData:= true;
                        DrawRaysInit(100);
                        RTdata.ZoomIn:= false;
                        SetUpHotKeys;
                     END;
                  END;
              6 : IF RayMenu.Chosen THEN HandleMenu(RTdata);
           END; {case}
   END; {case}
END;

PROCEDURE FiberInit(var RTdata : Tdata);

VAR Icount : INTEGER;

BEGIN
   DefineViewPort (8, 0.06, 1.0, 0.55, 0.92);
   DefineViewPort (9, 0.06, 0.45, 0.1, 0.5);
   DefineScale (8,250.0,260.0,-0.2,1.2);
   DefineScale (9,-20.0,100.0,-0.2,1.2);

   with RTdata DO BEGIN
      Fiber[0]{diam}:= 50E-6;
      Fiber[1]{n1}:= 1.51;
      Fiber[2]{n2}:= 1.485;
      Fiber[3]{length}:= 50*1E+3;
      Fiber[4]{pulse}:= 50E-9;
      Fiber[7]{lambda}:= 1.3E-6;
      maxmode:= 1;
      FOR Icount:=0 TO 100 DO FiberDelay[Icount]:= 0.0;
   END;
END;

PROCEDURE Fminit;
begin
   ExitFlag := false;
   DefineViewPort (1, 0.0,  0.46, 0.40, 0.94);
   DefineViewPort (2, 0.52, 1.0,  0.55, 0.94);
   DefineViewPort (3, 0.52, 1.0,  0.12, 0.49);
   DefineViewPort (4, 0.0, 0.38,  0.06, 0.19);
   DefineScale (1,0.0,1.0,1.0,0.0);  {up-side down}
   DefineScale (2,0,1,0,2.5);
   DefineScale (3,0,1,1.3,3.3);
   OpenViewPort (1);
   OpenViewPort (2);
   OpenViewPort (3);
   OpenViewPort (4);
   SelectViewport(1);
   SetColor(white);
   PutLabel(inside,'Fermat ray path');
   Print(2,2,'n');
   Print(2,12,'n''');
   SelectViewport(2);
   SelectScale(2);
   Axis(0.0,0.0,0.2,0.5);
   PutLabel(inside,'Optical path');
   SelectViewport(3);
   SelectScale(3);
   Axis(0.0,1.3,0.2,0.5);
   PutLabel(inside,'Total Optical path');
   SetWriteMode (XorPut);                       {ZZZZZZZ}
   RayFm.SetUpFm(0.5,0.5,1.0,1.5,1.0,1.0,200);
   RayFm.PlotFm(cyan,brown,lightgray);
   RayFm.DrawFm(0.5,cyan,brown,lightgray,blue);
   SetWriteMode (CopyPut);
   SelectMuppetPort;
   sliders.create(1,0,1,0.5, 0.0,0.36,0.45,2, '0','1.0','position',false);
   sliders.create(2,0.01,0.99,0.5, 0.43,0.06,0.345,2, '0', '1', 'height  ' ,true );
   sliders.create(3,1,2,1.0, 0.0,0.29,0.35,2, '1.0', '2.0', 'index n' ,false );
   sliders.create(4,1,2,1.5, 0.0,0.22,0.35,2, '1.0', '2.0', 'index n''' ,false );
   sliders.drawAll;
   RTdata.RTaction := True;
end;

VAR
   key : byte;
   preva : real;
BEGIN
   CUPSinit;
   NumChar:= 40;
   sliders.init;
   IndexF.init;
   SetUpMyMenu;
   RTdata.RToption:= Fmopt;
   RTdata.RTaction:= false;
   RTdata.lasta[1]:=0.5;
   RTdata.sides:= 1.4; {length of side of square for raindrop}
   RTdata.LensFile:= 'Achromat.len';
   RTdata.FuncString:= '1.0003 - exp(-y) * 0.0001';
   RTdata.clrindex:= false;
   RTdata.H2Oindex:= 1.33;
   preva:=0.5;
   SetUpHotKeys;
   FiberInit(RTdata);
   Fminit;
{   ShowHelp('RAYTRACE.HLP','Help 1' );  }
   ShowInitialScreen;{Help(helpScr0);}
   { initialisation of lens procedures }
   RTdata.SpotPlane:= 6.0;
   RTdata.ObjectDist:= - 50.0;
   RTdata.Radialdist:= 0.5;
   RTdata.Center:= 0.0;
   RTdata.LensLoaded:= false;
   REPEAT
      CheckForEvents;
      IF HotKeys.Pressed(key) THEN HandleHotKeys(key);
      IF RayMenu.Activated THEN HandleMenu(RTdata);
      IF Sliders.Changed then HandleSliders(RTdata);
      IF (RTdata.RToption = FmOpt) AND (RTdata.RTaction = false) THEN Fminit;
      preva:=RTdata.lasta[1];

      IF (RTdata.RToption = MrOpt) AND (RTdata.RTaction = false)
           THEN RayFm.Mirinit(500,4,0.5,200);

   UNTIL ExitFlag;
   RayFm.Done;
   RayMenu.Done;
   CupsDone;
END.
