           (*************************************************)
           (*************************************************)
           (**                                             **)
           (** Consortium of Upper-level Physics Software  **)
           (**             ( CUPS Project )                **)
           (**     (c) 1994 by John Wiley & Sons           **)
           (** Compiled with Utilities Ver. 1.7 (95/06/27) **)
           (**                                             **)
           (*************************************************)
           (*************************************************)

{$D+,L+}
              {****************************************}
              {****************************************}
              {   Unit PDETEC  Relativistic Kinematics }
              {                 detector simulation    }
              {                                        }
              {                         J. Rothberg    }
              {                                        }
              {                       10 Jun 1994      }
              {****************************************}
              {****************************************}


UNIT pdetec;

interface

uses Graph, Crt, cups, cupsmupp, cupsgui, cupsgrph, relutil, pscat12;

VAR
    numhits : Integer ;

TYPE  Ptarr = array[1..4] of Real;

VAR           { save previous hits }
    xs1,xs2,ys1,ys2,xs1z,xs2z,ys1z,ys2z,ycent : Ptarr;
    ssn,ssnz : Ptarr;
TYPE
    lstr = string[6];
TYPE
  dethit = record            {detector hits}
    seen    : Integer;
    count   : Integer;       {accumulated number of hits}
    xhit    : Real;
    yhit    : Real;
    Energy  : Real;
    Ebox    : Real;
    color   : Integer;
  END;
  Hitarr  = array[1..4] of dethit;

TYPE
  truhit = record             {true values}
    pnum    : Integer;        { particle number}
    name    : String[8];      { particle name}
    tanth   : Real;
    phi     : Real;
    Energy  : Real;
  END;
  Truarr  = array[1..4] of truhit;

TYPE
     detect = object
       detz1       : Real;          {det front edge z   cm }
       detzdel     : Real;          {det width in   z   cm }
       detzmax     : Real;          {size of YZ view}
       detzcent    : Real;          {det center in z}
       detrad      : Real;          {det radius    cm }
       detradsq    : Real;          {radius squared }
       decayz1     : Real;          {start of decay region  cm}
       decayz2     : Real;          {END of decay region  cm}
       DecayLen    : Real;          {decay length   cm}
       energyscale : Real;          { display scale   cm/MeV }
       EnergyRes   : Real;          {Energy Resolution  %/root(E) }
       positRes    : Real;          {position Resolution   cm}

       PROCEDURE firstinit;

     END;

     evntPtr = ^evnt;

     evnt   = object(detect)
       debugflag : boolean;
       evhits  : Integer;         {number of hits}
       color   : Integer;         {display color}
       events  : Integer;         {number of events}
       pbeam   : Real;            {incoming beam momentum}
       hits    : Hitarr;          {detector hits}
       decaypt : Real;            {decay point in z}
       truth   : Truarr;          {Monte Carlo truth}

      CONSTructor Init;
      PROCEDURE hitclear;
      PROCEDURE setscale;
      PROCEDURE setdetect(irad,zpos,ienscl,dec1,dec2,decleng: Real);
      PROCEDURE getdetect(VAR irad,zpos,ienscl,dec1,dec2,decleng: Real);
      PROCEDURE setResp(Eres,xyRes: Real);          {det response}
      PROCEDURE getResp(VAR Eres,xyRes: Real);
      PROCEDURE detdraw;
      PROCEDURE EnergyDeposit(parti:pardec; ihit:Integer;
                      VAR hitseen:Boolean; VAR labeng:Real);
      PROCEDURE fill(parti: pardec);
      PROCEDURE draw;
      PROCEDURE list(parti: pardec);
      PROCEDURE getmouse;
      PROCEDURE filloutdat;
      PROCEDURE WriteData;
    END;



implementation

     FUNCTION Rand_gauss(center, sigma: Real) : Real;
VAR
     twou, radius, theta : Real;
BEGIN
     twou:= -2*ln(1-random);
     radius:= Sqrt(twou);
     theta:= 2*cpi*Random;
     Rand_gauss:= sigma*radius*cos(theta) + center;

END;

     PROCEDURE detect.Firstinit;
                                             { detector geometry }
BEGIN
      numhits := 4;
      detz1  :=   1000;
    {  detzdel:=     50; }
      detzmax:=   1100;
      detzdel:=   detzmax/20;
      detrad :=    100;
      decayz1:= 0;                     {decay region}
      decayz2:= 0;
      decayLen:= 0;
      energyscale:= 0.004;             {energy scale factor  cm/MeV}
      EnergyRes  := 0;
      PositRes   := 0;

END;  {firstinit}

     CONSTructor evnt.init;
VAR   i  : Integer;
BEGIN
     initcirc;
     detect.firstinit;
     setscale;
     hitclear;
     debugflag:=true;
     FOR i := 1 TO  numhits DO
     BEGIN
         ssn[i]:=0; ssnz[i]:=0;
     END;
END;

     PROCEDURE evnt.hitclear;
VAR  ihit   : Integer;
BEGIN
      events:=0;
      {clear hit counters}
      FOR ihit:=1 TO numhits DO hits[ihit].count:=0;
END;

     PROCEDURE evnt.setscale;                        {set plot scale}
VAR
      drext  : Real;
BEGIN
      detradsq:=detrad*detrad;
      drext:= detrad*1.1;

      DefineScale(6,  -0.05*detzmax, detzmax, -drext,drext);   { YZ view }
      DefineScale(7, -drext,drext,   -drext,drext);          { XY view }


END;

     PROCEDURE evnt.setdetect(irad,zpos,ienscl,dec1,dec2,decleng : Real);
BEGIN                                          {set detector params}
       detrad   := irad;
       detz1    := zpos;
       decayz1  := dec1;
       decayz2  := dec2;
       decayLen := decleng;
       energyscale:= ienscl;
       setscale;
       detdraw;
END;

     PROCEDURE evnt.getdetect(VAR irad,zpos,ienscl,dec1,dec2,decleng :Real);
BEGIN
       irad  := detrad;
       zpos  := detz1;
       dec1  := decayz1;
       dec2  := decayz2;
       decleng:= decayLen;
       ienscl := energyscale;
END;

     PROCEDURE evnt.setResp(Eres,xyRes: Real);
BEGIN
        EnergyRes:= Eres;
        PositRes := xyRes;
END;

     PROCEDURE evnt.getResp(VAR Eres,xyRes: Real);
BEGIN
        Eres := EnergyRes;
        xyRes:= PositRes;
END;

     PROCEDURE evnt.detdraw;

VAR  i         : Integer;
     Xscr, Yscr: Integer;
     x,y       : Real;
     Radius    : Integer;
     detz2     : Real;
BEGIN
     { clear old hits }
     FOR i := 1 TO  4 DO
     BEGIN
         ssn[i]:=0; ssnz[i]:=0;
     END;

                   { YZ view }
   Hidecursor;
   OpenViewPort(6);
   SelectViewport(6);
   SelectScale(6);
   SetFillStyle (solidfill,blue);
   SetColor(white);
   Plottext(0,-detrad,'YZ  ');
   Plotline(-0.02*detzmax,0, detzmax,0);
   Plotline(0,-0.05*detrad,0,0.05*detrad);
   detz2:= detz1 + detzdel;
   detzcent:=  detz1+detzdel/2;
   Plottext(0.90*detz1,1.07*detrad, CONCAT('z=',numStr(detz1,4,0)));
      {detector outline}
   setcolor(cyan);
   Plotrect(detz1,-detrad,detz2,detrad);
   Xscr:=mapx(detz1+detzdel/2); Yscr:=mapy(0);
   floodfill(Xscr,Yscr,cyan);
   Plotline(detz1,0,detz2,0);
   setcolor(white);
                    {  XY view }
   OpenViewport(7);
   SelectViewport(7);
   SelectScale(7);
   SETCOLOR(white);
   PlotText(-detrad,-detrad,'XY');

     {detector outline}
   x:= 0;
   y:= 0;
   DrawFillCirc(x,y,detrad,blue);
   setcolor(cyan);
   Plotcirc(x,y,detrad);
   Plotline(-detrad,0,detrad,0);Plotline(0,-detrad,0,detrad);

   setcolor(white);
{  Plottext(0.53*detrad,0.99*detrad,CONCAT('R=',numStr(detrad,4,0),' cm'));}
{   Plottext(0.65*detrad,0.83*detrad,'    cm');   }

   Plottext(0.70*detrad,-0.90*detrad,numStr(energyscale,4,3));
   Plottext(0.70*detrad,-0.97*detrad,'cm/MeV');
   Plottext(-detrad,1.03*detrad,CONCAT('R=',numStr(detrad,4,0),' cm'));

   Showcursor;
END;

     PROCEDURE evnt.EnergyDeposit(parti:pardec; ihit:Integer;
                      VAR hitseen:Boolean; VAR labeng:Real);
BEGIN

       hitseen:= true;
       CASE truth[ihit].pnum OF
        {neutrino}
        4 : BEGIN
                    {  hitseen:= false;  }
                    {  labeng:=-1;        }
                      labeng:= parti.labenergy(ihit);
                      hits[ihit].color:=lightred;
                      END;
        {muon}
        5     : BEGIN
                      labeng := 200;          { minimum ionizing}
                      hits[ihit].color:=yellow;
                   END;
       ELSE
        BEGIN
        labeng:= parti.labenergy(ihit);
        hits[ihit].color:=white;
        END;
       END;
END;

     PROCEDURE evnt.fill(parti: pardec);
TYPE
     hitvar = array[1..4] of Real;
VAR
     ihit    : Integer;
     phi     : Real;
     hitrad  : Real;
     hitseen : boolean;
     labeng  : Real;
     labeng_res : Real;
     sigma      : Real;
     xtemp,ytemp: Real;

BEGIN
     evhits:= 2;

     decaypt:=decayz1;
     truth[1].name:=parti.par1.pname;
     truth[2].name:=parti.par2.pname;
     truth[1].pnum:=parti.par1.pnum;
     truth[2].pnum:=parti.par2.pnum;

     pbeam := parti.parin1.plab;
     events:= events + 1;

     FOR ihit:=1 TO evhits DO
     BEGIN
       EnergyDeposit(parti,ihit,hitseen,labeng);   { deposit energy in det.}
       hits[ihit].seen:= 0;
       hits[ihit].energy:= -1;
       phi:=parti.labphi(ihit);
       hitrad:= abs((detz1-decaypt)*parti.labtan(ihit));
       IF (hitrad*hitrad < detradsq) and hitseen THEN
       BEGIN
  {resolution smeared energy, resolution in % /sqroot(E (GeV)) }
          IF (EnergyRes > 0.1) and (labeng > 0) THEN
          BEGIN      {convert from % to fraction, GeV to MeV units}
            sigma:= 10.*EnergyRes/Sqrt(labeng/1000);
            labeng_res:= Rand_Gauss(labeng, sigma);
          END
          ELSE labeng_res:= labeng;

          hits[ihit].seen:= 1;
          hits[ihit].count := hits[ihit].count + 1;
          hits[ihit].xhit  := hitrad*cos(phi);
          hits[ihit].yhit  := hitrad*sin(phi);
          hits[ihit].energy:= labeng_res;
          hits[ihit].ebox  := hits[ihit].energy * energyscale/2;
   { resolution smeared position; resolution in cm }
          IF (positRes > 0.1 ) THEN
          BEGIN
            xtemp:= hits[ihit].xhit;
            ytemp:= hits[ihit].yhit;
            hits[ihit].xhit := Rand_gauss(xtemp,positRes);
            hits[ihit].yhit := Rand_gauss(ytemp,positRes);
          END;
       END;
     END;

END;

     PROCEDURE evnt.draw;
VAR
     x1, y1, x2, y2, del, delz  : Real;
     ihit : Integer;
     delmin : Real ;      { minimum box size }
BEGIN
     delmin := 3.0;
                        { XY view}
     Selectviewport(7);
     SelectScale(7);
{     setcolor(white);  }    { in xor mode gives yellow on blue }
     setwritemode(XORput);

     FOR ihit:= 1 TO evhits DO
     BEGIN
       setcolor(hits[ihit].color);
      IF(ssn[ihit]=1) THEN plotrect(xs1[ihit],ys1[ihit],xs2[ihit],ys2[ihit]);  { erase }
       ssn[ihit]:=0;
       IF (hits[ihit].seen = 1) THEN
       BEGIN
         del:= hits[ihit].ebox;
         IF( del < delmin ) THEN del:= delmin;
         x1:=  hits[ihit].xhit-del;
         y1:=  hits[ihit].yhit-del;
         x2:=  x1+2*del;
         y2:=  y1+2*del;
         plotrect(x1,y1,x2,y2);
         ssn[ihit]:=1;
         xs1[ihit]:=x1; xs2[ihit]:=x2; ys1[ihit]:=y1; ys2[ihit]:=y2;
       END;
     END;

                  { YZ view}
     Selectviewport(6);
     SelectScale(6);
   {  setcolor(yellow); }

     FOR ihit:= 1 TO evhits DO
     BEGIN
      setcolor(hits[ihit].color);
      IF(ssnz[ihit]=1) THEN
      BEGIN
            PlotRect(xs1z[ihit],ys1z[ihit],xs2z[ihit],ys2z[ihit]);  { erase }
            PlotLine(0,0,detz1,ycent[ihit]);
      END;
      ssnz[ihit]:=0;

       IF(hits[ihit].seen = 1 ) THEN
       BEGIN
         del:=  hits[ihit].ebox;
         delz:= detzdel/4;
         x1:=  detzcent-delz;
         y1:=  hits[ihit].yhit-del;
         x2:=  x1+2*delz;
         y2:=  y1+2*del;
         PlotRect(x1,y1,x2,y2);
         PlotLine(0,0,detz1,y1+del);
         ssnz[ihit]:=1;
         xs1z[ihit]:=x1; xs2z[ihit]:=x2; ys1z[ihit]:=y1; ys2z[ihit]:=y2;
         ycent[ihit]:=y1+del;
       END;
     END;
     setcolor(white);
     SetWriteMode(copyput);
END;

     PROCEDURE evnt.list(parti: pardec);

VAR
      cl2, cl1, cl3 : Integer;  {text columns}
VAR
      h,hs : Integer;
      plus : String[3];
BEGIN
       cl2 := 160; cl1:= 65; cl3:=230;  {text columns}
       OpenViewPort(5);
       SelectViewPort(5);
       SetColor(yellow);
       h:= TextHeight('0')+4; hs:=0;
       plus:=' + ';
       IF(parti.parin2.pnick = '    ') THEN plus:='   ';
       OutTextXY(5,2+    hs, CONCAT(
                             parti.parin1.pname , plus ,
                             parti.parin2.pname , ' --> ' ,
                             parti.par1.pname , ' + ' ,
                             parti.par2.pname ));
       SetColor(white);
       OutTextxy(5,2+3*h+hs, CONCAT(
                             'p beam ' , numStr(pbeam,8,1) ,' MeV',
                             '  events ', numStr(events,4,0)));
       hs:=hs+3;
      IF debugflag THEN
      BEGIN
       OutTextxy(5,2+4*h+hs, 'x hit  ');
       OutTextxy(5,2+5*h+hs, 'y hit  ');
       OutTextxy(5,2+6*h+hs, 'energy ');
       OutTextxy(cl3,2+4*h+hs, ' cm ');
       OutTextxy(cl3,2+5*h+hs, ' cm ');
       OutTextxy(cl3,2+6*h+hs, ' MeV');

       IF hits[1].seen = 1 THEN
       BEGIN
        OutTextxy(cl1, 2+4*h+hs, numStr(hits[1].xhit,8,2));
        OutTextxy(cl1, 2+5*h+hs, numStr(hits[1].yhit,8,2));
        OutTextxy(cl1, 2+6*h+hs, numStr(hits[1].energy,8,2));
       END;
       IF hits[2].seen = 1 THEN
       BEGIN
        OutTextxy(cl2,2+4*h+hs,  numStr(hits[2].xhit,8,2));
        OutTextxy(cl2,2+5*h+hs,  numStr(hits[2].yhit,8,2));
        OutTextxy(cl2,2+6*h+hs,  numStr(hits[2].energy,8,2));
       END;
      END;  {debugflag}

       hs:=hs+3;
       OutTextxy(5,2+7*h+hs, CONCAT(
                        'hits   ',numStr(hits[1].count, 8,0),'    ',
                                       numStr(hits[2].count, 8,0)));
END;

   PROCEDURE evnt.filloutdat;
                                {fill output data array}
BEGIN
  outdata[1] := hits[1].xhit;
  outdata[2] := hits[1].yhit;
  outdata[3] := hits[1].energy;
  outdata[4] := hits[2].xhit;
  outdata[5] := hits[2].yhit;
  outdata[6] := hits[2].energy;
  outdata[7] := detz1;
  outdata[8] := 0;
END;

     PROCEDURE evnt.WriteData;
VAR
     it : Integer;
BEGIN
     FOR it:=1 to maxout DO Write(outfile,outdata[it]:10:3);
     Writeln(outfile);
END;

      PROCEDURE evnt.getmouse;
VAR
    xmse,ymse : Real;
    inside    : boolean;
BEGIN
         SelectViewPort(5);
                                           {digitize point on XY view}
         mouseposn(xmse,ymse,7,7,inside);      {XY view}
         IF(inside) THEN
         BEGIN
           IF( xmse > -detrad) and ( xmse < detrad ) and
                          (ymse>-detrad) and (ymse<detrad)
           THEN
           BEGIN
             DrawBlankBox(40,1,36,darkgray);
             setcolor(yellow);
             OutTextxy(320,2, CONCAT(
                                     'mouse ',numStr(xmse,8,2),'    ',
                                      numStr(ymse,8,2) ,' cm'));
            END;
         END;

                               {digitize point on YZ view; redraw det}
         mouseposn(xmse,ymse,6,6,inside);
         IF(inside) THEN
         BEGIN
           IF( xmse >= detz1) and ( xmse < detz1+detzdel ) and
                          (ymse>-detrad) and (ymse<detrad)
             THEN
             BEGIN
               DrawBlankBox(40,1,36,darkgray);
               setcolor(lightcyan);
               OutTextxy(320,2, CONCAT(
                                       'mouse ',numStr(xmse,8,2),'    ',
                                     numStr(ymse,8,2)));

               setcolor(white);
             END;
         END;

         setcolor(white);
END;


END.


