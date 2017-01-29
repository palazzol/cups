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

                   {************************************}
                   {************************************}
                   {         DETHIST                    }
                   {                                    }
                   {        histogram utilities         }
                   {                                    }
                   {                      J. Rothberg   }
                   {                     31 May 1992    }
                   {                     1 June 1994    }
                   {************************************}

UNIT dethist;       { Histogram }

interface

uses Graph, Crt, cups, cupsmupp, cupsproc, cupsgrph, cupsgui;

CONST
    maxbins : Integer = 50;
    ymaxdef : Integer = 50;

VAR
    xxx,yyy: DVector;
    xh,yh  : DVector;

TYPE
    string8 = string[8];
    harray  = array[0..50] of Real;
    Functionxyz = function(x,y,z: Real): Real;

TYPE
     hplot = object                        { histogram }
      vname :   String[8];
      vport :   Integer;
      vscale:   Integer;
      vmin  :   Real;
      vmax  :   Real;
      vdiv  :   Real;
      vminy :   Real;
      vmaxy :   Real;
      vdivy :   Real;
      vbins :   Integer;
      valh  :   Real;
      overflow: Integer;
      events  : Integer;
      hist  :   array[0..50] OF Real;
      x1port,x2port,y1port,y2port : Real;   {port locations}

     PROCEDURE firstinit;
     PROCEDURE OpenPort(iport:Integer; ix1p,ix2p,iy1p,iy2p: Real);
     PROCEDURE init(ivmin, ivmax: Real; iport:Integer; ivname :String8);
     PROCEDURE fill(ival: Real);
     FUNCTION  hdata(ibin: Integer): Real;
     FUNCTION  hxval(ibin: Integer): Real;
     PROCEDURE hplotf(val: harray);
     PROCEDURE plot(evtno: Integer);
     PROCEDURE ploterr;
     PROCEDURE hout;
     PROCEDURE clearplot;
     PROCEDURE refresh;
     PROCEDURE fitfunc(center,sigma: Real; func: functionxyz);
     PROCEDURE sini(iminx,imaxx,iminy,imaxy : Real; iport:Integer;
                              ixname,iyname: String8);
     PROCEDURE scat(ixval,iyval : Real);

    END;


implementation

     PROCEDURE relabel(minx,maxx: Real; name: String8);
BEGIN
    OUTTextxy(2,2,name);
    OutTextxy(80,8,'min='+ NumStr(minx,6,0));
    OutTextxy(180,8,' max='+ NumStr(maxx,6,0));
END;

     PROCEDURE replotaxis(port,scale,bins:Integer; miny,maxy,minx,maxx,
                      divi,divy: Real; name:String8);
BEGIN
    OpenViewPort(port+10);      {clear window}
    SelectViewPort(port+10);
    ClearViewPort;

    SelectViewPort(port);
    OpenViewPort(port);
    DefineScale(scale,0,bins,miny,maxy);
    SelectScale(scale);
    Axis(0.0, 0.0, divi, divy);

    relabel(minx,maxx,name);

END;

    PROCEDURE hplot.OpenPort(iport:Integer; ix1p,ix2p,iy1p,iy2p: Real);
BEGIN
    x1port:=ix1p; x2port:=ix2p; y1port:=iy1p; y2port:= iy2p;
    vport:= iport;
    DefineViewPort(iport,ix1p,ix2p,iy1p,iy2p);
    OpenViewPort(iport);
    DefineViewPort(iport+10,ix1p-0.05,ix2p,iy1p,iy2p+0.01);   {to clear}
END;

    PROCEDURE hplot.init(ivmin, ivmax: Real; iport: Integer; ivname :String8);

VAR
  i : Integer;
BEGIN

    setcolor(white);


    vmin  := ivmin;
    vmax  := ivmax;
    vname := ivname;
    vport := iport;
    vscale:= vport+10;

    vdiv := 5;
    vminy:= 0; vmaxy:= ymaxdef; vdivy:= ymaxdef/5;
    vbins:= maxbins;
    overflow:= 0;

    xh.init(vbins);
    yh.init(vbins);

    OpenViewPort(vport);
    SelectViewport(vport);

    DefineScale(vscale,0,vbins,vminy,vmaxy);
    SelectScale(vscale);
    Axis(0.0, 0.0, vdiv, vdivy);
{    relabel(vmin,vmax,vname); }
    OUTTextxy(2,2,vname);

    FOR i := 1 TO vbins DO
    BEGIN
       hist[i] := 0;
    END;

END;

    PROCEDURE hplot.sini(iminx,imaxx,iminy,imaxy : Real; iport:Integer;
                              ixname,iyname: String8);
BEGIN
    setcolor(white);

    vmin   := iminx;
    vmax   := imaxx;
    vminy  := iminy;
    vmaxy  := imaxy;
    vport := iport;
    vscale:= vport+10;

    vdiv  := vmax/5;
    vdivy := vmaxy/5;
    xxx.init(1); yyy.init(1);

    OpenViewPort(vport);
    SelectViewport(vport);

    DefineScale(vscale,vmin,vmax,vminy,vmaxy);
    SelectScale(vscale);
    Axis(0.0, 0.0, vdiv, vdivy);

    OutTextxy(2,2,ixname+' (x)');
    OutTextxy(160,2,iyname+' (y)');
END;

    PROCEDURE hplot.firstinit;
BEGIN
END;

    PROCEDURE hplot.fill(ival : Real);
VAR
    ibin : Integer;
    hval : Real;
BEGIN
    hval := ival;
{    ibin:= Round(vbins*(hval-vmin)/(vmax-vmin)); }
     ibin:= Trunc(vbins*(hval-vmin)/(vmax-vmin)) + 1;
    IF (ibin < 1) THEN ibin:= 1;
    IF (ibin > vbins) THEN ibin:= vbins;
    hist[ibin] := hist[ibin] + 1;
    IF (hist[ibin] >= vmaxy)  THEN overflow := 1;
END;

    FUNCTION hplot.hdata(ibin: Integer): Real;

BEGIN
    hdata := hist[ibin];
END;

    FUNCTION hplot.hxval(ibin: Integer): Real;
BEGIN
{       hxval:= vmin + (vmax-vmin)*ibin/vbins - 1.0 ;}
       hxval:= vmin + (vmax-vmin)*(ibin-0.5)/vbins  ;
END;

     PROCEDURE hplot.hplotf(val: harray);       { plot function }
VAR     i   : Integer;
        xdat,ydat : DVector;
BEGIN
    SelectViewPort(vport);
    SelectScale(vscale);
    xdat.Init(51); ydat.Init(51);
    FOR i:= 1 to vbins DO
    BEGIN
       xdat.put(i,i);
       ydat.put(i,val[i]);
    END;
    SETCOLOR(lightmagenta);
    PlotDVectors(xdat,ydat,1,vbins);
    SETCOLOR(white);
    xdat.free; ydat.free;
END;

      PROCEDURE hplot.refresh;                 { replot data }
BEGIN
      replotaxis(vport,vscale,vbins,vminy,vmaxy,vmin,vmax,vdiv,vdivy,vname);
      plot(events);
      ploterr;
END;

    PROCEDURE hplot.plot(evtno: Integer);      { plot histogram }
VAR
    i,flag   : Integer;

BEGIN
    events := evtno;
        { check for overflow, rescale vertical axis }
    IF (overflow = 1) THEN
    BEGIN
      overflow := 0;
      vmaxy:= 2*vmaxy;
      vdivy:= 2*vdivy;
      replotaxis(vport,vscale,vbins,vminy,vmaxy,vmin,vmax,vdiv,vdivy,vname);
    END;

    SelectViewPort(vport);
    SelectScale(vscale);
    FOR i:= 1 to vbins DO
    BEGIN
       xh.put(i,i);
       yh.put(i,hist[i]);
    END;
    SETCOLOR(yellow);
    PlotPoints(xh,yh,'o');

    SetColor(lightgreen);
    RubOut(22, 2, 4, Graphbackcolor);
    print (22, 2, numStr(evtno,4,0)+ ' events');
    SetColor(white);
END;

    PROCEDURE hplot.scat(ixval, iyval :Real);
BEGIN
    SelectViewPort(vport);
    SelectScale(vscale);
    xxx.put(1,ixval);
    yyy.put(1,iyval);

    SetColor(yellow);
    PlotPoints(xxx,yyy,'.');
    SetColor(white);
END;

    PROCEDURE hplot.ploterr;                     { plot error bars }
VAR  i    : Integer;
     xval,hval,err   : Real;
BEGIN
    SelectViewPort(vport);
    SelectScale(vscale);
    SetColor(lightcyan);
    FOR i:= 1 to vbins DO
    BEGIN
       xval:= i;
       hval:= hist[i];
       err:= Sqrt(hval);
       plotline(xval,hval+err,xval,hval-err);
    END;
    SETCOLOR(white);
END;

    PROCEDURE hplot.hout;              { output histogram to file}
VAR  i        : Integer;
     outfile  : Text;
     outFilename : String[12];
BEGIN
    outFileName:=vname+'.his';
    Assign(outfile,outFileName);
    Rewrite(outfile);
    writeln(outfile,vbins:5,outfileName:16);
    FOR i := 1 TO vbins DO
    BEGIN
       writeln(outfile,i:5,hxval(i):10:2,hist[i]:10:0);
    END;
    close(outfile);
END;

    PROCEDURE hplot.clearplot;
VAR
  i  : Integer;
BEGIN
    OpenViewPort(vport);
    SelectViewPort(vport);

    DefineScale(vscale,0,vbins,vminy,vmaxy);
    SelectScale(vscale);
    Axis(0.0, 0.0, vdiv, vdivy);

    OUTTextxy(2,2,vname);
    FOR i := 1 TO vbins DO
    BEGIN
     hist[i] := 0;
    END;
END;

{ -------------------------------------------------------------------}
        PROCEDURE hplot.fitfunc(center,sigma: Real; func: functionxyz);
CONST
      ythresh  :   Real = 4;
VAR
      i           : Integer;
      sumf,sumf2  : Real;
      fun, wt     : Real;
      xval        : Real;
      data        : Real;
      norm        : Real;
      chisq       : Real;
      numbin      : Integer;
      funarr      : Harray;
BEGIN
         { calculate normalization }
       sumf  := 0;
       sumf2 := 0;
       FOR i := 1 TO maxbins DO
       BEGIN
         funarr[i] := 0;
         data := hdata(i);
         xval := hxval(i);
         fun := func(center,sigma,xval);
         funarr[i] := fun;

         IF (data > ythresh ) THEN
         BEGIN
           wt   := 1/data;
           sumf := sumf  + fun;
           sumf2:= sumf2 + fun*fun*wt;
         END;

       END;
       norm:=0;
       IF( sumf2 > 0 ) THEN norm := sumf/sumf2;

       OpenViewPort(vport+4);
       SelectViewPort(vport+4);
       OutTextxy(2,5, 'norm=   '+numStr(norm,10,2));
       OutTextxy(2,15,'Param1= '+numStr(center,10,2));
       OutTextxy(2,25,'Param2= '+numStr(sigma,10,2));

               { calculate chi squared }
       chisq:= 0;
       numbin := 0;
       FOR i := 1 TO maxbins DO
       BEGIN
           funarr[i] := norm*funarr[i];
           data := hdata(i);
           wt := 0;
           IF( data > ythresh ) THEN
           BEGIN
              numbin := numbin + 1;
              wt := 1/data;
           END;
           chisq := chisq + sqr(funarr[i]-data)*wt;
       END;

       OutTextxy(2,46, ' chisq= '+numStr(chisq,10,2)+
                                  numStr(numbin,5,0)+' bins' );

       refresh;
       hplotf(funarr);

END;


END.
