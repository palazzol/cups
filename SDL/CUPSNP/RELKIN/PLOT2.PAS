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
                   {         Plot2                      }
                   {               scatterplot          }
                   {               histogram            }
                   {                   for RELKIN       }
                   {                      J. Rothberg   }
                   {                                    }
                   {        10 June 1994                }
                   {************************************}

UNIT plot2;       { Scatter plot and Histogram }

interface

uses Graph, Crt, cups, cupsMUPP, cupsproc, cupsgrph, cupsgui;

    
TYPE
   plotvar = record               { attributes of variables}
      num:    Integer;
      name:   String[6];          { variable name }
      ncls:   String[4];          { CM/Lab; particle 1,2 }
      vmin:   Real;               { axis min     }
      vmax:   Real;               { axis max     }
      vdiv:   Real;               { axis subdivisions }
      val:    Real;               { current value }
   END;

   vars = array[1..9,1..4] of plotvar;
VAR
   vcons,varco: vars;
   Whitte: Integer;

VAR                      { kinematic variables}
      energy    : Integer;
      momentum  : Integer;
      beta      : Integer;
      theta     : Integer;
      costheta  : Integer;
      ptrans    : Integer;
      kinetic   : Integer;
      angdiff   : Integer;
      invmass   : Integer;
      cm1       : Integer;
      cm2       : Integer;
      lab1      : Integer;
      lab2      : Integer;

VAR
    xxx,yyy: DVector;
    xh,yh  : DVector;
   TYPE
     splot = object                  { scatterplot }
      vnum1:   Integer;
      vclass1: Integer;
      vname1:  String[6];
      vcls1:   String[4];
      vmin1:   Real;
      vmax1:   Real;
      vval1:   Real;
      vdiv1:   Real;
      vnum2:   Integer;
      vclass2: Integer;
      vname2:  String[6];
      vcls2:   String[4];
      vmin2:   Real;
      vmax2:   Real;
      vval2:   Real;
      vdiv2:   Real;

     PROCEDURE firstinit(inum1,iclass1,inum2,iclass2: Integer);
     PROCEDURE init(inum1,iclass1,inum2,iclass2: Integer);
     PROCEDURE getscl(VAR ixlow,ixhigh,iylow,iyhigh: Real);
     PROCEDURE rescl(     ixlow,ixhigh,iylow,iyhigh: Real);

     PROCEDURE plot;
     PROCEDURE clearplot;

    END;

   TYPE
     hplot = object                        { histogram }
      vnum1:   Integer;
      vclass1: Integer;
      vname1:  String[6];
      vcls1:   String[4];
      vmin1:   Real;
      vmax1:   Real;
      vmin2:   Real;
      vmax2:   Real;
      vdiv1:   Real;
      vdiv2:   Real;
      vbins:   Integer;
      valh:     Real;
      hist:    array[0..20] OF Real;
      hisx:    array[0..20] OF Real;

     PROCEDURE firstinit(inum1,iclass1: Integer);
     PROCEDURE init(inum1,iclass1: Integer);
     PROCEDURE fill;
     PROCEDURE plot(evtno: Integer);
     PROCEDURE resize(evtno: Integer);      { plot histogram }
     PROCEDURE clearplot;

    END;


implementation

    PROCEDURE splot.init(inum1,iclass1,inum2,iclass2: Integer);


BEGIN
    whitte:=white;

    setcolor(whitte);

    vnum1:= inum1; vclass1:= iclass1;
    vnum2:= inum2; vclass2:= iclass2;

    vname1:= vcons[inum1,iclass1].name;
    vname2:= vcons[inum2,iclass2].name;

    vcls1:= vcons[inum1,iclass1].ncls;
    vcls2:= vcons[inum2,iclass2].ncls;

    vmin1:= vcons[inum1,iclass1].vmin;
    vmin2:= vcons[inum2,iclass2].vmin;

    vmax1:= vcons[inum1,iclass1].vmax;
    vmax2:= vcons[inum2,iclass2].vmax;


     vdiv1:= (vmax1-vmin1)*0.25;
     vdiv2:= (vmax2-vmin2)*0.25;
     IF(vdiv1 < macheps) THEN vdiv1:=10;
     IF(vdiv2 < macheps) THEN vdiv2:=10;

    xxx.init(1);yyy.init(1);

    SelectViewPort(2);
    ClearViewport;
    OpenViewport(2);
    DefineScale(12,vmin1,vmax1,vmin2,vmax2);
    SelectScale(12);
    Axis(0.0, 0.0, vdiv1, vdiv2);
{    OUTTextxy(2,2,CONCAT(vname1,' ',vcls1,'  ',vname2,' ',vcls2));}
    OUTTextxy(42, 2,CONCAT('x=',vname1,' ',vcls1));
    OUTTextxy(42,12,CONCAT('y=',vname2,' ',vcls2));

END;

    PROCEDURE splot.firstinit(inum1,iclass1,inum2,iclass2: Integer);

VAR
  F: Text;
  rnum  : Integer;
  rname : String[6];
  rncls : Integer;
  rmin  : Real;
  rmax  : Real;
  rdiv  : Real;

  readmess: String[16];

TYPE
  rscls = array[1..4] of String[4];
VAR
  rcls : rscls;

BEGIN                      { read plot defaults from file }
 { initialize constants}
      energy    :=   1;
      momentum  :=   2;
      beta      :=   3;
      theta     :=   4;
      costheta  :=   5;
      ptrans    :=   6;
      kinetic   :=   7;
      angdiff   :=   8;
      invmass   :=   9;
      cm1       :=   1;
      cm2       :=   2;
      lab1      :=   3;
      lab2      :=   4;


  rcls[1] := 'CM1 ';
  rcls[2] := 'CM2 ';
  rcls[3] := 'Lb1 ';
  rcls[4] := 'Lb2 ';

 Assign(F,'plot.dat');
 {$I-} Reset(F); {$I+}
 if IOResult<>0 then begin
   Announce('Input file PLOT.DAT not found in current directory');
   CUPSdone; halt(1);
 end;
 readmess:= 'Plot data read';

 WHILE not Eof(F) do
 BEGIN
   Readln(F, rname, rncls, rnum, rmin, rmax, rdiv);
  {Writeln(rname:12, rncls:4, rnum:4, rmin:12:4, rmax:12:4, rdiv:12:4);}
   IF (rnum >= 1) and (rnum <= 9) and (rncls >= 1) and (rncls <= 4) THEN
   BEGIN
     With vcons[rnum,rncls] do
     BEGIN
       num:=  rnum;
       name:= rname;
       ncls:= rcls[rncls];
       vmin:= rmin;
       vmax:= rmax;
       vdiv:= rdiv;
       val := 0;
{     Writeln(num:4, vmin:12:1, vmax:12:1, vdiv:12:1, name:12, ncls:12);}
     END;  {vcons}
   END
   ELSE
   BEGIN
       readmess:='plot data error';
   END;   {if}
  END;

   {Message(readmess);}
   splot.init(inum1,iclass1,inum2,iclass2);

END;

    PROCEDURE splot.getscl(VAR   ixlow,ixhigh,iylow,iyhigh: Real);
BEGIN
    ixlow :=vcons[vnum1,vclass1].vmin;
    ixhigh:=vcons[vnum1,vclass1].vmax;
    iylow :=vcons[vnum2,vclass2].vmin;
    iyhigh:=vcons[vnum2,vclass2].vmax;
END;

    PROCEDURE splot.rescl( ixlow,ixhigh,iylow,iyhigh: Real);
BEGIN
    vcons[vnum1,vclass1].vmin := ixlow;
    vcons[vnum1,vclass1].vmax := ixhigh;
    vcons[vnum2,vclass2].vmin := iylow;
    vcons[vnum2,vclass2].vmax := iyhigh;
END;

    PROCEDURE splot.plot;                { plot scatterplot }

BEGIN


    vval1:= vcons[vnum1,vclass1].val;
    vval2:= vcons[vnum2,vclass2].val;


    SelectScale(12);
    SetColor(yellow);
    xxx.put(1,vval1);
    yyy.put(1,vval2);
    SelectViewPort(2);
    PlotPoints(xxx,yyy,'o');
    SetColor(whitte);
END;


    PROCEDURE splot.clearplot;
BEGIN
    OpenViewPort(2);
    DefineScale(12,vmin1,vmax1,vmin2,vmax2);
    SelectScale(12);
    Axis(0.0, 0.0, vdiv1, vdiv2);
 {   OUTTextxy(2,2,CONCAT(vname1,' ',vcls1,'  ',vname2,' ',vcls2)); }
    OUTTextxy(42, 2,CONCAT('x=',vname1,' ',vcls1));
    OUTTextxy(42,12,CONCAT('y=',vname2,' ',vcls2));

END;




    PROCEDURE hplot.init(inum1,iclass1: Integer);

VAR
  i : Integer;
BEGIN


    setcolor(whitte);

    vnum1:= inum1; vclass1:= iclass1;

    vname1:= vcons[inum1,iclass1].name;

    vcls1:= vcons[inum1,iclass1].ncls;

    vmin1:= vcons[inum1,iclass1].vmin;

    vmax1:= vcons[inum1,iclass1].vmax;

    vdiv1:= 5;
    vmin2:= 0; vmax2:= 50; vdiv2:= 10;
    vbins:= 20;

     vdiv1:= (vmax1-vmin1)*0.25;
     IF(vdiv1 < macheps) THEN vdiv1:=10;

    xh.init(vbins);
    yh.init(vbins);

    OpenViewPort(3);
    SelectViewport(3);

   { DefineScale(19,0,vbins,vmin2,vmax2);}
    DefineScale(19,vmin1,vmax1,vmin2,vmax2);
    SelectScale(19);
    Axis(0.0, 0.0, vdiv1, vdiv2);
    OUTTextxy(2,2,CONCAT(vname1,' ',vcls1));
    FOR i := 1 TO vbins DO
    BEGIN
       hist[i] := 0;
       hisx[i] := vmin1 + i*(vmax1-vmin1)/vbins;
    END;

    SetColor(LightCyan);
    OutTextxy(1,14,'#Events');
    SetColor(Whitte);
END;

    PROCEDURE hplot.firstinit(inum1,iclass1: Integer);
BEGIN
   {    setcolor(darkgray);
       axis(0.0, 0.0, vdiv1, vdiv2);
       setColor(whitte);
    }
       hplot.init(inum1,iclass1);
END;

    PROCEDURE hplot.fill;         { fill histogram array }
VAR
    ibin : Integer;
    hval : Real;
BEGIN
    hval := vcons[vnum1,vclass1].val;
    ibin:= Round(vbins*(hval-vmin1)/(vmax1-vmin1));
    IF (ibin < 1) THEN ibin:= 1;
    IF (ibin > vbins) THEN ibin:= vbins;
    hist[ibin] := hist[ibin] + 1;
END;


    PROCEDURE hplot.plot(evtno: Integer);      { plot histogram }
VAR
    i   : Integer;
    ix  : Real;
BEGIN
    SelectViewPort(3);
    SelectScale(19);
    FOR i:= 1 to vbins DO
    BEGIN
       if 5*GraphScales[19].ymax<6*hist[i] then resize(evtno);
       xh.put(i,hisx[i]);
       yh.put(i,hist[i]);
    END;
    SETCOLOR(yellow);
    PlotPoints(xh,yh,'o');

    SetColor(lightgreen);
    RubOut(11, 1, 4, Graphbackcolor);
    print (11, 1, CONCAT(numStr(evtno,4,0),' events'));
    SetColor(whitte);
    SelectViewPort(2);
END;

    PROCEDURE hplot.resize(evtno: Integer);      { plot histogram }
VAR
    i,j,max,size  : Integer;
    ix  : Real;
BEGIN
    DrawAxis(0.0, 0.0, 0, 0,black);
    OpenViewPort(3);
    GraphScales[19].ymax := 2*GraphScales[19].ymax;
    Axis(0.0, 0.0, 0, 0);
    SETCOLOR(yellow);
    FOR i:= 1 to vbins DO
     for j := 1 to round(hist[i]) do
      PlotSymbol(hisx[i],j,'o');
    OUTTextxy(2,2,CONCAT(vname1,' ',vcls1));
    SetColor(LightCyan);
    OutTextxy(1,14,'#Events');
    SetColor(lightgreen);
    RubOut(11, 1, 4, Graphbackcolor);
    print (11, 1, CONCAT(numStr(evtno,4,0),' events'));
    SetColor(whitte);
END;

    PROCEDURE hplot.clearplot;
VAR
  i  : Integer;
BEGIN
    OpenViewPort(3);
    SelectViewPort(3);
{    DefineScale(19,0,vbins,vmin2,vmax2); }
    DefineScale(19,vmin1,vmax1,vmin2,vmax2);
    SelectScale(19);
    Axis(0.0, 0.0, vdiv1, vdiv2);
    OUTTextxy(2,2,CONCAT(vname1,' ',vcls1));
    FOR i := 1 TO vbins DO
    BEGIN
     hist[i] := 0;
    END;
    SetColor(LightCyan);
    OutTextxy(1,14,'#Events');
    SetColor(Whitte);

END;


END.
