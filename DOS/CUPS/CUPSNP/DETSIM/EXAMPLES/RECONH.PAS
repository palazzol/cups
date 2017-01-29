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

                      {************************}
                      {*                      *}
                      {*       RECONH         *}
                      {* Reconstruct Events   *}
                      {*                      *}
                      {*       9 June 1994    *}
                      {*                      *}
                      {************************}

PROGRAM RECONH;     { Reconstruct events, plot histograms of variables}

USES crt, Graph, cupsmupp, cups, cupsgrph, dethist;

CONST   maxinp = 8;
        raddeg  = pi/180;
        cpi     = pi;
        InFileName : String[10] = 'RELDET.OUT';      {input file}
VAR
        hist1,hist2  : hplot;


    { reconstruct events }
        procedure recon(x1,y1,e1,x2,y2,e2,detz1: real);
VAR
     r1,r2    : Real;
     thet1,thet2,thet : Real;
     angsep   : Real;     {angular separation}
     invmass  : Real;
     sinth    : Real;
     energy   : real;

CONST
     eventno : Integer =0;
BEGIN
      eventno:= eventno+1;
      r1 := Sqrt( x1*x1 + y1*y1 );
      r2 := Sqrt( x2*x2 + y2*y2 );
      thet1:= arctan(r1/ detz1);
      thet2:= arctan(r2/ detz1);
      thet := thet1 + thet2;
      angsep:= thet/raddeg;
      sinth:= sin(thet/2);
      invmass:= 2*Sqrt(e1*e2)*sinth;
      energy:=e1+e2;

    { update and plot histograms }
      hist1.fill(invmass);
      hist2.fill(angsep);
      hist1.plot(eventno);
      hist2.plot(eventno);

   {  writeln(energy:8:2,sinth:8:4,invmass:8:2);
      readln;
   }
END;

VAR
        x1,y1,e1,x2,y2,e2,detz1,spare  : Real;
TYPE
        inarr = array[1..maxinp] of Real;
VAR
        infile  : Text;
        indata  : inarr;

        hmin, hmax  : Real;
        hname       : string[8];

BEGIN
        cupsinit;
    { viewports for histograms }
        hist1.OpenPort(3, 0.06, 0.50, 0.08, 0.68 );
        hist2.OpenPort(4, 0.56, 1.00, 0.08, 0.68 );


    { input event data file }
        ASSIGN(infile,InFileName);
        RESET(infile);
    { define histograms }
        hmin := 100;
        hmax := 200;
        hname := 'Inv mass';
        hist1.init(hmin,hmax,3,hname);
        hmin := 0;
        hmax := 30;
        hname := 'open ang';
        hist2.init(hmin,hmax,4,hname);

   { Read data and reconstruct events }

        WHILE not Eof(infile) DO
        BEGIN
           Readln(infile,x1,y1,e1,x2,y2,e2,detz1,spare);
           IF(e1 > 0 ) and (e2 > 0) THEN            { valid energy }
              recon(x1,y1,e1,x2,y2,e2,detz1);
        END;
        hist1.refresh;
        hist2.refresh;
        Readln;
END.

