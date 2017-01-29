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

                      {*****************************}
                      {*                           *}
                      {*       RECONFIT            *}
                      {*                           *}
                      {*   Example of fitting      *}
                      {*      histograms to        *}
                      {*        RELDET data.       *}
                      {*    use input param screen *}
                      {*                           *}
                      {*          J. Rothberg      *}
                      {*          9 June 1994      *}
                      {*****************************}

PROGRAM RECONFIT;     { Fit RELDET output data; plot histograms }

USES crt, Graph, cupsmupp, cups, cupsgui, cupsgrph, dethist, relutil;

CONST
        raddeg  = pi/180;
        cpi     = pi;
        InFileName : String[10] = 'RELDET.OUT';      {input data file}

CONST      { input parameter screen }
       inpx1 : Real = 0.05;
       inpx2 : Real = 0.95;
       inpy1 : Real = 0.87;
       inpy2 : Real = 0.98;

TYPE
       Functionxyz = function(x,y,z: Real): Real;
       String1 = String[1];

VAR
        hist1,hist2  : hplot;
        statq       : boolean;
{ ---------------------------------------------------------------}
           { function definitions }

        FUNCTION gaussfunc(center,sigma,xval: Real): Real; far;
VAR                                     { Gaussian }
        temp, sig2   : Real;
BEGIN
        sig2 := 2*sigma*sigma;
        temp := sqr(xval-center)/sig2;
        gaussfunc := exp(-temp);
END;

        FUNCTION bwfunc(center,sigma,xval: Real): Real; far;
VAR                                     { Breit-Wigner }
        temp, sig2  : Real;
BEGIN
        sig2 := sigma*sigma;
        temp := sqr(xval-center) + sig2;
        bwfunc := sig2/temp;
END;

        FUNCTION expfunc(lifetime,shift,xval: Real): Real; far;
VAR                                  { decaying exponential }
        temp   : Real;
BEGIN
        temp := (xval-shift)/lifetime;
        IF( temp > macheps) THEN expfunc := exp(-temp) ELSE expfunc:=1;
END;


{ -------------------------------------------------------------------}
        PROCEDURE readparam(VAR center1, sigma1, center2, sigma2 : Real);
                                      { input fit parameters }
VAR
        paramScreen   : TInputScreen;
BEGIN
  WITH paramScreen DO
  BEGIN
    init;
    DefineInputPort(inpx1,inpx2,inpy1,inpy2);
    LoadLine(' center {     }  sigma {      } '+
        '        center {      } sigma {     } ');
    LoadLine(' [  Ok  ] [Cancel] [Quit] ');
    SetNumber(1,center1);
    SetNumber(2,sigma1);
    SetNumber(3,center2);
    SetNumber(4,sigma2);
    SetNumberLimits(1, 100, 200);
    SetNumberLimits(2, 0.1,  40);
    SetNumberLimits(3, 3500, 4500);
    SetNumberLimits(4, 0.1,   200);

    AcceptScreen;
    IF NOT (canceled) THEN BEGIN
       center1 := GetNumber(1);
       sigma1  := GetNumber(2);
       center2 := GetNumber(3);
       sigma2  := GetNumber(4);
       statq   := GetBoolean(7);
       IF(statq) THEN CUPSdone ;
    END;
  END;
END;

{ --------------------------------------------------------------}
    { reconstruct events }
        PROCEDURE recon(x1,y1,e1,x2,y2,e2,detz1: real);
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
        { reconstruct invariant mass and energy }
      r1 := Sqrt( x1*x1 + y1*y1 );
      r2 := Sqrt( x2*x2 + y2*y2 );
      thet1:= Arctan(r1/ detz1);
      thet2:= Arctan(r2/ detz1);
      thet := thet1 + thet2;
      angsep:= thet/raddeg;
      sinth:= Sin(thet/2);
      invmass:= 2*Sqrt(e1*e2)*sinth;
      energy:=e1+e2;

    { update and plot histograms }
      hist1.fill(invmass);
      hist2.fill(energy);
      hist1.plot(eventno);
      hist2.plot(eventno);

   {  writeln(energy:8:2,sinth:8:4,invmass:8:2);
      readln;
   }
END;

{ ------------- Read data, reconstruct, plot, fit --------------------}
VAR
        hmin1, hmax1  : Real;
        hmin2, hmax2  : Real;
        hname       : String[8];
VAR
        x1,y1,e1,x2,y2,e2,detz1,spare  : Real;
        infile      : Text;
VAR
        center1, sigma1  : Real;
        center2, sigma2  : Real;
        i                : Integer;
        functofit        : Functionxyz;

BEGIN
        cupsinit;

    { input event data file }
        ASSIGN(infile,InFileName);
        RESET(infile);
  { Ports for fit results  }
        DefineViewPort(7, 0.06, 0.48, 0.70, 0.85);
        OpenViewPort(7);
        DefineViewPort(8, 0.54, 0.98, 0.70, 0.85);
        OpenViewPort(8);

  { Ports for two histograms}

        hist1.OpenPort(3, 0.06, 0.48, 0.08, 0.68 );
        hist2.OpenPort(4, 0.54, 0.98, 0.08, 0.68 );


    { define histograms }
        hmin1 := 100;
        hmax1 := 200;
        hname := 'Inv_mass';
        hist1.init(hmin1,hmax1,3,hname);
        hmin2 := 3500;
        hmax2 := 4500;
        hname := 'Energy';
        hist2.init(hmin2,hmax2,4,hname);

   { Read data and reconstruct events }

        WHILE not Eof(infile) DO
        BEGIN
           Readln(infile,x1,y1,e1,x2,y2,e2,detz1,spare);
           IF(e1 > 0 ) and (e2 > 0) THEN         { two hits, valid energy }
              recon(x1,y1,e1,x2,y2,e2,detz1);
        END;

  { replot }
        hist1.refresh;
        hist2.refresh;
        hist1.hout;
        hist2.hout;

  { starting fit parameters }
        center1 := 140;
        sigma1  :=  10;
        center2 := 4000;
        sigma2  :=  50;

  { get parameters and fit histograms }
        REPEAT
          readparam(center1,sigma1,center2,sigma2);
            functofit := gaussfunc;
            hist1.fitfunc(center1,sigma1,functofit);    { fit the function }

            functofit := gaussfunc;
            hist2.fitfunc(center2,sigma2,functofit);    { fit the function }

        UNTIL( false );

END.

