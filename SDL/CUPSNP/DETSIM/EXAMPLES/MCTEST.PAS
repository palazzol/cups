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
                      {*       MCTEST         *}
                      {*                      *}
                      {* Example for testing  *}
                      {*   distributions of   *}
                      {*   random functions   *}
                      {*                      *}
                      {*         J. Rothberg  *}
                      {*        9 Jun. 1994   *}
                      {*                      *}
                      {************************}

PROGRAM MCTEST;     { Test random number distributions; plot histograms }

USES crt, Graph, CUPSmupp, cups, CUPSgui, dethist, relutil;

CONST
          { input screen location }
       inpx1 : Real = 0.56;
       inpx2 : Real = 1.00;
       inpy1 : Real = 0.87;
       inpy2 : Real = 0.98;

VAR
        hist1,hist2  : hplot;
        HotKeys : THotKeys;
Type
        Functionxyz = function(x,y,z: Real): Real;

{ ---------------------------------------------------------------}

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
{-------------------------------------------------------------------}
        FUNCTION sumran(numran: Integer): Real; far;
VAR
        i     : Integer;
        sum   : Real;
BEGIN
        sum := 0.0;
        FOR i := 1 TO numran DO
        BEGIN
           sum := sum + Random;
        END;
        sumran := sum;
END;
{ -------------------------------------------------------------------}
        PROCEDURE readparam(VAR centerp, sigmap : Real);
                                                 { input fit parameters }
VAR
        paramScreen   : TInputScreen;
        qquit : boolean;
BEGIN
with paramScreen do
begin
       init;
       DefineInputPort(inpx1,inpx2,inpy1,inpy2);
       LoadLine('Center={      }  Sigma={      } ');
       LoadLine('     [  Ok  ]  [ Quit ] ');
       SetNumber(1, centerp);
       SetNumber(2, sigmap);
       AcceptScreen;
       qquit   := GetBoolean(4);
       centerp := GetNumber(1);
       sigmap  := GetNumber(2);
       done;
       IF qquit THEN CUPSDone;
end;
END;

        PROCEDURE recon(center,sigma: Real);
VAR
      numran  : Integer;
      value   : Real;
const
      eventno : Integer = 0;
BEGIN
      eventno := eventno + 1;

   {evaluate random functions and update histograms}

      numran := 12;
      value := sumran(numran);
      value := value - 6.0;
      value := value*sigma;
      value := value + center;
      hist1.fill(value);

      value   := Rand_gauss(center, sigma);
      hist2.fill(value);


   {   value   := Breitwig(center,2*sigma);   }
   {   hist2.fill(value);                      }

   {    value   := exponen(center);  }

   { plot updated histograms }

      hist1.plot(eventno);
      hist2.plot(eventno);

END;

VAR
        hmin, hmax  : Real;
        hname       : string[8];

VAR
        center, sigma  : Real;
        i,j            : Integer;
        functofit      : Functionxyz;
        key : byte;

CONST   maxevents      : Integer = 1000;

BEGIN
        cupsinit;

        DefineViewPort(8, 0.56, 1.00, 0.70, 0.85);
        OpenViewPort(8);
        DefineViewPort(7, 0.06, 0.50, 0.70, 0.85);
        OpenViewPort(7);

   { Ports for two histograms}

        hist1.OpenPort(3, 0.06, 0.50, 0.08, 0.68 );
        hist2.OpenPort(4, 0.56, 1.00, 0.08, 0.68 );

   { define and initialize two histograms}
        hmin := 100;
        hmax := 200;
        hname := 'Sum';
        hist1.init(hmin,hmax,3,hname);

        hmin := 100;
        hmax := 200;
        hname := 'Gaussian';
        hist2.init(hmin,hmax,4,hname);

   { define parameters of random functions }
        center := 150;
        sigma  := 10;

        i := 1;
        HotKeys.init(1);
        HotKeys.key[1] := 'Esc-Stop';
        HotKeys.Display;
{       FOR i:= 1 TO maxevents DO }
        repeat
              checkForEvents;
              FOR j:= 1 TO 10 do recon(center,sigma);
              i := i + 10;
        until (i=maxEvents) or HotKeys.Pressed(key);
        HotKeys.Clear;

        hist1.refresh;
        hist2.refresh;
        hist1.hout;
        hist2.hout;

        center := 150;
        sigma  :=  10;

        WHILE (sigma > 0) DO
        BEGIN
          readparam(center,sigma);
          IF (sigma > 0) THEN
          BEGIN
            functofit := gaussfunc;
            hist1.fitfunc(center,sigma,functofit);     { fit the function }

            functofit := gaussfunc;
            hist2.fitfunc(center,sigma,functofit);     { fit the function }
          END;
       END;

END.

