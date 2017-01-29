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
                      {*       TRFITMEN       *}
                      {*                      *}
                      {* Example of fitting   *}
                      {*   histograms and     *}
                      {*   random functions   *}
                      {*       input params   *}
                      {*         J. Rothberg  *}
                      {*       10 Jun. 1994   *}
                      {************************}

PROGRAM TRANDH;     { Fit random number distributions; plot histograms }

USES crt, Graph, CUPSmupp, cups, CUPSgui, dethist, relutil;

CONST
          { input screen location }
       inpx1 : Real = 0.54;
       inpx2 : Real = 0.96;
       inpy1 : Real = 0.87;
       inpy2 : Real = 0.98;

VAR
        hist1,hist2  : hplot;
TYPE
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
       LoadLine('      [  Ok  ]  [ Quit ] ');
       SetNumber(1, centerp);
       SetNumber(2, sigmap);

       SetNumberLimits(1, 100, 200);
       SetNumberLimits(2, 0.1,  40);

       AcceptScreen;
       qquit   := GetBoolean(4);
       centerp := GetNumber(1);
       sigmap  := GetNumber(2);
       done;
       if qquit then begin CupsDone; halt; end;
end;
END;

        PROCEDURE recon(center,sigma: Real);
VAR
      value   : Real;
const
      eventno : Integer = 0;
BEGIN
      eventno := eventno + 1;

   {evaluate random functions and update histograms}

      value   := Rand_gauss(center, sigma);
      hist1.fill(value);

      value   := Breitwig(center,2*sigma);
      hist2.fill(value);

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
        hotKeys        : THotKeys;
        key            : byte;
CONST   maxevents      : Integer = 500;
BEGIN
        cupsinit;

        DefineViewPort(7, 0.06, 0.48, 0.70, 0.85);
        OpenViewPort(7);
        DefineViewPort(8, 0.54, 0.96, 0.70, 0.85);
        OpenViewPort(8);

   { Ports for two histograms}

        hist1.OpenPort(3, 0.06, 0.48, 0.08, 0.68 );
        hist2.OpenPort(4, 0.54, 0.96, 0.08, 0.68 );

   { define and initialize two histograms}
        hmin := 100;
        hmax := 200;
        hname := 'Gaussian';
        hist1.init(hmin,hmax,3,hname);

        hmin := 100;
        hmax := 200;
        hname := 'BreitWig';
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

            functofit := bwfunc;
            hist2.fitfunc(center,sigma,functofit);     { fit the function }
          END;
       END;
       CUPSDONE;
END.

