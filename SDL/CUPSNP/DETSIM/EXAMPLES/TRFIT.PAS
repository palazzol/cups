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
                      {*       TRFIT          *}
                      {*                      *}
                      {* Example of fitting   *}
                      {*   histograms and     *}
                      {*   random functions   *}
                      {*         J. Rothberg  *}
                      {*        9 Jun 1994    *}
                      {*                      *}
                      {************************}

PROGRAM TRANDH;     { Fit random number distributions; plot histograms }

USES crt, Graph, cupsmupp, cupsgui, cups, cupsgrph, dethist, relutil;

VAR
        hist1,hist2  : hplot;
        HotKeys : THotKeys;
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

        PROCEDURE recon(center,sigma: Real);
VAR
      value   : Real;
CONST
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
  { ------------------------------------ fit functions ----------}
VAR
        hmin, hmax  : Real;
        hname       : string[8];

VAR
        center, sigma  : Real;
        i,j            : Integer;
        functofit      : Functionxyz;
        key : byte;

CONST   maxevents      : Integer = 500;
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

        center := 150;
        sigma  :=  10;

        functofit := gaussfunc;
        hist1.fitfunc(center,sigma,functofit);     { fit the function }

        functofit := bwfunc;
        hist2.fitfunc(center,sigma,functofit);     { fit the function }

   { wait for key }
        PressKeyMessage;
END.

