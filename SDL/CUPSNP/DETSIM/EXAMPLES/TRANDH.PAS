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
                      {*       TRANDH         *}
                      {*                      *}
                      {* Example of using     *}
                      {*   histograms and     *}
                      {*   random functions   *}
                      {*         J. Rothberg  *}
                      {*       9 Jun 1994     *}
                      {*                      *}
                      {************************}

PROGRAM TRANDH;     { Test random number distributions; plot histograms }

USES crt, Graph, cupsmupp, cupsgui, cups, cupsgrph, dethist, relutil;

VAR
        hist1,hist2  : hplot;


        procedure recon(center,sigma: Real);
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

VAR
        hmin, hmax  : Real;
        hname       : string[8];

VAR
        center, sigma  : Real;
        i,j            : Integer;
        HotKeys        : THotKeys;
        key            : byte;
CONST   maxevents      : Integer = 300;
BEGIN
        cupsinit;

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
   { wait for key }
        PressKeyMessage;
END.

