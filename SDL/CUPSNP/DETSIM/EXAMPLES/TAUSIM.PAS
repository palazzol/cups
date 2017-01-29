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
                      {*       TAUSIM         *}
                      {*                      *}
                      {* Particle decay,      *}
                      {*  Impact parameter    *}
                      {*                      *}
                      {*                      *}
                      {*         J. Rothberg  *}
                      {*       9 June 1994    *}
                      {************************}

PROGRAM TAUSIM;     { Simulate Decay, impact parameter }

USES crt, Graph, CUPSmupp, CUPS, CUPSgui, CUPSfunc, dethist, relutil;

CONST
                    { detector planes location }
        detloc1 = 18.0;
        detloc2 = 19.0;
        wht     = 0.10;  { drawn size of detector hit }

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

        FUNCTION expfunc(lifetime,shift,xval: Real): Real; far;
VAR                                  { decaying exponential }
        temp   : Real;
BEGIN
        temp := (xval-shift)/lifetime;
        IF( temp > macheps) THEN expfunc := exp(-temp) ELSE expfunc:=1;
END;

{ -------------------------------------------------------------------}
        PROCEDURE readparam(VAR events, centerp, sigmap, psiang : Real);
                                { input event and fit parameters }
VAR
        paramScreen   : TInputScreen;
        qquit         : boolean;
BEGIN
with paramScreen do
begin
       init;
       DefineInputPort(0.56, 1, 0.52, 0.8);
       LoadLine('number of events= {       }');
       LoadLine('Lifetime        = {       }');
       LoadLine('Shift           = {       } ');
       LoadLine('Decay Angle     = {       }');
       LoadLine('');
       LoadLine('      [  Ok  ]  [ Quit ] ');
       SetNumber(1, events);
       SetNumber(2, centerp);
       SetNumber(3, sigmap);
       SetNumber(4, psiang);
       SetNumberLimits(1,1,10000);
       SetNumberLimits(2,0.1,100);
       SetNumberLimits(3,-10,10);
       SetNumberLimits(4,0.001,0.75);
       AcceptScreen;
       qquit   := GetBoolean(6);
       events  := GetNumber(1);
       centerp := GetNumber(2);
       sigmap  := GetNumber(3);
       psiang  := GetNumber(4);
       done;
       IF qquit THEN CUPSDone;

end;
END;

        PROCEDURE recon(lifetime,shift,psi: Real);
VAR
           { "true" values }
      inlifetime       : Real;        { generated lifetime }
      decaypt          : Real;          {decay point, x}
      yhit1,yhit2      : Real;          { y at detectors }
      ximpact,yimpact  : Real;    { x ,y coordinates of impact param end}
      impactpar        : Real;          { impact parameter}
      tanpsi,sinpsi,cospsi  : Real;

           { reconstructed values }
      rx, ry             : Real;
      rpsi               : Real;   { decay angle }
      rslope             : Real;   { slope of secondary track }
      rsinpsi ,  rcospsi : Real;
      rdecayp            : Real;   { decay point }
      rximp   ,  ryimp   : Real;   { impact parameter x, y }
      rimpact            : Real;   { impact parameter, magnitude }


const
      eventno : Integer = 0;
BEGIN
      eventno := eventno + 1;

           { true decay angle }
      tanpsi:= Tan(psi);
      sinpsi:= Sin(psi);
      cospsi:= Cos(psi);

      inlifetime := lifetime;          { lifetime generated}


   {select decay point from exponential, calculate true impact parameter }

        decaypt   := exponen(inlifetime);
        ximpact   := decaypt*sinpsi*sinpsi;
        yimpact   := -decaypt*sinpsi*cospsi;
        impactpar := Sqrt(ximpact*ximpact+yimpact*yimpact);

           { hits at detector planes }
        yhit1 := (detloc1-decaypt)*tanpsi;
        yhit2 := (detloc2-decaypt)*tanpsi;

           { reconstructed decay point and impact parameter }
        rx      := detloc2-detloc1;
        ry      := yhit2-yhit1;
        rpsi    := ArcTan2(rx,ry);
        rslope  := Tan(rpsi);
        rsinpsi := Sin(rpsi);
        rcospsi := Cos(rpsi);
        rdecayp := detloc1-yhit1/rslope ;
        rximp   :=  rdecayp*rsinpsi*rsinpsi;
        ryimp   := -rdecayp*rsinpsi*rcospsi;
        rimpact := Sqrt(rximp*rximp + ryimp*ryimp);

            { fill histograms }
{
        hist2.fill(impactpar);
        hist1.fill(decaypt);
}
        hist2.fill(rimpact);
        hist1.fill(rdecayp);

            { plot updated histograms }
        hist1.plot(eventno);
        hist2.plot(eventno);

 { ------------------------------------------------}
        { plot track for first few events }
      IF(eventno < 8) THEN
      BEGIN
        SelectViewport(5);
        SelectScale(5);

             {  detector }
        SetColor(lightred);
        PlotLine(detloc1,-7,detloc1,7);
        PlotLine(detloc2,-7,detloc2,7);

             { secondary track }
        SetColor(lightblue);
        PlotLine(decaypt,0,detloc2,yhit2);

             { hit at detector }
        SetColor(yellow);
        PlotRect(detloc1-wht,yhit1-wht,detloc1+wht,yhit1+wht);
        PlotRect(detloc2-wht,yhit2-wht,detloc2+wht,yhit2+wht);

             { impact parameter line }
        SetColor (lightgreen);
        PlotLine(0,0,ximpact,yimpact);

             { extension of secondary track }
        SetColor (lightmagenta);
        PlotLine(ximpact,yimpact,decaypt,0);

        SetColor(white);
      END;
END;

{---------------------------------------------------------}
VAR
        hmin, hmax  : Real;
        hname       : string[8];

VAR
        lifetime, shift,psiang  : Real;
        events : Real;
        i,j            : Integer;
        functofit      : Functionxyz;
        Key            : byte;
        HotKeys        : THotKeys;
        maxevents      : Integer;
BEGIN
        cupsinit;
        lifetime :=    5.0;
        shift    :=      0;
        events   :=    400;
        psiang   :=    0.3;
        readparam(events,lifetime,shift,psiang);
        maxevents := Trunc(events);

            { results screens}
        DefineViewPort(8, 0.56, 1.00, 0.82, 0.97);
        OpenViewPort(8);
        DefineViewPort(7, 0.05, 0.50, 0.82, 0.97);
        OpenViewPort(7);
             { decay simulation window}
{       DefineViewPort(5, 0.03, 0.53, 0.50, 0.80);}
        DefineViewPort(5, 0.05, 0.50, 0.52, 0.80);
        OpenViewPort(5);
        DefineScale(5, -5.0, 20.0, -7.5, 7.5);

   { Ports for two histograms}

        hist1.OpenPort(3, 0.05, 0.50, 0.09, 0.49 );
        hist2.OpenPort(4, 0.56, 1.00, 0.09, 0.49 );

            { decay region }
        SelectViewport(5);
        SelectScale(5);
        SetColor(white);
        PlotLine(-4, 0, 19.5, 0);
        PlotLine(0, -0.3, 0, 0.3);

   { define and initialize two histograms}
        hmin := 0;
        hmax := 25;
        hname := 'Decay_Pt';
        hist1.init(hmin,hmax,3,hname);

        hmin := 0;
        hmax := 10;
        hname := 'Impact_Par';
        hist2.init(hmin,hmax,4,hname);

   { Event loop }

        i := 1;
        HotKeys.init(1);
        HotKeys.key[1] := 'Esc-Stop';
        HotKeys.Display;
{       FOR i:= 1 TO maxevents DO }
        repeat
              checkForEvents;
              FOR j:= 1 TO 10 do recon(lifetime,shift,psiang);;
              i := i + 10;
        until (i=maxEvents) or HotKeys.Pressed(key);
        HotKeys.Clear;

        hist1.refresh;
        hist2.refresh;
        hist1.hout;
        hist2.hout;

               { fit exponential function to data }
        WHILE (lifetime > 0) DO
        BEGIN
            readparam(events,lifetime,shift,psiang);

            functofit := expfunc;
            hist1.fitfunc(lifetime,shift,functofit);

            functofit := expfunc;
            hist2.fitfunc(lifetime,shift,functofit);

       END;
END.

