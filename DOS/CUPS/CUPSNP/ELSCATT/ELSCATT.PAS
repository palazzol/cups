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

                    {*********************************}
                    {*  Electron Scattering Program  *}
                    {*   Written by John Philpott    *}
                    {*    Version of June 1, 1994    *}
                    {*       Copyright @ 1994        *}
                    {*********************************}

PROGRAM ElScatt;
{$N+}
USES
   crt, graph, CUPSmupp, CUPS, CUPSfunc, CUPSproc,
   CUPSgui, CUPSgrph, ElScSupp;


{ ----------------------- Begin KINEMATICS Procedure ---------------------- }

function InViewPort( n : integer) : boolean;
{ Is mouse in ViewPort n ?}
begin
   with event do with Views[n] do
   InViewPort := (x<=vx2) and (x>=vx1) and (y<=vy2) and (y>=vy1);
end;

VAR
   KinProjectileSelector : Integer;
   KinTargetSelector     : Integer;
   KinNpoints            : Integer;
   Kinpsubi              : Float;

PROCEDURE Kinematics;

VAR
   ProjectileSelector : Integer;
   TargetSelector     : Integer;
   Npoints            : Integer;
   psubi              : Float;

PROCEDURE RestoreKinematicsConstants;
BEGIN
   ProjectileSelector := KinProjectileSelector;
   TargetSelector := KinTargetSelector;
   Npoints := KinNpoints;
   psubi := Kinpsubi;
END;

PROCEDURE SaveKinematicsConstants;
BEGIN
   KinProjectileSelector := ProjectileSelector;
   KinTargetSelector := TargetSelector;
   KinNpoints := Npoints;
   Kinpsubi := psubi;
END;

VAR
   Escape           : Boolean;
   ProjRestEnergy   : Float;
   TargetRestEnergy : Float;
   RecoilEnergy     : Float;
   RecoilRestEnergy : Float;
   esubi,esubf      : Float;
   psubf,theta      : Float;
   hbarOmega        : Float;
   q,omega          : Float;
   CapQsq,nu        : Float;

PROCEDURE Initialize;
BEGIN { Initialize (Kinematics) }
   ProjectileSelector := 1;
   TargetSelector     := 16;
   Npoints            := 50;
   psubi              := 300;
END;  { Initialize (Kinematics) }

PROCEDURE MousePosn(VAR X,Y:Float; VNum,SNum:integer; VAR button: integer);
{ Adapted from MUPPET's MousePosn Procedure }
VAR
   XFactor, YFactor : Float;
   xGlobal, yGlobal : integer;
BEGIN
   MouseGlobalPosn(xGlobal,yGlobal,button);
   WITH Views[VNum] DO
   WITH GraphScales[SNum] DO
   BEGIN
      xFactor := (xmax-xmin)/(Vx2-Vx1);
      X := (xFactor*(xGlobal-Vx1)) + xmin;
      yFactor := (ymax-ymin)/(Vy2-Vy1);
      Y := -(yFactor*(yGlobal-Vy1)) + ymax;
   END;
END;

FUNCTION PfxRebound: Float;
{ Calculate the forward component of psubf following a head-on
  elastic scattering collision (Maximum momentum transfer) }
{ Uses: TargetRestEnergy, ProjRestEnergy, psubi, esubi }
VAR
  CMenergy, pcm, ecm, beta, gamma : Float;
BEGIN
  CMenergy := sqrt(sqr(TargetRestEnergy) + sqr(ProjRestEnergy)
                           + 2*TargetRestEnergy*esubi);
  pcm   := TargetRestEnergy*psubi/CMenergy;
  ecm   := (TargetRestEnergy*esubi + sqr(ProjRestEnergy))/CMenergy;
  beta  := psubi/(TargetRestEnergy + esubi);
  gamma := (TargetRestEnergy + esubi)/CMenergy;
  PfxRebound := gamma*(beta*ecm - pcm);
END;

PROCEDURE FindLorentzScalars(psubf, theta: Float; VAR CapQsq, nu: Float);
{ Calculate CapQsq, nu from given values of psubf, theta }
{ Inputs are: ProjRestEnergy, psubi, esubi }
VAR
   q,esubf : Float;
BEGIN
   q := sqrt(sqr(psubi)-2*psubi*psubf*cos(theta)+sqr(psubf));
   esubf := sqrt(sqr(psubf) + sqr(ProjRestEnergy));
   nu := esubi - esubf;
   CapQsq := sqr(q) - sqr(nu);
END;

PROCEDURE FindTransferParams(psubf, theta: Float; VAR hbarOmega, q: Float);
{ Calculate hbarOmega, q from given values of psubf, theta }
{ Inputs are: TargetRestEnergy, ProjRestEnergy, psubi, esubi }
VAR
   esubf, RecoilEnergy, RecoilRestEnergy : Float;
BEGIN
   q := sqrt(sqr(psubi)-2*psubi*psubf*cos(theta)+sqr(psubf));
   esubf := sqrt(sqr(psubf) + sqr(ProjRestEnergy));
   RecoilEnergy := TargetRestEnergy + esubi - esubf;
   IF RecoilEnergy - q > 0 THEN
      RecoilRestEnergy := sqrt((RecoilEnergy + q)*(RecoilEnergy - q))
   ELSE RecoilRestEnergy := 0;
   hbarOmega := RecoilRestEnergy - TargetRestEnergy;
END;

PROCEDURE MakeFinalMom(esubf, q: Float; VAR psubf, theta: Float);
{ Calculate psubf, theta from given values of esubf, q.
  (Include possibility that the arguments may be slightly
  beyond their allowed ranges.) }
VAR
   arg : Float;
BEGIN
   IF esubf > ProjRestEnergy THEN
    BEGIN
       psubf := sqrt((esubf - ProjRestEnergy)*(esubf + ProjRestEnergy));
       arg := (sqr(psubi) + sqr(psubf) - sqr(q))/(2*psubi*psubf);
       IF arg > 1 THEN theta := 0
       ELSE
        IF arg < -1 THEN theta := pi
        ELSE theta := ArcCos(arg);
    END
   ELSE
    BEGIN
       psubf := 0;
       theta := 0;
    END;
END;

PROCEDURE PsubfFromTP(hbarOmega, q: Float; VAR psubf, theta: Float);
{ Calculate psubf, theta from given values of hbarOmega, q. }
VAR
   esubf, RecoilEnergy, RecoilRestEnergy : Float;
BEGIN
   RecoilRestEnergy := TargetRestEnergy + hbarOmega;
   RecoilEnergy := sqrt(sqr(RecoilRestEnergy) + sqr(q));
   esubf := TargetRestEnergy + esubi - RecoilEnergy;
   MakeFinalMom(esubf, q, psubf, theta);
END;

PROCEDURE PsubfFromLS(CapQsq, nu: Float; VAR psubf, theta: Float);
{ Calculate psubf, theta from given values of CapQsq, nu. }
VAR
   esubf, q : Float;
BEGIN
   esubf := esubi - nu;
   q := sqrt(CapQsq + sqr(nu));
   MakeFinalMom(esubf, q, psubf, theta);
END;

PROCEDURE SetMomScales(smom: Integer);
{ Purpose:  Set the scales and draw the axes
            for the momentum plot }
VAR
   xmin, xmax, ymax : Float;
BEGIN
   xmin := 1.1*PfxRebound;
   IF xmin > 0 THEN xmin := 0;
   xmax := 1.1*psubi;
   ymax := 0.5*(xmax - xmin);
   DefineScale(smom, xmin, xmax, -ymax, ymax);
   SelectScale(smom);
   Axis( 0.0, 0.0, TickSpace(2*ymax), TickSpace(2*ymax) );
END;

FUNCTION FindMaxhbarOmega: Float;
BEGIN
   FindMaxhbarOmega := sqrt(sqr(TargetRestEnergy) + sqr(ProjRestEnergy)
    + 2*TargetRestEnergy*esubi) - TargetRestEnergy - ProjRestEnergy;
END;

FUNCTION FindMaxQ: Float;
VAR
   pfx : Float;
BEGIN
   pfx := PfxRebound;
   FindMaxQ := sqrt(sqr(psubi)-2*psubi*pfx+sqr(pfx));
END;

FUNCTION FindMaxNu: Float;
BEGIN
   FindMaxNu := esubi - ProjRestEnergy;
END;

FUNCTION FindMaxCapQsq: Float;
VAR
   pfx, esubf, numax, qmax : Float;
BEGIN
   pfx := PfxRebound;
   esubf := sqrt(sqr(pfx) + sqr(ProjRestEnergy));
   numax := esubi - esubf;
   qmax := sqrt(sqr(psubi)-2*psubi*pfx+sqr(pfx));
   FindMaxCapQsq := (qmax + numax)*(qmax - numax);
END;

PROCEDURE SetOmegaScales(somega: Integer; VAR OmTick, qTick: Float);
{ Purpose:  Set the scales and draw the axes
            for the q versus hbarOmega plot }
VAR
   xmin, ymin : Float;
   xmax, ymax : Float;
BEGIN
   xmin := -0.1*FindMaxhbarOmega;
   ymin := -0.1*FindMaxQ/hbarc;
   xmax := -11*xmin;
   ymax := -11*ymin;
   DefineScale(somega, xmin,xmax, ymin,ymax);
   SelectScale(somega);
   OmTick := TickSpace(xmax);
   qTick := TickSpace(ymax);
   Axis( 0.0, 0.0, OmTick, qTick );
   qTick := hbarc*qTick;
END;

PROCEDURE FindLorentzLimits(VAR CapQsqmax, numax: Float);
VAR
   pfx, esubf : Float;
BEGIN
   pfx := PfxRebound;
   esubf := sqrt(sqr(pfx) + sqr(ProjRestEnergy));
   numax := esubi - esubf;
   CapQsqmax := 2*TargetRestEnergy*numax;
   IF pfx < 0.0 THEN numax := esubi - ProjRestEnergy;
END;

PROCEDURE SetNuScales(snu: Integer; VAR nuTick, CapQTick: Float);
{ Purpose:  Set the scales and draw the axes
            for the CapQsq versus nu plot }
VAR
   numax      : Float;
   CapQsqmax  : Float;
   xmin, ymin : Float;
   xmax, ymax : Float;
BEGIN
   FindLorentzLimits(CapQsqmax, numax);
   xmin := -0.1*numax;
   ymin := -0.1*CapQsqmax/(2*TargetRestEnergy);
   xmax := 1.1*numax;
   ymax := 1.1*CapQsqmax/(2*TargetRestEnergy);
   DefineScale(snu, xmin,xmax, ymin,ymax);
   SelectScale(snu);
   nuTick := TickSpace(xmax);
   CapQTick := TickSpace(ymax);
   Axis( 0.0, 0.0, nuTick, CapQTick );
END;

PROCEDURE PlotOmegaBoundary;
{ Purpose:  Outline the boundary of the physically available
            region on the q versus hbarOmega plot }
VAR
   i : Integer;
   pfx      : Float;
   hbarOmega,q : Float;
   psubf, theta : Float;
   xv, yv      : DVector;
BEGIN
   Request2DVectors(xv, yv, Npoints);
   pfx := PfxRebound;
   FOR i{psubf} := 1 TO Npoints DO
   BEGIN
      psubf := ((i-1)*pfx+(Npoints-i)*psubi)/(Npoints-1);
      IF psubf < 0 THEN
      BEGIN
         theta := pi;
         psubf := -psubf;
      END
      ELSE theta := 0;
      FindTransferParams(psubf, theta, hbarOmega, q);
      xv.Put(i, hbarOmega);
      yv.Put(i, q/hbarc);
   END;
   PlotDVectorsJP(xv,yv,1,Npoints);
   Plotline(xv.Value(1),yv.Value(1),xv.Value(Npoints),yv.Value(Npoints));
   Release2DVectors(xv, yv);
END;

PROCEDURE PlotNuBoundary;
{ Purpose:  Outline the boundary of the physically available
            region on the CapQsq versus nu plot }
VAR
   i : Integer;
   pfx    : Float;
   CapQsq,nu : Float;
   psubf, theta : Float;
   xv,yv     : DVector;
BEGIN
   Request2DVectors(xv, yv, Npoints);
   pfx := PfxRebound;
   FOR i{psubf} := 1 TO Npoints DO
   BEGIN
      psubf := ((i-1)*pfx+(Npoints-i)*psubi)/(Npoints-1);
      IF psubf < 0 THEN
      BEGIN
         theta := pi;
         psubf := -psubf;
      END
      ELSE theta := 0;
      FindLorentzScalars(psubf, theta, CapQsq, nu);
      xv.Put(i, nu);
      yv.Put(i, CapQsq/(2*TargetRestEnergy));
   END;
   PlotDVectorsJP(xv,yv,1,Npoints);
   Plotline(xv.Value(1),yv.Value(1),xv.Value(Npoints),yv.Value(Npoints));
   Release2DVectors(xv, yv);
END;

PROCEDURE PlotMomSymbol(px1, py1, px2, py2: Float; vmom,smom: Integer);
VAR
   x1, y1, x2, y2 : Integer;
BEGIN
   SelectViewPort(vmom);
   SelectScale(smom);
   Map(px1, py1, x1, y1);
   Map(px2, py2, x2, y2);
   Vector(x1, y1, x2, y2);
END;

PROCEDURE PlotOmegaSymbol(hbarOmega,q: Float; vomega,somega: Integer);
VAR
   x, y : Integer;
BEGIN
   SelectViewPort(vomega);
   SelectScale(somega);
   Map(hbarOmega, q/hbarc, x, y);
   PlotX(x, y);
END;

PROCEDURE PlotNuSymbol(CapQsq, nu: Float; vnu, snu: Integer);
VAR
   x, y : Integer;
BEGIN
   SelectViewPort(vnu);
   SelectScale(snu);
   Map(nu, CapQsq/(2*TargetRestEnergy), x, y);
   PlotX(x, y);
END;

FUNCTION Allowed(pfx, pfy: Float): Boolean;
VAR
   pftest, thetest, hbarOmtest, qtest : Float;
BEGIN
   pftest := sqrt(sqr(pfx)+sqr(pfy));
   thetest := ArcTan2(pfx,pfy);
   FindTransferParams(pftest,thetest, hbarOmtest,qtest);
   Allowed := hbarOmtest >= 0;
END;

PROCEDURE PrepareMomentumPlot(VAR vmom, smom: Integer);
BEGIN { PrepareMomentumPlot }
   vmom := 1;
   smom := 1;
   DefineViewPort(vmom, 0.07,0.47,  0.11,0.66);
   OpenViewPort(vmom);
   SetMomScales(smom);
   PutLabel(Bottom,'pf_x (MeV/c)');
   PutLabel(Left,'pf_y (MeV/c)');
END;  { PrepareMomentumPlot }

PROCEDURE DisplayResultsPlot(psubf, theta: Float; vmom, smom: Integer);
VAR
   i, j         : Integer;
   pfx, pfy{, pf} : Float;
BEGIN
   DefineViewPort(vmom, 0.0,0.47,  0.11,0.66);
   OpenViewPort(vmom);
   pfx := 2.4*psubi;
   pfy := 2.1*psubi;
   DefineScale(smom, -pfx, pfx, -pfy, pfy);
   SelectScale(smom);
   PlotMomSymbol(-psubi, 0, 0, 0, vmom, smom);
   pfx := psubf*cos(theta);
   pfy := psubf*sin(theta);
   IF pfy > 0 THEN j := 8 ELSE j := 5;
   Print(8,j,'Initial');
   Print(8,j+1,'momentum');
   SetColor(Yellow);
   PlotMomSymbol(0, 0, pfx, pfy, vmom, smom);
   i := 16 + Round(8*pfx/psubi);
   j := 7 - Round(3*pfy/psubi);
   IF pfy > 0 THEN j := j - 2 ELSE j := j + 1;
   Print(i+1,j,'Final');
   Print(i,j+1,'momentum');
   SetColor(LightMagenta);
   pfx := psubi - pfx;
   pfy := -pfy;
   PlotMomSymbol(0, 0, pfx, pfy, vmom, smom);
   i := 16 + Round(8*pfx/psubi);
   IF i > 28 THEN i := 28;  IF i < 18 THEN i := 18;
   j := 7 - Round(3*pfy/psubi);
   IF pfy > 0 THEN j := j - 3 ELSE j := j + 1;
   Print(i,j,'Momentum');
   Print(i,j+1,'transfer');
   Print(i,j+2,'to target');
   SetColor(White);
END;

PROCEDURE PrepareOmegaPlot(VAR vomega, somega: Integer;
                           VAR OmTick, qTick: Float);

   PROCEDURE PlotConstEx(hbarOmega: Float);
   { Plot line of constant nuclear excitation }
   { Inputs are: TargetRestEnergy, ProjRestEnergy, psubi, esubi }
   VAR
      CMenergy, pcm, ecm  : Float;
      beta, gamma         : Float;
      deltap, qmin, qmax  : Float;
   BEGIN
      CMenergy := sqrt(sqr(TargetRestEnergy) + sqr(ProjRestEnergy)
                               + 2*TargetRestEnergy*esubi);
      beta := psubi/(TargetRestEnergy + esubi);
      gamma := (TargetRestEnergy + esubi)/CMenergy;
      ecm := (TargetRestEnergy*esubi + sqr(ProjRestEnergy)
              - hbarOmega*(TargetRestEnergy + 0.5*hbarOmega) )/CMenergy;
      pcm := Sqrt( (ecm - ProjRestEnergy)*(ecm + ProjRestEnergy) );
      deltap := psubi - gamma*beta*ecm;
      qmax := deltap - gamma*pcm;
      qmin := deltap + gamma*pcm;
      Plotline(hbarOmega, qmin/hbarc, hbarOmega, qmax/hbarc);
   END;

   PROCEDURE PlotConstQ(q: Float);
   { Plot line of constant momentum transfer }
   { Inputs are: TargetRestEnergy, ProjRestEnergy, psubi, esubi }
   VAR
      psubf, theta : Float;
      hbarOmegaMax : Float;
   BEGIN
      psubf := psubi - q;
      IF psubf < 0 THEN
         BEGIN
            theta := pi;
            psubf := -psubf;
         END
      ELSE theta := 0;
      FindTransferParams(psubf, theta, hbarOmegaMax, q);
      Plotline(0, q/hbarc, hbarOmegaMax, q/hbarc);
   END;

VAR
   hbarOmega, hbarOmegaMax : Float;
   q, qmax : Float;
BEGIN { PrepareOmegaPlot }
   vomega := 2;
   somega := 2;
   DefineViewPort(vomega, 0.60,1.00,  0.11,0.66);
   OpenViewPort(vomega);
   SetOmegaScales(somega, OmTick, qTick);
   PutLabel(Bottom,'hbar omega (MeV)');
   PutLabel(Left,'q (1/fm)');
   PlotOmegaBoundary;
   SetColor(LightCyan);
   hbarOmegaMax := FindMaxhbarOmega;
   hbarOmega := 0;
   WHILE hbarOmega < hbarOmegaMax DO
   BEGIN
      PlotConstEx(hbarOmega);  { lines of fixed hbarOmega }
      hbarOmega := hbarOmega + OmTick;
   END;
   SetColor(lightRed);
   qmax := FindMaxQ;
   q := qTick;
   WHILE q < qmax DO
   BEGIN
      PlotConstQ(q);  { lines of fixed q }
      q := q + qTick;
   END;
   SetColor(White);
END;  { PrepareOmegaPlot }


PROCEDURE PrepareNuPlot(VAR vnu, snu: Integer;
                        VAR nuTick, CapQTick: Float);

   PROCEDURE PlotConstNu(nu: Float);
   { Plot line of constant projectile energy loss }
   { Inputs are: TargetRestEnergy, ProjRestEnergy, psubi, esubi }
   VAR
      temp, psubf, q  : Float;
      CapQsqtem, nutem : Float;
      CapQsqmin, CapQsqmax : Float;
   BEGIN
      temp := sqr(psubi) + sqr(nu) - 2*esubi*nu;
      IF temp > 0 THEN psubf := sqrt(temp) ELSE psubf := 0;
      FindLorentzLimits(CapQsqtem, nutem);
      CapQsqmax := 2*TargetRestEnergy*nu;
      IF CapQsqmax > CapQsqtem THEN
      BEGIN
         q := psubi + psubf;
         CapQsqmax := (q + nu)*(q - nu);
      END;
      q := psubi - psubf;
      CapQsqmin := (q + nu)*(q - nu);
      Plotline(nu, CapQsqmin/(2*TargetRestEnergy),
               nu, CapQsqmax/(2*TargetRestEnergy));
   END;

   PROCEDURE PlotConstCapQ(CapQsq: Float);
   { Plot line of constant four-momentum transfer }
   { Inputs are: TargetRestEnergy, ProjRestEnergy, psubi, esubi }
   VAR
      numin, numax, root : Float;
   BEGIN
      numin := CapQsq/(2*TargetRestEnergy);
      root := sqrt( CapQsq*(sqr(esubi)*CapQsq
                  + 4*sqr(ProjRestEnergy)*(sqr(psubi) - 0.25*CapQsq)) );
      numax := (root - esubi*CapQsq)/(2*sqr(ProjRestEnergy));
      Plotline(numin, numin, numax, numin);
   END;

VAR
   nu, numax : Float;
   CapQsq, CapQsqmax, DeltaQ : Float;
BEGIN { PrepareNuPlot }
   vnu := 2;
   snu := 2;
   DefineViewPort(vnu, 0.60,1.00,  0.11,0.66);
   OpenViewPort(vnu);
   SetNuScales(snu, nuTick, CapQTick);
   PutLabel(Bottom,'nu (MeV)');
   PutLabel(Left,'Q^2/(2Mc^2) (MeV)');
   PlotNuBoundary;

   FindLorentzLimits(CapQsqmax, numax);
   SetColor(LightCyan);
   nu := 0;
   WHILE nu < numax DO
      BEGIN
         PlotConstNu(nu);  { lines of fixed nu }
         nu := nu + nuTick;
      END;
   SetColor(lightRed);
   DeltaQ := 2*CapQTick*TargetRestEnergy;
   CapQsq := DeltaQ;
   WHILE CapQsq < CapQsqmax DO
      BEGIN
         PlotConstCapQ(CapQsq);  { lines of fixed CapQsq }
         CapQsq := CapQsq + DeltaQ;
      END;
   SetColor(White);
END;  { PrepareNuPlot }


PROCEDURE PlotConstEx(hbarOmega: Float);
{ Plot locus of final momenta yielding constant nuclear excitation }
{ Inputs are: TargetRestEnergy, ProjRestEnergy,
              psubi, esubi, Npoints }
VAR
   CMenergy, pcm, ecm : Float;
   beta, gamma        : Float;
   thetacm, dthetacm  : Float;
   pfcos, pfsin       : Float;
   psubf, theta       : Float;
   i                  : Integer;
   pfxv, pfyv         : DVector;
BEGIN
   CMenergy := sqrt(sqr(TargetRestEnergy) + sqr(ProjRestEnergy)
                            + 2*TargetRestEnergy*esubi);
   beta := psubi/(TargetRestEnergy + esubi);
   gamma := (TargetRestEnergy + esubi)/CMenergy;
   ecm := (TargetRestEnergy*esubi + sqr(ProjRestEnergy)
           - hbarOmega*(TargetRestEnergy + 0.5*hbarOmega) )/CMenergy;
   pcm := Sqrt( (ecm - ProjRestEnergy)*(ecm + ProjRestEnergy) );
   Request2DVectors(pfxv, pfyv, Npoints);
   dthetacm := 2*pi/(Npoints - 1);
   thetacm := -pi - dthetacm;
   FOR i{thetacm} := 1 to Npoints DO BEGIN
      thetacm := thetacm + dthetacm;
      pfxv.Put(i, gamma*(beta*ecm + pcm*cos(thetacm)));
      pfyv.Put(i, pcm*sin(thetacm));
   END;
   PlotDVectorsJP(pfxv, pfyv, 1, Npoints);
   Release2DVectors(pfxv, pfyv);
END;


PROCEDURE PlotConstQ(q: Float);
{ Plot locus of final momenta yielding constant momentum transfer }
{ Inputs are: TargetRestEnergy, ProjRestEnergy,
              psubi, esubi, Npoints }
VAR
   psubf, theta : Float;
   qangle, dqangle : Float;
   qangleMax    : Float;
   i            : Integer;
   pfxv,pfyv    : DVector;
BEGIN
   PsubfFromTP(0, q, psubf, theta);
   qangleMax := ArcTan2(psubi - psubf*cos(theta), psubf*sin(theta));
   Request2DVectors(pfxv, pfyv, Npoints);
   dqangle := 2*qangleMax/(Npoints - 1);
   qangle := -qangleMax - dqangle;
   FOR i{qangle} := 1 TO Npoints DO BEGIN
      qangle := qangle + dqangle;
      pfxv.Put(i, psubi - q*cos(qangle));
      pfyv.Put(i, q*sin(qangle));
   END;
   PlotDVectorsJP(pfxv,pfyv,1,Npoints);
   Release2DVectors(pfxv, pfyv);
END;


PROCEDURE TransferParams;

   PROCEDURE CalibrateMomentumPlot(vmom, smom: Integer;
                                   OmTick, qTick: Float);
   VAR
      hbarOmega, hbarOmegaMax : Float;
      q, qmax : Float;
   BEGIN { CalibrateMomentumPlot (for TransferParams) }
      SelectViewPort(vmom);
      SelectScale(smom);
      SetColor(LightCyan);
      hbarOmegaMax := FindMaxhbarOmega;
      hbarOmega := 0;
      WHILE hbarOmega < hbarOmegaMax DO
      BEGIN
         PlotConstEx(hbarOmega);  { lines of fixed hbarOmega }
         hbarOmega := hbarOmega + OmTick;
      END;
      SetColor(lightRed);
      qmax := FindMaxQ;
      q := qTick;
      WHILE q < qmax DO
      BEGIN
         PlotConstQ(q);  { lines of fixed q }
         q := q + qTick;
      END;
      SetColor(White);
   END;  { CalibrateMomentumPlot (for TransferParams) }

   PROCEDURE PrepareParametersPlot(ProjectileSelector,
                TargetSelector: Integer; VAR vparams: Integer);
   VAR
      sparams : Integer;
   BEGIN
      vparams := 3;
      sparams := 3;
      DefineViewPort(vparams, 0.00,1.00,  0.68,0.94);
      OpenViewPort(vparams);
      DefineScale(sparams, 0.0,100.0, 0.0,100.0);
      SelectScale(sparams);
      SetColor(Yellow);
      IF TargetSelector < 3 THEN
         Print(20, 1, Concat(ProjectileName(ProjectileSelector),
            ' scattering on a ',
            TargetName(TargetSelector), ' target' ))
      ELSE
         Print(20, 1, Concat(ProjectileName(ProjectileSelector),
            ' scattering on an ',
            TargetName(TargetSelector), ' target' ));
      SetColor(lightRed);
      Plotline(3.5, 11.5, 15.0, 11.5);
      Print(15, 6, '= line of constant q');
      SetColor(LightCyan);
      Plotline(53.5, 11.5, 65.0, 11.5);
      Print(55, 6, '= line of constant omega');
      SetColor(White);
      Print(4, 2, 'Initial momentum ....');  Print(34, 2, 'MeV/c');
      Print(4, 3, 'Final momentum ......');  Print(34, 3, 'MeV/c');
      Print(4, 4, 'Scattering angle ....');  Print(34, 4, 'deg.');
      Print(44, 2, 'Momentum transfer ..');  Print(73, 2, '1/fm');
      Print(44, 3, 'Target excitation E ');  Print(73, 3, 'MeV');
      Print(44, 4, 'Recoil angle .......');  Print(73, 4, 'deg.');
   END;

   PROCEDURE UpdateParametersPlot(vparams: Integer;
                                  psubf, theta, hbaromega, q: Float );
   VAR
      thetaproj, thetatarg : Float;
   BEGIN
      SelectViewPort(vparams);
      thetaproj := theta;
      IF thetaproj > pi THEN thetaproj := 2*pi - thetaproj;
      thetatarg := ArcTan2(psubi - psubf*cos(theta), -psubf*sin(theta));
      IF thetatarg > pi THEN thetatarg := 2*pi - thetatarg;
      Print(26, 2, NumStr(psubi,7,1));
      Print(26, 3, NumStr(psubf,7,1));
      Print(26, 4, NumStr(thetaproj*(180/pi),7,1));
      Print(64, 2, NumStr(q/hbarc,8,2));
      Print(64, 3, NumStr(hbarOmega,7,1));
      Print(64, 4, NumStr(thetatarg*(180/pi),7,1));
   END;

VAR
   vmom, smom : Integer;
   vomega, somega : Integer;
   vparams : Integer;

   PROCEDURE DragVector(VAR psubf, theta: Float);
   VAR
      pfx, newpfx : Float;
      pfy, newpfy : Float;
      button : integer;
   BEGIN
      if InViewPort(vmom) then
      REPEAT
         HideMouse;
         SetWriteMode(XORput);
         SetColor(Yellow XOR GraphBackColor);
         PlotMomSymbol(0, 0, psubf*cos(theta), psubf*sin(theta), vmom, smom);
         FindTransferParams(psubf, theta, hbarOmega, q);
         PlotOmegaSymbol(hbarOmega, q, vomega, somega);
         SetWriteMode(COPYput);
         SetColor(GraphBackColor);
         UpdateParametersPlot(vparams, psubf, theta, hbaromega, q);
         ShowMouse;
         MousePosn(pfx,pfy, vmom,smom, button);
         IF Allowed(pfx, pfy) THEN
         BEGIN
            psubf := sqrt(sqr(pfx) + sqr(pfy));
            theta := ArcTan2(pfx,pfy);
         END;
         HideMouse;
         SetWriteMode(XORput);
         SetColor(Yellow XOR GraphBackColor);
         PlotMomSymbol(0, 0, psubf*cos(theta), psubf*sin(theta), vmom, smom);
         FindTransferParams(psubf, theta, hbarOmega, q);
         PlotOmegaSymbol(hbarOmega, q, vomega, somega);
         SetWriteMode(COPYput);
         SetColor(White);
         UpdateParametersPlot(vparams, psubf, theta, hbaromega, q);
         ShowMouse;
         Delay(50);
      UNTIL button = 0;
   END;

VAR
   OmTick, qTick : Float;
   pfx, pfy : Float;
   quit : Boolean;
   key : Byte;
BEGIN { TransferParams (Kinematics) }
   ClearMuppetPort;
   TargetRestEnergy := TargetSelector*amu;
   ProjRestEnergy := ProjectileRestEnergy(ProjectileSelector);
   esubi := sqrt(sqr(psubi) + sqr(ProjRestEnergy));
   PrepareMomentumPlot(vmom, smom);
   PrepareOmegaPlot(vomega, somega, OmTick, qTick);
   CalibrateMomentumPlot(vmom, smom, OmTick, qTick);
   PlotMomSymbol(0, 0, psubi, 0, vmom, smom);
   PrepareParametersPlot(ProjectileSelector, TargetSelector, vparams);
   Message(Concat('Click and drag projectile''s final momentum ',
         '(yellow arrow in left hand plot)'));
   psubf := psubi;
   theta := pi/2;
   REPEAT { Find an allowed starting vector for the scatt pcle's momentum }
      pfx := 0.4*(psubi + psubf*cos(theta));  pfy := 0.4*psubf*sin(theta);
      psubf := sqrt(sqr(pfx) + sqr(pfy));
      theta := ArcTan2(pfx, pfy);
   UNTIL Allowed(pfx, pfy);
   SetWriteMode(XORput);
         SetColor(Yellow XOR GraphBackColor);
   PlotMomSymbol(0, 0, pfx, pfy, vmom, smom);
   FindTransferParams(psubf, theta, hbarOmega, q);
   PlotOmegaSymbol(hbarOmega, q, vomega, somega);
   SetWriteMode(COPYput);
   SetColor(White);
   UpdateParametersPlot(vparams, psubf, theta, hbaromega, q);

   quit := FALSE;
   HotKeys.Display;
   REPEAT
      CheckForEvents;
      IF HotKeys.Pressed(key) THEN
         BEGIN
            CASE key OF
            1: ShowHelp('elscatt.hlp','DragVectorInfoScreen');
            2: quit := TRUE;
            END; { CASE }
         END;
      IF Event.MouseClicked AND NOT quit THEN DragVector(psubf, theta);
   UNTIL quit;

   ClearMuppetPort;
   HideMouse;
   SetColor(White);
   DisplayResultsPlot(psubf, theta, vmom, smom);
   PrepareOmegaPlot(vomega, somega, OmTick, qTick);
   PrepareParametersPlot(ProjectileSelector, TargetSelector, vparams);
   SetColor(Yellow);
   PlotOmegaSymbol(hbarOmega, q, vomega, somega);
   SetColor(White);
   UpdateParametersPlot(vparams, psubf, theta, hbaromega, q);
   ShowMouse;
END;  { TransferParams (Kinematics) }


PROCEDURE PlotConstNu(nu: Float);
{ Plot locus of final momenta yielding constant projectile energy loss }
{ Inputs are: TargetRestEnergy, ProjRestEnergy, psubi, esubi }
VAR
   temp, psubf, q : Float;
   theta, dtheta : Float;
   thetamax : Float;
   i                    : Integer;
   pfxv, pfyv           : DVector;
BEGIN
   temp := sqr(psubi) + sqr(nu) - 2*esubi*nu;
   IF temp > 0 THEN psubf := sqrt(temp) ELSE psubf := 0;
   IF psubf > -PfxRebound THEN
   BEGIN
      q := sqrt(nu*(2*TargetRestEnergy + nu));
      MakeFinalMom(esubi - nu, q, psubf, thetamax);
   END
   ELSE thetamax := pi;
   Request2DVectors(pfxv, pfyv, Npoints);
   dtheta := 2*thetamax/(Npoints - 1);
   theta := -thetamax - dtheta;
   FOR i{theta} := 1 to Npoints DO
   BEGIN
      theta := theta + dtheta;
      pfxv.Put(i, psubf*cos(theta));
      pfyv.Put(i, psubf*sin(theta));
   END;
   PlotDVectorsJP(pfxv, pfyv, 1, Npoints);
   Release2DVectors(pfxv, pfyv);
END;


PROCEDURE PlotConstCapQ(CapQsq: Float);
{ Plot locus of final momenta yielding constant four-momentum transfer }
{ Inputs are: TargetRestEnergy, ProjRestEnergy, psubi, esubi }
VAR
   numin, numax, root : Float;
   nu, dnu, psubf, theta : Float;
   i            : Integer;
   pfxv,pfyv    : DVector;
BEGIN
   numin := CapQsq/(2*TargetRestEnergy);
   root := sqrt( CapQsq*(sqr(esubi)*CapQsq
               + 4*sqr(ProjRestEnergy)*(sqr(psubi) - 0.25*CapQsq)) );
   numax := (root - esubi*CapQsq)/(2*sqr(ProjRestEnergy));
   Request2DVectors(pfxv, pfyv, Npoints);
   dnu := (numax - numin)/(Npoints - 1);
   nu := numin - dnu;
   FOR i{nu} := 1 TO Npoints DO BEGIN
      nu := nu + dnu;
      PsubfFromLS(CapQsq, nu, psubf, theta);
      pfxv.Put(i, psubf*cos(theta));
      pfyv.Put(i, psubf*sin(theta));
   END;
   PlotDVectorsJP(pfxv, pfyv, 1, Npoints);
   FOR i{nu} := 1 to Npoints DO BEGIN
      pfyv.Put(i, -pfyv.Value(i));
   END;
   PlotDVectorsJP(pfxv, pfyv, 1, Npoints);
   Release2DVectors(pfxv, pfyv);
END;


PROCEDURE LorentzInvariants;

   PROCEDURE CalibrateMomentumPlot(vmom, smom: Integer;
                                   nuTick, CapQTick: Float);
   VAR
      CapQsq, CapQsqmax : Float;
      DeltaQ, nu, numax : Float;
   BEGIN { CalibrateMomentumPlot (for LorentzInvariants) }
      SelectViewPort(vmom);
      SelectScale(smom);
      PlotConstEx(0);  { Outline the momentum plot }
      FindLorentzLimits(CapQsqmax, numax);
      SetColor(LightCyan);
      nu := 0;
      WHILE nu < numax DO
         BEGIN
            PlotConstNu(nu);  { lines of fixed nu }
            nu := nu + nuTick;
         END;
      SetColor(lightRed);
      DeltaQ := 2*CapQTick*TargetRestEnergy;
      CapQsq := DeltaQ;
      WHILE CapQsq < CapQsqmax DO
         BEGIN
            PlotConstCapQ(CapQsq);  { lines of fixed CapQsq }
            CapQsq := CapQsq + DeltaQ;
         END;
      SetColor(White);
   END;  { CalibrateMomentumPlot (for LorentzInvariants) }

   PROCEDURE PrepareParametersPlot(ProjectileSelector,
             TargetSelector: Integer; VAR vparams: Integer);
   VAR
      sparams : Integer;
   BEGIN
      vparams := 3;
      sparams := 3;
      DefineViewPort(vparams, 0.00,1.00,  0.68,0.94);
      OpenViewPort(vparams);
      DefineScale(sparams, 0.0,100.0, 0.0,100.0);
      SelectScale(sparams);
      SetColor(Yellow);
      IF TargetSelector < 3 THEN
         Print(20, 1, Concat(ProjectileName(ProjectileSelector),
            ' scattering on a ',
            TargetName(TargetSelector), ' target' ))
      ELSE
         Print(20, 1, Concat(ProjectileName(ProjectileSelector),
            ' scattering on an ',
            TargetName(TargetSelector), ' target' ));
      SetColor(lightRed);
      Plotline(3.5, 15.0, 15.0, 15.0);
      Print(15, 6, '= line of constant Q^2');
      SetColor(LightCyan);
      Plotline(53.5, 15.0, 65.0, 15.0);
      Print(55, 6, '= line of constant nu');
      SetColor(White);
      Print(4, 2, 'Initial momentum ....');  Print(34, 2, 'MeV/c');
      Print(4, 3, 'Final momentum ......');  Print(34, 3, 'MeV/c');
      Print(4, 4, 'Scattering angle ....');  Print(34, 4, 'deg.');
      Print(44, 2, 'Q^2/(2Mc^2) ........');  Print(72, 2, 'MeV');
      Print(44, 3, 'Projectile E loss ..');  Print(72, 3, 'MeV');
      Print(44, 4, 'System cm speed ....');  Print(72, 4, 'c');
   END;

   PROCEDURE UpdateParametersPlot(vparams: Integer;
                                  psubf, theta, CapQsq, nu: Float );
   VAR
      thetaproj, hbarOmega, q, betacm : Float;
   BEGIN
      SelectViewPort(vparams);
      thetaproj := theta;
      IF thetaproj > pi THEN thetaproj := 2*pi - thetaproj;
      FindTransferParams(psubf, theta, hbarOmega, q);
      betacm := psubi/(TargetRestEnergy + esubi);
      Print(26, 2, NumStr(psubi,7,1));
      Print(26, 3, NumStr(psubf,7,1));
      Print(26, 4, NumStr(thetaproj*(180/pi),7,1));
      Print(64, 2, NumStr(CapQsq/(2*TargetRestEnergy),7,1));
      Print(64, 3, NumStr(nu,7,1));
      Print(64, 4, NumStr(betacm,7,4));
   END;

VAR
   vmom, smom : Integer;
   vnu, snu : Integer;
   vparams : Integer;

   PROCEDURE DragVector(VAR psubf, theta: Float);
   VAR
      pfx, newpfx : Float;
      pfy, newpfy : Float;
      button : integer;
   BEGIN
      REPEAT
         HideMouse;
         SetWriteMode(XORput);
         SetColor(Yellow XOR GraphBackColor);
         PlotMomSymbol(0, 0, psubf*cos(theta), psubf*sin(theta), vmom, smom);
         FindLorentzScalars(psubf, theta, CapQsq, nu);
         PlotNuSymbol(CapQsq, nu, vnu, snu);
         SetWriteMode(COPYput);
         SetColor(GraphBackColor);
         UpdateParametersPlot(vparams, psubf, theta, CapQsq, nu);
         ShowMouse;
         MousePosn(pfx,pfy, vmom,smom, button);
         IF Allowed(pfx, pfy) THEN
            BEGIN
               psubf := sqrt(sqr(pfx) + sqr(pfy));
               theta := ArcTan2(pfx,pfy);
            END;
         HideMouse;
         SetWriteMode(XORput);
         SetColor(Yellow XOR GraphBackColor);
         PlotMomSymbol(0, 0, psubf*cos(theta), psubf*sin(theta), vmom, smom);
         FindLorentzScalars(psubf, theta, CapQsq, nu);
         PlotNuSymbol(CapQsq, nu, vnu, snu);
         SetWriteMode(COPYput);
         SetColor(White);
         UpdateParametersPlot(vparams, psubf, theta, CapQsq, nu);
         ShowMouse;
         Delay(50);
      UNTIL button = 0;
   END;

VAR
   nuTick, CapQTick : Float;
   pfx, pfy : Float;
   quit : Boolean;
   key : Byte;
BEGIN { LorentzInvariants (Kinematics) }
   ClearMuppetPort;
   TargetRestEnergy := TargetSelector*amu;
   ProjRestEnergy := ProjectileRestEnergy(ProjectileSelector);
   esubi := sqrt(sqr(psubi) + sqr(ProjRestEnergy));
   PrepareMomentumPlot(vmom, smom);
   PrepareNuPlot(vnu, snu, nuTick, CapQTick);
   CalibrateMomentumPlot(vmom, smom, nuTick, CapQTick);
   PlotMomSymbol(0, 0, psubi, 0, vmom, smom);
   PrepareParametersPlot(ProjectileSelector, TargetSelector,
                         vparams);
   Message(Concat('Click and drag projectile''s final momentum ',
         '(yellow arrow in left hand plot)'));
   psubf := psubi;
   theta := pi/2;
   REPEAT
      pfx := 0.4*(psubi + psubf*cos(theta));  pfy := 0.4*psubf*sin(theta);
      psubf := sqrt(sqr(pfx) + sqr(pfy));
      theta := ArcTan2(pfx, pfy);
   UNTIL Allowed(pfx, pfy);
   SetWriteMode(XORput);
         SetColor(Yellow XOR GraphBackColor);
   PlotMomSymbol(0, 0, pfx, pfy, vmom, smom);
   FindLorentzScalars(psubf, theta, CapQsq, nu);
   PlotNuSymbol(CapQsq, nu, vnu, snu);
   SetWriteMode(COPYput);
   SetColor(White);
   UpdateParametersPlot(vparams, psubf, theta, CapQsq, nu);

   quit := FALSE;
   HotKeys.Display;
   REPEAT
      CheckForEvents;
      IF HotKeys.Pressed(key) THEN
         BEGIN
            CASE key OF
            1: ShowHelp('elscatt.hlp','DragVectorInfoScreen');
            2: quit := TRUE;
            END; { CASE }
         END;
      IF Event.MouseClicked AND NOT quit THEN DragVector(psubf, theta);
   UNTIL quit;

   ClearMuppetPort;
   HideMouse;
   SetColor(White);
   DisplayResultsPlot(psubf, theta, vmom, smom);
   PrepareNuPlot(vnu, snu, nuTick, CapQTick);
   PrepareParametersPlot(ProjectileSelector, TargetSelector, vparams);
   SetColor(Yellow);
   PlotNuSymbol(CapQsq, nu, vnu, snu);
   SetColor(White);
   UpdateParametersPlot(vparams, psubf, theta, CapQsq, nu);
   ShowMouse;
END;  { LorentzInvariants (Kinematics) }

PROCEDURE ChooseParameters;
VAR
   iplot : Integer;
   TargetScreen : TInputScreen;
BEGIN { ChooseParameters (Kinematics) }
   OpenScrn(TargetScreen);
   WITH TargetScreen DO
   BEGIN
      init;
      DefineInputport(  0.22,0.78,  0.15,0.85);
      LoadLine('');
      LoadLine('  Projectile and Target Parameters:');
      LoadLine('  Projectiles: #1 electron');
      LoadLine('               #1 muon');
      LoadLine('               #1 tauon');
      LoadLine('');
      LoadLine('  Initial momentum    {     } MeV/c  ');
      LoadLine('  (Range is 100 through 100000 MeV/c)');
      LoadLine('  Target mass number  {     }     ');
      LoadLine('  (Integer in range 1 through 250)');
      LoadLine('');
      LoadLine('  Plot What:');
      LoadLine('               #2 Transfer parameters');
      LoadLine('               #2 Lorentz scalars');
      LoadLine('');
      LoadLine('        [  Ok  ]  [Cancel]');
      SetRadioButton('1',ProjectileSelector);
      SetNumber(4,psubi);
      SetNumber(5,TargetSelector);
      SetNumberLimits(4,100,100000);
      SetNumberLimits(5,1,250);
      AcceptScreen;
      ProjectileSelector := GetRadioButton('1');
      psubi              := round(GetNumber(4));
      TargetSelector     := round(GetNumber(5));
      iplot              := GetRadioButton('2');
      IF NOT canceled THEN
         BEGIN
            IF iplot = 1 THEN TransferParams
                         ELSE LorentzInvariants;
         END;
      done;
   END;
   CloseScrn(TargetScreen);
END;  { ChooseParameters (Kinematics) }

BEGIN { Kinematics }
   RestoreKinematicsConstants;
   WITH MasterMenu DO
      CASE RowChosen OF
       0: Initialize;
       1: ShowHelp('elscatt.hlp','KinematicsInfoScreen');
       2: ChooseParameters;
       3: TransferParams;
       4: LorentzInvariants;
      END; {CASE}
   SaveKinematicsConstants;
END;  { Kinematics }


{ ------------------------ Begin ELASTIC Procedure ------------------------ }


VAR
   ElasDensitySelector : Integer;
   ElasTargetSelector  : Integer;
   ElasEquivSqRadius   : Float;
   ElasOscRadius       : Float;
   ElasFermiRadius     : Float;
   ElasDiffuseness     : Float;

PROCEDURE Elastic;

VAR
   DensitySelector : Integer;
   TargetSelector  : Integer;
   EquivSqRadius   : Float;
   OscRadius       : Float;
   FermiRadius     : Float;
   Diffuseness     : Float;

PROCEDURE RestoreElasticConstants;
BEGIN
   DensitySelector := ElasDensitySelector;
   TargetSelector  := ElasTargetSelector;
   EquivSqRadius   := ElasEquivSqRadius;
   OscRadius       := ElasOscRadius;
   FermiRadius     := ElasFermiRadius;
   Diffuseness     := ElasDiffuseness;
END;

PROCEDURE SaveElasticConstants;
BEGIN
   ElasDensitySelector := DensitySelector;
   ElasTargetSelector  := TargetSelector;
   ElasEquivSqRadius   := EquivSqRadius;
   ElasOscRadius       := OscRadius;
   ElasFermiRadius     := FermiRadius;
   ElasDiffuseness     := Diffuseness;
END;

PROCEDURE GetTargetParams (VAR EquivSqRadius, OscRadius,
                               FermiRadius, Diffuseness: Float);

   PROCEDURE SetParams (arg1, arg2, arg3, arg4: Float);
   BEGIN
      EquivSqRadius := arg1;
      OscRadius     := arg2;
      FermiRadius   := arg3;
      Diffuseness   := arg4/4.40;
   END;

VAR
   A, Athird: Float;
BEGIN
   IF TargetSelector < 17 THEN
      BEGIN
         CASE Round(TargetSelector / 2) OF
            { Equiv Sq   Oscillator   Fermi    Surface  }
            {  Radius      Radius     Radius  thickness }
            {   (fm)        (fm)       (fm)      (fm)
1: SetParams(   0.0     ,   0.0    ,   0.0   ,   0.0    );  { 2H   }
2: SetParams(   2.07    ,   1.31   ,   1.10  ,   1.4    );  { 4He  }
3: SetParams(   3.41    ,   0.0    ,   1.56  ,   2.3    );  { 6Li  }
4: SetParams(   0.0     ,   0.0    ,   0.0   ,   0.0    );  { none }
5: SetParams(   0.0     ,   0.0    ,   0.0   ,   0.0    );  { 10B  }
6: SetParams(   3.11    ,   1.64   ,   2.30  ,   1.85   );  { 12C  }
7: SetParams(   3.20    ,   1.67   ,   2.40  ,   1.85   );  { 14N  }
8: SetParams(   3.42    ,   1.76   ,   2.60  ,   1.8    );  { 16O  }
         END { CASE }
      END
   ELSE
      BEGIN
         A := TargetSelector;
         Athird := Pwr(A, 1/3);
         EquivSqRadius := 1.123*Athird + 2.352/Athird - 2.070/A;
         OscRadius := Pwr(TargetSelector, 1/6);
         FermiRadius := 1.123*Athird - 0.941/Athird;
         Diffuseness := 2.49/4.40;
      END;
   IF TargetSelector > 40 THEN
    OscRadius := 0;
END;

PROCEDURE Initialize;
BEGIN { Initialize (Elastic) }
   MakeFactorial;
   MakeDoubleFac;
   SetupSinglPclArray;
   DensitySelector := 1;
   TargetSelector := 16;
   GetTargetParams(EquivSqRadius, OscRadius, FermiRadius, Diffuseness);
END;  { Initialize (Elastic) }

PROCEDURE SetDensityScales(vdens, sdens: Integer);
{ Purpose:  Set the scales and draw the axes
            for the density plot }
VAR
   ymax : Real;
BEGIN
   SelectViewPort(vdens);
   ymax := 0.40;
   DefineScale(sdens, 0, Rmax, 0, ymax);
   SelectScale(sdens);
   Axis( 0.0, 0.0, TickSpace(Rmax), TickSpace(ymax) );
END;

PROCEDURE PrepareDensityPlot(VAR vdens, sdens: Integer);
BEGIN { PrepareDensityPlot }
   vdens := 1;
   sdens := 1;
   DefineViewPort(vdens, 0.07,0.47,  0.11,0.66);
   OpenViewPort(vdens);
   SetDensityScales(vdens, sdens);
   PutLabel(Bottom,'r (fm)');
   PutLabel(Left,'rho(r) ((1/fm)^3)');
END;  { PrepareDensityPlot }

PROCEDURE PlotDensity(vdens, sdens: Integer; rv, rhoRv: DVector);
BEGIN
   SelectViewPort(vdens);
   SelectScale(sdens);
   PlotDVectorsJP( rv, rhoRv, 1, NumR);
END;

PROCEDURE SetFormFactorScales(vform, sform: Integer);
{ Purpose:  Set the scales and draw the axes
            for the form factor plot }
VAR
   ymax : Real;
BEGIN
   SelectViewPort(vform);
   ymax := 1.1;
   DefineScale(sform, 0, Qmax, 0, ymax);
   SelectScale(sform);
   Axis( 0.0, 0.0, TickSpace(Qmax), TickSpace(ymax) );
END;

PROCEDURE PrepareFormFactorPlot(VAR vform, sform: Integer);
BEGIN { PrepareFormFactorPlot }
   vform := 2;
   sform := 2;
   DefineViewPort(vform, 0.60,1.00,  0.11,0.66);
   OpenViewPort(vform);
   SetFormFactorScales(vform, sform);
   PutLabel(Bottom,'q (1/fm)');
   PutLabel(Left,'|f(q)|');
END;  { PrepareFormFactorPlot }

PROCEDURE PrepareParametersPlot(VAR vparams: Integer);
VAR
   sparams : Integer;
BEGIN
   vparams := 3;
   sparams := 3;
   DefineViewPort(vparams, 0.36,1.00, 0.68,0.94);
   OpenViewPort(vparams);
   DefineScale(sparams, 0.0,100.0, 0.0,100.0);
   SelectScale(sparams);
   Print(1, 1, ' Elastic electron scattering on a spin-zero target.');
   Print(1, 2, '           Color coded plots show:-');
END;

PROCEDURE UpdateParametersPlot(vparams, Icount: Integer);
BEGIN
   SelectViewPort(vparams);
   Print(2, Icount + 2, Concat(NumStr(Icount,1,0), ') ',
            DensityName(DensitySelector),
            ' density for an ',
            TargetName(TargetSelector), ' target'));
END;

PROCEDURE OscDensity(n1, l1: Integer; rv: DVector;
                     VAR rhotempv: DVector);
{ Calculate the radial density of an oscillator single particle function }
VAR
   kp, kpmin, kpmax, i : Integer;
   rho, rhosq, sum, ftr : Float;
   cnv  : DVector;
BEGIN
   Request1DVector(cnv, NumOscCoeff);
   CofUU(n1, l1, n1, l1, cnv, kpmin, kpmax);
   FOR i := 1 TO NumR DO
   BEGIN
      sum := 0;
      rho := rv.Value(i)/OscRadius;
      rhosq := sqr(rho);
      ftr := Pwr(rho, kpmin - 1);
      FOR kp := kpmin TO kpmax DO
         IF Even(kp - kpmin) THEN
         BEGIN
            sum := sum + cnv.Value(kp)*ftr;
            ftr := ftr*rhosq;
         END;
      rhotempv.Put(i,sum*exp(-rhosq));
   END;
   Release1DVector(cnv);
END;

PROCEDURE OscModelDensity(rv: DVector; VAR rhoRv : DVector);
VAR
   i, isp, Numsp : Integer;
   number, n1, l1, jj1 : Integer;
   rhotempv : DVector;
   norm : Float;
BEGIN
   FOR i := 1 TO NumR DO
      BEGIN { Clear the density DVector }
         rhoRv.Put(i,0);
      END;
   Request1DVector(rhotempv, NumR);
   Numsp := 0;
   isp := 0;
   REPEAT { Loop over shell model single particle functions }
      Inc(isp);
      n1  := SinglPclArray[isp].nsp;
      l1  := SinglPclArray[isp].lsp;
      jj1 := SinglPclArray[isp].jjsp;
      number := 2*(jj1 + 1); { Number of nucleons }
      IF number > TargetSelector - Numsp THEN
         number := TargetSelector - Numsp;
      Numsp := Numsp + number;
      OscDensity(n1, l1, rv, rhotempv);
      FOR i := 1 TO NumR DO
      BEGIN { Collect contributions to the density }
         rhoRv.Put(i, rhoRv.Value(i) + number*rhotempv.Value(i) );
      END;
   UNTIL Numsp = TargetSelector;
   norm := 1/(4*pi*Pwr(OscRadius,3));
   FOR i := 1 TO NumR DO
   BEGIN
      rhoRv.Put(i, norm*rhoRv.Value(i) );
   END;
   Release1DVector(rhotempv);
END;

FUNCTION CalcDensity(rzero, rhozero, r: Float): Float;
VAR
   temp : Float;
BEGIN
   CASE DensitySelector OF
      1: BEGIN { Square }
            IF r < rzero THEN temp := rhozero
            ELSE IF r = rzero THEN temp := 0.5*rhozero
            ELSE temp := 0;
            CalcDensity := temp;
         END;
      2: BEGIN { Gaussian }
            CalcDensity := rhozero*exp(-sqr(r/rzero));
         END;
      3: BEGIN { Fermi }
            CalcDensity := rhozero/(1 + exp((r - rzero)/Diffuseness));
         END;
   END;
END;


PROCEDURE MakeDensity(rv: DVector; VAR rhoRv : DVector);
{ Uses DensitySelector, EquivSqRadius, FermiRadius, Diffuseness }
VAR
   rzero, rhozero : Float;
   i : Integer;
BEGIN
   CASE DensitySelector OF
      1: BEGIN { Square }
            rzero := EquivSqRadius;
            rhozero := 3*TargetSelector/(4*pi*Pwr(rzero,3));
         END;
      2: BEGIN { Gaussian }
            rzero := sqrt(2/5)*EquivSqRadius;
            rhozero := TargetSelector/(Pwr(sqrt(pi)*rzero,3));
         END;
      3: BEGIN { Fermi }
            rzero := FermiRadius;
            rhozero := 3*TargetSelector/(4*pi*Pwr(rzero,3));
            rhozero := rhozero/(1 + sqr(pi*diffuseness/rzero));
         END;
   END; { CASE }
   IF DensitySelector <> 4 THEN
      FOR i := 1 TO NumR DO
      BEGIN
         rhoRv.Put(i,CalcDensity(rzero, rhozero, rv.Value(i)));
      END
   ELSE OscModelDensity(rv, rhoRv);
END;

FUNCTION SimpInt(xv, yv: DVector; Npoints: Integer): Float;
{ Definite integral of y wrt x over the full
  range of values contained in the DVectors. }
VAR
   i, imax, ival  : Integer;
   sum1, sum2, h : Float;
BEGIN
   imax := round((Npoints - 1)/2);
   IF Npoints = 2*imax + 1 THEN
   BEGIN { Simpson's rule integration }
      sum1 := 0.0;  sum2 := 0.0;
      ival := 1;
      FOR i := 1 TO imax DO
      BEGIN
         ival := ival + 2;
         sum1 := sum1 + yv.Value(ival-1);
         sum2 := sum2 + yv.Value(ival);
      END;
      h := (xv.Value(Npoints) - xv.Value(1))/(Npoints - 1);
      SimpInt := (yv.Value(1) + 4*sum1 + 2*sum2
                   - yv.Value(Npoints))*h/3.0;
   END
   ELSE SimpInt := 0;
END;

FUNCTION Jzero(x: Float): Float;
BEGIN
   IF x = 0 THEN Jzero := 1
            ELSE Jzero := sin(x)/x;
END;

FUNCTION rhoQSquare(x: Float): Float;
BEGIN
   IF x = 0 THEN rhoQSquare := 1
            ELSE rhoQSquare := (3/sqr(x))*(sin(x)/x - cos(x));
END;

PROCEDURE PlotFormFactor(vform, sform: Integer; qv, rhoQv: DVector);
BEGIN
   SelectViewPort(vform);
   SelectScale(sform);
   PlotABSDvtr(qv, rhoQv, 1, NumQ);
END;

PROCEDURE MakeAndPlotFormFactor(vform, sform: Integer;
          rv, rhoRv, qv: DVector; VAR rhoQv : DVector);
VAR
   i, j : Integer;
   yv   : DVector;
   rzero : Float;
   q, r : Float;
   oldq, oldvalue, newvalue : Float;
   norm : Float;
   analytic : Boolean;
BEGIN
   analytic := TRUE;
   CASE DensitySelector OF
      1: BEGIN { Square }
            rzero := EquivSqRadius;
            FOR i := 1 TO NumQ DO
            BEGIN
               rhoQv.Put(i, rhoQsquare(qv.Value(i)*rzero));
            END;
         END;
      2: BEGIN { Gaussian }
            rzero := sqrt(2/5)*EquivSqRadius;
            FOR i := 1 TO NumQ DO
            BEGIN
               rhoQv.Put(i, exp(-0.25*sqr(qv.Value(i)*rzero)));
            END;
         END;
      3: BEGIN { Fermi }
            Analytic := FALSE;
         END;
      4: BEGIN { Oscillator shell model }
                                                     { *** ElasticFF  }
{            ElasticFormFactor(qv, rhoQv);        }  { expects OTHER  }
{            rhoQv.MultScalar(2/TargetSelector);  }  { TargetSelector }

            Analytic := FALSE;
         END;
   END; { CASE }
   IF analytic THEN PlotFormFactor(vform, sform, qv, rhoQv)
   ELSE
      BEGIN { Brute force integration + point-by-point plot }
         SelectViewPort(vform);
         SelectScale(sform);
         norm := 4*pi/TargetSelector;
         Request1DVector(yv, NumR);
         q := 0;
         newvalue := 0;
         FOR i := 1 TO NumQ DO
         BEGIN {Loop over q values }
            oldq := q;
            oldvalue := newvalue;
            q := qv.Value(i);
            FOR j := 1 TO NumR DO
            BEGIN { Set up the integrand }
               r := rv.Value(j);
               yv.Put(j,Jzero(q*r)*rhoRv.Value(j)*sqr(r));
            END;  { Set up the integrand }
            newvalue := ABS(norm*SimpInt(rv, yv, NumR));
            rhoQv.Put(i, newvalue);
            IF i > 1 THEN Plotline(oldq, oldvalue, q, newvalue);
         END;  { Loop over q values }
         Release1DVector(yv);
      END;  { Brute force integration + point-by-point plot }
END;

PROCEDURE SetCoulombScreenPC(var Screen : TInputScreen);
BEGIN
   WITH Screen DO
   BEGIN
      DefineInputport(  0.00,0.35,  0.68,0.94);
      LoadLine('Target mass number {    }');
      LoadLine('(+ve even integer <= 250)');
      LoadLine('Density shapes:');
      LoadLine('#1Square #1Gaussian');
      LoadLine('#1Fermi  #1Osc Shell Model');
      LoadLine('    [  Ok  ]  [Cancel]');
      SetNumber(1,TargetSelector);
      SetNumberLimits(1,1,250);
      SetRadioButton('1',DensitySelector);
   END;
END;

PROCEDURE SetCoulombScreenMac(var Screen : TInputScreen);
BEGIN
   WITH Screen DO
   BEGIN
      DefineInputport(  0.00,0.35,  0.60,0.91);
      LoadLine('Target mass number {   }');
      LoadLine('Density shapes:');                        
      LoadLine('#1Square #1Gaussian');
      LoadLine('#1Fermi  #1Osc SM');
      LoadLine('  [  Ok  ]   [Cancel]');
      SetNumber(1,TargetSelector);
      SetNumberLimits(1,1,250);
      SetRadioButton('1',DensitySelector);
   END;
END;

FUNCTION CheckData(Target, Density: Integer): Boolean;
BEGIN
   CheckData := FALSE;
   IF NOT Even(Target) THEN
      announce('Please enter a positive even integer.  Try again...')
   ELSE IF Target = 2 THEN
      announce('No densities available for A = 2.  Try again...')
   ELSE IF Target = 8 THEN
      announce('No stable nucleus for A = 8.  Try again...')
   ELSE IF Target = 10 THEN
      announce('No densities available for A = 10.  Try again...')
   ELSE IF (Density = 4) AND (Target > 40) THEN
     announce('No oscillator model available for A > 40.  Try again...')
   ELSE IF (Density = 4) AND (Target >= 6) AND (Target <= 10) THEN
     announce('No oscillator model available for A = 6-10.  Try again...')
   ELSE CheckData := TRUE;
END;

FUNCTION ChooseAction: Boolean;
VAR
   CoulombScreen   : TInputScreen;
   GoodValue       : Boolean;
   Target, Density : Integer;
BEGIN
   OpenScrn(CoulombScreen);
   WITH CoulombScreen DO
   BEGIN
      GoodValue := FALSE;
      REPEAT
         Init;
         SetCoulombScreenPC(CoulombScreen);
         Accept;
         Target   := Round(GetNumber(1));
         Density  := GetRadioButton('1');
         IF NOT Canceled THEN
            BEGIN
               GoodValue := CheckData(Target, Density);
               ChooseAction := TRUE;
            END
         ELSE ChooseAction := FALSE;
         Done;
      UNTIL GoodValue OR CancelPressed;
   END;
   IF GoodValue THEN
      BEGIN
         TargetSelector  := Target;
         DensitySelector := Density;
         GetTargetParams(EquivSqRadius, OscRadius,
                         FermiRadius, Diffuseness);
      END;
   CloseScrn(CoulombScreen);
END;

PROCEDURE Coulomb;
VAR
   Color : ARRAY[1..4] OF Integer;

   PROCEDURE SetColors;
   BEGIN
      color[1] := LightRed;
      color[2] := LightCyan;
      color[3] := Yellow;
      color[4] := White;
   END;

VAR
   vdens, sdens : Integer;
   vform, sform : Integer;
   vparams      : Integer;
   rv, rhoRv    : DVector;
   qv, rhoQv    : DVector;
   ActionSelector : Integer;
   Icount       : Integer;
   canceled     : Boolean;
BEGIN { Coulomb (Elastic) }
   SetColors;
   ClearMuppetPort;
   PrepareDensityPlot(vdens, sdens);
   PrepareFormFactorPlot(vform, sform);
   PrepareParametersPlot(vparams);
   NumR := 2*Round( Rmax/(2.0*Rstep) ) + 1;
   NumQ := 2*Round( Qmax/(2.0*Qstep) ) + 1;
   Request2DVectors(rv, rhoRv, NumR);
   Request2DVectors(qv, rhoQv, NumQ);
   SetGrid(NumR, Rstep, rv);
   SetGrid(NumQ, Qstep, qv);
   Message(' Enter your selection...');
   Icount   := 0;
   REPEAT
      canceled := NOT ChooseAction;
      IF NOT canceled THEN
      BEGIN
         Icount := Icount + 1;
         MakeDensity(rv, rhoRv);
         SetColor(Color[Icount]);
         PlotDensity(vdens, sdens, rv, rhoRv);
         MakeAndPlotFormFactor(vform, sform, rv, rhoRv, qv, rhoQv);
         UpdateParametersPlot(vparams, Icount);
         Message(' Enter another selection...');
      END;
   UNTIL (Icount = 4) OR canceled;
   SetColor(White);
   DefineViewPort(4, 0.00, 0.35, 0.67, 0.95);
   CloseViewPort(4);
   Release2DVectors(qv, rhoQv);
   Release2DVectors(rv, rhoRv);
END;  { Coulomb (Elastic) }

BEGIN { Elastic }
   RestoreElasticConstants;
   WITH MasterMenu DO
      CASE RowChosen OF
       0: initialize;
       1: ShowHelp('elscatt.hlp','ElasticInfoScreen');
       2: Coulomb;
      END; {CASE}
   SaveElasticConstants;
END;  { Elastic }

{ ----------------------- Begin INELASTIC Procedure ----------------------- }


TYPE
   FFConsts =
      RECORD
         gl, gs : Float;
      END;

VAR
   Iso0consts : FFconsts;
   Iso1consts : FFconsts;
   Neutconsts : FFconsts;
   Protconsts : FFconsts;
   Nogsconsts : FFconsts;
   Noglconsts : FFconsts;

PROCEDURE SetupFFconsts;
VAR
   c : Float;
BEGIN
   c := 1/sqrt(2);
   WITH Iso0consts DO
      BEGIN
         gl := c*(glneut + glprot);  gs := c*(gsneut + gsprot);
      END;
   WITH Iso1consts DO
      BEGIN
         gl := c*(glneut - glprot);  gs := c*(gsneut - gsprot);
      END;
   WITH Neutconsts DO
      BEGIN
         gl := glneut;  gs := gsneut;
      END;
   WITH Protconsts DO
      BEGIN
         gl := glprot;  gs := gsprot;
      END;
   WITH Nogsconsts DO
      BEGIN
         gl := glprot;  gs := 0;
      END;
   WITH Noglconsts DO
      BEGIN
         gl := 0;  gs := gsprot;
      END;
END;


VAR
   qValueInel : Float;


PROCEDURE Inelastic;


PROCEDURE GetJspinRange(VAR Jmin, Jmax: Integer);
   { USES HoleSelector, Nparticle, ParticleSelector }
VAR
   nh, lh, jjh : Integer;
   np, lp, jjp : Integer;
BEGIN
   MakeHoleQuantumNumbers(HoleSelector, nh, lh, jjh);
   MakeParticleQuantumNumbers(Nparticle, ParticleSelector, np, lp, jjp);
   Jmin := ABS(Round((jjp - jjh)/2));
   Jmax := Round((jjp + jjh)/2);
END;


PROCEDURE CycleJspin;
VAR
   Jmin, Jmax  : Integer;
BEGIN
   GetJspinRange(Jmin, Jmax);
   IF Jspin < Jmin THEN Jspin := Jmax;
   IF Jspin > Jmax THEN Jspin := Jmin;
END;


PROCEDURE AdjustJspin;
VAR
   Jmin, Jmax  : Integer;
BEGIN
   GetJspinRange(Jmin, Jmax);
   IF Jspin < Jmin THEN Jspin := Jmin;
   IF Jspin > Jmax THEN Jspin := Jmax;
END;


PROCEDURE ResetJspin;
BEGIN
   Jspin := 0;
   AdjustJspin;
END;


FUNCTION FindNumOrbit(np, lp, jjp: Integer): Integer;
VAR
   Npcle, NumOrbit : Integer;
   ntest, ltest, jjtest : Integer;
BEGIN
   Npcle := 2*(np - 1) + lp;
   NumOrbit := 0;
   REPEAT
      Inc(NumOrbit);
      MakeParticleQuantumNumbers(Npcle, NumOrbit, ntest, ltest, jjtest);
   UNTIL (np = ntest) AND (lp = ltest) AND (jjp = jjtest);
   FindNumOrbit := NumOrbit;
END;


PROCEDURE Initialize;
VAR
   np, lp, jjp : Integer;
BEGIN { Initialize (Inelastic) }
   rootpiby4 := 0.25*sqrt(pi);
   SetupLchar;
   Iselect          := 1;                { T = 0 excitation     }
   TargetSelector   := 3;                { 16 O target nucleus  }
   HoleSelector     := TargetSelector;   { Uppermost hole state }
   ParticleSelector := HoleSelector + 1; { Bottommost particle  }
   MakeHoleQuantumNumbers(ParticleSelector, np, lp, jjp);
   Nparticle := 2*(np - 1) + lp;
   ParticleSelector := FindNumOrbit(np, lp, jjp);
   Jspin := 0;
   AdjustJspin;
   MakeJFacts;
   SetupFFconsts;
   qValueInel := 2;
END;  { Initialize (Inelastic) }

PROCEDURE ShowInelasticTitle;
BEGIN
   DefineViewport(3, 0.00, 0.97, 0.885, 0.93);
   OpenViewPort(3);
   WITH SinglPclArray[HoleSelector] DO
   Print(2,1,Concat('Inelastic scatt to  [ particle( ',
      MakeParticleString(Nparticle, ParticleSelector),
      ' ) x hole( ', MakeHoleString(HoleSelector),
      ' ) ] J =', NumStr(Jspin,2,0), '  in ',
      MakeTargetString(TargetSelector)));
END;

PROCEDURE PrepareDensitiesAndFormFactorPlot(Iselect: Integer);
VAR
   EMconsts : FFconsts;
BEGIN
   CASE Iselect OF
   1:  EMconsts := Iso0consts;
   2:  EMconsts := Iso1consts;
   3:  EMconsts := Neutconsts;
   4:  EMconsts := Protconsts;
   5:  EMconsts := Nogsconsts;
   6:  EMconsts := Noglconsts;
   END; { CASE }
   ShowInelasticTitle;
   DefineViewport(1, 0.07, 0.47, 0.15, 0.85);
   OpenViewPort(1);
   CASE Iselect OF
   1:  Print(5,1,'** T = 0 EXCITATION **');
   2:  Print(5,1,'** T = 1 EXCITATION **');
   3:  Print(5,1,'** NEUTRON EXCITATION **');
   4:  Print(5,1,'** PROTON EXCITATION **');
   5:  Print(1,1,'** PROTON Convection Current **');
   6:  Print(4,1,'** PROTON Spin Current **');
   END; { CASE }
   PutLabel(Bottom,'r (fm)');
   PutLabel(Left,'|rho(r)|');

   DefineViewport(2, 0.57, 0.97, 0.15, 0.85);
   OpenViewPort(2);
   CASE Iselect OF
   1:  Print(5,1,'** T = 0 EXCITATION **');
   2:  Print(5,1,'** T = 1 EXCITATION **');
   3:  Print(5,1,'** NEUTRON EXCITATION **');
   4:  Print(5,1,'** PROTON EXCITATION **');
   5:  Print(1,1,'** PROTON Convection Current **');
   6:  Print(4,1,'** PROTON Spin Current **');
   END; { CASE }
   PutLabel(Bottom,'q (1/fm)');
   PutLabel(Left,'|f(q)|');
END;

PROCEDURE SetDensityScales(denmax: Float);
BEGIN
   DefineScale(1, 0.0, Rmax, 0.0, Denmax);
   SelectScale(1);
   SelectViewport(1);
   Axis(0.0, 0.0, TickSpace(Rmax), TickSpace(denmax));
END;

PROCEDURE SetFormFactorScales(FFmax: Float);
BEGIN
   DefineScale(2, 0.0, Qmax, 0.0, FFmax);
   SelectScale(2);
   SelectViewport(2);
   Axis(0.0, 0.0, TickSpace(Qmax), TickSpace(FFmax));
END;

PROCEDURE MakeDensitiesPlot(Iselect: Integer; rv: DVector; NumR: Integer);

VAR
   EMconsts : FFconsts;

   PROCEDURE ExplainNoCurves;

      PROCEDURE DisplayNoneExpectedMessage;
      BEGIN
         Print(10,10,'This state is');
         Print(8,11,'not populated via');
         Print(7,12,'electron scattering');
      END;

      PROCEDURE DisplayNoConvectionCurrentMessage;
      BEGIN
         Print(5,10,'The convection current');
         Print(5,11,'vanishes for this state');
      END;

      PROCEDURE DisplayNoSpinCurrentMessage;
      BEGIN
         Print(8,10,'The spin current');
         Print(5,11,'vanishes for this state');
      END;

   BEGIN
      IF Iselect < 5 THEN DisplayNoneExpectedMessage
      ELSE
         BEGIN
            WITH EMconsts DO
               BEGIN
                  IF gl <> 0 THEN DisplayNoConvectionCurrentMessage;
                  IF gs <> 0 THEN DisplayNoSpinCurrentMessage;
               END;
         END;
   END;

VAR
   nh, lh, jjh : Integer;
   np, lp, jjp : Integer;
   imin, imax  : Integer;
   rhov        : DVector;
   trans1v, trans2v : DVector;
   spin1v,  spin2v  : DVector;
   DensitiesFound   : Boolean;
   ftr, fCoul, fConv, fSpin : Float;
   cmin, cmax : Real;
   tmin, tmax, tmaxp : Real;
   denmax : Float;
BEGIN { MakeDensitiesPLot (Inelastic) }
   SelectViewPort(1);
   CASE Iselect OF
   1:  EMconsts := Iso0consts;
   2:  EMconsts := Iso1consts;
   3:  EMconsts := Neutconsts;
   4:  EMconsts := Protconsts;
   5:  EMconsts := Nogsconsts;
   6:  EMconsts := Noglconsts;
   END; { CASE }
   MakeHoleQuantumNumbers(HoleSelector, nh, lh, jjh);
   MakeParticleQuantumNumbers(Nparticle, ParticleSelector, np, lp, jjp);
   WITH EMconsts DO
      BEGIN
         ftr := sqrt((jjp + 1)/(2*Jspin + 1));
         fCoul := ftr*gl;
         ftr := ftr*(hbarc/(2*amu));
         fConv := ftr*gl;
         fSpin := ftr*gs;
      END;
   Request1DVector(rhov, NumR);
   Request2DVectors(trans1v, trans2v, NumR);
   Request2DVectors(spin1v, spin2v, NumR);
   DensitiesFound := FALSE;
   IF Even(lp + lh + Jspin) THEN
      BEGIN { Natural parity condition }
         IF Triang(lp, lh, Jspin) THEN
            BEGIN
               { trans1v and trans2v will contain L=J-1 and L=J+1
                 current densities, respectively }
               RaddenN(np, lp, jjp, nh, lh, jjh, Jspin, Aosc(TargetSelector),
                       rv, rhov, trans1v, trans2v, spin1v, spin2v);
               DensitiesFound := TRUE;
               rhov.MultScalar(fCoul);
               rhov.MinMax(imin, cmin, imax, cmax);
               cmax := MaxF(ABS(cmin), ABS(cmax));
               IF Jspin > 0 THEN
                  BEGIN
                     trans1v.SumOfVectors(fConv, trans1v, fSpin, spin1v);
                     trans1v.MinMax(imin, tmin, imax, tmaxp);
                     tmax := MaxF(ABS(tmin), ABS(tmaxp));
                     trans2v.SumOfVectors(fConv, trans2v, fSpin, spin2v);
                     trans2v.MinMax(imin, tmin, imax, tmaxp);
                     tmax := MaxF(ABS(tmin), ABS(tmax));
                     tmax := MaxF(ABS(tmax), ABS(tmaxp));
                  END
               ELSE tmax := 0;
               denmax := 1.1*MaxF(cmax, tmax);
               IF NOT (denmax = 0) THEN
                  BEGIN
                     SetDensityScales(denmax);
                     SetColor(LightGreen);
                     PlotABSDvtr(rv, rhov, 2, NumR);
                     Print(3,2,'Transition charge density');
                     IF Jspin > 0 THEN
                        BEGIN
                           SetColor(LightRed);
                           Print(4,3,'L = J-1 current density');
                           PlotABSDvtr(rv, trans1v, 2, NumR);
                           SetColor(LightBlue);
                           Print(4,4,'L = J+1 current density');
                           PlotABSDvtr(rv, trans2v, 2, NumR);
                        END;
                     SetColor(White);
                  END
               ELSE ExplainNoCurves;
            END
         ELSE ExplainNoCurves;
      END   { Natural parity condition }
   ELSE
      BEGIN { Unnatural parity condition }
         IF Jspin > 0 THEN
            BEGIN
               { trans1v will contain the L=J current density }
               RaddenU(np, lp, jjp, nh, lh, jjh, Jspin, Aosc(TargetSelector),
                       rv, trans1v, spin1v);
               DensitiesFound := TRUE;
               trans1v.SumOfVectors(fConv, trans1v, fSpin, spin1v);
               trans1v.MinMax(imin, tmin, imax, tmax);
               tmax := MaxF(ABS(tmin), ABS(tmax));
               denmax := 1.1*tmax;
               IF NOT (denmax = 0) THEN
                  BEGIN
                     SetDensityScales(denmax);
                     SetColor(LightRed);
                     PlotABSDvtr(rv, trans1v, 2, NumR);
                     Print(5,2,'L = J current density');
                     SetColor(White);
                  END
               ELSE ExplainNoCurves;
            END
         ELSE ExplainNoCurves;
      END;  { Unnatural parity condition }

(*   Begin set up for test calculation   *)
(*   of Fourier transforms               *)
(*                                       *)
(*   IF Even(lp + lh + Jspin) THEN       *)
(*      BEGIN                            *)
(*         testJv.Equate(1,rhov);        *)
(*         testJM1v.Equate(1,trans1v);   *)
(*         testJP1v.Equate(1,trans2v);   *)
(*      END                              *)
(*   ELSE testJv.Equate(1,trans1v);      *)
(*                                       *)
(*   End set up for test calculation     *)

   Release2DVectors(spin1v, spin2v);
   Release2DVectors(trans1v, trans2v);
   Release1DVector(rhov);
END;  { MakeDensitiesPlot (Inelastic) }

PROCEDURE MakeFormFactorPlot(Iselect: Integer; qv: DVector; NumQ: Integer);

VAR
   EMconsts : FFconsts;

   PROCEDURE ExplainNoCurves;

      PROCEDURE DisplayNoneExpectedMessage;
      BEGIN
         Print(10,10,'This state is');
         Print(8,11,'not populated via');
         Print(7,12,'electron scattering');
      END;

      PROCEDURE DisplayNoConvectionCurrentMessage;
      BEGIN
         Print(5,10,'The convection current');
         Print(5,11,'vanishes for this state');
      END;

      PROCEDURE DisplayNoSpinCurrentMessage;
      BEGIN
         Print(8,10,'The spin current');
         Print(5,11,'vanishes for this state');
      END;

   BEGIN
      IF Iselect < 5 THEN DisplayNoneExpectedMessage
      ELSE
         BEGIN
            WITH EMconsts DO
               BEGIN
                  IF gl <> 0 THEN DisplayNoConvectionCurrentMessage;
                  IF gs <> 0 THEN DisplayNoSpinCurrentMessage;
               END;
         END;
   END;

VAR
   nh, lh, jjh : Integer;
   np, lp, jjp : Integer;
   i, imin, imax : Integer;
   coulv, Ftransv : DVector;
   Currv, Spinv  : DVector;
   DensitiesFound : Boolean;
   ftr, fCoul, fConv, fSpin : Float;
   cmin, cmax : Real;
   tmin, tmax : Real;
   FFmax : Float;
BEGIN { MakeFormFactorPlot (Inelastic) }
   SelectViewPort(2);
   CASE Iselect OF
   1:  EMconsts := Iso0consts;
   2:  EMconsts := Iso1consts;
   3:  EMconsts := Neutconsts;
   4:  EMconsts := Protconsts;
   5:  EMconsts := Nogsconsts;
   6:  EMconsts := Noglconsts;
   END; { CASE }
   MakeHoleQuantumNumbers(HoleSelector, nh, lh, jjh);
   MakeParticleQuantumNumbers(Nparticle, ParticleSelector, np, lp, jjp);
   WITH EMconsts DO
      BEGIN
         ftr := sqrt(4*pi*(jjp + 1));
         fCoul := ftr*gl;
         ftr := ftr*(hbarc/(2*amu));
         fConv := ftr*2*gl;
         fSpin := ftr*Phase(lp + lh + Jspin)*gs/2;
      END;
   Request2DVectors(coulv, Ftransv, NumQ);
   Request2DVectors(Currv, Spinv, NumQ);
   DensitiesFound := FALSE;
   IF Even(lp + lh + Jspin) THEN
      BEGIN { Natural parity condition }
         IF Triang(lp, lh, Jspin) THEN
            BEGIN
               MomdenN(np, lp, jjp, nh, lh, jjh, Jspin, Aosc(TargetSelector),
                       qv, coulv, Currv{ Lperpv }, Spinv{ DiagSv });
               DensitiesFound := TRUE;
            END
         ELSE ExplainNoCurves;
      END   { Natural parity condition }
   ELSE
      BEGIN { Unnatural parity condition }
         IF Jspin > 0 THEN
            BEGIN
               MomdenU(np, lp, jjp, nh, lh, jjh, Jspin, Aosc(TargetSelector),
                       qv, Spinv{ Sperpv }, Currv{ DiagLv });
               DensitiesFound := TRUE;
            END
         ELSE ExplainNoCurves;
      END;  { Unnatural parity condition }
   IF DensitiesFound THEN
      BEGIN { Set up and plot Coulomb and transverse form factors }
         coulv.MultScalar(fCoul);
         coulv.MinMax(imin, cmin, imax, cmax);
         cmax := MaxF(ABS(cmin), ABS(cmax));
         Ftransv.SumOfVectors(fConv, Currv, fSpin, Spinv);
         FOR i := 1 TO NumQ DO
            Ftransv.Put(i, qv.Value(i)*Ftransv.Value(i));
         Ftransv.MinMax(imin, tmin, imax, tmax);
         tmax := MaxF(ABS(tmin), ABS(tmax));
         FFmax := 1.1*MaxF(cmax, tmax);
         IF NOT (FFmax = 0) THEN
            BEGIN
               SetFormFactorScales(FFmax);
               IF cmax > 0 THEN
                  BEGIN
                     SetColor(LightGreen);
                     Print(5,2,'Coulomb Form Factor');
                     PlotABSDvtr(qv, coulv, 1, NumQ);
                  END;
               IF tmax > 0 THEN
                  BEGIN
                     SetColor(LightRed);
                     Print(5,3,'Transverse Form Factor');
                     PlotABSDvtr(qv, Ftransv, 1, NumQ);
                  END;
               SetColor(White);
            END
         ELSE ExplainNoCurves;
      END;  { Set up and plot Coulomb and transverse form factors }
   Release2DVectors(Currv, Spinv);
   Release2DVectors(coulv, Ftransv);
END;  { MakeFormFactorPlot (Inelastic) }


FUNCTION Natural: Boolean;
VAR
   nh, lh, jjh, np, lp, jjp{, Nparticle} : Integer;
BEGIN
   MakeHoleQuantumNumbers(HoleSelector, nh, lh, jjh);
   MakeParticleQuantumNumbers(Nparticle, ParticleSelector, np, lp, jjp);
   Natural := Even(lp + lh + Jspin);
END;

PROCEDURE PlotDensitiesAndFormFactors(Iselect: Integer);
VAR
   rv, qv : DVector;
   Jmin, Jmax : Integer;
BEGIN
   ClearMuppetPort;
   PrepareDensitiesAndFormFactorPlot(Iselect);
   NumR := 2*Round( Rmax/(2.0*Rstep) ) + 1;
   Request1DVector(rv, NumR);
   SetGrid(NumR, Rstep, rv);

(* Note:  Values will be assigned to testJv              *)
(*        arrays in MakeDensitiesPlot                    *)
(*   Request3DVectors(testJv, testJM1v, testJP1v, NumR); *)

   MakeDensitiesPlot(Iselect, rv, NumR);
   NumQ := 2*Round( Qmax/(2.0*Qstep) ) + 1;
   Request1DVector(qv, NumQ);
   SetGrid(NumQ, Qstep, qv);
   IF qv.Value(1) = 0 THEN qv.Put(1,qv.Value(2)/100);
   MakeFormFactorPlot(Iselect, qv, NumQ);

(*   FourierTrans1(qv, rv, testJv);                             *)
(*   IF Natural THEN FourierTrans2(qv, rv, testJM1v, testJP1v); *)
(*   Release3DVectors(testJv, testJM1v, testJP1v);              *)

   Release1DVector(qv);
   Release1DVector(rv);
END;

PROCEDURE DensitiesAndFormFactors(Iselect: Integer);
VAR
   key : Byte;
   quit : Boolean;
   Hotkeys1 : THotKeys;

   PROCEDURE SetUpHotKeys;
   BEGIN
      WITH HotKeys1 DO
      BEGIN
         init(4);
         key[1] := 'F1-Help';
         key[2] := 'F3-Prev J';
         key[3] := 'F4-Next J';
         key[4] := 'F10-Menu';
      END;
   END;

   PROCEDURE SetUpScreen;
   BEGIN
      PlotDensitiesAndFormFactors(Iselect);
      Message(
        'Use hot keys to cycle J...  Other keys restore the master menu.');
      HotKeys1.display;
   END;

BEGIN { DensitiesAndFormFactors (Inelastic) }
   SetUpHotKeys;
   SetUpScreen;
   quit := FALSE;
   REPEAT
      CheckForEvents;
      IF HotKeys1.Pressed(key) THEN
         BEGIN
            CASE key OF
               1 : ShowHelp('elscatt.hlp','CycleJspinInfoScreen');
               2 : Dec(Jspin);
               3 : Inc(Jspin);
               4 : quit := TRUE;
            END;
            IF (key = 2) OR (key = 3) THEN
               BEGIN
                  CycleJspin;
                  SetUpScreen;
               END;
         END
      ELSE
         IF Event.KeyPressed OR Event.MouseClicked THEN quit := TRUE;
   UNTIL quit;
END;  { DensitiesAndFormFactors (Inelastic) }

PROCEDURE ChooseExcitationType;
VAR
   TypeMenu : TBarMenu;
BEGIN { ChooseExcitationType (Inelastic) }
   OpenMenu(TypeMenu);
   ClearMuppetPort;
   Message('Choose one of the excitation types listed below:-');
   WITH TypeMenu DO
   BEGIN
      init;
      Row[1] := 'T = 0 excitation';
      Row[2] := 'T = 1 excitation';
      Row[3] := 'Neutron excitation';
      Row[4] := 'Proton excitation';
      Row[5] := 'Proton excitation with gs = 0';
      Row[6] := 'Proton excitation with gl = 0';
      Draw(0.4, 0.9, 6, Iselect);
   END;
   {ShowMouse;}
   Iselect := TypeMenu.chosen;
   CloseMenu(TypeMenu);
   DensitiesAndFormFactors(Iselect);
END;

FUNCTION Occupied: Boolean;
{ Check whether selected particle orbit is
  already occupied in the target nucleus }
VAR
   i, np, lp, jjp : Integer;
   testocc : Boolean;
BEGIN
   testocc := FALSE;
   MakeParticleQuantumNumbers(Nparticle, ParticleSelector, np, lp, jjp);
   i := 0;
   REPEAT
      Inc(i);
      WITH SinglPclArray[i] DO
         testocc := (np = nsp) AND (lp = lsp) AND (jjp = jjsp);
   UNTIL testocc OR (i = TargetSelector);
   Occupied := testocc;
END;


PROCEDURE ChooseTarget;
VAR
   itarg, np, lp, jjp : Integer;
   TargMenu : TBarMenu;
BEGIN { ChooseTarget (Inelastic) }
   OpenMenu(TargMenu);
   ClearMuppetPort;
   Message('Choose one of the closed-shell targets listed below:-');
   WITH TargMenu DO
   BEGIN
      init;
      FOR itarg := 1 TO 6 DO Row[itarg] := MakeTargetString(itarg);
      Draw(0.4, 0.9, 6, TargetSelector);
   END;
   TargetSelector   := TargMenu.chosen;
   HoleSelector     := TargetSelector;     { Uppermost hole state }
   IF Occupied THEN
      BEGIN
         ParticleSelector := TargetSelector + 1; { Bottommost particle  }
         MakeHoleQuantumNumbers(ParticleSelector, np, lp, jjp);
         Nparticle := 2*(np - 1) + lp;
         ParticleSelector := FindNumOrbit(np, lp, jjp);
      END;
   ResetJspin;
   CloseMenu(TargMenu);
END;  { ChooseTarget (Inelastic) }


PROCEDURE ChooseHoleState;
VAR
   HoleMenu : TBarMenu;
   Icount   : Integer;
   NumOrbit : Integer;
   nh, lh, jjh : Integer;
BEGIN { ChooseHoleState (Inelastic) }
   OpenMenu(HoleMenu);
   ClearMuppetPort;
   Message('Choose one of the hole states listed below:-');
   WITH HoleMenu DO
   BEGIN
      init;
      Icount := 0;
      FOR NumOrbit := TargetSelector DOWNTO 1 DO
      BEGIN
         Row[TargetSelector + 1 - NumOrbit] :=
            Concat('nlj(hole) = ', MakeHoleString(NumOrbit));
         Icount := Icount + 1;
      END;
      Draw(0.35, 0.9, Icount, TargetSelector + 1 - HoleSelector);
   END;
   HoleSelector   := TargetSelector + 1 - HoleMenu.chosen;
   ResetJspin;
   CloseMenu(HoleMenu);
   DensitiesAndFormFactors(Iselect);
END;  { ChooseHoleState (Inelastic) }


FUNCTION ChooseNparticle: Boolean;
{ Request input value for Nparticle - the number
  of oscillator quanta in the particle state }
VAR
   NScreen : TInputScreen;
BEGIN
   OpenScrn(NScreen);
   WITH NScreen DO
   BEGIN
      Init;
      DefineInputport(  0.26,0.74,  0.38,0.68);
      LoadLine('');
      LoadLine(' Enter the number of oscillator     ');
      LoadLine(' quanta in the particle state  {   }');
      LoadLine('(An integer n in range 1 <= n <= 10)');
      LoadLine('');
      LoadLine('         [  Ok  ]  [Cancel]');
      SetNumber(1, Nparticle);
      SetNumberLimits(1, 1, 10);
      Accept;
      IF GetBoolean(2) THEN
         BEGIN
            Nparticle := round(GetNumber(1));
            ChooseNparticle := TRUE;
         END
      ELSE ChooseNparticle := FALSE;
      done;
   END;
   CloseScrn(NScreen);
   ClearMuppetPort;
END;


PROCEDURE ChooseParticleState;
VAR
   ParticleMenu            : TBarMenu;
   np, lp, jjp             : Integer;
   MaxNumOrbit, NumOrbit   : Integer;
   ParticleStateOK, cancel : Boolean;
BEGIN { ChooseParticleState ( Inelastic ) }
   ClearMuppetPort;
   REPEAT
      cancel := NOT ChooseNparticle;
      IF NOT cancel THEN
         BEGIN
            MaxNumOrbit := Nparticle + 1;
            Message('Choose one of the particle states listed below:-');
            OpenMenu(ParticleMenu);
            WITH ParticleMenu DO
            BEGIN
               FOR NumOrbit := 1 TO MaxNumOrbit DO
               BEGIN
                  Row[NumOrbit] := Concat('nlj(particle) = ',
                     MakeParticleString(Nparticle, NumOrbit));
               END;
               Draw(0.35, 0.9, MaxNumOrbit, 1);
            END;
            ParticleSelector := ParticleMenu.chosen;
            CloseMenu(ParticleMenu);
            IF Occupied THEN
               BEGIN
                  ClearMuppetPort;
                  Message(
      'Chosen state is occupied in the target nucleus.  Try again...');
                  ParticleStateOK := FALSE;
               END
            ELSE ParticleStateOK := TRUE;
         END;
   UNTIL cancel OR ParticleStateOK;
   IF NOT cancel THEN
      BEGIN
         ResetJspin;
         DensitiesAndFormFactors(Iselect);
      END;
END;  { ChooseParticleState ( Inelastic ) }


PROCEDURE ChooseSpin;
VAR
   SpinMenu : TBarMenu;
   nh, lh, jjh : Integer;
   np, lp, jjp : Integer;
   Jmin, Jmax, J : Integer;
   i : Integer;
BEGIN { ChooseSpin (Inelastic) }
   OpenMenu(SpinMenu);
   ClearMuppetPort;
   MakeHoleQuantumNumbers(HoleSelector, nh, lh, jjh);
   MakeParticleQuantumNumbers(Nparticle, ParticleSelector, np, lp, jjp);
   Jmin := ABS(Round((jjp - jjh)/2));
   Jmax := Round((jjp + jjh)/2);
   IF Jspin < Jmin THEN Jspin := Jmin;
   IF Jspin > Jmax THEN Jspin := Jmax;
   Message('Choose one of the particle-hole states listed below:-');
   WITH SpinMenu DO
   BEGIN
      init;
      i := 0;
      FOR J := Jmin TO Jmax DO
      BEGIN
         Inc(i);
         Row[i] := Concat(' [ particle( ',
                      MakeParticleString(Nparticle, ParticleSelector),
                       ' ) x hole( ', MakeHoleString(HoleSelector),
                       ' ) ] J =', NumStr(J,3,0));
      END;
      Draw(0.20, 0.9, i, Jspin - Jmin + 1);
   END;
   Jspin := SpinMenu.chosen + Jmin - 1;
   CloseMenu(SpinMenu);
   DensitiesAndFormFactors(Iselect);
END;  { ChooseSpin (Inelastic) }


PROCEDURE SetUpResultsScreen(CoulombFF, TransverseFF, gl, gs: Float;
                                        DeltaN: Integer);
VAR
   oscEnergy : Float;
BEGIN
   ShowInelasticTitle;
   DefineViewport(1, 0.035, 0.935, 0.15, 0.85);
   OpenViewPort(1);
   CASE Iselect OF
   1:  Print(26,2,'** T = 0 EXCITATION **');
   2:  Print(26,2,'** T = 1 EXCITATION **');
   3:  Print(26,2,'** NEUTRON EXCITATION **');
   4:  Print(26,2,'** PROTON EXCITATION **');
   5:  Print(22,2,'** PROTON Convection Current **');
   6:  Print(25,2,'** PROTON Spin Current **');
   END; { CASE }
   Print(21,3,Concat('Form Factors at q = ', NumStr(qValueInel,8,5), ' /fm'));
   SetColor(LightGreen);
   Print(20,6,Concat('Coulomb Form Factor    =',
                     NumStr(CoulombFF,10,5)));
   SetColor(LightRed);
   Print(20,7,Concat('Transverse Form Factor =',
                     NumStr(TransverseFF,10,5)));
   SetColor(White);
   IF Natural THEN
      Print(12,10,Concat('This is a ', NumStr(DeltaN,2,0),
         ' hbar omega NATURAL parity transition'))
   ELSE
      Print(11,10,Concat('This is a ', NumStr(DeltaN,2,0),
         ' hbar omega UNNATURAL parity transition'));
   Print(29,11,'with parameters');
   Print(22,13,Concat('Oscillator length =',
                      NumStr(Aosc(TargetSelector),6,3), ' fm'));
   oscEnergy := sqr(hbarc/Aosc(TargetSelector))/amu;
   Print(25,14,Concat('hbar omega = ', NumStr(oscEnergy,6,3), ' MeV'));
   Print(24,15,Concat('gl = ', NumStr(gl,6,3), '   gs = ', NumStr(gs,6,3)));
END;

PROCEDURE CalcAndDisplayResults(Iselect: Integer);
{ Modified version of MakeFormFactorPlot that calculates and }
{ displays numerical results for the Coulomb and transverse  }
{ form factors at a single q-value.                          }
VAR
   EMconsts : FFconsts;
   nh, lh, jjh : Integer;
   np, lp, jjp : Integer;
   DeltaN      : Integer;
   qv : DVector;
   coulv, Ftransv : DVector;
   Currv, Spinv  : DVector;
   DensitiesFound : Boolean;
   ftr, fCoul, fConv, fSpin : Float;
   CoulombFF, TransverseFF  : Float;
BEGIN { CalcAndDisplayResults (Inelastic) }
   CASE Iselect OF
   1:  EMconsts := Iso0consts;
   2:  EMconsts := Iso1consts;
   3:  EMconsts := Neutconsts;
   4:  EMconsts := Protconsts;
   5:  EMconsts := Nogsconsts;
   6:  EMconsts := Noglconsts;
   END; { CASE }
   MakeHoleQuantumNumbers(HoleSelector, nh, lh, jjh);
   MakeParticleQuantumNumbers(Nparticle, ParticleSelector, np, lp, jjp);
   WITH EMconsts DO
      BEGIN
         ftr := sqrt(4*pi*(jjp + 1));
         fCoul := ftr*gl;
         ftr := ftr*(hbarc/(2*amu));
         fConv := ftr*2*gl;
         fSpin := ftr*Phase(lp + lh + Jspin)*gs/2;
      END;
   Request1DVector(qv, 1);
   qv.Put(1, qValueInel);
   Request2DVectors(coulv, Ftransv, 1);
   Request2DVectors(Currv, Spinv, 1);
   DensitiesFound := FALSE;
   IF Even(lp + lh + Jspin) THEN
      BEGIN { Natural parity condition }
         IF Triang(lp, lh, Jspin) THEN
            BEGIN
               MomdenN(np, lp, jjp, nh, lh, jjh, Jspin, Aosc(TargetSelector),
                       qv, coulv, Currv{ Lperpv }, Spinv{ DiagSv });
               DensitiesFound := TRUE;
            END;
      END   { Natural parity condition }
   ELSE
      BEGIN { Unnatural parity condition }
         IF Jspin > 0 THEN
            BEGIN
               MomdenU(np, lp, jjp, nh, lh, jjh, Jspin, Aosc(TargetSelector),
                       qv, Spinv{ Sperpv }, Currv{ DiagLv });
               DensitiesFound := TRUE;
            END;
      END;  { Unnatural parity condition }
   IF DensitiesFound THEN
      BEGIN
         CoulombFF := fCoul*coulv.Value(1);
         TransverseFF := qValueInel*(fconv*Currv.Value(1)
                                     + fSpin*Spinv.Value(1));
      END
   ELSE
      BEGIN
         CoulombFF := 0;
         TransverseFF := 0;
      END;
   DeltaN := 2*(np - nh) + lp - lh;
   SetUpResultsScreen(CoulombFF, TransverseFF,
                      EMconsts.gl, EMconsts.gs, DeltaN);
   Release2DVectors(Currv, Spinv);
   Release2DVectors(coulv, Ftransv);
   Release1DVector(qv);
END;  { CalcAndDisplayResults (Inelastic) }

FUNCTION GetQValue: Boolean;
VAR
   QScreen : TInputScreen;
BEGIN
   OpenScrn(QScreen);
   WITH QScreen DO
   BEGIN
      Init;
      DefineInputport(  0.26,0.74,  0.35,0.65);
      LoadLine('');
      LoadLine('        Enter desired value');
      LoadLine('        of q {       } 1/fm  ');
      LoadLine('   (Limits are 0.00001 <= q <= 5)');
      LoadLine('');
      LoadLine('         [  Ok  ]  [Cancel]');
      SetNumber(1,qValueInel);
      SetNumberLimits(1, 0.00001, 5);
      AcceptScreen;
      qValueInel := GetNumber(1);
      GetQValue := NOT canceled;
      Done;
   END;
   CloseScrn(QScreen);
END;

PROCEDURE NumericalFormFactors(Iselect: Integer);
VAR
   key : Byte;
   quit : Boolean;
   Hotkeys1 : THotKeys;

   PROCEDURE SetUpHotKeys;
   BEGIN
      WITH HotKeys1 DO
      BEGIN
         init(5);
         key[1] := 'F1-Help';
         key[2] := 'F3-Prev J';
         key[3] := 'F4-Next J';
         key[4] := 'F5-Change q';
         key[5] := 'F10-Menu';
      END;
   END;

   PROCEDURE SetUpScreen;
   BEGIN
      CalcAndDisplayResults(Iselect);
      Message(Concat('Use hot keys to cycle J or change q...  ',
                     'Other keys restore the master menu.'));
      HotKeys1.display;
   END;

BEGIN { NumericalFormFactors (Inelastic) }
   SetUpHotKeys;
   SetUpScreen;
   quit := FALSE;
   REPEAT
      CheckForEvents;
      IF HotKeys1.Pressed(key) THEN
         BEGIN
            CASE key OF
               1 : ShowHelp('elscatt.hlp','NumericalResultsInfoScreen');
               2 : Dec(Jspin);
               3 : Inc(Jspin);
               4 : IF GetQValue THEN SetUpScreen;
               5 : quit := TRUE;
            END;
            IF (key = 2) OR (key = 3) THEN
               BEGIN
                  CycleJspin;
                  SetUpScreen;
               END;
         END
      ELSE
         IF Event.KeyPressed OR Event.MouseClicked THEN quit := TRUE;
   UNTIL quit;
END;  { NumericalFormFactors (Inelastic) }


PROCEDURE NumericalResults;
BEGIN { NumericalResults (Inelastic) }
   NumQ := 1;
   IF GetQValue THEN
      BEGIN
         ClearMuppetPort;
         NumericalFormFactors(Iselect);
      END;
END;  { NumericalResults (Inelastic) }

VAR
   np, lp, jjp : Integer;

BEGIN { Inelastic }
 WITH MasterMenu DO
    BEGIN
       IF RowChosen > 0 THEN
          BEGIN
             IF HoleSelector > TargetSelector THEN
                BEGIN
                   HoleSelector := TargetSelector;
                   Jspin := 0;
                   AdjustJspin;
                END;
             IF Occupied THEN
                BEGIN
                   ParticleSelector := TargetSelector + 1;
                   MakeHoleQuantumNumbers(ParticleSelector, np, lp, jjp);
                   Nparticle := 2*(np - 1) + lp;
                   ParticleSelector := FindNumOrbit(np, lp, jjp);
                   Jspin := 0;
                   AdjustJspin;
                END;
          END;
       CASE RowChosen OF
     0: Initialize;
     1: ShowHelp('elscatt.hlp','InelasticInfoScreen');
     2: ChooseExcitationType;
     3: BEGIN
           ChooseTarget;
           ChooseHoleState;
        END;
     4: ChooseParticleState;
     5: ChooseSpin;
     6: NumericalResults;
       END; {CASE}
    END;
END;  { Inelastic }


{ ------------------------ Begin RESPONSE Procedure ----------------------- }


VAR
   qValueResp : Float;

PROCEDURE Response;

CONST
   MaxDeltaN = 20;
   MaxJspin  = 10;

VAR
   WaitMessage : TWaitMessage;

PROCEDURE Initialize;
BEGIN { Initialize (Response) }
   rootpiby4 := 0.25*sqrt(pi);
   TargetSelector := 3;
   qValueResp := 2;
   MakeJFacts;
END;  { Initialize (Response) }

FUNCTION Expectation(Aosc, q: Float): Float;
{ Calculate ground state expectation of
  the operator  exp( i q dot (r1 - r2) )  }
VAR
   isp, jsp : Integer;
   Jmin, Jmax, J : Integer;
   n1, l1, jj1, n2, l2, jj2 : Integer;
   qv, rhoQv : DVector;
   sum, direct, exchange, term : Float;
BEGIN
   Request2DVectors(qv, rhoQv, 1);
   qv.Put(1, q);
   { Contribution for r1 = r2 (both protons the same) }
   sum := 0.5*TargetMassNumber(TargetSelector);
   { Contributions for r1 <> r2 (two different protons) }
   FOR isp := 1 TO TargetSelector DO
   BEGIN
      n1  := SinglPclArray[isp].nsp;
      l1  := SinglPclArray[isp].lsp;
      jj1 := SinglPclArray[isp].jjsp;
      FOR jsp := 1 TO TargetSelector DO
      BEGIN
         n2  := SinglPclArray[jsp].nsp;
         l2  := SinglPclArray[jsp].lsp;
         jj2 := SinglPclArray[jsp].jjsp;
         J := 0;  { Direct Term }
         direct := 4*pi;
         MomDensity(n1, l1, jj1, n1, l1, jj1, J, Aosc, qv, rhoQv);
         direct := direct*(jj1 + 1)*rhoQv.Value(1);
         MomDensity(n1, l2, jj2, n2, l2, jj2, J, Aosc, qv, rhoQv);
         direct := direct*(jj2 + 1)*rhoQv.Value(1);
         exchange := 0;  { Exchange Term }
         Jmin := MaxI(ABS(l1 - l2), ABS(Round((jj1 - jj2)/2)));
         Jmax := MinI(l1 + l2, Round((jj1 + jj2)/2));
         FOR J := Jmin TO Jmax DO
         BEGIN { Loop over J values }
            IF Even(l1 + l2 + J) THEN
            BEGIN { Parity OK }
               term := 4*pi*Phase(Round((jj1 - jj2)/2));
               MomDensity(n1, l1, jj1, n2, l2, jj2, J, Aosc, qv, rhoQv);
               term := term*sqrt(jj1 + 1)*rhoQv.Value(1);
               MomDensity(n2, l2, jj2, n1, l1, jj1, J, Aosc, qv, rhoQv);
               term := term*sqrt(jj2 + 1)*rhoQv.Value(1);
               exchange := exchange + term;
            END;  { Parity OK }
         END;  { Loop over J values }
         term := direct - exchange;
         sum := sum + term;
      END;
   END;
   Expectation := sum;
   Release2DVectors(qv, rhoQv);
END;

PROCEDURE FindStrengths(Aosc, q: Float; VAR StrengthTable: DMatrix);
{ Set up the strength table for a given q }
VAR
   ParticleIndex, Nparticle, MaxPcleIndex    : Integer;
   DeltaN, HoleIndex, MinNhole, Ncore, Nhole : Integer;
   nh, lh, jjh, np, lp, jjp, J, Jmin, Jmax   : Integer;
   qv, rhoQv : DVector;
   term : Float;
   particleOK : Boolean;
BEGIN
   Request2DVectors(qv, rhoQv, 1);
   qv.Put(1, q);

{ Don't include elastic contribution here! }
{   ElasticFormFactor(qv, rhoQv);
   StrengthTable.Put(1, 1, Sqr(rhoQv.Value(1))); }

   WITH SinglPclArray[TargetSelector] DO Ncore := 2*(nsp - 1) + lsp;
   FOR DeltaN := 0 TO MaxDeltaN DO
   BEGIN { Loop over excitation energies }
      MinNhole := Ncore - DeltaN;
      FOR HoleIndex := 1 TO TargetSelector DO
      BEGIN { Loop over hole states }
         WITH SinglPclArray[HoleIndex] DO Nhole := 2*(nsp - 1) + lsp;
         IF NOT (Nhole < MinNhole) THEN
         BEGIN { Hole state energy OK }
            MakeHoleQuantumNumbers(HoleIndex, nh, lh, jjh);
            Nparticle := Nhole + DeltaN;
            MaxPcleIndex := Nparticle + 1;
            FOR ParticleIndex := 1 TO MaxPcleIndex DO
            BEGIN { Loop over particle states }
               IF Nparticle > Ncore THEN particleOK := TRUE
               ELSE particleOK := NOT InCore(Nparticle, ParticleIndex);
               IF particleOK THEN
               BEGIN { Particle state OK }
                  WaitMessage.Update;
                  MakeParticleQuantumNumbers(Nparticle, ParticleIndex, np, lp, jjp);
                  Jmin := MaxI(ABS(lp - lh), ABS(Round((jjp - jjh)/2)));
                  Jmax := MinI(lp + lh, Round((jjp + jjh)/2));
                  FOR J := Jmin TO Jmax DO
                  BEGIN { Loop over J values }
                     IF Even(lp + lh + J) and (NOT (J > MaxJspin)) THEN
                     BEGIN { Parity condition OK  Update the strength table }
                        MomDensity(np, lp, jjp, nh, lh, jjh, J,
                                   Aosc, qv, rhoQv);
                        term := 4*pi*(jjp + 1)*sqr(rhoQv.Value(1));
                        StrengthTable.Put(DeltaN + 1, J + 1,
                        StrengthTable.Value(DeltaN + 1, J + 1) + term);
                     END;  { Parity condition OK }
                  END;  { Loop over J values }
               END;  { Particle state OK }
            END;  { Loop over particle states }
         END;  { Hole state energy OK }
      END;  { Loop over hole states }
   END;  { Loop over excitation energies }
   Release2DVectors(qv, rhoQv);
END;

PROCEDURE AnalyzeStrengths(StrengthTable: DMatrix; OscEnergy: Float;
                VAR Strength, EJbar, DeltaEJ: DVector);
VAR
   i, j : Integer;
   Jsum, EJsum, E2Jsum : Float;
BEGIN
   FOR j := 1 TO MaxJspin + 1 DO
   BEGIN
      WaitMessage.Update;
      Jsum := 0;  EJsum := 0;  E2Jsum := 0;
      FOR i := 1 TO MaxDeltaN + 1 DO
      BEGIN
         Jsum := Jsum + StrengthTable.Value(i, j);
         EJsum := EJsum + (i - 1)*StrengthTable.Value(i, j);
         E2Jsum := E2Jsum + sqr(i - 1)*StrengthTable.Value(i, j);
      END;
      EJsum := OscEnergy*EJsum;
      E2Jsum := sqr(OscEnergy)*E2Jsum;
      IF Jsum <> 0 THEN
      BEGIN
         Strength.Put(j, Jsum);
         EJbar.Put(j, EJsum/Jsum);
         DeltaEJ.Put(j, Sqrt(E2Jsum/Jsum - sqr(EJbar.Value(j))));
      END;
   END;
END;

PROCEDURE PrepareFixedQResponsePlot;
BEGIN
   DefineViewport(1, 0.07, 0.97, 0.15, 0.93);
   OpenViewPort(1);
   Print(18,1,'Approximate Inelastic Coulomb Response');
   Print(25,2,Concat('at q = ', NumStr(qValueResp,4,2),
         ' /fm on ', MakeTargetString(TargetSelector)));
   PutLabel(Bottom,'hbar omega (MeV)');
   PutLabel(Left,'R(q,omega) (1/MeV)');
END;

PROCEDURE PlotLabel(xValue, yValue: Float;
                    xshift, yshift: Integer; title : string);
{ Write text at arbitrary position on screen display }
{ Adapted from MUPPET's PutLabel procedure - RJP May 92 }

VAR x, y : integer;
BEGIN
   HideMouse;
   SetTextJustify(CenterText, CenterText);
   Map(xValue, yValue, x, y);
   OutTextXY(x + xshift, y - yshift, title);
   SetTextJustify(LeftText,TopText);
   ShowMouse;
END;

PROCEDURE PlotGaussian(J: Integer; StrengthJ, EJbar, DeltaEJ: Float);
CONST
   Npoints = 60;
VAR
   i : Integer;
   xv, yv      : DVector;
   norm, ftr, E : Float;
   Emin, Emax, dE : Float;
BEGIN
   IF DeltaEJ > 0 THEN
      BEGIN
         Request2DVectors(xv, yv, Npoints);
         norm := sqrt(2/pi)*StrengthJ/DeltaEJ;
         ftr := -0.5/sqr(DeltaEJ);
         Emin := EJbar - 3*DeltaEJ;
         Emax := EJbar + 3*DeltaEJ;
         dE := (Emax - Emin)/(Npoints - 1);
         E := Emin - dE;
         IF NOT (Emin < 0) THEN
          BEGIN
             FOR i := 1 TO Npoints DO
             BEGIN
                E := E + dE;
                xv.Put(i, E);
                yv.Put(i, norm*exp(ftr*sqr(E - EJbar)))
             END;
          END
         ELSE
          BEGIN
             FOR i := 1 TO Npoints DO
             BEGIN
                E := E + dE;
                xv.Put(i, E);
                IF E < 0 THEN yv.Put(i, 0)
                ELSE IF E < -Emin THEN yv.Put(i, norm
                  *(exp(ftr*sqr(E - EJbar)) - exp(ftr*sqr(E + EJbar))))
                ELSE yv.Put(i, norm*exp(ftr*sqr(E - EJbar)));
             END;
          END;
         PlotDVectorsJP(xv, yv, 1, Npoints);
         PlotLabel(EJbar, norm, 0, 10, Concat('J=', NumStr(J,2,0)));
         Release2DVectors(xv, yv);
      END;
END;

FUNCTION CheckSumRule(Strength: DVector): Float;
VAR
   j : Integer;
   sum : Float;
   qv, rhoQv : DVector;
BEGIN
{  Calculate the elastic scattering contribution }
   Request2DVectors(qv, rhoQv, 1);
   qv.Put(1, qValueResp);
   ElasticFormFactor(qv, rhoQv);
   sum := Sqr(rhoQv.Value(1));
   Release2DVectors(qv, rhoQv);
{  Add the summed inelastic scattering contributions }
   FOR j := 1 TO MaxJspin + 1 DO sum := sum + Strength.Value(j);
   CheckSumRule := sum;
END;

PROCEDURE PlotFixedQResponse(OscEnergy: Float;
              Strength, EJbar, DeltaEJ: DVector);
VAR
   j : Integer;
   MaxStrength, MaxEnergy : Float;
   struckNucleonKE, percent : Float;
BEGIN
   MaxStrength := 0;
   MaxEnergy := 0;
   FOR j := 1 TO MaxJspin + 1 DO
   BEGIN
      IF DeltaEJ.Value(j) <> 0 THEN
      BEGIN
         MaxStrength :=
            MaxF(MaxStrength, Strength.Value(j)/DeltaEJ.Value(j));
         MaxEnergy :=
            MaxF(MaxEnergy, Ejbar.Value(j) + 2*DeltaEJ.Value(j));
      END;
   END;
   DefineScale(1, 0, MaxEnergy, 0, 1.2*MaxStrength);
   SelectScale(1);
   Axis( 0.0, 0.0, TickSpace(MaxEnergy), TickSpace(MaxStrength) );
   SetColor(Yellow);
   FOR j := 1 TO MaxJspin + 1 DO
      PlotGaussian(j - 1,
                   Strength.Value(j), EJbar.Value(j), DeltaEJ.Value(j));
   SetColor(White);
   percent := 100*CheckSumRule(Strength)
                 /Expectation(Aosc(TargetSelector), qValueResp);
   Print(17,3,Concat('(', NumStr(percent,3,0),
              ' percent of Coulomb sum rule found)'));
   SetColor(LightRed);
   struckNucleonKE := Sqr(hbarc*qValueResp)/(2*amu);
   Plotline(struckNucleonKE, 0.25*MaxStrength,
            struckNucleonKE, 0.90*MaxStrength);
   PlotLabel(struckNucleonKE, MaxStrength, 0, -10, 'Struck nucleon');
   PlotLabel(struckNucleonKE, MaxStrength, 0, -20, 'kinetic energy');
   SetColor(White);
END;

PROCEDURE FixedQ;
VAR
   StrengthTable : DMatrix;
   OscEnergy : Float;
   Strength, EJbar, DeltaEJ : DVector;
BEGIN { FixedQ (Response) }
   ClearMuppetPort;
   NumQ := 1;
   Request1DMatrix(StrengthTable, MaxDeltaN + 1, MaxJspin + 1);
   Request3DVectors(Strength, EJbar, DeltaEJ, MaxJspin + 1);
   PrepareFixedQResponsePlot;
   WaitMessage.Show;
   FindStrengths(Aosc(TargetSelector), qValueResp, StrengthTable);
   OscEnergy := sqr(hbarc/Aosc(TargetSelector))/amu;
   AnalyzeStrengths(StrengthTable, OscEnergy, Strength, EJbar, DeltaEJ);
   WaitMessage.Hide;
   SetColor(GraphBackColor);
   Print(25,10,'Calculations in progress');
   Print(25,11,'    Please wait...');
   PlotFixedQresponse(OscEnergy, Strength, EJbar, DeltaEJ);
   Release3DVectors(Strength, EJbar, DeltaEJ);
   Release1DMatrix(StrengthTable);
END;  { FixedQ (Response) }

PROCEDURE ChooseQAndTarget;
VAR
   QScreen : TInputScreen;
   line    : string;
   itarg   : integer;
BEGIN { ChooseQAndTarget (Response) }
   OpenScrn(QScreen);
   WITH QScreen DO
   BEGIN
      Init;
      DefineInputport(  0.26,0.74,  0.32,0.68);
      LoadLine('Closed-shell targets:');
      line := '';
      FOR itarg := 1 TO 3 DO
         line := Concat(line, ' #1 ', MakeTargetString(itarg), '   ');
      LoadLine(line);
      line := '';
      FOR itarg := 4 TO 6 DO
         line := Concat(line, ' #1 ', MakeTargetString(itarg), '   ');
      LoadLine(line);
      LoadLine('');
      LoadLine('      Value of q {    } 1/fm   ');
      LoadLine('    (Limits are 0.01 <= q <= 4)');
      LoadLine('');
      LoadLine('        [  Ok  ]  [Cancel]');
      SetRadioButton('1',TargetSelector);
      SetNumber(7,qValueResp);
      SetNumberLimits(7,0.01,4);
      AcceptScreen;
      TargetSelector  := GetRadioButton('1');
      qValueResp := GetNumber(7);
      IF NOT Canceled THEN FixedQ;
      Done;
   END;
   CloseScrn(QScreen);
END;  { ChooseQAndTarget (Response) }

BEGIN { Response }
 WITH MasterMenu DO
    CASE RowChosen OF
  0: Initialize;
  1: ShowHelp('elscatt.hlp','ResponseInfoScreen');
  2: ChooseQAndTarget;
  3: FixedQ;
    END; {CASE}
END;  { Response }

PROCEDURE InitialCreditScreen;
VAR
   helpScr: HelpScrType;
BEGIN
   helpScr[ 1] := '                                                  ';
   helpScr[ 2] := '         THE ELECTRON SCATTERING PROBLEM          ';
   helpScr[ 3] := '                                                  ';
   helpScr[ 4] := '                        by                        ';
   helpScr[ 5] := '                                                  ';
   helpScr[ 6] := '                  John Philpott                   ';
   helpScr[ 7] := '           The Florida State University           ';
   helpScr[ 8] := '                                                  ';
   helpScr[ 9] := '                Copyright (c) 1994                ';
   helpScr[10] := '                                                  ';
   helpScr[11] := '                                                  ';
   helpScr[12] := '    This program allows the user to look at       ';
   helpScr[13] := '    various aspects of electron scattering from   ';
   helpScr[14] := '    nuclei.  Some important connections between   ';
   helpScr[15] := '    kinematic observables are illustrated.  You   ';
   helpScr[16] := '    can also plot elastic and inelastic transi-   ';
   helpScr[17] := '    tion densities and form factors for scat-     ';
   helpScr[18] := '    tering from various closed shell nuclei.      ';
   helpScr[19] := '    The nuclear Coulomb response is determined    ';
   helpScr[20] := '    within a simple model.  This simulation can   ';
   helpScr[21] := '    be used to show how detailed nuclear struc-   ';
   helpScr[22] := '    ture information can be deduced from elec-    ';
   helpScr[23] := '    tron scattering measurements.                 ';
   helpScr[24] := '                                                  ';
   helpScr[25] := '                                                  ';
   Help(helpScr);
END;

{ -------------------------- Handle Master Menu --------------------------- }

VAR
   quit : Boolean;

PROCEDURE HandleMenu;
BEGIN { HandleMenu }
   WITH MasterMenu DO
   BEGIN { WITH }
      CASE ColChosen OF
         1: CASE RowChosen OF
             1: AboutCUPS;
             2: InitialCreditScreen;
             3: Configuration;
             5: quit := TRUE;
            END;
         2: Kinematics;
         3: Elastic;
         4: Inelastic;
         5: Response;
      END; { CASE }
      IF RowChosen <> 0 THEN
      BEGIN
{        AutoChecking(ColChosen,RowChosen);}
         MasterMenu.Display;
         HotKeys.Display;
      END;
   END;  { WITH }
END;  { HandleMenu }

{ ------------------------ Handle Master Hot Keys ------------------------- }

PROCEDURE HandleHotKeys (key :byte);
BEGIN
   CASE key OF
    1 : ShowHelp('elscatt.hlp','GeneralHelpScreen');
    2 : IF MasterMenu.Chosen Then HandleMenu;
   END; {case}
END;

{ ----------------------------- MAIN PROGRAM ------------------------------ }

VAR
 key   :Byte;

BEGIN { Main Program }
   CUPSinit;
   SetUpMasterMenu;
   SetUpHotKeys;
   InitialCreditScreen;
   GraphBackColor := Black;

   WITH MasterMenu DO
   BEGIN { Initialize each program section }
      RowChosen := 0;
      FOR key := 2 TO 5 DO
      BEGIN
         ColChosen := key;
         HandleMenu;
      END;
   END;  { Initialize each program section }

   quit := FALSE;
   REPEAT       { main program loop }
      CheckForEvents;
      IF MasterMenu.Activated THEN HandleMenu;
      IF HotKeys.Pressed(key) THEN HandleHotKeys(key);
   UNTIL quit; { main program loop }
   CUPSdone;
END.  { Main Program }

{ ************************************************************************** }
