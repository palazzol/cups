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


UNIT SPIN_LAB;
{$N+}
                    {*********************************}
                    {*    Two-Nucleon Simulation     *}
                    {*         SPIN_LAB.PAS          *}
                    {*   Written by John Philpott    *}
                    {*        July XX, 1993.         *}
                    {*   Copyright (c) 1992, 1993    *}
                    {*********************************}
 
INTERFACE
 
USES
      crt, 
      graph, CUPS, CUPSmupp, CUPSfunc,
      CUPSproc, CUPSgui, CUPSgrph, Consts, Master;

{ --------------------------- Type Declarations --------------------------- }

TYPE
   ThreeVec = RECORD
                 x, y, z : Float;
              END;

TYPE
   SetupType = RECORD
                  ProjPol   : Integer;
                  TargPol   : Integer;
                  DetectPol : Integer;
               END;
   SetupArray = ARRAY[1..2] OF SetupType;

{ -------------------- Global Constants and Variables --------------------- }

VAR { Set in MakeScreenProjectors - Used in MakeScreenCoords }
   xscvec, yscvec, eyevec : ThreeVec;
   eyedist                : Float;

   eyepoint : ThreeVec;
   setup    : SetupArray;

PROCEDURE InitializeSpinlabConstants;
PROCEDURE SpinLab;
 
IMPLEMENTATION
 
PROCEDURE InitializeSpinlabConstants;

   PROCEDURE InitializeSetup(VAR setup: SetupType;
                             arg1, arg2, arg3: Integer);
   BEGIN
      WITH setup DO
         BEGIN
            ProjPol   := arg1;
            TargPol   := arg2;
            DetectPol := arg3;
         END;
   END;

BEGIN
   WITH eyepoint DO
      BEGIN
          x :=  40;
          y := -15;
          z :=  20;
      END;
   InitializeSetup(setup[1], 1, 1, 1);
   InitializeSetup(setup[2], 1, 1, 1);
END;

PROCEDURE SpinLab;

{ ----------------------------- Math Routines ----------------------------- }

PROCEDURE AddToVec(invec: ThreeVec; a, b, c: Float; VAR outvec: ThreeVec);
BEGIN
   WITH outvec DO
      BEGIN
         x := invec.x + a;
         y := invec.y + b;
         z := invec.z + c;
      END;
END;

FUNCTION Distance(vec1, vec2: ThreeVec): Float;
BEGIN
   Distance := sqrt(sqr(vec2.x - vec1.x)
    + sqr(vec2.y - vec1.y) + sqr(vec2.z - vec1.z));
END;

PROCEDURE VecSum(vecA, vecB: ThreeVec; VAR sumvec: ThreeVec);
BEGIN
   WITH sumvec DO
      BEGIN
         x := vecA.x + vecB.x;
         y := vecA.y + vecB.y;
         z := vecA.z + vecB.z;
      END;
END;

PROCEDURE VecDiff(vecA, vecB: ThreeVec; VAR diffvec: ThreeVec);
BEGIN
   WITH diffvec DO
      BEGIN
         x := vecA.x - vecB.x;
         y := vecA.y - vecB.y;
         z := vecA.z - vecB.z;
      END;
END;

PROCEDURE VecScalarMult(factor: Float; invec: ThreeVec;
                        VAR outvec: ThreeVec);
BEGIN
   WITH outvec DO
      BEGIN
         x := factor*invec.x;
         y := factor*invec.y;
         z := factor*invec.z;
      END;
END;

{ ----------------- Complex Matrix Manipulation Routines ------------------ }

TYPE
   CXMatrix = RECORD
         RealMat : DMatrix;
         ImagMat : DMatrix;
      END;

PROCEDURE CXMatConj(VAR cxmat: CXmatrix);
{ In place complex conjugate of a complex matrix }
BEGIN
   WITH cxmat DO ImagMat.MultScalar(-1);
END;

PROCEDURE AddMatrix(InMatA, InMatB: DMatrix; VAR OutMat: DMatrix);
{ Form the sum of two real matrices. }
{ ASSUMES that all three matrices have the SAME DIMENSIONS. }
VAR
   Nrows, Ncols    : Integer;
   Row, Col, Index : Integer;
BEGIN
   WITH OutMat DO
      BEGIN
         GetSize(Nrows, Ncols);
         FOR Row := 1 to NRows DO
         FOR Col := 1 TO NCols DO
         Put(Row, Col, InMatA.Value(Row, Col) + InMatB.Value(Row,Col));
      END;
END;

PROCEDURE CXMatMult(InMatA, InMatB: CXMatrix; VAR OutMat: CXMatrix);
{ Form the product of two COMPLEX matrices. }
{ ASSUMES that all three matrices have COMPATIBLE DIMENSIONS. }
VAR
   Amat, Bmat   : DMatrix;
   Nrows, Ncols : Integer;
BEGIN
   WITH OutMat DO
      BEGIN
         RealMat.GetSize(Nrows, Ncols);
         Request2DMatrices(Amat, Bmat, Nrows, Ncols);
         InMatA.RealMat.MultMatrix(InMatB.RealMat, Amat);
         InMatA.ImagMat.MultMatrix(InMatB.ImagMat, Bmat);
         Bmat.MultScalar(-1);
         AddMatrix(Amat, Bmat, RealMat);
         InMatA.RealMat.MultMatrix(InMatB.ImagMat, Amat);
         InMatA.ImagMat.MultMatrix(InMatB.RealMat, Bmat);
         AddMatrix(Amat, Bmat, ImagMat);
         Release2DMatrices(Amat, Bmat);
      END;
END;

FUNCTION TraceProdHerm(InMatA, InMatB: CXMatrix): Float;
{ Returns the trace of the product of two Hermitian matrices. }
{ ASSUMES that both (square) input matrices have the SAME DIMENSIONS. }
VAR
   Nrows, Ncols    : Integer;
   Row, Col, Index : Integer;
   sum : Float;
BEGIN
   InMatA.RealMat.GetSize(Nrows, Ncols);
   sum := 0;
   FOR Row:= 1 to NRows DO
   FOR Col:= 1 TO NCols DO
      sum := sum
 + InMatA.RealMat.Value(Row, Col)*InMatB.RealMat.Value(Row, Col)
 + InMatA.ImagMat.Value(Row, Col)*InMatB.ImagMat.Value(Row, Col);
   TraceProdHerm := sum;
END;

PROCEDURE TraceTarg(Rho4x4: CXMatrix; VAR Rho2x2: CXMatrix);
{ Returns the 2x2 complex matrix formed by taking
  the target trace of a 4x4 complex matrix }
BEGIN
   WITH Rho4x4 DO
      BEGIN
         Rho2x2.RealMat.Put(1,1, RealMat.Value(1,1) + RealMat.Value(2,2));
         Rho2x2.RealMat.Put(1,2, RealMat.Value(1,3) + RealMat.Value(2,4));
         Rho2x2.RealMat.Put(2,1, RealMat.Value(3,1) + RealMat.Value(4,2));
         Rho2x2.RealMat.Put(2,2, RealMat.Value(3,3) + RealMat.Value(4,4));
         Rho2x2.ImagMat.Put(1,1, ImagMat.Value(1,1) + ImagMat.Value(2,2));
         Rho2x2.ImagMat.Put(1,2, ImagMat.Value(1,3) + ImagMat.Value(2,4));
         Rho2x2.ImagMat.Put(2,1, ImagMat.Value(3,1) + ImagMat.Value(4,2));
         Rho2x2.ImagMat.Put(2,2, ImagMat.Value(3,3) + ImagMat.Value(4,4));
      END;
END;

PROCEDURE SetUpVector(len, theta, phi: Float; VAR vec: ThreeVec);
{ Returns a vector of length len that points in the direction
  specified by spherical polar angles theta and phi }
VAR
   thetarad, phirad, s : Float;
BEGIN
   thetarad := pi*theta/180;
   phirad := pi*phi/180;
   s := len*sin(thetarad);
   vec.x := s*cos(phirad);
   vec.y := s*sin(phirad);
   vec.z := len*cos(thetarad);
END;

PROCEDURE SetUpPolarizationVector(phi: Float; polint: Integer;
                                  VAR PVec: ThreeVec);
{ Returns a null vector for an unpolarized spin-1/2 object (polint = 1).
  Otherwise returns a unit vector referred to coordinate axes chosen
  such that
    "forwards" is in the xy plane and makes angle phi with the x axis
    "to left" is in the xy plane and makes angle phi with the y axis and
    "upwards" is parallel to the z axis.  (phi is specified in degrees) }
VAR
   angle : Float;
BEGIN
   CASE polint OF
1: SetUpVector(0, 0, 0, Pvec);          { unpolarized }
2: SetUpVector(1, 90, phi, Pvec);       { forwards    }
3: SetUpVector(1, 90, phi + 180, Pvec); { backwards   }
4: SetUpVector(1, 90, phi + 90, Pvec);  { to left     }
5: SetUpVector(1, 90, phi - 90, Pvec);  { to right    }
6: SetUpVector(1, 0, 0, Pvec);          { upwards     }
7: SetUpVector(1, 180, 0, Pvec);        { downwards   }
   END; { CASE }
END;

{ ---------------------- Cross Section Calculations ----------------------- }

PROCEDURE SetUpInitialSpinDensity(P1, P2: ThreeVec;
                                  VAR RhoSpin: CXMatrix);
{ Form the product matrix (1 + P1.Sig1)*(1 + P2.Sig2) }
BEGIN
   WITH RhoSpin DO
      BEGIN
         { The real part of the initial spin density matrix }
         RealMat.Put(1,1, (1 + P1.z)*(1 + P2.z));
         RealMat.Put(1,2, (1 + P1.z)*P2.x);
         RealMat.Put(1,3, P1.x*(1 + P2.z));
         RealMat.Put(1,4, (P1.x*P2.x - P1.y*P2.y));
         RealMat.Put(2,1, RealMat.Value(1,2));
         RealMat.Put(2,2, (1 + P1.z)*(1 - P2.z));
         RealMat.Put(2,3, (P1.x*P2.x + P1.y*P2.y));
         RealMat.Put(2,4, P1.x*(1 - P2.z));
         RealMat.Put(3,1, RealMat.Value(1,3));
         RealMat.Put(3,2, RealMat.Value(2,3));
         RealMat.Put(3,3, (1 - P1.z)*(1 + P2.z));
         RealMat.Put(3,4, (1 - P1.z)*P2.x);
         RealMat.Put(4,1, RealMat.Value(1,4));
         RealMat.Put(4,2, RealMat.Value(2,4));
         RealMat.Put(4,3, RealMat.Value(3,4));
         RealMat.Put(4,4, (1 - P1.z)*(1 - P2.z));
         { The imaginary part of the initial spin density matrix }
         ImagMat.Put(1,1,  0);
         ImagMat.Put(1,2, -(1 + P1.z)*P2.y);
         ImagMat.Put(1,3, -P1.y*(1 + P2.z));
         ImagMat.Put(1,4, -(P1.x*P2.y + P1.y*P2.x));
         ImagMat.Put(2,1, -ImagMat.Value(1,2));
         ImagMat.Put(2,2,  0);
         ImagMat.Put(2,3,  (P1.x*P2.y - P1.y*P2.x));
         ImagMat.Put(2,4, -P1.y*(1 - P2.z));
         ImagMat.Put(3,1, -ImagMat.Value(1,3));
         ImagMat.Put(3,2, -ImagMat.Value(2,3));
         ImagMat.Put(3,3,  0);
         ImagMat.Put(3,4, -(1 - P1.z)*P2.y);
         ImagMat.Put(4,1, -ImagMat.Value(1,4));
         ImagMat.Put(4,2, -ImagMat.Value(2,4));
         ImagMat.Put(4,3, -ImagMat.Value(3,4));
         ImagMat.Put(4,4,  0);
      END;
END;


PROCEDURE SetUpScattAmpMatrix(i: Integer; VAR theta: Real;
                              VAR ScattAmp: CXMatrix);
{ This procedure assumes that coordinate axes have been chosen such that
  x is parallel to p_hat ( towards the detector in the lab frame )
  y is parallel to n_hat x p_hat
  z is parallel to n_hat ( i.e. normal to the scattering plane ) }
VAR
   a, b, c, e, f : Complex;
BEGIN
   GetWolfAmps(theta, a, b, c, e, f, i);
   WITH ScattAmp DO
      BEGIN
         { The real part of the scattering amplitude matrix }
         RealMat.Put(1,1, a.re + b.re + 2*c.re);
         RealMat.Put(1,2, 0);
         RealMat.Put(1,3, 0);
         RealMat.Put(1,4, f.re - e.re);
         RealMat.Put(2,1, 0);
         RealMat.Put(2,2, a.re - b.re);
         RealMat.Put(2,3, e.re + f.re);
         RealMat.Put(2,4, 0);
         RealMat.Put(3,1, 0);
         RealMat.Put(3,2, RealMat.Value(2,3));
         RealMat.Put(3,3, RealMat.Value(2,2));
         RealMat.Put(3,4, 0);
         RealMat.Put(4,1, RealMat.Value(1,4));
         RealMat.Put(4,2, 0);
         RealMat.Put(4,3, 0);
         RealMat.Put(4,4, RealMat.Value(1,1) - 4*c.re);
         { The imaginary part of scattering amplitude matrix }
         ImagMat.Put(1,1, a.im + b.im + 2*c.im);
         ImagMat.Put(1,2, 0);
         ImagMat.Put(1,3, 0);
         ImagMat.Put(1,4, f.im - e.im);
         ImagMat.Put(2,1, 0);
         ImagMat.Put(2,2, a.im - b.im);
         ImagMat.Put(2,3, e.im + f.im);
         ImagMat.Put(2,4, 0);
         ImagMat.Put(3,1, 0);
         ImagMat.Put(3,2, ImagMat.Value(2,3));
         ImagMat.Put(3,3, ImagMat.Value(2,2));
         ImagMat.Put(3,4, 0);
         ImagMat.Put(4,1, ImagMat.Value(1,4));
         ImagMat.Put(4,2, 0);
         ImagMat.Put(4,3, 0);
         ImagMat.Put(4,4, ImagMat.Value(1,1) - 4*c.im);
      END;
END;

PROCEDURE SetUpDetectorSpinDensity(Pdetect: ThreeVec;
                                   ParallelProb, AntiparaProb: Float;
                                   VAR RhoDetector: CXMatrix);
{ Form the properly normalized detector response spin density matrix }
{ Pdetect is a unit vector in the direction of the detector's polariz-
  ation sensitivity (if any).   ParallelProb and AntiparaProb are the
  probabilities that the detector will see a particle polarized par-
  allel and antiparallel, respectively, to this direction. }
VAR
   sum, diff : Float;
BEGIN
   sum  := 0.5*(ParallelProb + AntiparaProb);
   diff := 0.5*(ParallelProb - AntiparaProb);
   WITH RhoDetector DO
      BEGIN
         { The real part of the detector efficiency matrix }
         RealMat.Put(1,1, sum + diff*Pdetect.z);
         RealMat.Put(1,2, diff*Pdetect.x);
         RealMat.Put(2,1, RealMat.Value(1,2));
         RealMat.Put(2,2, sum - diff*Pdetect.z);
         { The imaginary part of the detector efficiency matrix }
         ImagMat.Put(1,1,  0);
         ImagMat.Put(1,2, -diff*Pdetect.y);
         ImagMat.Put(2,1, -ImagMat.Value(1,2));
         ImagMat.Put(2,2,  0);
      END;
END;

PROCEDURE MakeSigma(j: Integer; setup: SetupArray;
                    i: Integer; VAR theta: real; VAR sig: Float);
{ This procedure assumes that coordinate axes have been chosen such that
  x is parallel to p_hat ( towards the detector in the lab frame )
  y is parallel to n_hat x p_hat
  z is parallel to n_hat ( normal to the scattering plane ) }
VAR
   norm, AntiparaProb      : Float;
   pvec1, pvec2, pdetect   : ThreeVec;
   MyRho, ScattAmp, CXtemp : CXMatrix;
   RhoDet, my2x2           : CXMatrix;

   m,n : Integer;

BEGIN
   Request2DMatrices(MyRho.RealMat, MyRho.ImagMat, 4, 4);
   Request2DMatrices(ScattAmp.RealMat, ScattAmp.ImagMat, 4, 4);
   Request2DMatrices(CXtemp.RealMat, CXtemp.ImagMat, 4, 4);
   Request2DMatrices(RhoDet.RealMat, RhoDet.ImagMat, 2, 2);
   Request2DMatrices(my2X2.RealMat, my2x2.ImagMat, 2, 2);
   norm := 10; { Converts to millibarn per steradian }
   SetUpScattAmpMatrix(i, theta, ScattAmp);
   WITH setup[j] DO
      BEGIN
         SetUpPolarizationVector(-0.5*theta, ProjPol, pvec1);
         SetUpPolarizationVector(-0.5*theta, TargPol, pvec2);
         SetUpPolarizationVector(0, DetectPol, pdetect);
         IF DetectPol = 1 THEN AntiparaProb := 1
                          ELSE AntiparaProb := 0;
      END;
   SetUpInitialSpinDensity(pvec1, pvec2, MyRho);
   CXMatMult(ScattAmp, MyRho, { to produce } CXtemp);
   CXMatConj(ScattAmp); { Forms hermitian conj. of (symmetric) ScattAmp }
   CXMatMult(CXtemp, ScattAmp, { to produce } MyRho);
   { Trace over the unobserved recoil particle's spin projections }
   TraceTarg(MyRho, my2x2);
   SetUpDetectorSpinDensity(pdetect, 1, AntiparaProb, RhoDet);
   sig := 0.25*norm*TraceProdHerm(RhoDet, my2x2);
   Release2DMatrices(my2x2.RealMat, my2x2.ImagMat);
   Release2DMatrices(RhoDet.RealMat, RhoDet.ImagMat);
   Release2DMatrices(CXtemp.RealMat, CXtemp.ImagMat);
   Release2DMatrices(ScattAmp.RealMat, ScattAmp.ImagMat);
   Release2DMatrices(MyRho.RealMat, MyRho.ImagMat);
END;

{ -------------------------- Interface Routines --------------------------- }

PROCEDURE MakeScreenProjectors(eyepoint: ThreeVec;
                       VAR eyedist:  Float;
                       VAR xscvec, yscvec, eyevec: ThreeVec);
{ eyevec is a unit vector directed from the origin towards the eye position.
  xscvec is a unit vector directed parallel to zhat cross eyevec,
  where zhat is a unit vector that points vertically upwards.
  yscvec is a unit vector directed parallel to eyevec cross xscvec.
  eyedist is the distance from the origin to the eye position. }
VAR
   Temp: Float;
BEGIN
   WITH eyepoint DO
      BEGIN
         Temp := sqr(x)+sqr(y);
         eyedist := sqrt(Temp+sqr(z));
         Temp := sqrt(Temp);
         eyevec.x := x/eyedist;
         eyevec.y := y/eyedist;
         eyevec.z := z/eyedist;
         xscvec.x := -y/Temp;
         xscvec.y :=  x/Temp;
         xscvec.z :=  0.0;
         yscvec.x := -eyevec.z*xscvec.y;
         yscvec.y :=  eyevec.z*xscvec.x;
         yscvec.z :=  eyevec.x*xscvec.y-eyevec.y*xscvec.x;
      END;
END;

PROCEDURE MakeScreenCoords(vector: ThreeVec; VAR xscreen,yscreen: Float);
{ The screen coordinates are determined by projecting the given vector
  onto a plane perpendicular to eyevec and then multiplying the resulting
  two-component vector by a proportionality factor that accounts for
  perspective. }
{ Note:  Uses GLOBAL VARIABLES  eyedist, xscvec, yscvec, eyevec }
VAR
   ndotr,ftr : Float;
BEGIN
   WITH vector DO
      BEGIN
         ndotr := x*eyevec.x + y*eyevec.y + z*eyevec.z;
         ftr := eyedist/(eyedist - ndotr);
         xscreen := ftr*(x*xscvec.x + y*xscvec.y + z*xscvec.z);
         yscreen := ftr*(x*yscvec.x + y*yscvec.y + z*yscvec.z);
      END;
END;

PROCEDURE GetSetupInfo(MySetup: Integer; VAR setup: SetupType);
{ Purpose:  Get input to determine the polarization properties
            of the projectile, target and detector }
VAR
   ViewScreen : TInputScreen;
   i, j, k    : Integer;
BEGIN
   OpenScrn(ViewScreen);
   WITH ViewScreen DO
   BEGIN
      init;
      DefineInputport(0.20, 0.85, 0.18, 0.82); 
      LoadLine('');
      LoadLine(Concat('         Choose polarizations for setup ',
                       NumStr(MySetup,1,0)));
      LoadLine('');
      LoadLine('                    Projectile  Target   Detector');
      LoadLine('');
      LoadLine('  Unpolarized           #1        #2        #3');
      LoadLine('  Polarized forwards    #1        #2        #3');
      LoadLine('  Polarized backwards   #1        #2        #3');
      LoadLine('  Polarized to left     #1        #2        #3');
      LoadLine('  Polarized to right    #1        #2        #3');
      LoadLine('  Polarized upwards     #1        #2        #3');
      LoadLine('  Polarized downwards   #1        #2        #3');
      LoadLine('');
      LoadLine('                      [  Ok  ]     [Cancel]');
      WITH setup DO
         BEGIN
            SetRadioButton('1',ProjPol);
            SetRadioButton('2',TargPol);
            SetRadioButton('3',DetectPol);
         END;
      AcceptScreen;
      IF NOT Canceled THEN
         BEGIN
            WITH setup DO
               BEGIN
                  ProjPol := GetRadioButton('1');
                  TargPol := GetRadioButton('2');
                  DetectPol := GetRadioButton('3');
            END;
         END;
      done;
   END; { WITH ViewScreen }
   CloseScrn(ViewScreen);
END;

PROCEDURE PrintDescription(i, j, polint: Integer);
BEGIN
   IF polint <> 1 THEN Print(i-1,j+1,'(polarized');
   CASE polint OF
1: Print(i-2,j+1,'(unpolarized)');
2: Print(i,j+2,'forwards)');
3: Print(i,j+2,'backwards)');
4: Print(i,j+2,'to left)');
5: Print(i,j+2,'to right)');
6: Print(i,j+2,'upwards)');
7: Print(i,j+2,'downwards)');
   END;
END;

PROCEDURE GetPrintLocation(x, y: Float; VAR i, j: Integer);
BEGIN
   Map( x, y, i, j);
   i := Round(i/ColWidth + 0.6);
   j := Round(j/RowHt + 0.6);
END;

PROCEDURE PrepareSetupDisplay(MySetup: Integer; setup: SetupType);
VAR
   i, j : Integer;
BEGIN
   ClearMUPPETport;
   DefineViewport(1, 0.1, 0.9, 0.1, 0.9);
   OpenViewPort(1);
   DefineScale(1, -2.7, 3.7, -1.8, 2.6);
   IF MySetup = 1 THEN SetColor(LightGreen)
                  ELSE SetColor(LightRed);
   Print(20,1,Concat('Polarizations for setup ', NumStr(MySetup,1,0)));
   WITH setup DO
      BEGIN
         GetPrintLocation(-2.3, 0.9, i, j);
         Print(i,j,'Projectile');
         PrintDescription(i+1,j,ProjPol);
         GetPrintLocation(-1.0, -0.25, i, j);
         Print(i,j,'Target');
         PrintDescription(i-1,j,TargPol);
         GetPrintLocation(2.0, -0.93, i, j);
         Print(i,j,'Detector');
         PrintDescription(i,j,DetectPol);
      END; { WITH setup }
   SetColor(Whitte);
END;

PROCEDURE PlotOneDash(vec1, vec2: ThreeVec);
VAR
   xsc1, ysc1, xsc2, ysc2 : Float;
BEGIN
   MakeScreenCoords(vec1, xsc1, ysc1);
   MakeScreenCoords(vec2, xsc2, ysc2);
   Plotline(xsc1, ysc1, xsc2, ysc2);
END;

PROCEDURE PlotdashedLine(vecA, vecB: ThreeVec);
VAR
   numstep, i, k    : Integer;
   step, dist, ftr  : Float;
   dvec, vec1, vec2 : ThreeVec;
   xsc1, ysc1, xsc2, ysc2 : Float;
BEGIN
   step := 0.2;
   dist := Distance(vecA,vecB);
   numstep := 2*round(0.5*dist/step) + 1;
   step := dist/numstep;
   IF dist > 0 THEN ftr := 1.0/numstep ELSE ftr := 0;
   dvec.x := ftr*(vecB.x - vecA.x);
   dvec.y := ftr*(vecB.y - vecA.y);
   dvec.z := ftr*(vecB.z - vecA.z);
   vec1 := vecA;
   numstep := trunc(numstep/2);
   FOR k := 1 TO numstep DO
      BEGIN
         VecSum(vec1, dvec, vec2);
         PlotOneDash(vec1, vec2);
         VecSum(vec2, dvec, vec1);
      END;
   VecSum(vec1, dvec, vec2);
   PlotOneDash(vec1, vec2);
END;

PROCEDURE PlotTriad(origin: ThreeVec; labangle: Float);
VAR
   c, s, xsc1, ysc1, xsc2, ysc2 : Float;
   x1, y1, x2, y2 : Integer;
   tempvec : ThreeVec;
BEGIN
   c := cos(labangle);  s := sin(labangle);
   MakeScreenCoords(origin, xsc1, ysc1);
   Map(xsc1, ysc1, x1, y1);
   AddToVec(origin, c, s, 0, tempvec);
   MakeScreenCoords(tempvec, xsc2, ysc2);
   Map(xsc2, ysc2, x2, y2);
   Vector(x1,y1, x2,y2);  { forwards }
   AddToVec(origin, -s, c, 0, tempvec);
   MakeScreenCoords(tempvec, xsc2, ysc2);
   Map(xsc2, ysc2, x2, y2);
   Vector(x1,y1, x2,y2);  { to left }
   AddToVec(origin, 0, 0, 1, tempvec);
   MakeScreenCoords(tempvec, xsc2, ysc2);
   Map(xsc2, ysc2, x2, y2);
   Vector(x1,y1, x2,y2);  { upwards }
END;

PROCEDURE PlotPolSymbol(origin: ThreeVec;
                        labangle: Float; polint: Integer);
VAR
   c, s, xsc1, ysc1, xsc2, ysc2 : Float;
   x1, y1, x2, y2 : Integer;
   tempvec : ThreeVec;
BEGIN
   MakeScreenCoords(origin, xsc1, ysc1);
   Map(xsc1, ysc1, x1, y1);
   SetFillStyle(SolidFill,getcolor);
   FillEllipse(x1, y1, 3, 3);
   IF (polint > 1) AND (polint < 6) THEN
      BEGIN
         c := 0.7*cos(labangle);
         s := 0.7*sin(labangle);
      END;
   IF (polint > 1) THEN
      BEGIN
         CASE polint OF
      2: AddToVec(origin, c, s, 0, tempvec);
      3: AddToVec(origin, -c, -s, 0, tempvec);
      4: AddToVec(origin, -s, c, 0, tempvec);
      5: AddToVec(origin, s, -c, 0, tempvec);
      6: AddToVec(origin, 0, 0,  0.7, tempvec);
      7: AddToVec(origin, 0, 0, -0.7, tempvec);
         END; { CASE }
         MakeScreenCoords(tempvec, xsc2, ysc2);
         Map(xsc2, ysc2, x2, y2);
         Vector(x1,y1, x2,y2);
      END;
END;

PROCEDURE SketchLayout(i: Integer; setup: SetupType);
VAR
   labangle : Float;
   Proj, target, beyond, detector : ThreeVec;
BEGIN
   Proj.x   := -3;  Proj.y   := 0;  Proj.z   := 0;
   target.x :=  0;  target.y := 0;  target.z := 0;
   beyond.x :=  2;  beyond.y := 0;  beyond.z := 0;
   labangle := (pi/180)*40;
   detector.x := 2*cos(labangle);
   detector.y := 2*sin(labangle);
   detector.z := 0;
   PlotTriad(Proj, 0);
   PlotTriad(target, 0);
   PlotTriad(detector, labangle);
   PlotdashedLine(Proj, target);
   PlotdashedLine(target, beyond);
   PlotdashedLine(target, detector);
   IF i = 1 THEN SetColor(LightGreen)
            ELSE SetColor(LightRed);
   WITH setup DO
      BEGIN
         PlotPolSymbol(Proj, 0, ProjPol);
         PlotPolSymbol(target, 0, TargPol);
         PlotPolSymbol(detector, labangle, DetectPol);
      END; { WITH setup }
   SetColor(whitte);
END;

PROCEDURE PrepareResultsPlot(MySystem: Integer;
                 LabKE, Thetmin, Thetmax, Sigmax: Float);

   FUNCTION ResizeScale: Float;
   VAR
      i    : Integer;
      temp : Float;
   BEGIN
      temp := 0;
      FOR i := 1 TO 2 DO
         WITH Setup[i] DO
            IF (ProjPol > 1) AND (TargPol > 1) THEN temp := 0.5;
      IF MySystem = 4 THEN temp := 2*temp;
      ResizeScale := 1 + temp;
   END;

VAR
   dxtik, dytik : Float;
BEGIN
   ClearMUPPETport;
 
   DefineViewport(1, 0.00, 0.97, 0.885, 0.93);
   OpenViewPort(1);
   CASE MySystem OF
1: Print(18,1,Concat('Proton on Proton Scattering at', NumStr(LabKE,8,2), ' MeV'));
2: Print(19,1,Concat('P + N Elastic Scattering at', NumStr(LabKE,8,2), ' MeV'));
3: Print(16,1,Concat(
      'P + N Charge Exchange Scattering at', NumStr(LabKE,8,2), ' MeV'));
4: Print(22,1,Concat('Neutron on Neutron Scattering at', NumStr(LabKE,8,2), ' MeV'));
   END; { CASE }
 
   DefineViewport(1, 0.0, 0.4, 0.53, 0.85);
   OpenViewPort(1);
   DefineScale(1, -2.5, 3.5, -1.2, 2.3);
   SetColor(LightGreen);
   Print(14,1,'Setup 1');
   SetColor(whitte);

   DefineViewport(2, 0.0, 0.4, 0.13, 0.45);
   OpenViewPort(2);
   DefineScale(2, -2.5, 3.5, -1.2, 2.3);
   SetColor(LightRed);
   Print(14,1,'Setup 2');
   SetColor(whitte);

   DefineViewport(3, 0.5, 0.97, 0.15, 0.85);
   OpenViewPort(3);
   Sigmax := ResizeScale*Sigmax;
   DefineScale(3, Thetmin, Thetmax, 0.0, Sigmax);
   SelectScale(3);
   dxtik := TickSpace(Thetmax - Thetmin);
   dytik := TickSpace(Sigmax);
   Axis(0.0, 0.0, dxtik, dytik);
   PutLabel(bottom,'Theta (deg)');
   PutLabel(left,'d(Sigma)/d(omega)');
   Print(4,1,'----- Unpolarized Cross Section');
   SetColor(LightGreen);
   Print(4,2,'----- Setup 1');
   SetColor(LightRed);
   Print(22,2,'----- Setup 2');
   SetColor(whitte);
   IF TheoryDone = TRUE THEN Print(8,3,'Solid lines are theory');
END;

{ ------------------------- Program Organization -------------------------- }

 
PROCEDURE GetSetup(MySetup: Integer; VAR setup: SetupType);
BEGIN
   GetSetupInfo(MySetup, setup);
   PrepareSetupDisplay(MySetup, setup);
   MakeScreenProjectors(eyepoint, eyedist, xscvec, yscvec, eyevec);
   SelectViewport(1);
   SelectScale(1);
   SketchLayout(MySetup, setup);
END;

PROCEDURE CalcResponses(setup: SetupArray);
{ Uses GLOBAL VARIABLES:  Thetmin, Thetmax, Numang }
VAR
   Pprojvec, Ptargvec : ThreeVec;
VAR
   theta        : Real;
   Phases       : PhaseType;
   PhasesFound  : Boolean;
   i, j         : Integer;
   sig, sigmax  : Float;
   sigmav       : DVector;
 
BEGIN { CalcResponses (SpinLab) }
   ClearMUPPETport;
   Request1DVector(thetav, Numang);
   RequestCXVectors(av, bv, cv, ev, fv, Numang);
{  Calculations based on the empirical phase shifts... }
   GetPhases(LabKE, PhasesFound, Phases);
   MakeScattAmps(Phases, Thetmin, Thetmax, Numang);
   FindReasonableScale(Numang, Sigmax);
   PrepareResultsPlot(MySystem, LabKE, Thetmin, Thetmax, Sigmax);
   MakeScreenProjectors(eyepoint, eyedist, xscvec, yscvec, eyevec);
   SelectViewport(1);
   SelectScale(1);
   SketchLayout(1, setup[1]);
   SelectViewport(2);
   SelectScale(2);
   SketchLayout(2, setup[2]);
{  Plot cross sections obtained from the empirical phase shifts }
   SelectViewPort(3);
   SelectScale(3);
   HideMouse;
   FOR j := 1 TO 2 DO
      BEGIN
         IF j = 1 THEN SetColor(LightGreen)
                  ELSE SetColor(LightRed);
         FOR i := 1 TO numang DO
            BEGIN
               MakeSigma(j, setup, i, theta, sig);
               PlotPoint(theta,sig);
            END;
      END;
   SetColor(whitte);
{  Plot the empirical unpolarized cross section }
   FOR i := 1 TO numang DO
      BEGIN
         MakeUnpolSigma(i, theta, sig);
         PlotPoint(theta,sig);
      END;
   ShowMouse;
{  Calculations based on the theoretical phase shifts... }
   IF TheoryDone = TRUE THEN
      BEGIN
         MakeScattAmps(TheoryPhases, Thetmin, Thetmax, Numang);
         Request1DVector(sigmav, Numang);
      {  Plot cross sections obtained from the theoretical phase shifts }
         FOR j := 1 TO 2 DO
            BEGIN
               IF j = 1 THEN SetColor(LightGreen)
                        ELSE SetColor(LightRed);
               FOR i := 1 TO numang DO
                  BEGIN
                     MakeSigma(j, setup, i, theta, sig);
                     sigmav.Put(i, sig);
                  END;
               PlotDvtr(thetav, sigmav, 1, numang);
            END;
         SetColor(whitte);
         Release1DVector(sigmav);
      END;
   ReleaseCXVectors(av, bv, cv, ev, fv);
   Release1DVector(thetav);
END;  { CalcResponses (SpinLab) }
 
BEGIN
 WITH MasterMenu DO
    CASE RowChosen OF
  1: ShowHelp('twobody.hlp','AboutSpinLab');
  2: GetSetup(1, setup[1]);
  3: GetSetup(2, setup[2]);
  4: CalcResponses(setup);
    END; {CASE}
END;

END.  { Unit SPINCALC.PAS }
