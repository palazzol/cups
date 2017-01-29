           (*************************************************)
           (*************************************************)
           (**                                             **)
           (**  Consortium of Upper-level Physics Software **)
           (**              ( CUPS Project )               **)
           (**      ( c) 1994 by John Wiley & Sons         **)
           (**  Compiled with Utilities Ver. 1.9 (95/09/10)**)
           (**                                             **)
           (*************************************************)
           (*************************************************)

{ Start of file QM_FUNC.PAS }

{ ----------------------------------------------------------------------
   Program Title:    Functions for wave mechanics units.
   Author:           Douglas E. Brandt
   Version:          0.11
   Program Summary:  
   Library Files:    MUPPET,CUPSFUNC,CUPSPROC, OBJECTS
   Utilities:
   Algorithms:
------------------------------------------------------------------------}


UNIT QM_FUNC;

{F+}

INTERFACE

USES CUPS, CUPSmupp,CUPSfunc,CUPSproc,Objects;

TYPE
   Function4 = FUNCTION(a,b,c,d: Real): Real;

FUNCTION wEM(k,m: Real): Real;
FUNCTION wSchrodinger(k,m: Real): Real;
FUNCTION wCubic(k,m: Real): Real;
FUNCTION wUser(k,m:Real): Real;
FUNCTION Gaussian(x, sigma, center: Real): Real;
FUNCTION Rectangle(x, width, center: Real): Real;
FUNCTION FTRectangle(x, width, center: Real): Real;
FUNCTION Triangle(x, width, center: Real): Real;
FUNCTION FTTriangle(x, width, center: Real): Real;
FUNCTION User(x, width , center: Real): Real;
FUNCTION FTUser(x, width, center: Real): Real;
PROCEDURE PRoots(Fct:FUNCTION4;p,q,r,a,b,Tol: REAL; VAR N, J: integer;
                  VAR M: DVector);
FUNCTION F2(e,v1,v2,L: Real): Real;


IMPLEMENTATION

FUNCTION wEM(k,m: Real): Real;

   BEGIN
      wEM := k;
   END;

FUNCTION wSchrodinger(k,m: Real): Real;
   BEGIN
      wSchrodinger := Sqr(k)/(2*m);
   END;


FUNCTION wCubic(k,m: Real): Real;

   BEGIN
      wCubic := 3*k*k*k/2 - 7*k*k + 17*k/2;
   END;


FUNCTION wUser(k,m:Real): Real;

   BEGIN
   END;


FUNCTION Gaussian(x, sigma, center: Real): Real;

   VAR
      A: Real;

   BEGIN

      A := -1.0 * Sqr(x - center)/(4 * Sqr(sigma));
      IF A > -28 THEN A := Exp(A) ELSE A := 0;
      Gaussian := A/Sqrt(sigma*Sqrt(2 * Pi));
   END;


FUNCTION Rectangle(x, width, center: Real): Real;

   BEGIN
      width := sqrt(12)*width;
      IF ABS(center - x) < (width/2) THEN Rectangle := 1/Sqrt(width)
      ELSE Rectangle := 0;
   END;


FUNCTION FTRectangle(x, width, center: Real): Real;

   VAR
      A: Real;

   BEGIN
      width := width/sqrt(12);
      IF x-center <> 0 THEN A := sqrt(width)*
                 (sin((x-center)/(2*width))/(x-center))
      ELSE A := 1/sqrt(4*width);
      FTRectangle := A/Sqrt(Pi/2);
   END;


FUNCTION Triangle(x, width, center: real): Real;

   BEGIN
      width := width*sqrt(10);
      IF (x < center) AND (x > center - width)
          THEN Triangle := ((x-center)/width + 1)*Sqrt(3/(2*width));
      IF (x > center) AND (x < center + width)
          THEN Triangle := ((center-x)/width +1)*Sqrt(3/(2*width));
      IF (x <= center - width) OR (x >= center + width) THEN Triangle := 0;
      IF x = center THEN Triangle := Sqrt(3/(2*width));
   END;


FUNCTION FTTriangle(x, width, center: real): real;

   BEGIN
      width := width/sqrt(10);
      IF x <> center THEN FTTriangle := (1-cos((x-center)/width))/
                                       (Sqr((x-center))/width)*Sqrt(3/(Pi/Width))
      ELSE FTTriangle := Sqrt(3/(pi/width))/(2*width);
   END;


FUNCTION User(x, width , center: real): real;

   BEGIN
   END;

FUNCTION FTUser(x, width, center: real): real;

   BEGIN
   END;


FUNCTION F2(e,v1,v2,L: Real): Real;

  VAR
      k1,k2,k3,q,u,sign: Real;

   BEGIN
      k1 := Sqrt(2 * (v1-e));
      k2 := Sqrt(2 * e);
      k3 := Sqrt(2 * (v2-e));
      u := k2 * L;
      IF Sqr(sin(u)) <> 0 THEN sign := sin(u)/Sqrt(Sqr(sin(u))) ELSE sign := 1;
      q := (cot(u) - (Sqr(k2) - k1 * k3 )/((k1+k3) * k2)) * sign;
      F2 := q;
   END;


PROCEDURE PRoots(Fct:FUNCTION4;p,q,r,a,b, Tol: REAL; VAR N, J: integer;
                                              VAR M: DVector);


CONST
   maxiter = 50;
VAR
  x,dx,xo,xol,fo,x1,f1,x2,f2: REAL;
  nf,i,iter: integer;

 PROCEDURE MOVES(VAR r,s,t,u,v,w,x,y : REAL);
 BEGIN
   r:=s; t:=u; v:=w; x:=y
 END;

BEGIN
   J := 0; nf := N-1;
   dx := (b-a)/N;
   FOR  i:=0 TO nf DO
      BEGIN
         x := a + i*dx;
         f1:= Fct(x,p,q,r); f2 := Fct(x+dx,p,q,r);
         IF (f1*f2 < 0.0) THEN
         BEGIN
            iter :=0;
            x1 := x; f1 := Fct(x1,p,q,r); x2 := x + dx; f2 := Fct(x2,p,q,r);
            xo:= x1;
            REPEAT
               xol := xo; xo :=(x1*f2-x2*f1)/(f2-f1); fo := Fct(xo,p,q,r);
               iter := iter + 1;
               IF (ABS(xo-xol) > Tol)  THEN
                  IF ABS(xo-x1) < 0.1*dx THEN
                     MOVES(x2,x1,f2,f1,x1,xo,f1,fo)
                  ELSE
                     IF ABS(xo-x2) < 0.1*dx THEN
                        MOVES(x1,x2,f1,f2,x2,xo,f2,fo)
                  ELSE
                     IF f1*fo > 0
                        THEN
                           MOVES(x1,xo,f1,fo,x2,x2,f2,f2)
                     ELSE
                        MOVES(x2,xo,f2,fo,x1,x1,f1,f1);
            UNTIL ((iter =maxiter) OR (ABS(xo-xol)<Tol));
            IF (iter=maxiter) THEN
               Error('FindZero: no convergence to root between '
                        +numStr(a,5,2)+' and  '+NumStr(b,5,2))
            ELSE
               BEGIN
                  J := J + 1;
                  M.Put(J,xo)
               END
         END
      END
END;

{F-}

END.  { End of file QM_FUNC.PAS }

