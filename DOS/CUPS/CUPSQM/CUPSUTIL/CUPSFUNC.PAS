                 (**********************************************)
                 (**********************************************)
                 (**                                          **)
                 (**   Unit CUPSfunc - Mathematical Functions **)
                 (**   Written by William M. MacDonald        **)
                 (**   for CUPS Project                       **)
                 (**   Department of Physics                  **)
                 (**   University of Maryland                 **)
                 (**   College Park, Maryland 20742           **)
                 (**   (c) 1994 by William M. MacDonald       **)
                 (**   Originated:  91/02/14                  **)
                 (**   Version: 1.3 (94/11/14)                **)
                 (**                                          **)
                 (**********************************************)
                 (**********************************************)

{$O+,F+}
UNIT CUPSfunc;

INTERFACE
uses CUPS;

(**************** CUPS FUNCTION LIBRARY ***********************)

TYPE 
   COMPLEX = RECORD
      Re : REAL;
      Im : REAL;
   END;

   Operator = char;

PROCEDURE Error(errorStr : string);

PROCEDURE StartClock;
FUNCTION  ReadClock : string;

FUNCTION Pwr(x, y: REAL): REAL;  {x ^ y}
FUNCTION Tan(x: REAL): REAL;
FUNCTION Csc(x: REAL): REAL;
FUNCTION Sec(x: REAL): REAL;
FUNCTION Cot(x: REAL): REAL;
FUNCTION ArcSin(x: REAL): REAL;
FUNCTION ArcCos(x: REAL): REAL;
FUNCTION ArcTan2(x,y: REAL): REAL;
FUNCTION Sinh(x: REAL): REAL;
FUNCTION Cosh(x: REAL): REAL;
FUNCTION Tanh(x: REAL): REAL;
FUNCTION Csch(x: REAL): REAL;
FUNCTION Sech(x: REAL): REAL;
FUNCTION Coth(x: REAL): REAL;
FUNCTION ArcSinh(x: REAL): REAL;
FUNCTION ArcCosh(x: REAL): REAL;
FUNCTION ArcTanh(x: REAL): REAL;
FUNCTION Log(x,y : REAL) : REAL;    {log x Y}
FUNCTION Log10(y : REAL) : REAL;    {log 10 Y}
FUNCTION Sgn(y : REAL) : REAL;
FUNCTION Sign(x,y: REAL): REAL;     {FORTRAN sign definition}

PROCEDURE CxOp(z1: complex; Op: Operator; z2: complex; VAR w: complex);
PROCEDURE RCprod(x: REAL; z: complex; VAR w: complex);
PROCEDURE CPwr(z:complex; y:REAL; VAR w:complex);
FUNCTION  CABS(z: complex): REAL;
FUNCTION  Arg(z : complex): REAL;
PROCEDURE CLog(z: complex; VAR w:complex);
PROCEDURE CEXP(z:complex; VAR w:complex);
PROCEDURE CSin(z:complex; VAR w:complex);
PROCEDURE CCos(z:complex; VAR w:complex);
PROCEDURE CTan(z:complex; VAR w:complex);

IMPLEMENTATION
uses dos, graph;
(*************ERROR REPORTING PROCEDURE *************************)

PROCEDURE Error(errorStr : string);
VAR i : integer;
BEGIN
   ErrorStr := 'Error: '+ErrorStr;
   i := GetGraphMode;
   i := GraphResult;
   IF (i <> -1 ) THEN Announce(errorStr) ELSE
   begin WRITELN(errorStr); StaticPause; end;
   IF HaltIfError THEN BEGIN CupsDone; halt END
   ELSE ErrorFound:=TRUE;
END;

(******************* TIMING ROUTINES ****************************)

VAR
   Hour, Minute, Second, Sec100: WORD;

PROCEDURE StartClock;
BEGIN
   GetTime(Hour,Minute,Second,Sec100)
END;

FUNCTION ReadClock : string;
VAR
   Houro,Minuteo,Secondo,Sec100o,
   Hourf,Minutef,Secondf,Sec100f: Integer;
   time : string;
BEGIN
   Houro:=Hour; Minuteo:=Minute; Secondo:=Second; Sec100o:=Sec100;
   GetTime(Hour,Minute,Second,Sec100);
   Hourf:=Hour; Minutef:=Minute; Secondf:=Second; Sec100f:=Sec100;
   Hourf := Hourf - Houro;
   Minutef:= Minutef - Minuteo;
   Secondf := Secondf - Secondo;
   Sec100f := Sec100f - Sec100o;
   IF Sec100f < 0 THEN
      BEGIN
         Secondf:=Secondf -1;
         Sec100f:=Sec100f + 100;
      END;
   IF Secondf < 0 THEN
      BEGIN
         Minutef:=Minutef -1;
         Secondf:=Secondf + 60;
      END;
   IF Minutef < 0 THEN
      BEGIN
         Hourf := Hourf - 1;
         Minutef:= Minutef + 60
      END;
   IF (Hourf=0) THEN Time:='' ELSE Time := NumStr(Hourf,2,0)+' hours: ';
   IF (Minutef<>0) THEN Time := Time + NumStr(Minutef,2,0)+' min: ';
   Time := Time + NumStr(Secondf,2,0)+'.'+ NumStr(Sec100f,2,0)+' sec';
   ReadClock := Time;
END;


(****************** CUPS FUNCTION LIBRARY ***********************)

FUNCTION Pwr(x,y : REAL) : REAL;
VAR
   n      : longint;
   negpwr : boolean;
   temp   : real;
BEGIN
   n := trunc(y);
   IF n<>y THEN
    BEGIN
      IF x<0 THEN
         Error('Pwr(x,y): undefined for x<0 for y not an integer') ELSE
      If x=0 THEN Pwr := 0 ELSE Pwr := exp(y*ln(x));
    END
   ELSE
    BEGIN
      negpwr := (n<0);
      n      := abs(n);
      temp   := 1;
      WHILE n > 0 DO
         BEGIN
            IF odd(n) THEN
                temp := temp*x;
            x := sqr(x);
            n := n SHR 1;
         END;
      IF negpwr THEN
       if (temp=0) then
        Error('Pwr(x,y): Division by zero') ELSE
       Pwr := 1/temp
      ELSE
         Pwr := temp;
    END
END;

FUNCTION Tan(x : REAL) : REAL;
VAR c : REAL;
BEGIN
   c := cos(x);
   IF c=0 THEN
      Error('Tan(x): infinite for x = pi/2 + n*pi')
   ELSE
      tan := sin(x) / c;
END;

FUNCTION Csc(x: REAL): REAL;
VAR s : REAL;
BEGIN
   s := sin(x);
   IF s<>0 THEN
      Csc := 1.0 / s 
   ELSE
      Error('Csc: infinite for x=n*pi')
END;

FUNCTION Sec(x: REAL): REAL;
VAR c : REAL;
BEGIN
   c := cos(x);
   IF c <> 0 THEN
      Sec := 1/c
   ELSE
      Error('Sec: infinite for x = pi/2 + n*pi')
END;

FUNCTION Cot(x: REAL): REAL;
VAR s : REAL;
BEGIN
   s := sin(x);
   IF s=0 THEN
      Error('Cot(x): infinite for x = n*pi')
   ELSE
      cot := cos(x) / s;
END;

FUNCTION ArcSin(x: REAL): REAL;
BEGIN
 IF (abs(x)>1.0) THEN
    Error('ArcSin(x): undefined for |x| >1')
 ELSE
   IF x=1 THEN
       ArcSin := PI/2.0
   ELSE
     IF x = -1 THEN
       ArcSin := - PI/2.0
     ELSE
       ArcSin:=ArcTan(x/sqrt(1.0-sqr(x)));
 END;

FUNCTION ArcCos(x: REAL): REAL;
BEGIN
 IF (abs(x)>1.0) THEN
    Error('ArcCos(x): not defined for |x| > 1')
 ELSE
   IF x = 0 THEN ArcCos:=Pi/2.0
   ELSE
     IF x<0 THEN ArcCos:= ArcTan(Sqrt(1.0-sqr(x))/x) + Pi
     ELSE        ArcCos:= ArcTan(Sqrt(1.0-sqr(x))/x)
END;

FUNCTION ArcTan2(x,y: REAL): REAL;
BEGIN
   IF (x=0) THEN
      IF (y=0) THEN ArcTan2 := 0
      ELSE ArcTan2 := PI - sgn(y)*PI/2.0
   ELSE
     IF (x>0) THEN
       IF (y>=0) THEN ArcTan2 := ArcTan(y/x)
       ELSE ArcTan2 := ArcTan(y/x) + 2.0*PI
     ELSE
       ArcTan2 := Arctan(y/x) + Pi
END;

FUNCTION Sinh(x: REAL): REAL;
BEGIN
   Sinh:=(exp(x) - exp(-x))/2.0
END;

FUNCTION Cosh(x: REAL): REAL;
BEGIN
   Cosh:=(exp(x) + exp(-x))/2.0
END;

FUNCTION Tanh(x: REAL): REAL;
VAR temp : extended;
BEGIN
   IF (abs(x) < -ln(macheps)) THEN
     BEGIN
       temp := exp(2.0*x);
       Tanh :=(temp-1)/(temp+1)
     END
   ELSE IF x > 0 THEN Tanh := 1
        ELSE Tanh := -1
END;

FUNCTION Sech(x: REAL): REAL;
VAR
  z,f,recexp,p: EXTENDED;
  n,i: longint;
BEGIN
  IF abs(x) < 50 THEN
    BEGIN
      z := exp(x);
      Sech := 2.0/(z + 1.0/z)
    END
  ELSE
    BEGIN
      z:=abs(x)/ln(10);
      n:=trunc(z);
      f:=exp(frac(z)*ln(10));
      recexp:=1/f;
      p := 0.1;
      WHILE n > 0 DO
         BEGIN
            IF odd(n) THEN recexp := recexp*p;
            p := sqr(p);
            n := n SHR 1;
         END;
      Sech:=2.0*recexp/(1.0+sqr(recexp))
    END
END;

FUNCTION Csch(x: REAL): REAL;
VAR
  z,f,recexp,p: EXTENDED;
  n,i: longint;
BEGIN
  IF x = 0.0 THEN
    Error('Csch: infinite for x = 0')
  ELSE IF abs(x) < 50 THEN
    BEGIN
      z := exp(x);
      Csch := 2.0/(z - 1.0/z)
    END
  ELSE
    BEGIN
      z:=abs(x)/ln(10);
      n:=trunc(z);
      f:=exp(frac(z)*ln(10));
      recexp:=1/f;
      p := 0.1;
      WHILE n > 0 DO
      BEGIN
         IF odd(n) THEN recexp := recexp*p;
         p := sqr(p);
         n := n SHR 1;
      END;
      IF x>0 THEN
        Csch:= 2.0*recexp/(1.0-sqr(recexp))
      ELSE
        Csch:=-2.0*recexp/(1.0-sqr(recexp))
    END
END;

FUNCTION Coth(x: REAL): REAL;
VAR temp : extended;
BEGIN
   IF (abs(x) < -ln(macheps)) THEN
     BEGIN
       temp := exp(2.0*x);
       IF temp <> 1.0 THEN
         Coth := (temp + 1.0)/(temp - 1.0)
       ELSE Error('Coth: infinite for x = 0');
     END
   ELSE IF x > 0 THEN Coth := 1
        ELSE Coth := -1
END;

FUNCTION ArcSinh(x: REAL): REAL;
BEGIN
  ArcSinh := LN(x + sqrt(sqr(x) + 1.0))
END;

FUNCTION ArcCosh(x: REAL): REAL;
BEGIN
  IF x >= 1.0 THEN
    ArcCosh:=LN(x + sqrt(x*x - 1.0))
  ELSE
    Error ('ArcCosh(x) : complex value for x < 1')
END;

FUNCTION ArcTanh(x: REAL): REAL;
BEGIN
   IF abs(x)<1.0 THEN
      ArcTanh := LN((1.0 + x)/(1.0 - x))/2.0
   ELSE
      Error('ArcTanh(x): undefined for Abs(x)>= 1');
END;

FUNCTION Log(x,y : REAL) : REAL;
BEGIN
   IF (x<0) OR (y<0) OR (x=1.0) THEN
      Error('Log(x,y): undefined for x<0 or y<0 or x=1')
   ELSE
      Log :=  Ln(y) / Ln(x);
END;

FUNCTION Log10(y : REAL) : REAL;
BEGIN
   IF y<=0 THEN
      Error('Log10(y): undefined for y<=0')
   ELSE
      Log10 := Ln(y)/Ln(10);
END;

FUNCTION Sgn(y : REAL) : REAL;
BEGIN
   IF y>0 THEN sgn:=1 ELSE
   IF y=0 THEN sgn:=0 ELSE sgn:=-1;
END;

FUNCTION Sign(x,y: REAL): REAL;
BEGIN
  IF y < 0 THEN sign := -Abs(x) ELSE sign := Abs(x)
END;


PROCEDURE CxOp(z1: complex; Op: Operator; z2: complex; VAR w: complex);
VAR
   den, amp, r : REAL;
   l : complex;
BEGIN
    CASE  Op OF
      '+': BEGIN
             w.Re := z1.Re + z2.Re;
             w.Im := z1.Im + z2.Im
           END;
      '-': BEGIN
             w.Re := z1.Re - z2.Re;
             w.Im := z1.Im - z2.Im
           END;
      '*': BEGIN
             w.Re := z1.Re*z2.Re - z1.Im*z2.Im;
             w.Im := z1.Re*z2.Im + z1.Im*z2.Re
           END;
      '/': BEGIN
             IF (z2.Re=0) AND (z2.Im=0) THEN
               Error('CxOp: attempted division by zero')
             ELSE
               IF (abs(z2.Re)>abs(z2.Im)) THEN
                 BEGIN
                   r := z2.Im/z2.Re;
                   den := z2.Re + z2.Im*r;
                   w.Re := (z1.Re + z1.Im*r)/den;
                   w.Im := (z1.Im - z1.Re*r)/den;
                 END
               ELSE
                 BEGIN
                  r := z2.Re/z2.Im;
                  den := z2.Re*r + z2.Im;
                  w.Re := (z1.Re*r + z1.Im)/den;
                  w.Im := (z1.Im*r - z1.Re)/den
                 END
          END;
      '^': BEGIN
               clog(z1,l);
               amp  := exp( z2.re*l.re -z2.im*l.im);
               den  := z2.im*l.re +z2.re*l.im;
               w.re := amp*cos(den);
               w.im := amp*sin(den);
           END;
    END; {case}
END;

PROCEDURE RCprod(x: REAL; z: complex; VAR w: complex);
BEGIN
   w.Re := x*z.Re;
   w.Im := x*z.Im
END;

FUNCTION CAbs(z: complex): REAL;
BEGIN
   IF (z.Re=0) AND (z.Im=0) THEN cabs := 0 ELSE
   IF (abs(z.Re) > abs(z.Im)) THEN
        cabs := abs(z.Re)*Sqrt(1 + Sqr(z.Im/z.Re))
   ELSE cabs := abs(z.Im)*Sqrt(1 + Sqr(z.Re/z.Im));
END;

FUNCTION Arg(z : complex): REAL; {Argument is in quadrants 1..4}
VAR
   a: REAL;
BEGIN
   IF z.re<>0 THEN a:=arcTan((z.im)/(z.re)) ELSE a:= sgn(z.im)*pi/2;
   IF z.re<0 THEN
    IF z.im>=0 THEN arg:=a+pi ELSE arg:=a-pi
   ELSE arg:=a;
END;

PROCEDURE CLog(z: complex; VAR w:complex);
VAR
   zabs: REAL;
BEGIN
   zabs := Sqrt(Sqr(z.Re) + Sqr(z.Im));
   IF zabs=0 THEN
      Error('CLog: infinite when z has magnitude zero')
   ELSE
      BEGIN
         w.Re := Ln(zabs);
         w.Im := Arg(z)
      END
END;

PROCEDURE CPwr(z:complex; y:REAL; VAR w:complex);
VAR
   argz,ampw: REAL;
BEGIN
   IF (z.Re=0) AND (z.Im=0) THEN ampw:=0 ELSE
   ampw := exp( y/2.0* ln(Sqr(z.Re)+Sqr(z.Im)) );
   argz:= y*Arg(z);
   w.Re:= ampw*Cos(argz);
   w.Im:= ampw*Sin(argz)
END;

PROCEDURE CExp(z:complex; VAR w:complex);
VAR
   s : REAL;
BEGIN
  s    := Exp(z.Re);
  w.Re := s*Cos(z.im);
  w.Im := s*Sin(z.im);
END;

PROCEDURE CSin(z:complex; VAR w:complex);
VAR e : real;
BEGIN
   e := exp(z.im);
   w.Re := Sin(z.Re)*(e+1/e)/2;
   w.Im := Cos(z.Re)*(e-1/e)/2;
END;

PROCEDURE CCos(z:complex; VAR w:complex);
VAR e : real;
BEGIN
   e := exp(z.im);
   w.Re := Cos(z.Re)*(e+1/e)/2;
   w.Im :=-Sin(z.Re)*(e-1/e)/2;
END;

PROCEDURE CTan(z:complex; VAR w:complex);
BEGIN
   w.Re := Sin (2.0*z.Re)/(Cos(2.0*z.Re) + Cosh(z.Im));
   w.Im := Sinh(2.0*z.Im)/(Cos(2.0*z.Re) + Cosh(2.0*z.Im))
END;

END.{CUPSfunc}
