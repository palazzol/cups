                 (**********************************************)
                 (**********************************************)
                 (**                                          **)
                 (**     Unit CUPSpars - Parser Utility       **)
                 (**     Written by Jarek Tuszynski           **)
                 (**     for CUPS Project                     **)
                 (**     Department of Physics and Astronomy  **)
                 (**     George Mason University              **)
                 (**     Fairfax, Virginia 22030              **)
                 (**     Based on Program by S. Saperstone    **)
                 (**     (c) 1994 by Jarek Tuszynski          **)
                 (**     Originated:  92/02/13                **)
                 (**     Version: 1.3 (94/11/14)              **)
                 (**                                          **)
                 (**********************************************)
                 (**********************************************)

{$O+,F+}
UNIT CUPSpars;

INTERFACE
USES CUPSmupp, CUPS, CUPSfunc, CUPSproc;
TYPE
   TParser = object
      PROCEDURE Init;
      FUNCTION  f ( x,y,z,t : REAL ) : REAL;
      PROCEDURE cxf(x,y,z,t : complex; VAR out : complex);
      FUNCTION  parse( z1,z2,z3,z4 : char; inString : string) : boolean;
      PROCEDURE SetRealVariable (name : string; num : real);
      PROCEDURE SetComplexVariable (name : string; num : complex);
   PRIVATE
      RPNstring   : string;         {Reverse Polish Notation of the function}
      ch1,ch2,ch3,ch4 : char;
      complexMode : boolean;
      NumConst    : integer;
      constName   : ARRAY [1..20] OF string[10];
      reConstNum  : ARRAY [1..20] OF real;
      imConstNum  : ARRAY [1..20] OF real;
      PROCEDURE Encode( VAR inString : string);
   END;

   PROCEDURE AboutParser( VAR ParserHelp : HelpScrType);


IMPLEMENTATION
USES crt;
VAR
  errorFlag : boolean;

PROCEDURE AboutParser( VAR ParserHelp : HelpScrType);
BEGIN
   ParserHelp[01] := '                  About Parser                    ';
   ParserHelp[02] := '                                                  ';
   ParserHelp[03] := 'The parser can recognize the following:           ';
   ParserHelp[04] := '1) regular functions:                             ';
   ParserHelp[05] := ' abs      -absolute value                         ';
   ParserHelp[06] := ' exp      -exponential                            ';
   ParserHelp[07] := ' int,frac -integer and fractional part of argument';
   ParserHelp[08] := ' re,im    -real and imaginery part of complex num ';
   ParserHelp[09] := ' sqrt,sqr -square root and square                 ';
   ParserHelp[10] := ' log,ln   -logarithms of base 10 and e            ';
   ParserHelp[11] := ' H        -unit step (1 if x>0 and 0 otherwise)   ';
   ParserHelp[12] := ' sgn      -sign (-1 if x<0, 0 if x=0, 1 if x>0)   ';
   ParserHelp[13] := ' rand     -random number from 0 to 1              ';
   ParserHelp[14] := '2) trigonometric and hyperbolic functions:        ';
   ParserHelp[15] := ' sin,    cos,    tan,    csc,     sec,     cot    ';
   ParserHelp[16] := ' sinh,   cosh,   tanh,   csch,    sech,    coth,  ';
   ParserHelp[17] := ' arcsin, arccos, arctan, arcsinh, arccosh, arctanh';
   ParserHelp[18] := '3) constants  : pi=3.14159, e=2.71828, i=sqrt(-1) ';
   ParserHelp[19] := '4) operations : +, -, *, /, ^, !                  ';
   ParserHelp[20] := '5) shortcuts  : r=sqrt(x^2+y^2) and  c=x+i*y      ';
   ParserHelp[21] := '6) parenthesis: [,{,(,),},]                       ';
   ParserHelp[22] := '                                                  ';
   ParserHelp[23] := ' If you are working with complex numbers you have ';
   ParserHelp[24] := ' to convert to reals using: re, im or abs. Notice ';
   ParserHelp[25] := ' that not all functions have infinite domain.     ';
END;

PROCEDURE TParser.Encode( VAR inString : string);
VAR
  outString : string;
  t, m : integer;
  more : boolean;
  k, len, lensub : integer;
CONST
  num = 32;
  long  : ARRAY[1..num] OF string[7] =
   ('abs','exp','frac','int','ln','arcsinh','arccosh','arctanh','arcsin',
    'arccos','arctan','sinh','cosh','tanh','cos','sin','tan','pi','sqrt',
    'sqr','sech','csch','coth','sec','csc','cot','log','h','im','re','sgn','rand');
  short : ARRAY[1..num] OF char = 'AEFILNVBDGJKMOCSTPRQUWXYZ?@H><=;';
BEGIN  { encode }
  REPEAT                                               {skip blanks}
     k := pos(' ',inString);
     IF k<>0 THEN delete(InString,k,1);
  UNTIL k=0;
  FOR k := 1 TO length(inString) DO                    {put to lower case}
   IF inString[k] IN ['A'..'Z'] THEN inString[k]:=chr(ord(inString[k])+32);
  k := 0;                                              {mark end of numbers}
  WHILE k<length(inString) DO
  BEGIN
     IF inString[k+1] IN ['0'..'9','.'] then
     BEGIN
        WHILE inString[k+1] IN ['0'..'9','.'] DO inc(k);
        inc(k);
        insert(' ',inString,k);
     END;
     inc(k);
  END;
  FOR t := 1 TO NumConst DO           {encode constants}
  REPEAT
     k := pos(ConstName[t], inString);
     IF k = 0 THEN
     BEGIN
       more := false;
       outString := inString;
     END
     ELSE
     BEGIN
       more := true;
       len := length( inString);
       lensub := length( ConstName[t]);
       outString := copy( inString, 1, k-1) + chr(127+t)
                  + copy( inString, k+lensub, len-k-lensub+1);
     END; { else }
     inString := outString;
  UNTIL NOT more;
  FOR t := 1 TO num DO        {encode functions}
  REPEAT
     k := pos(long[t], inString);
     IF k = 0 THEN
     BEGIN
       more := false;
       outString := inString;
     END
     ELSE
     BEGIN
       more := true;
       len := length( inString);
       lensub := length( long[t]);
       outString := copy( inString, 1, k-1) + short[t]
                  + copy( inString, k+lensub, len-k-lensub+1);
     END; { else }
     inString := outString;
  UNTIL NOT more;
  inString := outString;
END;  { encode }



PROCEDURE unary_minus(VAR source : string);
VAR
  p : integer;
  symb : char;
BEGIN
  p := 1;
  IF source[1] = '-' THEN source[1] := '_';
  IF source[1] = '+' THEN delete(source,1,1);
  p := 2;
  WHILE p <= length(source) DO
  BEGIN
     symb := source[p];
     IF (symb='-') AND (source[p-1] IN ['*','/','+','-','^','(','{','['])
     THEN source[p] := '_';
     IF (symb='+') AND (source[p-1] IN ['*','/','+','-','^','(','{','['])
     THEN delete(source,p,1);
     p := p + 1;
  END;  { while }
END;


FUNCTION FillRPNstring(source : string; VAR RPNstrng : string) : boolean;
VAR
  opstk : string;
  stktop, rank : integer;
  srank, oprank : ARRAY [1..256] OF integer;   (* ranks FOR source AND opstk *)
  sPos, rPos: integer;                 (* positions IN source AND RPNstring *)
BEGIN                                               (* fill the ARRAY srank *)
  sPos := 1;
  WHILE sPos<=length(source) DO
  BEGIN
     CASE source[sPos] OF
      '(','{','[' : srank[sPos] := 0;
      ')','}',']' : srank[sPos] := 2;
      '+','-'     : srank[sPos] := 3;
      '*','/'     : srank[sPos] := 4;
      '_'         : srank[sPos] := 5;
      '^','!'     : srank[sPos] := 6;
      ';'..'Z'    : srank[sPos] := 7;
     ELSE           srank[sPos] :=-1;                   (* an operand *)
     END; {case}
     sPos := sPos+1;
  END (* WHILE *);
  srank[sPos] := 1;                                             (* a blank *)
           (* create the string "RPNstrng", temporarily stacking operators *)
  sPos := 0;
  rPos := 1;
  stktop := 1;
  oprank[1] := -2;
  REPEAT
    inc(sPos);
    rank := srank[sPos];
    IF rank>=1 THEN                           (* an operator, ')', OR blank *)
      WHILE oprank[stktop]>=rank DO         (* pop operators from the stack *)
      BEGIN
        RPNstrng[rPos] := opstk[stktop];
        dec(stktop);
        inc(rPos);
      END;
    CASE rank OF
     2   : dec(stktop);                           (* pop '(' from the stack *)
     1,-1: BEGIN                      (* place operand OR blank IN RPNstrng *)
             RPNstrng[rPos] := source[sPos];
             inc(rPos);
           END;
    ELSE   BEGIN                       (* push '(' OR operator on the stack *)
             inc(stktop);
             opstk[stktop] := source[sPos];
             oprank[stktop] := srank[sPos];
           END;
    END; {case}
  UNTIL rank=1;
  RPNstrng[0] := chr(rPos-2);
  if (stktop>1) and (opstk[stktop] in ['(','{','[']) then
   announce('Parser Error: Right hand bracket expected.');
  FillRPNstring := (stktop=1);
END (* FillRPNstring *);


FUNCTION TParser.f(x,y,z,t : REAL) : REAL;
CONST infinite = 1.7e+38;
VAR
    opndstk : ARRAY[1..256] OF REAL;                      (*stack OF operands*)
    stktop,i : integer;
    position,p,code : integer;                     (* position IN RPNstring *)
    symb : char;                                (* next symbol IN RPNstring *)
    op2,op1 : REAL;                         (* two top operand on the stack *)
    value : REAL;                         (*value TO be pushed on the stack *)
    cx,cy,cz,ct,out : complex;

   FUNCTION pop : REAL;               {in: opndstk,stktop; out: stktop}
   BEGIN
      IF stktop >=1 THEN
      BEGIN
         pop := opndstk[stktop];
         stktop := stktop - 1;
      END
      ELSE errorFlag:=true;  {too few operands}
   END;

   PROCEDURE push( newtop : REAL);    {in: opndstk,stktop; out: stktop}
   BEGIN
       stktop := stktop + 1;
       opndstk[stktop] := newtop;
   END;

BEGIN (* f *)
    stktop := 0;
    position := 1;
    symb := RPNstring[position];
    ComplexMode := ComplexMode            or (Pos('<',RPNstring)>0) or
                   (Pos('>',RPNstring)>0) or (Pos('A',RPNstring)>0);
    IF complexMode THEN
    BEGIN
       cx.re:=x; cx.im:=0; cy.re:=y; cy.im:=0;
       cz.re:=z; cz.im:=0; ct.re:=t; ct.im:=0;
       cxf(cx,cy,cz,ct,out);
       IF out.im=0 THEN f:=out.re ELSE
       Error('Parser output is a complex number. Real number expected.');
    END ELSE
    WHILE (position<=length(RPNstring)) AND (NOT ErrorFlag) DO
    BEGIN                      (* find next value TO be pushed on the stack *)
        IF symb IN ['0'..'9', '.'] THEN
        BEGIN
           p := position;
           WHILE (RPNstring[position+1]<>' ') DO inc(position);
           inc(position);
           val ( copy(RPNstring,p,position-p), value, code);
        END ELSE
        IF symb=ch1 THEN value:=x ELSE
        IF symb=ch2 THEN value:=y ELSE
        IF symb=ch3 THEN value:=z ELSE
        IF symb=ch4 THEN value:=t ELSE
        IF (symb>=chr(128)) AND (symb<=chr(148)) THEN
        BEGIN
           i := 1;
           WHILE (i<=NumConst) AND (symb<>chr(127+i)) DO inc(i);
           value := ReConstNum[i];
           IF ImConstNum[i]<>0 THEN BEGIN ComplexMode := true; exit END;
        END ELSE
        CASE symb OF
         '+': BEGIN op2:=pop; value:=pop+op2 END;
         '-': BEGIN op2:=pop; value:=pop-op2 END;
         '*': BEGIN op2:=pop; value:=pop*op2 END;
         '/': BEGIN
                 op2:=pop;
                 op1:=pop;
                 IF op2<>0 THEN value:=op1/op2  ELSE
                 IF op1<>0 THEN value:=infinite ELSE value:=0;
              END;
         '^': BEGIN
                 op2:=pop;
                 op1:=pop;
                 IF (op1=0) AND (op2<0) THEN value:=infinite
                 ELSE value:=pwr(op1,op2);
              END;
         'P': value := pi;                                           {pi}
         'r': value := sqrt(x*x+y*y+z*z);                            {r}
         'e': value := exp(1);                                       {e}
         '_': value := -1*pop;                                       {-}
         'i','c': BEGIN ComplexMode := true; exit END;               {i,c}
         'H': IF pop<=0 THEN value:=0 ELSE value:=1;
         'L': BEGIN
                 op2:=pop;
                 IF op2>0 THEN value:=ln(op2) ELSE
                 IF op2=0 THEN value:=-infinite  ELSE
                 error('Ln(x) is undifinded FOR x<0');
              END;
         'E': value := exp( pop);
         'A': value := abs( pop);
         'C': value := cos( pop);
         'S': value := sin( pop);
         'T': BEGIN
                 op2:= pop;
                 IF cos(op2)=0 THEN value := infinite
                 ELSE value := tan( op2);
              END;
         'R': BEGIN
                 op2:=pop;
                 IF op2>=0 THEN value := sqrt( op2)
                 ELSE error('sqrt(x) is undefined FOR x<0');
              END;
         'Q': value := sqr ( pop);
         'K': value := sinh( pop);
         'M': value := cosh( pop);
         'O': value := tanh( pop);
         'D': value := arcsin( pop);
         'G': value := arccos( pop);
         'J': value := arctan( pop);
         'N': value := arcsinh( pop);
         'V': value := arccosh( pop);
         'B': value := arctanh( pop);
         'U': value := sech( pop);
         'W': value := csch( pop);
         'X': value := coth( pop);
         'Y': value := sec(  pop);
         'Z': value := csc(  pop);
         '?': value := cot(  pop);
         '@': value := log10(pop);
         'F': value := frac( pop);
         'I': value := int(  pop);
         '=': value := sgn(  pop);
         ';': value := random;
         '!': BEGIN
                 op2:=pop;
                 value:=1;
                 IF (op2=trunc(op2)) AND (op2>=0) THEN
                   FOR p := trunc(op2) DOWNTO 1 DO value:=value*p
                 ELSE Error('Factorial is only definded for positive integers.');
              END;
         ELSE errorFlag := true;                          {illegal symbol}
        END; {case}
        push( value);
        inc(position);
        symb := RPNstring[position];
    END (* WHILE *);
   IF NOT complexMode THEN
   IF stktop <= 1 THEN f:=pop ELSE errorFlag := true;    {too many operands}
END (* f *);


PROCEDURE TParser.cxf(x,y,z,t : complex;  VAR out : complex);
CONST infinite = 1.7e+38;
VAR
    opndstk : packed ARRAY[1..256] OF complex;            (*stack OF operands*)
    stktop,i : integer;
    position,p,code : integer;                     (* position IN RPNstring *)
    symb : char;                                (* next symbol IN RPNstring *)
    a,b,c : complex;               (* next TO top operand on the stack *)
    r,s : real;

   PROCEDURE pop( VAR element : complex);
   BEGIN
      IF stktop >=1 THEN
      BEGIN
         element := opndstk[stktop];
         stktop := stktop - 1;
      END
      ELSE errorFlag:=true;  {too few operands}
   END;

   PROCEDURE push( element : complex);
   BEGIN
       stktop := stktop + 1;
       opndstk[stktop] := element;
   END;

   PROCEDURE invert(VAR c,z : complex);
   VAR r : real;
   BEGIN
      r := (sqr(c.re) + sqr(c.im));
      if r=0 then error('Parser: Division by complex number (0+i*0)') else
      begin z.re:=c.re/r;  z.im:=-c.im/r end;
   END;

PROCEDURE cLog10(z: complex; VAR w:complex);
VAR
   zabs: REAL;
BEGIN
   zabs := Sqrt(Sqr(z.Re) + Sqr(z.Im));
   IF zabs=0 THEN
      Error('CLog10: infinite when z has magnitude zero')
   ELSE
      BEGIN
         w.Re := Log10(zabs);
         w.Im := Arg(z);
      END
END; 

BEGIN (*  *)
  stktop := 0;
  position := 1;
  symb := RPNstring[position];
  WHILE (position<=length(RPNstring)) AND (NOT ErrorFlag) DO
  BEGIN                      (* find next value TO be pushed on the stack *)
    c.re:=0; c.im:=0;
    IF symb IN ['0'..'9', '.'] THEN
    BEGIN
       p := position;
       WHILE (RPNstring[position+1]<>' ') DO inc(position);
       inc(position);
       val( copy(RPNstring,p,position-p), c.re, code);
    END ELSE
    IF symb=ch1 THEN c:=x ELSE
    IF symb=ch2 THEN c:=y ELSE
    IF symb=ch3 THEN c:=z ELSE
    IF symb=ch4 THEN c:=t ELSE
    IF (symb>=chr(128)) AND (symb<=chr(148)) THEN
    BEGIN
       i := 1;
       WHILE (i<=NumConst) AND (symb<>chr(127+i)) DO inc(i);
       c.re := ReConstNum[i];
       c.im := ImConstNum[i];
    END ELSE
    CASE symb OF
     '+','-','*','/','^' :                                             {+-*/^}
          BEGIN pop(b); pop(a); CxOp(a,symb,b,c); END;
     'r': c.re := sqrt(sqr(x.re)+sqr(y.re)+sqr(z.re));                 {r}
     'c': BEGIN c.re := x.re; c.im := y.re END;                        {c}
     'e': c.re := exp(1);                                              {e}
     'P': c.re := pi;                                                  {pi}
     'i': c.im := 1;                                                   {i}
     '_': BEGIN pop(a); c.re:=-a.re; c.im:=-a.im; END;                 {-}
     '>': BEGIN pop(a); c.re:=a.im END;                                {im}
     '<': BEGIN pop(a); c.re:=a.re END;                                {re}
     'H': BEGIN pop(a); IF a.re>0 THEN c.re:=1;
                        IF a.im>0 THEN c.im:=1; END;                   {H}
     'A': BEGIN pop(a); c.re:=cAbs(a) END;                             {abs}
     'E': BEGIN pop(a); cExp(a,c) END;                                 {exp}
     'L': BEGIN pop(a); cLog(a,c) END;                                 {ln}
     'C': BEGIN pop(a); cCos(a,c) END;                                 {cos}
     'S': BEGIN pop(a); cSin(a,c) END;                                 {sin}
     'T': BEGIN pop(a); cTan(a,c) END;                                 {tan}
     'R': BEGIN pop(a); cPwr(a,0.5,c) END;                             {sqrt}
     'Q': BEGIN pop(a); c.re:=sqr(a.re)-sqr(a.im); c.im:=2*a.re*a.im END;{sqr}
     'K': BEGIN pop(a); cExp(a,a); invert(a,b);
                c.re:=(a.re-b.re)/2; c.im:=(a.im-b.im)/2 END;          {sinh}
     'M': BEGIN pop(a); cExp(a,a); invert(a,b);
                c.re:=(a.re+b.re)/2; c.im:=(a.im+b.im)/2 END;          {cosh}
     'O': BEGIN pop(a); cExp(a,a); cPwr(a,2,b); a.im:=b.im;
                a.re:=b.re-1; b.re:=b.re+1; CxOp(a,'/',b,c) END;       {tanh}
     'D': BEGIN pop(a); b.re:=1-sqr(a.re)+sqr(a.im); b.im:=-2*a.re*a.im;
                r:=Arg(b); s:=sqrt(cAbs(b));
                b.re:=s*cos(r/2)-a.im; b.im:=s*sin(r/2)+a.re;
                cLog(b,a); c.re:=a.im; c.im:=-a.re; END;             {arcsin}
     'G': BEGIN pop(a); b.re:=1-sqr(a.re)+sqr(a.im); b.im:=-2*a.re*a.im;
                r:=Arg(b); s:=-sqrt(cAbs(b));
                b.re:=s*cos(r/2)-a.im; b.im:=s*sin(r/2)+a.re;
                cLog(b,a); c.re:=a.im; c.im:=-a.re; END;             {arccos}
     'J': BEGIN pop(a); r:=sqr(a.re)+sqr(a.im+1); b.im:=2*a.re/r;
                b.re:=(1-sqr(a.re)-sqr(a.im))/r; cLog(b,a);
                c.re:=a.im/2; c.im:=-a.re/2; END;                    {arctan}
     'N': BEGIN pop(a); b.re:=sqr(a.re)-sqr(a.im)+1; b.im:=2*a.re*a.im;
                r:=Arg(b); s:=sqrt(cAbs(b)); b.re:=s*cos(r/2)+a.re;
                b.im:=s*sin(r/2)+a.im; cLog(b,c); END;               {arcsinh}

     'V': BEGIN pop(a); b.re:=sqr(a.re)-sqr(a.im)-1; b.im:=2*a.re*a.im;
                r:=Arg(b); s:=sqrt(cAbs(b)); b.re:=s*cos(r/2)+a.re;
                b.im:=s*sin(r/2)+a.im; cLog(b,c); END;               {arccosh}

     'B': BEGIN pop(a); r:=sqr(1-a.re)+sqr(a.im); b.im:=2*a.im/r;
                b.re:=(1-sqr(a.re)-sqr(a.im))/r; cLog(b,a);
                c.re:=a.re/2; c.im:=a.im/2; END;                     {arctanh}
     'U': BEGIN pop(a); cExp(a,a); invert(a,b); c.re:=(a.re+b.re)/2;
                c.im:=(a.im+b.im)/2; invert(c,c) END;                {sech}
     'W': BEGIN pop(a); cExp(a,a); invert(a,b); c.re:=(a.re-b.re)/2;
                c.im:=(a.im-b.im)/2; invert(c,c) END;                {csch}
     'X': BEGIN pop(a); cExp(a,a); cPwr(a,2,b); a.re:=b.re+1;
                a.im:=b.im; b.re:=b.re-1;  CxOp(a,'/',b,c) END;      {coth}                                    {tanh}
     'Y': BEGIN pop(a); cCos(a,b); invert(b,c) END;                  {sec}
     'Z': BEGIN pop(a); cSin(a,b); invert(b,c) END;                  {csc}
     '?': BEGIN pop(a); cTan(a,b); invert(b,c) END;                  {cot}
     '@': BEGIN pop(a); cLog10(a,c); END;                            {log}
     'F': BEGIN pop(a); c.re:=frac(a.re); c.im:=frac(a.im) END;      {frac}
     'I': BEGIN pop(a); c.re:=int(a.re);  c.im:=int(a.im)  END;      {int}
     '=': BEGIN pop(a); c.re:=sgn(a.re);  c.im:=sgn(a.im)  END;      {sgn}
     ';': BEGIN         c.re:=random;     c.im := random;  END;      {rand}
     '!': BEGIN pop(a); c.im:=1; c.re:=1;                            {!}
             IF  (a.im=trunc(a.im)) AND (a.im>=0)
             AND (a.re=trunc(a.re)) AND (a.re>=0) THEN
             BEGIN
               FOR p := trunc(a.re) DOWNTO 1 DO c.re:=c.re*p;
               FOR p := trunc(a.im) DOWNTO 1 DO c.im:=c.im*p;
             END
             ELSE Error('Factorial is only definded for positive integers.');
          END;
     ELSE errorFlag := true;                              {illegal symbol}
    END; {case}
    push (c);
    position := position + 1;
    symb := RPNstring[position];
  END (* WHILE *);
 IF stktop <= 1 THEN pop(out) ELSE errorFlag := true;    {too many operands}
END (* c *);


FUNCTION TParser.parse( z1,z2,z3,z4 : char; inString : string) : boolean;
VAR
   z : REAL;
   cx, out : complex;
BEGIN
    IF (numConst<0) OR (numConst>20) THEN init;
    ch1:=z1; ch2:=z2;
    ch3:=z3; ch4:=z4;
    errorFound  := false;
    errorFlag   := false;
    complexMode := false;
    encode( inString);
    unary_minus( inString);
    if FillRPNstring(inString,RPNstring) then
    begin
       HaltIfError := false;
       z := f(1,1,1,1);
       IF complexMode THEN
       BEGIN
          cx.re:=1; cx.im:=0;
          cxf(cx,cx,cx,cx,out);
       END;
       parse := NOT (errorFlag OR errorFound);
       HaltIfError := true;
       errorFound  := false;
    end
    else parse := false;
END;

PROCEDURE TParser.Init;
VAR i : integer;
BEGIN
   randomize;
   NumConst := 0;
   FOR i := 1 TO  20 DO constName[i]  := '';
   FOR i := 1 TO  20 DO ReConstNum[i] := 0;
   FOR i := 1 TO  20 DO ImConstNum[i] := 0;
END;

PROCEDURE TParser.SetRealVariable(name : string; num : real);
VAR cplx : complex;
BEGIN
   cplx.re := num;
   cplx.im := 0;
   SetComplexVariable(name,cplx);
END;

PROCEDURE TParser.SetComplexVariable(name : string; num : complex);
VAR
   i : integer;
BEGIN
   FOR i := 1 TO length(name) DO                    {put to lower case}
    IF name[i] IN ['A'..'Z'] THEN name[i]:=chr(ord(name[i])+32);
   IF length(name)>10 THEN
   announce(concat('Const name ',name,' will be truncated TO ',copy(name,1,10)));
   name := copy(name,1,10);
   i := 1;
   WHILE (i<=NumConst) AND (constName[i]<>name) DO inc(i);
   IF (constName[i]=name) THEN
   BEGIN
      ReConstNum[i] := num.re;
      ImConstNum[i] := num.im;
   END ELSE
   IF NumConst=20 THEN
    announce('Parser can not have more then 20 constants.') ELSE
   BEGIN
      inc(NumConst);
      ConstName [NumConst] := name;
      ReConstNum[NumConst] := num.re;
      ImConstNum[NumConst] := num.im;
   END;
END;

END. {CUPSpars}
