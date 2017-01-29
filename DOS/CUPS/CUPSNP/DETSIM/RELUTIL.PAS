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

                     {*******************************}
                     {               RELUTIL         }
                     {    Utilities                  }
                     {              21 May 1992      }
                     {              18 Oct 1993      }
                     {*******************************}
UNIT relutil;

Interface

USES graph, crt, cups, CUPSmupp, cupsgrph, cupsfunc ;
TYPE
    lstr = string[6];

TYPE    point   =RECORD  v:INTEGER;
                         h:INTEGER;
                 END;
        rpnt    =RECORD  v:REAL;
                         h:REAL;
                 END;
VAR
        circsteps  : Integer;

VAR     circr : ARRAY[1..33] OF point;
        unitcirc : ARRAY[1..33] OF rpnt;

PROCEDURE LocatePoint(X,Y:integer; VAR col,row:integer);
PROCEDURE DrawBlankBox(X,Y,len:integer; ThisColor:word);
PROCEDURE plotrect(x1,y1,x2,y2: Real);
PROCEDURE plotcirc(x,y,rad: Real);
PROCEDURE plottext(x,y: Real; sval :lStr);
PROCEDURE Initcirc;
PROCEDURE DrawFillCirc(X,Y,rad:Real; ThisColor:word);
FUNCTION decstr(aa:real; ii:integer):string;
FUNCTION Rand_gauss(center, sigma: Real) : Real;      { Random Gaussian}
FUNCTION breitwig(center,fullwid: Real): Real;        { Random Breit_Wigner}
FUNCTION exponen(lifetime: Real) : Real;              { Random exponential}

Implementation
                 { from MUPPET}

PROCEDURE LocatePoint(X,Y:integer; VAR col,row:integer);
BEGIN Col := Round((x-0.6) * ColWidth);
      Row := Round((y-0.6) * RowHt);
END;


PROCEDURE DrawBlankBox(X,Y,len:integer; ThisColor:word);
VAR   Row,Col: integer;
      ThisRect: ARRAY[1..5] OF point;
BEGIN LocatePoint(X,Y,col,row);
      Col := Col - Round(0.1 * ColWidth);
      Row := Row - Round(0.5 * RowHt) + 2;
      ThisRect[1].v := Col;              ThisRect[1].h := Row+1;
      ThisRect[2].v := Col;              ThisRect[2].h := Row+RowHt;
      ThisRect[3].v := Col+len*ColWidth; ThisRect[3].h := Row+RowHt;
      ThisRect[4].v := Col+len*ColWidth; ThisRect[4].h := Row+1;
      ThisRect[5].v := Col;              ThisRect[5].h := Row+1;
      SetColor(ThisColor);
      SetFillStyle(1,ThisColor);
      DrawPoly(5,ThisRect);
      FillPoly(5,ThisRect);
END;

PROCEDURE InitCirc;
VAR   i     : Integer;
      phi   : Real;

BEGIN
      circsteps := 33;
      FOR i:=1 TO circsteps DO
      BEGIN
         phi:= (i-1)*2*pi/(circsteps-1);
         unitcirc[i].v:= cos(phi);   unitcirc[i].h:=sin(phi);
      END;
END;

PROCEDURE DrawFillCirc(X,Y,rad:Real; ThisColor:word);
VAR   i : integer;
      Xscr,Yscr,Radius : Integer;
BEGIN
         Xscr:=mapx(x); Yscr:=mapy(y);
         Radius:= mapx(rad);
         Radius:= Radius - Xscr;

      FOR i :=1 TO circsteps DO
      BEGIN
         circr[i].v:= Round(unitcirc[i].v * radius + xscr);
         circr[i].h:= Round(unitcirc[i].h * radius + yscr);
      END;
      SetColor(ThisColor);
      SetFillStyle(1,ThisColor);
      DrawPoly(circsteps,circr);
      FillPoly(circsteps,circr);
END;

PROCEDURE plotrect(x1,y1,x2,y2: Real);
BEGIN
     Plotline(x1,y1,x2,y1); Plotline(x2,y1,x2,y2);
     Plotline(x2,y2,x1,y2); Plotline(x1,y2,x1,y1);
END;

PROCEDURE plotcirc(x,y,rad: Real);
VAR
   Xscr, Yscr, Radius   : Integer;
BEGIN

   Xscr:=mapx(x); Yscr:=mapy(y);
   Radius:= mapx(rad);
   Radius:= Radius - Xscr;
   Graph.Circle(Xscr, Yscr, Radius);

END;

PROCEDURE plottext(x,y: Real; sval :lStr);
VAR
     Xscr, Yscr  : Integer;
BEGIN
     Xscr:=mapx(x); Yscr:=mapy(y);
     OutTextxy(Xscr,Yscr,sval);
END;

FUNCTION decstr(aa:real; ii:integer):string;
VAR  tempstr  : string;
     jj       : Integer;
BEGIN
    jj:=0;
    IF (aa > 0.01) and (aa <10000) THEN
    BEGIN
      IF(aa <= 1) and (aa > 0) THEN jj := 3 ELSE
      IF(aa > 1) and (aa < 100) THEN jj:=2  ELSE
      IF(aa >= 100) and (aa < 1000) THEN jj:=1;
      str(aa:ii:jj, tempstr);
      decstr:= tempstr;
    END
    ELSE IF ( (aa>0) and (aa < 0.01) ) or (aa > 10000) THEN
      BEGIN
         str(aa:ii,tempstr); decstr:=tempstr;
      END
    ELSE
      BEGIN
        str(aa:ii:0,tempstr); decstr:=tempstr;
      END;

END;


FUNCTION Rand_gauss(center, sigma: Real) : Real;   {Random Gaussian}
VAR
     twou, radius, theta : Real;
BEGIN
     twou:= -2*ln(1-random);
     radius:= Sqrt(twou);
     theta:= 2*pi*Random;
     Rand_gauss:= sigma*radius*cos(theta) + center;

END;

FUNCTION breitwig(center,fullwid: Real): Real;   {random Breit_Wigner}
VAR
     rr: Real;
BEGIN
     rr := 0.8*Random + 0.1;                            {cut off tails}
     breitwig:= center + 0.5*fullwid*Tan(pi*(rr-0.5));
END;

FUNCTION exponen(lifetime: Real): Real;       {random exponential}
BEGIN
     exponen:= -ln(1-random)*lifetime;
END;


END.
