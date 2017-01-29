            (*************************************************)
            (*************************************************)
            (**                                             **)
            (** Consortium of Upper-level Physics Software  **)
            (**             ( CUPS Project )                **)
            (**     (c) 1994 by John Wiley & Sons           **)
            (** Compiled with Utilities Ver. 1.8 (95/08/08) **)
            (**                                             **)
            (*************************************************)
            (*************************************************)


unit LensMatS;
{ provides Turbo Pascal wih facilities for }
{ skew ray tracing and seidel aberration data }

interface

uses
    crt,graph,dos,CUPSMUPP,CUPS,CupsProc,CupsFunc,Cupsgui,CupsGrph;

const
	done = 1111;	{ init flag }
	up = 1;   { arrow directions }
	down = -1;

type
     rays = record
               x,y,z:real;    { position }
               l,m,n:real;    { direction cosines }
               failed:boolean; { set to true if ray bends out of lens }
               color : INTEGER;
            end;

	rayPaths = array[0..102] of rays;

     lenses = record
               c:array[0..102] of real; { curvatures }
               t:array[0..102] of real; { thicnkesses }
               n:array[0..102] of real; { refractive indexes }
               r:array[0..102] of real; { radius (stops only) }
               x,y,z:real;               { position of leftmost surface}
               scale:real;               { size on screen }
               fl:real;                  { focal length }
               bfl:real;                  { back focal length }
			gip:real;	{ gaussian image point }
              end;

	traceModes = (finite,paraxial);

	vector = array[1..70] of real;

var
     lens:lenses;             { optical system }
     lensFile:text;           { standard serial text file }
     last:integer;            { last surface in lens }
	TraceMode:TraceModes;
	RayPath:raypaths;		{ ray coordinates }
     initialized:integer;     { flag indicating initialization state }
     saved:boolean;	{ TRUE if a successfully loaded or saved }
	AberrCount:integer;
	NoAberrData:boolean; { indicates whether aberration data has been
                               calculated for current system }

function sPos(s:integer):real;
{ distance of surface s from first surface }

function rad(a:real):real;
{ converts degrees to radians }

procedure focus(var fl,bfl,gip:real);
{ find focal lengths of current system }

procedure initTrace;
{ initialize ray tracing variables }

procedure loadLens( VAR loaded : boolean; fileName : string);
{ loads lens system }

procedure saveLens(fileName:string);
{ saves lens system }

procedure trace(x,y,z,u,v:real);
{ traces ray through lens using current trace mode}

procedure DrawLens(Lenscolor : integer);
{ draws current lens system }

procedure drawExitRay(ExitRay:rays;distance:real;raycolor:integer);
{ draws ray from last surface to image plane }

procedure getSeidel(y:real;var total:Vector);
{ calculate seidel coefficients }

implementation

procedure getAngles(var u,v:real;ray:rays);
{ returns angle given direction cosines }
begin
     u := ArcCos(ray.n);
	v := ArcTan2(ray.l,ray.m);
end; {fn angles}


procedure initTrace;
{ initialize ray tracing variables }
var
	s:integer; {loop counter}
begin
     initialized := done;	{ indicate initialization }
	lens.x := 0;
	lens.y := 200;
	lens.z := 50;
	lens.scale := 50;
	traceMode := finite;
	NoAberrData := true;

     lens.c[0] := 0;
     lens.t[0] := 0;
     lens.n[0] := 1;

	{ set initial refractive indexes }
     for s := 1 to 50 do
     begin
          if odd(s) then
             lens.n[s] := 1.5
          else
              lens.n[s] := 1;
          lens.r[s] := 0;
     end; {for}
	last := 0;
end; {initTrace}

function rad(a:real):real;
{ converts degrees to radians }
begin
     rad := a*pi/180;
end; {fn rad}

function open(var fp:text; name:string; filetype:integer):boolean;
{ checks to see if file exists }
begin
     assign(fp,name);
     {$I-}
     if filetype = 0 then
          Reset(fp)
     else
          rewrite(fp);
     {$I+}
    open := (ioresult = 0);
  end { open };

function cosine(x:real):real;
{ cos x. gives proper value of cos(pi/2) }
begin
   if x = pi/2 then
      cosine := 0  { not -0 }
   else
      if x = 0 then
         cosine := 1
      else
         cosine := cos(x);
end; {cosine}


function sine(x:real):real;
{  gives proper value of sin(pi/2) }
begin
	if x = pi/2 then
		sine := 1  { not -1 }
	else
		if x = 0 then
			sine := 0
		else
			sine := sin(x);
end; {sine}


procedure SetCosines(u,v:real;var ray:rays);
{ converts angle u into direction cosines l,m,n }
begin
     ray.l := sine(u)*cosine(v);
     ray.m := sine(u)*sine(v);
     ray.n := cosine(u);
end; { getCosines }

function even(n:integer):boolean;
{ check if n is even }
begin
   if n mod 2 = 0 then
      even := true
   else
      even := false;
end; {fn even}

function Zcoord(r,y:real):real;
var
     a:real;
begin
     if abs(y) > abs(r) then
          Zcoord := r
     else
     begin
          a := abs(r) - sqrt(r*r - y*y);
          if r < 0 then
               Zcoord := -a
          else
               Zcoord := a;
     end; { if }
end; {func Zcoord}

function sPos(s:integer):real;
{ distance of surface s from first surface }
var
     i:integer; { loop counter}
     dz:real;   { working }
begin
     dz := 0;
     for i:= 1 to (s-1) do
         dz := dz + Lens.t[i];
     spos := + dz;
end; {fn sPos}


procedure curve(s:integer;x,y,z:real);
{ draw arc of curvature c at x,y,z }
const
   h =1; {Lens aperture radius}
   steps = 20;
var
   newy,newz,lasty,lastz : real; { y,z coords }
   i : integer;          { loop counter  }
   r : real;             { radius of curvature }
   start : integer;      { counter }

begin
  if Lens.r[s] <> 0 then { encountered a stop }
  begin
     { }
  end
  else
  begin
     if Lens.c[s] = 0 then
        PlotLine(z,y+h,z,y-h)
     else
     begin
        r := 1/Lens.c[s];
        lasty := h;
        lastz:= z + Zcoord(r,lasty);
        for i := (steps - 1) downto -steps do
        begin
           newy:= i/steps;
           newz:= z + Zcoord(r,newy);
           PlotLine(lastz,lasty,newz,newy);
           lastz:= newz;
           lasty:= newy;
           end; { for }
       end; { else }
   end; { else }
end; { proc curve }

function gap(c:real):real;
begin
	if last > 0 then
	     if c = 0 then
     	     gap := 0
	     else
     	     gap :=1/c - Zcoord(1/c,1);
end; { fn gap }


PROCEDURE DrawLens(Lenscolor : integer);
{ draws current lens system }

CONST
     h = 1;   { lens heights }
VAR
   i : integer;     { loop counter }
   x, y, z : real;    { position }
   yplus, yminus, zleft, zright : REAL;
   fillstyle : boolean; { pattern }
BEGIN
   IF (last > 0) and (initialized = done) THEN
   BEGIN
      x:= 0;
      y:= 0;
      z:= 0;
      fillstyle:= true;
      setColor(Lenscolor);
      FOR i:= 1 TO last DO
      BEGIN
         curve(i,x,y,z);
	 IF even(i) AND (Lens.r[i] = 0) THEN
	 BEGIN
            yplus:= y + h;
            yminus:= y - h;
            IF last > 0 THEN
            BEGIN
               IF lens.c[i - 1] = 0 THEN
                  zleft:= z - lens.t[i - 1]
               ELSE
                  zleft:= z - lens.t[i - 1] + Zcoord(1/lens.c[i-1],yplus);
               IF lens.c[i] = 0 THEN
                  zright:= z
               ELSE
                  zright:= z + Zcoord(1/lens.c[i],yplus);
            END;
            PlotLine(zleft,yplus,zright,yplus);
            PlotLine(zleft,yminus,zright,yminus);
	    IF fillstyle = true THEN
	    BEGIN
               SetFillStyle(XHatchFill,GetColor);
               FloodFill(MapX(z)-1,MapY(0.0),GetColor);
	    END
	    ELSE
	    BEGIN
               SetFillStyle(HatchFill,GetColor);
               FloodFill(MapX(z)-1,MapY(0.0),GetColor);
	    END;
               	fillstyle := NOT fillstyle;  { swap pattern }
         END; {if}
         z:= z + Lens.t[i];
      END;{for }
   END {if last}
   ELSE
      IF initialized <> done THEN
      writeln('Trace system not initialized. Use InitTrace.');
END; {proc DrawLens}

Procedure loadLens( VAR loaded : boolean; fileName : string);
{ loads lens system }
var
     i:integer; { loop counter }
     key:char; { working }
	flag:boolean;

  begin
     if open(Lensfile,fileName,0) then
     begin
          readln(Lensfile,last);
          for i:= 1 to last do
          begin
               readln(Lensfile,Lens.c[i]);
               readln(Lensfile,Lens.t[i]);
               readln(Lensfile,Lens.n[i]);
               readln(Lensfile,Lens.r[i]);
               readln(Lensfile,Lens.fl);
               readln(Lensfile,Lens.bfl);
          end;{ for }
          close(Lensfile);
		loaded := true;
	end; {if }
end; {fn loadLens}

procedure saveLens(fileName:string);
{ saves lens system }
var
     i:integer; { loop counter }
     key:char;  { dummy variable }
	flag:boolean;
begin
	saved := false;
     if open(Lensfile,filename,1) then
     begin
          writeln(Lensfile,last);
          for i:= 1 to last do
          begin
               writeln(Lensfile,Lens.c[i]);
               writeln(Lensfile,Lens.t[i]);
               writeln(Lensfile,Lens.n[i]);
               writeln(Lensfile,Lens.r[i]);
               writeln(Lensfile,Lens.fl);
               writeln(Lensfile,Lens.bfl);
          end;{ for }
          close(Lensfile);
		saved := true;
     end;{ else}
end; {proc saveLens}


procedure refractF(var ray:rays; surface:integer; var failed:boolean);
{ traces ray through surface. Transforms var ray }
var
     x0,y0:real;         { working variables }
     cos1,cos2:real;     { working }
     d:real;             { distance }
     c:real;             { surface curvature }
     n1,n2:real;         { indexes before & after surface }
     k,del:real;         { working variable }
     radius:real;        { radius of aperture }
     f,g:real;
     sgcf:real;          { sqrt(g*g - c*f) }


function delta:real;
{ works out distance between plane and actual surface }
begin
          f := c*(x0*x0+y0*y0);
          g := ray.n - c*(ray.l*x0 + ray.m*y0);
          if (g*g -c*f)<0  then
          begin
               failed := true;
               delta := 0;
          end
          else
          begin
               sgcf := sqrt(g*g - c*f);
               delta := f/(g + sgcf);
               failed := false;
          end;
end; {func delta}

begin
     c := Lens.c[surface];          { curvature of surface }
     n1 := Lens.n[surface-1];  { index of medium before surface }
     n2 := Lens.n[surface];    {  "        "      after surface }

     d := Lens.t[surface-1] - (ray.z-spos(surface-1));
     ray.z := ray.z - spos(surface);         { make surface origin}
     x0 := ray.x + ray.l/ray.n*d;
     y0 := ray.y + ray.m/ray.n*d;

     { handle stops }
     if Lens.r[surface] = 0 then
            radius := 1
     else
            radius := Lens.r[surface];

     del := delta;
     ray.x := x0 + ray.l*del;
     ray.y := y0 + ray.m*del;
     ray.z := ray.n*del;
     if (sqrt(ray.x*ray.x+ray.y*ray.y) >= radius)  then
           failed := true;

     if not failed then
     begin
           cos1 := sgcf; { ray.n - c*del - c*(ray.l*x0 + ray.m*y0);}
           cos2 := sqrt(abs(n2*n2 - n1*n1 + n1*n1*cos1*cos1))/n2;
           k := c*(n2*cos2 - n1*cos1);
           ray.l := (n1*ray.l - k*ray.x)/n2;
           ray.m := (n1*ray.m - k*ray.y)/n2;
           ray.n := (n1*ray.n - k*ray.z + n2*cos2 - n1*cos1)/n2;
     end; {if not failed};
     ray.z :=  sPos(surface) + ray.z;
end; {proc finite}

procedure RefractP(var ray:rays;s:integer; var failed:boolean);
{ paraxial ray trace }
var
   c,n,n2,u,k:real; {working}
begin
     u := arctan(ray.m/ray.n);
     c := Lens.c[s];
     n := Lens.n[s-1];
     n2 := Lens.n[s];
     k := (n2-n)*c;
     ray.y := ray.y + Lens.t[s-1]*u;
     u :=(n*u - ray.y*k)/n2;
     ray.m := sin(u);
     ray.n := cos(u);
     ray.z := spos(s);
	if ray.y >1 then
		failed := true;
end; { proc paraxial }

procedure drawExitRay(ExitRay:rays;distance:real;raycolor:integer);
{ draws ray from last surface to image plane }
var
   delta:real;
   xx,yy:real;
begin
	setcolor(raycolor);
     delta := ExitRay.z - spos(last);
	with ExitRay do
	begin
		yy := y + (distance-delta)*m/n;
		xx := x + (distance-delta)*l/n;
         { xLine(x,y,z, xx,yy,distance + spos(last));  }
         PlotLine(z,y, distance + spos(last),yy);
	end; {with}
end; {drawExitRay}


procedure trace(x,y,z,u,v:real);
{ traces ray through lens using current trace mode}
var
   surface:integer; { loop counter }
   ray:rays;        { ray position & direction cosines }
   failed:boolean;

begin
     ray.x := x;
     ray.y := y;
     ray.z := z;
     SetCosines(u,v,ray); { direction cosines }
     Lens.t[0] := -z;
     surface := 1;
     failed := false;
     rayPath[0] := ray;
     while (surface <= last) and not failed do
     begin
          case traceMode of
               finite:   RefractF(ray,surface,failed);
               Paraxial: RefractP(ray,surface,failed);
               else
          end; {case}
          rayPath[surface] := ray;
         surface := surface + 1;
     end; { while }
     if failed then
          rayPath[0].failed := true
     else
         rayPath[0].failed := false;
end; {proc trace}

procedure focus(var fl,bfl,gip:real);
{ find focal lengths of current system }
var
   y1,z1,u,v:real; { ray variables }
begin
     if last = 0 then
     begin
        fl := 0;
        bfl := 0;
        gip := 0;
     end
     else
     begin
          y1 := 0.1;
          z1 := - 1;
          u := 0;
          v := 0;
          trace(0,y1,z1,u,pi/2);
          getAngles(u,v,RayPath[last]);
          fl := y1/u;
          bfl := RayPath[last].y/u;

          u := y1/z1/2;
          trace(0,0,0,u,pi/2);
          getAngles(u,v,RayPath[last]);
          gip := -RayPath[last].y/u;
     end; {if}
end; {focus}

procedure paraxial2(s:integer;var y,u:real);
{ paraxial ray trace }
var
   c,n,n2,k:real; {working}
begin
     c := lens.c[s];
     n := lens.n[s-1];
     n2 := lens.n[s];
     k := (n2-n)*c;
     y := y + lens.t[s-1]*u;
     u :=(n*u - y*k)/n2;
end; { proc paraxial2 }

procedure getSeidel(y:real;var total:Vector);
{ calculate seidel coefficients }
var
   z,u,u1,n1,n2:real; { ray variables }
   A,Ap,delta,nnp:real;        { working }
   up,yp:real; { principal ray angle & height }
   H:real;     { larange invariant }
   s,i:integer;  { loop counters }
   seidel:array[1..5] of real; { seidel coefficients }
begin
     for i:= 1 to 6 do
         total[i] := 0;
     u := 0;
     up := rad(-3.25); { principal ray }
     yp := y;
     for s := 1 to last do
     begin
          u1 := u;
          n1 := lens.n[s-1];
          n2 := lens.n[s];

          paraxial2(s,y,u);
          paraxial2(s,yp,up);
          { refraction invariants }
          A := n1*u1 + n1*y*lens.c[s];
          Ap := n1*u1 + n1*yp*lens.c[s];
          { laragnge invariant }
          H := A*yp - Ap*y;
          nnp := n1*n2;
          delta := (n1*u-n2*u1)/nnp;

          seidel[1] := -(1/8)*y*sqr(A)*delta; { spherical aberration }
          seidel[2] := -0.5*A*Ap*y*delta;  { coma }
          seidel[3] := -0.5*sqr(Ap)*y*delta;  { astigmatism }
          seidel[4] := -(1/2)*seidel[3] - (1/4)*sqr(H)*lens.c[s]*(1/n1-1/n2);
          seidel[5] :=  2*seidel[4]*Ap/(A+1);
          for i := 1 to 5 do
              total[i] := total[i] + seidel[i];
		{ focus2(lens.fl,lens.bfl,lens.gip); }
		total[6] := lens.fl; { focal length taken as last aberration }
     end; { for }
end; {proc getSeidel}



END.