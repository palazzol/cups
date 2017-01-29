           (*************************************************)
           (*************************************************)
           (**                                             **)
           (**  Consortium of Upper-level Physics Software **)
           (**              ( CUPS Project )               **)
           (**      ( c) 1994 by John Wiley & Sons         **)
           (**  Compiled with Utilities Ver. 2.0 (95/10/10)**)
           (**                                             **)
           (*************************************************)
           (*************************************************)


          {****************************************************}
          {****************************************************}
          {**                Unit Crystals.Pas               **}
          {**  Written by Jarek Tuszynski and Graham Keeler  **}
          {**                                                **}
          {**              Last change: 5/3/95               **}
          {****************************************************}
          {****************************************************}

UNIT crystals;

INTERFACE

USES dos, crt, graph, cups, cupsmupp, cupsgrph, cupsgui;

{*********************** CRYSTAL OBJECT ***********************************}

CONST
   maxV = 50;

TYPE
   link = ^node;
   node = record
      atom : integer;
      next : link;
   end;
   atomType = record
      pos : point3D;
      kind : integer;
   end;

TYPE
   TCrystal = object (TGraph3D)
      Box        : boolean;
      alpha      : real;
      beta       : real;
      gamma      : real;
      size       : real;
      distance   : real;
{   alpha      : real    = 30;
   beta       : real    = 20;
   gamma      : real    = 0;
   size       : real    = 1;
   distance   : real    = 2.5;
}
      procedure init;
      procedure vertex(num,kind : integer; x,y,z : real);
      procedure edge(i,j : integer);
      procedure setElement(kind,color,radius : integer);
      procedure draw;
      procedure done;
   private
      numV  : integer;
      elements : array[1..10] of record color,radius : integer end;
      atoms : array[1..maxV] of atomType;
      adj   : array[1..maxV] of link;
      terminator : link;
   end;


{********************* END CRYSTAL OBJECT *********************************}


procedure Simple_Cubic_Structure(var crystal : TCrystal);

procedure FCC_Structure(var crystal : TCrystal);

procedure BCC_Structure(var crystal : TCrystal);

procedure NaCl_Structure(var crystal : TCrystal);

procedure CsCl_Structure(var crystal : TCrystal);

procedure Diamond_Structure(var crystal : TCrystal);

procedure FCCBZone(var crystal : TCrystal);

procedure BCCBZone(var crystal : TCrystal);

procedure Cu3Au_Structure(var crystal : TCrystal);

procedure ZnS_Structure(var crystal : TCrystal);

procedure CaF2_Structure(var crystal : TCrystal);

procedure CdI2_Structure(var crystal : TCrystal);

procedure NiAs_Structure(var crystal : TCrystal);

procedure tetrahedral(var crystal : TCrystal);

procedure hexagonal(var crystal : TCrystal);

procedure HCP_Structure(var crystal : TCrystal);

{-----------------------------------------------------------------}


IMPLEMENTATION

{--------------------------------------------------------------------------}

procedure TCrystal.init;
var
   z : link;
   n : integer;
begin
   TGraph3D.init;
   numV:=0;
   new(terminator);
   terminator^.next:=terminator;
   for n := 1 to maxV do adj[n] := terminator;
end;

{--------------------------------------------------------------------------}

procedure TCrystal.vertex(num,kind : integer; x,y,z : real);
begin
   inc(numV);
   atoms[num].kind := kind;
   atoms[num].pos[1] := x;
   atoms[num].pos[2] := y;
   atoms[num].pos[3] := z;
end;

{--------------------------------------------------------------------------}

procedure TCrystal.edge(i,j : integer);
var
   t : link;
begin
   new(t);  t^.atom:=i;  t^.next:=adj[j];  adj[j]:=t;
   new(t);  t^.atom:=j;  t^.next:=adj[i];  adj[i]:=t;
end;

{--------------------------------------------------------------------------}

procedure TCrystal.draw;
type depthType = record n:integer; d:real end;
var
   n,m,r : integer;
   a,b : pointType;
   t : link;
   depth : array[1..maxV] of depthType;
   order : array[1..maxV] of real;
   postn : array[1..maxV] of pointType;

   procedure QuickSort(l,r : integer);
   var i,j : integer;
       v,t : depthType;
   begin
      if r>l then
      begin
         v:=depth[r]; i:=l-1; j:=r;
         repeat
            repeat inc(i) until (depth[i].d<=v.d) or (i>=maxV);
            repeat dec(j) until (depth[j].d>=v.d) or (j<=1);
            t:=depth[i]; depth[i]:=depth[j]; depth[j]:=t;
         until j<=i;
         depth[j]:=depth[i]; depth[i]:=depth[r]; depth[r]:=t;
         QuickSort(l,i-1);
         QuickSort(i+1,r);
      end;
   end;

begin
   If Box and BoxDrawn then DrawBox;
   for n := 1 to numV do
    with atoms[n] do
    begin
        GetPoint(pos[1],pos[2],pos[3],postn[n]);
        order[n]:=RMat[3,1]*pos[1] +RMat[3,2]*pos[2] +RMat[3,3]*pos[3];
        depth[n].d:=order[n];
        depth[n].n:=n;
    end;
   QuickSort(1,numV);
   HideCursor;
   SetColor(whitte);
   for n := 1 to numV do
   begin
      m:=depth[n].n;
      a:=postn[m];
      t:=adj[m];
      while t<>terminator do
      begin
         if order[m]>=order[t^.atom] then
          line(a.x, a.y, postn[t^.atom].x, postn[t^.atom].y);
         t:=t^.next;
      end;
      if atoms[m].kind>0 then begin
         r:=elements[atoms[m].kind].radius;
         SetFillStyle(solidFill,elements[atoms[m].kind].color);
         FillEllipse( a.x, a.y,r,r);
      end;
   end;
   ShowCursor;
end;

{--------------------------------------------------------------------------}

procedure TCrystal.SetElement (kind,color,radius : integer);
begin
   elements[kind].color:=color;
   elements[kind].radius:=radius;
end;

procedure TCrystal.done;
var n : integer;
    t,u : link;
begin
   for n := 1 to numV do
   begin
      t:=adj[n];
      while t<>terminator do
      begin
         u:=t;
         t:=t^.next;
         dispose(u);
      end;
   end;
   numV:=0;
   for n := 1 to maxV do adj[n] := terminator;
   dispose(terminator);
end;

{********************* END CRYSTAL OBJECT *********************************}


procedure Simple_Cubic_Structure(var crystal : TCrystal);
begin
   with crystal do
   begin
      Box := false;
      Set3DScales(-1,1, -1,1,-1,1);
      SetElement(1, blue, 9);
      vertex(01,1, 1, 1, 1);
      vertex(02,1, 1, 1,-1);
      vertex(03,1, 1,-1, 1);
      vertex(04,1, 1,-1,-1);
      vertex(05,1,-1, 1, 1);
      vertex(06,1,-1, 1,-1);
      vertex(07,1,-1,-1, 1);
      vertex(08,1,-1,-1,-1);

      edge(1,2);
      edge(3,4);
      edge(5,6);
      edge(7,8);
      edge(1,3);
      edge(2,4);
      edge(5,7);
      edge(6,8);
      edge(1,5);
      edge(2,6);
      edge(3,7);
      edge(4,8);
   end;
end;

procedure FCC_Structure(var crystal : TCrystal);
begin
   with crystal do
   begin
      Box := true;
      Set3DScales(-1,1, -1,1,-1,1);
      SetElement(1, blue, 10);
      vertex(01,1, 1, 1, 1);
      vertex(02,1, 1, 1,-1);
      vertex(03,1, 1,-1, 1);
      vertex(04,1, 1,-1,-1);
      vertex(05,1,-1, 1, 1);
      vertex(06,1,-1, 1,-1);
      vertex(07,1,-1,-1, 1);
      vertex(08,1,-1,-1,-1);
      vertex(09,1, 1, 0, 0);
      vertex(10,1, 0, 1, 0);
      vertex(11,1, 0, 0, 1);
      vertex(12,1,-1, 0, 0);
      vertex(13,1, 0,-1, 0);
      vertex(14,1, 0, 0,-1);

      edge(1,9);
      edge(2,9);
      edge(3,9);
      edge(4,9);
      edge(1,10);
      edge(2,10);
      edge(5,10);
      edge(6,10);
      edge(1,11);
      edge(3,11);
      edge(5,11);
      edge(7,11);
      edge(5,12);
      edge(6,12);
      edge(7,12);
      edge(8,12);
      edge(3,13);
      edge(4,13);
      edge(7,13);
      edge(8,13);
      edge(2,14);
      edge(4,14);
      edge(6,14);
      edge(8,14);
      edge(10,9);
      edge(11,9);
      edge(13,9);
      edge(14,9);
      edge(11,10);
      edge(12,10);
      edge(14,10);
      edge(12,11);
      edge(13,11);
      edge(13,12);
      edge(14,12);
      edge(14,13);
   end;
end;

procedure BCC_Structure(var crystal : TCrystal);
begin
   with crystal do
   begin
      Box := false;
      Set3DScales(-1,1, -1,1,-1,1);
      SetElement(1, blue, 10);  
      vertex(01,1, 1, 1, 1);
      vertex(02,1, 1, 1,-1);
      vertex(03,1, 1,-1, 1);
      vertex(04,1, 1,-1,-1);
      vertex(05,1,-1, 1, 1);
      vertex(06,1,-1, 1,-1);
      vertex(07,1,-1,-1, 1);
      vertex(08,1,-1,-1,-1);
      vertex(09,1, 0, 0, 0);

      edge(1,2);
      edge(3,4);
      edge(5,6);
      edge(7,8);
      edge(1,3);
      edge(2,4);
      edge(5,7);
      edge(6,8);
      edge(1,5);
      edge(2,6);
      edge(3,7);
      edge(4,8);
      edge(1,9);
      edge(2,9);
      edge(3,9);
      edge(4,9);
      edge(5,9);
      edge(6,9);
      edge(7,9);
      edge(8,9);
   end;
end;


procedure NaCl_Structure(var crystal : TCrystal);
begin
   with crystal do
   begin
      Box := false;
      Set3DScales(-1,1, -1,1,-1,1);
      SetElement(1, blue, 9);  {Na}
      SetElement(2,  red, 13);  {Cl}
      vertex(01,1, 1, 1, 1);
      vertex(02,1, 1, 1,-1);
      vertex(03,1, 1,-1, 1);
      vertex(04,1, 1,-1,-1);
      vertex(05,1,-1, 1, 1);
      vertex(06,1,-1, 1,-1);
      vertex(07,1,-1,-1, 1);
      vertex(08,1,-1,-1,-1);
      vertex(09,1, 0, 0, 1);
      vertex(10,1, 0, 0,-1);
      vertex(11,1, 0, 1, 0);
      vertex(12,1, 0,-1, 0);
      vertex(13,1, 1, 0, 0);
      vertex(14,1,-1, 0, 0);
      vertex(15,2, 0, 0, 0);
      vertex(16,2, 0, 1, 1);
      vertex(17,2, 0, 1,-1);
      vertex(18,2, 0,-1, 1);
      vertex(19,2, 0,-1,-1);
      vertex(20,2, 1, 0, 1);
      vertex(21,2, 1, 0,-1);
      vertex(22,2,-1, 0, 1);
      vertex(23,2,-1, 0,-1);
      vertex(24,2, 1, 1, 0);
      vertex(25,2, 1,-1, 0);
      vertex(26,2,-1, 1, 0);
      vertex(27,2,-1,-1, 0);

      edge(1,24);     edge(24,2);
      edge(3,25);     edge(25,4);
      edge(5,26);     edge(26,6);
      edge(7,27);     edge(27,8);
      edge(1,20);     edge(20,3);
      edge(2,21);     edge(21,4);
      edge(5,22);     edge(22,7);
      edge(6,23);     edge(23,8);
      edge(1,16);     edge(16,5);
      edge(2,17);     edge(17,6);
      edge(3,18);     edge(18,7);
      edge(4,19);     edge(19,8);
      edge(9,15);     edge(15,10);
      edge(11,15);    edge(15,12);
      edge(13,15);    edge(15,14);

      edge(16,11);    edge(11,17);
      edge(18,12);    edge(12,19);
      edge(20,13);    edge(13,21);
      edge(22,14);    edge(14,23);
      edge(24,13);    edge(13,25);
      edge(26,14);    edge(14,27);
      edge(16,9);     edge(9,18);
      edge(17,10);    edge(10,19);
      edge(20,9);     edge(9,22);
      edge(21,10);    edge(10,23);
      edge(24,11);    edge(11,26);
      edge(25,12);    edge(12,27);
   end;
end;

procedure CsCl_Structure(var crystal : TCrystal);
begin
   with crystal do
   begin
      Box := false;
      Set3DScales(-1,1, -1,1,-1,1);
      SetElement(1, blue, 9);  {Cs}
      SetElement(2,  red, 13);  {Cl}
      vertex(01,1, 1, 1, 1);
      vertex(02,1, 1, 1,-1);
      vertex(03,1, 1,-1, 1);
      vertex(04,1, 1,-1,-1);
      vertex(05,1,-1, 1, 1);
      vertex(06,1,-1, 1,-1);
      vertex(07,1,-1,-1, 1);
      vertex(08,1,-1,-1,-1);
      vertex(09,2, 0, 0, 0);

      edge(1,2);
      edge(3,4);
      edge(5,6);
      edge(7,8);
      edge(1,3);
      edge(2,4);
      edge(5,7);
      edge(6,8);
      edge(1,5);
      edge(2,6);
      edge(3,7);
      edge(4,8);
      edge(1,9);
      edge(2,9);
      edge(3,9);
      edge(4,9);
      edge(5,9);
      edge(6,9);
      edge(7,9);
      edge(8,9);
   end;
end;


procedure Diamond_Structure(var crystal : TCrystal);
begin
   with crystal do
   begin
      Box := true;
      Set3DScales(-2,2, -2,2,-2,2);
      SetElement(1, blue, 10);  {C}
      vertex(01,1, 2, 2,-2);
      vertex(02,1, 2,-2, 2);
      vertex(03,1,-2, 2, 2);
      vertex(04,1,-2,-2,-2);
      vertex(05,1, 0, 0, 2);
      vertex(06,1, 0, 0,-2);
      vertex(07,1, 0, 2, 0);
      vertex(08,1, 0,-2, 0);
      vertex(09,1, 2, 0, 0);
      vertex(10,1,-2, 0, 0);
      vertex(11,1, 1,-1, 1);
      vertex(12,1,-1, 1, 1);
      vertex(13,1, 1, 1,-1);
      vertex(14,1,-1,-1,-1);

      edge(2,11);     edge(11,9);
      edge(5,11);     edge(11,8);
      edge(5,12);     edge(12,7);
      edge(3,12);     edge(12,10);
      edge(7,13);     edge(13,6);
      edge(1,13);     edge(13,9);
      edge(8,14);     edge(14,6);
      edge(10,14);    edge(14,4);
   end;
end;


procedure FCCBZone(var crystal : TCrystal);
begin
   with crystal do
   begin
      Box := true;
      Set3DScales(-1,1, -1,1,-1,1);
      vertex(01,0, 1,0.5, 0);
      vertex(02,0, 1, 0, 0.5);
      vertex(03,0, 1,-0.5, 0);
      vertex(04,0, 1, 0,-0.5);
      vertex(05,0,-1, 0.5, 0);
      vertex(06,0,-1, 0, 0.5);
      vertex(07,0,-1,-0.5, 0);
      vertex(08,0,-1, 0,-0.5);
      vertex(09,0, 0.5, 1, 0);
      vertex(10,0, 0, 1, 0.5);
      vertex(11,0,-0.5, 1, 0);
      vertex(12,0, 0, 1,-0.5);
      vertex(13,0, 0.5,-1, 0);
      vertex(14,0, 0,-1, 0.5);
      vertex(15,0,-0.5,-1, 0);
      vertex(16,0, 0,-1,-0.5);
      vertex(17,0, 0.5, 0, 1);
      vertex(18,0, 0, 0.5, 1);
      vertex(19,0,-0.5, 0, 1);
      vertex(20,0, 0,-0.5, 1);
      vertex(21,0, 0.5, 0,-1);
      vertex(22,0, 0, 0.5,-1);
      vertex(23,0,-0.5, 0,-1);
      vertex(24,0, 0,-0.5,-1);

      edge(1,2); edge(2,3);
      edge(3,4); edge(4,1);
      edge(5,6); edge(6,7);
      edge(7,8); edge(8,5);
      edge(9,10); edge(10,11);
      edge(11,12); edge(12,9);
      edge(13,14); edge(14,15);
      edge(15,16); edge(16,13);
      edge(17,18); edge(18,19);
      edge(19,20); edge(20,17);
      edge(21,22); edge(22,23);
      edge(23,24); edge(24,21);
      edge(1,9);
      edge(11,5);
      edge(7,15);
      edge(13,3);
      edge(2,17);
      edge(19,6);
      edge(8,23);
      edge(21,4);
      edge(10,18);
      edge(20,14);
      edge(16,24);
      edge(22,12);
   end;
end;


procedure BCCBZone(var crystal : TCrystal);
begin
   with crystal do
   begin
      Box := true;
      Set3DScales(-1,1, -1,1,-1,1);
      vertex(01,0, 1, 0, 0);
      vertex(02,0, 0, 1, 0);
      vertex(03,0, 0, 0, 1);
      vertex(04,0,-1, 0, 0);
      vertex(05,0, 0,-1, 0);
      vertex(06,0, 0, 0,-1);
      vertex(07,0,0.5,0.5,0.5);
      vertex(08,0,-0.5,0.5,0.5);
      vertex(09,0,0.5,-0.5,0.5);
      vertex(10,0,0.5,0.5,-0.5);
      vertex(11,0,0.5,-0.5,-0.5);
      vertex(12,0,-0.5,0.5,-0.5);
      vertex(13,0,-0.5,-0.5,0.5);
      vertex(14,0,-0.5,-0.5,-0.5);

      edge(1,7);
      edge(2,7);
      edge(3,7);
      edge(4,8);
      edge(2,8);
      edge(3,8);
      edge(1,9);
      edge(5,9);
      edge(3,9);
      edge(1,10);
      edge(2,10);
      edge(6,10);
      edge(1,11);
      edge(5,11);
      edge(6,11);
      edge(4,12);
      edge(2,12);
      edge(6,12);
      edge(4,13);
      edge(5,13);
      edge(3,13);
      edge(4,14);
      edge(5,14);
      edge(6,14);
   end;
end;

procedure Cu3Au_Structure(var crystal : TCrystal);
begin
   with crystal do
   begin
      Box := false;
      Set3DScales(-1,1, -1,1,-1,1);
      SetElement(1, blue, 9);
      SetElement(2,  red,13);
      SetElement(3,blakk, 0);
      vertex(01,1, 1, 1, 1);
      vertex(02,1, 1, 1,-1);
      vertex(03,1, 1,-1, 1);
      vertex(04,1, 1,-1,-1);
      vertex(05,1,-1, 1, 1);
      vertex(06,1,-1, 1,-1);
      vertex(07,1,-1,-1, 1);
      vertex(08,1,-1,-1,-1);
      vertex(09,2, 0, 0, 1);
      vertex(10,2, 0, 0,-1);
      vertex(11,2, 0, 1, 0);
      vertex(12,2, 0,-1, 0);
      vertex(13,2, 1, 0, 0);
      vertex(14,2,-1, 0, 0);
      vertex(16,3, 0, 1, 1);
      vertex(17,3, 0, 1,-1);
      vertex(18,3, 0,-1, 1);
      vertex(19,3, 0,-1,-1);
      vertex(20,3, 1, 0, 1);
      vertex(21,3, 1, 0,-1);
      vertex(22,3,-1, 0, 1);
      vertex(23,3,-1, 0,-1);
      vertex(24,3, 1, 1, 0);
      vertex(25,3, 1,-1, 0);
      vertex(26,3,-1, 1, 0);
      vertex(15,3,-1,-1, 0);

      edge(1,24);     edge(24,2);
      edge(3,25);     edge(25,4);
      edge(5,26);     edge(26,6);
      edge(7,15);     edge(15,8);
      edge(1,20);     edge(20,3);
      edge(2,21);     edge(21,4);
      edge(5,22);     edge(22,7);
      edge(6,23);     edge(23,8);
      edge(1,16);     edge(16,5);
      edge(2,17);     edge(17,6);
      edge(3,18);     edge(18,7);
      edge(4,19);     edge(19,8);

      edge(16,11);    edge(11,17);
      edge(18,12);    edge(12,19);
      edge(20,13);    edge(13,21);
      edge(22,14);    edge(14,23);
      edge(24,13);    edge(13,25);
      edge(26,14);    edge(14,15);
      edge(16,9);     edge(9,18);
      edge(17,10);    edge(10,19);
      edge(20,9);     edge(9,22);
      edge(21,10);    edge(10,23);
      edge(24,11);    edge(11,26);
      edge(25,12);    edge(12,15);
   end;
end;

procedure ZnS_Structure(var crystal : TCrystal);
begin
   with crystal do
   begin
      Box := false;
      Set3DScales(-2,2, -2,2,-2,2);
      SetElement(1, blue, 10); {S}
      SetElement(2,  red, 4);  {Zn}
      vertex(01,1, 2, 2, 2);
      vertex(02,1, 2, 2,-2);
      vertex(03,1, 2,-2, 2);
      vertex(04,1, 2,-2,-2);
      vertex(05,1,-2, 2, 2);
      vertex(06,1,-2, 2,-2);
      vertex(07,1,-2,-2, 2);
      vertex(08,1,-2,-2,-2);
      vertex(09,1, 0, 0, 2);
      vertex(10,1, 0, 0,-2);
      vertex(11,1, 0, 2, 0);
      vertex(12,1, 0,-2, 0);
      vertex(13,1, 2, 0, 0);
      vertex(14,1,-2, 0, 0);
      vertex(15,2, 1,-1, 1);
      vertex(16,2,-1, 1, 1);
      vertex(17,2, 1, 1,-1);
      vertex(18,2,-1,-1,-1);

      edge(1,2);      edge(3,4);
      edge(5,6);      edge(7,8);
      edge(1,3);      edge(2,4);
      edge(5,7);      edge(6,8);
      edge(1,5);      edge(2,6);
      edge(3,7);      edge(4,8);

      edge(3,15);     edge(15,9);
      edge(12,15);    edge(15,13);
      edge(5,16);     edge(16,9);
      edge(11,16);    edge(16,14);
      edge(2,17);     edge(17,10);
      edge(11,17);    edge(17,13);
      edge(8,18);     edge(18,10);
      edge(12,18);    edge(18,14);
   end;
end;

procedure CaF2_Structure(var crystal : TCrystal);
begin
   with crystal do
   begin
      Box := false;
      Set3DScales(-2,2, -2,2,-2,2);
      SetElement(1, blue, 6);   {S}
      SetElement(2,  red, 10);  {Zn}
      vertex(01,1, 2, 2, 2);
      vertex(02,1, 2, 2,-2);
      vertex(03,1, 2,-2, 2);
      vertex(04,1, 2,-2,-2);
      vertex(05,1,-2, 2, 2);
      vertex(06,1,-2, 2,-2);
      vertex(07,1,-2,-2, 2);
      vertex(08,1,-2,-2,-2);
      vertex(09,1, 0, 0, 2);
      vertex(10,1, 0, 0,-2);
      vertex(11,1, 0, 2, 0);
      vertex(12,1, 0,-2, 0);
      vertex(13,1, 2, 0, 0);
      vertex(14,1,-2, 0, 0);
      vertex(15,2, 1, 1, 1);
      vertex(16,2, 1, 1,-1);
      vertex(17,2, 1,-1, 1);
      vertex(18,2, 1,-1,-1);
      vertex(19,2,-1, 1, 1);
      vertex(20,2,-1, 1,-1);
      vertex(21,2,-1,-1, 1);
      vertex(22,2,-1,-1,-1);

      edge(1,2);      edge(3,4);
      edge(5,6);      edge(7,8);
      edge(1,3);      edge(2,4);
      edge(5,7);      edge(6,8);
      edge(1,5);      edge(2,6);
      edge(3,7);      edge(4,8);

      edge(3,17);     edge(17,9);
      edge(12,17);    edge(17,13);
      edge(5,19);     edge(19,9);
      edge(11,19);    edge(19,14);
      edge(2,16);     edge(16,10);
      edge(11,16);    edge(16,13);
      edge(8,22);     edge(22,10);
      edge(12,22);    edge(22,14);

      edge(4,18);     edge(18,10);
      edge(12,18);    edge(18,13);
      edge(1,15);     edge(15,9);
      edge(11,15);    edge(15,13);
      edge(7,21);     edge(21,9);
      edge(12,21);    edge(21,14);
      edge(6,20);     edge(20,11);
      edge(10,20);    edge(20,14);

      edge(15,16);    edge(17,18);
      edge(19,20);    edge(21,22);
      edge(15,17);    edge(16,18);
      edge(19,21);    edge(20,22);
      edge(15,19);    edge(16,20);
      edge(17,21);    edge(18,22);
   end;
end;

procedure CdI2_Structure(var crystal : TCrystal);
begin
   with crystal do
   begin
      Box := true;
      Set3DScales(-2,2, -2,2,-2,2);
      SetElement(1, blue, 10);
      SetElement(2,  red, 6);
      vertex(01,1, 2, 0, 2);
      vertex(02,1, 2, 0,-2);
      vertex(03,1,-2, 0, 2);
      vertex(04,1,-2, 0,-2);
      vertex(05,1, 0, 1, 2);
      vertex(06,1, 0, 1,-2);
      vertex(07,1, 0,-1, 2);
      vertex(08,1, 0,-1,-2);
      vertex(09,1, 2, 0, 0);
      vertex(10,1,-2, 0, 0);
      vertex(11,1, 0, 1, 0);
      vertex(12,1, 0,-1, 0);
      vertex(13,2, 2/3, 0, 1);
      vertex(14,2,-2/3, 0,-1);

      edge(1,9);     edge(9,2);
      edge(3,10);    edge(10,4);
      edge(5,11);    edge(11,6);
      edge(7,12);    edge(12,8);
      edge(1,5);     edge(5,3);
      edge(1,7);     edge(7,3);
      edge(2,6);     edge(6,4);
      edge(2,8);     edge(8,4);
   end;
end;

procedure NiAs_Structure(var crystal : TCrystal);
begin
   with crystal do
   begin
      Box := true;
      Set3DScales(-2,2, -2,2,-2,2);
      SetElement(1, blue, 10);
      SetElement(2,  red, 6);
      vertex(01,1, 2, 0, 2);
      vertex(02,1, 2, 0,-2);
      vertex(03,1,-2, 0, 2);
      vertex(04,1,-2, 0,-2);
      vertex(05,1, 0, 1, 2);
      vertex(06,1, 0, 1,-2);
      vertex(07,1, 0,-1, 2);
      vertex(08,1, 0,-1,-2);
      vertex(09,1, 2, 0, 0);
      vertex(10,1,-2, 0, 0);
      vertex(11,1, 0, 1, 0);
      vertex(12,1, 0,-1, 0);
      vertex(13,2, 2/3, 0, 1);
      vertex(14,2, 2/3, 0,-1);
      vertex(15,1,-2/3, 0, 0);

      edge(1,9);     edge(9,2);
      edge(3,10);    edge(10,4);
      edge(5,11);    edge(11,6);
      edge(7,12);    edge(12,8);
      edge(1,5);     edge(5,3);
      edge(1,7);     edge(7,3);
      edge(2,6);     edge(6,4);
      edge(2,8);     edge(8,4);
   end;
end;

procedure tetrahedral(var crystal : TCrystal);
var y,z : real;
begin
   z:=sqrt(2/3);
   y:=sqrt(3)/2;
   with crystal do
   begin
      Box := true;
      Set3DScales(-1,1, -1,1,-1,1);
      SetElement(1, blue, 10);  {C}
      vertex(1,1, 0,-y/3, z);
      vertex(2,1, 0, y,-z);
      vertex(3,1, 1,-y,-z);
      vertex(4,1,-1,-y,-z);
      edge(1,2);    edge(1,3);
      edge(1,4);    edge(2,3);
      edge(2,4);    edge(3,4);
   end;
end;

procedure hexagonal(var crystal : TCrystal);
var y : real;
begin
   y:=sqrt(3)/2;
   with crystal do
   begin
      Box := true;
      Set3DScales(-1,1, -1,1,-1,1);
      SetElement(1, blue, 10);  {C}
      vertex(1,1, 0,   0,-1);
      vertex(2,1, 1,   0,-1);
      vertex(3,1, 0.5, y,-1);
      vertex(4,1,-0.5, y,-1);
      vertex(5,1,-1,   0,-1);
      vertex(6,1,-0.5,-y,-1);
      vertex(7,1, 0.5,-y,-1);
      vertex(8,1, 0,   0, 1);
      vertex(9,1, 1,   0, 1);
      vertex(10,1, 0.5, y, 1);
      vertex(11,1,-0.5, y, 1);
      vertex(12,1,-1,   0, 1);
      vertex(13,1,-0.5,-y, 1);
      vertex(14,1, 0.5,-y, 1);
      edge(1,2);    edge(1,3);
      edge(1,4);    edge(1,5);
      edge(1,6);    edge(1,7);
      edge(2,3);    edge(3,4);
      edge(4,5);    edge(5,6);
      edge(6,7);    edge(7,2);
      edge(8,9);    edge(8,10);
      edge(8,11);   edge(8,12);
      edge(8,13);   edge(8,14);
      edge(9,10);   edge(10,11);
      edge(11,12);  edge(12,13);
      edge(13,14);  edge(14,9);
      edge(1,8);    edge(2,9);
      edge(3,10);   edge(4,11);
      edge(5,12);   edge(6,13);
      edge(7,14);
   end;
end;

procedure HCP_Structure(var crystal : TCrystal);
var x,y,z : real;
begin
   x:=sqrt(1/3)/2;
   y:=sqrt(3)/2;
   z:=sqrt(2/3);
   with crystal do
   begin
      Box := true;
      Set3DScales(-1,1, -1,1,-1,1);
      SetElement(1, blue, 10);  {C}
      vertex(1,1, 0,   0,-z);
      vertex(2,1, 1,   0,-z);
      vertex(3,1, 0.5, y,-z);
      vertex(4,1,-0.5, y,-z);
      vertex(5,1,-1,   0,-z);
      vertex(6,1,-0.5,-y,-z);
      vertex(7,1, 0.5,-y,-z);
      vertex(8,1, 0,   0, z);
      vertex(9,1, 1,   0, z);
      vertex(10,1, 0.5, y, z);
      vertex(11,1,-0.5, y, z);
      vertex(12,1,-1,   0, z);
      vertex(13,1,-0.5,-y, z);
      vertex(14,1, 0.5,-y, z);
      vertex(15,1, 0.5, x, 0);
      vertex(16,1,-0.5, x, 0);
      vertex(17,1, 0,-2*x, 0);
      edge(1,2);    edge(1,3);
      edge(1,4);    edge(1,5);
      edge(1,6);    edge(1,7);
      edge(2,3);    edge(3,4);
      edge(4,5);    edge(5,6);
      edge(6,7);    edge(7,2);
      edge(8,9);    edge(8,10);
      edge(8,11);   edge(8,12);
      edge(8,13);   edge(8,14);
      edge(9,10);   edge(10,11);
      edge(11,12);  edge(12,13);
      edge(13,14);  edge(14,9);
      edge(1,15);   edge(2,15);
      edge(3,15);   edge(1,16);
      edge(4,16);   edge(5,16);
      edge(1,17);   edge(6,17);
      edge(7,17);
      edge(15,16);  edge(16,17);
      edge(17,15);
      edge(8,15);   edge(9,15);
      edge(10,15);  edge(8,16);
      edge(11,16);  edge(12,16);
      edge(8,17);   edge(13,17);
      edge(14,17);
   end;
end;
end.
