                  (**********************************)
                  (**********************************)
                  (*    Spherical Harmonic Plot     *)
                  (*   Written by Jarek Tuszynski   *)
                  (*         Jan 28,1994.           *)
                  (*       Copyright @ 1994         *)
                  (**********************************)
                  (**********************************)
unit SphHar;

interface
uses crt,graph, cups, cupsgui, cupsgrph, cupsmupp, cupsproc, cupsfunc;
type
   TSphericalHar = object( TGraph3d )
      constructor Init( vp,l,m : integer);
      procedure   PlotFigure(VAR X_,Z_,P_ : DVector; name_: string);
      function    Changed(var l,m : integer) : boolean;        {this should be in main loop}
      function    ReadData(var l,m : integer) : boolean;
      DESTRUCTOR  done;
   private
      X,Z,P : DVector;
      active,Plot2D,NewData : boolean;
      size,beta : real;
      sliders  : TSliders;
      LButtons,MButtons : TButtons;
      name     : string;
      vpNumber : integer;
      button   : rectType;
      xmax,ymin,ymax,middle : real;
      procedure Draw2DPlot;
      procedure Draw3DPlot;
   end;


implementation

{----------------- Calculations of Spherical Harmonics ---------------------}

FUNCTION PtInside(x,y : integer; r:rectType) : boolean;
BEGIN
    PtInside := (x>r.x1) AND (x<r.x2) AND (y>r.y1) AND (y<r.y2);
END;


{----------------------------- TSphericalHar -------------------------------}


constructor TSphericalHar.Init(vp,l,m : integer);
{ Sets up the whole environment without drawing anything }
VAR
   xmin : real;
   d : integer;
BEGIN
   IF vp>10 THEN
      Error('TSphericalHar.Init: Please draw your graph in viewports 1 to 10.');
   active   := false;
   Plot2D   := false;
   NewData  := false;
   beta     := 15;
   vpNumber := VP;
   d        := round(0.025*GetMaxX);
   size     := 1.7;
   sliders.init;
   WITH views[vpNumber] DO
   BEGIN
      xmin := vx1/GetMaxX;
      xmax := vx2/GetMaxX;
      ymin := 1-vy2/GetMaxY;
      ymax := 1-(vy1+1)/GetMaxY;
      button.x1:=vx1+6;     button.y1:=vy1;
      button.x2:=vx1+5+d;   button.y2:=vy1+d;
   END;
   DefineViewPort(10+vpNumber, xmin+0.04, xmax, ymin, ymax);
   SelectViewPort(10+vpNumber);
   TGraph3D.init;
   SelectMuppetPort;
   SetDistance(size,2.5);
   SetBoxRatios(1,1,1);
   SetViewAngles(0,beta,0);
   arrows := true;
   middle := (ymax+ymin)/2;
   sliders.create(1,-90,90, beta, xmin+0.01, ymin, ymax-0.04,0,'','','',true );
   sliders.create(2, 0.8,3, size, xmax+0.02, ymin, ymax,1,'','','size',true );
   SetColor(white);
   message('Use button in upper-left corner to switch between 2D and 3D plots.');
   PrintXY(0.65,0.80,'Angular Momenta:');
   PrintXY(0.65,0.74,'Orbital(l)   Magnetic(m)');
   LButtons.Init;
   for d := 0 to 5 do Lbuttons.create(d, 0.69,0.70-d*0.05, numStr(d,1,0));
   LButtons.Number := l;
   LButtons.DrawAll;
   MButtons.Init;
   for d := 0 to l do Mbuttons.create(d, 0.87,0.70-d*0.05, numStr(d,1,0));
   MButtons.Number := m;
   MButtons.DrawAll;
END;


procedure TSphericalHar.PlotFigure( VAR X_,Z_,P_ : DVector; name_: string);
var axisLen : real;
BEGIN
   name := name_;
   x := x_;
   z := z_;
   p := p_;
   active := true;
   if Plot2D then Draw2DPlot else Draw3DPlot;
END;

function TSphericalHar.ReadData(var l,m : integer) : boolean;
var
   screen : TInputScreen;
   lTemp ,mTemp : integer;
   OK,escape : boolean;
begin
   with screen do
   begin
      init;
      DefineInputPort(0.19,0.81, 0.35,0.65);
      LoadLine('Choose angular momenta quantum numbers:');
      LoadLine('');
      LoadLine(concat('  orbital  l = {   }  0 ',chr(243),' l ',chr(243),' 5'));
      LoadLine(concat('  magnetic m = {   } -l ',chr(243),' m ',chr(243),' l'));
      LoadLine('');
      LoadLine('          [  Ok  ]   [Cancel]');
      SetNumber(1,l);
      SetNumber(2,m);
      SetNumberLimits(1,0,5);
      SetNumberLimits(2,-5,5);
      repeat
         AcceptScreen;
         lTemp  := round(GetNumber(1));
         mTemp  := round(GetNumber(2));
         OK := (abs(mTemp)<=lTemp);
         If not OK then
          announce('Absolute value of m have to be smaller or equal to l');
         escape := Canceled;
      until Ok or Escape;
      if OK then begin l:=lTemp; m:=mTemp end;
      done;
   end;
   NewData  := not Escape;
   ReadData := NewData;
end;


function TSphericalHar.changed(var l,m : integer) : boolean;
{ Control of all events. This procedure should be in event loop.}
VAR
   i,oldl : integer;
   lStr : string[2];
   change : boolean;
BEGIN
   IF active THEN
   BEGIN
      IF sliders.Changed and (NOT Plot2D) THEN
      BEGIN                              {angle or size sliders changed}
         OpenViewPort(10+vpNumber);
         SetWriteMode (XorPut);
         DrawBox;
         REPEAT
            CheckForEvents;
            DrawBox;
            beta := sliders.value(1);
            size := sliders.value(2);
            SetViewAngles(0,beta,0);
            SetDistance(size,2.5);
            DrawBox;
            Delay(20);
         UNTIL NOT sliders.changed;
         DrawBox;
         SetWriteMode (CopyPut);
         Draw3DPlot;
      END;
      IF event.mouseClicked THEN
      BEGIN
         IF Plot2D THEN i:=event.x+6 ELSE i:=event.x;
         IF PtInside(i,event.y,button) THEN
         BEGIN
            CloseViewPort(vpNumber);
            IF NOT Plot2D THEN
            BEGIN
               sliders.erase(1,black);          {erase 3D graph}
               Draw2DPlot;
            END ELSE
            begin
               SelectViewPort(vpNumber);        {erase 2D graph}
               DrawAxis(0,0, 0.5,0.5,black);
               SetColor(black);
               PutLabel(left,  'Z');
               PutLabel(bottom,'X');
               Draw3DPlot;
            end;
            Plot2D := NOT Plot2D;
         END;
      END;
      Change := MButtons.Changed;
      m := MButtons.number;
      If (NewData or LButtons.Changed) then with MButtons do
      begin
         if NewData then oldl := LButtons.number
         else begin oldl := l; l := LButtons.number; end;
         LButtons.number := l;
         change := not NewData;
         for i := l+1 to oldl do begin erase( i,black); delete( i); end;
         for i := oldl+1 to l do
          create( i, 0.87,0.70-i*0.05, concat(chr(241),numStr( i,1,0)) );
         number := m;
         DrawAll;
         If abs(m)>l then
         begin
            m := round(sgn(m)*l);
            MButtons.number := m;
            MButtons.DrawAll;
         end;
         NewData := false;
      end;
   END;
   if (Change and Plot2D) then
   begin
      SelectViewPort(vpNumber);        {erase 2D graph}
      DrawAxis(0,0, 0.5,0.5,black);
   end;
   changed := change;
END;


DESTRUCTOR TSphericalHar.done;
{ Releases all memory and clears screen.}
var i : integer;
BEGIN
   Sliders.done;
   ClearMuppetPort;
   active := false;
END;

PROCEDURE TSphericalHar.Draw2DPlot;
{ Draws 2D plot of the Spherical Harmonics (this procedure was moved from }
{ the main program).}
VAR
   envir : TEnvironment;
   i : integer;
BEGIN
   envir.Save;
   envir.Standardize;
   OpenViewPort(vpNumber);
   Axis(0,0, 0.5,0.5);
   PutLabel(left,  'Z');
   PutLabel(bottom,'X');
   FOR i:=2 TO x.size DO
   BEGIN
      IF P.value(i)>=0 THEN SetColor(lightblue) ELSE SetColor(yellow);
      PlotLine(X.value(i-1),Z.value(i-1), X.value(i),Z.value(i));
   END;
   SelectMuppetPort;
   HideMouse;
   SetColor(white);
   SetFillStyle(SolidFill,DarkGray);
   WITH button DO bar(x1-6,y1,x2-6,y2);
   WITH button DO rectangle(x1-6,y1,x2-6,y2);
   SetFillStyle(SolidFill,black);
   WITH button DO bar(x1,y1+6,x2-12,y2-6);
   ShowMouse;
   SelectViewPort(vpNumber);
   Print(4,1,name);
   envir.Reset;
END;

PROCEDURE TSphericalHar.Draw3DPlot;
{ Draws 3D plot of the Spherical Harmonics (this procedure is modified }
{ TGraph3D.PlotSurface procedure fron CUPSgrph).}
TYPE
   nodePtr  = ^nodeType;
   nodeType = RECORD
                 point : pointType;
                 next  : nodePtr;
               END;
VAR
   zAvr,maxZ,minY,r  : REAL;
   xs,ys,maxN,n,l,ang,angle,part,i : integer;
   headL,headR,nodeR,nodeL,nodeL1,nodeR1 : nodePtr;
   shadeNum          : integer;
   polyR,polyL,poly  : ARRAY[1..4] OF PointType;
   c,q,o             : pointType;

   PROCEDURE FindCoords (num,ang : integer; VAR pointL,pointR : pointType);
   VAR
      xx,yy,zz : real;
   BEGIN
      xx := x.value(num)*cos(ang*pi/180);
      yy := x.value(num)*sin(ang*pi/180);
      zz := z.value(num);
      getPoint(xx,yy, zz,pointL);
      getPoint(xx,yy,-zz,pointR);
   END;

var
   envir : TEnvironment;
BEGIN
   envir.Save;
   envir.Standardize;
   sliders.drawAll;
   HideMouse;
   SetFillStyle(SolidFill,DarkGray);
   WITH button DO bar(x1,y1,x2,y2);
   WITH button DO rectangle(x1,y1,x2,y2);
   SetFillStyle(SolidFill,black);
   WITH button DO bar(x1+6,y1+6,x2-6,y2-6);
   OpenViewPort(10+vpNumber);
   SetColor(LineColor);
   r := 1;
   with GraphScales[scaleNumber] do
    Set3DScales( r*xmin,r*xmax, r*xmin,r*xmax, r*ymin,r*ymax );
   SetViewAngles(0,beta,0);
   MaxZ := GraphScales[scaleNumber].ymax;
   maxN := x.size div 8 + 1;
   GetPoint(0,0,0,c);
   c.x := 2*c.x;
   c.y := 2*c.y;
   FOR n := 1 TO maxN DO   {draw surface}
   BEGIN
      new (nodeL);         new (nodeR);
      FindCoords( 2*n, 90, nodeL^.point, nodeR^.point);
      IF N=1 THEN headL := nodeL ELSE nodeL1^.next := nodeL;
      IF N=1 THEN headR := nodeR ELSE nodeR1^.next := nodeR;
      nodeL1 := nodeL;     nodeR1 := nodeR;
   END;
   for part := 1 to 2 do
    FOR ang := 1 TO 9 DO
    BEGIN
       if part=1 then angle:=90-10*ang else angle:=-10*(ang);
       nodeL := headL;  nodeR := headR;
       FindCoords( 2, angle, polyL[4], polyR[4]);
       FOR n := 2 TO maxN DO
       BEGIN
          polyL[1] := nodeL^.next^.point;  polyR[1] := nodeR^.next^.point;
          polyL[2] := nodeL^.point;        polyR[2] := nodeR^.point;
          polyL[3] := polyL[4];            polyR[3] := polyR[4];
          FindCoords( 2*n, angle, polyL[4] ,polyR[4] );
          shadeNum := Trunc( 8*x.value(2*n)/maxZ +1) MOD 9;
          SetFillPattern(shade[shadeNum],surfaceColor);
          for i := 1 to 4 do
           begin poly[i].x:=c.x-polyL[i].x; poly[i].y:=polyL[i].y end;
          FillPoly(4,polyL);               FillPoly(4,poly);
          for i := 1 to 4 do
           begin poly[i].x:=c.x-polyR[i].x; poly[i].y:=polyR[i].y end;
          FillPoly(4,polyR);               FillPoly(4,poly);
          nodeL^.point := polyL[3];        nodeR^.point := polyR[3];
          nodeL := nodeL^.next;            nodeR := nodeR^.next;
       END;
       nodeL^.point := polyL[4];           nodeR^.point := polyR[4];
    END;
   FOR n := 1 TO maxN DO
   BEGIN
      nodeL := headL;         nodeR := headR;
      headL := headL^.next;   headR := headR^.next;
      dispose(nodeL);         dispose(nodeR);
   END;
   ShowMouse;
   SetColor(white);
   Print(1,1,name);
   envir.Reset;
END;


end.
