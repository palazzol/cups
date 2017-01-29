           (*************************************************)
           (*************************************************)
           (**                                             **)
           (** Consortium of Upper-level Physics Software  **)
           (**             ( CUPS Project )                **)
           (**     (c) 1994 by John Wiley & Sons           **)
           (** Compiled with Utilities Ver. 1.0 (94/08/08) **)
           (**                                             **)
           (*************************************************)
           (*************************************************)

                   {************************************}
                   {************************************}
                   {***       Program Gauss.Pas      ***}
                   {***  Written by Jarek Tuszynski  ***}
                   {***        June 3, 1991          ***}
                   {************************************}
                   {************************************}
program Gauss;
{$F+,O+}

USES crt, Graph, CupsMupp, CUPS, CupsProc,
     CupsFunc, CupsGui, CupsGrph, CupsPars;
type
   SymmetryType  = (spherical, cylindrical, rectangular);
   InputFuncType = (charge, potential, field, comparison);
   T4Graphs = object
      active : boolean;
      procedure init;
      procedure Initialize( var q,v,e : Dvector );
      procedure check;
      procedure done;
   private
      plot : array[1..3] of TPlot3D;
      potential, charge, field : dMatrix;
   end;

var
   HotKeys      : THotKeys;
   Menu         : TMenu;
   F            : dVector;
   SymmetryKind : SymmetryType;
   InitialFunc  : InputFuncType;
   QuitRequest  : boolean;
   Drawing      : boolean;
   func         : TParser;
   QString,VString,EString,FString : string;
   MaxR         : real;
   NumPoints    : integer;
   plots3D      : T4Graphs;


{---------------------------- Help Screens --------------------------------}

procedure AboutProgram;
var
   screen : helpScrType;
begin
   screen[01] := '                                                  ';
   screen[02] := '    Calculating Charge Density, Potential and     ';
   screen[03] := '                                                  ';
   screen[04] := '        Electric Field in Symmetric Cases         ';
   screen[05] := '                                                  ';
   screen[06] := '                                                  ';
   screen[07] := '          Written by Jarek Tuszynski              ';
   screen[08] := '           George Mason University                ';
   screen[09] := '         (c) 1995, John Wiley & Sons              ';
   screen[10] := '                                                  ';
   screen[11] := '  This program allows the user to specify a       ';
   screen[12] := '  continous charge distribution for one of three  ';
   screen[13] := '  symmetries: spherical, cylindrical, or planar.  ';
   screen[14] := '  Once a charge density function and symmetry     ';
   screen[15] := '  have been specified the program calculates      ';
   screen[16] := '  the field using Gauss'' Law and then also       ';
   screen[17] := '  finds the potential. The program also allows    ';
   screen[18] := '  the user to enter a functional form for the     ';
   screen[19] := '  potential or field and calculates the other     ';
   screen[20] := '  two functions, using Poisson''s Equation.       ';
   screen[21] := '  Users can then compare functions calculated     ';
   screen[22] := '  by the program with their own hand calculations ';
   screen[23] := '  by using a "comparison function".               ';
   screen[24] := '                                                  ';
   screen[25] := '    Press any Key or Click the Mouse when done.   ';
   help(screen);
end;

Procedure ShowHelpScreen;
var
   helpScr    : HelpScrType;
begin
   helpScr[01] :=  'To run the program:                              ';
   helpScr[02] :=  '  - choose coordinate system.                    ';
   helpScr[03] :=  '  - input charge distribution, potential, or     ';
   helpScr[04] :=  '    electric field function.                     ';
   helpScr[05] :=  '  - compare the output with your calculations    ';
   helpScr[06] :=  '    using comparison function.                   ';
   helpScr[07] :=  ' Menu:                                           ';
   helpScr[08] :=  ' - File:                                         ';
   helpScr[09] :=  '   Gives you some information about program and  ';
   helpScr[10] :=  '   project, let you quit the program.            ';
   helpScr[11] :=  ' - Input:                                        ';
   helpScr[12] :=  '   Input charge density, potential or electric   ';
   helpScr[13] :=  '   field function and program will compute the   ';
   helpScr[14] :=  '   other two functions.                          ';
   helpScr[15] :=  ' - Symmetry:                                     ';
   helpScr[16] :=  '   System can have either: spherical             ';
   helpScr[17] :=  '   cylindrical or planar symmetry.               ';
   helpScr[18] :=  ' Hot Keys:                                       ';
   helpScr[19] :=  ' - Help: Shows this screen.                      ';
   helpScr[20] :=  ' - 2D/3D plot: Switchs between 2D and 3D plots.  ';
   helpScr[21] :=  ' - Menu: Activares the menu.                     ';
   helpScr[22] :=  ' In 3D plot use sliders to rotate plots and      ';
   helpScr[23] :=  ' buttons in upper-left corner to switch between  ';
   helpScr[24] :=  ' contour plots and 3D plots.';
   helpScr[25] :=  '     Press any Key or Click Mouse when done.     ';
   help(helpScr);
end;

{----------------------------- General SetUp -----------------------------}

PROCEDURE SetUpMenu;
BEGIN
   WITH Menu DO
   BEGIN
      init;
      column(1,'File');
       row(1,1,'About CUPS');
       row(1,2,'About Program');
       row(1,3,'Configuration');
       row(1,4,'-------------');
       row(1,5,'Exit Program');
      column(2,'Input');
       row(2,1,'Charge Density Function');
       row(2,2,'Potential Function');
       row(2,3,'Electric Field Function');
       row(2,4,'Comparison Function');
      column(3,'Symmetry');
       row(3,1,'Spherical');
       row(3,2,'Cylindrical');
       row(3,3,'Planar');
      display;
      AutoChecking(3,1);
      AutoChecking(2,1);
      rowActivate(1,4,false);
   END;
END;


PROCEDURE SetUpHotKeys;
BEGIN
   WITH HotKeys DO BEGIN
      init(3);
      key[1] := 'F1-Help';
      key[2] := 'F2-3D Plot';
      key[3] := 'F10-Menu';
   END;
   HotKeys.Display;
END;

{----------------------------- T4Graphs -----------------------------------}

procedure DrawFieldVectors (var E : dVector);
{ Draws field vectors in the current view-port. stand-alone }
VAR
   i,j,k,color,size : integer;
   maxE,minE,E_,Ex,Ey,disX,disY,r : real;
begin
   color := GetColor;
   SetColor(white);
   HideMouse;
   ClearViewPort;
   with views[ViewPortNumber] do rectangle(0,0,vx2-vx1,vy2-vy1);
   ShowMouse;
   size := e.size div 10;
   with views[viewPortNumber] do
   begin
      disX := (vx2-vx1) / (size-1);    {horiz. grid spacing}
      disY := (vy2-vy1) / (size-1);    {vert.  grid spacing}
   end;
   E.MinMax(i,minE,i,maxE);
   if (maxE-minE>macheps) then
   begin
      if (abs(minE)<abs(maxE)) then maxE:=abs(maxE) else maxE:=abs(minE);
      if (SymmetryKind=rectangular) then
       FOR i := 2 TO size-1 DO
       begin
          E_ := 0;
          for k := 0 to 9 do E_ := E_ + E.value(round(10*i-k));
          Ex := ( (E_/10)/maxE + sgn(E_) )*0.2;
          SetColor(rainbow[1].colors[trunc(abs(E_/maxE))+1]);
          FOR j := 2 TO size-1 DO
           vector(round(disX*(i-1-Ex)), round(disY*(j-1)),
                  round(disX*(i-1+Ex)), round(disY*(j-1)));
       END
      else
       FOR i := 2 TO size-1 DO
        FOR j := 2 TO size-1 DO
         begin
            r := sqrt(1.0*i*i+1.0*j*j);
            if (r<size) then
            begin
               E_ := 0;
               for k := 0 to 9 do E_ := E_ + E.value(round(10*r-k));
               Ex := (i/r)*( (E_/10)/maxE +sgn(E_) )*0.2;
               Ey := (j/r)*( (E_/10)/maxE +sgn(E_) )*0.2;
               SetColor(rainbow[1].colors[trunc(abs(E_/maxE))+1]);
               vector(round(disX*(i-1-Ex)), round(disY*(j-1-Ey)),
                      round(disX*(i-1+Ex)), round(disY*(j-1+Ey)));
            end;
         END
   end;
   SetColor(color);
end;

procedure CreateMatrix( var v : dvector; var m : dmatrix);
var
   i,j,r,size,size2 : integer;
   x,min,max : real;
   n : dvector;
begin
   size  := NumPoints div 5;
   if m.NCols>0 then m.free;
   m.init(size, size);
   size2 := Size div 2;
   if (SymmetryKind=rectangular) then
   begin
      for i := 1 to size do
      begin
         x := 0;
         for j := 1 to 5 do x := x + v.value(5*(i-1) +j);
         for j := 1 to size do m.put(j,i,x);
      end;
   end
   else
   begin
      n.init(size2);
      for i := 1 to size2 do
      begin
         x := 0;
         for j := 1 to 10 do x := x + v.value(10*(i-1) +j);
         n.put(i,x);
      end;
      for i := 1 to size do for j := 1 to size do
      begin
         r := round(sqrt(sqr(i-size2)+sqr(j-size2)));
         if r=0 then m.put( i,j, n.value(1) ) else
         if r<size2 then m.put( i,j, n.value(r) ) else
         m.put( i,j, n.value(size2));
      end;
      n.free;
   end;
   m.MinMax(i,i,min,i,i,max);
   if max-min<1e-6 then
   begin
      if min*max<0 then min:=0 else min:=(max+min)/2;
      m.fill(min);
   end;
end;


procedure T4Graphs.Init;
begin
   potential.NCols := 0;
   charge.NCols := 0;
   field.NCols := 0;
end;

procedure T4Graphs.Initialize( var q,v,e : DVector);
var i,vp : integer;
begin
   vp := ViewPortNumber;
   active := true;
   CreateMatrix(q, charge);
   CreateMatrix(v, potential);
   CreateMatrix(e, field);
   DefineViewPort (4, 0.0,  1.0, 0.05, 0.95);     {cleans area of the screen}
   CloseViewPort(4);
   DefineViewPort(4, 0.00, 0.49, 0.51, 0.93);
   DefineViewPort(5, 0.51, 1.00, 0.51, 0.93);
   DefineViewPort(6, 0.00, 0.49, 0.07, 0.49);
   DefineViewPort(7, 0.59, 0.92, 0.07, 0.49);
   for i := 1 to 3 do plot[i].init(3+i);
   for i := 1 to 3 do plot[i].SetViewAngles(300,20,0);
   plot[1].PlotMatrix(potential,'Potential');
   plot[2].PlotMatrix(charge,   'Charge Distribution');
   plot[3].PlotMatrix(field,    'Field Magnitude');
   SelectViewPort(7);
   DrawFieldVectors(e);
   PutLabel( inside, 'Electric Field');
   SelectViewPort(vp);
end;

procedure T4Graphs.done;
var i,vp : integer;
begin
   potential.free;
   charge.free;
   field.free;
   vp := viewPortNumber;
   for i := 1 to 3 do plot[i].done;
   DefineViewPort (4, 0.0,  1.0, 0.04, 0.95);     {cleans area of the screen}
   CloseViewPort(4);
   SelectViewPort(vp);
end;

procedure T4Graphs.check;
var i : integer;
begin
   if active then
    for i := 1 to 3 do plot[i].check;
end;


{---------------------------- Function Input Screen ------------------------}

function InputFunctionScreen : boolean;
VAR
   FScreen   : TInputScreen;
   succes,cancel : boolean;
   FStr,s    : string;
   n         : byte;
   ParserScr : helpScrType;
   r : char;
BEGIN
   if SymmetryKind=Rectangular then r := 'x' else r := 'r';
   case SymmetryKind of
    Spherical   : s:='(Spherical Symmetry)';
    Cylindrical : s:='(Cylindrical Symmetry)';
    Rectangular : s:='(Planar Symmetry)';
   end;
   case InitialFunc of
    Charge    : FStr := 'Rho('+r+')=';
    Potential : FStr := 'V('+r+')=';
    Field     : FStr := 'E('+r+')=';
    Comparison: FStr := 'F('+r+')=';
   end;
   with FScreen do
   begin
      init;
      if (InitialFunc=Comparison) then
         DefineInputPort (0.03, 0.97, 0.31, 0.69)
      else
         DefineInputPort (0.03, 0.97, 0.35, 0.65);
      case InitialFunc of
       Charge    : LoadLine('          Input a Charge Density Function '+s);
       Potential : LoadLine('             Input a Potential Function '+s);
       Field     : LoadLine('          Input a Electric Field Function '+s);
       Comparison: LoadLine('                   Input a Function '+s);
      end;
      LoadLine(concat(fStr,'"                                                             " '));
      LoadLine('      Range of '+r+' from 0 to {        }');
      LoadLine('');
      LoadLine('                     [  Ok  ]   [Cancel]   [ Help ]');
      case InitialFunc of
       Charge    : SetString(1,QString);
       Potential : SetString(1,VString);
       Field     : SetString(1,EString);
       Comparison: SetString(1,FString);
      end;
      SetNumber(2,MaxR);
      SetNumberLimits(2,1E-10,1e10);
      AboutParser(ParserScr);
      SetHelpScreen(ParserScr);
      AcceptScreen;
      FStr := GetString(1);
      case InitialFunc of
       Charge    : QString := FStr;
       Potential : VString := FStr;
       Field     : EString := FStr;
       Comparison: FString := FStr;
      end;
      MaxR := GetNumber(2);
      Cancel := canceled;
      done;
   end;
   if not cancel then
   begin
      succes := func.parse('r','x','r','r',fStr);
      if not succes then announce('Error: Cannot evaluate this function.');
   end;
   InputFunctionScreen := (not Cancel) and succes;
END;


{---------------------------- Drawing Tools -------------------------------}

Procedure DrawLegend;
var
   xc1,xc2,xc3 : integer;
   s : string;
begin
   SelectMuppetPort;
   HideMouse;
   xc1 := round(GetMaxX*0.805);
   xc2 := round(GetMaxX*0.9);
   xc3 := round(GetMaxX*0.97);
   SetColor(white);
   SetTextJustify(LeftText,TopText);
   OutTextXY(xc1,3*RowHt+3,'Charge Density');
   OutTextXY(xc1,5*RowHt+3,'Potential');
   OutTextXY(xc1,7*RowHt+3,'Electric Field');
   SetColor(yellow);
   line(xc1,3*RowHt,xc2,3*RowHt);
   SetColor(LightGreen);
   line(xc1,5*RowHt,xc2,5*RowHt);
   SetColor(LightRed);
   line(xc1,7*RowHt,xc2,7*RowHt);
   SetColor(white);
   case SymmetryKind of
    Spherical   : s:=' Spherical  ';
    Cylindrical : s:=' Cylindrical';
    Rectangular : s:=' Planar     ';
   end;
   SetFillStyle(SolidFill,green);
   bar3D(xc1+2,9*RowHt-5,xc3,11*RowHt+2,3,true);
   OutTextXY(xc1, 9*RowHt+3,s);
   OutTextXY(xc1,10*RowHt+3,' Symmetry');
   ShowMouse;
end;


procedure Cloud(Q : dVector);
var
   i,j,num,r,color : integer;
   min,max,xFactor,yFactor,xc,yc,radius,n : real;
begin
   ClearMKBuffers;
   event.KeyPressed := false;
   event.MouseClicked := false;
   OpenViewPort(1);
   PutLabel(inside,'Charge Distribution Diagram');
   Q.MinMax(i,min,i,max);
   if (abs(min)<abs(max)) then max:=abs(max) else max:=abs(min);
   n := Q.length/maxR;
   DefineScale(1,0,MaxR,0,MaxR/2);
   if (max<>min) then
   with views[viewPortNumber] do with GraphScales[1] do
   begin
      Xfactor := (Vx2-Vx1)/(xmax-xmin);
      Yfactor := (Vy2-Vy1)/(ymax-ymin);
      i := 0;
      while (i<8000) and not (event.KeyPressed or event.mouseClicked) do
      begin
         CheckForEvents;
         for j := 1 to 100 do
         begin
            xc := random*MaxR;
            yc := random*MaxR/2;
            if (SymmetryKind=rectangular) then r := trunc(n*xc)
            else r := trunc(n*sqrt(xc*xc + yc*yc));
            if (r>0) and (r<Q.size) then
            begin
               radius := abs(Q.value(r)/max);
               if (radius > random) then
               begin
                 inc(i);
                 if Q.value(r)<0 then color:=yellow else color:=lightred;
                 PutPixel(Round(Xfactor*(xc-xmin)),
                          Round(Yfactor*(ymax-yc)),color);
               end;
            end;
         end;
      end;
   end;
end;

procedure Scale( var y : dVector);
var
   i : integer;
   min1,max1,min2,max2,d,v : real;
begin
   Min1:=y.value(1); Min2:=y.value(2);
   if Min1>Min2 then begin v:=min1; min1:=min2; min2:=v; end;
   Max2:=Min1;       Max1:=Min2;
   FOR  i := 3 TO y.Size DO
   BEGIN
      v:=y.value(i);
      IF v<Min1 THEN begin min2:=min1; min1:=v end else
      IF v<Min2 THEN min2:=v;
      IF v>Max1 THEN begin max2:=max1; max1:=v end else
      IF v>Max2 THEN max2:=v;
   END;
   if (max1-max2)>(max2-min2) then max1:=max2;
   if (min2-min1)>(max2-min2) then min1:=min2;
   if abs(max1-min1)<1e-5 then begin max1:=max1+10; min1:=min1-10 end;
   d := (max1-min1)/10;
   DefineScale(2,0,MaxR,min1-d,max1+d);
end;


procedure Plot(var x,y : dVector; color : integer);
var
   i,c,vpNum : integer;
   min1,max1,min2,max2,d,v : real;
begin
   vpNum  := ViewPortNumber;
   c      := getColor;
   Scale(y);
   SetColor(color);
   PlotDVectors(x,y,1,x.size);
   SetTextJustify(RightText,CenterText);
   i := mapY(0);
   if (i>0) then
   begin
      SetLineStyle(SolidLn,0,ThickWidth);
      Line(0,i,5,i);
      SetLineStyle(SolidLn,0,NormWidth);
      SelectMuppetPort;
      with views[vpNum] do outTextXY(vx1-1, vy1+i, '0');
      SelectViewPort(vpNum);
      SetTextJustify(LeftText,TopText);
   end;
   SetColor(c);
end;

procedure PlotPoints(var x,y : dVector; color : integer);
var
   i,c,xc,yc : integer;
begin
   c := getColor;
   Scale(y);
   SetColor(color);
   SetTextJustify(CenterText,CenterText);
   for i := 1 to y.length div 20 do
   begin
      xc := mapX(x.value(20*i));
      yc := mapY(y.value(20*i));
      outTextXY( xc, yc, 'x');
   end;
   SetLineStyle(DottedLn,0,1);
   PlotDVectors(x,y,1,x.size);
   SetLineStyle(SolidLn,0,1);
   SetTextJustify(LeftText,TopText);
   SetColor(c);
end;


procedure DrawComparisonFunction;
var
   value,radius,F0 : real;
   r : integer;
   x   : DVector;
begin
   SelectViewPort(2);
   x.init(NumPoints);
   for r := 1 to NumPoints do x.put(r,(r-1)*MaxR/(NumPoints-1));
   PlotPoints(x,f,DarkGray);      {errase old ones}
   ErrorFound  := false;
   HaltIfError := false;
   for r := 1 to NumPoints do
   begin
      radius := (r-1)*MaxR/(NumPoints-1);
      value := func.f(radius,radius,0,0);
      IF errorFound THEN
      BEGIN HaltIfError:=true; ErrorFound:=false; Exit END;
      if value>1E6 then value := 1E6;
      F.put(r,value);
   end;
   if (abs(F.value(1)-1e6)<1) then
    F.put(1, 3*F.value(2)-3*F.value(3)+F.value(4) );
   HaltIfError := true;
   PlotPoints(x,f,lightCyan);
   x.free;
end;


{-------------------------- Physics of the Problem ------------------------}

procedure Find_E_V (Q : dVector; var E,V : dVector);
var
   r : integer;
   y : dVector;
   q_,v_,e_ : array[1..5] of real;
begin
   y.init(NumPoints);
   case SymmetryKind of
    spherical  : begin                                       {E(Q)}
       for r := 1 to NumPoints do y.put(r, Q.value(r)*sqr(r-1.0) );
       for r := 2 to NumPoints do E.put(r, y.Simpson(1,r)/sqr(r-1.0) );  end;
    cylindrical: begin
       for r := 1 to NumPoints do y.put(r, Q.value(r)*(r-1) );
       for r := 2 to NumPoints do E.put(r, y.Simpson(1,r)/(r-1) );       end;
    rectangular: begin
       for r := 1 to NumPoints do E.put(r, Q.Simpson(1,r));      {E(Q)}
       E.AddScalar(-E.value(NumPoints)/2);                                end;
   end;
   if (SymmetryKind<>rectangular) then
    E.put(1, 3*E.value(2)-3*E.value(3)+E.value(4));
   y.free;
   for r := 1 to NumPoints do V.put(r,-E.Simpson(1,r));        {V(E)}

   for r := 1 to 5 do q_[r]:= q.value(r);
   for r := 1 to 5 do e_[r]:= e.value(r);
   for r := 1 to 5 do v_[r]:= v.value(r);
end;

procedure Find_E_Q (V : dVector; var E,Q : dVector);
var
   r : integer;
   y : dvector;
   q_,v_,e_ : array[1..5] of real;
begin
   y.init(NumPoints);
   case SymmetryKind of
    spherical  : begin                                     {Q(V)}
       for r := 1 to NumPoints do y.put(r, V.value(r)*(r-1) );
       for r := 2 to NumPoints do Q.put(r, y.dVdx(r,1)/sqr(r-1.0) ); end;
    cylindrical:
       for r := 2 to NumPoints do Q.put(r, V.dVdx(r,1)/(r-1) );
    rectangular: begin
       for r := 1 to NumPoints do y.put( r, V.value(r) / r );
       for r := 1 to NumPoints do Q.put( r, y.dVdx(r,1) );     {Q(V)} end;
   end;
   y.free;
   if (SymmetryKind<>rectangular) then
    Q.put(1, 3*Q.value(2)-3*Q.value(3)+Q.value(4));
   for r := 1 to NumPoints do E.put(r, -V.dVdx(r,1) );          {E(V)}

   for r := 1 to 5 do q_[r]:= q.value(r);
   for r := 1 to 5 do e_[r]:= e.value(r);
   for r := 1 to 5 do v_[r]:= v.value(r);
end;


procedure Find_V_Q (E : dVector; var V,Q : dVector);
var
   r : integer;
   y   : DVector;
   q_,v_,e_ : array[1..5] of real;
begin
   y.init(NumPoints);
   case SymmetryKind of
    spherical  : begin                                   {Q(E)}
       for r := 1 to NumPoints do y.put(r, E.value(r)*sqr(r-1.0) );
       for r := 2 to NumPoints do Q.put(r, y.dVdx(r,1)/sqr(r-1.0) ); end;
    cylindrical: begin
       for r := 1 to NumPoints do y.put(r, E.value(r)*(r-1) );
       for r := 2 to NumPoints do Q.put(r, y.dVdx(r,1)/(r-1) );      end;
    rectangular:
       for r := 1 to NumPoints do Q.put( r, E.dVdx(r,1) );   {Q(E)}
   end;
   y.free;
   if (SymmetryKind<>rectangular) then
    Q.put(1, 3*Q.value(2)-3*Q.value(3)+Q.value(4) );
   for r := 1 to NumPoints do V.put(r,-E.Simpson(1,r));    {V(E)}

   for r := 1 to 5 do q_[r]:= q.value(r);
   for r := 1 to 5 do e_[r]:= e.value(r);
   for r := 1 to 5 do v_[r]:= v.value(r);
end;



procedure Draw3Functions;
var
   Q,V,E,x  : dvector;
   i,j,r     : integer;
   value,radius : real;
begin
   x.init(NumPoints);
   Q.init(NumPoints);
   V.init(NumPoints);
   E.init(NumPoints);
   ErrorFound  := false;
   HaltIfError := false;
   for r := 1 to NumPoints do
   begin
      radius := (r-1)*MaxR/(NumPoints-1);
      x.put(r,radius);
      value := func.f(radius,radius,0,0);
      IF errorFound THEN
      BEGIN HaltIfError:=true; ErrorFound:=false; Exit END;
      if value>1E6 then value := 1E6;
      Q.put(r,value);
   end;
   if (abs(Q.value(1)-1e6)<1) then
    Q.put(1, 3*Q.value(2)-3*Q.value(3)+Q.value(4) );
   HaltIfError := true;
   if InitialFunc=Potential then V.equate(1,Q) else
   if InitialFunc=Field     then E.equate(1,Q);
   if not ErrorFound then
   begin
      case InitialFunc of
       Charge   : Find_E_V(Q,E,V);
       Potential: Find_E_Q(V,E,Q);
       Field    : Find_V_Q(E,V,Q);
      end;
      if plots3D.active then plots3D.Initialize(Q,V,E) else
      begin
         CloseViewPort(3);
         DrawLegend;
         OpenViewPort(2);
         PutLabel(inside,'Charge Distribution & Potential & Electric Field Functions');
         Plot(x,Q,yellow);
         Plot(x,E,LightRed);
         Plot(x,V,LightGreen);
         Axis( 0, 1E20, TickSpace(MaxR), 2E20);
         Cloud(Q);
         drawing := true;
      end;
   end;
   Q.free;
   V.free;
   x.free;
   E.free;
end;


{--------------------------- Main Control ---------------------------------}

procedure HandleMenu;
var
   fn : functionXY;
   oldFunc : InputFuncType;
   fStr : string;
begin
   with menu do
   case colChosen of
    1 : case rowChosen of
         1: AboutCUPS;
         2: AboutProgram;
         3: Configuration;
         5: quitRequest := true;
        end;
    2 : begin
           oldFunc := InitialFunc;
           case rowChosen of
            1: InitialFunc := Charge;
            2: InitialFunc := Potential;
            3: InitialFunc := Field;
            4: InitialFunc := Comparison;
           end;
           if InputFunctionScreen then
            if InitialFunc=Comparison then DrawComparisonFunction
            else Draw3Functions
           else InitialFunc := oldFunc;
           if InitialFunc=Comparison then
           begin
              InitialFunc := oldFunc;
              case InitialFunc of
               Charge    : fStr := QString;
               Potential : fStr := VString;
               Field     : fStr := EString;
              end;
              func.parse('r','x','r','r',fStr);
           end;
        end;
    3 : begin
          case rowChosen of
           1: SymmetryKind:=Spherical;
           2: SymmetryKind:=Cylindrical;
           3: SymmetryKind:=Rectangular;
          end;
          Draw3Functions;
        end;
   end;
end;

procedure HandleHotKeys (key : integer);
begin
   CASE key OF
    1 : ShowHelpScreen;
    2 : begin
           plots3D.active := not plots3D.active;
           if not plots3D.active then Plots3D.done;
           if plots3D.active then HotKeys.key[2] := 'F2-2D Plots'
           else HotKeys.key[2] := 'F2-3D Plots';
           menu.rowActivate(2,4,not plots3D.active);
           HotKeys.Display;
           Draw3Functions;
        end;
    3 : if menu.chosen then HandleMenu;
   end; {case}
end;


procedure initialize;
var
   r          : integer;
   radius     : real;
   Ok         : boolean;
begin
   InitialFunc  := charge;
   QuitRequest  := false;
   SymmetryKind := spherical;
   QString      :=  'h(2-r)'{'h(r-4)*h(6-r)'};
   VString      := '';
   EString      := '';
   FString      := '';
   NumPoints    := 200;
   MaxR         := 10;
   DefineViewPort (1, 0.05, 0.8, 0.08, 0.48);
   DefineViewPort (2, 0.05, 0.8, 0.52, 0.92);
   DefineViewPort (3, 0.00, 1.0, 0.49, 0.93);
   SetUpMenu;
   SetUpHotKeys;
   f.init(NumPoints);
   OpenViewPort(1);
   SetColor(white);
   PutLabel(inside,'Charge Distribution Diagram');
   OpenViewPort(2);
   PutLabel(inside,'Charge Distribution & Potential & Electric Field Functions');
   DefineScale(2,-MaxR,MaxR,0,MaxR);
   Axis( 0, 100, 2, 200);
   DrawLegend;
   func.init;
   Ok := func.parse('r','x','r','x',QString);
   AboutProgram;
   Draw3Functions;
end;

VAR
   key : byte;
BEGIN
   CUPSinit;
   initialize;
   REPEAT
      if not drawing then CheckForEvents;
      drawing := false;
      IF HotKeys.Pressed(key) THEN HandleHotKeys(key);
      IF Menu.Activated THEN HandleMenu;
      Plots3D.Check;
   UNTIL QuitRequest;
   menu.done;
   CUPSdone;
END.
