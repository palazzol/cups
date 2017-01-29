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

Program  TwoLens;

USES crt, graph, CUPSmupp, CUPS, CUPSfunc, CUPSproc, CUPSgui, CUPSgrph;

Type
  LensType   = (Convex1, Convex2, mirror);
  DragChoice = (Fpoint1, Fpoint2,Fpoint1B, Fpoint2B, obj, Lens1, Lens2, Mirror1, None);
  HelpContextType = (fileitem, lenstypes, raypaths, zoom, finetune,
                      setdata, display, general);
  Float = Extended;
  Scales = Record
    xr,xl,yb,yt : Real;
  End;
  Sarray = ARRAY[1..5] OF Scales;

Const   ObjectColor = White;
        ImageColor  = Yellow;
        IIcolor     = Brown;
        LensColor   = LightCyan;
        FP1Color    = LightRed;
        FP2Color    = LightRed;
        AxisColor   = White;
        Ray1        = LightGreen;
        Ray2        = LightBlue;
        Ray3        = LightMagenta;
        Ray4        = Green;
        Ray5        = Blue;

        Rcheck = 0.000001;
        Sxl   = 0.050; {ViewPort coordinates}
        Sxr   = 0.90;
        Sby   = 0.38;
        Sty   = 0.9;
        Sx    = 1;


Var  Menu           : TMenu;
     HotKeys        : THotKeys;
     sliders        : TSliders;
     DragButtons    : TButtons;
     LSystem        : LensType;
     Nrays          : Integer;
     Pick           : DragChoice;
     HelpContext    : HelpContextType;
     GOx,FP1,FP2    : Float;
     GL1,GL2,FPy    : Float;
     GIx,GIy,GLY    : Float;
     GOy, Oy,Ix,Iy  : Float;
     GIFx, GIFy     : Float;
     IFx,IFy        : Float;
     Sy             : Float;
     VNum,Snum      : Integer;
     C1,C2          : integer;
     Ovirtual       : Boolean;
     ILvirtual      : Boolean;
     IRvirtual      : Boolean;
     Wsize,FT       : Boolean;
     EscPressed     : Boolean;
     QuitFlag       : Boolean;
     S              : Sarray;

procedure intro;
var IntroScreen : HelpScrType;
begin
   IntroScreen[01] := '                                                 ';
   IntroScreen[02] := '      RAY DIAGRAMS FOR LENSES AND MIRRORS        ';
   IntroScreen[03] := '                                                 ';
   IntroScreen[04] := '                        by                       ';
   IntroScreen[05] := '                                                 ';
   IntroScreen[06] := '          Peter Flynn and Graham Keeler          ';
   IntroScreen[07] := '            University of Salford, UK            ';
   IntroScreen[08] := '                                                 ';
   IntroScreen[09] := '                   Version 1.0                   ';
   IntroScreen[10] := '               Copyright (c) 1995                ';
   IntroScreen[11] := '                                                 ';
   IntroScreen[12] := '                                                 ';
   IntroScreen[13] := ' This program shows ray diagrams for single      ';
   IntroScreen[14] := ' mirrors or lenses, or systems of two lenses,    ';
   IntroScreen[15] := ' to illustrate the formation of images from real ';
   IntroScreen[16] := ' or virtual objects.                             ';
   IntroScreen[17] := '                                                 ';
   IntroScreen[18] := ' The three principal rays are shown for each     ';
   IntroScreen[19] := ' case, or optionally five rays for two lens      ';
   IntroScreen[20] := ' systems. Adjustments are most easily made by    ';
   IntroScreen[21] := ' clicking and dragging object, lens/mirror or    ';
   IntroScreen[22] := ' focal point.                                    ';
   IntroScreen[23] := '                                                 ';
   IntroScreen[24] := '    Press <Enter> or Click Mouse to continue.    ';
   IntroScreen[25] := '                                                 ';
   help(IntroScreen);
end;

Procedure  SetUpHotKeys(var HotKeys : THotKeys);
BEGIN
  WITH HotKeys DO
  BEGIN
    init(6);
    key[1] := 'F1-Help';
    key[2]:= 'F2-Split/Scrn';
    IF Wsize=True THEN key[2] := 'F2-Full/Scrn';
    Key[3] := 'F3-Center';
    Key[4] := 'F4-Zoom-in';
    Key[5] := 'F5-Zoom-out';
    Key[6] := 'F10-Menu';
  Display;
  END;
END;

Procedure  SetUpMenu(var Menu : TMenu);
BEGIN
  WITH Menu DO
  BEGIN
    init;
    column(1, 'File');
         row(1,1, 'About CUPS');
         row(1,2, 'About Program');
         row(1,3, 'Configuration');
         row(1,4, '-------------');
         row(1,5, 'Exit Program');
    column(2, 'LensType');
        row(2,1, 'Distant Object');
        row(2,2, 'LENSES');
        row(2,3, '  Convex Only');
        row(2,4, '  Concave Only');
        row(2,5, '  Convex-Convex');
        row(2,6, '  Concave-Concave');
        row(2,7, '  Convex-Concave');
        row(2,8, '  Concave-Convex');
        row(2,9, '  Microscope');
        row(2,10,'  Telescope');
        row(2,11,'MIRRORS');
        row(2,12,'  Concave Mirror');
        row(2,13,'  Convex Mirror');
    column(3, 'Ray Paths');
        if LSystem=Mirror then begin
           row(3,1, 'Center of Curvature');
           row(3,2, 'Center of Mirror');
        end else begin
           row(3,1, '3 Rays');
           row(3,2, '5 Rays');
        end;
    column(4, 'Zoom');
        row(4,1, 'Manual');
        row(4,2, 'ZOOM-In');
        row(4,3, '  X-Zoom 50%');
        row(4,4, '  Y-Zoom 50%');
        row(4,5, '  Both 50%');
        row(4,6, '  Variable');
        row(4,7, 'ZOOM-Out');
        row(4,8, '  X-Zoom 200%');
        row(4,9, '  Y-Zoom 200%');
        row(4,10, '  Both 200%');
        row(4,11, 'PAN');
        row(4,12, '  Left');
        row(4,13, '  Right');
        row(4,14, '  Up');
        row(4,15, '  Down');
    if FT then begin
      column(5, 'Drag Mode');
        row(5,1, 'Object Distance');
    end
    else begin
      column(5, 'Fine Tune');
        row(5,1, 'Object');
    end;
    row(5,2, 'Focal Point 1');
    row(5,3, 'Focal Point 2');
    row(5,4, 'Lens 1');
    row(5,5, 'Lens 2');
    row(5,6, 'Return to Drag Mode');
    column(6, 'Set Data');
    Rowactivate(5,6,ft);
    Rowactivate(1,4,False);
    Rowactivate(2,2,False);Rowactivate(2,11,False);
    Rowactivate(4,2,False);Rowactivate(4,7,False);
    Rowactivate(4,11,False);
    AutoChecking(3,(Nrays-1) div 2);
  END;
END;

function ClickedInViewPort(vp : integer) : boolean;
begin
   with views[vp] do
    with event do
     ClickedInViewPort := ( mouseClicked and
                            (x<vx2) and (x>vx1) and (y<vy2) and (y>vy1) );
end;

Procedure InfoWindow;
Begin
  IF Wsize=True THEN Begin
    SelectViewPort(2);
      SetColor(white);
      Print(1,1, 'Object  ');
      Print(1,2, ' Global   ');
      if LSystem=Mirror then print(1,3, ' To Mirror')
      else Print(1,3, ' To Lens  ');
      Print(1,4, ' Height   ');
      SetColor(Yellow);
    Case Lsystem of
     Convex1:
      Begin
        Print(22,1, 'Image ');
        Print(22,2, ' Global   ');
        Print(22,3, ' To Lens  ');
        Print(22,4, ' Height   ');
        Setcolor(Red);
        Print(42,1, 'F. Points');
        Print(42,2, ' Global     ');
        Print(42,3, ' F. Length  ');
        Setcolor(Cyan);
        Print(62,1, 'Lens ');
        Print(62,2, ' Global  ');
      End;
     Convex2:
      Begin
        Print(36,1, 'End Image');
        Print(36,2, ' Global   ');
        Print(36,3, ' To Lens  ');
        Print(36,4, ' Height   ');
        Setcolor(Brown);
        Print(18,1, 'Mid-Image');
        Print(18,2, ' Global   ');
        Print(18,3, ' To Lens  ');
        Print(18,4, ' Height   ');
        Setcolor(Red);
        Print(54,1, 'F. Lengths');
        Print(54,2, ' Left   ');
        Print(54,3, ' Right  ');
        Setcolor(Cyan);
        Print(68,1, 'Lenses');
        Print(68,2, ' L   ');
        Print(68,3, ' R   ');
      End;
     Mirror:
      Begin
        Print(22,1, 'Image ');
        Print(22,2, ' Global    ');
        Print(22,3, ' To Mirror ');
        Print(22,4, ' Height    ');
        Setcolor(Red);
        Print(42,1, 'F. Points');
        Print(42,2, ' Global     ');
        Print(42,3, ' F. Length  ');
        Setcolor(Cyan);
        Print(62,1, 'Mirror ');
        Print(62,2, ' Global  ');
      End;
    End;
  End;
  IF Wsize=False THEN Begin
    SelectViewPort(3);
    CASE Lsystem OF
      Convex1, Mirror: Begin
                 Setcolor(White);
                 Print(46,1, 'Object Dist. =');
                 Print(46,2, 'Image  Dist. =');
                 Print(46,3, 'Object Size  =');
                 Print(46,4, 'Image  Size  =');
                 Print(46,5, 'Focal Length =');
               End;
      Convex2: Begin
                 Setcolor(white);
                 Print(40,1, 'Object    x=       y=');
                 Print(40,2, 'Mid Image x=       y=');
                 Print(40,3, 'End Image x=       y= ');
                 Print(40,4, 'FP(L)=         FP(R)=');
                 Print(40,5, 'Lens Separation     =');
               End;
   End;
  End;
End;

Procedure  Clearup(var VNum : integer);
Begin
  CloseViewPort(0);
{  ClearViewPort;}
  Menu.Display;
  HotKeys.Display;
  IF Lsystem=Convex2 THEN
     WITH Menu DO Begin
      RowActivate(5,3,True);RowActivate(5,4,True);RowActivate(5,5,True);
      RowActivate(3,2,True);End
     ELSE WITH Menu DO Begin
      RowActivate(5,3,False);RowActivate(5,4,False);RowActivate(5,5,False);
      if LSystem=Mirror then RowActivate(3,2,True)else RowActivate(3,2,False);
     End;

  IF Wsize=True THEN Begin
    GraphBackcolor:= Darkgray;
    OpenViewPort(2);
    VNum:=1;
  End ELSE VNum:=3;

  GraphBackcolor:= Black;
  OpenViewport(VNum);
  SelectScale(VNum);
  Axis(2*s[3].xr,0,5,5);
End;

Procedure  SetUpsliders(var Sliders : TSliders;
                        var DragButtons : TButtons;
                        Pick : DRagChoice);
Var Lstr,Hstr,Name   : String;
    Ox : Float;
Begin
  With Sliders DO
  Begin

    Done;

    Case Pick OF
    Obj: Begin
           Ox:=GL1-GOx;
           Str((Ox-1):5:3,Lstr);Str((Ox+1):5:3,Hstr);
           If WSize then create(1,  Ox-1 ,Ox+1,Ox, 0.03, 0.30,0.58,3, ''+Lstr,''+Hstr,'Object Distance',false)
              else create(1,  Ox-1 ,Ox+1,Ox, 0.03, 0.08,0.58,3, ''+Lstr,''+Hstr,'Object Distance',false);
           Str((GOy-2):5:1,Lstr);Str((GOy+2):5:1,Hstr);
           create(2,GOy-2,GOy+2,GOy, 0.95,0.38,0.9,2, ''+Lstr, ''+Hstr,   'Height' ,true );
         End;
    Fpoint1: Begin
               if LSystem=Convex2 then Name:='Left Lens F.Length'
                  else Name:='Focal Length';
               Str((FP1-2):5:3,Lstr);Str((FP1+2):5:3,Hstr);
               If WSize then create(1, FP1-2 ,FP1+2,FP1, 0.03, 0.30,0.58,3, ''+Lstr,''+Hstr,Name,false)
                  else create(1, FP1-2 ,FP1+2,FP1, 0.03, 0.08,0.58,3, ''+Lstr,''+Hstr,Name,false);
             End;
    Fpoint2: Begin
               Str((FP2-2):5:3,Lstr);Str((FP2+2):5:3,Hstr);
               If WSize then create(1, FP2-2 ,FP2+2,FP2, 0.03, 0.30,0.58,3, ''+Lstr,''+Hstr,'Right Lens F.Length ',false)
                  else create(1, FP2-2 ,FP2+2,FP2, 0.03, 0.08,0.58,3, ''+Lstr,''+Hstr,'Right Lens F.Length ',false);
             End;
    Fpoint1B: Begin
               if LSystem=Convex2 then Name:='Left Lens F.Length'
                  else Name:='Focal Length';
               Str((FP1-2):5:3,Lstr);Str((FP1+2):5:3,Hstr);
               If WSize then create(1, FP1-2 ,FP1+2,FP1, 0.03, 0.30,0.58,3, ''+Lstr,''+Hstr,Name,false)
                  else create(1, FP1-2 ,FP1+2,FP1, 0.03, 0.08,0.58,3, ''+Lstr,''+Hstr,Name,false);
             End;{shouldn't happen}
    Fpoint2B: Begin
               Str((FP2-2):5:3,Lstr);Str((FP2+2):5:3,Hstr);
               If WSize then create(1, FP2-2 ,FP2+2,FP2, 0.03, 0.30,0.58,3, ''+Lstr,''+Hstr,'Right Lens F.Length ',false)
                  else create(1, FP2-2 ,FP2+2,FP2, 0.03, 0.08,0.58,3, ''+Lstr,''+Hstr,'Right Lens F.Length ',false);
             End;{shouldn't happen}
    Lens1:   Begin
               Str((GL1-2):5:3,Lstr);Str((GL1+2):5:3,Hstr);
               If WSize then create(1, GL1-2 ,GL1+2,GL1, 0.03, 0.30,0.58,3, ''+Lstr,''+Hstr,'Left Lens ',false)
                  else create(1, GL1-2 ,GL1+2,GL1, 0.03, 0.08,0.58,3, ''+Lstr,''+Hstr,'Left Lens ',false);
             End;
    Lens2:   Begin
               Str((GL2-2):5:3,Lstr);Str((GL2+2):5:3,Hstr);
               If WSize then create(1, GL2-2 ,GL2+2,GL2, 0.03, 0.30,0.58,3, ''+Lstr,''+Hstr,'Right Lens ',false)
                  else create(1, GL2-2 ,GL2+2,GL2, 0.03, 0.08,0.58,3, ''+Lstr,''+Hstr,'Right Lens ',false);
             End;
    Mirror1: Begin
               Str((GL1-2):5:3,Lstr);Str((GL1+2):5:3,Hstr);
               If WSize then create(1, GL1-2 ,GL1+2,GL1, 0.03, 0.30,0.58,3, ''+Lstr,''+Hstr,'Left Lens ',false)
                  else create(1, GL1-2 ,GL1+2,GL1, 0.03, 0.08,0.58,3, ''+Lstr,''+Hstr,'Left Lens ',false);
             End;
    End;
    drawAll;
  End;
  DragButtons.Done;
  If WSize then begin
     DragButtons.Create(0, 0.67, 0.315, 'Fine Tune');
     DragButtons.Create(1, 0.85, 0.315, 'Drag Mode');
  end else begin
     DragButtons.Create(0, 0.67, 0.095, 'Fine Tune');
     DragButtons.Create(1, 0.85, 0.095, 'Drag Mode');
  end;
  DragButtons.number := 0;
  DragButtons.DrawAll;
end;

Procedure CvexImage(Var Ix,Iy: Float; Var Ovirtual, Ivirtual: Boolean);
Var
  Ox :Float;
Begin
  Ox:= GL1 - GOx;
  IF ABS(Ox-C1*FP1)>RCheck THEN
    Ix:= Ox*C1*FP1/(Ox-C1*FP1) {from lens formula 1/u + 1/v = 1/f
                                hence             v = uf/(u - f)  }
  ELSE Ix:=800;
  IF abs(Ox)>0 THEN Iy:= -GOY*(Ix/Ox) {from similar triangles}
    ELSE Iy:= 0;
  IF GOx>GL1 THEN Ovirtual:= True ELSE Ovirtual:=False;
  IF Ix<0 THEN Ivirtual:= True ELSE Ivirtual:=False;
End;

Procedure FImage(Var IFx,IFy: Float; var IRvirtual: Boolean);
Var U,D,Ox  : Float;
Begin
  Ox:= GL1 - GOx;
  D:= ABS(GL1-GL2);
  U:= D - Ix;
  IF ABS(U-C2*FP2)>RCheck THEN
    IFx:= U*C2*FP2/(U-C2*FP2) {from lens formula 1/u + 1/v = 1/f
                                hence             v = uf/(u - f)  }
  ELSE IFx:=800;
  IF abs(U)>0 THEN IFy:= -Iy*(IFx/U)  {from similar triangles}
    ELSE IFy:= 0;
  IF IFx<0 THEN IRvirtual:= True ELSE IRvirtual:=False;
End;

Procedure MirrorImage(Var Ix,Iy: Float; Var Ovirtual, Ivirtual: Boolean);
Var
  Ox  :Float;

Begin
  Ox:= GL1 - GOx;
  IF ABS(Ox-C1*FP1)>RCheck THEN Ix:= -Ox*C1*FP1/(Ox-C1*FP1)
    else Ix:=800;
  IF abs(Ox)>0 THEN Iy:= GOY*(Ix/Ox) ELSE Iy:=0;
  IF GOx>GL1 THEN Ovirtual:= True ELSE Ovirtual:=False;
  IF Ix>0 THEN Ivirtual:= True ELSE Ivirtual:=False;
End;

PROCEDURE Cvexvector (x1,y1,x2,y2,C : integer);
VAR
   a,s : REAL;
   l : integer;
BEGIN
   if C=1 then Vector(x1,y1,x2,y2) else
   begin
      line (x1,y1,x2,y2);
      IF (x2<>x1) THEN a:=arcTan((y2-y1)/(x2-x1)) ELSE a:=pi/2;
      IF (x2<x1) OR ((x1=x2) AND (y2<y1)) THEN a:=a+pi;
      s := sqr(1.0*x2-x1)+sqr(1.0*y2-y1);
      IF s>70 THEN l:=6 ELSE l:=round(s+5) DIV 15;
      moveTo (x2,y2);
      lineRel (round(l*cos(5*pi/6-a)),+round(l*sin(5*pi/6-a)));
      moveTo (x2,y2);
      lineRel (round(l*cos(7*pi/6-a)),+round(l*sin(7*pi/6-a)));
      {draw inverted 'arrows' on lenses}
   end;
END;

PROCEDURE Mirrortail (x1,y1,x2,y2,C : integer);
VAR
   a,s : REAL;
   l : integer;
BEGIN
   IF (x2<>x1) THEN a:=arcTan((y2-y1)/(x2-x1)) ELSE a:=pi/2;
   IF (x2<x1) OR ((x1=x2) AND (y2<y1)) THEN a:=a+pi;
   s := sqr(1.0*x2-x1)+sqr(1.0*y2-y1);
   IF s>70 THEN l:=6 ELSE l:=round(s+5) DIV 15;
   moveTo (x2,y2);
   if ((C=-1) and (y2>y1)) or ((C=1) and (y2<y1)) then
      lineRel (round(l*cos(5*pi/6-a)),+round(l*sin(5*pi/6-a)))
   else
      lineRel (round(l*cos(7*pi/6-a)),+round(l*sin(7*pi/6-a)));
END;


Procedure DrawObject(Vx,Vy: Float);
VAR
   xtail, ytail, xhead, yhead : Integer;
Begin
   Map(Vx, 0, xtail, ytail);
   Map(Vx, Vy, xhead, yhead);
   Vector(xtail, ytail, xhead, yhead);
End;

Procedure DrawConvex(Cx,Cy: Float; C:Integer);
VAR
   xtail, ytail, xhead, yhead : Integer;
Begin
   Map(Cx, 0, xtail, ytail);
   Map(Cx, Cy, xhead, yhead);
   CvexVector(xtail, ytail, xhead, yhead, C);
   Map(Cx, -Cy, xhead, yhead);
   CvexVector(xtail, ytail, xhead, yhead, C);
End;

Procedure DrawMirror(Cx,Cy: Float; C:Integer);
VAR
   xtail, ytail, xhead, yhead, x, y, l, step : Integer;
   s : real;
Begin
   Map(Cx, -Cy, xtail, ytail);
   Map(Cx, Cy, xhead, yhead);
   line(xtail, ytail, xhead, yhead);
   s := sqr(1.0*xhead-xtail)+sqr(1.0*yhead-ytail);
   IF s>70 THEN l:=6 ELSE l:=round(s+5) DIV 15;
   for step:=0 to 9 do begin {draw shading behind mirror}
     x:=xtail+step*(xhead-xtail) div 10;
     y:=ytail+step*(yhead-ytail) div 10;
     moveTo (x+1,y+1);
     lineRel (round(l),-round(l));
   end;
   Map(Cx, 0, xtail, ytail);
   Map(Cx, Cy, xhead, yhead);
   MirrorTail(xtail, ytail, xhead, yhead-1, C);
   Map(Cx, -Cy, xhead, yhead);
   MirrorTail(xtail, ytail, xhead, yhead+1, C);
End;

Procedure  DrawFpoints(Fx,Fy,FL: Float);
Begin
  if (LSystem<>Mirror) or (C1=1) then PlotLine(FL-Fx, -Fy, FL-Fx, Fy);
  if (LSystem<>Mirror) or (C1=-1) then PlotLine(Fx+FL, -Fy, Fx+FL, Fy);
End;

Procedure MakeBox(xposn, yposn, size: Float);
VAR
   xsize, ysize : Float;
BEGIN
   xsize := 0.3*size;
   ysize := 0.5*size;
   Plotline(xposn - xsize, yposn - ysize, xposn - xsize, yposn + ysize);
   Plotline(xposn - xsize, yposn + ysize, xposn + xsize, yposn + ysize);
   Plotline(xposn + xsize, yposn + ysize, xposn + xsize, yposn - ysize);
   Plotline(xposn + xsize, yposn - ysize, xposn - xsize, yposn - ysize);
END;

Procedure MakeCross(xposn, yposn, size: Float);
VAR
   xsize, ysize : Float;
BEGIN
   xsize := 0.3*size;
   ysize := 0.5*size;
   Plotline(xposn - xsize, yposn - ysize, xposn + xsize, yposn + ysize);
   Plotline(xposn + xsize, yposn - ysize, xposn - xsize, yposn + ysize);
END;

PROCEDURE DrawCenter(Fx, FL, C1: Float);
BEGIN
   MakeBox(FL-2*C1*Fx, 0, 0.5);
   MakeCross(FL-2*C1*Fx, 0, 0.5);
END;

procedure PlotDottedLine(x1, y1, x2, y2 : Float);
begin
   SetLineStyle(1,0,NormWidth);
   PlotLine(x1, y1, x2, y2); {extrapolated ray back to virtual object}
   SetLineStyle(0,0,NormWidth);
end;

Procedure  Rays(VOx, VOy, VIx, VIy, VL, FP : Float;
                Ovirtual, Ivirtual : Boolean; C : Integer);
Var  YP,M1 :Float;
Begin
    Setcolor(Ray1 XOR GraphBackColor); {ray through center of lens}
    if abs(VOx-VL)>RCheck then M1:=VOy/(VOx-VL) else
    if abs(VIx-VL)>RCheck then M1:=VIy/(VIx-VL) else M1:=0; {slope of incident beam}
    if not Ovirtual then begin
       PlotLine(VOx, VOy, VL, 0); {ray from object to lens}
       if Ivirtual then begin
          PlotLine(VL,0,S[3].xr, -M1*(VL-S[3].xr)); {ray from lens to infinity}
          if VIx<VOx then begin
             PlotDottedLine(VOx, VOy, VIx, VIy); {extrapolated ray back to virtual image}
          end;
       end else
       PlotLine(VL, 0, VIx, VIy); {ray to real image}
    end
    else begin
       PlotLine(0, -M1*VL,VL,0); {ray from infinity to lens}
       if Ivirtual then begin
          PlotLine(VOx, VOy, S[3].xr, -M1*(VL-S[3].xr)); {ray from object to infinity}
          if (VIx>VOx) then PlotLine(VL, 0, VIx, VIy) {ray from lens to image}
          else PlotLine(VL, 0, VOx, VOy); end {ray from lens to object}
       else begin
          PlotLine(VL, 0, VIx, VIy); {ray from lens to image}
          if VIx<VOx then begin
             PlotDottedLine(VIx, VIy, VOx, VOy); {extrapolated ray to virtual object}
          end;
       end;
    end;

    Setcolor(Ray2 XOR GraphBackColor); {parallel ray to lens, through second focus}
    if not OVirtual then PlotLine(VOx, VOy, VL, VOy) {horizontal ray from object to lens}
       else begin
          PlotLine(0, VOy, VL, VOy); {ray from infinity to lens}
          PlotDottedLine(VL, VOy, VOx, VOy); {extrapolated ray to virtual object}
       end;
    if not IVirtual then Plotline(VL, VOy, VIx, VIy) {ray from lens to image}
       else begin
          if abs(FP)>RCheck then
             M1:= -VOy/(C*FP) {slope of ray through focus}
          else M1:=0;
          Plotline(VL, VOy, S[3].xr, -M1*(VL+C*FP-S[3].xr)); {ray from lens to infinity}
          PlotDottedLine(VL, VOy, VIx, VIy); {extrapolated ray back to virtual image}
       end;

    Setcolor(Ray3 XOR GraphBackColor); {ray through first focus}
    if not OVirtual then PlotLine(VOx, VOy, VL, VIy) {ray from object to lens}
       else begin
          if abs(FP)>RCheck then
             M1:= -VIy/(C*FP) {slope of incident ray}
          else M1:=0;
          Plotline(0, M1*(VL-C*FP), VL, VIy); {ray from infinity to lens}
          PlotDottedLine(VL, VIy, VOx, VOy); {extrapolated ray to object}
       end;
    if not IVirtual then Plotline(VL, VIy, VIx, VIy) {ray from lens to image}
       else begin
          PlotLine(VL, VIy, S[3].xr, VIy); {ray from lens to infinity}
          PlotDottedLine(VL, VIy, VIx, VIy); {extrapolated ray back to virtual image}
       end;
End;

Procedure  MirrorRays(VOx,VOy,VIx,VIy,VL,FP:Float;Ovirtual,Ivirtual:Boolean;C:Integer);
Var  YP,M1 :Float;
Begin

  Setcolor(Ray1 XOR GraphBackColor);
  if NRays=5 then begin {ray reflected from center of mirror}
    if abs(VOx-VL)>RCheck then M1:=VOy/(VOx-VL) else {slope of incident beam}
    if abs(VIx-VL)>RCheck then M1:=VIy/(VIx-VL) else M1:=0;
    if not Ovirtual then
       PlotLine(VOx, VOy, VL, 0) {ray from object to mirror}
    else begin
       PlotLine(0, -M1*VL, VL, 0); {ray from infinity to mirror}
       PlotDottedLine(VL, 0, VOx, VOy); {extrapolated ray to virtual object}
    end;
    if not Ivirtual then
       PlotLine(VL, 0, VIx, VIy) {ray from mirror back to real image}
    else begin
       PlotLine(VL, 0, 0, M1*VL); {ray from mirror back to infinity}
       PlotDottedLine(VL, 0, VIx, VIy); {extrapolated ray to virtual image}
    end;
  end else begin {ray through center of curvature of mirror}
    if abs(VOx-2*C*FP)>RCheck then M1:=VOy/(VOx-(VL-2*C*FP)) else
    if abs(VIx-2*C*FP)>RCheck then M1:=VIy/(VIx-(VL-2*C*FP)) else M1:=0; {slope of incident beam}
    if not Ovirtual then begin
       PlotLine(VOx, VOy, VL, 2*M1*C*FP); {ray from object to mirror}
       if Ivirtual then begin
          PlotLine(0, -M1*(VL-2*C*FP), VOx, VOy); {ray from mirror back to infinity}
          PlotDottedLine(VL, 2*M1*C*FP, VIx, VIy); {extrapolated ray to virtual image}
       end
       else if VIx<VOx then
          PlotLine(VOx, VOy, VIx, VIy); {ray from object back to image}
    end
    else begin
       PlotLine(0, -M1*(VL-2*C*FP), VL, 2*M1*C*FP); {ray from infinity to mirror}
       PlotDottedLine(VL, 2*M1*C*FP, VOx, VOy); {extrapolated ray to virtual object}
       if IVirtual and (VIx>VOx) then begin
          PlotDottedLine(VOx, VOy, VIx, VIy); {extrapolated ray to virtual image}
       end;
    end;
  end;

  Setcolor(Ray2 XOR GraphBackColor); {parallel ray to mirror, back through focus}
  if not OVirtual then PlotLine(VOx, VOy, VL, VOy) {horizontal ray from object to mirror}
     else begin
        PlotLine(0, VOy, VL, VOy); {ray from infinity to mirror}
        PlotDottedLine(VL, VOy, VOx, VOy); {extrapolated ray to virtual object}
     end;
  if not IVirtual then Plotline(VL, VOy, VIx, VIy) {ray from mirror back to image}
     else begin
        if abs(FP)>RCheck then
           M1:= -VOy/(C*FP) {slope of ray through focus}
        else M1:=0;
        Plotline(VL, VOy, 0, M1*(VL-C*FP)); {ray from mirror back to infinity}
        PlotDottedLine(VL, VOy, VIx, VIy); {extrapolated ray to virtual image}
     end;

  Setcolor(Ray3 XOR GraphBackColor); {ray through first focus, reflected parallel}
  if not OVirtual then PlotLine(VOx, VOy, VL, VIy) {ray from object to mirror}
     else begin
        if abs(FP)>RCheck then
           M1:= -VIy/(C*FP) {slope of incident ray}
        else M1:=0;
        Plotline(0, M1*(VL-C*FP), VL, VIy); {ray from infinity to mirror}
        PlotDottedLine(VL, VIy, VOx, VOy); {extrapolated ray to object}
     end;
  if not IVirtual then Plotline(VL, VIy, VIx, VIy) {ray from mirror back to image}
     else begin
        PlotLine(VL, VIy, 0, VIy); {ray from mirror back to infinity}
        PlotDottedLine(VL, VIy, VIx, VIy); {extrapolated ray to virtual image}
     end;
End;

Procedure  Rays2(VOx,VOy,VIx,VIy,VIFx,VIFy,VL1,VL2,FP1,FP2:Float;
                 Ovirtual,ILvirtual,IRvirtual:Boolean;C1,C2:Integer);
{called with Rays2(GOx,GOy,GIx,GIy,GIFx,GIFy,GL1,GL2,FP1,FP2,
                 Ovirtual,ILvirtual,IRvirtual,C1,C2);}
Var  M1,M2,YP1,YP2,YQ :Float;
Begin
   Setcolor(Ray1 XOR GraphBackColor); {ray through center of lens 1}
   if abs(VOx-VL1)>RCheck then M1:=VOy/(VOx-VL1) else
   if abs(VIx-VL1)>RCheck then M1:=VIy/(VIx-VL1) else M1:=0; {slope of incident beam}
   YP2:=M1*(VL2-VL1); {y coord where ray strikes lens 2}
   if not Ovirtual then begin
      PlotLine(VOx, VOy, VL1, 0); {ray from object to lens}
      if ILvirtual and (VIx<VOx) then begin
         PlotDottedLine(VOx, VOy, VIx, VIy); {extrapolated ray back to virtual intermediate image}
      end;
   end
   else begin
      PlotLine(0, -M1*VL1,VL1,0); {ray from infinity to lens}
      if VOx>VL2 then begin
         PlotDottedLine(VL2, YP2, VOx, VOy); {extrapolated ray to virtual object}
      end;
   end;
   PlotLine(VL1, 0, VL2, YP2); {ray through center of lens 1 to lens 2}
   if VIx>VL2 then begin
      PlotDottedLine(VL2, YP2, VIx, VIy); {extrapolated ray to 'real' image beyond lens 2}
   end;
   IF IRvirtual THEN Begin
     if abs(VIFx-VL2)>RCheck then
        M1:= (VIFy-YP2)/(VIFx-VL2) {slope of ray from lens 2}
     else M1:=0;
     YQ:=M1*(S[3].xr-VL2) + YP2;
     PlotLine(VL2, YP2, S[3].xr, YQ); {ray from second lens to infinity}
     PlotDottedLine(VL2, YP2, VIFx,VIFy); {extrapolated ray back to virtual final image}
   end
   else PlotLine(VL2, YP2, VIFx, VIFy); {ray to real final image}

   Setcolor(Ray2 XOR GraphBackColor); {parallel ray to lens 1, through second focus}
   if not OVirtual then PlotLine(VOx, VOy, VL1, VOy) {horizontal ray from object to lens}
   else begin
      PlotLine(0, VOy, VL1, VOy); {ray from infinity to lens}
      PlotDottedLine(VL1, VOy, VOx, VOy); {extrapolated ray to virtual object}
   end;
   if abs(FP1)>RCheck then
      M1:= VOy/(C1*FP1) {slope of ray through focus}
   else M1:=0;
   YP2:= M1*(VL1+C1*FP1-VL2); {y coord where ray strikes lens 2}
   Plotline(VL1, VOy, VL2, YP2); {ray from lens 1 through focus to lens 2}
   if ILvirtual then begin
      PlotDottedLine(VL1, VOy, VIx, VIy); {extrapolated ray back to virtual intermediate image}
   end
   else if VIx>VL2 then begin
      PlotDottedLine(VL2, YP2, VIx, VIy); {extrapolated ray to 'real' image beyond lens 2}
   end;
   IF IRvirtual THEN Begin
      if abs(VIFx-VL2)>RCheck then
         M1:= (VIFy-YP2)/(VIFx-VL2) {slope of ray to final image}
      else M1:=0;
      YQ:=M1*(S[3].xr-VL2) + YP2;
      PlotLine(VL2, YP2, S[3].xr, YQ); {ray from second lens to infinity}
      PlotDottedLine(VL2, YP2, VIFx,VIFy); {extrapolated ray back to virtual final image}
   end
   else PlotLine(VL2, YP2, VIFx, VIFy); {ray to real final image}

   Setcolor(Ray3 XOR GraphBackColor); {ray through first focus, parallel between lenses, through second focus of second lens}
   if not OVirtual then PlotLine(VOx, VOy, VL1, VIy) {ray from object to lens}
   else begin
      if abs(FP1)>RCheck then
         M1:= -VIy/(C1*FP1) {slope of ray through focus to lens}
      else M1:=0;
      Plotline(0, M1*(VL1-C1*FP1), VL1, VIy); {ray from infinity to lens}
      PlotDottedLine(VL1, VIy, VOx, VOy); {extrapolated ray to virtual object}
   end;
   Plotline(VL1, VIy, VL2, VIy); {horizontal ray from lens 1 to lens 2}
   if ILvirtual then begin
      PlotDottedLine(VL1, VIy, VIx, VIy); {extrapolated ray back to virtual intermediate image}
   end
   else if VIx>VL2 then begin
      PlotDottedLine(VL2, VIy, VIx, VIy); {extrapolated ray to 'real' image beyond lens 2}
   end;
   if IRVirtual then begin
      if abs(FP2)>RCheck then
         M1:= VIy/(C2*FP2) {slope of ray through focus of lens 2}
      else M1:=0;
      YQ:= M1*(VL2+C2*FP2-S[3].xr);
      Plotline(VL2, VIy, S[3].xr, YQ); {ray from second lens to infinity}
      PlotDottedLine(VL2, VIy, VIFx, VIFy); {extrapolated ray back to virtual final image}
   end
   else Plotline(VL2, VIy, VIFx, VIFy); {ray to real final image}

   IF (Nrays=5) THEN Begin
      Setcolor(Ray4 XOR GraphBackColor); {ray through centre of lens 2}
      if abs(VIFx-VL2)>RCheck then
         M2:=VIFy/(VIFx-VL2) {slope of ray through center of second lens}
      else M2:=0;
      YP1:=-M2*(VL2-VL1); {y coord where ray emerges from lens 1}
      if abs(VOx-VL1)>RCheck then
         M1:=(VOy-YP1)/(VOx-VL1) {slope of ray to lens 1}
      else M1:=0;
      YQ:=-M1*VL1+YP1;
      if not Ovirtual then
         PlotLine(VOx, VOy, VL1, YP1) {ray from object to lens}
      else begin
         PlotLine(0, YQ, VL1, YP1); {ray from infinity to lens}
         PlotDottedLine(VL1, YP1, VOx, VOy); {extrapolated ray to virtual object}
      end;
      if ILvirtual then begin
         PlotDottedLine(VL1, YP1, VIx, VIy); {extrapolated ray back to virtual intermediate image}
      end
      else if not IRvirtual and (VIx>VIFx) then begin
         PlotDottedLine(VIFx, VIFy, VIx, VIy); {extrapolated ray to 'real' image beyond }
      end;
      PlotLine(VL1, YP1, VL2, 0); {ray through lens 1 to center of lens 2}
      IF IRvirtual THEN Begin
         YQ:=M2*(S[3].xr-VL2);
         PlotLine(VL2, 0, S[3].xr, YQ); {ray from second lens to infinity}
         if VIFx<VL1 then begin
            PlotDottedLine(VL1, YP1, VIFx,VIFy); {extrapolated ray back to virtual final image}
         end;
      end
      else PlotLine(VL2, 0, VIFx, VIFy); {ray to real final image}

      Setcolor(Ray5 XOR GraphBackColor); {ray through first focus of lens 2}
      if abs(FP2)>RCheck then
         M2:=VIFy/(C2*FP2) {slope of ray through focus to second lens}
      else M2:=0;
      YP1:=-M2*(VL2-C2*FP2-VL1); {y coord where ray emerges from lens 1}
      if abs(VOx-VL1)>RCheck then
         M1:=(VOy-YP1)/(VOx-VL1) {slope of ray to lens 1}
      else M1:=0;
      YQ:=-M1*VL1+YP1;
      if not OVirtual then PlotLine(VOx, VOy, VL1, YP1) {ray from object to lens}
      else begin
         PlotLine(0, YQ, VL1, YP1); {ray from infinity to lens}
         PlotDottedLine(VL1, YP1, VOx, VOy); {extrapolated ray to virtual object}
      end;
      Plotline(VL1, YP1, VL2, VIFy); {ray from lens 1 through focus to lens 2}
      if ILvirtual then begin
         PlotDottedLine(VL1, YP1, VIx, VIy); {extrapolated ray back to virtual intermediate image}
      end
      else if VIx>VL2 then begin
         PlotDottedLine(VL2, VIFy, VIx, VIy); {extrapolated ray to 'real' image beyond lens 2}
      end;
      IF IRvirtual THEN Begin
         PlotLine(VL2, VIFy, S[3].xr, VIFy); {horizontal ray from second lens to infinity}
         PlotDottedLine(VL2, VIFy, VIFx, VIFy); {extrapolated ray back to virtual final image}
      end
      else PlotLine(VL2, VIFy, VIFx, VIFy); {ray to real final image}
   End;
End;

Procedure  CvexAll1;
Var Px     :Integer;
Begin
  HideMouse;
  SelectViewPort(VNum);
  SetWriteMode(XORput);
  SetColor(ObjectColor XOR GraphBackColor);
  DrawObject(GOx, GOy);
  SetColor(LensColor XOR GraphBackColor);
  DrawConvex(GL1,GLy*Sy,C1);
  SetColor(FP1Color XOR GraphBackColor);
  DrawFPoints(FP1,FPy*Sy,GL1);
  SetColor(ImageColor XOR GraphBackColor);
  DrawObject(GIx, GIy);
  Rays(GOx, GOy, GIx, GIy, GL1, FP1, Ovirtual, ILvirtual,C1);
  SetLineStyle(0,0,1);
  SetWriteMode(COPYput);
  ShowMouse;


  IF Wsize=True THEN Begin
    SelectViewPort(2);
    SetColor(White);
    RubOut(11,2,7,Darkgray); Rubout(32,2,8,Darkgray);
    RubOut(11,3,7,Darkgray); Rubout(32,3,8,Darkgray);
    Rubout(11,4,7,Darkgray);Rubout(32,4,8,Darkgray);
    IF (Pick=FPoint1)OR(Pick=FPoint1B)OR(Pick=Lens1)Or(Pick=Mirror1) THEN Begin
      Rubout(54,2,7,Darkgray);
      Rubout(54,3,7,Darkgray); End;
    IF (Pick=Lens1)Or(Pick=Mirror1) THEN RubOut(71,2,8,DarkGray);
    Print(11,2,+NumStr(GOx,5,3)); Print(11,3,+NumStr(GL1-GOx,5,3));Print(11,4,+NumStr(GOy,5,3));
    Print(32,2,+NumStr(GIx,5,3)); Print(32,3,+NumStr(Ix,5,3));Print(32,4,+NumStr(GIy,5,3));
    Print(54,2,+NumStr(GL1 - FP1,5,3)); Print(54,3,+NumStr(FP1,5,3));
    Print(71,2,+Numstr(GL1,5,3));
  End else Begin
    Setcolor(White);
    Px:=61;
    Rubout(Px,1,7,Black);Rubout(Px,2,7,Black);Rubout(Px,3,7,Black);
    Rubout(Px,4,7,Black);Rubout(Px,5,7,Black);
    Print(Px,1,+NumStr(GL1-GOx,5,3));Print(Px,2,+NumStr(Ix,5,3));
    Print(Px,3,+NumStr(GOy,5,3));Print(Px,4,+NumStr(Iy,5,3));
    Print(Px,5,+NumStr(FP1,5,3));
    Delay(10);
  End;
End;

Procedure CvexAll2;
Begin
  HideMouse;
  SelectViewPort(VNum);
  SetWriteMode(XORput);
  SetColor(ObjectColor XOR GraphBackColor);
  DrawObject(GOx, GOy);
  SetColor(IIcolor XOR GraphBackcolor);
  DrawObject(GIx, GIy);
  SetColor(LensColor XOR GraphBackColor);
  DrawConvex(GL1,GLy*Sy,C1);
  DrawConvex(GL2,GLy*Sy,C2);
  SetColor(FP1Color XOR GraphBackColor);
  DrawFPoints(FP1,FPy*Sy,GL1);
  SetColor(FP2Color XOR GraphBackColor);
  DrawFPoints(FP2,FPy*Sy,GL2);
  SetColor(ImageColor XOR GraphBackColor);
  DrawObject(GIFx, GIFy);
  Rays2(GOx,GOy,GIx,GIy,GIFx,GIFy,GL1,GL2,FP1,FP2,
        Ovirtual,ILvirtual,IRvirtual,C1,C2);

  SetLineStyle(0,0,1);
  SetWriteMode(COPYput);
  ShowMouse;
  IF Wsize=True THEN Begin
    SelectViewPort(2);
    SetColor(White);
    RubOut(11,2,7,Darkgray); Rubout(28,2,7,Darkgray);
    RubOut(11,3,7,Darkgray); Rubout(28,3,7,Darkgray);
    Rubout(11,4,7,Darkgray);Rubout(28,4,7,Darkgray);
    Rubout(46,2,7,Darkgray);
    RubOut(46,3,7,Darkgray); Rubout(28,3,7,Darkgray);
    Rubout(46,4,10,Darkgray);Rubout(28,4,7,Darkgray);
    IF (Pick=FPoint1)or(Pick=FPoint1B) THEN Rubout(62,2,6,Darkgray);
    IF (Pick=Lens1)Or(Pick=Mirror1) THEN Rubout(73,2,7,Darkgray);
    IF (Pick=FPoint2)or(Pick=FPoint2B) THEN Rubout(62,3,6,Darkgray);
    IF Pick=Lens2 THEN Rubout(73,3,7,Darkgray);
    Print(11,2,+NumStr(GOx,5,2));Print(28,2,+NumStr(GIx,5,2));
    Print(11,3,+NumStr(GL1-GOx,5,2));Print(28,3,+NumStr(Ix,5,2));
    Print(11,4,+NumStr(GOy,5,2));Print(28,4,+NumStr(GIy,5,2));
    Print(46,2,+NumStr(GIFx,5,2));Print(62,2,+NumStr(FP1,4,2));
    Print(46,3,+NumStr(IFx,5,2));Print(62,3,+NumStr(FP2,4,2));
    Print(46,4,+NumStr(GIFy,5,2));
    Print(73,2,+NumStr(GL1,5,2));
    Print(73,3,+NumStr(GL2,5,2));
  End else begin     {IF Wsize=False }
    RubOut(52,1,7,Black); Rubout(61,1,7,Black);
    RubOut(52,2,7,Black); Rubout(61,2,7,Black);
    Rubout(52,3,7,Black); Rubout(61,3,7,Black);
    Rubout(46,4,5,Black);
    RubOut(61,4,7,Black);
    RubOut(61,5,5,Black);
    SetColor(White);
    Print(52,1,+NumStr(GL1-GOx,5,2)); Print(61,1,+NumStr(GOy,5,2));
    Print(52,2,+NumStr(Ix,5,2)); Print(61,2,+NumStr(GIy,5,2));
    Print(52,3,+NumStr(IFx,5,2));Print(61,3,+NumStr(GIFy,5,2));
    Print(46,4,+NumStr(FP1,5,2));
    Print(61,4,+NumStr(FP2,5,2));
    Print(61,5,+NumStr(ABS(GL1-GL2),5,2));
  End;

  IF Wsize=False THEN Delay(10) ELSE Delay(5);

End;

Procedure  MirrorAll;
Var Px     :Integer;
Begin
  HideMouse;
  SelectViewPort(VNum);
  SetWriteMode(XORput);
  SetColor(ObjectColor XOR GraphBackColor);
  DrawObject(GOx, GOy);
  SetColor(LensColor XOR GraphBackColor);
  DrawMirror(GL1,GLy*Sy,C1);
  SetColor(FP1Color XOR GraphBackColor);
  DrawFPoints(FP1,FPy*Sy,GL1);
  DrawCenter(FP1,GL1,C1);
  SetColor(ImageColor XOR GraphBackColor);
  DrawObject(GIx, GIy);
  MirrorRays(GOx, GOy, GIx, GIy, GL1, FP1, Ovirtual, ILvirtual,C1);
  SetLineStyle(0,0,1);
  SetWriteMode(COPYput);
  ShowMouse;


  IF Wsize=True THEN Begin
    SelectViewPort(2);
    SetColor(White);
    RubOut(11,2,7,Darkgray); Rubout(32,2,8,Darkgray);
    RubOut(11,3,7,Darkgray); Rubout(32,3,8,Darkgray);
    Rubout(11,4,7,Darkgray);Rubout(32,4,8,Darkgray);
    IF (Pick=FPoint1)or(Pick=FPoint1B)OR(Pick=Lens1)Or(Pick=Mirror1) THEN Begin
      Rubout(54,2,7,Darkgray);
      Rubout(54,3,7,Darkgray); End;
    IF (Pick=Lens1)Or(Pick=Mirror1) THEN RubOut(71,2,8,DarkGray);
    Print(11,2,+NumStr(GOx,5,3)); Print(11,3,+NumStr(GL1-GOx,5,3));Print(11,4,+NumStr(GOy,5,3));
    Print(32,2,+NumStr(GIx,5,3)); Print(32,3,+NumStr(Ix,5,3));Print(32,4,+NumStr(GIy,5,3));
    Print(54,2,+NumStr(GL1 - FP1,5,3)); Print(54,3,+NumStr(FP1,5,3));
    Print(71,2,+Numstr(GL1,5,3));
  End else Begin     {If Wsize=False THEN }
    Setcolor(White);
    Px:=61;
    Rubout(Px,1,7,Black);Rubout(Px,2,7,Black);Rubout(Px,3,7,Black);
    Rubout(Px,4,7,Black);Rubout(Px,5,7,Black);
    Print(Px,1,+NumStr(GL1-GOx,5,3));Print(Px,2,+NumStr(Ix,5,3));
    Print(Px,3,+NumStr(GOy,5,3));Print(Px,4,+NumStr(Iy,5,3));
    Print(Px,5,+NumStr(FP1,5,3));
    Delay(10);
  End;
End;

Procedure Showall;
begin
  Case Lsystem OF
    Convex1: CvexAll1;
    Convex2: CvexAll2;
    Mirror : MirrorAll;
   End;
End;

Procedure  Refresh;
Begin
  Clearup(VNum);
  Showall;
  InfoWindow;
  If FT then SetUpSliders(Sliders, DragButtons, Pick);
End;

Procedure  CvexSetUp1;
Begin
  GOx:= 5;
  GL1:= 10;
  FP1:= 2.5;
  GOY:= 5;
  GLy:= 5;
  OY:= GOY;
  FPy:= 1*Sy;
  Snum:=1;
  WITH S[Snum] DO  Begin
    xl:= 0.0; xr:= 20; yb:= -10; yt:= 10;
    DefineScale(1, xl, xr, yb, yt);
    DefineScale(3, xl, xr, yb, yt);
  End;
  S[3]:= S[1];
  SetUpMenu(Menu);

  CvexImage(Ix, Iy, Ovirtual, ILvirtual);
  GIy:= Iy;
  GIx:= Ix + GL1;

End;


Procedure CvexSetUp2;
Begin
  GOx:=4.0; GOy:=5.0; GL1:=10.0; GL2:=30.0;
  FP1:=2.5; FP2:= 5; GLy:= 10; FPy:= 3;
  Snum:=2;
  WITH S[Snum] DO Begin
    xl:= 0.0; xr:= 40; yb:= -20; yt:= 20;
    DefineScale(1, xl, xr, yb, yt);
    DefineScale(3, xl, xr, yb, yt);
  End;
  S[3]:= S[2];
  SetUpMenu(Menu);

  CVexImage(Ix, Iy, Ovirtual, ILvirtual);
  GIy:= Iy;
  GIx:= Ix + GL1;
  FImage(IFx, IFy, IRvirtual);
  GIFx:= IFx+GL2;
  GIFy:= IFy;

{  CvexAll2;  }

End;

Procedure Microscope;
Begin
  GOx:=7.07; GOy:=-2.44; GL1:=10.0; GL2:=20.0;
  FP1:=2; FP2:= 5; GLy:= 10; FPy:= 3;
  Snum:=2;
  WITH S[Snum] DO Begin
    xl:= 0.0; xr:= 30; yb:= -20; yt:= 20;
    DefineScale(1, xl, xr, yb, yt);
    DefineScale(3, xl, xr, yb, yt);
  End;
  S[3]:= S[2];
  SetUpMenu(Menu);

  CVexImage(Ix, Iy, Ovirtual, ILvirtual);
  GIy:= Iy;
  GIx:= Ix + GL1;
  FImage(IFx, IFy, IRvirtual);
  GIFx:= IFx+GL2;
  GIFy:= IFy;

{  CvexAll2;}

End;

Procedure Telescope;
Begin
  GOx:=-440; GOy:=10; GL1:=60.0; GL2:=91.3;
  FP1:=25; FP2:= 5; GLy:= 10; FPy:= 3;
  Snum:=2;
  WITH S[Snum] DO Begin
    xl:= 0.0; xr:= 120; yb:= -20; yt:= 20;
    DefineScale(1, xl, xr, yb, yt);
    DefineScale(3, xl, xr, yb, yt);
  End;
  S[3]:= S[2];
  SetUpMenu(Menu);

  CVexImage(Ix, Iy, Ovirtual, ILvirtual);
  GIy:= Iy;
  GIx:= Ix + GL1;
  FImage(IFx, IFy, IRvirtual);
  GIFx:= IFx+GL2;
  GIFy:= IFy;

{  CvexAll2;}

End;

Procedure OatInf;
Var  Mid :Float;
Begin
 Mid:= S[3].xl+Trunc(ABS(S[3].xl-S[3].xr)/2);
 GOx:=Mid-510;
 SetUpMenu(Menu);
 Case Lsystem of
  Convex1:
  Begin
    CvexImage(Ix, Iy, Ovirtual, ILvirtual);
    GIy:= Iy;
    GIx:= Ix + GL1;
  End;

  Convex2:
  Begin
    CVexImage(Ix, Iy, Ovirtual, ILvirtual);
    GIy:= Iy;
    GIx:= Ix + GL1;
    FImage(IFx, IFy, IRvirtual);
    GIFx:= IFx+GL2;
    GIFy:= IFy;
  End;

  Mirror:
  Begin
    MirrorImage(Ix, Iy, Ovirtual, ILvirtual);
    GIy:= Iy;
    GIx:= Ix + GL1;
  End;
 End;
End;

Procedure  MirrorSetUp;
Begin
  GOx:= 2.5;
  GL1:= 10;
  FP1:= 2.5;
  GOY:= 5;
  GLy:= 5;
  OY:= GOY;
  FPy:= 1*Sy;
  Snum:=1;
  WITH S[Snum] DO  Begin
    xl:= 0.0; xr:= 20; yb:= -10; yt:= 10;
    DefineScale(1, xl, xr, yb, yt);
    DefineScale(3, xl, xr, yb, yt);
  End;
  S[3]:= S[1];
  SetUpMenu(Menu);

  MirrorImage(Ix, Iy, Ovirtual, ILvirtual);
  GIy:= Iy;
  GIx:= Ix + GL1;

{  MirrorAll;}

End;

procedure Rcursor(a, b : PointType); far;
begin
   line(a.x, a.y, a.x, b.y);
   line(a.x, b.y, b.x, b.y);
   line(b.x, b.y, b.x, a.y);
   line(b.x, a.y, a.x, a.y);
end;

procedure VariableZoom;
var x,y : real;
    gx,gy,button : Integer;
    Inside,LFlag : Boolean;
    a, b : PointType;
    Quit : boolean;
begin
  with S[3] do begin
     SelectViewport(VNum);
     Message('Click and drag with mouse to mark Zoom area');
     repeat
        MouseGlobalPosn(gx,gy,Button);
     until button<>0;
     MousePosn(x,y,Vnum,1,Inside);
     a.x:=gx-round(Sxl*GetMaxX);
     a.y:=gy-round((1-Sty)*GetMaxY);
     xl:=x; yt:=y;
     SetColor(LightGray);
     SetLineStyle(DashedLn,0,NormWidth);
     DrawRubberBand(Rcursor, a, b, GraphBackColor, true);
     MousePosn(xr,yb,VNum,1,Inside);
     Quit := ((xl=xr) or (yt=yb));
     if not Quit then
     begin
        if xl>xr then begin x:=xl; xl:=xr; xr:=x; end;
        if yt<yb then begin y:=yt; yt:=yb; yb:=y; end;
        DefineScale(1,xl,xr,yb,yt);
        DefineScale(3,xl,xr,yb,yt);
        GLy:= (yt - yb)/4;
        FPy:= (yt - yb)/20;
     end else beep;
     SetLineStyle(SolidLn,0,NormWidth);
  end;
  Refresh;
End;

Procedure  ZoomIn;
Var Gc, Gyc, Gx, Gy : Float;
Begin
  WITH S[3] DO
  Begin
    Gc:= (xr + xl)/2; Gx:=xr - xl;
    Gyc:= (yb + yt)/2; Gy:=yt - yb;
    xl:= Trunc(Gc - Gx/4); xr:= Trunc(Gc+ Gx/4);
    yb:= Trunc(Gyc - Gy/4); yt:= Trunc(Gyc + Gy/4);
    DefineScale(1,xl,xr,yb,yt);
    DefineScale(3,xl,xr,yb,yt);
    GLy:= Gy/8;
    FPy:= Gy/40;
  End;
  Refresh;
End;

Procedure  ZoomOut;
Var Gc, Gyc, Gx, Gy : Float;
Begin
  WITH S[3] DO
  Begin
    Gc:= (xr + xl)/2; Gx:=xr - xl;
    Gyc:= (yb + yt)/2; Gy:=yt - yb;
    xl:= Gc - Gx; xr:= Gc+ Gx;
    yb:= Gyc - Gy; yt:= Gyc + Gy;
    DefineScale(1,xl,xr,yb,yt);
    DefineScale(3,xl,xr,yb,yt);
    GLy:= Gy/2;
    FPy:= Gy*0.1;
  End;
  Refresh;
End;

Procedure HZoomIn;
Var Gc, Gx : Float;
Begin
  WITH S[3] DO
  Begin
    Gc:= (xr + xl)/2; Gx:=xr - xl;
    xl:= Trunc(Gc - Gx/4); xr:= Trunc(Gc+ Gx/4);
    DefineScale(1,xl,xr,yb,yt);
    DefineScale(3,xl,xr,yb,yt);
  End;
  Refresh;
End;

Procedure HZoomOut;
Var Gc, Gx : Float;
Begin
  WITH S[3] DO
  Begin
    Gc:= (xr + xl)/2; Gx:=xr - xl;
    xl:= Gc - Gx; xr:= Gc+ Gx;
    DefineScale(1,xl,xr,yb,yt);
    DefineScale(3,xl,xr,yb,yt);
  End;
  Refresh;
End;

Procedure VZoomIn;
Var Gyc, Gy : Float;
Begin
  WITH S[3] DO
  Begin
    Gyc:= (yb + yt)/2; Gy:=yt - yb;
    yb:= Trunc(Gyc - Gy/4); yt:= Trunc(Gyc + Gy/4);
    DefineScale(1,xl,xr,yb,yt);
    DefineScale(3,xl,xr,yb,yt);
    GLy:= Gy/8;
    FPy:= Gy/40;
  End;
  Refresh;
End;

Procedure VZoomOut;
Var Gyc, Gy : Float;
Begin
  WITH S[3] DO
  Begin
    Gyc:= (yb + yt)/2; Gy:=yt - yb;
    yb:= Gyc - Gy; yt:= Gyc + Gy;
    DefineScale(1,xl,xr,yb,yt);
    DefineScale(3,xl,xr,yb,yt);
    GLy:= Gy/2;
    FPy:= Gy*0.1;
  End;
  Refresh;
End;

Procedure HPAN(one:Integer);
Var mv : Real;
Begin
  WITH S[3] DO
  Begin
    mv:= one*(xr-xl)/2;
    xl:= xl + mv; xr:= xr + mv;
    DefineScale(1,xl,xr,yb,yt);
    DefineScale(3,xl,xr,yb,yt);
  End;
  Refresh;
end;

Procedure VPAN(one:Integer);
Var mv : Real;
Begin
  WITH S[3] DO
  Begin
    mv:= one*(yt-yb)/2;
    yb:= yb + mv; yt:= yt + mv;
    DefineScale(1,xl,xr,yb,yt);
    DefineScale(3,xl,xr,yb,yt);
  End;
  Refresh;
end;

Procedure  CenterSystem;
Var Gc, Gyc : Float;
Begin
 WITH S[3] DO
 Begin
    Gc:= (xr + xl)/2;
    Gyc:= (yb + yt)/2;
    yt:=yt - Gyc; yb:=yb - Gyc;
    case Lsystem of
     Convex1:
     Begin
       Gc:= Gc - GL1;
     End;
     convex2:
     Begin
       Gc:= Gc - (GL1 + ABS(GL1-GL2)/2);
     End;
     Mirror:
    Begin
       Gc:= Gc - GL1;
     End;
    End;
    xl:=xl - Gc; xr:=xr - Gc;
    DefineScale(1,xl,xr,yb,yt);
    DefineScale(3,xl,xr,yb,yt);
 End;
 Refresh;
End;

Procedure SetAxes;
Var  Iscreen      : TInputScreen;
     NScale       : Boolean;
Begin
  NScale:=True;
  WITH IScreen DO
  Begin
    Init;
    DefineInputport(0.25, 0.7, 0.35, 0.7);
    LoadLine('SET GLOBAL LIMITS FOR LENS WINDOW');
    LoadLine('');
    LoadLine('  LHS  x = {      }');
    LoadLine('  RHS  x = {      }');
    LoadLine('  Low  y = {      }');
    LoadLine('  High y = {      }');
    LoadLine('');
    LoadLine('    [  Ok  ]   [Cancel] ');
    WITH S[3] DO
    Begin
      SetNumber(1, xl); Setnumber(2, xr);
      SetNumber(3, yb); Setnumber(4, yt);
    End;
    AcceptScreen;
    WITH S[3] DO
    Begin
      xl:= Trunc(GetNumber(1)); xr:= Trunc(GetNumber(2));
      yb:= Trunc(GetNumber(3)); yt:= Trunc(GetNumber(4));

      IF (xr<=xl)OR(yb>=yt) THEN NScale:=False;

      IF NOT(Canceled)AND(NScale=True) THEN Begin
        DefineScale(1,xl,xr,yb,yt);
        DefineScale(3,xl,xr,yb,yt);
        Refresh;
      End;
    End;
    Done;
  End;
End;

Procedure  Windowsize;
Begin
  Wsize := not Wsize;
  CloseViewPort(1); CloseViewPort(2); CloseViewPort(3);
  SetUpHotkeys(HotKeys);
  Refresh;
End;

procedure ReadDataScreen(var EscPressed : boolean);
var OldGL1, OldGL2 : Float;
    DataScreen : TInputScreen;
begin
  {MAC only
   New(DataScreen);}
   with DataScreen do begin
      init;
      if LSystem=Convex2 then
         DefineInputPort (0.15, 0.85, 0.06, 0.55)
      else DefineInputPort (0.15, 0.85, 0.06, 0.45);
      LoadLine('SET DATA');
      LoadLine('   Object Height                         : {       }');
      LoadLine('   Object Distance (from lens or mirror) : {       }');
      LoadLine('     - may be negative for virtual object');
      if LSystem=Convex2 then begin
         LoadLine('   Position of Lens 1                    : {       }');
         LoadLine('   Focal Length of Lens 1                : {       }');
         LoadLine('     - negative for diverging lens/mirror');
         LoadLine('   Position of Lens 2                    : {       }');
         LoadLine('   Focal Length of Lens 2                : {       }');
      end else begin
         LoadLine('   Lens position                         : {       }');
         LoadLine('   Focal Length                          : {       }');
         LoadLine('     - negative for diverging lens/mirror');
      end;
      LoadLine(' ');
      LoadLine('     [  Ok  ]      [ Help ]      [Cancel]');
      SetNumber(1,GOy);  {use old values as defaults}
      SetNumber(2,GL1-GOx);
      SetNumber(3,GL1);
      SetNumber(4,C1*FP1);
      if LSystem=Convex2 then begin
         SetNumber(5,GL2);
         SetNumber(6,C2*FP2);
      end;
      SetHelpFile('TwoLens.hlp','set data help');
      AcceptScreen;
      if Canceled then
         EscPressed := true
      else begin
         EscPressed := false;
         OldGL1:=GL1; GL1 := GetNumber(3);
         GOy := GetNumber(1);
         GOx := GL1 - GetNumber(2);
         FP1 := GetNumber(4);
         if FP1<0 then begin FP1:=-FP1; C1:=-1; end else C1:=1;
         if LSystem=Convex2 then begin
            OldGL2:=GL2; GL2 := GetNumber(5);
            if GL2<GL1 then begin
               announce('Attempting to interchange lenses');
               GL1:=OldGL1; GL2:=OldGL2; GOx:=GOx+OldGL1-GL1;
            end;
            FP2 := GetNumber(6);
            if FP2<0 then begin FP2:=-FP2; C2:=-1; end else C2:=1;
         end;

         if LSystem=Mirror then begin
            MirrorImage(Ix, Iy, Ovirtual, ILvirtual);
            GIy:=  Iy;
            GIx:= Ix + GL1;
         end else begin
            CvexImage(Ix, Iy, Ovirtual, ILvirtual);
            GIy:=  Iy;
            GIx:= Ix + GL1;
         end;

         If LSystem=Convex2 then
           Begin FImage(IFx, IFy, IRvirtual); GIFx:=IFx+GL2; GIFy:=IFy; End;
         Showall;

      end;
      done;
   end;
   {MAC only
    Dispose(DataScreen);}

end;


Procedure  HandleMenu(Menu : TMenu);
BEGIN
  WITH Menu DO
  BEGIN
    CASE ColChosen OF
      1: begin
          HelpContext := fileitem;
          Case RowChosen OF
           1: AboutCups;
           2: begin
                Intro;
                ShowHelp('twolens.HLP','general help');
              end;
           3: Configuration;
           5: QuitFlag := true;
          End;
         end;

      2: begin
          HelpContext := lenstypes;
          Case RowChosen OF
           1: OatInf;
           3: Begin LSystem:= Convex1; C1:=1; CvexSetup1; End;
           4: Begin LSystem:= Convex1; C1:=-1;CvexSetup1; End;
           5: Begin Lsystem:= Convex2; C1:=1; C2:=1; CvexSetup2; End;
           6: Begin Lsystem:= Convex2; C1:=-1; C2:=-1; CvexSetup2; End;
           7: Begin Lsystem:= Convex2; C1:=1; C2:=-1; CvexSetup2; End;
           8: Begin Lsystem:= Convex2; C1:=-1; C2:=1; CvexSetup2; End;
           9: Begin Lsystem:= Convex2; C1:=1; C2:=1; Microscope; End;
           10: Begin Lsystem:= Convex2; C1:=1; C2:=1; Telescope; End;
           12: Begin LSystem:= Mirror; C1:=1; MirrorSetup; End;
           13: Begin LSystem:= Mirror; C1:=-1; MirrorSetup; End;
          End;
          if FT then begin
             FT:=false;
             SetUpMenu(Menu);{ SetupHotKeys(HotKeys);}
          end;
          Refresh;
         End;

      3: begin
          HelpContext := raypaths;
          Case Rowchosen OF
           1: NRays:=3;
           2: NRays:=5;
          End;
          Refresh;
         end;

      4: begin
          HelpContext := zoom;
          Case RowChosen OF
           1: SetAxes;
           3: HZoomIn;
           4: VzoomIn;
           5: ZoomIn;
           6: VariableZoom;
           8: HZoomOut;
           9: VZoomOut;
           10: ZoomOut;
           12: HPAN(-1);
           13: HPAN(+1);
           14: VPAN(+1);
           15: VPAN(-1);
          End;
         end;

      5: begin
          HelpContext := finetune;
          FT:=True;
          Case RowChosen OF
           1: Pick:=Obj;
           2: Pick:=FPoint1;
           3: Pick:=Fpoint2;
           4: Pick:=Lens1;
           5: Pick:=Lens2;
           6: FT:=False;
          End;
          SetUpMenu(Menu); {SetupHotKeys(HotKeys);}
          Refresh;
         End;

      6: begin
           HelpContext := setdata;
           ReadDataScreen(EscPressed);
           if not EscPressed then Refresh;
         end;
    End;
  End;
End;

Procedure  HandleHotKeys(Key: Byte);
BEGIN
    if key>1 then HelpContext := display;
    CASE key OF
      1: case HelpContext of
          fileitem    : ShowHelp('twolens.HLP','file help');
          lenstypes   : ShowHelp('twolens.HLP','lenstypes help');
          raypaths    : ShowHelp('twolens.HLP','raypaths help');
          zoom        : ShowHelp('twolens.HLP','zoom help');
          finetune    : ShowHelp('twolens.HLP','finetune help');
          setdata     : ShowHelp('twolens.HLP','set data help');
          display     : ShowHelp('twolens.HLP','display help');
          general     : ShowHelp('twolens.HLP','general help');
          end{case};

      2: Windowsize;
      3: CenterSystem;
      4: VariableZoom;
      5: Zoomout;
      6: IF Menu.Chosen THEN HandleMenu(Menu);
    End;
END;

Procedure HandleSliders;
var VL1, VL2 : Float;
Begin
  SelectViewPort(Vnum);
  SetWriteMode(XORPut);
  ShowAll;
  VL1:=GL1; VL2:=GL2;
  Case Pick OF
    Obj: Begin GOx:= GL1-Sliders.value(1); GOy:= Sliders.value(2); End;
    Fpoint1:  FP1:= Sliders.value(1);{shouldn't happen}
    Fpoint2:  FP2:= Sliders.value(1);{shouldn't happen}
    Fpoint1B: FP1:= Sliders.value(1);
    Fpoint2B: FP2:= Sliders.value(1);
    Lens1:    VL1:= Sliders.value(1);
    Lens2:    VL2:= Sliders.value(1);
    Mirror1:  GL1:= Sliders.value(1);
  End;
  if (VL1<VL2) or (LSystem<>Convex2) then begin
     GL1:=VL1;  GL2:=VL2;
  end
  else beep;

  if LSystem=Mirror then begin
     MirrorImage(Ix, Iy, Ovirtual, ILvirtual);
     GIy:= Iy;
     GIx:= Ix + GL1;
  end else begin
     CvexImage(Ix, Iy, Ovirtual, ILvirtual);
     GIy:= Iy;
     GIx:= Ix + GL1;
  end;

  If Lsystem=Convex2 then
    Begin FImage(IFx, IFy, IRvirtual); GIFx:=IFx+GL2; GIFy:=IFy; End;
  ShowAll;
End;

Procedure  DragWhat;
Var   Dobx,Dfp1   : Float;
      Dfp2,Dl1    : Float;
      Dl2         : Float;
      DFP1B,DFP2B : Float;
      GFP1,GFP2   : Float;
      x,y         : Real;
      Inside      : Boolean;
Begin
 GFP1:= GL1 - FP1;
 GFP2:= GL2 - FP2;
 MousePosn(x,y,1,1,Inside);
 Case Lsystem OF
 Convex1:
  Begin
    Dobx:= ABS(x-GOx);
    DFP1:= ABS(x-GFP1);
    DFP1B:=ABS(x-GL1-(GL1-GFP1));
    Dl1:= ABS(x-GL1);
    IF (DFP1<DOBx)AND(Dfp1<Dl1)AND(DFP1<DFP1B) THEN Pick:= Fpoint1 ELSE
    IF (DFP1B<DOBx)AND(Dfp1B<Dl1)AND(DFP1B<DFP1) THEN Pick:= Fpoint1B ELSE
    IF (Dl1<Dfp1)AND(dl1<Dobx)AND(DL1<DFP1B) THEN Pick:= Lens1 ELSE
    Pick:= Obj;
  End;
 Convex2:
  Begin
    Dobx:= ABS(x-GOx); Dfp1:= ABS(x-GFP1);
    Dfp2:= ABS(x-GFP2); Dl1:= ABS(x-GL1);
    Dl2:= ABS(x-GL2);
    DFP1B:=ABS(x-GL1-(GL1-GFP1)); DFP2B:=ABS(x-GL2-(GL2-GFP2));
    IF (Dfp1 <Dobx)AND(dfp1 <dL1) AND(Dfp1 <dl2) AND(dfp1 <dfp2)AND(DFP1 <DFP1B)AND(DFP1 <DFP2B)THEN Pick:=Fpoint1 ELSE
    IF (Dfp1B<Dobx)AND(dfp1B<dL1) AND(Dfp1B<dl2) AND(dfp1B<dfp2)AND(DFP1B<DFP1) AND(DFP1B<DFP2B)THEN Pick:=Fpoint1B ELSE
    IF (Dl1  <Dobx)AND(dl1  <dfp1)AND(Dl1  <dl2) AND(dl1  <dfp2)AND(Dl1  <DFP1B)AND(Dl1  <DFP2B)THEN Pick:=Lens1 ELSE
    IF (Dfp2 <Dobx)AND(dfp2 <dL1) AND(Dfp2 <dl2) AND(dfp2 <dfp1)AND(DFP2 <DFP1B)AND(DFP2 <DFP2B)THEN Pick:=Fpoint2 ELSE
    IF (Dfp2B<Dobx)AND(dfp2B<dL1) AND(Dfp2B<dl2) AND(dfp2B<dfp2)AND(DFP2B<DFP1B)AND(DFP2B<DFP1) THEN Pick:=Fpoint2B ELSE
    IF (Dl2  <Dobx)AND(dl2  <dL1) AND(Dl2  <dfp1)AND(dl2  <dfp2)AND(Dl2  <DFP1B)AND(Dl2  <DFP2B)THEN Pick:=Lens2 ELSE
    Pick:= Obj;
  End;
 Mirror:
  Begin
    Dobx:= ABS(x-GOx);
    DFP1:= ABS(x-GFP1);
    DFP1B:=ABS(x-GL1-(GL1-GFP1));
    Dl1:= ABS(x-GL1);
    IF (DFP1<DOBx)AND(Dfp1<Dl1)AND(DFP1<DFP1B) THEN Pick:= Fpoint1 ELSE
    IF (DFP1B<DOBx)AND(Dfp1B<Dl1)AND(DFP1B<DFP1) THEN Pick:= Fpoint1B ELSE
    IF (Dl1<Dfp1)AND(dl1<Dobx)AND(DL1<DFP1B) THEN Pick:= Mirror1 ELSE
    Pick:= Obj;
  End;
 End;
End;

Procedure  Drag;
Var  x,y,xold,yold: Real;
     gx,gy,button : Integer;
     Inside,LFlag : Boolean;
     FP           : Float;

Begin
  Dragwhat;
  xold:=0;yold:=0;
  Repeat
    MouseGlobalPosn(gx,gy,Button);
    MousePosn(x,y,VNum,1,Inside);

    if (x<>xold)or (y<>yold) then begin
       Showall;

       CASE Pick OF
         Fpoint1 : begin
                      FP1:= GL1-x;
                      if FP1<0 then begin
                         FP1:=-FP1; C1:=-C1;
                         Pick:=Fpoint1B;
                      end;
                   end;
         Fpoint2 : begin
                      FP2:= GL2-x;
                      if FP2<0 then begin
                         FP2:=-FP2; C2:=-C2;
                         Pick:=Fpoint2B;
                      end;
                   end;
         Fpoint1B: begin
                      FP1:= x-GL1;
                      if FP1<0 then begin
                         FP1:=-FP1; C1:=-C1;
                         Pick:=Fpoint1;
                      end;
                   end;
         Fpoint2B: begin
                      FP2:= x-GL2;
                      if FP2<0 then begin
                         FP2:=-FP2; C2:=-C2;
                         Pick:=Fpoint2;
                      end;
                   end;
         Obj     : Begin GOx:= x;  GOy:= y; End;
         Lens1   : if (LSystem<>Convex2) or (x<GL2) then GL1:= x else beep;
         Lens2   : if x>GL1 then GL2:= x else beep;
         Mirror1 : GL1:= x;
       End;

       if LSystem=Mirror then begin
          MirrorImage(Ix, Iy, Ovirtual, ILvirtual);
          GIy:=  Iy;
          GIx:= Ix + GL1;
       end else begin
          CvexImage(Ix, Iy, Ovirtual, ILvirtual);
          GIy:=  Iy;
          GIx:= Ix + GL1;
       end;

       If LSystem=Convex2 then
         Begin FImage(IFx, IFy, IRvirtual); GIFx:=IFx+GL2; GIFy:=IFy; End;
       Showall;
    end;
    xold:=x;yold:=y;
  Until (Button=0);
End;

Procedure  Initialize;
Begin
  Sliders.init;
  DragButtons.init;
  FT:= False;
  Wsize:= True;
  QuitFlag := false;
  SetUpMenu(Menu);
  Menu.AutoChecking(2,3);
  SetUpHotKeys(HotKeys);
  VNum:=1; Snum:=1;
  NRays:= 3;
  Sy:=1;
  C1:=1; C2:=1;
  Lsystem:= Convex1;
  HelpContext := general;
  DefineViewPort(1, Sxl, Sxr, Sby, Sty);
  DefineViewPort(2,0.0, 1.0, 0.08, 0.27);
  DefineViewPort(3, Sxl, Sxr, 0.165,0.94);
  DefineViewPort(10, 0.0, 1.0, 0.0, 1.0);
  DefineScale(1, 0, 20, -10, 10);
  DefineScale(2, 0, 40, -30, 30);
  CvexSetUp1;
  Refresh;
end;


VAR
 key   :Byte;

BEGIN { Main Program }
  CUPSinit;
  Initialize;
  Intro;

  REPEAT      { main program loop }
    CheckForEvents;
    IF Menu.Activated THEN HandleMenu(Menu);
    IF HotKeys.Pressed(key) THEN HandleHotKeys(key);
    IF FT and Sliders.Changed THEN HandleSliders;
    IF FT=True THEN BEGIN
       IF DragButtons.Changed and (DragButtons.number=1) then begin
          FT:=False;
          SetUpMenu(Menu); {SetupHotKeys(HotKeys);}
          Clearup(VNum); InfoWindow;
          ShowAll;
       end;
    END ELSE
       IF ClickedInVIewPort(VNum) THEN Drag;
  UNTIL QuitFlag; { main program loop }
  Menu.Done;
  HotKeys.Done;
  sliders.Done;
  DragButtons.Done;
  CUPSdone;
END.  { Main Program }
