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

UNIT srminkx;

INTERFACE
USES Crt, Dos, Graph, CUPSmupp, CUPS, CUPSgrph, CUPSfunc,CUPSproc, CUPSgui,
        Objects;

TYPE

   Point = Record
      x,y: Integer;
   End;

 TRegion = Record
   x,y: Real;
   Length, Height: Real;
   END;

Region = Object

      UpLeft,LowRt: Point;

      FUNCTION PtInRegion(TestPoint: Point): Boolean;
      PROCEDURE Draw(Color: Integer);
   End;

   TSpaceVector = Object

   x,y,z: Real;
   Gamma,beta2: Real;
   END;

   TKey = Object

   x,y: Real;
   KeyType: Integer;
   KeyRegion: Region;
   Length: Real;
   KeyLabel: String;
   Key: Char;
   Defined: Boolean;
   Selected: Boolean;
   Selectable: Boolean;
   Holdable: Boolean;
   Group: Byte;
   Active: Boolean;
   PROCEDURE Erase;
   PROCEDURE Draw;
   PROCEDURE Depress;
   PROCEDURE Release;
   PROCEDURE HighLight;
   PROCEDURE UnHighLight;
   PROCEDURE Activate;
   PROCEDURE Inactivate;
   PROCEDURE Create(kx,ky: Real;KLabel: String;KLength: Real;
                                  CanSelect,CanHold: Boolean; GroupNum: Byte);
            
   END;

   CONST
      TMax = 1;
      Tmin = -1;

      Decimal = '.';
      Minus = '-';
      RightArrow = char(213);
      LeftArrow = char(214);
      BackSpace = char(8);
      Del = char(210);

      DeskTopColor = 1;

      XYTGraph = 3;
      XYGraph = 2;
      XTGraph = 1;
      XYZGraph = 4;
      XYTPrimeGraph = 7;
      XYPrimeGraph = 6;
      XTPrimeGraph = 5;
      XYZPrimeGraph = 8;



    VAR
       Menu: TMenu;
       HotKeys: THotKeys;
       EditChars: SET OF Char;
       Digits: SET OF '0'..'9';


PROCEDURE SetMinkowskiPalette;
PROCEDURE StartUpScreen;
PROCEDURE Initialize;
PROCEDURE LorentzTransform(t,x,y,z: Real; VAR tt,xx,yy,zz: Real;
                                                       Beta: TSpaceVector);
PROCEDURE InvLorentzTransform(t,x,y,z: Real; VAR tt,xx,yy,zz: Real;
                                                       Beta: TSpaceVector);
PROCEDURE DisplayPrompt(Number: Byte);
PROCEDURE LabelAxes(GraphNum, GraphType: Integer);
PROCEDURE Draw3DBox(CRegion: TRegion);
PROCEDURE InitializePrompt;
PROCEDURE ShowHelp(currentHelp: Byte);
PROCEDURE DisplayWrite;
FUNCTION MyNum2Str(num : real; size : integer) : string;

IMPLEMENTATION
CONST

      MBlack = 0;
      MCyan = 1;
      MMagenta = 2;
      MDarkGray = 3;
      MYellow = 11;
      MBlue = 7;
      MLightCyan = 8;
      MLightGray = 4;
      MLightMagenta = 12;
      MWhite = 15;

PROCEDURE Draw3DBox(CRegion: TRegion);

   BEGIN
      WITH CRegion DO
         BEGIN
            HideCursor;
            SetColor(MWhite);
            PlotLine(x,y,x+length,y);
            PlotLine(x+length,y,x+length,y+height);
            PlotLine(x+length,y+height,x,y+height);
            PlotLine(x,y+height,x,y);
            PlotLine(x+1,y+1,x+length-1,y+1);
            PlotLine(x+length-1,y+1,x+length-1,y+height-1);
            PlotLine(x+length-1,y+height-1,x+1,y+height-1);
            PlotLine(x+1,y+height-1,x+1,y+1);
            PlotLine(x,y,x+1,y+1);
            PlotLine(x+length,y,x+length-1,y+1);
            PlotLine(x+length,y+height,x+length-1,y+height-1);
            PlotLine(x,y+height,x+1,y+height-1);
            SetFillStyle(1,MDarkGray);
            floodfill(MapX(x+0.5),MapY(y+2),MWhite);
            floodfill(MapX(x+2),MapY(y+height-0.5),MWhite);
            SetFillStyle(1,MWhite);
            floodFill(MapX(x+2.5),MapY(y+2.5),MWhite);
            SetFillStyle(1,MLightGray);
            floodfill(MapX(x+2),MapY(y+0.5),MWhite);
            floodfill(MapX(x+length- 0.5),MapY(y+2),MWhite);
            ShowCursor;
         END;
   END;

PROCEDURE SetMinkowskiPalette;

   BEGIN
      {GetPalette(Pal1);}
      SetPalette(0,0);
      SetPalette(1,3);
      SetPalette(2,5);
      SetPalette(3,56);
      SetPalette(4,7);
      SetPalette(5,7);
      SetPalette(6,7);
      SetPalette(7,1);
      SetPalette(8,59);
      SetPalette(9,59);
      SetPalette(10,59);
      SetPalette(11,62);
      SetPalette(12,61);
      SetPalette(13,61);
      SetPalette(14,61);
      SetPalette(15,63);
      SETRGBPalette(5,25,0,25);
      SETRGBPalette(3,0,20,25);
   END;

PROCEDURE LabelAxes(GraphNum, GraphType: Integer);

   BEGIN
      SelectViewPort(GraphNum);
      SelectScale(5);
      SetColor(MYellow);
      HideCursor;
      CASE GraphType OF
         XYGraph, XYPrimeGraph:
            BEGIN
               PlotSymbol(0.9,-0.1,'x');
               PlotSymbol(0.1,0.9,'y');
            END;
         XTGraph, XTPrimeGraph:
            BEGIN
               PlotSymbol(0.9,-0.1,'x');
               PlotSymbol(0.1,0.9,'t');
            END;
         XYTGraph, XYTPrimeGraph:
            BEGIN
               PlotSymbol(0.9,-0.35,'x');
               PlotSymbol(0.9,0.64,'y');
               PlotSymbol(0.1,0.9,'t');
            END;
         XYZGraph, XYZPrimeGraph:
            BEGIN
               PlotSymbol(0.9,-0.35,'x');
               PlotSymbol(0.9,0.64,'y');
               PlotSymbol(0.1,0.9,'z');
            END;
      END;
      ShowCursor;
   END;

PROCEDURE Initialize;

   BEGIN
      CupsInit;
      Menu.Init;
      HotKeys.Init(6);
      hotKeys.key[1] := 'F1-Help';
      hotKeys.key[2] := 'F2-Run';
      hotKeys.key[3] := 'F3-Edit';
      hotKeys.key[4] := 'F5-Faster';
      hotKeys.key[5] := 'F6-Slower';
      hotKeys.key[6] := 'F10-Menu';
      HotKeys.HotKeyColor := MLightMagenta;
      HotKeys.BackColor := MBlue;
      HotKeys.DisactivateColor := MDarkGray;
      Menu.Column(1,'File');
      Menu.Row(1,1,'About CUPS');
      Menu.Row(1,2,'About Program');
      Menu.Row(1,3,'Configuration');
      Menu.Row(1,4,'-------------');
      Menu.Row(1,5,'Load File');
      Menu.Row(1,6,'Save File');
      Menu.Row(1,7,'-------------');
      Menu.Row(1,8,'Exit Program');
      Menu.Column(2,'Objects');
      Menu.Row(2,1,'Rockets, Sticks, Flashers, etc.');
      {Menu.Row(2,2,'Scalars');}
      Menu.Row(2,2,'Four Vectors');
      Menu.Row(2,3,'2nd Rank Tensors');
      Menu.Column(3,'Graph #1');
      Menu.Row(3,1,'xt');
      Menu.Row(3,2,'xy');
      Menu.Row(3,3,'xyt');
      Menu.Row(3,4,'xyz');
      Menu.Row(3,5,'-----------------');
      Menu.Row(3,6,'Reference Frame 1');
      Menu.Row(3,7,'Reference Frame 2');
      Menu.Row(3,8,'-----------------');
      Menu.Row(3,9,'Show World Lines');
      Menu.Column(4,'Graph #2');
      Menu.Row(4,1,'xy');
      Menu.Row(4,2,'xt');
      Menu.Row(4,3,'xyt');
      Menu.Row(4,4,'xyz');
      Menu.Row(4,5,'-----------------');
      Menu.Row(4,6,'Reference Frame 1');
      Menu.Row(4,7,'Reference Frame 2');
      Menu.Row(4,8,'-----------------');
      Menu.Row(4,9,'Show World Lines');
      Menu.Column(4,'Graph #2');
      Menu.RowActivate(1,4,False);
      Menu.RowActivate(1,7,False);
      Menu.RowActivate(3,5,False);
      Menu.RowActivate(3,8,False);
      Menu.RowActivate(4,5,False);
      Menu.RowActivate(4,8,False);
      {Menu.RowActivate(2,2,False);
      Menu.RowActivate(2,3,False);
      Menu.RowActivate(2,4,False);}
      Menu.RowChecked(2,1,True);
      Menu.RowChecked(3,3,True);
      Menu.RowChecked(4,3,True);
      Menu.RowChecked(3,6,True);
      Menu.RowChecked(3,9,True);
      Menu.RowChecked(4,7,True);
      Menu.RowChecked(4,9,True);
      Menu.BackColor := MBlue;
      Menu.TextColor := MWhite;
      Menu.GrayOutColor := MCyan;
      Menu.HiGrayOutColor := MLightCyan;
      Menu.Display;
      HotKeys.Display;
      Digits := ['0','1','2','3','4','5','6','7','8','9'];
      EditChars := Digits + [Minus,Decimal,BackSpace,RightArrow,LeftArrow,'E','e'];
   END;

PROCEDURE StartupScreen;
   VAR
      AboutScr : HelpScrType;
   BEGIN              {                                                  }
      AboutScr[01] := '                                                  ';
      AboutScr[02] := '                CUPS MODERN PHYSICS               ';
      AboutScr[03] := '                SPECIAL  RELATIVITY               ';
      AboutScr[04] := '                                                  ';
      AboutScr[05] := '                        by                        ';
      AboutScr[06] := '                 Douglas E. Brandt                ';
      AboutScr[07] := '           Eastern Illinois University            ';
      AboutScr[08] := '                                                  ';
      AboutScr[09] := '          Version 0.01     May 24, 1994           ';
      AboutScr[10] := '                Copyright (c) 1994                ';
      AboutScr[11] := '                                                  ';
      AboutScr[12] := '                                                  ';
      AboutScr[13] := '                                                  ';
      AboutScr[14] := '   This program  allows users to investigate      ';
      AboutScr[15] := '   the transfomation properties of several types  ';
      AboutScr[16] := '   of objects under the Lorentz transformation.   ';
      AboutScr[17] := '   Users can display a representation of these    ';
      AboutScr[18] := '   objects in various types of graphs and can     ';
      AboutScr[19] := '   view the time evolution of the motion.         ';
      AboutScr[20] := '                                                  ';
      AboutScr[21] := '                                                  ';
      AboutScr[22] := '                                                  ';
      AboutScr[23] := '                                                  ';
      AboutScr[24] := '    Press any Key or Click Mouse to continue.     ';
      AboutScr[25] := '                                                  ';
      Help(AboutScr);
   END;

PROCEDURE LorentzTransform(t,x,y,z: Real; VAR tt,xx,yy,zz: Real;
                                                       Beta: TSpaceVector);

   VAR
      c1,cxx,cyy,czz,cxy,cxz,cyz: Real;

   BEGIN

      IF Beta.Beta2 <> 0 THEN c1 := (Beta.Gamma-1)/Beta.Beta2 ELSE c1 := 0;
      Cxx := 1 + c1*Sqr(Beta.x);
      Cyy := 1 + c1*Sqr(Beta.y);
      Czz := 1 + c1*Sqr(Beta.z);
      Cxy := c1 * Beta.x*beta.y;
      Cxz := c1 * Beta.x*beta.z;
      Cyz := c1 * Beta.y*beta.z;
      tt := Beta.Gamma*(t - Beta.x*x-Beta.y*y-Beta.z*z);
      xx := Beta.Gamma*(-Beta.x*t) + Cxx*x + Cxy*y + Cxz*z;
      yy := Beta.Gamma*(-Beta.y*t) + Cxy*x + Cyy*y + Cyz*z;
      zz := Beta.Gamma*(-Beta.z*t) + Cxz*x + Cyz*y + Czz*z;
   END;

PROCEDURE InvLorentzTransform(t,x,y,z: Real; VAR tt,xx,yy,zz: Real;
                                                       Beta: TSpaceVector);
   VAR
      InvBeta: TSpaceVector;

   BEGIN
      InvBeta.x := -Beta.x;
      InvBeta.y := -Beta.y;
      InvBeta.z := -Beta.z;
      InvBeta.Beta2 := Beta.Beta2;
      InvBeta.Gamma := Beta.Gamma;
      LorentzTransform(t,x,y,z,tt,xx,yy,zz,InvBeta);
   END;

{***************************************************************************

   TKeys Methods

***************************************************************************}
PROCEDURE TKey.Draw;

   BEGIN
     IF active THEN
     BEGIN
      HideCursor;
      SetColor(MWhite);
      PlotLine(x,y,x+length,y);
      PlotLine(x+length,y,x+length,y+5);
      PlotLine(x+length,y+5,x,y+5);
      PlotLine(x,y+5,x,y);
      PlotLine(x+1,y+1,x+length-1,y+1);
      PlotLine(x+length-1,y+1,x+length-1,y+4);
      PlotLine(x+length-1,y+4,x+1,y+4);
      PlotLine(x+1,y+4,x+1,y+1);
      PlotLine(x,y,x+1,y+1);
      PlotLine(x+length,y,x+length-1,y+1);
      PlotLine(x+length,y+5,x+length-1,y+4);
      PlotLine(x,y+5,x+1,y+4);
      SetFillStyle(1,MLightGray);
      floodfill(MapX(x+0.5),MapY(y+2),MWhite);
      floodfill(MapX(x+2),MapY(y+4.5),MWhite);
      floodFill(MapX(x+2.5),MapY(y+2.5),MWhite);
      SetFillStyle(1,MDarkGray);
      floodfill(MapX(x+2),MapY(y+0.5),MWhite);
      floodfill(MapX(x+length- 0.5),MapY(y+2),MWhite);
      OutTextXY(MapX(x+length/2)-TextWidth(KeyLabel) DIV 2 +1,
                  MapY(y+2.5)-TextHeight(KeyLabel) DIV 2+1,KeyLabel);
      ShowCursor;
     END;
  END;

PROCEDURE TKey.Erase;
  BEGIN
   SelectViewPort(1);
   SelectScale(1);
   SetFillStyle(1,DeskTopColor);
   HIdeCursor;
   floodfill(MapX(x+2),MapY(y+0.5),DeskTopColor);
   ShowCursor;
  END;

PROCEDURE TKey.Depress;
   VAR r,s: Integer;

   BEGIN

     SelectViewPort(1);
     SelectScale(1);
     SetFillStyle(1,MDarkGray);
     Hidecursor;
     SetColor(MDarkGray);
     r:= MapX(x+length/2)-TextWidth(KeyLabel) DIV 2;
     s:= MapY(y+2.5)-TextHeight(KeyLabel) DIV 2;
     OutTextXY(MapX(x+length/2)-TextWidth(KeyLabel) DIV 2 +1,
                  MapY(y+2.5)-TextHeight(KeyLabel) DIV 2+1,KeyLabel);
     floodfill(MapX(x+0.5),MapY(y+2),MWhite);
     floodfill(MapX(x+2),MapY(y+4.5),MWhite);
     floodFill(MapX(x+1.2),MapY(y+2),MWhite);
     SetFillStyle(1,MLightGray);
     floodfill(MapX(x+2),MapY(y+0.5),MWhite);
     floodfill(MapX(x+length-0.5),MapY(y+2),MWhite);
     SetColor(MWhite);
     OutTextXY(MapX(x+length/2)-TextWidth(KeyLabel) DIV 2 +1,
                  MapY(y+2.5)-TextHeight(KeyLabel) DIV 2+1,KeyLabel);
     ShowCursor;
{     SetColor(MWhite);
     PlotSymbol(x+2.5,y+2.5,Key);}
  END;

PROCEDURE TKey.Release;

   BEGIN
     SelectViewPort(1);
     SelectScale(1);
     SetFillStyle(1,MLightGray);
     HideCursor;
     SetColor(MLightGray);
      OutTextXY(MapX(x+length/2)-TextWidth(KeyLabel) DIV 2 +1,
                  MapY(y+2.5)-TextHeight(KeyLabel) DIV 2+1,KeyLabel);
     floodfill(MapX(x+0.5),MapY(y+2),MWhite);
     floodfill(MapX(x+2),MapY(y+4.5),MWhite);
     floodFill(MapX(x+1.2),MapY(y+2),MWhite);
     SetFillStyle(1,MDarkGray);
     floodfill(MapX(x+2),MapY(y+0.5),MWhite);
     floodfill(MapX(x+length-0.5),MapY(y+2),MWhite);
     SetColor(MWhite);
      OutTextXY(MapX(x+length/2)-TextWidth(KeyLabel) DIV 2 +1,
                  MapY(y+2.5)-TextHeight(KeyLabel) DIV 2+1,KeyLabel);
     ShowCursor;
{     PlotSymbol(x+2.5,y+2.5,Key);}
  END;

PROCEDURE TKey.HighLight;

   BEGIN
     SelectViewPort(1);
     SelectScale(1);
     SetColor(MBlue);
     Hidecursor;
     OutTextXY(MapX(x+length/2)-TextWidth(KeyLabel) DIV 2 +1,
                  MapY(y+2.5)-TextHeight(KeyLabel) DIV 2+1,KeyLabel);
     SetFillStyle(1,MLightCyan);
     floodfill(MapX(x+0.5),MapY(y+2),MWhite);
     floodfill(MapX(x+2),MapY(y+4.5),MWhite);
     floodFill(MapX(x+1.2),MapY(y+2),MWhite);
     SetFillStyle(1,MCyan);
     floodfill(MapX(x+2),MapY(y+0.5),MWhite);
     floodfill(MapX(x+length-0.5),MapY(y+2),MWhite);
     OutTextXY(MapX(x+length/2)-TextWidth(KeyLabel) DIV 2 +1,
                  MapY(y+2.5)-TextHeight(KeyLabel) DIV 2+1,KeyLabel);
     ShowCursor;
{     SetColor(MWhite);
     PlotSymbol(x+2.5,y+2.5,Key);}
  END;

PROCEDURE TKey.UnHighLight;

   BEGIN
     SelectViewPort(1);
     SelectScale(1);
     SetFillStyle(1,MLightGray);
     Hidecursor;
     SetColor(MDarkGray);
     OutTextXY(MapX(x+length/2)-TextWidth(KeyLabel) DIV 2 +1,
                  MapY(y+2.5)-TextHeight(KeyLabel) DIV 2+1,KeyLabel);
     floodfill(MapX(x+0.5),MapY(y+2),MWhite);
     floodfill(MapX(x+2),MapY(y+4.5),MWhite);
     floodFill(MapX(x+1.2),MapY(y+2),MWhite);
     SetFillStyle(1,MDarkGray);
     floodfill(MapX(x+2),MapY(y+0.5),MWhite);
     floodfill(MapX(x+length-0.5),MapY(y+2),MWhite);
     SetColor(MWhite);
     OutTextXY(MapX(x+length/2)-TextWidth(KeyLabel) DIV 2 +1,
                  MapY(y+2.5)-TextHeight(KeyLabel) DIV 2+1,KeyLabel);
     ShowCursor;
{     SetColor(MWhite);
     PlotSymbol(x+2.5,y+2.5,Key);}
  END;

PROCEDURE TKey.Activate;

   BEGIN
     SelectViewPort(1);
     SelectScale(1);
      HideCursor;
      SetColor(MWhite);
      OutTextXY(MapX(x+length/2)-TextWidth(KeyLabel) DIV 2 +1,
                  MapY(y+2.5)-TextHeight(KeyLabel) DIV 2+1,KeyLabel);
      ShowCursor;
      Active := True;
   END;

PROCEDURE TKey.Inactivate;

   BEGIN
     SelectViewPort(1);
     SelectScale(1);
      HideCursor;
      SetColor(MDarkGray);
      OutTextXY(MapX(x+length/2)-TextWidth(KeyLabel) DIV 2 +1,
                  MapY(y+2.5)-TextHeight(KeyLabel) DIV 2+1,KeyLabel);
      ShowCursor;
      Active := False;
   END;



PROCEDURE TKey.Create(kx,ky: Real; KLabel: String; KLength: Real;
                                 Canselect,canhold: Boolean; GroupNum: Byte);

   BEGIN
      x := kx;
      y := ky;
      Active := True;
      Length := KLength;
      KeyLabel := KLabel;
      KeyRegion.UpLeft.x := MapX(x);
      KeyRegion.UpLeft.y := MapY(y+5)+30;
      KeyRegion.LowRt.x := MapX(x+Length);
      KeyRegion.LowRt.y := MapY(y)+30;
      Defined := True;
      Selectable := CanSelect;
      Holdable := CanHold;
      Group := GroupNum;
   END;

{***************************************************************************

     Region Methods

***************************************************************************}

FUNCTION Region.PtInRegion(TestPoint: Point): Boolean;

   BEGIN
      IF ((TestPoint.x >= UpLeft.x) AND (TestPoint.y >= UpLeft.y)
          AND (TestPoint.x <= LowRt.x) AND (TestPoint.y <= LowRt.y)) THEN
          PtInRegion := True
      ELSE PtInRegion := False;
   END;

PROCEDURE Region.Draw;
   BEGIN
   END;


{PROCEDURE DisplayPrompt(Number: Byte);

   VAR
      Prompt1: String[30];
      Prompt2: String[30];
      Prompt3: String[30];

   BEGIN
      DefineViewPort(10,0.58,0.98,0.075,0.3);
      DefineScale(10,0,100,0,100);
      GraphBackColor := MLightCyan;
      SetColor(MWhite);
      OpenViewPort(10);
      CASE Number OF
       1: BEGIN
             Prompt1 := 'Change objects, change motion';
             Prompt2 := 'of frame 2, change graph types,';
             Prompt3 := 'or run or increment time.';
          END;
       2: BEGIN
             Prompt1 := 'Tab or click on field to edit,';
             Prompt2 := 'Click on edit, F3 or enter to';
             Prompt3 := 'end edit mode.';
          END;
       3: BEGIN
             Prompt1 := 'Click on run/stop or F2 to stop';
             Prompt2 := 'animation.  F5 for faster or F6';
             Prompt3 := 'for slower animation.';
          END;
       4: BEGIN
             Prompt1 := 'Select type of object to add.';
             Prompt2 := 'Enter or click on OK to accept.';
             Prompt3 := 'Esc or click on cancel cancels.';
          END;
       5: BEGIN
             Prompt1 := 'SPEED EXCEEDS SPEED OF LIGHT!';
             Prompt2 := 'Continue to edit velocity the';
             Prompt3 := 'component values';
          END;
      END;
      SelectScale(10);
      SetColor(MBlack);
      SetTextJustify(CenterText,CenterText);
      OutTextXY(Mapx(50),Mapy(35),Prompt3);
      OutTextXY(Mapx(50),Mapy(50),Prompt2);
      OutTextXY(Mapx(50),Mapy(65),Prompt1);
      SetTextJustify(LeftText,TopText);
      GraphBackColor := MBlack;
      SelectViewPort(1);
      SelectScale(1);
   END;  }

PROCEDURE DisplayWrite;

   BEGIN
      SetColor(MWhite);
      HideCursor;
      outtextxy(MapX(58),MapY(41),CHR(225));
      outtextxy(MapX(59),MapY(40),'x');
      outtextxy(MapX(72),MapY(41),CHR(225));
      outtextxy(MapX(73),MapY(40),'y');
      outtextxy(MapX(86),MapY(41),CHR(225));
      outtextxy(MapX(87),MapY(40),'z');
      outtextxy(MapX(44),MapY(70),'t=');
      outtextxy(MapX(68),MapY(25),'Gamma');
      outtextxy(MapX(1), MapY(98),'Graph #1');
      outtextxy(MapX(0.5), MapY(65),'Reference');
      outtextxy(MapX(2.5), MapY(62),'Frame:');
      outtextxy(MapX(89), MapY(98),'Graph #2');
      outtextxy(MapX(88.5), MapY(65),'Reference');
      outtextxy(MapX(90.5), MapY(62),'Frame:');
      OutTextXY(MapX(44),MapY(98), 'Show World');
      OutTextXY(MapX(45),MapY(95), 'Lines in');
      OutTextXY(MapX(45.5),MapY(92), 'Graph #');
      outtextxy(MapX(62),MapY(49),'Motion of Reference Frame 2');
      outtextxy(MapX(61),MapY(46),'Relative to Reference Frame 1');
      DefineViewPort(6,0.579,0.88,0.52,0.92);
      DefineScale(6,TMin,TMax,TMin,Tmax);
      DefineViewPort(5,0.12,0.421,0.52,0.92);
      DefineScale(5,TMin,TMax,TMin,TMax);
      GraphBackColor := MBlack;
      OpenViewPort(6);
      OpenViewPort(5);
      ShowCursor;
   END;

PROCEDURE InitializePrompt;

   BEGIN
      DefineViewport(11,0.55,0.98,0.08,0.200);
      GraphBackColor := MLightCyan;
      SetColor(MWhite);
      HideCursor;
      OPenViewPort(11);
      DefineScale(10,0,100,0,100);
      SelectScale(10);
      PlotLine(0,0,2,10);
      PlotLine(2,10,2,90);
      PlotLine(2,90,0,100);
      PlotLine(2,90,98,90);
      PlotLine(98,90,100,100);
      PlotLine(98,90,98,10);
      PlotLine(98,10,100,0);
      PlotLine(98,10,2,10);
      ShowCursor;
      GraphBackColor := Black;
   END;

PROCEDURE DisplayPrompt(Number: Byte);

   VAR i: Integer;
      Prompt1: String[32];
      Prompt2: String[32];
      Prompt3: String[32];
      MyPattern: FillPatternType;

   BEGIN
     { FOR i := 1 TO 8 DO IF ((i MOD 2) = 0) THEN MyPattern[i] := $AA
       ELSE MyPattern[i] := $55;
      SetFillPattern(MyPattern,MLightCyan);}
      SetFillStyle(1,MLightCyan);
      SelectViewPort(11);
      SelectScale(10);
      SetColor(MWhite);
      HideCursor;
      Bar(MapX(2)+1,MapY(10)-1,MapX(98)-1,MapY(90)+1);
      {Pause;
      SetFillStyle(1,MLightCyan);}
      {FloodFill(Mapx(50),MapY(15),MWhite);}
      CASE Number OF
       1: BEGIN
             Prompt1 := 'Change objects, change motion';
             Prompt2 := 'of frame 2, change graph types,';
             Prompt3 := 'run or increment time.';
          END;
       2: BEGIN
             Prompt1 := 'Tab or click on field to edit,';
             Prompt2 := 'F3, enter, or click on edit to';
             Prompt3 := 'end edit mode.';
          END;
       3: BEGIN
             Prompt1 := 'F2 or click on run/stop to stop';
             Prompt2 := 'animation.  F5 for faster or F6';
             Prompt3 := 'for slower animation.';
          END;
       4: BEGIN
             Prompt1 := 'Select type of object to add.';
             Prompt2 := 'Enter or click on OK to accept.';
             Prompt3 := 'Esc or click on cancel cancels.';
          END;
       5: BEGIN
             Prompt1 := 'SPEED EQUAL TO OR EXCEEDS SPEED';
             Prompt2 := 'OF LIGHT! Continue to edit the';
             Prompt3 := 'velocity component values.';
          END;
       6: BEGIN
             Prompt1 := 'Change component values, change';
             Prompt2 := 'symmetry, change graph type, or';
             Prompt3 := 'change reference frame 2 motion.';
          END;
       7: BEGIN
             Prompt1 := 'Change component values, change';
             Prompt2 := 'graph type, or change reference';
             Prompt3 := 'frame #2 motion.';
          END;
      END;
      SelectScale(10);
      SetColor(MBlack);
      SetTextJustify(CenterText,CenterText);
      OutTextXY(Mapx(50),Mapy(28),Prompt3);
      OutTextXY(Mapx(50),Mapy(50),Prompt2);
      OutTextXY(Mapx(50),Mapy(72),Prompt1);
      ShowCursor;
      SetTextJustify(LeftText,TopText);
      GraphBackColor := MBlack;
      SelectViewPort(1);
      SelectScale(1);
   END;

PROCEDURE ShowHelp(currentHelp: Byte);

   VAR
      HelpScreen: HelpScrType;

  BEGIN
   CASE CurrentHelp OF
    1: BEGIN
       HelpScreen[01] := '                                                  ';
       HelpScreen[02] := ' Objects may be added, deleted, editted or all    ';
       HelpScreen[03] := ' objects removed by clicking on appropriate       ';
       HelpScreen[04] := ' button or selecting same under objects menu. The ';
       HelpScreen[05] := ' next key or menu item brings the next objects    ';
       HelpScreen[06] := ' card to the front for viewing or editting.       ';
       HelpScreen[07] := '                                                  ';
       HelpScreen[08] := ' Click or hold buttons labelled '+CHR(225)+'x, '+CHR(225)+'y, '+CHR(225)+'z to     ';
       HelpScreen[09] := ' change the motion of reference frame 2.  Graphs  ';
       HelpScreen[10] := ' will be updated if there are no clicks on these  ';
       HelpScreen[11] := ' buttons for 2 seconds.                                   ';
       HelpScreen[12] := '                                                  ';
       HelpScreen[13] := ' Change what is displayed in a graph by clicking  ';
       HelpScreen[14] := ' on button for the graph type and reference frame ';
       HelpScreen[15] := ' desired or alternatively selecting from either   ';
       HelpScreen[16] := ' the Graph #1 or Graph #2 menus.                  ';
       HelpScreen[17] := '                                                  ';
       HelpScreen[18] := ' Change time by clicking on the up or down buttons';
       HelpScreen[19] := ' or run time by clicking on the run/stop button   ';
       HelpScreen[20] := ' near the time display.  Alternatively, run time  ';
       HelpScreen[21] := ' by pressing the F2 key.                          ';
       HelpScreen[22] := '                                                  ';
       HelpScreen[23] := '                                                  ';
       HelpScreen[24] := '    Press any Key or Click Mouse to continue.     ';
       HelpScreen[25] := '                                                  ';
      END;
     2:BEGIN
       HelpScreen[01] := '                                                  ';
       HelpScreen[02] := ' Edit the values of the quantities shown on the   ';
       HelpScreen[03] := ' visible information card.  You can move to       ';
       HelpScreen[04] := ' another field by pressing the tab key, clicking  ';
       HelpScreen[05] := ' on the next button, or clicking on the field to  ';
       HelpScreen[06] := ' edit.                                            ';
       HelpScreen[07] := '                                                  ';
       HelpScreen[08] := ' Values can be entered using standard real number ';
       HelpScreen[09] := ' notation.  If the velocity components result in  ';
       HelpScreen[10] := ' speed greater than or equal to the speed of      ';
       HelpScreen[11] := ' light, edit mode will lock in on the velocity    ';
       HelpScreen[12] := ' components until the values result in a speed    ';
       HelpScreen[13] := ' less than the speed of light.  Values related to ';
       HelpScreen[14] := ' the one being editted are automatically updated. ';
       HelpScreen[15] := '                                                  ';
       HelpScreen[16] := ' Edit mode can be exitted by pressing the F3 key  ';
       HelpScreen[17] := ' or clicking on the edit button.  The graphs will ';
       HelpScreen[18] := ' be updated after exitting edit mode.             ';
       HelpScreen[19] := '                                                  ';
       HelpScreen[20] := '                                                  ';
       HelpScreen[21] := '                                                  ';
       HelpScreen[22] := '                                                  ';
       HelpScreen[23] := '                                                  ';
       HelpScreen[24] := '    Press any Key or Click Mouse to continue.     ';
       HelpScreen[25] := '                                                  ';
      END;
     3:BEGIN
       HelpScreen[01] := '                                                  ';
       HelpScreen[02] := ' Objects may be added, deleted, editted or all    ';
       HelpScreen[03] := ' objects removed by clicking on appropriate       ';
       HelpScreen[04] := ' button or selecting same under objects menu. The ';
       HelpScreen[05] := ' next key or menu item brings the next objects    ';
       HelpScreen[06] := ' card to the front for viewing or editting.       ';
       HelpScreen[07] := '                                                  ';
       HelpScreen[08] := ' Click or hold buttons labelled '+CHR(225)+'x, '+CHR(225)+'y, '+CHR(225)+'z to     ';
       HelpScreen[09] := ' change the motion of reference frame 2.  Graphs  ';
       HelpScreen[10] := ' will be updated if there are no clicks on these  ';
       HelpScreen[11] := ' buttons for 2 seconds.                                   ';
       HelpScreen[12] := '                                                  ';
       HelpScreen[13] := ' Change what is displayed in a graph by clicking  ';
       HelpScreen[14] := ' on button for the graph type and reference frame ';
       HelpScreen[15] := ' desired or alternatively selecting from either   ';
       HelpScreen[16] := ' the Graph #1 or Graph #2 menus.                  ';
       HelpScreen[17] := '                                                  ';
       HelpScreen[18] := ' Change time by clicking on the up or down buttons';
       HelpScreen[19] := ' or run time by clicking on the run/stop button   ';
       HelpScreen[20] := ' near the time display.  Alternatively, run time  ';
       HelpScreen[21] := ' by pressing the F2 key.                          ';
       HelpScreen[22] := '                                                  ';
       HelpScreen[23] := '                                                  ';
       HelpScreen[24] := '    Press any Key or Click Mouse to continue.     ';
       HelpScreen[25] := '                                                  ';
      END;
     4:BEGIN
       HelpScreen[01] := '                                                  ';
       HelpScreen[02] := ' Objects may be added, deleted, editted or all    ';
       HelpScreen[03] := ' objects removed by clicking on appropriate       ';
       HelpScreen[04] := ' button or selecting same under objects menu. The ';
       HelpScreen[05] := ' next key or menu item brings the next objects    ';
       HelpScreen[06] := ' card to the front for viewing or editting.       ';
       HelpScreen[07] := '                                                  ';
       HelpScreen[08] := ' Click or hold buttons labelled '+CHR(225)+'x, '+CHR(225)+'y, '+CHR(225)+'z to     ';
       HelpScreen[09] := ' change the motion of reference frame 2.  Graphs  ';
       HelpScreen[10] := ' will be updated if there are no clicks on these  ';
       HelpScreen[11] := ' buttons for 2 seconds.                                   ';
       HelpScreen[12] := '                                                  ';
       HelpScreen[13] := ' Change what is displayed in a graph by clicking  ';
       HelpScreen[14] := ' on button for the graph type and reference frame ';
       HelpScreen[15] := ' desired or alternatively selecting from either   ';
       HelpScreen[16] := ' the Graph #1 or Graph #2 menus.                  ';
       HelpScreen[17] := '                                                  ';
       HelpScreen[18] := ' Change time by clicking on the up or down buttons';
       HelpScreen[19] := ' or run time by clicking on the run/stop button   ';
       HelpScreen[20] := ' near the time display.  Alternatively, run time  ';
       HelpScreen[21] := ' by pressing the F2 key.                          ';
       HelpScreen[22] := '                                                  ';
       HelpScreen[23] := '                                                  ';
       HelpScreen[24] := '    Press any Key or Click Mouse to continue.     ';
       HelpScreen[25] := '                                                  ';
      END;
     5:BEGIN
       HelpScreen[01] := '                                                  ';
       HelpScreen[02] := ' Objects may be added, deleted, editted or all    ';
       HelpScreen[03] := ' objects removed by clicking on appropriate       ';
       HelpScreen[04] := ' button or selecting same under objects menu. The ';
       HelpScreen[05] := ' next key or menu item brings the next objects    ';
       HelpScreen[06] := ' card to the front for viewing or editting.       ';
       HelpScreen[07] := '                                                  ';
       HelpScreen[08] := ' Click or hold buttons labelled '+CHR(225)+'x, '+CHR(225)+'y, '+CHR(225)+'z to     ';
       HelpScreen[09] := ' change the motion of reference frame 2.  Graphs  ';
       HelpScreen[10] := ' will be updated if there are no clicks on these  ';
       HelpScreen[11] := ' buttons for 2 seconds.                                   ';
       HelpScreen[12] := '                                                  ';
       HelpScreen[13] := ' Change what is displayed in a graph by clicking  ';
       HelpScreen[14] := ' on button for the graph type and reference frame ';
       HelpScreen[15] := ' desired or alternatively selecting from either   ';
       HelpScreen[16] := ' the Graph #1 or Graph #2 menus.                  ';
       HelpScreen[17] := '                                                  ';
       HelpScreen[18] := ' Change time by clicking on the up or down buttons';
       HelpScreen[19] := ' or run time by clicking on the run/stop button   ';
       HelpScreen[20] := ' near the time display.  Alternatively, run time  ';
       HelpScreen[21] := ' by pressing the F2 key.                          ';
       HelpScreen[22] := '                                                  ';
       HelpScreen[23] := '                                                  ';
       HelpScreen[24] := '    Press any Key or Click Mouse to continue.     ';
       HelpScreen[25] := '                                                  ';
      END;
     6:BEGIN
       HelpScreen[01] := '                                                  ';
       HelpScreen[02] := ' Tensor components are shown for frame 1 and 2.   ';
       HelpScreen[03] := ' The component values can be edited with the      ';
       HelpScreen[04] := ' keyboard or can be incremented by clicking the   ';
       HelpScreen[05] := ' up and down buttons to the left of the component ';
       HelpScreen[06] := ' value display.  The current component to edit    ';
       HelpScreen[07] := ' be changed by using the tab key or clicking on   ';
       HelpScreen[08] := ' the desired component.                           ';
       HelpScreen[09] := '                                                  ';
       HelpScreen[10] := ' The symmetry of the tensor can be forced by      ';
       HelpScreen[11] := ' clicking on the desired symmetry check box at    ';
       HelpScreen[12] := ' the bottom of the component display.             ';
       HelpScreen[13] := '                                                  ';
       HelpScreen[14] := ' The graph display of the tensor depends on the   ';
       HelpScreen[15] := ' symmetry selected for the tensor.  No symmetry   ';
       HelpScreen[16] := ' or symmetric tensors display the tensor as the   ';
       HelpScreen[17] := ' action the tensor would have in mapping the      ';
       HelpScreen[18] := ' edges of a 4 cube into a new 4 parallelpiped.    ';
       HelpScreen[19] := ' Antisymmetric tensors diplay the tensor as two   ';
       HelpScreen[20] := ' 3 space vectors in analogy to electric and       ';
       HelpScreen[21] := ' magnetic fields.                                 ';
       HelpScreen[22] := '                                                  ';
       HelpScreen[23] := '                                                  ';
       HelpScreen[24] := '    Press any Key or Click Mouse to continue.     ';
       HelpScreen[25] := '                                                  ';
      END;
     7:BEGIN
       HelpScreen[01] := '                                                  ';
       HelpScreen[02] := ' Vector components are shown for frame 1 and 2.   ';
       HelpScreen[03] := ' The component values can be edited with the      ';
       HelpScreen[04] := ' keyboard or can be incremented by clicking the   ';
       HelpScreen[05] := ' up and down buttons to the left of the component ';
       HelpScreen[06] := ' value display.  The current component to edit    ';
       HelpScreen[07] := ' be changed by using the tab key or clicking on   ';
       HelpScreen[08] := ' the desired component.                           ';
       HelpScreen[09] := '                                                  ';
       HelpScreen[10] := ' Change what is displayed in a graph by clicking  ';
       HelpScreen[11] := ' on button for the graph type and reference frame ';
       HelpScreen[12] := ' desired or alternatively selecting from either   ';
       HelpScreen[13] := ' the Graph #1 or Graph #2 menus.                  ';
       HelpScreen[14] := '   ';
       HelpScreen[15] := '   ';
       HelpScreen[16] := '   ';
       HelpScreen[17] := '   ';
       HelpScreen[18] := '   ';
       HelpScreen[19] := '   ';
       HelpScreen[20] := '   ';
       HelpScreen[21] := '   ';
       HelpScreen[22] := '                                                  ';
       HelpScreen[23] := '                                                  ';
       HelpScreen[24] := '    Press any Key or Click Mouse to continue.     ';
       HelpScreen[25] := '                                                  ';
      END;
    END;
    Help(HelpScreen);
  END;

FUNCTION MyNum2Str(num : real; size : integer) : string;
VAR
   mag,sign,decim,n : integer;
   nStr : string;
   NoInteger : boolean;
BEGIN
   IF size<=0 THEN nStr:=''
   ELSE
     IF ((num<0.00000001) AND (num>-0.00000001)) THEN nStr := '0'
     ELSE
     BEGIN
        IF num<0 THEN sign:=1 ELSE sign:=0;
        size := size-sign;                          {space avaiable }
        num := abs(num);
        mag := trunc(ln(num)/ln(10));               {order of magnitude}
        IF (mag<0) THEN dec(mag);                   {correction for small num}
        decim:= size - (length(ScNumStr(num,1))-1); {num of decimals in Sc N}
        IF (int(num)<>0) THEN
        BEGIN
           IF (mag+1<size) THEN
             nStr := numStr(num, size, size-mag-2)  {2 becouse of .}
           ELSE
             nStr := ScNumStr(num,decim);
        END
        ELSE
        BEGIN
           IF (3-mag<=size) AND (mag>-4) THEN
              nStr := numStr(num, size, size-2)
           ELSE
              nStr := ScNumStr(num,decim);
        END;
        n := 1;
        WHILE n<=length(nStr) DO                {delete extra spaces}
         IF nStr[n]= ' ' THEN delete(nStr,n,1) ELSE inc(n);
        n := Pos('E',nStr);
        IF (n=0) THEN n := length(nStr);
        IF Pos('.',nStr)<>0 THEN                {if not integer...}
         WHILE (nStr[n]='0') DO
         BEGIN                                  {and has 0's on the end...}
            Delete(nStr,n,1);                   {...delete them}
            dec(n);
         END;
        IF nStr[n]='.' then Delete(nStr,n,1);   {delete . if no decimals}
        IF (sign=1) AND (nStr<>'0') THEN
         nStr := concat('-',nStr); {negative num}
     END;
   MyNum2Str := nStr;
END;



END.