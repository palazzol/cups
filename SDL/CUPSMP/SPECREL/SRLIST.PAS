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

{M 65520, 0, 65536}
UNIT srlist;

INTERFACE
USES Dos, Crt, Graph, CUPSmupp, CUPSscrn, CUPS, CUPSgui;

TYPE
   DescriptionString = String[48];
   DescriptionArray = ARRAY [1..30] Of DescriptionString;

FUNCTION SelectFromList(ListLength: Integer; ListArray: DescriptionArray): Integer;

IMPLEMENTATION

VAR
   Slider: TSlider;
   Image: TImage;
   CurrentTop, NewPosition, OldPosition, NumberOfRows: Integer;
   Found,Escape,Delete: Boolean;
   envir: TEnvironment;

FUNCTION SelectFromList(ListLength: Integer; ListArray: DescriptionArray): Integer;


FUNCTION Min(a,b:Integer): Integer;

   BEGIN
      IF (a<b) THEN Min := a ELSE Min := b;
   END;

PROCEDURE PrintList(OldPosition,NewPosition: Integer; VAR CurrentTop: Integer);

   VAR
      CurrentBottom, i, OldHighLight, NewHighLight: Integer;
   BEGIN
     RowHt := 18;
     CurrentBottom := CurrentTop + NumberOfRows-1;
     OldHighlight := OldPosition-CurrentTop+1;
     NewHighLight := NewPosition-CurrentTop+1;
     IF ((NewPosition >= CurrentTop) AND (NewPosition <= CurrentBottom)) THEN
       BEGIN
            HideCursor;
            SetFillStyle(SolidFill,6);
            Bar(20,10+(OldHighLight-1)*RowHt,442,10+(OldHighLight)*RowHt);
            SetColor(15);
            OutTextXY(24,10+OldHighLight*RowHt-12,ListArray[OldPosition]);
            SetFillStyle(SolidFill,7);
            Bar(20,10+(NewHighLight-1)*RowHt,442,10+NewHighLight*RowHt);
            {CurrentSelected := CurrentSelected+NewPosition-OldPosition;}
            OutTextXY(24,10+NewHighLight*RowHt-12,ListArray[NewPosition]);
            ShowCursor;
         END;
      IF NewPosition < CurrentTop THEN
         BEGIN
            CurrentTop := NewPosition;
            SetFillStyle(SolidFill,7);
            SetColor(15);
            HideCursor;
            BAR(20,10,442,10+RowHt);
            OutTextXY(24,10+RowHt-12,ListArray[CurrentTop]);
            SetFillStyle(SolidFill,6);
            SetColor(15);
            FOR i := 1 TO MIN(NumberOfRows-1,ListLength-CurrentTop) DO
              BEGIN
                BAR(20,10+i*RowHt,442,10+(i+1)*RowHt);
                OutTEXTXY(24,10+(i+1)*RowHt-12,ListArray[CurrentTop+i]);
              END;
            ShowCursor;
         END;
      IF NewPosition > CurrentBottom THEN
         BEGIN
            CurrentTop := NewPosition-NumberOfRows+1;
            CurrentBottom := CurrentTop+NumberOfRows-1;
            SetFillStyle(SolidFill,7);
            SetColor(15);
            HideCursor;
            BAR(20,(NumberOfRows-1)*RowHt,442,10+NumberOfRows*RowHt);
            OutTEXTXY(24,10+NumberOfRows*RowHt-12,ListArray[CurrentBottom]);
            SetFillStyle(SolidFill,6);
            SetColor(15);
            FOR i := 1 TO MIN(NumberOfRows-1,ListLength-CurrentTop) DO
              BEGIN
                BAR(20,10+(i-1)*RowHt,442,10+i*RowHt);
                OutTextXY(24,10+i*RowHt-12,ListArray[CurrentTop+i-1]);
              END;
            ShowCursor;
         END;
      OldHighLight := NewHighLight;
   END;

BEGIN
   envir.Save;
   Found := False;
   Escape := False;
   Delete := False;
   NumberOfRows := 17;
   OldPosition := 0;
   NewPosition := 1;
   CurrentTop := 40;
   SelectMuppetPort;
   Image.Get(80,30,560,450);
   HideCursor;
   SetFillStyle(SolidFill,3);
   Bar3D(81,31,559,449,0,True);
   SetColor(15);
   OutTextXY(245,41,'Load Configuration');
   DefineViewPort(13,80/640.0,560/640.0,30/480,410/480);
   SelectViewPort(13);
   DefineScale(13,0,1,0,1);
   SelectScale(13);
   Slider.Create(1,ListLength,ListLength,0.82,0.194,0.830,-1,'','','',true);
   slider.step := 1;
   Slider.draw;
   SetFillStyle(SolidFill,6);
   Bar(20,10,442,316);
   PrintList(OldPosition,NewPosition,CurrentTop);
   Bar3D(120,345,180,360,3,true);
   Bar3D(200,345,260,360,3,true);
   Bar3D(280,345,340,360,3,true);
   SetColor(0);
   OutTextXY(206,350,'Delete');
   OutTextXY(134,350,'Load');
   OutTextXY(286,350,'Cancel');
   {Bar3D(}
   ShowCursor;
   OldPosition := NewPosition;
   CheckForEvents;
   REPEAT
    BEGIN
     CheckForEvents;
     NewPosition := ListLength - round(slider.value) +1;
     IF NewPosition<>OldPosition THEN
       BEGIN
         PrintList(OldPosition, NewPosition,CurrentTop);
         Delay(100);
       END
     ELSE
     WITH event DO
      BEGIN{1}
       IF MouseClicked THEN
        BEGIN{2}
         IF doubleClicked AND (x>100) AND (x<522) AND (y>80) AND (y<386) THEN
          BEGIN{3}
           NewPosition := Min(CurrentTop + (y-80) DIV 18,ListLength);
           found  := true;
           PrintList(OldPosition,NewPosition,CurrentTop);
           Delay(500);
          END{3}
         ELSE
          IF (x>100) AND (x<522) AND (y>80) AND (y<386) THEN
           BEGIN{4}
            NewPosition := Min(CurrentTop + (y-80) DIV 18,ListLength);
            IF NewPosition <> OldPosition THEN
              BEGIN
               PrintList(OldPosition,NewPosition,CurrentTop);
               Delay(30);
              END
           END{4}
          ELSE IF ((y>=415) AND (y<=430)) THEN
           IF ((x>=200) AND (x<= 260)) THEN Found := True
           ELSE
            IF ((x>=280) AND (x<= 340)) THEN
             BEGIN{5}
              Delete := True;
              Found := True;
             END{5}
            ELSE IF ((x>=360) AND (x<= 420)) THEN Escape := True;
        END;{2}
       {IF keyPressed AND
          ((readKey=chr(27)) OR (readKey=chr(13))) THEN
        IF (readKey=chr(13)) THEN found:=true ELSE escape:=true;}
        IF KeyPressed THEN
          CASE ReadKey OF
            chr(13): found := True;
            chr(27): escape := True;
            {chr(213): IF OldPosition <> 1 THEN
                          BEGIN
                            NewPosition := OldPosition-1;
                            PrintList(OldPosition,NewPosition,CurrentTop);
                          END;
            chr(214): IF OldPosition <> ListLength THEN
                          BEGIN
                            NewPosition := OldPosition+1;
                            PrintList(OldPosition,NewPosition,CurrentTop);
                          END;}

          END;
      END;{1}
      IF OldPosition<>NewPosition THEN
       BEGIN
        Slider.Reset(ListLength+1-NewPosition);
        OldPosition := NewPosition;
       END;
      END;
   UNTIL (found OR escape);
   HideCursor;
   SelectMuppetPort;
   Image.Put(80,30);
   ShowCursor;
   envir.Reset;
   IF Delete = True THEN OldPosition := -OldPosition;
   IF Found THEN SelectFromList := OldPosition ELSE SelectFromList := 0;
END;

END.