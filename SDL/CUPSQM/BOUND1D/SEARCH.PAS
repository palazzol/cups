UNIT Search;

{$D-}
{$N+}

INTERFACE

USES
   Graph, CUPSmupp, CUPS, CUPSgui;

TYPE
   YValueFunc     = FUNCTION(XValue:Real; VAR nValue:Integer):Real;
   FastYValueFunc = FUNCTION(XValue:Real):Real;

VAR
   FuncYValue     : YValueFunc;
   FuncFastYValue : FastYValueFunc;
   AbortedSearch: boolean;


PROCEDURE SearchForZero(YName,XName:String;lowestV:Real;
                        VAR XValue:real);

PROCEDURE FastSearchForZero(minX,maxX:real; VAR XValue:real);


IMPLEMENTATION

CONST
   searchBackColor   : integer = blue;
   searchTextColor   : integer = white;
   searchHiBackColor : integer = black;
   searchHiTextColor : integer = white;
   searchErrorColor  : integer = red;
   XSTEP : real = 0.0000005;            {These are the exit criteria}
   YSTEP : real = 0.000001;
   MAXCOUNT : real = 50;
   NUMSIZE = 10;

   xCol : Integer = 65;                {position for printing on screen}
   row1 : Integer =  6;
   row2 : Integer =  9;
   errorCol : Integer = 14;
   errorRow : Integer = 12;
   countCol : Integer = 13;
   countRow : Integer = 23;

VAR
   upYValue, lowYValue : Real;
   UpNValue, lowNValue : Integer;
   inputStr1,inputStr2 : String;       {for requesting input}

PROCEDURE SetHuntZeroHelpScreen(VAR A:HelpScrType);
   VAR
      OK : Boolean;
      i  : Integer;
   BEGIN
      FOR i:=1 TO 25 DO A[i] := '';
      A[1]  := ' This facility illustrates the principle of';
      A[2]  := ' finding eigenvalues by the method of "hunt';
      A[3]  := ' and shoot".  ';
      A[4]  := ' ';
      A[5]  := ' Before you can use it, you need to know TWO';
      A[6]  := ' values of the binding energy for which the';
      A[7]  := ' solution of the equation diverges for large';
      A[8]  := ' values of x - up for one energy, down for the';
      A[9]  := ' other.  You should have been able to find two';
      A[10] := ' such values by using the TRY ENERGY menu item.';
      A[11] := ' ';
      A[12] := ' Since solutions of the wave equation are real';
      A[13] := ' exponentials where the potential is zero, the';
      A[14] := ' fact that the solution diverges for large x';
      A[15] := ' means that a part of the solution is a';
      A[16] := ' DIVERGING exponential there.  The condition';
      A[17] := ' for an eigenfunction is that this part of the';
      A[18] := ' solution has zero amplitude.';
      A[19] := ' ';
      A[20] := ' This part of the program chooses energies';
      A[21] := ' between the two values you give it, by finer';
      A[22] := ' and finer bisection, until it gets to a value';
      A[23] := ' for which the solution does not diverge.';
      A[24] := '';
      A[25] := ' Press <Enter> or click the mouse to continue.';
   END;

PROCEDURE DisplayBackgroundScreen(YName,XName:String; lowestV,maxX,minX:Real);
   BEGIN
      DefineViewport(20,0.00,1.00, 0.00,1.00);
      OpenViewport(20);
      SetFillStyle(SolidFill,blue);
      HideCursor;
      WITH views[20] DO Bar(Vx1+1,Vy1+1,Vx2-1,Vy2-1);
      ShowCursor;
      inputStr1 := concat('         First value of the binding energy (0<EB<',
                          NumStr(abs(lowestV),3,0),
                          ') :');
      inputStr2 := concat('         Second value of the binding energy (0<EB<',
                          NumStr(abs(lowestV),3,0),
                          ') :');

      SetColor(white);
      Print(1,1,
'   The Schroedinger equation is solved by hunting for a binding energy for');
      Print(1,2,
'   which the solution at large positive x behaves like a exponential which');
      Print(1,3,
'   converges to zero.  You will be asked to supply two values of EB for which ');
      Print(1,4,
'   the DIVERGING exponential part of the solutions are of different sign.  ');

      Print(1,row1,inputStr1);
      Print(1,row1+1, '    Diverging exponential part of wave function (large x) =');

      Print(1,row2, inputStr2);
      Print(1,row2+1,'    Diverging exponential part of wave function (large x) =');

      SetColor(yellow);
      Print(xCol,row1,NumStr(maxX,3,0));
      Print(xCol,row2,NumStr(minX,3,0));
   END;

PROCEDURE RequestAnEntry(i:Integer; VAR x:Real; VAR escape:Boolean);
   VAR
      thisInputScreen : TInputScreen;
      thisHelpScreen  : HelpScrType;
   BEGIN
      SetHuntZeroHelpScreen(thisHelpScreen);
      WITH thisInputScreen DO
         BEGIN
            Init;
            SetHelpScreen(thisHelpScreen);
            DefineInputPort(0.02,0.98, (0.85-i*0.15),(0.95-i*0.15));
            IF i=1 THEN
               LoadLine(concat(inputStr1,'         {        }'))
            ELSE
               LoadLine(concat(inputStr2,'         {        }'));
            LoadLine('         [  Ok  ]        [Cancel]        [ Help ]');
            SetNumber(1,x);
            AcceptScreen;
            escape := Canceled;
            IF NOT escape THEN
               x := GetNumber(1);
            Done;
         END;
   END;

PROCEDURE ShowValue(row:Integer; thisValue:Real);
   VAR
      message : String;
      thisRow : Integer;
      oldVNum : Integer;
   BEGIN
      oldVNum := viewportNumber;
      SelectViewport(20);
      message := NumStr(thisValue,8,6);
      RubOut(xCol,row,9,blue);
      SetColor(yellow);
      Print(xCol,row,message);
      SelectViewport(oldVNum);
   END;

PROCEDURE ShowXValue(i:integer; thisXValue:real);
   BEGIN
      IF i=1 THEN ShowValue(row1,thisXValue);
      IF i=2 THEN ShowValue(row2,thisXValue);
   END;

PROCEDURE ShowYValue(i:Integer; ThisYValue:Real);
   VAR
      message : String;
      thisRow : Integer;
   BEGIN
      IF i=1 THEN ShowValue(row1+1,-thisYValue);
      IF i=2 THEN ShowValue(row2+1,-thisYValue);
   END;

PROCEDURE PrintErrorMessage(thisStr:String);
   VAR
      oldVNum : Integer;
   BEGIN
      oldVNum := viewportNumber;
      SelectViewport(20);
      RubOut(errorCol-1,errorRow,  length(thisStr)+2,searchErrorColor);
      SetColor(white);
      Print (errorCol,errorRow,thisStr);
      Beep;
      SelectViewport(oldVNum);
   END;

PROCEDURE EraseErrorMessage(thisStr:String);
   VAR
      oldVNum : Integer;
   BEGIN
      oldVNum := viewportNumber;
      SelectViewport(20);
      RubOut(errorCol-1,errorRow,  length(thisStr)+2,blue);
      SelectViewport(oldVNum);
   END;

PROCEDURE PrepareForCounter;
   VAR
      oldVNum : Integer;
   BEGIN
      oldVNum := viewportNumber;
      SelectViewport(20);
      SetColor(white);
      Print (4,countRow-1, 'Number of bisections');
      SelectViewport(oldVNum);
   END;

PROCEDURE ShowCounter(n:Integer);
   VAR
      oldVNum : Integer;
   BEGIN
      oldVNum := viewportNumber;
      SelectViewport(20);
      RubOut(countCol-1,countRow, 4,searchErrorColor);
      SetColor(white);
      Print (countCol,countRow, NumStr(n,2,0));
      SelectViewport(oldVNum);
   END;

PROCEDURE NarrowNodes(VAR maxX,minX:Real; VAR goOn:Boolean);
   VAR
      thisYValue : Real;
      thisXValue : Real;
      thisNValue : Integer;
      dNup,dNlow : Integer;

   PROCEDURE PutThisAsUp;
      BEGIN
         maxX := thisXValue;
         upNValue := thisNValue;
         upYValue := thisYValue
      END;

   PROCEDURE PutThisAsLow;
      BEGIN
         minX := thisXValue;
         lowNValue := thisNValue;
         lowYValue := thisYValue;
      END;

   BEGIN
      thisXValue := (MaxX+MinX)/2;
      thisYValue := FuncYValue(thisXValue,thisNValue);
      dNup  := abs(upNValue - thisNValue);
      dNlow := abs(thisNValue - lowNValue);
      IF (thisYValue*upYValue)<=0 THEN
       { an acceptable pair of bounds has been found }
         BEGIN
            goOn := true;
            IF dNup<dNlow
               THEN PutThisAsUp
               ELSE PutThisAsLow;
         END
      ELSE
       { number of nodes is even and greater than 2 }
         BEGIN
            goOn := false;
            IF dNup=0
               THEN PutThisAsUp;
            IF dNlow=0
               THEN PutThisAsLow;
            IF (dNup<>0) AND (dNlow<>0) THEN
               IF dNup<dNlow
                  THEN PutThisAsUp
                  ELSE PutThisAsLow;
         END;
   END;

FUNCTION OKtoProceed(VAR maxX,minX:Real): boolean;
   CONST
      blank  = '                                                    ';
      error1 = 'THERE ARE NO EIGENVALUES IN THIS RANGE.  TRY AGAIN. ';
      error2 = 'SEVERAL EIGENVALUES IN THIS RANGE.  I WILL FIND ONE.';
   VAR
      goOn : Boolean;
   BEGIN
      EraseErrorMessage(blank);
      IF ((upYValue*lowYValue)<0)
       {If the input values give asymptotes of different sign proceed }
         OR ((upYValue=0) AND (lowYValue<>0))
         OR ((lowYValue=0) AND (upYValue<>0))
            THEN OKToProceed := true
      ELSE
       { otherwise we have to count nodes to get a workable starting range }
         BEGIN
            IF abs(UpNValue-lowNValue)=0 THEN
               BEGIN
                  PrintErrorMessage(error1);
                  OKtoProceed := false;
               END
            ELSE
               BEGIN
                  goOn := false;
                  PrintErrorMessage(error2);
                  WHILE NOT goOn DO
                     NarrowNodes(maxX,minX,goOn);
                  OKtoProceed := true;
               END;
         END;
   END;

PROCEDURE InitializeSearch(VAR YName,XName:String;
                           lowestV:Real;
                           VAR XValue:Real;
                           VAR nValue:Integer;
                           VAR searchCounter:Integer;
                           VAR maxX,minX:Real);
   CONST
      Blank = '                                                       ';
      Error = 'THE VALUE IS NOT WITHIN THE ALLOWED RANGE.  TRY AGAIN. ';
   VAR
      i,tmp           : Integer;
      thisYValue,temp : Real;
      temporaryString : String;
      act             : Char;
      OKFlag,inRange  : Boolean;
   BEGIN
      maxX := abs(lowestV*0.99);
      minX := abs(lowestV*0.01);
      SearchCounter := 0;
      DisplayBackgroundScreen(YName,XName,lowestV,maxX,minX);

      REPEAT
         i := 1;
         REPEAT
            REPEAT
               IF i=1 THEN XValue := maxX
                      ELSE XValue := minX;
               RequestAnEntry(i,XValue,abortedSearch);
               IF abortedSearch THEN
                  EXIT;
               IF (XValue>0) AND (XValue<abs(lowestV))
                  THEN
                     BEGIN
                        inRange := true;
                        EraseErrorMessage(blank);
                     END
                  ELSE
                     BEGIN
                        PrintErrorMessage(error);
                        inRange := false;
                     END;

            UNTIL inRange;
            thisYValue := FuncYValue(XValue,NValue);
            IF i=1 THEN
               BEGIN
                  MaxX     := XValue;
                  upYValue := thisYValue;
                  UpNValue := NValue;
               END
            ELSE
               BEGIN
                  MinX      := XValue;
                  lowYValue := thisYValue;
                  LowNValue := NValue;
               END;
            ShowXValue(i,XValue);
            ShowYValue(i,ThisYValue);
            i := i + 1;
         UNTIL i>2;
      UNTIL OKToProceed(maxX,minX);
      IF MinX>MaxX THEN
         BEGIN
            temp := MaxX;   MaxX := MinX;        MinX := temp;
            temp:=upYValue; upYValue:=lowYValue; lowYValue:=temp;
            tmp :=UpNValue; UpNValue:=LowNValue; LowNValue:=tmp;
            ShowXValue(1,maxX);
            ShowXValue(2,minX);
            ShowYValue(1,upYValue);
            ShowYValue(2,lowYValue);
         END;
      PrepareForCounter;
   END;

PROCEDURE Narrowbounds(VAR XValue:real;
                       VAR nValue:Integer;
                       VAR searchCounter: integer;
                       VAR maxX,minX: real);
   VAR
      ThisYValue : real;
      UpLow: integer;            {counter: 1 for upper, 2 for lower}
   BEGIN
      SearchCounter := SearchCounter + 1;
      XValue := (MaxX+MinX)/2;
      ThisYValue := FuncYValue(XValue,nValue);
   IF (ThisYValue*upYValue)>=0 THEN
         BEGIN
            MaxX := XValue;
            UpLow := 1;
         END
      ELSE
         BEGIN
            MinX := XValue;
            UpLow := 2;
         END;

      ShowXValue(UpLow,XValue);
      ShowYValue(UpLow,ThisYValue);
      IF UpLow=1 THEN
         upYValue:=ThisYValue
      ELSE
         lowYValue:=ThisYValue;
      ShowCounter(searchCounter);
   END;

FUNCTION ReadyToExit(searchCounter:integer; maxX,minX:real): boolean;
   BEGIN
      ReadyToExit := false;
      IF AbortedSearch THEN ReadyToExit := true;
      IF (SearchCounter>MAXCOUNT-1) THEN ReadyToExit := true;
      IF ((MaxX-MinX)<XSTEP) THEN ReadyToExit := true;
      IF (abs(upYValue)<YSTEP) THEN ReadyToExit := true;
      IF (abs(lowYValue)<YSTEP) THEN ReadyToExit := true;
   END;

PROCEDURE SearchForZero(YName,XName:String; lowestV:Real; VAR XValue:real);
   VAR
      searchCounter : Integer;
      maxX,minX     : Real;
      nValue        : Integer;
   BEGIN
      IF xStep<macheps THEN xStep := macheps;
      IF yStep<macheps THEN yStep := macheps;
      AbortedSearch := false;
      InitializeSearch(YName,XName,lowestV,xValue,nValue,
                       searchCounter,maxX,minX);
      IF NOT AbortedSearch THEN
         BEGIN
            WHILE NOT ReadyToExit(searchCounter,maxX,minX) DO
               NarrowBounds(xValue,nValue,searchCounter,
                            maxX,minX);
            IF upYValue<>lowYValue THEN
               XValue := (upYValue*MinX-lowYValue*MaxX)/(upYValue-lowYValue)
            ELSE XValue := (MaxX+MinX)/2;
         END;
   END;

PROCEDURE FastSearchForZero(minX,maxX:real; VAR XValue:real);
   VAR
      searchCounter: integer;
      temp,thisYvalue: real;
      upLow: integer;
   BEGIN
      IF xStep<macheps THEN xStep := macheps;
      IF yStep<macheps THEN yStep := macheps;
      abortedSearch := false;
      searchCounter := 0;
      IF MinX>MaxX THEN
         BEGIN
            Temp := MaxX;
            MaxX := MinX;
            MinX := Temp;
         END;
         upYValue := FuncFastYValue(maxX);
         lowYValue:= FuncFastYValue(minX);

      WHILE NOT ReadyToExit(searchCounter,maxX,minX) DO
         BEGIN
            searchCounter := searchCounter+1;
            {IF upYValue<>lowYValue THEN
               XValue := (upYValue*MinX-lowYValue*MaxX)/(upYValue-lowYValue)
            ELSE}
            XValue := (MaxX+MinX)/2;

            ThisYValue := FuncFastYValue(XValue);
            IF (ThisYValue*upYValue)>=0 THEN
               BEGIN
                  MaxX := XValue;
                  UpLow := 1;
               END
            ELSE
               BEGIN
                  MinX := XValue;
                  UpLow := 2;
               END;
         END;    {while not readyToExit}
  END;

BEGIN
END.