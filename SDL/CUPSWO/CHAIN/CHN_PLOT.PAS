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

Unit Chn_Plot;
{$R-}
Interface
Uses Chn_Base,CHN_LST,Chn_Gbl,
     Cups,CupsGui,CUPSmupp,CupsGrph,CupsFunc,CupsProc,Crt,Graph;

Type
    PlotPtr=^Plot;

    Plot = Object
          Xo,Yo,Width,Height:           Real;
          SclNum:                       ShortInt;
          Scale:                        ScaleRect;
          AltPlotSize:                  PlotRect;
          HasAtoms,
          Visible,
          ExPanded:                     Boolean;
          PlotAxes:                     Axes;
          ExpandBut:                    ExpandButton;
          PlotSclBut:                   SclButton;
          AttrBut:                      AttributeBut;
          ButtonList:                   List;
          PlotTitle:                    Title;
          Constructor Initialize( ISclNum: Byte; x, y, w, h: Real);
          Destructor Done;                           Virtual;
          Procedure Display;                         Virtual;
          Procedure Remove;                          Virtual;
          Function MouseInside : BOOLEAN;            Virtual;
          Procedure LockFocus;                       Virtual;
          Procedure SetScale(Sx1, Sy1, Sx2, Sy2: Real );
          Procedure ChangeScale;                     Virtual;
          Procedure Expand;                          Virtual;
          Procedure RePlot;   Virtual; {draws inside a plot.  Should be over-ridden}
          Procedure ChangeAttributes;         Virtual;
    End;

   TButton = object
      procedure Create(x,y : real; name_, key_: string);
      function  Clicked : boolean;
   private
      box : rectType;
      name : string;
      pos : pointType;
      key : integer;
      Extended : boolean;
   end;

Procedure RemovePlots;
Procedure RemoveAnalysis;
Procedure DisplayAnalysis;
Procedure UpDate;


VAR
   SourceList:          List;


Implementation

Uses Chn_Anly,Chn_Graf;

{------------------------ TButton type ---------------------------------}

PROCEDURE Error(errorStr : string);
VAR i : integer;
BEGIN
   ErrorStr := 'Error: '+ErrorStr;
   i := GetGraphMode;
   IF GraphResult=0 THEN announce(errorStr) ELSE writeln(errorStr);
   PAUSE;
   HALT;
END;

PROCEDURE SetRect(VAR r : rectType; x1,y1,x2,y2 : integer);
BEGIN
   r.x1:=x1; r.y1:=y1; r.x2:=x2; r.y2:=y2;
END;

PROCEDURE SetPoint(VAR p : PointType; x,y : integer);
BEGIN
   p.x:=x; p.y:=y;
END;

FUNCTION PtInside(var p:pointType;var r:rectType) : boolean;
BEGIN
    PtInside := (p.x>r.x1) AND (p.x<r.x2) AND (p.y>r.y1) AND (p.y<r.y2);
END;

procedure TButton.Create(x,y : real; name_,key_ : string);
var
   m,i : integer;
   envir : TEnvironment;
CONST
   KeyName: ARRAY[1..23] OF STRING[6] =
      ('F1','F2','F3','F4','F5','F6','F7','F8','F9','F10','UP','DOWN','LEFT',
      'RIGHT','PGUP','PGDN','END','HOME','INS','DEL','TAB','ESC','RETURN');
   KeyNum : ARRAY[1..23] OF byte =
      (59,60,61,62,63,64,65,66,67,68,72,80,75,77,73,81,79,71,82,83,09,27,13);
BEGIN
   FOR m:=1 TO length(key_) DO key_[m] := upcase(key_[m]);
   m := 0;
   Extended := false;
   if (length(key_)=1) and (ord(key_[1])>=65) and (ord(key_[1])<=90) then
      Key := ord(key_[1])
   ELSE begin
      REPEAT inc(m) UNTIL (key_=KeyName[m]) OR (m=24);
      IF m=24 THEN
         ERROR('TButton.Create: '+key_+' is an invalid key name')
      else key := KeyNum[m];
      Extended := (m<21);
   end;
   name := Name_;
   for i:=length(name_) downto 1 do
    IF (name_[i]=' ') then Delete(name_,i,1)    {delete spaces}
    else name_[i] := upCase(name_[i]);          {change to uper-case}
   IF (name_='OK')     THEN name := '  OK  ';
   IF (name_='CANCEL') THEN name := 'Cancel';
   IF (name_='HELP')   THEN name := ' Help ';
   envir.Save;
   envir.Standardize;
   viewPortNumber := 0;
   SetRect( box, round(x*GetMaxX), round((1-y)*GetMaxY-RowHt+5),
                 round(x*GetMaxX+(length(name)+1)*ColWidth), round((1-y)*GetMaxY));
   HideMouse;
   SetFillStyle(solidFill,LightGray);
   SetColor(black);
   if name='  OK  ' then SetLineStyle(SolidLn,0,ThickWidth);
   WITH box DO bar3D( x1,y1,x2,y2,3,true);
   SetLineStyle(SolidLn,0,NormWidth);
   SetTextJustify(CenterText,CenterText);
   SetColor(black);
   WITH box DO OutTextXY((x2+x1) DIV 2, (y2+y1) DIV 2+1, name);
   ShowMouse;
   Envir.Reset;
End;

function TButton.Clicked : boolean;
var
   e : pointType;
begin
   e.x:=event.x;  e.y:=event.y;
   with event do
   clicked := (MouseClicked AND PtInside(e,box)) OR
              (KeyPressed and (ExtendedKey=Extended) and
              (ord(ReadKey)=key));
END;

{------------------------------------------------------------------------}

{****************************************************************************}
{Graphics OutPut Object                                                      }
{****************************************************************************}


Procedure RemovePlots;
Begin;
SelectViewport(0);
SetFillStyle(SolidFill,Black);
Bar(0,Round(0.045*GetMaxy),GetMaxX,Round(0.95*GetMaxy));
   {Bar seems faster!
    Anly1.remove;
    Anly2.remove;
    TheChain.remove;}
End;

Procedure RemoveAnalysis;

Begin;
      Anly1.Remove;
      Anly2.Remove;
End;
Procedure DisplayAnalysis;

Begin;
      Anly1.Display;
      Anly2.Display;
End;

Procedure UpDate;

Begin;
      If Anly1.visible  Then Anly1.Display;
      If Anly2.Visible  Then Anly2.Display;

      If TheChain.Visible Then
        Begin
        TheChain.Display;
        TheChain.LockFocus;
        ShowEnergy;
        End;
End;



Constructor Plot.Initialize( ISclNum: Byte; x, y, w, h: Real);

Var
   i:   ShortInt;             

Begin;
      PlotAxes.Initialize( @Self );
      PlotAxes.Visible:=TRUE;
      With Scale do
           begin;
                 xMin:=0;
                 yMin:=-1;
                 xMax:=10;
                 yMax:=1;
           end;
      Xo:=x;
      Yo:=y;
      Width:=w;
      Height:=h;
      SclNum:=ISclNum;
      DefineViewPort(1, x, x + w, y, y + h );
      Visible:=True;
      Expanded:=False;
      With AltPlotSize do
           begin;
                 Left:=0.1;
                 Bottom:=0.1;
                 HSize:=0.8;
                 VSize:=0.8;
           end;
      ButtonList.Init;
      plotTitle.Initialize(@self);
End;

Destructor Plot.Done;

Begin;
      Self.ButtonList.Done(False);     {This May Be A Bug - Buttons be True}
      Self.Remove;
      ButtonList.Remove;
      PlotAxes.Done;
      PlotTitle.Done
End;

Procedure Plot.LockFocus;

Begin;
      DefineViewPort( 1, Xo, Xo + Width, Yo, Yo + Height );
      SelectViewPort( 1 ); 
      With Scale do DefineScale( SclNum,  xMin, xMax,yMin, yMax  );
      SelectScale( SclNum );
End;


Procedure Plot.Display;

Begin;
     if Not Visible Then Exit;
      LockFocus;
      HideCursor;
      SelectViewport( 1 );
      OpenViewPort( 1 );
      PlotAxes.Display;
      Self.ButtonList.Display;
      PlotTitle.Display;
      ShowCursor;
End;

Procedure Plot.Remove;


Begin;
      LockFocus;
      HideCursor;
      If HasAtoms Then AtomList.Remove;
      PlotAxes.Remove;
      ButtonList.Remove;
      PlotTitle.Remove;
      CloseViewport( 1 );
      ShowCursor;
End;

Procedure Plot.SetScale(Sx1, Sy1, Sx2, Sy2: Real );

Begin; 
       With Scale do
            Begin;
                  xMin:=Sx1;
                  yMin:=Sy1;
                  xMax:=Sx2;
                  yMax:=Sy2;
                  DefineScale( SclNum ,  xMin,xMax, yMin, yMax );
            End;
       SelectScale( SclNum );
       PlotAxes.SetUp;
End;
{****************************************************************************}
Function Plot.MouseInside : BOOLEAN;

Var
   xpos, ypos:  REAL;
   inside:      BOOLEAN;

Begin;
      LockFocus;
      MouseInside:=False;
      If Event.MouseClicked then
         begin;
               If not(visible) then exit;
               MousePosn(xpos,ypos, 1, SclNum, Inside);
               If Inside then MouseInside:=True;
         end;
End;
{****************************************************************************}
Procedure Plot.ChangeScale;
Begin;
     If NOT(HasAtoms) THEN
     Announce('Programming BUG.  ChangeScale should be cast to either Analysis for Graph Plot types.');
End;
Procedure Plot.Replot;
Begin;
     If NOT(HasAtoms) THEN
     Announce('Programming BUG.  ReplotAnalysis should be cast to either Analysis for Graph Plot types.');
End;

Procedure Plot.Expand;

Var
   TempSize:        PlotRect;

Begin;
      RemovePlots;
      LockFocus;
      Expanded:=not(Expanded);
      TempSize:=AltPlotSize;
      AtomList.Remove;
      PlotAxes.Remove;
      Remove;
      HideCursor;

      With TempSize do DefineViewPort( 1, Left, Left+hSize, Bottom, Bottom+vSize );
      With AltPlotSize do
           begin;
                 Left:=Xo;
                 Bottom:=Yo;
                 hSize:=width;
                 vSize:=height;
           end;
     With TempSize do
          begin;
                Xo:=left;
                Yo:=bottom;
                width:=hSize;
                height:=vSize;
          end;
     If Not(Expanded) then
          Begin {All plots will show since we have just contracted}
          TheChain.Visible:=TRUE;
          Anly1.Visible:=TRUE;
          Anly2.Visible:=TRUE;
          Update;
          End
        else {Set All plots to not visible except self.  Then replot.}
        begin;
             Anly1.Visible:=FALSE;
             Anly2.Visible:=FALSE;
             TheChain.Visible:=FALSE;
             self.Visible:=TRUE;
             LockFocus;
             Display;
             PlotAxes.Display;
        end;
     ShowCursor;
End;

Procedure Plot.ChangeAttributes;

Begin;
      Announce('Programming Error->wrong object referenced.');
End;
end.