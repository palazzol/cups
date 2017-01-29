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

{$R-}
UNIT Wav_LST;

INTERFACE

USES
   Wav_Gbl,Wav_Base,Cups,CupsGui,CUPSmupp,CupsGrph,CupsFunc,CupsProc,Crt,Graph;

TYPE
    NodePtr=^Node;
    Node = Record
         Item: BasePtr;
         Next: NodePtr;
    End;

    List = Object
         Nodes:     NodePtr;
         Constructor Init;
         Destructor Done( DestroyItems: Boolean );
         Procedure Empty;
         Procedure AddItem( anItem: BasePtr );
         Procedure DeleteItem( anItem : BasePtr );
         Procedure Display;
         Procedure MakeVisible;
         Procedure SetVisible;
         Procedure UpdateDVec;
         Procedure Remove;
         Procedure ReturnClickedItem( Var clickedItemPtr: BasePtr );
         Procedure OutLine;
         Procedure Filled;
         Procedure DetectorUpdate(ThePlot: pointer);
         Procedure DetectorPlot(ThePlot: pointer; Selection: DisplayType);
         Procedure RePlotDetector(ThePlot:   Pointer;
                                  TheTime:   Dvector;
                                  Selection: DisplayType;
                                  Counter:   Integer);
         Procedure ZeroDarray( theplot: pointer );
         Procedure Clear;
         Procedure CalcGridIndex;
         Procedure ArchiveAll(Var filehandle: Text);
         Function  NumberOfItems: Integer;
         Function  GetItemColor(index:Integer):Word;
         Procedure Sort;
         Procedure RenumberDetectors;
         Procedure InspectAll;
         Function  NoConflict(index:Integer):Boolean;
    End;

Implementation
Uses Wav_Plot,  Wav_SS, Wav_Anly;
{****************************************************************************}
{List Storage Object                                                         }
{****************************************************************************}
Constructor List.Init;

Begin;
      Self.Nodes:=Nil;
End;

Destructor List.Done( DestroyItems: Boolean );
Var
   Initial: NodePtr;

Begin;
      While Nodes <> Nil do
            begin;
                  Initial:=Nodes;
                  If DestroyItems then Dispose(Initial^.Item,Done);
                  Nodes:=Initial^.Next;
                  Dispose(Initial);
            end;
End;

Procedure List.Empty;
Var
   Initial: NodePtr;

Begin;
      While Nodes <> Nil do
            begin;
                  Initial:=Nodes;
                  Dispose(Initial^.Item,Done);
                  Nodes:=Initial^.Next;
                  Dispose(Initial);
            end;
      Nodes:=nil;
End;

Procedure List.Clear;
Var
   anItem: NodePtr;

Begin;
      anItem:=nodes;
      While anItem <> Nil do
            begin;
                  anItem^.Item^.Clear;
                  anItem:=anItem^.next;
            end;
End;
Procedure List.AddItem( anItem: BasePtr );

Var
   NewNode:  NodePtr;

Begin;
      New(NewNode);
      NewNode^.Item:=anitem;
      NewNode^.Next:=Nodes;
      Nodes:=NewNode;
End;

Procedure List.DeleteItem( anItem: BasePtr );

Var
   Initial,Previous:  NodePtr;

Begin;
      Initial:=nodes;
      Previous:=Nil;
      While (Initial^.Item <> anItem) and (initial<>nil) do
            begin;
                  Previous:=Initial;
                  Initial:=Initial^.next;
            end;
      If not(Previous=Nil) then
         Previous^.Next:=Initial^.Next
      else
          Nodes:=Initial^.Next;
      If initial<>nil Then Dispose( Initial ) else announce('Item not in list!');     {make sure the item was in the list!}
End;

Procedure List.Display;

Var
   anItem: NodePtr;

Begin;
      anItem:=nodes;
      While anItem <> Nil do
            begin;
                  anItem^.Item^.Display;
                  anItem:=anItem^.next;
            end;
End;

Procedure List.MakeVisible;

Var
   anItem: NodePtr;

Begin;
      anItem:=nodes;
      While anItem <> Nil do
            begin;
                  anItem^.Item^.isVisible:=True;
                  anItem:=anItem^.next;
            end;
End;


Procedure List.SetVisible;

Var
   anItem: NodePtr;

Begin;
      anItem:=nodes;
      While anItem <> Nil do
            begin
            Case EquationType of
              SineGordon,
              KleinGordon,
              doubleSineGordon,
              phiFour: if (typeOf(anItem^.Item^)=typeOf(SegmentButton)) or
                          (typeOf(anItem^.Item^)=typeOf(sourceButton))  or
                          (typeOf(anItem^.Item^)=typeOf(reflectorButton))then
                           anItem^.Item^.isVisible:=False;
              Diffusion:if typeOf(anItem^.Item^)=typeOf(ReflectorButton) then
                       anItem^.Item^.isVisible:=False;
              Schroedinger:if (typeOf(anItem^.Item^)=typeOf(ReflectorButton))or
                              (typeOf(anItem^.Item^)=typeOf(reflectorButton))or
                              (typeOf(anItem^.Item^)=typeOf(sourceButton)) then
                       anItem^.Item^.isVisible:=False;
            end;{of Case}
            anItem:=anItem^.next;
            end;
End;

Function  List.GetItemColor(index:Integer):Word;
Var
   anItem: NodePtr;

Begin
      anItem:=nodes;
      While (anItem <> Nil) and (index>0) do
            begin;
                  Dec(index);
                  GetItemColor:=anItem^.Item^.objColor;
                  anItem:=anItem^.next;
            end;
End;

Function  List.NoConflict(index:Integer):Boolean;
Var
   anItem: NodePtr;

Begin;
      NoConflict:=TRUE;
      anItem:=nodes;
      While anItem <> Nil do
            begin;
                  If (anItem^.Item^.GridIndex=Index)Then NoConflict:=FALSE;
                  anItem:=anItem^.next;
            end;
End;

Procedure List.CalcGridIndex;
Var
   anItem: NodePtr;
Begin;
      anItem:=nodes;
      While anItem <> Nil do
            begin;
                  anItem^.Item^.CalcGridIndex;
                  anItem:=anItem^.next;
            end;
End;

Procedure List.InspectAll;
Var
   anItem: NodePtr;
   active:Boolean;
Begin;
      anItem:=nodes;
      While anItem <> Nil do
            begin
            {Inspector will return FALSE if object has been destroyed.}
            active:=anItem^.Item^.Inspector;
            anItem:=anItem^.next;
            end;
End;

Procedure List.OutLine;

Var
   anItem: NodePtr;

Begin;
      anItem:=nodes;
      While anItem <> Nil do
            begin;
                  anItem^.Item^.Outlined:=True;
                  anItem:=anItem^.next;
            end;
End;

Procedure List.Filled;

Var
   anItem: NodePtr;

Begin;
      anItem:=nodes;
      While anItem <> Nil do
            begin;
                  anItem^.Item^.Outlined:=False;
                  anItem:=anItem^.next;
            end;
End;


Procedure List.UpdateDvec;

Var
   anItem: NodePtr;

Begin;
      anItem:=nodes;
      While anItem <> Nil do
            begin;
                  SourcePtr(anItem^.Item)^.AddWaveToDVec;
                  anItem:=anItem^.next;
            end;
End;

Procedure List.Remove;

Var
   anitem: NodePtr;
   
Begin;
       anItem:=nodes;
       While anItem <> Nil do
             begin;
                   anItem^.Item^.Remove;
                   anItem := anItem^.next;
             end;
End;

Procedure List.ReturnClickedItem( Var clickedItemPtr: BasePtr );

Var
   Initial, PreCursor:          NodePtr;
   Equal:                       Boolean;

Begin; 
      Initial:=Nodes;
      Equal:=False;
      While (Initial <> Nil) and not(Equal) do
            begin;
                  Equal:=Initial^.item^.MouseInside;
                  PreCursor:=Initial;
                  Initial:=Initial^.next;
            end;
   If Equal then ClickedItemPtr:=PreCursor^.item
            else ClickedItemPtr:=NIL;
End;

Procedure List.DetectorUpdate(ThePlot: pointer);

Var
   anItem:              NodePtr;
   Right,Left,Magnitude:      Real;
   Frac:                Real;
   i,j:                 Integer;

Begin;
      anItem:=nodes;
      While anItem <> Nil do
            begin;
                  With AnlyPlotPtr(ThePlot)^ do
                   With anItem^.Item^.ParDetector do
                       begin
                            anItem^.Item^.GetDetectorValues(Right,Left,Magnitude);
                            Darray[Num].Put(AnalysisCounter,Right);
                            Darray[Num+MaxDetNum].Put(AnalysisCounter,Left);
                            Darray[Num+2*MaxDetNum].Put(AnalysisCounter,Magnitude);
                       end;
                  anItem:=anItem^.next;
            end;
End;

Procedure List.DetectorPlot(ThePlot: Pointer; Selection: DisplayType);

Var
   anItem:              NodePtr;
   i:                   Integer;
   xPix1,xPix2:         Word;
Begin;
      if NOT(AnlyPlotPtr(ThePlot)^.isVisible) then exit;
      anItem:=nodes;
      While anItem <> Nil do
            begin;
                  With AnlyPlotPtr(ThePlot)^ do
                   With anItem^.Item^.ParDetector do
                          begin;
                                i:=AnalysisCounter;
                                xPix1:=MapX(Tvec.VecPtr^[i-1]);
                                xPix2:=MapX(Tvec.VecPtr^[i]);
                                SetColor( anItem^.Item^.objColor);
                                LockFocus;
                                Case Selection of
                                   showRight: Line(xPix1,MapY(Darray[Num].VecPtr^[i-1]),
                                                  xPix2,MapY(Darray[Num].VecPtr^[i]));
                                   showLeft:  Line(xPix1,MapY(Darray[Num+MaxDetNum].VecPtr^[i-1]),
                                                  xPix2,MapY(Darray[Num+MaxDetNum].VecPtr^[i]));
                                   showTotal:   Line(xPix1,MapY(Darray[Num+2*MaxDetNum].VecPtr^[i-1]),
                                                  xPix2,MapY(Darray[Num+2*MaxDetNum].VecPtr^[i]));
                                End{of Case}
                          end;
                  anItem:=anItem^.next;
            end;
End;

Procedure List.RePlotDetector(ThePlot:   Pointer;
                              TheTime:   Dvector;
                              Selection: DisplayType;
                              Counter:   Integer);

Var
   anItem:              NodePtr;

Begin;
      anItem:=nodes;
      While anItem <> Nil do
            begin;
                  With anItem^.Item^.ParDetector do
                       begin
                             With AnlyPlotPtr(ThePlot)^ do
                               begin

                                            LockFocus;
                                            SetColor( anItem^.Item^.objColor);

                                            Case Selection of
                                                 showRight: PlotDvectors(TheTime,Darray[Num],1,Counter);
                                                 showLeft:  PlotDvectors(TheTime,Darray[Num+MaxDetNum],1,Counter);
                                                 showtotal: PlotDvectors(TheTime,Darray[Num+2*MaxDetNum],1,Counter);
                                            End; {of Case}

                               end; {of With}
                       end; {of With}
                  anItem:=anItem^.next;
            end; {of While}
End;


Procedure List.ZeroDarray(thePlot: Pointer);

Var
   anitem: NodePtr;

Begin;
       anItem:=nodes;
       While anItem <> Nil do
             begin;
                   With AnlyPlotPtr(ThePlot)^ do
                    With anItem^.Item^.ParDetector do
                        begin
                             If not(Freeze) then
                                begin
                                     Darray[Num].Fill(0);
                                     Darray[Num+MaxDetNum].Fill(0);
                                     Darray[Num+2*MaxDetNum].Fill(0);
                                end;
                        end;
                   anItem := anItem^.next;
             end;
End;

Procedure List.ArchiveAll(Var filehandle: Text);

Var
   anitem: NodePtr;
   
Begin;
       anItem:=nodes;
       While anItem <> Nil do
             begin;
                   anItem^.Item^.Archive(filehandle);
                   anItem := anItem^.next;
             end;
End;

Function List.NumberOfItems: Integer;

Var
   anitem: NodePtr;
   aNum:   Integer;
   
Begin;
      aNum:=0;
      anItem:=nodes;
      While anItem <> Nil do
            begin;
                  Inc(aNum);
                  anItem := anItem^.next;
            end;
      NumberOfItems:=aNum;
End;

Procedure List.Sort;

Var
   aNode: NodePtr;
   tempBasePtr: BasePtr;
   swap:Boolean;
   DUMMY:Real;
Begin;
  Repeat
    swap:=FALSE;
    aNode:=nodes;
    While (aNode <> Nil) AND (aNode^.next <> Nil) do
            begin;
            dummy:=aNode^.Item^.GridIndex;
            dummy:=aNode^.next^.item^.GridIndex;
            If (aNode^.Item^.GridIndex >  aNode^.next^.item^.GridIndex) then
                    Begin
                    tempBasePtr:=aNode^.Item;
                    aNode^.Item:=aNode^.next^.item;
                    aNode^.next^.item:=tempBasePtr;
                    swap:=TRUE;
                    End ;
                  aNode:=aNode^.next;
            end;
  Until swap=FALSE;
End;

Procedure List.RenumberDetectors;

Var
   anItem: NodePtr;
   i:      Integer;
   TheNum: Integer;

Procedure ResetAnalysis( ThePlot: AnlyPlotPtr );

Var
   i:   Integer;

Begin
      With ThePlot^ do
           If (AnalysisType=YvsT) then
              begin
                   For i:=1 to 3*MaxDetNum do
                       begin
                            Darray[i].Free;
                            Darray[i].Init(1);
                       end;
                   Tvec.Free;
                   Tvec.Init(1);

                   {AnalysisType:=Disabled;}
                   PlotAxes.Remove;
                  { PlotAxes.IsVisible:=False;   removed by wc.}
                   ClearScreen;
              end;
End;
Begin;
      TheNum:=NumberofItems;

      i:=0;

      ResetAnalysis(@Anly1);
      ResetAnalysis(@Anly2);

      anItem:=nodes;
      While anItem <> Nil do
            begin
                 With anItem^.Item^.ParDetector do
                      begin
                           Num:=TheNum - i;
                           anItem^.Item^.objColor:=TheNum - i;
                      end;
                 Inc(i);
                 anItem:=anItem^.next;
            end;

      Dec(DetectNum);
End;

end.
