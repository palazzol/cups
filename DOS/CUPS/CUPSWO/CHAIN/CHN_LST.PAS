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

UNIT Chn_LST;
{$R-}
INTERFACE

USES
   Chn_Gbl,Chn_Base,Cups,CupsGui,CUPSmupp,CupsGrph,CupsFunc,CupsProc,Crt,Graph;

TYPE
    ColourType = (SameAll, SameCell, Cycle);

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
         Procedure SetCellData(CItem,CNum: Integer; All:Boolean; Nm,Ny,Nv,Nk1,Nk2,Nk3,Nd,Ns: Real);
         Procedure GetAtom(AtmNumber: Integer; Var Nm,Ny,Nv,Nk1,Nk2,Nk3,Nd,Ns: Real);
         Procedure GetAnAtom(AtmNumber: Integer; Var anatom: BasePtr);
         Procedure SetAtom(AtmNumber: Integer; Nm,Ny,Nv,Nk1,Nk2,Nk3,Nd,Ns: Real);
         Procedure GetAtomInspector;
         Procedure OutLine;
         Procedure NotOutLine;
         Procedure RadSize( choice: ShortInt; rad: Real );
         Procedure UpdateDVec;
         Procedure UpdateList;
         Procedure Remove;
         Procedure ReturnClickedItem( Var clickedItemPtr: BasePtr );
         Procedure AdjustDvec;
         Procedure ChangeColour( ColorTyp: ColourType; Acolour: Word );
         Function  IsItASource( num: Integer ): Boolean;
         Function  GetItemPtr(num:Integer):BasePtr;
End;

Var AtomList:   List;

Implementation

Uses Chn_Plot;
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
   Nptr:         NodePtr;

Begin;
      While Nodes <> Nil do
            begin;
                  Nptr:=Nodes;
                  Nptr^.item^.Done;
                  Dispose(Nptr^.item);
                  Nodes:=Nptr^.Next;
                  Dispose(Nptr);
            end;
End;

Procedure List.AddItem( anItem: BasePtr );

Var
   NewNode:  NodePtr;
   MemBlock: LongInt;
   MemReq,MaxBlock:   String;

Begin;
  MemBlock:= SizeOf(NewNode);
  IF (MemBlock > MaxAvail) THEN
  BEGIN
     Str(MemBlock:10,MemReq);
     Str(MaxAvail:10,MaxBlock);
     Error('List.AddItem: Request for node mem '+ MemReq +
               ' exceeds free memory ' + MaxBlock);
   END
   ELSE
     Begin
     New(NewNode);
     NewNode^.Item:=anitem;
     NewNode^.Next:=Nodes;
     Nodes:=NewNode;
     End;
End;

Procedure List.DeleteItem( anItem: BasePtr );

Var
   Initial,Previous:  NodePtr;

Begin;
      Initial:=nodes;
      Previous:=Nil;
      While Initial^.Item <> anItem do
            begin;
                  Previous:=Initial;
                  Initial:=Initial^.next;
            end;
      If not(Previous=Nil) then
         Previous^.Next:=Initial^.Next
      else
          Nodes:=Initial^.Next;
      Dispose( Initial );

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

Function  List.GetItemPtr(num:Integer):BasePtr;
Var
   anItem: NodePtr;

Begin;
      anItem:=nodes;
      While (anItem <> Nil) And (num<>anItem^.Item^.parAtm.num) do
            begin;
                  anItem:=anItem^.next;
            end;
GetItemPtr:=anItem^.Item;
End;
Procedure List.SetCellData(CItem,CNum: Integer; All:Boolean; Nm,Ny,Nv,Nk1,Nk2,Nk3,Nd,Ns: Real);

Var
   anItem:      NodePtr;
   dummy,
   i,j:         Integer;
   SelectCell:  Real;

Begin;
{CNum is the cell that will change if ALL is not selected.}
      i:=yVec.Length-1;
      anItem:=nodes;
      dummy:=yvec.length;
      While anItem <> Nil do
            begin;
                  if ((i>0) and (i<(Yvec.Length-1))) then
                     begin;
                           SelectCell:=(i Mod (Cell))/(CItem);
                           if SelectCell=0 then SelectCell:=(Cell/CItem);
                           if SelectCell=1 then
                           if All or (((i-1) Div Cell + 1)= CNum) then
                              begin;
                                    anItem^.Item^.ParAtm.y:=Ny;
                                    anItem^.Item^.ChangeData(Nm,Nv,Nk1,Nk2,Nk3,Nd,Ns);
                              end;
                     end;
                  AnItem:=anItem^.next;
                  Dec(i);
          end;
      UpdateDVec;
End;

Procedure List.GetAtom(AtmNumber: Integer; Var Nm,Ny,Nv,Nk1,Nk2,Nk3,Nd,Ns: Real);

Var
   anItem: NodePtr;
   i:      Integer;
Begin;
      i:=yVec.Length-1;
      anItem:=nodes;
      While anItem <> Nil do
            begin;
                  If i=AtmNumber then
                     begin;
                           With anItem^.Item^.ParAtm do
                                begin;
                                      Ny:=y;
                                      Nv:=v;
                                      Nm:=m;
                                      Nk1:=k1;
                                      Nk2:=K2;
                                      Nk3:=K3;
                                      Nd:=Damp;
                                      Ns:=Self;
                                end;
                           exit;
                     end;
                  anItem:=anItem^.next;
                  Dec(i);
            end;
End;

Procedure List.GetAnAtom(AtmNumber: Integer; Var anatom: BasePtr);

Var
   anItem: NodePtr;

Begin;
      anItem:=nodes;
      While anItem <> Nil do
            begin;
                  If AtmNumber=anitem^.item^.ParAtm.Num then anatom:=anitem^.item;
                  anItem:=anItem^.next;
            end;
End;

Procedure List.SetAtom(AtmNumber: Integer; Nm,Ny,Nv,Nk1,Nk2,Nk3,Nd,Ns: Real);

Var
   anItem: NodePtr;
   i:      Integer;

Begin;
      i:=0;
      anItem:=nodes;
      While anItem <> Nil do
            begin;
                  If i=AtmNumber then
                     begin;
                           With anItem^.Item^.ParAtm do
                                begin;
                                      y:=Ny;
                                      v:=Nv;
                                      m:=Nm;
                                      k1:=Nk1;
                                      k2:=Nk2;
                                      k3:=Nk3;
                                      Damp:=Nd;
                                      Self:=Ns;
                                end;
                           exit;
                     end;
                  anItem:=anItem^.next;
                  Inc(i);
            end;
End;

Procedure List.GetAtomInspector;

Var
   anItem:    NodePtr;
   anAtomNum,
   i:         Integer;
   Cancel,Ok,
   Leave:     Boolean;
   InputScrn: TInputScreen;

Begin;
      With InputScrn do
           begin;
                 Init;
                 DefineInputPort( 0.38, 0.71, 0.50 ,0.90 );
                 LoadLine('      Examine Atom      ');
                 LoadLine('         Number         ');
                 LoadLine('                        ');
                 LoadLine('         {    }         ');
                 LoadLine('                        ');
                 LoadLine(' You may also select an ');
                 LoadLine(' atom by doubleclicking.');
                 LoadLine('                        ');
                 LoadLine('  [  Ok  ]    [Cancel]  ');

                 SetNumber(1, 1);
                 Repeat
                       AcceptScreen;
                       anAtomNum:=Yvec.Length - trunc(GetNumber(1)) -1;

                       If ((anAtomNum>0) and (anAtomNum<(Yvec.Length-1))) then
                          Leave:=True
                       else Announce('That Atom Number is out of Range.');
                 Until Leave=True;

                 Ok:=GetBoolean(2);
                 Cancel:=GetBoolean(3);

                 Done;
            end;
            If Cancel then exit;
       anItem:=nodes;
      i:=0;
      While anItem <> Nil do
            begin;
                  If i=anAtomNum then
                     begin;
                           anItem^.Item^.Inspector;
                           exit;
                     end;
                  anItem:=anItem^.next;
                  Inc(i);
            end;
End;

Procedure List.OutLine;

Var
   anItem: NodePtr;

Begin;
      anItem:=nodes;
      While anItem <> Nil do
            begin;
                  anItem^.Item^.AtomOutline:=True;
                  anItem:=anItem^.next;
            end;
End;

Procedure List.NotOutLine;

Var
   anItem: NodePtr;

Begin;
      anItem:=nodes;
      While anItem <> Nil do
            begin;
                  anItem^.Item^.AtomOutline:=False;
                  anItem:=anItem^.next;
            end;
End;

Procedure List.RadSize( choice: ShortInt; Rad: Real );

Var
   anItem:         NodePtr;
   TempRad:        Integer;

Begin;
      Case Choice of
      1: TempRad:=Trunc(Rad);
      2: TempRad:=MapX(Rad);
      End;

      anItem:=nodes;
      While anItem <> Nil do
            begin;
                  anItem^.Item^.Radius:=TempRad;
                  anItem:=anItem^.next;
            end;
End;

Procedure List.UpdateDvec;

Var
   anItem:      NodePtr;
   MaxNum,i:    Word;
   j,k:         Integer;
   Two:         Integer;
Begin;
     anItem:=nodes;
     While anItem <> Nil do
           begin;
                  YVec.Put(anItem^.Item^.ParAtm.Num+1, anItem^.Item^.ParAtm.y);
                  VVec.Put(anItem^.Item^.ParAtm.Num+1, anItem^.Item^.ParAtm.v);
                  MVec.Put(anItem^.Item^.ParAtm.Num+1, anItem^.Item^.ParAtm.m);
                  K1vec.Put(anItem^.Item^.ParAtm.Num+1, anItem^.Item^.ParAtm.k1);
                  K2vec.Put(anItem^.Item^.ParAtm.Num+1, anItem^.Item^.ParAtm.k2);
                  K3vec.Put(anItem^.Item^.ParAtm.Num+1, anItem^.Item^.ParAtm.k3);
                  KSvec.Put(anItem^.Item^.ParAtm.Num+1, anItem^.Item^.ParAtm.Self);
                  KDvec.Put(anItem^.Item^.ParAtm.Num+1, anItem^.Item^.ParAtm.Damp);
                  anItem:=anItem^.next;
            end;

     MaxNum:=Yvec.Length;
     if leftB=Continuous
       Then
       Begin
       MVec.VecPtr^[1]:=MVec.VecPtr^[MaxNum-1];
       K1vec.Vecptr^[1]:=K1Vec.VecPtr^[MaxNum-1];
       K2vec.Vecptr^[1]:=K2vec.VecPtr^[MaxNum-1];
       K3vec.Vecptr^[1]:=K3Vec.VecPtr^[MaxNum-1];
       Ksvec.Vecptr^[1]:=KsVec.VecPtr^[MaxNum-1];
       Kdvec.Vecptr^[1]:=KdVec.VecPtr^[MaxNum-1];
       End;
     if leftB=Free Then
       Begin
       K1vec.Vecptr^[1]:=0;
       K2vec.Vecptr^[1]:=0;
       K3vec.Vecptr^[1]:=0;
       End;
     if rightB=Free Then
       Begin
       K1vec.Vecptr^[maxNum-1]:=0;
       K2vec.Vecptr^[maxNum-1]:=0;
       K3vec.Vecptr^[maxNum-1]:=0;
       End;
     For i:=2 to (MaxNum-1) do
                Avec.VecPtr^[i]:=
                Accel(i,YVec.VecPtr^[i-1],YVec.VecPtr^[i],YVec.VecPtr^[i+1],VVec.VecPtr^[i]);
     SourceList.AdjustDvec;
     AdjustForBoundry;
     CalcEnergy;
End;

Procedure List.UpdateList;

Var
   anItem: NodePtr;
   MaxNum:      Integer;

Begin;
      MaxNum:=Yvec.Length;
      anItem:=nodes;
      While anItem <> Nil do
        if (anItem^.Item^.ParAtm.Num =0) OR
           (anItem^.Item^.ParAtm.Num =maxNum-2) OR
           (anItem^.Item^.ParAtm.Num =MaxNum-1)
            Then {These atoms are at the boudnary!}
              Begin
              YVec.Get(anItem^.Item^.ParAtm.Num+1, anItem^.Item^.ParAtm.y);
              VVec.Get(anItem^.Item^.ParAtm.Num+1, anItem^.Item^.ParAtm.v);
              anItem:=anItem^.next
              End
            Else
            begin;
                  YVec.Get(anItem^.Item^.ParAtm.Num+1, anItem^.Item^.ParAtm.y);
                  VVec.Get(anItem^.Item^.ParAtm.Num+1, anItem^.Item^.ParAtm.v);
                  MVec.Get(anItem^.Item^.ParAtm.Num+1, anItem^.Item^.ParAtm.m);
                  K1vec.Get(anItem^.Item^.ParAtm.Num+1, anItem^.Item^.ParAtm.k1);
                  K2vec.Get(anItem^.Item^.ParAtm.Num+1, anItem^.Item^.ParAtm.k2);
                  K3vec.Get(anItem^.Item^.ParAtm.Num+1, anItem^.Item^.ParAtm.k3);
                  KSvec.Get(anItem^.Item^.ParAtm.Num+1, anItem^.Item^.ParAtm.Self);
                  KDvec.Get(anItem^.Item^.ParAtm.Num+1, anItem^.Item^.ParAtm.Damp);
                  anItem:=anItem^.next;
            end;
          CalcEnergy;
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

Procedure List.AdjustDvec;

Var
   Initial:     NodePtr;
   i,Position:  Integer;
   Ytemp,DeltaY: Real;


Begin;
      Initial:=Nodes;
      While Initial <> Nil do
            begin;
                  Position:=Initial^.Item^.ParAtm.Num + 1;
                  If Initial^.Item^.ParSource.Excite=Force then
                     begin
                          Avec.VecPtr^[Position]:=Avec.VecPtr^[Position] +
                          Initial^.Item^.GetSourceValue(time)/Mvec.VecPtr^[Position];
                          Initial^.Item^.Power:=Initial^.Item^.GetSourceValue(time)*Vvec.VecPtr^[Position];
                     end
                  else
                      begin;
                            {Ytemp:=Yvec.VecPtr^[Position];}
                            {yTemp:=Initial^.Item^.ParAtm.y; }
                            yTemp:=Initial^.Item^.GetSourceValue(time-dt);
                            Yvec.VecPtr^[Position]:=Initial^.Item^.GetSourceValue(time);
                            {Initial^.Item^.ParAtm.y:=Yvec.VecPtr^[Position]; }
                            DeltaY:=Yvec.VecPtr^[Position]-Ytemp;

                            i:=Position-1;
                            if i>1 Then Avec.VecPtr^[i]:=
                            Accel(i,YVec.VecPtr^[i-1],YVec.VecPtr^[i],YVec.VecPtr^[i+1],VVec.VecPtr^[i]);

                            i:=Position;
                            Avec.VecPtr^[i]:=
                            Accel(i,YVec.VecPtr^[i-1],YVec.VecPtr^[i],YVec.VecPtr^[i+1],VVec.VecPtr^[i]);

                            i:=Position+1;
                            if i< aVec.Length Then Avec.VecPtr^[i]:=
                            Accel(i,YVec.VecPtr^[i-1],YVec.VecPtr^[i],YVec.VecPtr^[i+1],VVec.VecPtr^[i]);

                            Initial^.Item^.Power:=
                              (-Mvec.VecPtr^[Position])*(Avec.VecPtr^[Position])*(DeltaY/dt);
                      end;
                  Initial:=Initial^.Next;
            end;
End;

Procedure List.ChangeColour( ColorTyp: ColourType; Acolour: Word );

Var
   anItem:      NodePtr;
   counter,i:   Integer;

Begin;
      anItem:=nodes;
      i:=0;
      Counter:=0;
      Remove;
      While anItem <> Nil do
            begin;
                  Case Colortyp of
                       SameAll: anItem^.Item^.AtomColour:=AColour;
                       SameCell:
                                begin;
                                      anitem^.Item^.AtomColour:=Counter;
                                      If (i Mod Cell) = 0 then Inc(Counter);
                                      If Counter=8 then Counter:=1;
                                end;
                       Cycle:
                             begin;
                                   anitem^.Item^.AtomColour:=i;
                                   If i=7 then i:=0;
                             end;
                  End;
                  Inc(i);
                  anItem:=anItem^.next;
            end;
      Display;
End;

Function  List.IsItASource( num: Integer ): Boolean;

Var
   anatom:      BasePtr;

Begin;
      GetAnAtom(num,anatom);
      If anatom^.source=True then IsItASource:=True else IsItASource:=False;
End;

end.
