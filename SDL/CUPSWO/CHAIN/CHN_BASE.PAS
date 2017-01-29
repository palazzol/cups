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

UNIT Chn_Base;
{$R-}
INTERFACE

USES
   Chn_Gbl,Cups,CupsGui,CUPSmupp,CupsGrph,CupsFunc,CupsProc,Crt,Graph;

TYPE

    BasePtr=^Basis;

    AnalysisArray = Array[1..5] of BasePtr;

    AxesPtr=^Axes;

    Basis = Object
          Visible,
          Source,
          AtomOutline:                     Boolean;
          AtomColour:                      Word;
          Radius:                          Integer;
          ParAtm:                          AtomInfo;
          ParSource:                       SourceInfo;
          Power:                           Real;
          ParentPointer:                   pointer;
          ObjColor:                        Word;

          Constructor Initialize;
          Destructor Done;                                           Virtual;
          Procedure Display;                                         Virtual;
          Procedure Remove;                                          Virtual;
          Function MouseInside: Boolean;                             Virtual;
          Procedure Drag;                                            Virtual;
          Procedure Inspector;                                       Virtual;
          Procedure ChangeData( nm, nv, nk1, nk2, nk3,nd,ns: Real ); Virtual;
          Function  GetSourceValue(t:Real): Real;                    Virtual;
    End;

    Title = Object(Basis)
      titleStr:   String;
      Constructor  Initialize( Plot : pointer);
      Destructor   Done;     Virtual;
      Procedure    Display;               Virtual;
      Procedure    Remove;                Virtual;
      Procedure    SetStr(aString:String);
      Procedure    SetTitleColor(theColor:Word);
    End;

    Button = Object( basis )
          Ix,Iy,w,h:        Real;   
          ButtonNum:        ShortInt;
          box:              Array[1..4] of PointType;
          Constructor Initialize( Plot : pointer; theButton: ShortInt );
          Destructor Done;                  Virtual;
          Procedure Display;                Virtual;
          Procedure Remove;                 Virtual;
          Procedure DrawIcon;               Virtual;
          Function MouseInside: BOOLEAN;    Virtual;
          Procedure Action;                 Virtual;
          Procedure CalculateBox;           Virtual;
   End;

   CornerButton = Object( Chn_Base.Button )
                CornerNum:   ShortInt;
                Constructor Initialize( Plot: pointer; theCorner: Integer );
                Destructor Done;            Virtual;
                Procedure CalculateBox;     Virtual;
   End;

   SclButton = Object( Chn_Base.CornerButton )
              Constructor Initialize( Plot: pointer );
              Destructor Done;               Virtual;
              Procedure Action;              Virtual;
              Procedure DrawIcon;            Virtual;
   End;

   ExpandButton = Object( Chn_Base.CornerButton )
              Constructor Initialize( Plot: pointer);
              Destructor Done;               Virtual;
              Procedure Action;              Virtual;
              Procedure DrawIcon;            Virtual;
   End;

   AttributeBut = Object( Chn_Base.CornerButton )
              Constructor Initialize( Plot: pointer);
              Destructor Done;               Virtual;
              Procedure Action;              Virtual;
              Procedure DrawIcon;            Virtual;
   End;

   Axes = Object( Chn_Base.basis )
         AxesTic:         AxesRect;
         ShowCell:        Boolean;
         Constructor Initialize( Plot: pointer );
         Procedure SetUp;
         Destructor Done;                            Virtual;
         Procedure Display;                          Virtual;
         Procedure Remove;                           Virtual;
    End;

    AtomPtr=^Atom;

    Atom = Object( basis )
         Outlined:            Boolean;
         OutRect:             Array[1..4] of PointType;
         Constructor Initialize(Anum: Word; Ay: Real; Plot: pointer );
         Destructor Done;                                              Virtual;
         Procedure ChangeData( nm, nv, nk1, nk2, nk3 ,nd, ns: Real );  Virtual;
         Procedure ChangeY( Newy: Real );
         Procedure ShowVelocity( colour:    Word );
        { Procedure DrawEnvelope( colour:    Word ); }
         Procedure Display;                                            Virtual;
         Procedure Remove;                                             Virtual;
         Function  MouseInside: BOOLEAN;                               Virtual;
         Procedure Drag;                                               Virtual;
         Procedure Inspector;                                          Virtual;
         Procedure MakeSource;
         Function  GetSourceValue(t:Real): Real;                       Virtual;
    End;


    Procedure VariableInit;
    Procedure VariableDone;

Var

   MaxData:       Word;

IMPLEMENTATION

USES Chn_plot,CHN_LST,Chn_Anly;

{****************************************************************************}
{Various Porcedures and Functions                                            }
{****************************************************************************}


Procedure VariableInit;

Var
   i:   ShortInt;

Begin;
      YVec.Init(InitNum);
      VVec.Init(InitNum);
      YInitVec.Init(InitNum);
      VInitVec.Init(InitNum);
      MVec.Init(InitNum);
      K1vec.Init(InitNum);
      K2vec.Init(InitNum);
      K3vec.Init(InitNum);
      KSvec.Init(InitNum);
      KDvec.Init(InitNum);
      Avec.Init(InitNum);
      Yold.Init(InitNum);
      Time:=0;
      SourcePeriod:=50;
      Energy:=0;
      KineticEnergy:=0;
      PotentialEnergy:=0;
      dT:=0.1;
      AnimationSpeed:=-8;
      Running:=False;
      Loading:=False;
      TPlus:=True;
      ShowV:=False;
      ShowEnvelope:=False;
      LeftB:=Continuous;
      RightB:=Continuous;
      MassScaling:=True;
      Cell:=4;
      SourceList.Init;
      SingleStep:=False;
      With ModeData do
           begin;
                 Amp1:=0.5;B1:=False;
                 Amp2:=0.5;B2:=False;
                 Amp3:=0.5;B3:=False;
                 Amp4:=0.5;B4:=True;
                 Amp5:=0.5;B5:=False;
                 Amp6:=0.5;B6:=False;
                 Amp7:=0.5;B7:=False;
                 Mode6:=6;
                 Mode7:=7;
           end;
End;

Procedure VariableDone;

Begin;
      YVec.Free;
      VVec.Free;
      YInitVec.Free;
      VInitVec.Free;
      MVec.Free;
      K1vec.Free;
      K2vec.Free;
      K3vec.Free;
      KSvec.Free;
      KDvec.Free;
      Avec.Free;
      Yold.Free;
      SourceList.Done(False);
End;
{****************************************************************************}
{Base Object for All Other Objects                                           }
{****************************************************************************}
Constructor Basis.Initialize;

Begin;
      Visible:=True;
      With ParAtm do
           begin;
                 Num:=1;
                 y:=0;
                 m:=0;
                 v:=0;
                 k1:=0;
                 k2:=0;
                 k3:=0;
           end;
      Power:=0;
      With ParSource do
           begin;
                 Kind:=NoSource;
                 Excite:=Displacement;
                 Periodic:=True;
                 Freq:=0;
                 Amp:=0;
                 A:=0;
                 Xo:=0;
                 FuncStr:='';
           end;
      ParentPointer:=Nil;
      If AtOutline
        Then AtomOutline:=TRUE
        Else AtomOutline:=FALSE;
      Source:=False;
      Radius:=4;
End;

Destructor Basis.Done;
Begin;
      {Self.Remove;}
      ParentPointer:=Nil;
End;

Procedure Basis.Display;
Begin;
End;

Procedure Basis.Remove;
Begin;
End;

Function Basis.MouseInside: Boolean;
Begin;
End;

Procedure Basis.Drag;
Begin;
End;

Procedure Basis.Inspector;
Begin;
End;

Procedure Basis.ChangeData( nm, nv, nk1, nk2, nk3, nd, ns: Real );
Begin;
End;

Function  Basis.GetSourceValue(t:Real): Real;
Begin;
End;
{****************************************************************************}
{Title Object                                                                 }
{****************************************************************************}
Constructor Title.Initialize( Plot: pointer);

Begin;
      Basis.Initialize;
      ParentPointer:=PlotPtr(Plot);
      objColor:=Magenta;
      TitleStr:='Title';
      Visible:=True;
End;

Procedure Title.SetTitleColor(theColor:Word);
Begin
  objColor:=theColor;
End;

Procedure Title.Remove;
Var
   OldColour,i:   Word;
   x,y:Real;
Begin;
      PlotPtr(parentPointer)^.LockFocus;
      OldColour:=GetColor;
      x:=PlotPtr(parentPointer)^.Xo+PlotPtr(parentPointer)^.width/2-Length(titleStr)*colWidth/GetMaxX/2;
      y:=PlotPtr(parentPointer)^.Yo+PlotPtr(parentPointer)^.height+1.5/80;
      SelectViewPort(0);
      RuboutXY(x,y,Length(TitleStr),Black);
      SetColor( OldColour );
      PlotPtr(parentPointer)^.LockFocus;
End;
Procedure Title.SetStr(aString:String);
Begin
  TitleStr:=aString;
End;
Procedure Title.Display;
Var
   OldColor,i:   Word;
   x,y:Real;
Begin
  PlotPtr(parentPointer)^.LockFocus;
  OldColor:=GetColor;
  x:=PlotPtr(parentPointer)^.Xo+PlotPtr(parentPointer)^.width/2 -Length(titleStr)*ColWidth/GetMaxX/2;
  y:=PlotPtr(parentPointer)^.Yo+PlotPtr(parentPointer)^.height+1.5/80;
  SelectViewport(0);
  RuboutXY(x,y,Length(TitleStr),objColor);
  SetColor(White);
  PrintXY(x,y,TitleStr);
  SetColor(OldColor);
  PlotPtr(parentPointer)^.LockFocus;
End;
Destructor Title.Done;
Begin;
      Self.Remove;
      Basis.Done;
End;
{****************************************************************************}
{Atom Object                                                                 }
{****************************************************************************}
Constructor Atom.Initialize(Anum: Word; Ay: Real; Plot: pointer );

Begin;
      Basis.Initialize;
      Outlined:=FALSE;
      ParentPointer:=PlotPtr(Plot);
      With ParAtm do                     
           begin;
                 Num:=Anum;
                 y:=Ay;
           end;
End;

Destructor Atom.Done;

Begin;
      Self.Remove;
      Basis.Done;
End;

Procedure Atom.ChangeData( nm, nv, nk1, nk2, nk3, nd, ns: Real );

Begin;
      With ParAtm do
           begin;
                 m:=nm;
                 v:=nv;
                 k1:=nk1;
                 k2:=nk2;
                 k3:=nk3;
                 Damp:=nd;
                 Self:=ns;
           end;
End;

Procedure Atom.ChangeY( Newy: Real );

Begin;
      With ParAtm do y:=Newy;
end;

Procedure Atom.ShowVelocity( colour:    Word );

Var
   OldColour:   Word;

Begin;
      OldColour:=GetColor;
      SetColor(colour);
      With ParAtm do Line(MapX(num), MapY(y), MapX(num), MapY(y+v));
      SetColor(OldColour);
End;

Procedure Atom.Display;

Var
   SclConst:     Real;
   OldColour:    Word;
   OldFillStyle: FillSettingsType;

Procedure DrawUpdateRemove;

Begin;
      If PlotPtr(ParentPointer)^.Visible then
      begin;
      If MassScaling then SclConst:=Sqrt(ParAtm.m) else SclConst:=1;
      If ShowEnvelope Then
        Begin
        SetColor(lightGray);
        SetFillStyle( SolidFill, lightGray);
        End
        Else
        Begin
        SetColor(GraphBackColor);
        SetFillStyle( SolidFill, GraphBackColor );
        End;

      With ParAtm do
           If AtomOutline then
              Circle(mapx(num), mapy(y), trunc(SclConst*radius)+1)
           else
               FillEllipse(mapx(num), mapy(y), trunc(SclConst*radius)+1, trunc(SclConst*radius)+1);
      If ShowV then ShowVelocity( GraphBackColor );
      end;

      Yvec.Get(ParAtm.num+1,ParAtm.Y);
      VVec.Get(ParAtm.num+1,ParAtm.V);

      If not(PlotPtr(ParentPointer)^.Visible) then Begin SetColor(OldColour); Exit; End;

      If Source then SetColor( Black ) else SetColor( White );
      SetFillStyle( SolidFill, AtomColour );

     With ParAtm do
          If AtomOutline then
             Circle(mapx(num), mapy(y), trunc(SclConst*radius)+1)
          else
             FillEllipse(mapx(num), mapy(y), trunc(SclConst*radius)+1, trunc(SclConst*radius)+1);

      If ShowV then ShowVelocity( White );
End;

Begin;
      If ((ParAtm.Num=0) or (ParAtm.Num=(Yvec.Length-1))) then exit;
      If ((ParAtm.Num<PlotPtr(parentPointer)^.Scale.xMin) or
         (ParAtm.Num>PlotPtr(parentPointer)^.Scale.xMax)) then
          If not(Running) then exit;
      OldColour:=GetColor;
      PlotPtr(parentPointer)^.LockFocus;
      If Running then
         begin;
               DrawUpdateRemove;
               SetColor(oldColour);
               Exit;
         end;
      HideCursor;
      GetFillSettings( OldFillStyle );

      If MassScaling then SclConst:=Sqrt(ParAtm.m) else SclConst:=1;

      If Source then SetColor( Black ) else SetColor( OldColour );
      SetFillStyle( SolidFill, AtomColour );
      With ParAtm do
           begin;
                 If outlined or AtomOutline then
                    begin;
                          if outlined then
                             begin;
                                   SetColor(  OldColour );
                                   Rectangle(mapx(num) - trunc(SclConst*radius)+1,
                                             mapy(y) - trunc(SclConst*radius)+1,
                                             mapx(num) + trunc(SclConst*radius)+1,
                                             mapy(y) + trunc(SclConst*radius)+1);
                             end;
                          if AtomOutline then
                             Circle(mapx(num), mapy(y), trunc(SclConst*radius)+1);
                    end
                 else FillEllipse(mapx(num), mapy(y), trunc(SclConst*radius)+1, trunc(SclConst*radius)+1);
           end;
      If ShowV then ShowVelocity( White );
      SetColor( OldColour );
      SetFillStyle( OldFillStyle.pattern, OldFillStyle.color );
      ShowCursor;
End;

Procedure Atom.Remove;

Var
   SclConst:     Real;
   OldColour:    Word;
   OldFillStyle: FillSettingsType;

Begin;
      If ((ParAtm.Num=0) or (ParAtm.Num=(Yvec.Length-1))) then exit;
      OldColour:=GetColor;
      HideCursor;
      PlotPtr(parentPointer)^.LockFocus;
      If MassScaling then SclConst:=Sqrt(ParAtm.m) else SclConst:=1;
      GetFillSettings( OldFillStyle );

      SetColor(GraphBackColor);
      SetFillStyle( SolidFill, GraphBackColor );
      With ParAtm do
           begin;
                 If outlined or AtomOutline then
                    begin;
                          if outlined then
                             Rectangle(mapx(num) - trunc(SclConst*radius)+1,
                                       mapy(y) - trunc(SclConst*radius)+1,
                                       mapx(num) + trunc(SclConst*radius)+1,
                                       mapy(y) + trunc(SclConst*radius)+1);
                          if AtomOutline then
                             Circle(mapx(num), mapy(y), trunc(SclConst*radius)+1);
                    end
                 else FillEllipse(mapx(num), mapy(y), trunc(SclConst*radius)+1, trunc(SclConst*radius)+1);
           end;
      If ShowV then ShowVelocity(GraphBackColor);
      SetColor( OldColour );
      SetFillStyle( OldFillStyle.pattern, OldFillStyle.color );
      ShowCursor;
End;

Function Atom.MouseInside: Boolean;

Var
   xpos, ypos:          Real;
   Inside:              Boolean;

Begin;
      MouseInside:=False;
      PlotPtr(parentPointer)^.LockFocus;
      MousePosn(xpos, ypos, 1, PlotPtr(parentPointer)^.SclNum, Inside);
      With ParAtm do
           if ( mapx(xpos) >= mapx(num) - radius ) and
              ( mapx(xpos) <= mapx(num) + radius ) and
              ( mapy(ypos) >= mapy(y) - radius ) and
              ( mapy(ypos) <= mapy(y) + radius ) and
              Inside
           then MouseInside := True;
End;

Procedure Atom.Drag;

Const
     colour = DarkGray;

VAR
   ytemp,
   xSet,ySet,
   yRub,
   xpos,ypos:           Real;
   num,
   x,y,
   button:              Integer;
   Storage1,
   Storage2,
   Inside:              Boolean;

Procedure InitDragCoor;

Begin;
      If PlotPtr(parentPointer)^.expanded then num:=19 else num:=9;
      xSet:=ParAtm.num;
      yPos:=ParAtm.y;
      If ((yPos<0.01) and (yPos>-0.01)) then ySet:=0 else ySet:=yPos;
      Print(1,num,'Position = ('+Num2Str(xSet,5)+' '+Num2Str(ySet,5)+')');
      yRub:=yPos;
End;

Procedure MainDragCoor;

Begin;
      If not(yRub=yPos) then
         begin;
               yRub:=yPos;
               If ((yPos<0.01) and (yPos>-0.01)) then ySet:=0 else ySet:=yPos;
               RubOut(12,num,15,colour);
               Print(12,num,'('+Num2Str(xSet,5)+' '+Num2Str(ySet,5)+')');
               yRub:=yPos;
         end;
End;

Procedure DoneDragCoor;

Begin;
      RubOut(1,num,26,colour);
End;

Begin;
      If ((LeftB=Fixed) AND (ParAtm.Num=1)) then Exit;
      If ((RightB=Fixed) AND (ParAtm.Num=Yvec.Length-2)) then Exit;
      HideCursor;
      PlotPtr(parentPointer)^.LockFocus;
      With ParAtm do ytemp:=y;
      Storage2:=ShowEnvelope;
      ShowEnvelope:=False;
      Remove;
      SetWriteMode(XorPut);
      Storage1:=AtomOutline;
      AtomOutline:=False;
      Outlined:=True;
      Display;
      InitDragCoor;
      Repeat
            Display;
            MouseGlobalPosn( x, y, button );
            MousePosn( xpos, ypos, 1, PlotPtr(parentPointer)^.SclNum, Inside );
            ChangeY( ypos );
            MainDragCoor;
            Display;
      Until (button = 0);
      DoneDragCoor;
      SetWriteMode(NormalPut);
      Remove;
      Outlined:=False;
      AtomOutline:=Storage1;
      ShowEnvelope:=Storage2;
      Display;
      ChangeY( ypos );
      ShowCursor;
      {PlotPtr(parentPointer)^.}Atomlist.UpdateDvec;
      IF PlotPtr(parentPointer)^.Visible THEN ShowEnergy;
End;

Procedure Atom.Inspector;

Var
   DataInptScrn:  TInputScreen;
   Ok, Cancel:    Boolean;

Begin;
      With DataInptScrn do
           begin;
                 Init;
                 DefineInputPort( 0.31, 0.70, 0.27 ,0.90 );
                 LoadLine('     Atom Information       ');
                 LoadLine('                            ');
                 LoadLine('  Y Position = {     }      ');
                 LoadLine('                            ');
                 Loadline('  Mass       = {     }      ');
                 LoadLine('                            ');
                 LoadLine('  Velocity   = {     }      ');
                 Loadline('                            ');
                 Loadline('  Linear    Coef =  {     } ');
                 Loadline('  Quadratic Coef =  {     } ');
                 Loadline('  Cubic     Coef =  {     } ');
                 Loadline('  Damping   Coef =  {     } ');
                 Loadline('  Self      Coef =  {     } ');
If Source then   Loadline('       [ Edit Source ]      ')
   Else          Loadline('       [ Make Source ]      ');
                 LoadLine(' [  Ok  ] [Cancel] [ Help ] ');
                 LoadLine('                            ');
                 With ParAtm do
                      begin;
                            SetNumber( 1, y );
                            SetNumber( 2, m );
                            SetNumber( 3, v );
                            SetNumber( 4, k1 );
                            SetNumber( 5, k2 );
                            SetNumber( 6, k3 );
                            SetNumber( 7, damp );
                            SetNumber( 8, self );
                      end;

                 SetHelpFile('Chain.Hlp', 'Atom Help');
                 AcceptScreen;
                 Ok:=GetBoolean(10);
                 Cancel:=GetBoolean(11);
                 If GetBoolean(9) then
                    begin;
                          MakeSource;
                          Ok:=True;{make sure you get the values from the screen}
                    end;

                 If Ok then
                    With ParAtm  do
                         begin;
                               Remove;
                               y:=GetNumber(1);
                               If ((LeftB=Fixed) AND(ParAtm.Num=1)) then y:=0;
                               If ((RightB=Fixed) AND(ParAtm.Num=Yvec.Length-2)) then y:=0;
                               m:=Abs(GetNumber(2));
                               if m<10*macheps then m:=10*macheps;
                               v:=GetNumber(3);
                               k1:=GetNumber(4);
                               k2:=GetNumber(5);
                               k3:=GetNumber(6);
                               Damp:=GetNumber(7);
                               Self:=GetNumber(8);
                               Display;
                         end;
                 If GetBoolean(9) then
                    begin;
                          Remove;
                          Display;
                    end;

                 AtomList.UpdateDVec;
                 If PlotPtr(parentPointer)^.visible THEN
                    begin;
                          PlotPtr(parentPointer)^.LockFocus;
                          ShowEnergy;
                    end;
                 Done;
           end;
End;

Procedure Atom.MakeSource;

Var
   NewSource,
   OldSource,
   Leave:               Boolean;
   OldNumber,
   OldColour,
   OldBack,
   NumPts:              Word;
   PeriodBut,
   FuncBut,
   SourceTypeBut:       TButtons;
   Ok_Button,Cancel_Button : TButton;
   Info:                TSliders;
   SourceInc,
   Amp,Freq,A,Xo:       Real;
   Ivec,Jvec:           Dvector;
   anatom:              BasePtr;
   msg:                 String;
   Ok,Cancel:           Boolean;
   ConInputPort:        TInputScreen;

Procedure SetButtons;

Begin;
      With FuncBut do
           begin;
                 Init;
                 Create(11, 0.13, 0.88, 'Sine');
                 Create(22, 0.13, 0.84, 'Gaussian');
                 Create(33, 0.13, 0.80, 'Sym Pulse');
                 Create(44, 0.13, 0.76, 'Pulse');
                 Create(77, 0.13, 0.72, 'Mod Gaussian');
                 Create(88, 0.13, 0.68, 'User Defined');
         {       Create(55, 0.43, 0.60, 'Disable');
                 Create(66, 0.43, 0.56, 'Exit');  }
                 Number:=11;
                 FuncBut.DrawAll;
           end;
      Ok_Button.Create(0.43,0.58,'Ok','Return');
      Cancel_Button.Create(0.43,0.54,'Cancel','Esc');
      With SourceTypeBut do
           begin;
                 Init;
                 Create(11, 0.13, 0.60, 'Force');
                 Create(22, 0.13, 0.56, 'Displace');
                 Number:=11;
                 SourceTypeBut.DrawAll;
           end;

      With PeriodBut do
           begin;
                 Init;
                 Create(11, 0.28, 0.60, 'Periodic');
                 Create(22, 0.28, 0.56, 'Single');
                 Number:=22;
                 PeriodBut.DrawAll;
           end;

End;

Procedure SetSineSliders;

Begin;
      DefineScale(6,0,SourcePeriod,-3,3);

      If NewSource then
         begin;
               Amp:=0.5;
               Freq:=0.1;
         end
      else
         begin;
               Amp:=anatom^.ParSource.Amp;
               Freq:=anatom^.ParSource.Freq;
         end;

      With Info do
           begin;
                 Init;
                 Create(1,-2,2,Amp,0.38,0.90,0.64,2,'-2','2','Amp',True);
                 Create(2,0.01,2,Freq,0.48,0.90,0.64,4,'0.01','2','Freq',True);
                 DrawAll;
           end;
End;

Procedure SetGaussianSliders;

Begin;
      DefineScale(6,0,SourcePeriod,-3,3);

      If NewSource then
         begin;
               Amp:=1;
               A:=2;
               Xo:=5;
         end
      else
         begin;
               Amp:=anatom^.ParSource.Amp;
               A:=anatom^.ParSource.A;
               Xo:=anatom^.ParSource.Xo;
         end;

      With Info do
           begin;
                 Init;
                 Create(1,-2,2,Amp,0.33,0.90,0.64,2,'-2','2','Amp',True);
                 Create(2,0.1,SourcePeriod,A,0.42,0.90,0.64,2,'0.1',msg,'Width',True);
                 Create(3,0,SourcePeriod,Xo,0.51,0.90,0.64,2,'0',msg,'Delay',True);
                 DrawAll;
           end;
End;

Procedure SetModGaussianSliders;

Begin;
      DefineScale(6,0,SourcePeriod,-3,3);

      If NewSource then
         begin;
               Amp:=1;
               A:=5;
               Xo:=5;
               Freq:=0.1;
         end
      else
         begin;
               Amp:=anatom^.ParSource.Amp;
               A:=anatom^.ParSource.A;
               Xo:=anatom^.ParSource.Xo;
               Freq:=anatom^.ParSource.Freq;
         end;

      With Info do
           begin;
                 Init;
                 Create(1,-2,2,Amp,0.32,0.90,0.64,2,'-2','2','Amp',True);
                 Create(2,0.1,SourcePeriod,A,0.39,0.90,0.64,2,'0.1',msg,'Width',True);
                 Create(3,0,SourcePeriod,Xo,0.46,0.90,0.64,2,'0',msg,'Delay',True);
                 Create(4,0.01,2,Freq,0.54,0.90,0.64,2,'0.01','2','Freq',True);
                 DrawAll;
           end;
End;

Procedure SetPulseStepSliders;

Begin;
         DefineScale(6,0,SourcePeriod,-3,3);

      If NewSource then
         begin;
               Amp:=1;
               A:=2;
               Xo:=1;
         end
      else
         begin;
               Amp:=anatom^.ParSource.Amp;
               A:=anatom^.ParSource.A;
               Xo:=anatom^.ParSource.Xo;
         end;

      With Info do
           begin;
                 Init;
                 Create(1,-2,2,Amp,0.33,0.90,0.64,2,'-2','2','Amp',True);
                 Create(2,0.02,SourcePeriod,A,0.42,0.90,0.64,2,'0.02',msg,'Width',True);
                 Create(3,0,SourcePeriod,Xo,0.51,0.90,0.64,2,'0',msg,'Delay',True);
                 DrawAll;
           end;
End;

Procedure DrawSine;

Var
   i:           Integer;
   Value,
   localtime:   Real;

Begin;
      SelectViewPort(6);
      SelectScale(6);
      Axis(0,-3,SourceInc,1);
      LocalTime:=0;
      For i:=1 to NumPts do
          begin;
                Value:=Amp*Sin(2*Pi*Freq*LocalTime);
                Ivec.Put(i,Value);
                Jvec.Put(i,LocalTime);
                LocalTime:=LocalTime + dt;
          end;
      SetColor(White);
      PlotDvectors(Jvec,Ivec,1,NumPts);
End;

Function DrawUserFunc(funcStr:String) : boolean;
Var
   i:           Integer;
   Value,
   localtime:   Real;

Begin;
{      Parser.Init;
      Parser.Parse('t',' ',' ',' ',funcStr);}
      SelectViewPort(6);
      SelectScale(6);
      Axis(0,-3,SourceInc,1);
      LocalTime:=0;
      ErrorFound := false;
      HaltIfError := False;
      i:=1;
      while (i<=NumPts) and not ErrorFound do
      begin;
         Value:=Parser.F(localTime,0,0,0);
         Ivec.Put(i,Value);
         Jvec.Put(i,LocalTime);
         LocalTime:=LocalTime + dt;
         inc(i);
      end;
      SetColor(White);
      if ErrorFound Then announce('Parser Error - Please Try Again')
      else PlotDvectors(Jvec,Ivec,1,NumPts);
      DrawUserFunc := not ErrorFound;
      ErrorFound := false;
      HaltIfError := True;
End;

Procedure DrawGaussian;

Var
   i:           Integer;
   Value,
   j:           Real;

Begin;
      SelectViewPort(6);
      SelectScale(6);
      Axis(0,-5,SourceInc,1);
      j:=0;
      For i:=1 to NumPts do
          begin;
                Value:=Sqr(2*(j-Xo)/A);
                If (Value<10) then Value:=Amp*Exp(-Value) else Value:=0;
                Ivec.Put(i,Value);
                Jvec.Put(i,j);
                j:=j+dt;
          end;
      SetColor(White);
      PlotDvectors(Jvec,Ivec,1,NumPts);
End;

Procedure DrawModGaussian;

Var
   i:           Integer;
   Value2,
   Value,
   j:           Real;

Begin;
      SelectViewPort(6);
      SelectScale(6);
      Axis(0,-5,SourceInc,1);
      j:=0;
      For i:=1 to NumPts do
          begin;
                Value:=Sqr(2*(j-Xo)/A);
                Value2:=Sin(2*Pi*Freq*j);
                If (Value<10) then Value:=Amp*Value2*Exp(-Value) else Value:=0;
                Ivec.Put(i,Value);
                Jvec.Put(i,j);
                j:=j+dt;
          end;
      SetColor(White);
      PlotDvectors(Jvec,Ivec,1,NumPts);
End;

Procedure DrawPulse;

Var
   i:           Integer;
   Value,
   j:           Real;

Begin;
      SelectViewPort(6);
      SelectScale(6);
      Axis(0,-5,SourceInc,1);
      j:=0;
      For i:=1 to NumPts do
          begin;
                If (j<(Xo-A/2)) then Value:=0;
                If (((Xo-A/2)<j) and (j<(Xo))) then Value:=Amp;
                If (((Xo)<j) and (j<(A/2+Xo))) then Value:=-Amp;
                If (j>(A/2+Xo)) then Value:=0;
                Ivec.Put(i,Value);
                Jvec.Put(i,j);
                j:=j+dt;
          end;
      SetColor(White);
      PlotDvectors(Jvec,Ivec,1,NumPts);
End;

Procedure DrawStep;

Var
   i:           Integer;
   Value,
   j:           Real;

Begin;
      SelectViewPort(6);
      SelectScale(6);
      Axis(0,-5,SourceInc,1);
      j:=0;
      For i:=1 to NumPts do
          begin;
                If (j<(Xo-A/2)) then Value:=0;
                If (((Xo-A/2)<j) and (j<(A/2+Xo))) then Value:=Amp;
                If (j>(A/2+Xo)) then Value:=0;
                Ivec.Put(i,Value);
                Jvec.Put(i,j);
                j:=j+dt;
          end;
      SetColor(White);
      PlotDvectors(Jvec,Ivec,1,NumPts);
End;

Procedure RemoveGraph;

Begin;
      SelectViewPort(6);
      SelectScale(6);
      SetColor(Black);
      PlotDVectors(Jvec,Ivec,1,NumPts);
End;

Procedure RemoveSliders;

Begin;
      GraphBackColor:=Blue;
      OpenViewPort(5);
      SetColor(white);
      Print(2,1,'Function Type:');
      Print(2,8,'Source Type:');
      GraphBackColor:=Black;
      OpenViewPort(6);
      Case OldNumber of
           11:begin;
                    Info.Delete(1);
                    Info.Delete(2);
              end;
           77:begin;
                    Info.Delete(1);
                    Info.Delete(2);
                    Info.Delete(3);
                    Info.Delete(4);
              end;
           22,
           33,
           44:begin;
                    Info.Delete(1);
                    Info.Delete(2);
                    Info.Delete(3);
              end;
           88:funcBut.Delete(89);
           End;
      FuncBut.DrawAll;
      Ok_Button.Create(0.43,0.58,'Ok','Return');
      Cancel_Button.Create(0.43,0.54,'Cancel','Esc');
      SourceTypeBut.DrawAll;
      PeriodBut.DrawAll;
End;

Function DefineFuncString : boolean;

Var
   Ok,Cancel,
   Leave:       Boolean;
   FuncScreen:  TinputScreen;
   tempStr:     String;

Begin
     With FuncScreen do
          begin
               Init;
               Parser.Init;
               DefineInputPort(0.10,0.90,0.30,0.51);
               LoadLine('Enter a Function of time:                          ');
               LoadLine('F(t)="                                                      "');
               LoadLine('');
               LoadLine('    [  Ok  ]   [Cancel]   [ Help ]   [ View ]   ');
               SetHelpFile('Chain.Hlp','Source-Parser Help');
               SetString(1,ParSource.FuncStr);
               Leave:=False;
               Repeat
                     AcceptScreen;
                     tempStr:=GetString(1);
                     if (tempStr='')OR(tempStr=' ') Then tempStr:='0';
                     Ok := Parser.Parse('t',' ',' ',' ',tempStr);
                     If (NOT Ok) And (OkPressed OR GetBoolean(5)) then
                      Announce('That function is not acceptable - f(t).');
                     If (GetBoolean(5) And Ok) or OkPressed then
                     Begin
                        RemoveGraph;
                        Ok := DrawUserFunc(tempStr);
                     End;
                     Leave := (OkPressed And Ok) or CancelPressed;
               Until Leave;
               if OKPressed Then ParSource.FuncStr:=GetString(1);
               DefineFuncString := OKPressed;
               Done;{with FuncScreen}
          End;
end;

Procedure ExecuteButtons;

Begin;
            If SourceTypeBut.Changed then
               Case SourceTypeBut.Number of
                    11: ; {Force}
                    22: ; {DisplaceMent}
               End;

            If PeriodBut.Changed then
               Case PeriodBut.Number of
                    11: ; {Periodic}
                    22: ; {Single}
               End;


            If FuncBut.Changed then
               Case FuncBut.Number of
                    11: begin;
                              RemoveGraph;
                              RemoveSliders;
                              SetSineSliders;
                              DrawSine;
                              OldNumber:=11;
                        end;
                    22: begin;
                              RemoveGraph;
                              RemoveSliders;
                              SetGaussianSliders;
                              DrawGaussian;
                              OldNumber:=22;
                        end;
                    33: begin;
                              RemoveGraph;
                              RemoveSliders;
                              SetPulseStepSliders;
                              DrawPulse;
                              OldNumber:=33;
                        end;
                    44: begin;
                              RemoveGraph;
                              RemoveSliders;
                              SetPulseStepSliders;
                              DrawStep;
                              OldNumber:=44;
                        end;
                    77: begin;
                              RemoveGraph;
                              RemoveSliders;
                              SetModGaussianSliders;
                              DrawModGaussian;
                              OldNumber:=77;
                        end;
                    88 :Begin
                        RemoveGraph;
                        RemoveSliders;
                        if DefineFuncString then
                        begin
                           funcBut.Create(89, 0.33, 0.68, 'HIDE');
                           OldNumber:=88;
                        end else begin
                           oldNumber := 11;
                           funcBut.Number := oldNumber;
                           SetSineSliders;
                           DrawSine;
                        end;
                        FuncBut.DrawAll;
                        End;
                    89 :Begin
                        RemoveGraph;
                        RemoveSliders;
                        Announce('test');
                        OldNumber:=88;
                        End;
               End;
        if Cancel_Button.Clicked then begin
            Source:=False;
            Leave:=True;
        end;
        if Ok_Button.Clicked then Leave:=True;  {Exit}
End;

Procedure ExecuteSliders;

Begin;
      If Info.Changed then
         Case FuncBut.Number of
              11: begin;
                        Amp:=Info.Value(1);
                        Freq:=Info.Value(2);
                        RemoveGraph;
                        DrawSine;
                  end;
              22: begin;
                        Amp:=Info.Value(1);
                        A:=Info.Value(2);
                        Xo:=Info.Value(3);
                        RemoveGraph;
                        DrawGaussian;
                  end;
              33: begin;
                        Amp:=Info.Value(1);
                        A:=Info.Value(2);
                        Xo:=Info.Value(3);
                        RemoveGraph;
                        DrawPulse;
                  end;
              44: begin;
                        Amp:=Info.Value(1);
                        A:=Info.Value(2);
                        Xo:=Info.Value(3);
                        RemoveGraph;
                        DrawStep;
                  end;
              77: begin;
                        Amp:=Info.Value(1);
                        A:=Info.Value(2);
                        Xo:=Info.Value(3);
                        Freq:=Info.Value(4);
                        RemoveGraph;
                        DrawModGaussian;
                  end;
         End;
End;

Procedure SetSourceData;

Begin;
      With anatom^ do
           begin;
                 Case OldNumber of
                      11:begin;
                               ParSource.Kind:=Sine;
                               ParSource.Freq:=Freq;
                               ParSource.Amp:=Amp;
                         end;
                      22:begin;
                               ParSource.Kind:=Gaussian;
                               ParSource.Xo:=Xo;
                               ParSource.A:=A;
                               ParSource.Amp:=Amp;
                         end;
                      33:begin;
                               ParSource.Kind:=Pulse;
                               ParSource.Xo:=Xo;
                               ParSource.A:=A;
                               ParSource.Amp:=Amp;
                         end;
                      44:begin;
                               ParSource.Kind:=Step;
                               ParSource.Xo:=Xo;
                               ParSource.A:=A;
                               ParSource.Amp:=Amp;
                         end;
                      77:begin;
                               ParSource.Kind:=ModGaussian;
                               ParSource.Xo:=Xo;
                               ParSource.A:=A;
                               ParSource.Amp:=Amp;
                               ParSource.Freq:=Freq;
                         end;
                      88: ParSource.Kind:=UserDefined;
                 End;

                 Case SourceTypeBut.Number of
                      11: Parsource.Excite:=Force;
                      22: ParSource.Excite:=Displacement;
                 End;

                 Case PeriodBut.Number of
                      11: Parsource.Periodic:=True;
                      22: ParSource.Periodic:=False;
                 End;

           end;
      If (OldSource=True) then SourceList.DeleteItem(anatom);
      If not(FuncBut.Number=55) then
         begin;
               Source:=True;
               SourceList.AddItem(anatom);
         end;
End;

Procedure InitAtomSource;

Begin;
      With anatom^ do
           begin;
                 Case ParSource.Kind of
                      NoSource: begin;
                                      NewSource:=True;
                                      FuncBut.Number:=11;
                                      SetSineSliders;
                                      DrawSine;
                                      OldNumber:=11;
                                end;
                      Sine:     begin;
                                      NewSource:=False;
                                      FuncBut.Number:=11;
                                      SetSineSliders;
                                      DrawSine;
                                      OldNumber:=11;
                                      NewSource:=True;
                                end;
                      Gaussian: begin;
                                      NewSource:=False;
                                      FuncBut.Number:=22;
                                      SetGaussianSliders;
                                      DrawGaussian;
                                      OldNumber:=22;
                                      NewSource:=True;
                                end;
                      Pulse:    begin;
                                      NewSource:=False;
                                      FuncBut.Number:=33;
                                      SetPulseStepSliders;
                                      DrawPulse;
                                      OldNumber:=33;
                                      NewSource:=True;
                                end;
                      Step:     begin;
                                      NewSource:=False;
                                      FuncBut.Number:=44;
                                      SetPulseStepSliders;
                                      DrawStep;
                                      OldNumber:=44;
                                      NewSource:=True;
                                end;
                      ModGaussian: begin;
                                         NewSource:=False;
                                         FuncBut.Number:=77;
                                         SetModGaussianSliders;
                                         DrawModGaussian;
                                         OldNumber:=77;
                                         NewSource:=True;
                                   end;
                      UserDefined:begin;
                                         NewSource:=False;
                                         FuncBut.Number:=88;
                                         funcBut.Create(89, 0.33, 0.68, 'HIDE');
                                         DefineScale(6,0,SourcePeriod,-3,3);
                                         info.init;  {make sure the sliders are initialized}
                                         DrawUserFunc(ParSource.FuncStr);
                                         OldNumber:=88;
                                         NewSource:=True;
                                   end;
                 End; {of Case}

                 Case ParSource.Excite of
                      Force:         SourceTypeBut.Number:=11;
                      Displacement:  SourceTypeBut.Number:=22;
                 End; {of Case}

                 If ParSource.Periodic then PeriodBut.Number:=11 else PeriodBut.Number:=22;
           end; {of WITH}

           FuncBut.DrawAll;
           Ok_Button.Create(0.43,0.58,'Ok','Return');
           Cancel_Button.Create(0.43,0.54,'Cancel','Esc');
           SourceTypeBut.DrawAll;
           PeriodBut.DrawAll;
End;

Begin;
      AtomList.GetAnAtom(ParAtm.Num,anatom);

      SourceInc:=Round(SourcePeriod/5);
      msg:=Num2Str(SourcePeriod,3);

      OldSource:=Source;
      Leave:=False;
      OldColour:=GetColor;
      OldBack:=GraphBackColor;
      NumPts:=trunc(SourcePeriod/dt) + 1;
      if Numpts> 512 Then NumPts:=512;
      Ivec.Init(NumPts);
      Jvec.Init(NumPts);
      RemoveAnalysis;

      DefineViewPort(5, 0.10, 0.90, 0.51, 0.95);
      DefineViewPort(6, 0.60, 0.88, 0.58, 0.90);
      GraphBackColor:=Blue;
      OpenViewPort(5);
      Print(2,1,'Function Type:');
      Print(2,8,'Source Type:');
      GraphBackColor:=Black;
      OpenViewPort(6);

      SetButtons;

      InitAtomSource;

      Repeat
            CheckForEvents;
            ExecuteButtons;
            ExecuteSliders;
      Until Leave;

      SetSourceData;
      FuncBut.Done;
      SourceTypeBut.Done;
      Info.Done;
      Ivec.Free;
      Jvec.Free;

      CloseViewPort(6);
      CloseViewPort(5);

      SetColor(OldColour);
      GraphBackColor:=OldBack;
      DisplayAnalysis;
      Update;
End;

Function  Atom.GetSourceValue(t:Real): Real;

Var
   SourceTime,
   Value,Value2:       Real;

Begin;
      SourceTime:=t;
      If not(ParSource.Periodic) and (SourceTime>SourcePeriod) then exit;
      If (ParSource.Periodic) and (SourceTime>SourcePeriod) then
         begin;
               SourceTime:=SourceTime-SourcePeriod*Trunc(SourceTime/SourcePeriod);
         end;

      With ParSource do
      Case Kind of
           Sine:      GetSourceValue:=Amp*Sin(2*Pi*Freq*t);
           Gaussian:  begin;
                            Value:=Sqr(2*(SourceTime-Xo)/A);
                            If (Value<10) then Value:=Amp*Exp(-Value) else Value:=0;
                            GetSourceValue:=Value;
                      end;
           ModGaussian:  begin;
                               Value:=Sqr(2*(SourceTime-Xo)/A);
                               Value2:=Sin(2*Pi*Freq*t);
                               If (Value<10) then Value:=Amp*Value2*Exp(-Value) else Value:=0;
                               GetSourceValue:=Value;
                         end;
           Step:      If ((SourceTime<(Xo-A/2)) or (SourceTime>(A/2+Xo))) then
                             GetSourceValue:=0
                          else
                             GetSourceValue:=Amp;
           Pulse:   begin;
                          GetSourceValue:=0;
                          If (((Xo-A/2)<SourceTime) and (SourceTime<(Xo))) then
                             GetSourceValue:=Amp;
                          If (((Xo)<SourceTime) and (SourceTime<(A/2+Xo))) then
                             GetSourceValue:=-Amp;
                    end;
           UserDefined: Begin
                        Parser.Parse('t',' ',' ',' ',funcStr);
                        GetSourceValue:=Parser.F(t,0,0,0);
                        End;
      End;  {of Case}
End;
{****************************************************************************}
{Axes Object                                                                 }
{****************************************************************************}
Constructor Axes.Initialize( Plot: pointer);

Begin;
      Basis.Initialize;
      ParentPointer:=PlotPtr(Plot);
      Visible:=False;
End;
Procedure Axes.Setup;

Begin;
      {Visible:=True; }
      PlotPtr(parentPointer)^.LockFocus;
      With PlotPtr(parentPointer)^.Scale do
           begin;
                 If (xMin<0.0001) and (xMin>-0.0001) then xMin:=0;
                 If (yMin<0.0001) and (yMin>-0.0001) then yMin:=0;
                 AxesTic.xOrg:=xMin;
                 If (yMax>0) then AxesTic.yOrg:=yMin else AxesTic.yOrg:=yMax;
                 If AnlyPlotPtr(parentpointer)^.AnalysisPlot in [FFT_x,FFT_V,FFT_T]
                   Then AxesTic.yOrg:=0;
                 AxesTic.xTic:=2*TickSpace(xMax-xMin);
                 AxesTic.yTic:=2*TickSpace(yMax-yMin);
                 If AnlyPlotPtr(parentpointer)^.AnalysisPlot in [FFT_x,FFT_V,FFT_T]then
                     AxesTic.xTic:=Round((xMax-xMin)/4 + 0.51);
                 If AnlyPlotPtr(parentpointer)^.AnalysisPlot=YvsTDensity then
                     AxesTic.xTic:=Trunc((xMax-xMin)/5 + 1);
           end; {of With}
      If PlotPtr(parentPointer)^.HasAtoms then
        With PlotPtr(parentPointer)^.Scale do
           If ((xMax-xMin)<100) then AxesTic.xTic:=Trunc((xMax-xMin)/20 + 1)
                              else AxesTic.xTic:=Trunc((xMax-xMin)/10 + 1);
End;

Destructor Axes.Done;

Begin;
      Self.Remove;
      Basis.Done;
End;

Procedure Axes.Display;

Var
   i,OldColour:   Word;
   CellTot:       Integer;

Begin;
      If not(Visible) then exit;
      if Not(PlotPtr(parentPointer)^.Visible) Then exit;
      PlotPtr(parentPointer)^.LockFocus;
      OldColour:=GetColor;
      With AxesTic do
           begin;
                 Axis(  xOrg, yOrg, xTic, yTic  );
                 CellTot:=((Yvec.length-2) div Cell) + 1;
                 If ShowCell and (Cell > 1) then
                    For i:=1 to CellTot do
                        begin;
                              SetLineStyle(DottedLn, 0, NormWidth);
                              SetColor(Magenta);
                              Line(MapX(i*Cell+0.5),MapY(yOrg),MapX(i*Cell+0.5),MapY(10*yTic));
                              SetLineStyle(SolidLn, 0, NormWidth);
                        end;
           end;
      SetColor( OldColour );
End;

Procedure Axes.Remove;

Var
   OldColour,i:   Word;
   CellTot:       Integer;

Begin;
      PlotPtr(parentPointer)^.LockFocus;
      OldColour:=GetColor;
      With AxesTic do
           begin;
                 DrawAxis( xOrg, yOrg, xTic, yTic, Black );
                 CellTot:=((Yvec.length-2) div Cell) + 1;
                 If ShowCell and (Cell > 1) then
                    For i:=1 to CellTot do
                        begin;
                              SetLineStyle(DottedLn, 0, NormWidth);
                              SetColor(GraphBackColor);
                              Line(MapX(i*Cell+0.5),MapY(yOrg),MapX(i*Cell+0.5),MapY(10*yTic));
                              SetLineStyle(SolidLn, 0, NormWidth);
                        end;
           end;
      SetColor( OldColour );
End;

{****************************************************************************}
{Button Objects                                                              }
{****************************************************************************}
Constructor Button.Initialize( Plot : pointer; theButton: ShortInt );

Begin;
      Basis.Initialize;
      ParentPointer:=PlotPtr(Plot);
      PlotPtr(parentPointer)^.LockFocus;
      ButtonNum:=thebutton;
      PlotPtr(parentPointer)^.ButtonList.addItem(@self);
End;

Destructor Button.done;

Begin;
      Self.Remove;
      Basis.Done;
End;

Procedure Button.Display;

Var
   oldColour:            Word;
   oldFillStyle:        FillSettingsType;

Begin;
      HideCursor;
      PlotPtr(parentPointer)^.LockFocus;
      OldColour:=GetColor;
      GetFillSettings( oldFillStyle );
      Self.CalculateBox;
      Graph.Rectangle( box[1].x, box[1].y, box[3].x, box[3].y );
      Self.DrawIcon;
      SetFillStyle( oldFillStyle.pattern, oldFillStyle.color );
      SetColor( oldColour );
      ShowCursor;
End;

Procedure Button.Remove;

VAR
   oldColour:     Word;
   oldFillStyle: FillSettingsType;

Begin;
      HideCursor;
      PlotPtr(parentPointer)^.LockFocus;
      CalculateBox;
      OldColour:=GetColor;
      GetFillSettings( oldFillstyle );
      SetColor( graphBackColor );
      SetFillStyle( SOLIDFILL, graphBackColor );
      FillPoly( 4, box );
      SetColor( oldColour );
      SetFillStyle( oldFillStyle.pattern, oldFillStyle.color );
      ShowCursor;
END;

Function Button.MouseInside : BOOLEAN;

Var
   xpos,ypos:   Real;
   inside:      Boolean;
   Xin,Yin:     Boolean;

Begin;
      PlotPtr(parentPointer)^.LockFocus;
      Self.CalculateBox;
      MouseInside:=False;
      If not(PlotPtr(parentPointer)^.visible) then exit;
      If Event.MouseClicked then
         begin;
               MousePosn(xpos,ypos, 1, PlotPtr(parentPointer)^.SclNum, Inside);
               If (xpos > Ix) and (xpos < (Ix + w)) then Xin:=True else Xin:=False;
               If ((Iy + h)>Iy) then
                  If (ypos > Iy) and (ypos < (Iy + h)) then Yin:=True else Yin:=False
               else
                  If (ypos < Iy) and (ypos > (Iy + h)) then Yin:=True else Yin:=False;
               If Xin and Yin then
                      begin
                            MouseInside:=True;
                            Beep;
                      end;
         end;
End;

Procedure Button.DrawIcon;

Begin;
End;

Procedure Button.Action;

Begin;
End;                                        

PROCEDURE Button.CalculateBox;   

Begin;
      with PlotPtr(parentPointer)^.Scale do
           begin;
                 Ix:=xMin;
                 Iy:=yMin;
                 w:=(xMax-xMin)/20;
                 h:=(yMax-yMin)/10;
           end;
      PlotPtr(parentPointer)^.lockFocus;
      Iy:=Iy + (ButtonNum-1)*h;

      Box[1].x:=mapx(Ix) + 1;
      Box[1].y:=mapy(Iy) - 1;

      Box[2].x:=mapx(Ix + w) + 1;
      Box[2].y:=box[1].y;

      Box[3].x:=box[2].x;
      Box[3].y:=mapy(Iy + h) + 1;

      Box[4].x:=box[1].x;
      Box[4].y:=box[3].y
End;

{****************************************************************************}
Constructor CornerButton.Initialize( Plot : pointer; theCorner : INTEGER );

Begin;
      Button.Initialize( PlotPtr(Plot), theCorner );
      CornerNum:=theCorner;
End;

Destructor CornerButton.Done;

Begin;
      Button.Done;
End;

Procedure CornerButton.CalculateBox;
                                              
Begin;
      Case CornerNum of
           1:                  
              begin;
                    with PlotPtr(parentPointer)^.Scale do
                         begin;
                               Ix:=xMin;
                               Iy:=yMax;
                               w:=(xMax-xMin)/20;
                               h:=(yMax-yMin)/10;
                        end;
                        Iy:=Iy - h;
              end;
           2:
             begin;
                    with PlotPtr(parentPointer)^.Scale do
                         begin;
                               Ix:=xMax;
                               Iy:=yMax;
                               w:=(xMax-xMin)/20;
                               h:=(yMax-yMin)/10;
                         end;
                    Iy:=Iy - h;
                    Ix:=Ix - w;
             end;
           3:
             begin;
                    with PlotPtr(parentPointer)^.Scale do
                         begin;
                               Ix:=xMax;
                               Iy:=yMin;
                               w:=(xMax-xMin)/20;
                               h:=(yMax-yMin)/10;
                         end;
                   Iy:=Iy;
                   Ix:=Ix - w;
             end;
      End;
      PlotPtr(parentPointer)^.lockFocus;

      Box[1].x := mapx(Ix);
      Box[1].y := mapy(Iy);

      Box[2].x := mapx(Ix + w) - 1;
      Box[2].y := box[1].y;

      Box[3].x := box[2].x;
      Box[3].y := mapy(Iy + h) + 1;

      Box[4].x := box[1].x;
      Box[4].y := box[3].y;
End;                                                                              

{****************************************************************************}

Constructor SclButton.Initialize( Plot : Pointer);

Begin;
      CornerButton.Initialize( PlotPtr(Plot), 1 );
End;

Destructor SclButton.Done;

Begin;
      CornerButton.Done;
End;

Procedure SclButton.DrawIcon;

Var
   oldColour:    Word;
   oldFillStyle: FillSettingsType;

Begin;
      HideCursor;
      PlotPtr(parentPointer)^.LockFocus;
      OldColour:=GetColor;
      GetFillSettings( oldFillStyle );
      SetFillStyle(SOLIDFILL, Red);
      FillPoly(4, box);
      SetFillStyle(oldFillStyle.pattern, oldFillStyle.color);
      ShowCursor;
End;


Procedure SclButton.Action;
Begin;
PlotPtr(ParentPointer)^.ChangeScale;
End;

{****************************************************************************}
Constructor ExpandButton.Initialize( Plot : pointer);

Begin;
      CornerButton.Initialize( PlotPtr(Plot), 2 );
End;

Destructor ExpandButton.Done;

Begin;
      CornerButton.Done;
End;

Procedure ExpandButton.DrawIcon;

Var
   oldColour:    Word;
   oldFillStyle: FillSettingsType;

Begin;
      HideCursor;
      PlotPtr(parentPointer)^.LockFocus;
      OldColour:=GetColor;
      GetFillSettings( oldFillStyle );
      SetFillStyle(SOLIDFILL, Magenta);
      FillPoly(4, box);
      SetFillStyle(oldFillStyle.pattern, oldFillStyle.color);
      ShowCursor;
End;


Procedure ExpandButton.Action;

Begin;
      PlotPtr(parentPointer)^.Expand;
      if PlotPtr(parentPointer)^.HasAtoms Then ShowEnergy;
      IF PlotPtr(parentPointer)^.Visible AND PlotPtr(parentPointer)^.Expanded THEN ShowEnergy;
End;
{****************************************************************************}
Constructor AttributeBut.Initialize( Plot : pointer);

Begin;
      CornerButton.Initialize( PlotPtr(Plot), 3 );
End;

Destructor AttributeBut.Done;

Begin;
      CornerButton.Done;
End;

Procedure AttributeBut.DrawIcon;

Var
   oldColour:    Word;
   oldFillStyle: FillSettingsType;

Begin;
      HideCursor;
      PlotPtr(parentPointer)^.LockFocus;
      OldColour:=GetColor;
      GetFillSettings( oldFillStyle );
      SetFillStyle(SOLIDFILL, Blue);
      FillPoly(4, box);
      SetFillStyle(oldFillStyle.pattern, oldFillStyle.color);
      ShowCursor;
End;


Procedure AttributeBut.Action;

Begin;
      PlotPtr(parentPointer)^.ChangeAttributes;
End;
{****************************************************************************}

End.