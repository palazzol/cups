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

UNIT Chn_Fun;
{$R-}
INTERFACE

USES
   Chn_Gbl,Chn_Lst,Chn_Graf,Chn_Plot,
   Cups,CupsGui,CUPSmupp,CupsGrph,CupsFunc,CupsProc,Crt,Graph;

Type
    InitType = (Gauss, Pul, SymPul);

Procedure Zero;
Procedure ZeroVelocity;
Procedure RandomPosVel;
Procedure SineFun;
Procedure ModulatedGaussian;
Procedure Gauss_Pulse_SymPulse(InitTYP: InitType);
Procedure Modes;
Procedure SetCMFrame;
Procedure UserDefinedInit;


Implementation


Procedure Zero;

Begin
      Yvec.Fill(0);
      Vvec.Fill(0);
      AtomList.UpdateList;
End;

Procedure ZeroVelocity;

Begin
     Vvec.Fill(0);
     AtomList.UpdateList;
End;

Procedure RandomPosVel;

Var
   i:           Integer;

Begin;
      For i:=1 to Yvec.Length do
          begin;
                Yvec.Put(i,(-0.5 + Random(256)/255));
                Vvec.Put(i,(-0.5 + Random(256)/255));
          end;
      AtomList.UpdateList;
End;

Procedure SineFun;

Var
   Amp,Lambda:  Real;
   yDraw,xDraw:   Dvector;
   OldColour,
   drawPts:    Word;

Procedure UpdateSine;

Var
   Value:         Real;
   i:             Word;

Begin;
      For i:=0 to Yvec.Length-3 do
          begin;
                Value:=Amp*Sin(2*PI*i/lambda);
                Yvec.Put(i+2,Value);
          end;
      AtomList.UpdateList;
      AtomList.Display;
End;

Procedure CalcDrawSine;

Var
   i:            Word;
   j:            Real;

Begin;
      j:=0.9;
      For i:=0 to drawPts-1 do
          begin;
                yDraw.Put(i+1,Amp*Sin(2*PI*i*(1+1/(drawPts-1))/(10.0*Lambda)));
                j:=j + 0.1;
                xDraw.Put(i+1,j);
          end;

      TheChain.LockFocus;
      SetColor(White);
      PlotDVectors(xDraw,yDraw,1,drawPts);
      SetColor(OldColour);
End;

Procedure RemoveSine;

Begin;
      TheChain.LockFocus;
      SetColor(GraphBackColor);
      PlotDVectors(xDraw,yDraw,1,drawPts);
      SetColor(OldColour);
      AtomList.Display;
End;

Procedure SliderWindow;

Var
   Leave:    Boolean;
   Info:     TSliders;
   Buttons:  TButtons;
   Ok:       TButton;
   Length:   String[3];
Begin;
{xxx}
      Leave:=False;
      DefineViewPort(4, 0.55, 0.90, 0.55, 0.90);
      GraphBackColor := blue;
      OpenViewPort(4);
      GraphBackColor := DarkGray;
      print(1,1,' Define Sine Function:');
      Str(2*(Yvec.Length-2):0,Length);
      With Info do
           begin;
                 Init;
                 Create(1,0,1,0.5, 0.56,0.8,0.88, 3,'0','1', 'Amplitude',False);
                 Create(2,1,2*(Yvec.Length-2),(Yvec.Length-3),0.56,0.72,0.88, 2,'1',Length,'Lambda',False);
                 DrawAll;
           end;

      With Buttons do
           begin;
                 Init;
                 Create(11,0.57,0.65,'Draw Sine');
                 Create(22,0.75,0.65,'Hide Sine');
{                 Create(33,0.80,0.60,'Exit'); }
                 Number:=11;
                 Buttons.DrawAll;
           end;
      Ok.Create(0.67,0.565,'  Ok  ','Return');
      Repeat
            CheckForEvents;

            If Info.Changed then
               begin;
                     amp:=Info.Value(1);
                     Lambda:=Info.Value(2);
                     If Buttons.Number=11 then RemoveSine;
                     AtoMlist.Remove;
                     UpdateSine;
                     If Buttons.Number=11 then CalcDrawSine;
               end;

           If Buttons.Changed then
               Case Buttons.Number of
                    11:  CalcDrawSine;
                    22:  RemoveSine;
                 {   33:  Leave:=True;}
               End;
           Leave := Ok.Clicked;
      Until Leave=True;

      CloseViewPort(4);
      Info.Done;
      Buttons.Done;
End;

Begin;
      OldColour:=GetColor;
      AtomList.Remove;
      drawPts:=10*(Yvec.Length-2);
      If (drawPts>4096) then drawPts:=4096;
      Amp:=0.5;
      Lambda:=Yvec.Length-3;
      yDraw.Init(drawPts);
      xDraw.Init(drawPts);
      Vvec.Fill(0.0);   {make sure the Atoms get the zero velocity}
      yvec.Fill(0.0);
      UpdateSine;
      CalcDrawSine;
      SliderWindow;

      yDraw.Free;
      xDraw.Free;
      AtomList.UpdateList;
      SetColor(oldColour);
End;

Procedure ModulatedGaussian;

Var
   Lambda,
   Amp,Xo,A:  Real;
   yDraw,xDraw:   Dvector;
   OldColour,
   drawPts:    Word;

Procedure UpdateModGaussian;

Var
   Value,
   Value2:        Real;
   i:             Word;

Begin;
      For i:=2 to Yvec.Length-1 do
          begin;
                Value:=Sqr(2*(i-1-Xo)/A);
                Value2:=Sin(2*PI*(i-2)/lambda);
                If (Value<10) then Value:=Amp*Value2*Exp(-Value) else Value:=0;
                Yvec.Put(i,Value);
          end;
      AtomList.UpdateList;
      AtomList.Display;
End;

Procedure CalcDrawModGaussian;

Var
   i:            Word;
   Value,j,
   Value2:       Real;

Begin;
      j:=0.9;
      For i:=0 to drawPts-1 do
          begin;
                Value:=Sqr(2*(j-Xo)/A);
                Value2:=Sin(2*PI*i*(1+1/(drawPts-1))/(10.0*Lambda));
                If (Value<10) then Value:=Amp*Value2*Exp(-Value) else Value:=0;
                yDraw.Put(i+1,Value);
                j:=j + 0.1;
                xDraw.Put(i+1,j);
          end;

      TheChain.LockFocus;
      SetColor(White);
      PlotDVectors(xDraw,yDraw,1,drawPts);
      SetColor(OldColour);
End;

Procedure RemoveFunction;

Begin;
      TheChain.LockFocus;
      SetColor(GraphBackColor);
      PlotDVectors(xDraw,yDraw,1,drawPts);
      SetColor(OldColour);
      AtomList.Display;
End;

Procedure SliderWindow;

Var
   Leave:    Boolean;
   Info:     TSliders;
   Buttons:  TButtons;
   Ok:       TButton;
   Length2,
   Length:   String[3];

Begin;
      Leave:=False;
      DefineViewPort(4, 0.55, 0.90, 0.55, 0.90);
      GraphBackColor := blue;
      OpenViewPort(4);
      GraphBackColor := DarkGray;
      Str((Yvec.Length-2):0,Length);
      Str((Yvec.Length-3):0,Length2);
      With Info do
           begin;
                 Init;
                 Create(1,0,5,Amp,0.56,0.86,0.88,2,'0','5','Amp',False);
                 Create(2,1,(Yvec.Length-2),A,0.56,0.80,0.88,2,'1',Length,'Width',False);
                 Create(3,0,(Yvec.Length-2),Xo,0.56,0.74,0.88,2,'0',Length,'Position',False);
                 Create(4,1,(Yvec.Length-3),Lambda,0.56,0.68,0.88, 2,'1',Length2,'Lambda',False);
                 DrawAll;
           end;

      With Buttons do
           begin;
                 Init;
                 Create(11,0.57,0.63,'Draw Func');
                 Create(22,0.75,0.63,'Hide Func');
{                 Create(33,0.80,0.58,'Exit');}
                 Number:=11;
                 Buttons.DrawAll;
           end;
      Ok.Create(0.67,0.565,'  Ok  ','Return');

      Repeat
            CheckForEvents;

            If Info.Changed then
               begin;
                     amp:=Info.Value(1);
                     A:=Info.Value(2);
                     Xo:=Info.Value(3);
                     Lambda:=Info.Value(4);
                     If Buttons.Number=11 then  RemoveFunction;
                     AtomList.Remove;
                     UpdateModGaussian;
                     If Buttons.Number=11 then CalcDrawModGaussian;
               end;

           If Buttons.Changed then
               Case Buttons.Number of
                    11:  CalcDrawModGaussian;
                    22:  RemoveFunction;
{                    33:  Leave:=True;}
               End;
           Leave := Ok.Clicked;
      Until Leave=True;

      CloseViewPort(4);
      Info.Done;
      Buttons.Done;
End;

Begin;
      OldColour:=GetColor;
      AtomList.Remove;
      drawPts:=10*(Yvec.Length-2);
      If (drawPts>4096) then drawPts:=4096;
      Amp:=0.75;
      Xo:=(Yvec.Length-1)/2;
      A:=(Yvec.Length-2)/2;;
      Lambda:=5;
      yDraw.Init(drawPts);
      xDraw.Init(drawPts);
      vVec.Fill(0.0);
      Yvec.Fill(0);

      UpdateModGaussian;
      CalcDrawModGaussian;

      SliderWindow;

      yDraw.Free;
      xDraw.Free;
      AtomList.UpdateList;  {Set the velocity of the atoms}
      SetColor(OldColour)
End;

Procedure Gauss_Pulse_SymPulse(InitTYP: InitType);

Var
   Amp,Xo,A:  Real;
   yDraw,xDraw:   Dvector;
   OldColour,
   drawPts:    Word;

Procedure UpdateGaussian;

Var
   Value:         Real;
   i:             Word;

Begin;
      For i:=2 to Yvec.Length-1 do
          begin;
                Value:=Sqr(2*(i-Xo-1)/A);
                If (Value<10) then Value:=Amp*Exp(-Value) else Value:=0;
                Yvec.Put(i,Value);
          end;
      AtomList.UpdateList;
      AtomList.Display;
End;

Procedure CalcDrawGaussian;

Var
   i:            Word;
   Value,j:      Real;

Begin;
      j:=0.9;
      For i:=0 to drawPts-1 do
          begin;
                Value:=Sqr(2*(j-Xo)/A);
                If (Value<10) then Value:=Amp*Exp(-Value) else Value:=0;
                yDraw.Put(i+1,Value);
                j:=j + 0.1;
                xDraw.Put(i+1,j);
          end;

      TheChain.LockFocus;
      SetColor(White);
      PlotDVectors(xDraw,yDraw,1,drawPts);
      SetColor(OldColour);
End;

Procedure UpdatePulse;

Var
   Value:         Real;
   i:             Word;

Begin;
      For i:=2 to Yvec.Length-1 do
          begin;
                If ((i-1)<(Xo-A/2)) then Value:=0;
                If (((Xo-A/2)<=(i-1)) and ((i-1)<=(A/2+Xo))) then Value:=Amp;
                If ((i-1)>(A/2+Xo)) then Value:=0;
                Yvec.Put(i,Value);
          end;
      AtomList.UpdateList;
      AtomList.Display;
End;

Procedure CalcDrawPulse;

Var
   i:            Word;
   Value,j:      Real;

Begin;
      j:=1.4;
      For i:=0 to drawPts-1 do
          begin;
                If (j<(Xo-A/2)) then Value:=0;
                If (((Xo-A/2)<j) and (j<(A/2+Xo))) then Value:=Amp;
                If (j>(A/2+Xo)) then Value:=0;
                yDraw.Put(i+1,Value);
                xDraw.Put(i+1,j);
                j:=j+0.1;
          end;

      TheChain.LockFocus;
      SetColor(White);
      PlotDVectors(xDraw,yDraw,1,drawPts);
      SetColor(OldColour);
End;

Procedure UpdateSymPulse;

Var
   Value:         Real;
   i:             Word;

Begin;
      For i:=2 to Yvec.Length-1 do
          begin;
                If ((i-1)<(Xo-A)) then Value:=0;
                If (((Xo-A)<(i-1)) and ((i-1)<(Xo))) then Value:=Amp;
                If (((Xo)<(i-1)) and ((i-1)<(A+Xo))) then Value:=-Amp;
                If ((i-1)>(A+Xo)) then Value:=0;
                Yvec.Put(i,Value);
          end;
      AtomList.UpdateList;
      AtomList.Display;
End;

Procedure CalcDrawSymPulse;

Var
   i:            Word;
   Value,j:      Real;

Begin;
      j:=0.9;
      For i:=0 to drawPts-1 do
          begin;
                If (j<(Xo-A)) then Value:=0;
                If (((Xo-A)<j) and (j<(Xo))) then Value:=Amp;
                If (((Xo)<j) and (j<(A+Xo))) then Value:=-Amp;
                If (j>(A+Xo)) then Value:=0;
                yDraw.Put(i+1,Value);
                xDraw.Put(i+1,j);
                j:=j+0.1;
          end;

      TheChain.LockFocus;
      SetColor(White);
      PlotDVectors(xDraw,yDraw,1,drawPts);
      SetColor(OldColour);
End;

Procedure RemoveFunction;

Begin;
      TheChain.LockFocus;
      SetColor(GraphBackColor);
      PlotDVectors(xDraw,yDraw,1,drawPts);
      SetColor(OldColour);
      AtomList.Display;
End;

Procedure SliderWindow;

Var
   Leave:    Boolean;
   Info:     TSliders;
   Buttons:  TButtons;
   Ok:       TButton;
   Length:   String[3];

Begin;
      Leave:=False;
      DefineViewPort(4, 0.55, 0.90, 0.55, 0.90);
      GraphBackColor := blue;
      OpenViewPort(4);
      GraphBackColor := DarkGray;
      Str((Yvec.Length-2):0,Length);
      With Info do
           begin;
                 Init;
                 Create(1,0,5,Amp,0.56,0.85,0.88,2,'0','5','Amp',False);
                 Create(2,1,(Yvec.Length-2),A,0.56,0.78,0.88,2,'1',Length,'Width',False);
                 Create(3,0,(Yvec.Length-2),Xo,0.56,0.71,0.88,2,'0',Length,'Position',False);
                 DrawAll;
           end;

      With Buttons do
           begin;
                 Init;
                 Create(11,0.57,0.65,'Draw Func');
                 Create(22,0.75,0.65,'Hide Func');
{                 Create(33,0.80,0.60,'Exit'); }
                 Number:=11;
                 Buttons.DrawAll;
           end;
      Ok.Create(0.67,0.565,'  Ok  ','Return');

      Repeat
            CheckForEvents;

            If Info.Changed then
               begin;
                     amp:=Info.Value(1);
                     A:=Info.Value(2);
                     Xo:=Info.Value(3);
                     If Buttons.Number=11 then  RemoveFunction;
                     AtomList.Remove;
                     Case InitTYP of
                          Gauss:  UpdateGaussian;
                          Pul:    UpdatePulse;
                          SymPul: UpdateSymPulse;
                     End; {of Case}
                     If Buttons.Number=11 then
                        Case InitTYP of
                             Gauss:  CalcDrawGaussian;
                             Pul:    CalcDrawPulse;
                             SymPul: CalcDrawSymPulse;
                        End; {of Case}
               end;

           If Buttons.Changed then
               Case Buttons.Number of
                    11:  Case InitTYP of
                             Gauss:  CalcDrawGaussian;
                             Pul:    CalcDrawPulse;
                             SymPul: CalcDrawSymPulse;
                         End; {of Case}
                    22:  RemoveFunction;
{                    33:  Leave:=True;}
               End;
           Leave := Ok.Clicked;
      Until Leave=True;

      CloseViewPort(4);
      Info.Done;
      Buttons.Done;
End;

Begin;
      OldColour:=GetColor;
      AtomList.Remove;
      drawPts:=10*(Yvec.Length-2);
      If (drawPts>4096) then drawPts:=4096;
      Amp:=0.5;
      Xo:=(Yvec.Length-1)/2;
      Case InitTYP of
           Gauss: A:=2;
           Pul:   A:=4;
           SymPul: A:=4;
      End;
      yDraw.Init(drawPts);
      xDraw.Init(drawPts);
      Vvec.Fill(0.0);    {Set the velocity of the atoms}
      Yvec.Fill(0.0);
      Case InitTYP of
           Gauss:  Begin;
                         UpdateGaussian;
                         CalcDrawGaussian;
                    End;
           Pul:    Begin;
                         UpdatePulse;
                         CalcDrawPulse;
                    End;
           SymPul: Begin;
                         UpdateSymPulse;
                         CalcDrawSymPulse;
                    End;
      End;

      SliderWindow;

      yDraw.Free;
      xDraw.Free;
      AtomList.UpdateList;
      SetColor(OldColour)
End;
{--------------------------------------------------------------------------------------------------------------}
Procedure UserDefinedInit;
Var
   Ok,Cancel,GoodFunc,
   Leave,DrawFunc:    Boolean;
   FuncScreen:  TinputScreen;
   Clear:       Boolean;
   FuncStr:String;
   i,numOfAtoms:           Integer;
   drawPts:     Word;
   x,t,tempTime:         Real;
   xDraw,yDraw:   Dvector;
   yTempVec,vTempVec:DVector;
   oldColour:     Word;

Begin
Clear:=True;
OldColour:=GetColor;
numOfAtoms:=yVec.Length-2;
drawPts:=10*numOfAtoms;
If (drawPts>1024) then drawPts:=1024;
xDraw.init(drawPts);
yDraw.init(drawPts);
yTempVec.init(yVec.Length);
yTempVec.Equate(1,yVec);
vTempVec.init(vVec.Length);
vTempVec.Equate(1,vVec);
For i:=1 to drawPts Do xDraw.VecPtr^[i]:=((i-1)/DrawPts)*(numOfAtoms+1);
tempTime:=time;
time:=0;
Parser.init;
     With FuncScreen do
          begin
               funcScreen.Init;
               DefineInputPort(0.01,1.0,0.52,0.90);
               LoadLine('                         Input the initial function. ');
               LoadLine('Add to exisiting wavefunction?  #F  ');
               LoadLine('Function =                                         ');
               LoadLine('"                                                                            "');
               LoadLine('"                                                                            "');
               LoadLine('');
               LoadLine('  Input a function of x and t  --  (e.g.,cos(3*x)*Exp(-2*t).');
               LoadLine('         [  Ok  ]        [Cancel]        [ Help ]        [ View ]       ');
               SetHelpFile('Chain.Hlp','Chain-Parser Help');
               SetString(2,ParserStr1);
               SetString(3,ParserStr2);
               Leave:=False;
               Repeat
                     AcceptScreen;
                     clear:=Not GetBoolean(1);
                     Ok:=GetBoolean(4);
                     Cancel:=GetBoolean(5);
                     DrawFunc:=GetBoolean(7);
                     If Cancel Then Begin
                                    Time:=tempTime;
                                    yVec.Equate(1,yTempVec);
                                    vVec.Equate(1,vTempVec);
                                    xDraw.Free;
                                    yDraw.Free;
                                    yTempVec.Free;
                                    vTempVec.Free;
                                    FuncScreen.Done;
                                    AtomList.UpdateList;
                                    SetColor(oldColour);
                                    Exit;
                                    End;
                     FuncStr:=GetString(2)+GetString(3);
                     If FuncStr='' then FuncStr:='0';
                     GoodFunc := Parser.Parse('x','t',' ',' ',funcStr);
                     if GoodFunc and (DrawFunc or Ok) Then
                     Begin
                       errorFound := false;
                       HaltIfError := false;
                       i:=1;
                       while (i<=yVec.Length) and not ErrorFound Do begin
                         yVec.VecPtr^[i]:=Parser.F(i-1,0,0,0);
                         vVec.VecPtr^[i]:=(Parser.F(i-1,dt,0,0)-Parser.F(i-1,-dt,0,0))/(2*dt);
                         if not clear then begin
                            yVec.VecPtr^[i]:=yVec.VecPtr^[i]+yTempVec.Vecptr^[i];
                            vVec.VecPtr^[i]:=vVec.VecPtr^[i]+vTempVec.VecPtr^[i];
                         End;
                         inc(i);
                       end;
                       GoodFunc := not ErrorFound;
                       errorFound := false;
                       HaltIfError := true;
                     end;
                     if GoodFunc and DrawFunc Then
                     Begin
                       errorFound := false;
                       HaltIfError := false;
                       x:=0;
                       t:=0;
                       i:=1;
                       while (i<=drawPts) and not ErrorFound Do
                       Begin
                         x:=xDraw.VecPtr^[i];
                         t:=0;
                         yDraw.vecPtr^[i]:= Parser.F(x,t,0,0);
                         inc(i);
                       End;
                         AtomList.UpdateList;
                         TheChain.Display;
                         TheChain.LockFocus;
                         SetColor(White);
                         PlotDVectors(xDraw,yDraw,1,drawPts);
                         SetColor(OldColour);
                         GoodFunc := not ErrorFound;
                         errorFound := false;
                         HaltIfError := true;
                       End;
                     if GoodFunc and OK then Leave:=True;
                     if not GoodFunc then
                       Announce('That function is not acceptable - f(x,t).');
               Until Leave;
               ParserStr1:=getstring(2);
               ParserSTr2:=getString(3);
               FuncStr:=GetString(2)+GetString(3);
               FuncScreen.Done;
          end;
yInitVec.Equate(1,yVec);
vInitVec.Equate(1,vVec);
time:=0;
xDraw.Free;
yDraw.Free;
yTempVec.Free;
vTempVec.Free;
AtomList.UpdateList;
SetColor(oldColour);
End;



{----------------------------------------------------------------------------------------------------------------}
Procedure Modes;

Var
   Omega,Value,
   Value2,Amp:          Real;
   OldColour,i,j:       Word;
   InptModeScrn:        TInputScreen;
   Ok, Cancel:          Boolean;


Begin;
OldColour:=GetColor;
      With InptModeScrn do
           begin;
                 Init;
                 DefineInputPort( 0.31, 0.70, 0.37 ,0.90 );

                 LoadLine('      Mode Preferences      ');
                 LoadLine('   for a Monatomic Chain    ');
                 LoadLine(' Mode Number       Amplitude');
                 LoadLine('  N = 1       #F    {    }  ');
                 Loadline('  N = 2       #F    {    }  ');
                 Loadline('  N = 3       #F    {    }  ');
                 Loadline('  N = 4       #T    {    }  ');
                 Loadline('  N = 5       #F    {    }  ');
                 Loadline('  N = {    }  #F    {    }  ');
                 Loadline('  N = {    }  #F    {    }  ');
                 LoadLine('                            ');
                 LoadLine(' [  Ok  ] [Cancel] [ Help ] ');

                 With ModeData do
                      begin;
                            SetBoolean(1,B1);SetNumber(2,  Amp1);
                            SetBoolean(3,B2);SetNumber(4,  Amp2);
                            SetBoolean(5,B3);SetNumber(6,  Amp3);
                            SetBoolean(7,B4);SetNumber(8,  Amp4);
                            SetBoolean(9,B5);SetNumber(10, Amp5);

                            SetNumber(11, Mode6);
                            SetBoolean(12,B6);
                            SetNumber(13, Amp6);

                            SetNumber(14, Mode7);
                            SetBoolean(15,B7);
                            SetNumber(16, Amp7);
                      end;

                 SetHelpFile('Chain.Hlp', 'Mode Help');


                 AcceptScreen;

                 Ok:=GetBoolean(17);
                 Cancel:=GetBoolean(18);

                 If Ok then
                    With ModeData do
                         begin;
                               B1:=getBoolean(1);Amp1:=GetNumber(2);
                               B2:=getBoolean(3);Amp2:=GetNumber(4);
                               B3:=getBoolean(5);Amp3:=GetNumber(6);
                               B4:=getBoolean(7);Amp4:=GetNumber(8);
                               B5:=getBoolean(9);Amp5:=GetNumber(10);

                               Mode6:=GetNumber(11);
                               B6:=GetBoolean(12);
                               Amp6:=GetNumber(13);

                               Mode7:=GetNumber(14);
                               B7:=GetBoolean(15);
                               Amp7:=GetNumber(16);
                         end;


                 If Ok then
                    begin;
                          AtomList.Remove;
                          For i:=0 to (Yvec.Length-3) do
                              begin;
                                    Value:=0;
                                    Value2:=0;
                                    Omega:=0;


                    If (LeftB=Free) and (RightB=Fixed) then
                       begin;
                                    For j:=1 to 5 do
                                        If GetBoolean(2*j-1) then
                 Value:=Value + GetNumber(2*j)*Sin(Pi/2 + (2*j-1)*Pi*(i)/(2*(Yvec.Length-3)));
                                        If GetBoolean(12) then
                 Value:=Value + GetNumber(13)*Sin(Pi/2 + (2*GetNumber(11)-1)*Pi*(i)/(2*(Yvec.Length-3)));
                                        If GetBoolean(15) then
                 Value:=Value + GetNumber(16)*Sin(Pi/2 + (2*GetNumber(14)-1)*Pi*(i)/(2*(Yvec.Length-3)));

                 Yvec.Put(i+2,Value);

                 Vvec.Put(i+2,0);

                       end;{ End of Do j}

                    If (LeftB=Fixed) and (RightB=Free) then
                       begin;
                                    For j:=1 to 5 do
                                        If GetBoolean(2*j-1) then
                 Value:=Value + GetNumber(2*j)*Sin((2*j-1)*Pi*(i)/(2*(Yvec.Length-3)));
                                        If GetBoolean(12) then
                 Value:=Value + GetNumber(13)*Sin((2*GetNumber(11)-1)*Pi*(i)/(2*(Yvec.Length-3)));
                                        If GetBoolean(15) then
                 Value:=Value + GetNumber(16)*Sin((2*GetNumber(14)-1)*Pi*(i)/(2*(Yvec.Length-3)));

                 Yvec.Put(i+2,Value);

                 Vvec.Put(i+2,0);

                       end;{ End of Do j}

                    If (LeftB=Fixed) and (RightB=Fixed) then
                       begin;
                                    For j:=1 to 5 do
                                        If GetBoolean(2*j-1) then
                 Value:=Value + GetNumber(2*j)*Sin(j*Pi*(i)/(Yvec.Length-3));
                                        If GetBoolean(12) then
                 Value:=Value + GetNumber(13)*Sin(GetNumber(11)*Pi*(i)/(Yvec.Length-3));
                                        If GetBoolean(15) then
                 Value:=Value + GetNumber(16)*Sin(GetNumber(14)*Pi*(i)/(Yvec.Length-3));

                 Yvec.Put(i+2,Value);

                 Vvec.Put(i+2,0);

                       end;{ End of Do j}

                    If (LeftB=Free) and (RightB=Free) then
                       begin;
                                    For j:=1 to 5 do
                                        If GetBoolean(2*j-1) then
                 Value:=Value + GetNumber(2*j)*Sin(Pi/2 + j*Pi*(i)/(Yvec.Length-3));
                                        If GetBoolean(12) then
                 Value:=Value + GetNumber(13)*Sin(Pi/2 + GetNumber(11)*Pi*(i)/(Yvec.Length-3));
                                        If GetBoolean(15) then
                 Value:=Value + GetNumber(16)*Sin(Pi/2 + GetNumber(14)*Pi*(i)/(Yvec.Length-3));

                 Yvec.Put(i+2,Value);

                 Vvec.Put(i+2,0);

                       end;{ End of Do j}

                    If (RightB=Continuous) and (RightB=Continuous) then
                       begin;
                                    For j:=1 to 5 do
                                        If GetBoolean(2*j-1) then
                                           begin;
                 Value:=Value + GetNumber(2*j)*Sin(2*j*Pi*(i+0.5)/(Yvec.Length-2));
                 Omega:=Sqrt(K1vec.Value(i+1))*abs(sin(Pi*j/(Yvec.Length-2)));
                 Value2:=Value2 - omega*Cos(2*Pi*j*(i+0.5)/(Yvec.Length-2));
                                           end;
                                        If GetBoolean(12) then
                                           begin;
                 Value:=Value + GetNumber(13)*Sin(2*GetNumber(11)*Pi*(i+0.5)/(Yvec.Length-2));
                 Omega:=Sqrt(K1vec.Value(i+1))*abs(sin(Pi*GetNumber(11)/(Yvec.Length-2)));
                 Value2:=Value2 - omega*Cos(2*Pi*GetNumber(11)*(i+0.5)/(Yvec.Length-2));
                                           end;
                                        If GetBoolean(15) then
                                           begin;
                 Value:=Value + GetNumber(16)*Sin(2*GetNumber(14)*Pi*(i+0.5)/(Yvec.Length-2));
                 Omega:=Sqrt(K1vec.Value(i+1))*abs(sin(Pi*GetNumber(14)/(Yvec.Length-2)));
                 Value2:=Value2 - omega*Cos(2*Pi*GetNumber(14)*(i+0.5)/(Yvec.Length-2));
                                           end;

                 Yvec.Put(i+2,Value);

                 Vvec.Put(i+2,Value2);
                       end;
                          end;

                          AtomList.UpdateList;
                          {Display;}
                          SetColor(OldColour);
                 end;

                 If Cancel then
                    begin;
                          Done;
                          SetColor(oldColour);
                          Exit;
                    end;

                 Done;
           end;
SetColor(OldColour);
End;

Procedure SetCMFrame;

Var
   M,V,L,P,LM: Real;
   i:   Integer;

Begin;
      M:=0;

      P:=0;
      V:=0;

      LM:=0;
      L:=0;
      For i:=2 to (Yvec.Length-1) do M:=(M + Mvec.VecPtr^[i]);

      For i:=2 to (Yvec.Length-1) do P:=P + (Mvec.VecPtr^[i]*Vvec.Vecptr^[i]);
      V:=P/M;
      For i:=2 to (Yvec.Length-1) do Vvec.Vecptr^[i]:=(Vvec.Vecptr^[i] - V);

      For i:=2 to (Yvec.Length-1) do LM:=LM + (Mvec.VecPtr^[i]*Yvec.Vecptr^[i]);
      L:=LM/M;
      For i:=2 to (Yvec.Length-1) do Yvec.Vecptr^[i]:=(Yvec.Vecptr^[i] - L);
      if Not Running Then AtomList.UpdateList;
End;

End.