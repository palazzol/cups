           (*************************************************)
           (*************************************************)
           (**                                             **)
           (** Consortium of Upper-level Physics Software  **)
           (**             ( CUPS Project )                **)
           (**     (c) 1994 by John Wiley & Sons           **)
           (** Compiled with Utilities Ver. 1.5 (95/02/13) **)
           (**                                             **)
           (*************************************************)
           (*************************************************)

{+-------------------------------------------------------------------}
{| PROCEDURES FOR MOTION GENERATOR for MECHANICS section of CUPS
{| Author:  Randall Jones
{|    Uses (10-03-92) CUPS utilities
{| Date:  {1/12/93}
{+-------------------------------------------------------------------}
UNIT GMUtil1;
INTERFACE
       USES Graph,Crt,Dos,CUPS,CUPSMUPP,CUPSgui,CupsGrph,CUPSProc,GMUtil2;

{+---------------------------------------------------------------------}
{| The names and calling structures of Procedures and Functions defined
{| in the main routine must be described here so the procedures in this
{| unit know what they look like.
{+---------------------------------------------------------------------}
      TYPE
      DynProc1 = PROCEDURE;
      DynProc2 = PROCEDURE(
                 VAR v1, v2, v3, v4, v5, v6, v7, v8, v9,v10,
                     v11,v12,v13,v14,v15,v16,v17,v18,v19,v20:Real);
      DynProc3 = PROCEDURE(yAxis,xAxis:String5; Erase:Boolean;
                 VAR  v1, v2, v3, v4, v5, v6, v7, v8, v9,v10,
                      v11,v12,v13,v14,v15,v16,v17,v18,v19,v20:Real);
      DynFun1 = FUNCTION(FunName:String5;
                VAR  v1, v2, v3, v4, v5, v6, v7, v8, v9,v10,
                     v11,v12,v13,v14,v15,v16,v17,v18,v19,v20:Real):Real;
       VAR
          DefForceParms:DynProc1;
          CalcAccel:DynProc2;
          CalcDynFun:DynFun1;
          InitWindow:DynProc3;
          AnimateWindow:DynProc3;
     PROCEDURE RunMotSim(DefForceParmsProc:DynProc1;
                         CalcAccelProc:DynProc2;
                         CalcDynFunProc:DynFun1;
                         InitWindowProc:DynProc3;
                         AnimateWindowProc:DynProc3);
{+---------------------------------------------------------------------}
{| Functions and Procedures
{+---------------------------------------------------------------------}
   PROCEDURE NumericalInit;
   PROCEDURE Euler(dt,t,tEnd:Real);
   PROCEDURE RK2(dt,t,tEnd:Real);
   PROCEDURE RK4Deriv(t:Real; VAR xV,xVP:DVector);
   PROCEDURE RK4Newt(dt,t,tEnd:Real);
   PROCEDURE GenPos(dt,tStart,tEnd:Real);
   PROCEDURE CallCalcAccel(t:Real);
   FUNCTION CallCalcDynFun(FunName:String5;
                           VAR Vals:ValsType):Real;
   PROCEDURE CallInitWindow(yAxis,xAxis:String5; Erase:Boolean);
   PROCEDURE CallAnimateWindow(yAxis,xAxis:String5; Erase:Boolean;
                               VAR DynVals:ValsType);
   PROCEDURE RerunMotion;
   PROCEDURE ResetVals;
   PROCEDURE InitGenMot;
   PROCEDURE HandleMouse;
   PROCEDURE HandleHotKey(KeyHit:Byte);
   PROCEDURE GenMotion;

IMPLEMENTATION
{+---------------------------------------------------------------------}
{| The arguments for the user-defined procedures and functions are so
{| nasty that an intermediary procdure is used with fewer arguments
{| This can be done since ForceDef is global.
{+---------------------------------------------------------------------}
   PROCEDURE CallCalcAccel(t:Real);
   BEGIN
      ForceDef.VarVals[1] := t;
      WITH ForceDef DO
         CalcAccel(VarVals[1],
            VarVals[2],VarVals[3],VarVals[4],
            VarVals[5],VarVals[6],VarVals[7],
            VarVals[8],VarVals[9],VarVals[10],
            ParmVals[1],ParmVals[2],ParmVals[3],ParmVals[4],ParmVals[5],
            ParmVals[6],ParmVals[7],ParmVals[8],ParmVals[9],ParmVals[10]);
   END;

   FUNCTION CallCalcDynFun(FunName:String5;
                           VAR Vals:ValsType):Real;
   BEGIN
      WITH ForceDef DO
      CallCalcDynFun := CalcDynFun(FunName,Vals[1],Vals[2],Vals[3],Vals[4],
            Vals[5],Vals[6],Vals[7],Vals[8],Vals[9],Vals[10],
            ParmVals[1],ParmVals[2],ParmVals[3],ParmVals[4],ParmVals[5],
            ParmVals[6],ParmVals[7],ParmVals[8],ParmVals[9],ParmVals[10]);
   END;

   PROCEDURE CallInitWindow(yAxis,xAxis:String5; Erase:Boolean);
   BEGIN
      WITH ForceDef DO
         InitWindow(yAxis,xAxis,Erase,VarVals[1],
            VarVals[2],VarVals[3],VarVals[4],
            VarVals[5],VarVals[6],VarVals[7],
            VarVals[8],VarVals[9],VarVals[10],
            ParmVals[1],ParmVals[2],ParmVals[3],ParmVals[4],ParmVals[5],
            ParmVals[6],ParmVals[7],ParmVals[8],ParmVals[9],ParmVals[10]);
   END;

   PROCEDURE CallAnimateWindow(yAxis,xAxis:String5; Erase:Boolean;
                               VAR DynVals:ValsType);
   BEGIN
      WITH ForceDef DO
      AnimateWindow(yAxis,xAxis,Erase,DynVals[1],
            DynVals[2],DynVals[3],DynVals[4],
            DynVals[5],DynVals[6],DynVals[7],
            DynVals[8],DynVals[9],DynVals[10],
            ParmVals[1],ParmVals[2],ParmVals[3],ParmVals[4],ParmVals[5],
            ParmVals[6],ParmVals[7],ParmVals[8],ParmVals[9],ParmVals[10]);
   END;

{+-------------------------------------------------------------------}
{| Numerical routines
{+-------------------------------------------------------------------}
   PROCEDURE NumericalInit;
      BEGIN
         NumNumMeth := 3;
         NumProcs[1] := 'Euler';
         NumProcs[2] := 'RK2';
         NumProcs[3] := 'RK4';
      END;

    PROCEDURE Euler(dt,t,tEnd:Real);
   {+----------------------------------------------------------------}
   {| This procedure uses the Euler method to generate the next value
   {| of the position and velocity
   {| Note:  If tEnd<tStart, a negative dt is used and calculation is
   {| performed backwards.
   {+----------------------------------------------------------------}
    VAR
        IterInd,i,NumIter,PosInd,VelInd,AccInd: integer;
    BEGIN
        NumIter:=Trunc(Abs(tEnd-t)/dt);
        if (tEnd<t) then dt:=-dt;
        For IterInd:=1 TO NumIter DO
           BEGIN
              CallCalcAccel(t);
              WITH ForceDef DO FOR i := 1 to NumDim DO
                 BEGIN
                    PosInd := 3*i-1;
                    VelInd := PosInd+1;
                    AccInd := PosInd+2;
                    VarVals[PosInd] := VarVals[PosInd] + VarVals[VelInd]*dt;
                    VarVals[VelInd] := VarVals[VelInd] + VarVals[AccInd]*dt;
                 END;
              t := t + dt;
           END;
       {Include remaining interval}
        dt:=tEnd-t;
        IF dt<>0.0 THEN
           BEGIN
              CallCalcAccel(t);
              WITH ForceDef DO FOR i := 1 to NumDim DO
                 BEGIN
                    PosInd := 3*i-1;
                    VelInd := PosInd+1;
                    AccInd := PosInd+2;
                    VarVals[PosInd] := VarVals[PosInd] + VarVals[VelInd]*dt;
                    VarVals[VelInd] := VarVals[VelInd] + VarVals[AccInd]*dt;
                 END;
           END;
    END;

    PROCEDURE RK2(dt,t,tEnd:Real);
   {+----------------------------------------------------------------}
   {| This procedure uses an RK2 method to generate the next value
   {| of the position and velocity
   {| Note:  If tEnd<tStart, a negative dt is used and calculation is
   {| performed backwards.
   {+----------------------------------------------------------------}
    VAR
        IterInd,i,NumIter,PosInd,VelInd,AccInd: integer;
        TmpVals:ValsType;
    BEGIN
        NumIter:=Trunc(Abs(tEnd-t)/dt);
        if (tEnd<t) then dt:=-dt;
        For IterInd:=1 TO NumIter DO
           BEGIN
              CallCalcAccel(t);
              WITH ForceDef DO FOR i := 1 to NumDim DO
                 BEGIN
                    PosInd := 3*i-1;
                    VelInd := PosInd+1;
                    AccInd := PosInd+2;
                    TmpVals[PosInd] := VarVals[PosInd];
                    TmpVals[VelInd] := VarVals[VelInd];
                    TmpVals[AccInd] := VarVals[AccInd];
                    VarVals[PosInd] := VarVals[PosInd] + VarVals[VelInd]*dt;
                    VarVals[VelInd] := VarVals[VelInd] + VarVals[AccInd]*dt;
                 END;
              t := t + dt;
              CallCalcAccel(t);
              WITH ForceDef DO FOR i := 1 to NumDim DO
                 BEGIN
                    PosInd := 3*i-1;
                    VelInd := PosInd+1;
                    AccInd := PosInd+2;
                    VarVals[PosInd] := TmpVals[PosInd] +
                                   (TmpVals[VelInd]+VarVals[VelInd])/2*dt;
                    VarVals[VelInd] := TmpVals[VelInd] +
                                   (TmpVals[AccInd]+VarVals[AccInd])/2*dt;
                 END;
           END;
       {Include remaining interval}
        dt:=tEnd-t;
        IF dt<>0.0 THEN
           BEGIN
              CallCalcAccel(t);
              WITH ForceDef DO FOR i := 1 to NumDim DO
                 BEGIN
                    PosInd := 3*i-1;
                    VelInd := PosInd+1;
                    AccInd := PosInd+2;
                    TmpVals[PosInd] := VarVals[PosInd];
                    TmpVals[VelInd] := VarVals[VelInd];
                    TmpVals[AccInd] := VarVals[AccInd];
                    VarVals[PosInd] := VarVals[PosInd] + VarVals[VelInd]*dt;
                    VarVals[VelInd] := VarVals[VelInd] + VarVals[AccInd]*dt;
                 END;
              t := t + dt;
              CallCalcAccel(t);
              WITH ForceDef DO FOR i := 1 to NumDim DO
                 BEGIN
                    PosInd := 3*i-1;
                    VelInd := PosInd+1;
                    AccInd := PosInd+2;
                    VarVals[PosInd] := TmpVals[PosInd] +
                                   (TmpVals[VelInd]+VarVals[VelInd])/2*dt;
                    VarVals[VelInd] := TmpVals[VelInd] +
                                   (TmpVals[AccInd]+VarVals[AccInd])/2*dt;
                 END;
           END;
    END;

   PROCEDURE RK4Deriv(t:Real; VAR xV,xVP:DVector);
   VAR
      i,i2,i3:Integer;
   BEGIN WITH ForceDef DO
      BEGIN
         FOR i := 1 to NumDim DO
            BEGIN
               i3 := 3*i;
               i2 := 2*i;
               VarVals[i3-1] := xV.Value(i2-1);    {Position}
               VarVals[i3] := xV.Value(i2);        {Velocity}
               xVP.Put(i2-1,VarVals[i3]);          {Vel -> PosP [1,3,5]}
            END;
         CallCalcAccel(t);
         FOR i := 1 to NumDim DO
            xVP.Put(2*i,VarVals[3*i+1]); {Acc -> VelP [2,4,6]}
      END;
   END;

    PROCEDURE RK4Newt(dt,t,tEnd:Real);
{+-------------------------------------------------------------------}
{| This procedure uses the RK4 method to generate the next value
{| of the position and velocity.  Since Position and Velocity info
{| are not of type DVector their values must be transfered to DVectors.
{| Values are stored as [x,vx,y,vy,z,vz]
{| Note:  If tEnd<tStart, a negative dt is used and calculation is
{| performed backwards.
{+-------------------------------------------------------------------}
    VAR
        i,NumIter: integer;
    BEGIN
        WITH ForceDef DO FOR i := 1 to NumDim DO
           BEGIN
              xV.Put(2*i-1,VarVals[3*i-1]);     {Position}
              xV.Put(2*i,VarVals[3*i]);         {Velocity}
           END;
        NumIter:=Trunc(Abs(tEnd-t)/dt);
        if (tEnd<t) then dt:=-dt;
        For i:=1 TO NumIter DO StepRK4(RK4Deriv,t,dt,xV);
        {Include remaining interval}
        dt:=tEnd-t;
        IF dt<>0.0 THEN StepRk4(RK4Deriv,t,dt,xV);
        WITH ForceDef DO FOR i := 1 to NumDim DO
           BEGIN
              VarVals[3*i-1] := xV.Value(2*i-1);   {Position}
              VarVals[3*i] := xV.Value(2*i);       {Velocity}
           END;
    END;

   PROCEDURE GenPos(dt,tStart,tEnd:Real);
  {+-------------------------------------------------------------------}
  {| This procedure generates the next value of the dynamical variables
  {| Note that CallCalcAccel(t) inserts t into VarVals[1].
  {+-------------------------------------------------------------------}
    BEGIN
        CASE ForceDef.NumMeth OF
         1: Euler(dt,tStart,tEnd);
         2: RK2(dt,tStart,tEnd);
         3: RK4Newt(dt,tStart,tEnd);
        END;
       {Calculate acceleration at this point}
        CallCalcAccel(tEnd);
    END;


   FUNCTION SelDynVar(GraphVar:Integer; VAR VarValsHere:ValsType):Real;
  {+-------------------------------------------------------------------}
  {| This function is used to select a numerical value of a dynamical
  {| variable or function.
  {| GraphVar - [ 0..10] Variable: (0,t,x,y,z,vx,vy,vz,ax,ay,az)
  {|            [11..20] Function
  {+-------------------------------------------------------------------}
   BEGIN
      WITH ForceDef DO
         IF GraphVar <=10
            THEN SelDynVar := VarValsHere[GraphVar]
            ELSE SelDynVar := CallCalcDynFun(VarNames[GraphVar],VarValsHere);
   END;

   PROCEDURE PrintData;
      VAR
         VarToDisp,i,yPos:Integer;
         Val:Real;
      BEGIN
         yPos := 3;
         SelectMuppetPort;
         RubOut(PrintPos[0],yPos,12,Blakk);
         Print(PrintPos[0],yPos,Concat(Num2Str(dtAnim,5),PrintUnits[0]));
         i := 1;
         REPEAT
            VarToDisp := ForceDef.DisplayVars[i];
            IF VarToDisp <>0 THEN
               BEGIN
                  Val := SelDynVar(VarToDisp,ForceDef.VarVals);
                  RubOut(PrintPos[i],yPos,14,Blakk);
                  Print(PrintPos[i],yPos,Concat(Num2Str(Val,7),PrintUnits[i]));
                  INC(i);
               END;
            UNTIL (i>3) OR (VarToDisp=0);
        END;

   PROCEDURE ResetVals;
  {+-------------------------------------------------------------------}
  {| This procedure initializes the dynamical variables.
  {+-------------------------------------------------------------------}
   VAR
      i:Integer;
      t:Real;
   BEGIN
      WITH ForceDef DO
         BEGIN
            dtAnim := dtAnimInit;
            t:=InitVals[1];
            FOR i := 1 to NumDim DO
               BEGIN
                  VarVals[3*i-1] := InitVals[2*i];    {Position}
                  VarVals[3*i] := InitVals[2*i+1];    {Velocity}
               END;
            CallCalcAccel(t);
            FOR i := 1 to NumDynVars DO DataStore[i,1] := VarVals[i];
            NumPnts := 1;
         END;
   END;

{+---------------------------------------------------------------------}
{| Graph Set-Up
{+---------------------------------------------------------------------}
   PROCEDURE RerunMotion;
  {+-------------------------------------------------------------------}
  {| This procedure reproduces the graphs from data stored in the
  {| arrays DataStore[1..10,1..500];
  {+-------------------------------------------------------------------}
   VAR
      OldVarVals: ValsType;
      xVarOld,yVarOld,xVarNew,yVarNew: Real;
      i,n,nD,StartPnt,NumToDraw: Integer;
      xScr,yScr:Integer;
      xAxisVar,yAxisVar:String5;
   BEGIN
      HideCursor;
      SetColor(ColArray[ColVar]);
      SetWriteMode(NormalPut);
      IF NumPnts<=MaxData
         THEN BEGIN StartPnt := 0; NumToDraw := NumPnts; END
         ELSE BEGIN StartPnt := NumPnts MOD MaxData; NumToDraw:=MaxData; END;
      WITH ForceDef DO
         BEGIN
            FOR i := 1 to NumDynVars DO
               VarVals[i] := DataStore[i,1+StartPnt];
            FOR n := 1 to NumToDraw-1 DO
               BEGIN
                  OldVarVals := VarVals;
                  nD := 1 + ((StartPnt+n) MOD MaxData);
                  FOR i := 1 to NumDynVars DO VarVals[i] := DataStore[i,nD];
                  FOR i := 1 TO NumOutWins DO WITH OutWin[i] DO
                     BEGIN
                        xVarOld := SelDynVar(HorizVar,OldVarVals);
                        yVarOld := SelDynVar(VertVar,OldVarVals);
                        xVarNew := SelDynVar(HorizVar,ForceDef.VarVals);
                        yVarNew := SelDynVar(VertVar,ForceDef.VarVals);
                        xAxisVar := VarNames[HorizVar];
                        yAxisVar := VarNames[VertVar];
                        SelectViewPort(i);
                        SelectScale(i);
                        IF PlotTraj THEN
                           PlotLine(xVarOld,yVarOld,xVarNew,yVarNew);
                     END;
               END;
         END;
      SetColor(15); SetWriteMode(NormalPut);
      PrintData;
      ShowCursor;
    END;

   PROCEDURE InitOutWins(Erase:Boolean);
  {+-------------------------------------------------------------------}
  {| This procedure puts a circle on each graph at the current values
  {| of the dynamical values and calls InitAnimWin to display
  {| the inital user defined graphics.  Current output variables are
  {| written to the screen as well.  The procedure is also used to erase
  {| the circles when the system is reset.
  {| Erase is Passed to the user defined procedure InitAnimWin to allow
  {| the user to take different action if InitOutWins is being used to
  {| erase the current screen.
  {+-------------------------------------------------------------------}
   VAR
      i:Integer;
      xVar,yVar:Real;
      xScr,yScr:Integer;
      xAxisVar,yAxisVar:String5;
   BEGIN
      WITH ForceDef DO FOR i := 1 TO NumOutWins DO WITH OutWin[i] DO
         BEGIN
            SelectViewPort(i);
            SelectScale(i);
            xAxisVar := VarNames[HorizVar];
            yAxisVar := VarNames[VertVar];
            CallInitWindow(yAxisVar,xAxisVar,Erase);
            SetColor(15);
         END;
      IF (NumPnts>0) AND (NOT Erase) THEN RerunMotion;
      WITH ForceDef DO FOR i := 1 TO NumOutWins DO WITH OutWin[i] DO
         BEGIN
            SelectViewPort(i);
            SelectScale(i);
            xAxisVar := VarNames[HorizVar];
            yAxisVar := VarNames[VertVar];
            xVar := SelDynVar(HorizVar,VarVals);
            yVar := SelDynVar(VertVar,VarVals);
            SetWriteMode(XORPut);
            IF NOT Erase THEN With GraphScales[i] DO
               InWindow[i] := (xVar>=Xmin) AND (xVar<=Xmax) AND
                              (yVar>=Ymin) AND (yVar<=Ymax);
            IF InWindow[i] THEN
               BEGIN
                  Map(xVar,yVar,xScr,yScr);
                  PutImage(xScr-5,yScr-5,CircleData,XORPut);
                 {+----------------------------------------------------}
                 {| Call user defined procedures to initialize graphics
                 {+----------------------------------------------------}
                  SetWriteMode(XORPut);
                  CallAnimateWindow(yAxisVar,xAxisVar,Erase,VarVals);
               END;
         END;
     {+----------------------------------------------------------------}
     {| Write dynamical variables to screen.
     {+----------------------------------------------------------------}
      SetWriteMode(NormalPut);
      SetColor(15);
      PrintData;
   END;

   PROCEDURE OpenPlot(UserFlag:Boolean);
   {+-----------------------------------------------------------------+}
   {| This procedure clears the graphics screen then displays each
   {| window, with axis and labels.
   {| The boxes around each window are drawn explicitly rather than
   {| using OpenViewPort so that windows may be stacked transparently.
   {| If UserFlag is true, InitWins is called to draw initial values
   {| Find better name - Userflag is false when moving windows around
   {+-----------------------------------------------------------------+}
   VAR
      i: Integer;
      Tickx,Ticky: Real;
   BEGIN
      ClearMuppetPort;
      HideCursor;
      SetWriteMode(NormalPut);
     {+----------------------------------------------------------------}
     {| Display all output windows
     {+----------------------------------------------------------------}
      WITH ForceDef DO FOR i := 1 to NumOutWins DO WITH OutWin[i] DO
         BEGIN
            DefineViewPort(i,LeftEdge,LeftEdge+Width,
                           1-TopEdge-Height,1-TopEdge);
            IF Transparent THEN
               BEGIN
                  SelectMUPPETport;
                  Rectangle(Round(LeftEdge*GetMaxX),
                            Round(TopEdge*GetMaxY),
                            Round((LeftEdge+Width)*GetMaxX),
                            Round((TopEdge+Height)*GetMaxY));
                  SelectViewport(i);
               END
            ELSE OpenViewPort(i);
            DefineScale(i,HMin,HMAx,VMin,VMax);
            SelectScale(i);
            IF HorizVar=1 THEN dtAnimMax := 0.02*ABS(HMax-HMin);
            IF DisplayAxes THEN
               BEGIN
                  IF(HorizVar=0) THEN Tickx := 20
                     ELSE Tickx := TickSpace(HMax-HMin);
                  IF(VertVar=0)  THEN Ticky := 20
                     ELSE Ticky := TickSpace(VMax-VMin);
                  Axis(0,0,Tickx,Ticky);
                  PutLabel(Bottom,VarDesc[HorizVar]);
                  PutLabel(Left,VarDesc[VertVar]);
               END;
            SetColor(Yellow);
            PutLabel(Inside,GraphTitle);
            SetColor(White);
         END;
      PrintForceTitle;
      PrintText;
      MainMenu.Display;
      HotKeys.Display;
      IF UserFlag THEN InitOutWins(False);
      ShowCursor;
   END;

   PROCEDURE GenMotion;
  {+-------------------------------------------------------------------}
  {| This procedure generates the next time step for the dynamical
  {| variables and updates the graphs.
  {| To minimize blinking of animation circle, old values of the dynamical
  {| variables are stored so that circle can be erased and then redrawn
  {| immediately.
  {+-------------------------------------------------------------------}
   VAR
      OldVarVals: ValsType;
      t,tOld,xVarOld,yVarOld,xVarNew,yVarNew: Real;
      xScr,yScr:Integer;
      i,nD: Integer;
      xAxisVar,yAxisVar:String5;
   BEGIN
      tOld := ForceDef.VarVals[1];
      t := tOld + dtAnim - dtShift;
      dtShift := 0.0;
      OldVarVals := ForceDef.VarVals;
      GenPos(dt,tOld,t);
      nD := 1 + (NumPnts MOD MaxData);
      NumPnts := NumPnts + 1;
      WITH ForceDef DO
         FOR i := 1 to NumDynVars DO DataStore[i,nD] := VarVals[i];
      IF NOT RUNFLAG THEN HideCursor;
      SetColor(15); SetWriteMode(XORPut);
      WITH ForceDef DO FOR i := 1 TO NumOutWins DO WITH OutWin[i] DO
         BEGIN
            xVarOld := SelDynVar(HorizVar,OldVarVals);
            yVarOld := SelDynVar(VertVar,OldVarVals);
            xVarNew := SelDynVar(HorizVar,ForceDef.VarVals);
            yVarNew := SelDynVar(VertVar,ForceDef.VarVals);
            xAxisVar := VarNames[HorizVar];
            yAxisVar := VarNames[VertVar];
            SelectViewPort(i);
            SelectScale(i);
           {+----------------------------------------------------------}
           {| Erase old stuff if it was inside the window
           {+----------------------------------------------------------}
            IF InWindow[i] THEN
               BEGIN
                  Map(xVarOld,yVarOld,xScr,yScr);
                  PutImage(xScr-5,yScr-5,CircleData,XORPut);
                  CallAnimateWindow(yAxisVar,xAxisVar,True,OldVarVals);
               END;
           {+----------------------------------------------------------}
           {| Draw line between old and new points in WriteMode(0) so
           {| the endpoints of each segment are not left out.
           {| Note: the window is blank at this point.
           {+----------------------------------------------------------}
            SetColor(ColArray[ColVar]); SetWriteMode(NormalPut);
            IF PlotTraj THEN PlotLine(xVarOld,yVarOld,xVarNew,yVarNew);
            SetColor(15); SetWriteMode(XORPut);
           {+----------------------------------------------------------}
           {| Draw new stuff if it is inside window
           {+----------------------------------------------------------}
            With GraphScales[i] DO
               InWindow[i] := (xVarNew>=Xmin) AND (xVarNew<=Xmax) AND
                              (yVarNew>=Ymin) AND (yVarNew<=Ymax);
            IF InWindow[i] THEN
               BEGIN
                  CallAnimateWindow(yAxisVar,xAxisVar,False,ForceDef.VarVals);
                  SetColor(15); SetWriteMode(XORPut);
                  Map(xVarNew,yVarNew,xScr,yScr);
                  PutImage(xScr-5,yScr-5,CircleData,XORPut);
               END;
         END;
      SetColor(15); SetWriteMode(NormalPut);
      PrintData;
      IF NOT RUNFLAG THEN ShowCursor;
    END;

   PROCEDURE GetForceParms(FirstTime:Boolean);
  {+-------------------------------------------------------------------}
  {| This procedure allows the user to modify the numerical values of
  {| the force parameters.
  {+-------------------------------------------------------------------}
   VAR
      i: Integer;
   BEGIN
      WITH GenericScreen DO WITH ForceDef DO
         BEGIN
            init;
            DefineInputPort(0.01,0.99,0.05,0.3+0.05*NumParms);
            LoadLine(ForceDesc);
            LoadLine('  Enter parameter values');
            LoadLine('');
            FOR i := 1 TO NumParms DO
               BEGIN
                  LoadLine(Concat(PadStr(VarDesc[20+i],22),
                                  PadStr(VarNames[20+i],6),
                                  '= {        } ',VarUnits[20+i]));
                  SetNumber(i,ParmVals[i]);
               END;
            LoadLine('   #F Reset Dynamical Variables');
            LoadLine('');
            LoadLine(' [  OK  ]      [Cancel]      [ Help ]');
            SetHelpFile('GenMot.HLP','GetForceParameterHelp');
            IF ResetFlag THEN SetBoolean(NumParms+1,True);
            AcceptScreen;
            IF NOT canceled THEN
               BEGIN
                  IF NOT FirstTime THEN InitOutWins(True);
                  FOR i:=1 TO NumParms DO ParmVals[i]:=GetNumber(i);
                  ReSetFlag := GetBoolean(NumParms+1);
                  IF (NOT FirstTime) THEN
                     BEGIN
                        IF ResetFlag THEN ResetVals;
                        ChangeColor(1);
                        InitOutWins(False);
                     END;
               END;
            done;
        END;
   END;

   PROCEDURE GetInitCond(FirstTime:Boolean);
  {+------------------------------------------------------------------}
  {| This procedure allows the user to specify the initial values of
  {| the dynamical variables.
  {+------------------------------------------------------------------}
   VAR
      i:Integer;
   BEGIN
      WITH GenericScreen DO WITH ForceDef DO
         BEGIN
            init;
            DefineInputPort(0.2,0.9,0.05,0.35);
            LoadLine('Initial Values');
            LoadLine(ConCat(PadStr(InitNames[1],5),
                           '{        }',InitUnits[1]));
            SetNumber(1,InitVals[1]);
            FOR i := 1 to NumDim DO
               BEGIN
                  LoadLine(ConCat(PadStr(InitNames[2*i],5),
                           '{        }',PadStr(InitUnits[2*i],8),
                           PadStr(InitNames[2*i+1],5),
                           '{        }',PadStr(InitUnits[2*i+1],8)));
                  SetNumber(2*i,InitVals[2*i]);
                  SetNumber(2*i+1,InitVals[2*i+1]);
               END;
            LoadLine('');
            LoadLine(' [  OK  ]      [Cancel]      [ Help ]');
            SetHelpFile('GenMot.HLP','GetInitialConditionsHelp');
            AcceptScreen;
            IF NOT canceled THEN
               BEGIN
                  IF NOT FirstTime THEN InitOutWins(True);
                  FOR i := 1 to 1+2*NumDim DO
                     InitVals[i] := GetNumber(i);
                  IF NOT FirstTime THEN
                     BEGIN
                        ResetVals;
                        ChangeColor(1);
                        InitOutWins(False);
                     END;
               END;
            done;
         END;
   END;

   PROCEDURE GetNumParms(FirstTime:Boolean);
  {+------------------------------------------------------------------}
  {| This procedure allows the user to specify values of various
  {| numerical parameters.
  {|    Note that dtAnim can be changed during the animation but
  {|    dtAnimInit is not, so this screen will reset dtAnim to
  {|    whatever value was specified last time on this screen.
  {+------------------------------------------------------------------}
   VAR
      i:Integer;
   BEGIN
      WITH GenericScreen DO WITH ForceDef DO
         BEGIN
            init;
            DefineInputPort(0.2,0.9,0.05,0.5);
            LoadLine('Numerical Parameters');
            LoadLine(Concat('Animation Update Interval := {        }',
                     VarUnits[1]));
            SetNumber(1,dtAnimInit);
            LoadLine(Concat('Numerical interval := {        }',
                     VarUnits[1]));
            SetNumber(2,dt);
            LoadLine(' #F Reset Dynamical Variables');
            IF ResetFlag THEN SetBoolean(3,True);
            LoadLine('Select Numerical Method');
            FOR i := 1 to NumNumMeth DO
               LoadLine(Concat('   #1 ',NumProcs[i]));
            SetRadioButton('1',NumMeth);
            LoadLine(' [  OK  ]      [Cancel]      [ Help ]');
            SetHelpFile('GenMot.HLP','GetNumericalParametersHelp');
            AcceptScreen;
            IF NOT canceled THEN
               BEGIN
                  dtAnimInit := GetNumber(1);
                  dtAnimMax := 20*dtAnimInit;
                  dtAnim := dtAnimInit;
                  dt := GetNumber(2);
                  ReSetFlag := GetBoolean(3);
                  NumMeth := GetRadioButton('1');
                  IF (NOT FirstTime) AND ResetFlag THEN
                     BEGIN
                        InitOutWins(True); ResetVals; InitOutWins(False);
                     END;
               END;
            done;
         END;
   END;

{+-------------------------------------------------------------}
{| Modify graphs
{+-------------------------------------------------------------}

   FUNCTION ClickedWindow:Integer;
  {+-----------------------------------------------------------------+}
  {| This function checks to see if a mouse click was inside a window
  {| If so, it returns the number of that window.
  {| If the mouse was clicked on the bottom row, pass this click on to
  {| the HotKey handler.
  {+-----------------------------------------------------------------+}
   VAR
      WinInside,OutWinInd:Integer;
   BEGIN
      WinInside := 0;
      WITH Event DO IF y<(GetMaxY-RowHt) THEN
         FOR OutWinInd := 1 to ForceDef.NumOutWins DO
            WITH Views[OutWinInd] DO WITH GraphScales[OutWinInd] DO
               IF (x>=Vx1) AND (x<=Vx2) AND (y>=Vy1) AND (y<=Vy2)
                  THEN WinInside := OutWinInd;
      ClickedWindow := WinInside;
   END;

   PROCEDURE SelectWindow(VAR Index: Integer; VAR GraphName:String);
  {+-----------------------------------------------------------------+}
  {| This procedure is used to select a window.
  {| It is used when modifying the graph contents of the window
  {| and when modifying the window itself.
  {+-----------------------------------------------------------------+}
   CONST
      MessageText = 'Select Window to Modify:';
   VAR
      KeyHit:Byte;
      DoneFlag:Boolean;
      WinInside:Integer;
   BEGIN
      IF (Index<1) OR (Index>ForceDef.NumOutWins) THEN Index := 1;
      SelWinKeys.Display;
      SelectMuppetPort;
      DoneFlag := False;
      GetGraphName(Index,GraphName);
      HiliteWin(Index);
      Message(Concat(MessageText,GraphName,'             ESC when done'));
      KeyHit := 0;
      REPEAT
         CheckForEvents;
         IF Event.MouseClicked OR Event.KeyPressed THEN
            BEGIN
               HiliteWin(Index);
               WinInside := ClickedWindow;
               IF WinInside<>0
                  THEN BEGIN Index := WinInside; DoneFlag := True; END
                  ELSE IF SelWinKeys.Pressed(KeyHit) THEN WITH ForceDef DO
                     CASE KeyHit OF
                        1: DoneFlag := True;                        {ESC}
                        2: Index := 1 + Index Mod NumOutWins;       {RIGHT}
                        3: Index := NumOutWins -
                            (NumOutWins-Index+1) MOD NumOutwins;    {LEFT}
                        4: DoneFlag := True;                        {ENTER}
                     END; {CASE}
               GetGraphName(Index,GraphName);
               Message(Concat(MessageText,GraphName,'             ESC to QUIT'));
               IF KeyHit<>1 THEN HiliteWin(Index);
               ClearMKBuffers;
            END;
      UNTIL DoneFlag;
      ClearMKBuffers;
      CheckForEvents;
      IF KeyHit=1 THEN Index := 0;
      MainMenu.Display;
      HotKeys.Display;
   END;

   PROCEDURE ChangeScales(GraphInd,ScaleChoice:Integer);
  {+-----------------------------------------------------------------+}
  {| This procedure allows the user to change the graph scales by
  {| ScaleChoice
  {|     2       specifying values
  {|     3       Zoom In
  {|     4       Zoom Out
  {+-----------------------------------------------------------------+}
   VAR
      VarName:String[5];
      xVar,yVar,ScreenPos,button: Integer;
      LocalGenericScreen:TInputScreen;
      temp,dist,x,y:Real;
      Inside:Boolean;
   BEGIN
      xVar := ForceDef.OutWin[GraphInd].HorizVar;
      yVar := ForceDef.OutWin[GraphInd].VertVar;
      IF (xVar=0) AND (yVar=0)
      THEN Announce('No Variables')
      ELSE
      CASE ScaleChoice OF
      2: WITH LocalGenericScreen DO WITH ForceDef DO
         BEGIN
            init;
            DefineInputPort(0.2,0.7,0.05,0.4);
            ScreenPos := 1;
            VarName := VarNames[xVar];
            LoadLine(Concat(' HORIZONTAL VARIABLE: ',VarName));
            IF xVar<>0 THEN
               BEGIN
                  LoadLine(Concat(' Minimum: {        }  ',VarUnits[xVar]));
                  LoadLine(Concat(' Maximum: {        }  ',VarUnits[xVar]));
                  SetNumber(ScreenPos,OutWin[GraphInd].HMin);
                  SetNumber(ScreenPos+1,OutWin[GraphInd].HMax);
                  ScreenPos := ScreenPos+2;
               END;
            VarName := VarNames[yVar];
            LoadLine(Concat(' VERTICAL VARIABLE: ',VarName));
            IF yVar<>0 THEN
               BEGIN
                  LoadLine(Concat(' Minimum: {        }  ',VarUnits[yVar]));
                  LoadLine(Concat(' Maximum: {        }  ',VarUnits[yVar]));
                  SetNumber(ScreenPos,OutWin[Graphind].VMin);
                  SetNumber(ScreenPos+1,OutWin[Graphind].VMax);
               END;
            LoadLine('');
            LoadLine('[  OK  ]      [Cancel]      [ Help ]');
            SetHelpFile('GenMot.HLP','ChangeScalesHelp');
            AcceptScreen;
            ScreenPos := 1;
            IF NOT canceled THEN
               BEGIN
                  IF xVar<>0 THEN WITH OutWin[GraphInd] DO
                     BEGIN
                        HMin := GetNumber(1);
                        HMax := GetNumber(2);
                        IF HMin>HMax THEN
                           BEGIN
                              temp := HMIn; HMin := HMax; HMax := temp;
                           END;
                        ScreenPos := 3;
                        IF xVar=1 THEN dtAnimMax := 0.02*ABS(HMax-HMin);
                     END;
                  IF YVar<>0 THEN WITH OutWin[GraphInd] DO
                     BEGIN
                        VMin := GetNumber(ScreenPos);
                        VMax := GetNumber(ScreenPos + 1);
                        IF VMin>VMax THEN
                           BEGIN
                              temp := VMin; VMin := VMax; VMax := temp;
                           END;
                     END;
               END;
            done;
         END;
      3: WITH ForceDef DO WITH OutWin[GraphInd] DO
         BEGIN
            Message('Click mouse at point to zoom in to');
            REPEAT MouseGlobalPosn(xVar,yVar,button) UNTIL button<>0;
            MousePosn(X, Y, GraphInd,GraphInd,Inside);
            HMax := (HMax+X)/2;
            HMin := (HMin+X)/2;
            VMax := (VMax+Y)/2;
            VMin := (VMin+Y)/2;
            ClearMKbuffers;
         END;
      4: WITH ForceDef DO WITH OutWin[GraphInd] DO
         BEGIN
            IF xVar<>0 THEN
               BEGIN
                  dist := HMax - HMin;
                  HMax := HMax + dist;
                  HMin := HMin - dist;
               END;
            IF YVar<>0 THEN
               BEGIN
                  dist := VMax - VMin;
                  VMax := VMax + dist;
                  VMin := VMin - dist;
               END;
         END;
      END; {CASE}
   END;

   PROCEDURE GraphContentChange(VAR GraphInd:Integer; VAR GraphName:String);
  {+----------------------------------------------------------------+}
  {| This routine allows the user to modify the variables describing
  {| the window specified by GraphInd.
  {+----------------------------------------------------------------+}
   VAR
      TxtStr:String;
      LocalESC:Boolean;
      ScaleChoice:Integer;
   BEGIN
      IF GraphInd<>0 THEN WITH GenericScreen DO WITH ForceDef DO
                          WITH OutWin[GraphInd] DO
         BEGIN
            init;
            DefineInputPort(0.1,0.9,0.05,0.5);
            LoadLine(GraphName);
            LoadLine(Concat(' Graph Title: "',PadStr(GraphTitle,40),'"'));
            LoadLine('#T Plot Trajectory     #T Display Axes     #F Transparent');
            LoadLine('     #F Change Vertical Variable');
            LoadLine('     #F Change Horizontal Variable');
            LoadLine('Graph Scales:');
            LoadLine('#1 No change   #1 Specify   #1 Zoom In   #1 Zoom Out');
            IF NOT PlotTraj THEN SetBoolean(2,False);
            IF NOT DisplayAxes THEN SetBoolean(3,False);
            IF Transparent THEN SetBoolean(4,True);
            LoadLine('             [  OK  ]      [Cancel]      [ Help ]');
            SetHelpFile('GenMot.HLP','ChangeGraphContentsHelp');
            AcceptScreen;
            IF NOT canceled THEN
               BEGIN
                  GraphTitle := GetString(1);
                  PlotTraj := GetBoolean(2);
                  DisplayAxes := GetBoolean(3);
                  Transparent := GetBoolean(4);
                  IF GetBoolean(5) THEN
                     GetPlotVar('Vertical',VertVar,LocalESC);
                  IF GetBoolean(6) THEN
                     GetPlotVar('Horizontal',HorizVar,LocalESC);
                  IF HorizVar=0 THEN BEGIN HMin := -10; HMax := 10; END;
                  IF VertVar=0  THEN BEGIN VMin := -10; VMax := 10; END;
                  GetGraphName(GraphInd,GraphName);
                  ScaleChoice := GetRadioButton('1');
                  IF ScaleChoice<>1 THEN ChangeScales(GraphInd,ScaleChoice);
                  OpenPlot(True);
               END
               ELSE HiliteWin(GraphInd);
            done;
         END;
   END;

   PROCEDURE WindowPlaceChange(PosSiz:Integer; GraphIndex:Integer;
                             VAR RedrawFlag:Boolean);
  {+-------------------------------------------------------------------}
  {| This procedure allows the user to change the position or size of
  {| the graph indicated by GraphIndex.  If PosSiz=1, Position is
  {| changed.  If PosSiz=2, Size is changed.
  {+-------------------------------------------------------------------}
   VAR
      x,y,dx,dy:Real;
      xScr,yScr,xScrOld,yScrOld: Integer;
      dxScr,dyScr,dxScrOld,dyScrOld: Integer;
      KeyHit:Byte;
   BEGIN
      WITH ForceDef.OutWin[GraphIndex] DO
         BEGIN
            x := LeftEdge;  y := TopEdge;
            dx := Width;    dy := Height;
         END;
      xScr := Round(x*GetMaxX);  xScrOld := xScr;
      yScr := Round(y*GetMaxY);  yScrOld := yScr;
      dxScr := Round(dx*GetMaxX); dxScrOld := dxScr;
      dyScr := Round(dy*GetMaxY); dyScrOld := dyScr;
      CASE PosSiz OF
         1: Message('Use Arrow Keys to change position');
         2: Message('Use Arrow Keys to change size');
      END; {CASE}
      ArrowKeys.Display;
      SetWriteMode(XORPut);
      Rectangle(xScr,yScr,xScr+dxScr,yScr+dyScr);
      REPEAT
         CheckForEvents;
         IF ArrowKeys.Pressed(KeyHit) THEN
            BEGIN
               CASE PosSiz OF
                  1: CASE KeyHit OF    {Change Position}
                     2:  BEGIN y:=y-0.01; IF y<0.0 THEN y:=0.0; END;
                     3:  BEGIN y:=y+0.01; IF y>1-dy THEN y:=1-dy; END;
                     4:  BEGIN x:=x-0.01; IF x<0.0 THEN x:=0.0; END;
                     5:  BEGIN x:=x+0.01; IF x>1-dx THEN x:=1-dx; END;
                     6: RedrawFlag := True;
                     END; {CASE}
                  2:  CASE KeyHit OF   {Change Size}
                      2: BEGIN dy:=dy-0.01; IF dy<0.05 THEN dy:=0.05; END;
                      3: BEGIN dy:=dy+0.01; IF y+dy>1 THEN dy:=1-y; END;
                      4: BEGIN dx:=dx-0.01; IF dx<0.05 THEN dx:=0.05; END;
                      5: BEGIN dx:=dx+0.01; IF x+dx>1 THEN dx:=1-x; END;
                      6: RedrawFlag := True;
                      END; {CASE}
                  END; {CASE}
               dxScr := Round(dx*GetMaxX);
               dyScr := Round(dy*GetMaxY);
               xScr := Round(x*GetMaxX);
               yScr := Round(y*GetMaxY);
               Rectangle(xScrOld,yScrOld,xScrOld+dxScrOld,yScrOld+dyScrOld);
               Rectangle(xScr,yScr,xScr+dxScr,yScr+dyScr);
               xScrOld := xScr;  dxScrOld := dxScr;
               yScrOld := yScr;  dyScrOld := dyScr;
            END;
      UNTIL (KeyHit=1) OR (KeyHit=6);
      IF RedrawFlag THEN WITH ForceDef.OutWin[GraphIndex] DO
         BEGIN
            LeftEdge := x;  Width := dx;
            TopEdge := y;   Height := dy;
         END;
      Rectangle(xScr,yScr,xScr+dxScr,yScr+dyScr);
      SetWriteMode(NormalPut);
   END;

   PROCEDURE WindowAdd(VAR GraphIndex:Integer;
             VAR GraphName:String; VAR RedrawFlag:Boolean);
   BEGIN
      IF ForceDef.NumOutWins=MaxGraphs
         THEN Announce(Concat('Only ',Num2Str(MaxGraphs,2),
                        ' Graphs allowed.      Hit any key to continue'))
         ELSE WITH ForceDef DO
            BEGIN
               NumOutWins := NumOutWins + 1;
               OutWin[NumOutWins] := OutWin[GraphIndex];
               GraphIndex := NumOutWins;
               WindowPlaceChange(2,GraphIndex,RedrawFlag);
               OpenPlot(False);  HiliteWin(GraphIndex);
               WindowPlaceChange(1,GraphIndex,RedrawFlag);
               RedrawFlag := True;
            END;
   END;

   PROCEDURE WindowDelete(VAR GraphIndex:Integer; GraphName:String;
             VAR RedrawFlag:Boolean);
   VAR
      z:Char;
      Index:Integer;
   BEGIN
      IF ForceDef.NumOutWins=1
         THEN Announce('Cannot Delete last Window - Hit any key to continue')
         ELSE WITH ForceDef DO
            BEGIN
               Beep;
               Message(Concat('Delete ',GraphName,'! Are you Sure? (Y/N)'));
               Repeat until Keypressed;
               z := ReadKey;
               IF (z='Y') OR (z='y') THEN
                  BEGIN
                     NumOutWins := NumOutWins - 1;
                     FOR Index := GraphIndex TO NumOutWins DO
                           OutWin[Index] := OutWin[Index+1];
                     GraphIndex := 0;
                     RedrawFlag := True;
                     ClearMKBuffers;
                     CheckForEvents;
                  END;
            END;
   END;

   PROCEDURE ChangeWindows;
   VAR
      BarChoice,GraphIndex:Integer;
      GraphName,MessageText:String;
      RedrawFlag:Boolean;
      Image:TImage;
      FileBar:TBarMenu;
   BEGIN
      SelectWindow(GraphIndex,GraphName);
      IF GraphIndex<>0 THEN
         BEGIN
            FileBar.Init;
            FileBar.Row[1] := 'Change Location';
            FileBar.Row[2] := 'Change Size';
            FileBar.Row[3] := 'Copy to New Window';
            FileBar.Row[4] := 'Delete Window';
            FileBar.Row[5] := 'Change Contents';
            FileBar.Row[6] := 'Done';
            BarChoice:=6;
            REPEAT
               MessageText := 'Choose function        Selected Graph: ';
               Message(MessageText+GraphName);
               REPEAT
                  RedrawFlag := False;
                  Image.Get(0,0,Round(0.4*GetMaxX),Round(0.5*GetMaxY));
                  FileBar.Draw(0.1,0.9,6,BarChoice);
                  BarChoice := FileBar.Chosen;
                  Image.Put(0,0);
                  CASE BarChoice OF
                     1: WindowPlaceChange(1,GraphIndex,RedrawFlag);
                     2: WindowPlaceChange(2,GraphIndex,RedrawFlag);
                     3: WindowAdd(GraphIndex,GraphName,RedrawFlag);
                     4: WindowDelete(GraphIndex,GraphName,RedrawFlag);
                     5: GraphContentChange(GraphIndex,GraphName);
                  END; {CASE}
                  IF RedrawFlag THEN
                     BEGIN
                        OpenPlot(False);
                        HiliteWin(GraphIndex);
                     END;
                  Message(MessageText+GraphName);
               UNTIL (BarChoice=6) or (GraphIndex=0);
               HiliteWin(GraphIndex);
               SelectWindow(GraphIndex,GraphName);
            UNTIL GraphIndex=0;
            OpenPlot(True);
         END;
   END;

{+---------------------------------------------------------------------}
{| Configuration procedures
{+---------------------------------------------------------------------}
   PROCEDURE GetUserHelp;
  {+-------------------------------------------------------------------}
  {| This procedure uses MUPPET InputScreen utilities to allow
  {| the user to modify the help screen.  Each line of the screen is
  {| taken as one text variable, so that the entire screen can be
  {| modified.  Only 22 lines can be displayed using Accept, so
  {| the lines available for this help screen are also limited.
  {+-------------------------------------------------------------------}
   VAR
      i,j:Integer;
      ch:String[1];
      HelpLine:String;
   BEGIN
      WITH GenericScreen DO WITH ForceDef DO
         BEGIN
            init;
            DefineInputPort(0.33,0.99,0,0.99);
            Message('Modify User-Defined Help');
            HelpLine := Concat('"',PadStr('',50),'"');
            FOR i := 1 TO 22 DO
               BEGIN
                  LoadLine(HelpLine);
                  SetString(i,UserHelpScreen[i]);
               END;
            LoadLine('    [  OK  ]      [Cancel]      [ Help ]');
            SetHelpFile('GenMot.HLP','GetUserDefinedHelpHelp');
            AcceptScreen;
            IF NOT canceled THEN
               FOR i := 1 to 22 DO UserHelpScreen[i] := GetString(i);;
            done;
         END;
      MainMenu.Display;
   END;

   PROCEDURE SaveData;
   VAR
      i,n,nD,StartPnt:Integer;
      TxtStr:String;
      f:Text;
      ESCFlag:Boolean;
   BEGIN
      IF NumPnts=0
         THEN Announce('No data to be saved')
         ELSE WITH GenericScreen DO WITH ForceDef DO
         BEGIN
            init;
            DefineInputPort(0.2,0.9,0.05,0.35);
            LoadLine('Select name for DataFile:  (.DAT is assumed)');
            LoadLine(Concat('"                                         "'));
            LoadLine('');
            LoadLine('             [  OK  ]      [Cancel]      [ Help ]');
            SetHelpFile('GenMot.HLP','SaveDataHelp');
            AcceptScreen;
            ESCFlag := canceled;
            TxtStr := GetString(1);
            done;
         END;
      IF NOT ESCFlag THEN WITH ForceDef DO
         BEGIN
            TxtStr := Concat(TxtStr,'.DAT');
            Assign(f,TxtStr);
            Rewrite(f);
            WRITELN(f,'Data file for Force defined by:');
            WRITELN(f,ForceTitle);
           {+----------------------------------------------------------}
           {| Write Numerical info
           {+----------------------------------------------------------}
            WRITELN(f,'Numerical Info');
            WRITELN(f,NumMeth,' ',Num2Str(dtAnim,10),' ',Num2Str(dt,10));
           {+----------------------------------------------------------}
           {| Write parameter values, init values and Min,Max values
           {| Note that names and units are not used when the file is
           {| read in;  these are for identification purposes only.
           {+----------------------------------------------------------}
            WRITELN(f,'User defined parameters:');
            FOR i := 1 to 10 DO
               WRITELN(f,PadStr(VarNames[20+i],12),
                         FNum2Str(ParmVals[i],11),' ',VarUnits[20+i]);
            WRITELN(f,'Initial Values:');
            FOR i := 1 to 7 DO
               WRITELN(f,PadStr(InitNames[i],10),
                         FNum2Str(InitVals[i],11),' ',InitUnits[i]);
           {+----------------------------------------------------------}
           {| Write dynamical variable names and units
           {+----------------------------------------------------------}
            WRITELN(f,'Dynamical Variables:');
            FOR i := 1 to NumDynVars DO WRITE(f,PadStr(VarNames[i],12));
            WRITELN(f);
            FOR i := 1 to NumDynVars DO WRITE(f,PadStr(VarUnits[i],12));
            WRITELN(f);
           {+----------------------------------------------------------}
           {| Write dynamical variable data.
           {+----------------------------------------------------------}
            IF NumPnts<=MaxData
               THEN StartPnt := 0
               ELSE BEGIN StartPnt := NumPnts MOD MaxData;
                          NumPnts:=MaxData; END;
            FOR n := 0 to NumPnts-1 DO
               BEGIN
                  nD := 1 + ((StartPnt+n) MOD MaxData);
                  FOR i := 1 to NumDynVars DO
                     WRITE(f,FNum2Str(DataStore[i,nD],11),' ');
                  WRITELN(f);
               END;
            CLOSE(f);
         END;
   END;

   PROCEDURE SaveConfig;
  {+----------------------------------------------------------------+}
  {| This procedure writes the configuration info to the output file
  {+----------------------------------------------------------------+}
   VAR
      i,OutWinInd,PLotTrajInt,DisplayAxesInt,TransparentInt: Integer;
      TxtStr:String;
      f:Text;
      ESCFlag:Boolean;
   BEGIN
      TxtStr := Concat(ForceDef.ConfigFileName,'.');
      TxtStr := COPY(TxtStr,1,POS('.',TxtStr)-1);
      WITH GenericScreen DO WITH ForceDef DO
         BEGIN
            init;
            DefineInputPort(0.2,0.9,0.05,0.35);
            LoadLine('Select name for Configuration File (.PRM us assumed)');
            LoadLine(Concat('"',PadStr(TxtStr,50),'"'));
            LoadLine('   Assign this name to variable CONFIGFILENAME ');
            LoadLine('   in procedure DefForceParms to make this');
            LoadLine('   file the default at start-up');
            LoadLine('');
            LoadLine('             [  OK  ]      [Cancel]      [ Help ]');
            SetHelpFile('GenMot.HLP','SaveParmsHelp');
            AcceptScreen;
            ESCFlag := canceled;
            TxtStr := GetString(1);
            done;
         END;
      IF NOT ESCFlag THEN WITH ForceDef DO
         BEGIN
            ConfigFileName := Concat(TxtStr,'.PRM');
            Assign(f,ConfigFileName);
            Rewrite(f);
           {+----------------------------------------------------------}
           {| Describe Force file used to generate this configuration
           {| Note that this info is not used when the file is read in;
           {| it is for identification purposes only.
           {+----------------------------------------------------------}
            WRITELN(f,'Configuration file for Force defined by:');
            WRITELN(f,ForceTitle);
            WRITELN(f,'UserHelpScreen:');
            FOR i := 1 to 23 DO WRITELN(f,UserHelpScreen[i]);
           {+----------------------------------------------------------}
           {| Write Numerical info
           {+----------------------------------------------------------}
            WRITELN(f,'Numerical Info');
            WRITELN(f,NumMeth,' ',dtAnim,' ',dt);
           {+----------------------------------------------------------}
           {| Write parameter values, init values and Min,Max values
           {| Note that names and units are not used when the file is
           {| read in;  these are for identification purposes only.
           {+----------------------------------------------------------}
            WRITELN(f,'User defined parameters:');
            FOR i := 1 to 10 DO
               WRITELN(f,PadStr(VarNames[20+i],10),ParmVals[i],
                       ' ',VarUnits[20+i]);
            WRITELN(f,'Initial Values:');
            FOR i := 1 to 7 DO
               WRITELN(f,PadStr(InitNames[i],10),InitVals[i],
                         ' ',InitUnits[i]);
           {+----------------------------------------------------------}
           {| Write window definition data.
           {+----------------------------------------------------------}
            WRITELN(f,DisplayVars[1],' ',DisplayVars[2],' ',DisplayVars[3],
                    '  Variables to display numerically');
            WRITELN(f,NumStr(NumOutWins,10,0),' Number of Windows');
            FOR OutWinInd := 1 to NumOutWins DO WITH OutWin[OutWinInd] DO
               BEGIN
                  WRITELN(f,GraphTitle);
                  WRITELN(f,FNum2Str(LeftEdge,10),FNum2Str(TopEdge,10),
                            FNum2Str(Width,10),FNum2Str(Height,10),
                            FNum2Str(VertVar,10),fNum2Str(HorizVar,10));
                  WRITELN(f,FNum2Str(HMin,10),FNum2Str(HMax,10),
                            FNum2Str(VMin,10),FNum2Str(VMax,10));
                  IF PlotTraj THEN PlotTrajInt:=1
                              ELSE PlotTrajInt:=0;
                  IF DisplayAxes THEN DisplayAxesInt:=1
                                 ELSE DisplayAxesInt:=0;
                  IF Transparent THEN TransparentInt:=1
                                 ELSE TransparentInt:=0;
                  WRITELN(f,PlotTrajInt,' ',DisplayAxesInt,' ',TransparentInt);
               END;
            CLOSE(f);
         END;
   END;

   PROCEDURE LoadConfig(FileName:String; InitFlag:Boolean);
  {+----------------------------------------------------------------+}
  {| This procedure reads data from the configuration file
  {+----------------------------------------------------------------+}
   VAR
      f:Text;
      i,OutWinInd,PlotTrajInt,DisplayAxesInt,TransparentInt: Integer;
      TxtStr:String;
      NameStr:String[10];
      ESCFlag,YesFlag:Boolean;
   BEGIN
      IF FileName<>'' THEN
         BEGIN
            Assign(f,FileName);
            Reset(f);
            READLN(f); READLN(f,TxtStr);
            ESCFlag := FALSE;
            IF NOT InitFlag THEN WITH GenericScreen DO
               BEGIN
                  init;
                  DefineInputPort(0.1,0.95,0.05,0.3);
                  LoadLine('This configuration file was defined for the following force:');
                  LoadLine(TxtStr);
                  LoadLine('           #T - OK ?');
                  LoadLine('');
                  LoadLine('             [  OK  ]      [Cancel]      [ Help ]');
                  SetHelpFile('GenMot.HLP','LoadParmsHelp');
                  AcceptScreen;
                  ESCFlag := canceled;
                  YesFlag := GetBoolean(1);
                  done;
               END;
            IF NOT ESCFlag THEN WITH ForceDef DO
               BEGIN
                  ConfigFileName := FileName;
                  READLN(f);
                  FOR i := 1 to 23 DO READLN(f,UserHelpScreen[i]);
                  READLN(f);
                  READLN(f,NumMeth,dtAnimInit,dt);
                  READLN(f);   {Read Parameter Values}
                  FOR i := 1 to 10 DO READLN(f,NameStr,ParmVals[i]);
                  READLN(f);   {Read Initial Values}
                  FOR i :=  1 to 7 DO READLN(f,NameStr,InitVals[i]);
                  READLN(f,DisplayVars[1],DisplayVars[2],DisplayVars[3]);
                  READLN(f,NumOutWins);
                  FOR OutWinInd := 1 to NumOutWins DO WITH OutWin[OutWinInd] DO
                     BEGIN
                        READLN(f,GraphTitle);
                        READLN(f,LeftEdge,TopEdge,Width,Height,
                                 VertVar,HorizVar);
                        READLN(f,HMin,HMax,VMin,VMax);
                        READLN(f,PlotTrajInt,DisplayAxesInt,TransparentInt);
                        PlotTraj := (PlotTrajInt=1);
                        DisplayAxes := (DisplayAxesInt=1);
                        Transparent := (TransparentInt=1);
                     END;
                  ResetVals;
                  DefineViewPort(1,0.05,0.95,0.8,1.0);
                  OpenViewPort(1);
                  SetColor(Yellow);
                  Print(5,1,'This Program has been Generated using the CUPS GenMot Utilities');
                  Print(17,2,'Author: Randy Jones, Loyola College in Maryland');
                  Print(30,3,'Copyright (c) 1994');
                  SetColor(whitte);
                  Help(UserHelpScreen);
                  OpenPlot(True);
               END;
            CLOSE(f);
         END;
   END;

   PROCEDURE LoadGeneric(NumGraphs:Integer);
   VAR
      OutWinInd:Integer;
   BEGIN
      ForceDef.NumOutWins := NumGraphs;
      FOR OutWinInd := 1 to NumGraphs DO WITH ForceDef.OutWin[OutWinInd] DO
         BEGIN
            PlotTraj := True;
            DisplayAxes := True;
            Transparent := False;
            HorizVar := 1;
            VertVar := OutWinInd+1;
            HMin := 0.0; HMax := 10.0;
            VMin := -10.0; VMax := 10.0;
            GraphTitle := '';
            IF NumGraphs=2 THEN Width := 0.8 ELSE Width := 0.35;
            Height := 0.34;
            LeftEdge := 0.1;
            TopEdge := 0.15 + 0.4*((OutWinInd-1)mod 2);
         END;
      IF NumGraphs=4 THEN WITH ForceDef DO
         BEGIN
            WITH OutWin[3] DO LeftEdge := 0.6;
            WITH OutWin[4] DO BEGIN LeftEdge := 0.6; VertVar := 11; END;
         END;
   END;

    PROCEDURE LoadStartUpFile;
   {+------------------------------------------------------------------}
   {| Check that .PRM file exists and load it.  If not, run through
   {| parameter input screens to get start-up values.
   {+------------------------------------------------------------------}
   VAR
       f:Text;
   BEGIN
       Assign(f,ForceDef.ConfigFileName);
       {$i-} Reset(f); Close(f); {$i+}
       IF ioResult=0
          THEN LoadConfig(ForceDef.ConfigFileName,True)
          ELSE
             BEGIN
                  DefineViewPort(1,0.05,0.95,0.65,0.8);
                  OpenViewPort(1);
                  SetColor(Yellow);
                  Print(5,1,'This Program has been Generated using the CUPS GenMot Utilities');
                  Print(17,2,'Author: Randy Jones, Loyola College in Maryland');
                  Print(30,3,'Copyright (c) 1994');
                  DefineViewPort(1,0.15,0.85,0.51,0.6);
                  OpenViewPort(1);
                  SetColor(whitte);
                  Print(5,1,'The .PRM file for this program was not found');
                  Print(5,2,'  Please enter parameters requested below');
                PrintForceTitle;
                LoadGeneric(2);
                ForceDef.ParmVals[1] := 1.0;  {often the mass}
                GetForceParms(True);
                GetInitCond(True);
                GetNumParms(True);
                ResetVals;
                SaveConfig;
                OpenPlot(True);
             END;
   END;

{+-------------------------------------------------------------------}
{| Initialization Routines
{+-------------------------------------------------------------------}
   PROCEDURE InitVars;
  {+----------------------------------------------------------------}
  {| This procedure initializes all variables before calling user-
  {| defined procedures.
  {+----------------------------------------------------------------}
   VAR
      i:Integer;
   BEGIN
      ColVar := 3;
      ResetFlag := True;
      WITH ForceDef DO
         BEGIN
            NumDim := 0;
            NumParms := 0;
            NumDynFuns := 0;
            FOR i := 0 to 30 DO VarDesc[i] := '';
            VarNames[0] := 'none';
            VarUnits[0] := '';
         END;
      GenTimeVar('t','s');
      GenDynVar('x','vx','ax','m');
      GenDynVar('y','vy','ay','m');
      GenDynVar('z','vz','az','m');
      FOR i := 1 to 10 DO
         GenForceParm(ConCat('Parm',NumStr(i,1,0)),'None','None');
      FOR i := 1 to 10 DO
         GenDynFun(Concat('Fun',NumStr(i,1,0)),'None','None');
      FOR i := 1 to 25 DO UserHelpScreen[i] := ' ';
      WITH ForceDef DO
         BEGIN
            VarVals[0] := 0;
            FOR i := 1 to 10 DO
               BEGIN VarVals[i] := 0; ParmVals[i] := 0; END;
            FOR i := 1 to 7 DO InitVals[i] := 0;
            ConfigFileName := 'NewConf';
            ForceTitle := '';
            ForceDesc := '';
            NumDim := 0;
            NumDynVars := 0;
            NumParms := 0;
            NumDynFuns := 0;
            GenTimeVarCalled := False;
            dt := 0.01; dtAnimInit := 0.1; dtAnimMax := 20*dtAnimInit;
            DisplayVars[1] := 1;
            DisplayVars[2] := 2;
            DisplayVars[3] := 3;
            NumMeth := 3;
         END;
   END;

   PROCEDURE InitVars2;
  {+----------------------------------------------------------------}
  {| This procedure is called after calling the user defined
  {| procedure DefForceParms in order to initialize a few variables
  {| after the system is defined.  Additional variables will be
  {| defined when the start-up configuration is loaded.
  {+----------------------------------------------------------------}
   VAR
      i:Integer;
   BEGIN
      WITH ForceDef DO
         BEGIN
            IF NumDim=0 THEN
               BEGIN
                  Announce('ERROR in PROCEDURE DefForceParms - No dynamical variables defined');
                  NumDim := 1;  NumDynVars := 4;
               END;
            xV.Init(2*NumDim);
            xVP.Init(2*NumDim);
         END;
   END;

   PROCEDURE InitGenMot;
   VAR
      i:Integer;
   BEGIN
      FOR i := 0 to 5 DO ColArray[i] := 15-i;
      GetCircle(CircleData);
      SetUpMenu;
      SetUpHotKeys;
      NumericalInit;
      InitVars;
      DefForceParms;
      InitVars2;
      LoadStartUpFile;
      ReSetFlag := True;
      DoneFlag := False;
      RunFlag := False;
      dtShift := 0.0;
   END;

{+-------------------------------------------------------------------}
{| Main Loop Routines
{+-------------------------------------------------------------------}
   PROCEDURE HandleMenu;
   VAR
      GraphName,FileName:String;
      GraphInd:Integer;
   BEGIN
      IF MainMenu.Chosen THEN WITH MainMenu DO
         CASE colChosen OF
          1: CASE rowChosen OF
              1: AboutCUPS;
              2: ShowHelp('GenMot.HLP','AboutProgram');
              3: Configuration;
              4: LoadConfig(OpenFile('*.PRM'),False);
              5: SaveConfig;
              6: SaveData;
              7: DoneFlag := True;
             END {CASE};
          2: CASE rowChosen OF
              1: BEGIN
                   InitOutWins(True);
                   ResetVals; ColVar := 3;
                   InitOutWins(False);
                 END;
              2: BEGIN ResetVals; OpenPlot(True); END;
              3: OpenPlot(True);
              4: ChangeColor(1);
              5: ShowHelp('GenMot.HLP','GraphsHelp');
             END {CASE};
          3: CASE rowChosen OF
              1: GetForceParms(False);
              2: GetInitCond(False);
              3: GetNumParms(False);
              4: ShowHelp('GenMot.HLP','ParametersHelp');
             END {CASE};
          4: CASE rowChosen OF
              1: ShowHelp('GenMot.HLP','GeneralHelp');
              2: Help(UserHelpScreen);
             END {CASE};
          5: CASE rowChosen OF
              1: BEGIN
                    SelectWindow(GraphInd,GraphName);
                    GraphContentChange(GraphInd,GraphName);
                 END;
              2: GetDisplayVars;
              3: ChangeWindows;
              4: GetUserHelp;
              5: BEGIN LoadGeneric(2); OpenPlot(True); END;
              6: BEGIN LoadGeneric(4); OpenPlot(True); END;
              7: ShowHelp('GenMot.HLP','WindowsHelp');
             END {CASE};
         END {CASE};
      ClearMkBuffers;
      CheckForEvents;
      RunFlag := False;
   END;

    Procedure HandleHotKey(KeyHit:Byte);
   {+----------------------------------------------------------------}
   {| This procedure handles hot keys for animation
   {+----------------------------------------------------------------}
   VAR
      t:Real;
   BEGIN
     {+----------------------------------------------------------------}
     {| Determine the appropriate action for the hotkey choice
     {+----------------------------------------------------------------}
      CASE keyHit OF
         1: BEGIN                       {F2 - RUN}
               RunFlag:=True;
              {Note: If cursor is left on, this runs half as fast - why?}
               Hidecursor;
            END;
         2: BEGIN                    {F3 - STEP}
               RunFlag:=False;
               ShowCursor;
               GenMotion;
            END;
         3: dtAnim := -dtAnim;                       {F4 - REVERSE}
         4: ChangeSpeed(dtAnim,-1);                  {F5 - SLOW DOWN}
         5: IF ABS(dtAnim)<dtAnimMax THEN            {F6 - SPEED UP}
            BEGIN
               ChangeSpeed(dtAnim,1);
               t := ForceDef.VarVals[1];
               dtShift := t+dtAnim - Round((t+dtAnim)/dtAnim)*dtAnim;
            END;
         6: HandleMenu                              {F10 - MENU}
      END {CASE};
      HideCursor;
      PrintData;
      ShowCursor;
      ClearMkBuffers;
      CheckForEvents;
   END;

   PROCEDURE HandleMouse;
  {+-------------------------------------------------------------------}
  {| During Animation:
  {|    Mouse is hidden to improve animation speed.
  {|    Mouse click will pause animation.
  {| When animation is paused:
  {|    Click at bottom of screen is ignored (passed on to HotKeys)
  {|    Select a window for graph modification
  {|    Select the numerical window to modify displayed values
  {|    Select MainMenu
  {+-------------------------------------------------------------------}
   VAR
      OutWinInd,WinInside:Integer;
      GraphName:String;
   BEGIN
      IF RunFlag
      THEN
         BEGIN
            RunFlag := False;
            ShowCursor;
            ClearMkBuffers;
            CheckForEvents;   {WHY IS THIS NECESSARY? - CLICK IS NOT CLEARED WITHOUT IT}
         END
      ELSE IF Event.y<(GetMaxY-RowHt) THEN
         BEGIN
            IF Event.y<=RowHt THEN HandleMenu
               ELSE IF Event.y<=3*RowHt THEN GetDisplayVars
               ELSE
                  BEGIN
                     WinInside := ClickedWindow;
                     IF WinInside<>0 THEN
                        BEGIN
                           HiliteWin(WinInside);
                           GetGraphName(WinInside,GraphName);
                           GraphContentChange(WinInside,GraphName);
                        END;
                  END;
            END;
      ShowCursor;
   END;

{+-------------------------------------------------------------------}
{| Main Routine
{+-------------------------------------------------------------------}
   PROCEDURE RunMotSim(DefForceParmsProc:DynProc1;
                       CalcAccelProc:DynProc2;
                       CalcDynFunProc:DynFun1;
                       InitWindowProc:DynProc3;
                       AnimateWindowProc:DynProc3);
   BEGIN
      CUPSinit;
          DefForceParms := DefForceParmsProc;
          CalcAccel := CalcAccelProc;
          CalcDynFun := CalcDynFunProc;
          InitWindow := InitWindowProc;
          AnimateWindow := AnimateWindowProc;
      InitGenMot;
      ShowCursor;
      REPEAT
         CheckForEvents;
         IF event.mouseClicked THEN HandleMouse;
         IF HotKeys.Pressed(KeyHit) THEN HandleHotKey(KeyHit);
         IF Event.ExtendedKey AND (Event.ReadKey=';') THEN
            ShowHelp('GenMot.HLP','GeneralHelp');
         IF RunFlag THEN GenMotion;
         IF Event.KeyPressed AND NOT Event.ExtendedKey THEN
            BEGIN
               RunFlag := False;
               ShowCursor;
            END;
      UNTIL DoneFlag;
      CUPSdone;
   END;



END.
