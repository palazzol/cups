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

PROGRAM Rotate;
{+-------------------------------------------------------------------}
{| PROCEDURE for demonstrating Euler angles
{| Version D
{| Author:  Randall Jones
{| Date:  {8/93}
{+-------------------------------------------------------------------}
USES CRT, Printer, Graph,
     CUPS, CUPSMUPP, CUPSgui, CUPSGrph, CUPSfunc, CUPSproc;
TYPE
   VectorType = ARRAY[1..3] OF Real;
   EulerAngleType = RECORD phi,theta,psi:Real END; {RECORD}
   DrawFlagType = RECORD Body,Omega,Torque,AngMom:Boolean END; {RECORD}
   GraphObjectType =
      RECORD
         Title:String;
         NumPnts: Integer;
         NumTxt:Integer;
         NumSegs: Integer;
         FrontCol,BackCol: Byte;
         rVecs: ARRAY[1..30] OF VectorType;
         Segments: ARRAY[1..50,1..2] OF BYTE;
         Txt:ARRAY[1..10] OF STRING[10];
      END; {RECORD}
   GraphVectorType =
      RECORD
         Title:String;
         NumTxt:Integer;
         FrontCol,BackCol: Byte;
         magnitude:Real;
         rVecs: ARRAY[1..5] OF VectorType;
         Segments: ARRAY[1..3,1..2] OF BYTE;
         Txt:STRING[10];
      END; {RECORD}
   ProblemTypes = (FixedRotation,FixedTorque,SpinningTop);
VAR
   GraphBody,GraphAxes: GraphObjectType;
   GraphOmega,GraphTorque,GraphAngMom:GraphVectorType;
   rVec1,rVec2:DVector;
   DrawMat,DrawMat2,EulerMat,InvEulerMat,TempMat:DMatrix;
   OmegaFixed,OmegaBody,TorqueFixed,TorqueBody:DVector;
   AngMomFixed,AngMomBody:DVector;
   Origin,InitOmega,InitTorque,EulerVec,RK4Vec:DVector;
   FixedDraw,BodyDraw:DrawFlagType;
   Problem:ProblemTypes;
   Anim:TFrames;
   MainMenu: TMenu;
   ChangeObjDimKeys,HotKeys:THotKeys;
   DegToRad,RadToDeg:Real;
   InitBodyAngle,CurrBodyAngle:EulerAngleType;
   TwoViews,StereoGraphic,Sliders,IncludeTorque,IncludeAngMom:Boolean;
   DoneFlag,RunFlag:Boolean;
   t,dt,Mass,PiT2:Real;
   KeyHit:Byte;
   TimeDelay:Integer;
   ObjDim,MomInert:VectorType;
   TorqueScale,AngMomScale,OmegaScale:Real;
   TorqueMag:Real;

   PROCEDURE GenVector(VAR Vector,Tail:DVector;
                       VAR GraphVector:GraphVectorType;
                       Scale:Real);
  {+------------------------------------------------------------------}
  {| This procedure generates the points representing a 3-D vector.
  {| The head is generated lying in a plane parallel to the z axis.
  {| Points are inserted into GraphVector, the object holding the graphing
  {| information for this vector.
  {+------------------------------------------------------------------}
   VAR
      i,n,rInd:integer;
      mag,rho,a:Real;
      CosTh,SinTh,CosPh,SinPh:Real;
   BEGIN WITH GraphVector DO BEGIN
      mag := sqrt( SQR(Vector.Value(1)) +
                   SQR(Vector.Value(2)) +
                   SQR(Vector.Value(3)) );
      magnitude := mag*scale;
      IF magnitude<0.1 THEN magnitude := 0;
      IF magnitude<>0 THEN
         BEGIN
           {+----------------------------------------------------------}
           {| Determine the spherical variables (theta,phi) giving the
           {| orientation of the vector
           {+----------------------------------------------------------}
            rho := sqrt(SQR(Vector.Value(1))+SQR(Vector.Value(2)));
            CosTh := Vector.Value(3)/mag; SinTh := rho/mag;
            IF rho<>0
               THEN BEGIN
                       CosPh:=Vector.Value(1)/rho;
                       SinPh:=Vector.Value(2)/rho
                    END
               ELSE BEGIN CosPh:=0; SinPh:=1; END;
           {+----------------------------------------------------------}
           {| Generate the vector coordinates at this orientation.
           {| rVecs[3],rVecs[4] hold positions of ends of arrow head.
           {| rVecs[5] holds position for label.
           {+----------------------------------------------------------}
            FOR i := 1 to 3 DO
               BEGIN
                  rVecs[1,i] := Tail.Value(i);
                  rVecs[2,i] := Tail.Value(i) + Vector.Value(i)*Scale;
                  rVecs[5,i] := rVecs[2,i] + Vector.Value(i)/mag;
               END;
            a := 0.5;
            mag := mag*scale;
            rVecs[3,1] := rVecs[1,1] + a*CosTh*CosPh + (mag-a)*SinTh*CosPh;
            rVecs[3,2] := rVecs[1,2] + a*CosTh*SinPh + (mag-a)*SinTh*SinPh;
            rVecs[3,3] := rVecs[1,3] - a*SinTh       + (mag-a)*CosTh;
            rVecs[4,1] := rVecs[1,1] - a*CosTh*CosPh + (mag-a)*SinTh*CosPh;
            rVecs[4,2] := rVecs[1,2] - a*CosTh*SinPh + (mag-a)*SinTh*SinPh;
            rVecs[4,3] := rVecs[1,3] + a*SinTh       + (mag-a)*CosTh;
         END;
   END;  END;

   PROCEDURE SetUpObject;
  {+-------------------------------------------------------------------}
  {| Set up GraphBody with information for cube.
  {+-------------------------------------------------------------------}
   VAR
      Size:Real;
      i,j,k:integer;
   BEGIN
      WITH GraphBody DO
         BEGIN
            Size := 2;
            Title := 'Body';
            NumPnts := 23;
            NumTxt := 3;
            NumSegs := 18;
            FrontCol := 15;
            BackCol := 7;
           {+---------------------------------------------------------}
           {| Set up vectors representing the 8 vertices of a cube:
           {| (---)(--+)(-+-)(-++)(+--)(+-+)(+++)(++-)
           {| Note that (-+-) and (-++) are switched from, normal
           {| counting order as are (++-) and (+++).  This simplifies
           {| for ease of segment definitions.  Similar to
           {| Karnaugh Maps actually.
           {+---------------------------------------------------------}
            FOR i := 0 to 1 DO FOR j := 0 to 1 DO FOR k := 0 to 1 DO
            BEGIN
               rVecs[4*i+2*j+k+1,1] := ObjDim[1]*(i-0.5);
               rVecs[4*i+2*j+k+1,2] := ObjDim[2]*(j-0.5);
               rVecs[4*i+2*j+k+1,3] := ObjDim[3]*(k-0.5);
            END;
            rVecs[3,3] := -rVecs[3,3]; rVecs[7,3] := -rVecs[7,3];
            rVecs[4,3] := -rVecs[4,3]; rVecs[8,3] := -rVecs[8,3];
            FOR i := 9 to 20 DO  FOR j := 1 to 3 DO rVecs[i,j] := 0;
            FOR i := 1 to 3 DO
               BEGIN
                  rVecs[8+i,i] :=  ObjDim[i]/2;
                  rVecs[11+i,i] := -ObjDim[i]/2;
                  rVecs[14+i,i] :=  4*Size;
                  rVecs[17+i,i] := -4*Size;
               END;
           {+---------------------------------------------------------}
           {| Set up vectors representing text
           {+---------------------------------------------------------}
            FOR i := 21 TO 23 DO FOR j := 1 to 3 DO rVecs[i,j] := 0;
            FOR i := 1 to 3 DO rVecs[20+i,i] := 4.5*Size;
           {+---------------------------------------------------------}
           {| Describe line segments
           {+---------------------------------------------------------}
            FOR i := 1 to 4 DO
               BEGIN
                  Segments[i,1] := i;
                  Segments[i,2] := i+1;
                  Segments[i+4,1] := i+4;
                  Segments[i+4,2] := i+5;
                  Segments[i+8,1] := i;
                  Segments[i+8,2] := i+4;
               END;
            Segments[4,2] := 1;
            Segments[8,2] := 5;
            FOR i := 1 to 6 DO
               BEGIN
                  Segments[i+12,1] := 8+i;
                  Segments[i+12,2] := 14+i;
               END;
           {+---------------------------------------------------------}
           {| Describe text
           {+---------------------------------------------------------}
            Txt[1] := 'x';
            Txt[2] := 'y';
            Txt[3] := 'z';
         END;
   END;

   PROCEDURE SetUpGraphObjects;
  {+------------------------------------------------------------------}
  {| This procedure sets up the definition of the Body, a vector and
  {| the coordinate axes.
  {+------------------------------------------------------------------}
   VAR
      i,j:Integer;
      size:Real;
   BEGIN
      SetUpObject;
     {+----------------------------------------------------------------}
     {| Set up GraphAxes with information for coordinate axes.
     {+----------------------------------------------------------------}
      WITH GraphAxes DO
         BEGIN
            Title := 'Fixed Coordinate Axes';
            NumPnts := 7;
            NumTxt := 3;
            NumSegs := 3;
            FrontCol := LightMagenta;
            BackCol  := Magenta;
            size := 8;
            For i := 1 to 7 DO FOR j := 1 to 3 DO rVecs[i,j] := 0.0;
            rVecs[2,1] :=  size;
            rVecs[3,2] :=  size;
            rVecs[4,3] :=  size;
            rVecs[5,1] :=  size+1;
            rVecs[6,2] :=  size+1;
            rVecs[7,3]:=  size+1;
            FOR i := 1 to 3 DO
               BEGIN
                  Segments[i,1] := 1;
                  Segments[i,2] := i+1;
               END;
            Txt[1] := 'x''';
            Txt[2] := 'y''';
            Txt[3] := 'z''';
         END;
     {+----------------------------------------------------------------}
     {| Set up GraphOmega with information for vector.
     {| Specific vector positions are inserted by GenVector.
     {+----------------------------------------------------------------}
      WITH GraphOmega DO
         BEGIN
            Title := 'Omega';
            NumTxt := 1;
            FrontCol := LightRed;
            BackCol := Red;
            Segments[1,1] := 1; Segments[1,2] := 2;
            Segments[2,1] := 2; Segments[2,2] := 3;
            Segments[3,1] := 2; Segments[3,2] := 4;
            Txt := 'w';
         END;
     {+----------------------------------------------------------------}
     {| Set up GraphTorque with information for vector.
     {| Specific vector positions are inserted by GenVector.
     {+----------------------------------------------------------------}
      WITH GraphTorque DO
         BEGIN
            Title := 'Torque';
            NumTxt := 1;
            FrontCol := LightCyan;
            BackCol := Cyan;
            Segments[1,1] := 1; Segments[1,2] := 2;
            Segments[2,1] := 2; Segments[2,2] := 3;
            Segments[3,1] := 2; Segments[3,2] := 4;
            Txt := 'T';
         END;
      WITH GraphAngMom DO
         BEGIN
            Title := 'AngMom';
            NumTxt := 1;
            FrontCol := LightGreen;
            BackCol := Green;
            Segments[1,1] := 1; Segments[1,2] := 2;
            Segments[2,1] := 2; Segments[2,2] := 3;
            Segments[3,1] := 2; Segments[3,2] := 4;
            Txt := 'L';
         END;
   END;

{+--------------------------------------------------------------------------}
{| Rotation Utilities
{+--------------------------------------------------------------------------}
   PROCEDURE GenRotMat(VAR RotMat:Dmatrix; EulerAngle:EulerAngleType);
  {+------------------------------------------------------------------}
  {| This program generates a rotation matrix from the Euler Angles.
  {| This matrix rotates the coordinates of a point by the Euler Angles.
  {+------------------------------------------------------------------}
   VAR
      CosPhi,CosTheta,CosPsi:Real;
      SinPhi,SinTheta,SinPsi:Real;
   BEGIN
      WITH EulerAngle DO
         BEGIN
            CosPhi :=   Cos(phi);   SinPhi :=   -Sin(phi);
            CosTheta := Cos(theta); SinTheta := -Sin(theta);
            CosPsi :=   Cos(psi);   SinPsi :=   -Sin(psi);
         END;
      RotMat.Put(1,1, CosPhi*CosPsi - CosTheta*SinPsi*SinPhi);
      RotMat.Put(2,1,-SinPhi*CosPsi - CosTheta*SinPsi*CosPhi);
      RotMat.Put(3,1,                 SinTheta*SinPsi);
      RotMat.Put(1,2, CosPhi*SinPsi + CosTheta*CosPsi*SinPhi);
      RotMat.Put(2,2,-SinPhi*SinPsi + CosTheta*CosPsi*CosPhi);
      RotMat.Put(3,2,               - SinTheta*CosPsi);
      RotMat.Put(1,3, SinPhi*SinTheta);
      RotMat.Put(2,3, CosPhi*SinTheta);
      RotMat.Put(3,3,        CosTheta);
   END;

{+---------------------------------------------------------------------}
{| Object drawing utilities
{+---------------------------------------------------------------------}
   PROCEDURE DrawVector(VAR GraphVector:GraphVectorType;
                        VAR RotMat:DMatrix);
  {+-------------------------------------------------------------------}
  {| This procedure plots the line segments describing a vector.
  {| DrawMat must be included in RotMat to generate the appropriate
  {| orientiation for visualization.
  {+-------------------------------------------------------------------}
   VAR
      i,n:integer;
      Scale:Real;
      TempRVecs: ARRAY [1..5,2..3] OF Real;
      xPosFlag:ARRAY[1..5] OF Boolean;
   BEGIN
      WITH GraphVector DO IF magnitude<>0 THEN
      BEGIN
        {+-------------------------------------------------------------}
        {| Rotate points according to RotMat, then shrink or expand
        {| y-z dimensions based on x value.
        {+-------------------------------------------------------------}
         FOR n := 1 to 5 DO
            BEGIN
               FOR i := 1 TO 3 DO rVec1.Put(i,rVecs[n,i]);
               RotMat.MultVector(rVec1,rVec2);
               Scale := 1 + rVec2.Value(1)/20;
               TempRVecs[n,2] := rVec2.Value(2)*Scale;
               TempRVecs[n,3] := rVec2.Value(3)*Scale;
               xPosFlag[n] := rVec2.Value(1)<=0;
            END;
        {+-------------------------------------------------------------}
        {| Draw y-z components of figure.
        {+-------------------------------------------------------------}
         FOR  i := 1 to 3 DO
            BEGIN
               IF xPosFlag[Segments[i,1]] AND xPosFlag[Segments[i,2]]
                  THEN SetColor(BackCol) ELSE SetColor(FrontCol);
               PlotLine(TempRVecs[Segments[i,1],2],
                        TempRVecs[Segments[i,1],3],
                        TempRVecs[Segments[i,2],2],
                        TempRVecs[Segments[i,2],3]);
            END;
        {+-------------------------------------------------------------}
        {| Write text at appropriate positions.
        {+-------------------------------------------------------------}
         IF NumTxt<>0 THEN
            BEGIN
               IF xPosFlag[5] THEN SetColor(BackCol)
                              ELSE SetColor(FrontCol);
               OutTextXY(MapX(TempRVecs[5,2]),
                         MapY(TempRVecs[5,3]),txt);
            END;
      END;
      SetColor(15);
   END;

   PROCEDURE DrawObject(VAR GraphObject:GraphObjectType;
                        VAR RotMat:DMatrix);
  {+------------------------------------------------------------------}
  {| This procedure plots the line segments describing a figure.
  {| DrawMat must be included in RotMat to generate the appropriate
  {| orientiation for visualization.
  {+------------------------------------------------------------------}
   VAR
      i,n,rInd:integer;
      Scale:Real;
      TempRVecs: ARRAY [1..30,2..3] OF Real;
      xPosFlag: ARRAY[1..30] OF Boolean;
   BEGIN
      WITH GraphObject DO
      BEGIN
        {+-------------------------------------------------------------}
        {| Rotate the points according to RotMat, then shrink or expand
        {| y-z dimensions based on x value.
        {+-------------------------------------------------------------}
         FOR n := 1 to NumPnts DO
            BEGIN
               FOR i := 1 TO 3 DO rVec1.Put(i,rVecs[n,i]);
               RotMat.MultVector(rVec1,rVec2);
               Scale := 1 + rVec2.Value(1)/20;
               TempRVecs[n,2] := rVec2.Value(2)*Scale;
               TempRVecs[n,3] := rVec2.Value(3)*Scale;
               xPosFlag[n] := rVec2.Value(1)<=0;
            END;
        {+-------------------------------------------------------------}
        {| Draw y-z components of figure.
        {+-------------------------------------------------------------}
         FOR  i := 1 to NumSegs DO
            BEGIN
               IF xPosFlag[Segments[i,1]] AND xPosFlag[Segments[i,2]]
                  THEN SetColor(BackCol) ELSE SetColor(FrontCol);
               PlotLine(TempRVecs[Segments[i,1],2],
                        TempRVecs[Segments[i,1],3],
                        TempRVecs[Segments[i,2],2],
                        TempRVecs[Segments[i,2],3]);
            END;
        {+-------------------------------------------------------------}
        {| Write text at appropriate positions.
        {+-------------------------------------------------------------}
         IF NumTxt<>0 THEN
            FOR i := 1 TO NumTxt DO
               BEGIN
                  rInd := NumPnts-NumTxt + i;
                  IF xPosFlag[rInd] THEN SetColor(BackCol)
                                    ELSE SetColor(FrontCol);
                  OutTextXY(MapX(TempRVecs[rInd,2]),
                            MapY(TempRVecs[rInd,3]),Txt[i]);
               END;
      END;
      SetColor(15);
   END;

   PROCEDURE PrintData;
   BEGIN
      GraphBackColor := Cyan;
      OpenViewPort(6);
      GraphBackColor := DarkGray;
      WITH CurrBodyAngle DO IF StereoGraphic AND TwoViews
      THEN
         BEGIN
            Print(3,1,Concat('t:    ',NumStr(t,7,3),' s'));
            Print(3,2,Concat('phi:  ',NumStr(RadToDeg*ABS(phi),6,2),' deg'));
            Print(3,3,Concat('theta:',NumStr(RadToDeg*theta,6,2),' deg'));
            Print(3,4,Concat('psi:  ',NumStr(RadToDeg*ABS(psi),7,2),' deg'));
         END
      ELSE
         BEGIN
            Print(1,1,Concat('t:',NumStr(t,7,3),' s'));
            Print(13,1,Concat(' phi:',NumStr(RadToDeg*phi,6,2),' deg'));
            Print(29,1,Concat(' theta:',NumStr(RadToDeg*theta,6,2),' deg'));
            Print(47,1,Concat(' psi:',NumStr(RadToDeg*psi,7,2),' deg'));
            SelectScale(2);
            PlotLine(12,-1,12,1);
            PlotLine(28,-1,28,1);
            PlotLine(46,-1,46,1);
         END;
   END;

   PROCEDURE DrawScreen;
  {+-------------------------------------------------------------------}
  {| Draw the selected objects on the screen.
  {+-------------------------------------------------------------------}
   BEGIN
      HideMouse;
      SelectScale(1);
      WITH FixedDraw DO
         BEGIN
            OpenViewPort(1);
            Print(1,1,'Fixed Frame');
            DrawMat.MultMatrix(EulerMat,TempMat);
            DrawObject(GraphAxes,DrawMat);
            IF (Body)   THEN DrawObject(GraphBody,TempMat);
            IF (Omega)  THEN DrawVector(GraphOmega, DrawMat);
            IF (Torque) THEN DrawVector(GraphTorque,DrawMat);
            IF (AngMom) THEN DrawVector(GraphAngMom,DrawMat);
            IF StereoGraphic THEN
               BEGIN
                  OpenViewPort(3);
                  Print(1,1,'Fixed Frame');
                  DrawObject(GraphAxes,DrawMat2);
                  DrawMat2.MultMatrix(EulerMat,TempMat);
                  IF (Body)   THEN DrawObject(GraphBody,TempMat);
                  IF (Omega)  THEN DrawVector(GraphOmega, DrawMat2);
                  IF (Torque) THEN DrawVector(GraphTorque,DrawMat2);
                  IF (AngMom) THEN DrawVector(GraphAngMom,DrawMat2);
               END;
         END;
      IF TwoViews THEN WITH BodyDraw DO
         BEGIN
            OpenViewPort(2);
            Print(1,1,'Body Centered Frame');
            DrawMat.MultMatrix(InvEulerMat,TempMat);
            DrawObject(GraphAxes,TempMat);
            IF (Body)   THEN DrawObject(GraphBody,  DrawMat);
            IF (Omega)  THEN DrawVector(GraphOmega, TempMat);
            IF (Torque) THEN DrawVector(GraphTorque,TempMat);
            IF (AngMom) THEN DrawVector(GraphAngMom,TempMat);
            IF StereoGraphic THEN
               BEGIN
                  OpenViewPort(4);
                  Print(1,1,'Body Centered Frame');
                  DrawMat2.MultMatrix(InvEulerMat,TempMat);
                  DrawObject(GraphAxes,TempMat);
                  IF (Body)   THEN DrawObject(GraphBody,  DrawMat2);
                  IF (Omega)  THEN DrawVector(GraphOmega, TempMat);
                  IF (Torque) THEN DrawVector(GraphTorque,TempMat);
                  IF (AngMom) THEN DrawVector(GraphAngMom,TempMat);
               END;
         END;
      PrintData;
      ShowMouse;
   END;

   PROCEDURE CalcMomentofInertia;
   BEGIN
      MomInert[1] := 2*Mass*(SQR(ObjDim[2])+SQR(ObjDim[3]));
      MomInert[2] := 2*Mass*(SQR(ObjDim[1])+SQR(ObjDim[3]));
      MomInert[3] := 2*Mass*(SQR(ObjDim[1])+SQR(ObjDim[2]));
   END;

   PROCEDURE CalcAngularMomentum;
   VAR
      i:Integer;
      L:VectorType;
   BEGIN
      FOR i := 1 to 3 DO L[i] := OmegaBody.Value(i)*MomInert[i];
      FOR i := 1 to 3 DO AngMomBody.Put(i,L[i]);
      EulerMat.MultVector(AngMomBody,AngMomFixed);
      IF IncludeAngMom THEN
         GenVector(AngMomFixed,Origin,GraphAngMom,AngMomScale);
   END;

   PROCEDURE CalcTorque;
   VAR
      T:VectorType;
      i:Integer;
      mag,SinTheta:Real;
   BEGIN
      CASE Problem OF
         FixedRotation: WITH OmegaBody DO
            BEGIN
               T[1] := Value(2)*Value(3)*(MomInert[3]-MomInert[2]);
               T[2] := Value(3)*Value(1)*(MomInert[1]-MomInert[3]);
               T[3] := Value(1)*Value(2)*(MomInert[2]-MomInert[1]);
               FOR i := 1 to 3 DO TorqueBody.Put(i,T[i]);
               EulerMat.MultVector(TorqueBody,TorqueFixed);
            END;
         FixedTorque: InvEulerMat.MultVector(TorqueFixed,TorqueBody);
         SpinningTop:
            BEGIN
               SinTheta := Sin(CurrBodyAngle.theta);
               T[1] := TorqueMag*SinTheta*Cos(CurrBodyAngle.phi);
               T[2] := TorqueMag*SinTheta*Sin(CurrBodyAngle.phi);
               T[3] := 0;
               FOR i := 1 to 3 DO TorqueFixed.Put(i,T[i]);
               InvEulerMat.MultVector(TorqueFixed,TorqueBody);
            END;
      END; {CASE}
      IF IncludeTorque THEN
         BEGIN
            FOR i:=1 TO 3 DO rVec1.Put(i,AngMomFixed.Value(i)*AngMomScale);
            GenVector(TorqueFixed,rVec1,GraphTorque,TorqueScale);
         END;
   END;

   PROCEDURE ResetSystem;
   VAR
      i:Integer;
   BEGIN
      ClearMkBuffers;
      CheckForEvents;
      ClearMuppetPort;
      MainMenu.Display;
      IF NOT Sliders THEN HotKeys.Display;
      GraphBackColor := Red;
      OpenViewPort(5);
      CASE Problem OF
         FixedRotation: Print(1,1,'Fixed w Axis:');
         FixedTorque:   Print(1,1,'Fixed Torque:');
         SpinningTop:   Print(1,1,'Spinning Top:');
      END; {CASE}
      GraphBackColor := DarkGray;
      IF StereoGraphic THEN
         BEGIN
            GraphBackColor := Green;
            OpenViewPort(7);
            GraphBackColor := DarkGray;
            Print(1,1,'Focus past screen');
            Print(1,2,'to bring adjacent');
            Print(1,3,'images together');
         END;
     {+---------------------------------------------------------------}
     {| Set up Initial Euler Angles and corresponding rotation matrices
     {+---------------------------------------------------------------}
      t := 0;
      CurrBodyAngle := InitBodyAngle;
      GenRotMat(EulerMat,CurrBodyAngle);
      EulerMat.Transpose(InvEulerMat);
     {+---------------------------------------------------------------}
     {| Set up initial angular velocity.
     {+---------------------------------------------------------------}
      OmegaFixed.Equate(1.0,InitOmega);
      GenVector(OmegaFixed,Origin,GraphOmega,OmegaScale);
      InvEulerMat.MultVector(OmegaFixed,OmegaBody);
     {+---------------------------------------------------------------}
     {| Set up initial angular momentum.
     {+---------------------------------------------------------------}
      CalcMomentofInertia;
      IncludeAngMom := FixedDraw.AngMom OR BodyDraw.AngMom;
      CalcAngularMomentum;
     {+---------------------------------------------------------------}
     {| Set up initial torque.
     {+---------------------------------------------------------------}
      IF Problem=FixedTorque THEN TorqueFixed.Equate(1.0,InitTorque);
      IncludeTorque := FixedDraw.Torque OR BodyDraw.Torque;
      CalcTorque;
     {+---------------------------------------------------------------}
     {| Draw screens
     {+---------------------------------------------------------------}
      DrawScreen;
      Anim.Close;  {This sets firstPage=TRUE}
      Anim.Select(1);   {This will copy all of page 0 to page 1}
      Anim.Switch;  {This toggles firstPage to FALSE}
      Anim.Pause;   {This sets active and visual pages to page 0}
   END;

   PROCEDURE FindEuler(VAR Vector:DVector;
                       VAR Angle:EulerAngleType;
                       VAR mag:Real);
  {+------------------------------------------------------------------}
  {| This procedure determines the Euler angles (phi and theta only)
  {| corresponding to a particular vector.  SphPhi is the spherical
  {| vector phi.  Note that Euler.Phi is Pi/2 greater than SphPhi.
  {+------------------------------------------------------------------}
   VAR
      rho,SphPhi:Real;
   BEGIN
      WITH Angle DO BEGIN phi:=0; theta:=0; psi:=0; END;
      mag := sqrt( SQR(Vector.Value(1)) +
                   SQR(Vector.Value(2)) +
                   SQR(Vector.Value(3)) );
      IF mag<>0 THEN
         BEGIN
            rho := sqrt(SQR(Vector.Value(1)) + SQR(Vector.Value(2)));
            Angle.theta := ArcCos(Vector.Value(3)/mag);
            IF rho<>0 THEN
               BEGIN
                  SphPhi := ArcCos(Vector.Value(1)/rho);
                  IF Vector.Value(2)<0 THEN SphPhi := 2*Pi - SphPhi;
                  Angle.phi := SphPhi + Pi/2;
                  IF Angle.phi>=2*Pi-0.0001 THEN Angle.phi := Angle.phi - 2*Pi;
               END;
            WITH Angle DO
               BEGIN
                  IF abs(phi)  < 1E-10 THEN phi   := 0;
                  IF abs(theta)< 1E-10 THEN theta := 0;
                  IF abs(psi)  < 1E-10 THEN psi   := 0;
               END;
         END;
   END;


   PROCEDURE OpenPlot;
  {+------------------------------------------------------------------}
  {| This procedure sets up the graphics windows.
  {+------------------------------------------------------------------}
   VAR
      yVal:Real;
      i:Integer;
   BEGIN
      SetUpObject;
      ClearMuppetPort;
     {+----------------------------------------------------------------}
     {| Set Up and Display ViewPorts
     {+----------------------------------------------------------------}
      DefineScale(1,-12,12,-12,12);
      DefineScale(2,0,65,-1,1);
      DefineViewPort(1,0.1,0.65,0.07,0.865);
      DefineViewPort(5,0.00,0.18,0.87,0.93);           {Problem Label}
      DefineViewPort(6,0.18,1.0,0.87,0.93);            {Variable Values}
      IF Sliders THEN Exit;
      IF StereoGraphic THEN IF TwoViews
         THEN
            BEGIN
               DefineViewPort(1,0.00,0.28,0.5,0.94);
               DefineViewPort(3,0.30,0.58,0.5,0.94);
               DefineViewPort(2,0.00,0.28,0.06,0.50);
               DefineViewPort(4,0.30,0.58,0.06,0.50);
               DefineViewPort(5,0.66,1.0,0.8,0.9);  {Problem Label}
               DefineViewPort(6,0.66,1.0,0.4,0.8);  {Variable Values}
               DefineViewPort(7,0.76,1.0,0.15,0.38); {Stereographic note}
            END
         ELSE
            BEGIN
               DefineViewPort(1,0.00,0.35,0.2,0.8);
               DefineViewPort(3,0.37,0.72,0.2,0.8);
               DefineViewPort(7,0.76,1.00,0.15,0.38); {Stereographic note}
            END
      ELSE IF TwoViews THEN
         BEGIN
            DefineViewPort(1,0.00,0.48,0.1,0.84);
            DefineViewPort(2,0.52,1.00,0.1,0.84);
         END;
      Anim.Close;  {This sets firstPage=TRUE and sets a and v pages to 0}
   END;

   PROCEDURE ChangeDisplays;
  {+-------------------------------------------------------------------}
  {| This procedure uses an input screen to determine what is plotted.
  {+-------------------------------------------------------------------}
   VAR
      InputScreen:TInputScreen;
   BEGIN
      WITH InputScreen DO
         BEGIN
            init;
            DefineInputPort(0.0,0.6,0,0.9);
            LoadLine('  CHANGE DISPLAY');
            LoadLine('#F Include Body-Centered Frame');
            IF TwoViews THEN SetBoolean(1,TRUE);
            LoadLine('Objects drawn in Fixed Frame:');
            LoadLine('   #F Body        #F Torque');
            LoadLine('   #F Omega       #F Angular Momentum');
            WITH FixedDraw DO
               BEGIN
                  IF Body   THEN SetBoolean(2,True);
                  IF Torque THEN SetBoolean(3,True);
                  IF Omega  THEN SetBoolean(4,True);
                  IF AngMom THEN SetBoolean(5,True);
               END;
            LoadLine('Objects drawn in Body Frame:');
            LoadLine('   #F Body        #F Torque');
            LoadLine('   #F Omega       #F Angular Momentum');
            WITH BodyDraw DO
               BEGIN
                  IF Body   THEN SetBoolean(6,True);
                  IF Torque THEN SetBoolean(7,True);
                  IF Omega  THEN SetBoolean(8,True);
                  IF AngMom THEN SetBoolean(9,True);
               END;
            LoadLine('#F StereoGraphic Projection');
            LoadLine('Omega Scale Factor:           {          }');
            LoadLine('Torque Scale Factor:          {          }');
            LoadLine('Angular Momentum Scale Factor:{          }');
            SetNumber(11,OmegaScale);
            SetNumber(12,TorqueScale);
            SetNumber(13,AngMomScale);
            IF StereoGraphic THEN SetBoolean(10,TRUE);
            LoadLine('');
            LoadLine(' [  OK  ]      [Cancel]      [ Help ]');
            SetHelpFile('Rotate.HLP','ChangeDisplaysHelp');
            AcceptScreen;
            IF NOT canceled THEN
               BEGIN
                  TwoViews := GetBoolean(1);
                  WITH FixedDraw DO
                     BEGIN
                        Body :=   GetBoolean(2);
                        Torque := GetBoolean(3);
                        Omega :=  GetBoolean(4);
                        AngMom := GetBoolean(5);
                        IF Torque THEN AngMom := True;
                     END;
                  WITH BodyDraw DO
                     BEGIN
                        Body :=   GetBoolean(6);
                        Torque := GetBoolean(7);
                        Omega :=  GetBoolean(8);
                        AngMom := GetBoolean(9);
                        IF Torque THEN AngMom := True;
                     END;
                  StereoGraphic := GetBoolean(10);
                  OmegaScale := GetNumber(11);
                  TorqueScale := GetNumber(12);
                  AngMomScale := GetNumber(13);
                  OpenPlot;
                  ResetSystem;
               END;
            done;
         END;
   END;

   PROCEDURE GetInitConds;
  {+-------------------------------------------------------------------}
  {| This procedure uses an input screen to get initial orientations of
  {| the body, angular velocity and torque.
  {+-------------------------------------------------------------------}
   VAR
      OmegaMag:Real;
      InitOmegaAngle,InitTorqueAngle:EulerAngleType;
      changed:Boolean;
      i:Integer;
      InputScreen:TInputScreen;
   BEGIN
      FindEuler(InitOmega,InitOmegaAngle,OmegaMag);
      WITH InputScreen DO
         BEGIN
            init;
            DefineInputPort(0.0,0.6,0,0.87);
            LoadLine('  Initial body orientation:');
            LoadLine('     phi: {          } deg');
            LoadLine('   theta: {          } deg');
            LoadLine('     psi: {          } deg');
            SetNumber(1,RadToDeg*InitBodyAngle.phi);
            SetNumber(2,RadToDeg*InitBodyAngle.theta);
            SetNumber(3,RadToDeg*InitBodyAngle.psi);
            LoadLine('  Initial angular velocity:');
            LoadLine('           phi: {          } deg');
            LoadLine('         theta: {          } deg');
            LoadLine('     Magnitude  {          } rad/s');
            SetNumber(4,RadToDeg*InitOmegaAngle.phi);
            SetNumber(5,RadToDeg*InitOmegaAngle.theta);
            SetNumber(6,OmegaMag);
            IF Problem=FixedTorque THEN
               BEGIN
                  LoadLine('  Initial torque:');
                  LoadLine('           phi: {          } deg');
                  LoadLine('         theta: {          } deg');
                  LoadLine('     Magnitude  {          } N-m');
                  FindEuler(InitTorque,InitTorqueAngle,TorqueMag);
                  SetNumber(7,RadToDeg*InitTorqueAngle.phi);
                  SetNumber(8,RadToDeg*InitTorqueAngle.theta);
                  SetNumber(9,TorqueMag);
               END;
            IF Problem=SpinningTop THEN
               BEGIN
                  LoadLine('Initial torque - Magnitude:  {          } N-m');
                  SetNumber(7,TorqueMag);
               END;
            LoadLine('');
            LoadLine(' [  OK  ]      [Cancel]      [ Help ]');
            SetHelpFile('Rotate.HLP','GetInitCondHelp');
            AcceptScreen;
            changed := NOT canceled;
            IF changed THEN
               BEGIN
                  InitBodyAngle.phi    := DegToRad*GetNumber(1);
                  InitBodyAngle.theta  := DegToRad*GetNumber(2);
                  InitBodyAngle.psi    := DegToRad*GetNumber(3);
                  InitOmegaAngle.phi   := DegToRad*GetNumber(4);
                  InitOmegaAngle.theta := DegToRad*GetNumber(5);
                  OmegaMag             := GetNumber(6);
                  GenRotMat(TempMat,InitOmegaAngle);
                  rVec1.Put(1,0); rVec1.Put(2,0); rVec1.Put(3,OmegaMag);
                  TempMat.MultVector(rVec1,InitOmega);
                  IF Problem=FixedTorque THEN
                     BEGIN
                        InitTorqueAngle.phi   := DegToRad*GetNumber(7);
                        InitTorqueAngle.theta := DegToRad*GetNumber(8);
                        TorqueMag := GetNumber(9);
                        GenRotMat(TempMat,InitTorqueAngle);
                        rVec1.Put(1,0); rVec1.Put(2,0); rVec1.Put(3,TorqueMag);
                        TempMat.MultVector(rVec1,InitTorque);
                     END;
                  IF Problem=SpinningTop THEN TorqueMag := GetNumber(7);
                  ResetSystem;
               END;
            done;
        END;
   END;

   PROCEDURE CalcRK4P(t:Real; VAR RK4Vec,RK4VecP:DVector); FAR;
  {+-------------------------------------------------------------------}
  {| This procedure provides derivatives for RK4 for constant Torque
  {| problems.
  {+-------------------------------------------------------------------}
   VAR
      i:Integer;
      PhiP,ThetaP,PsiP:Real;
      SinTheta,CosTheta,SinPsi,CosPsi:Real;
      Omega,OmegaP:VectorType;
   BEGIN
     {+----------------------------------------------------------------}
     {| Insert current values of Euler angles and omega into
     {| variables.  Note that Omega[i] is used locally to hold the
     {| components of omega in the body frame.
     {+----------------------------------------------------------------}
      CurrBodyAngle.phi    := RK4Vec.Value(1);
      CurrBodyAngle.theta  := RK4Vec.Value(2);
      CurrBodyAngle.psi    := RK4Vec.Value(3);
      FOR i := 1 to 3 DO
         BEGIN
            Omega[i] := RK4Vec.Value(3+i);
            OmegaBody.Put(i,Omega[i]);
         END;
      GenRotMat(EulerMat,CurrBodyAngle);
      EulerMat.Transpose(InvEulerMat);
     {+----------------------------------------------------------------}
     {| Now evaluate PhiP,ThetaP,PsiP
     {+----------------------------------------------------------------}
      WITH CurrBodyAngle DO
         BEGIN
            SinTheta := Sin(theta); CosTheta := Cos(theta);
            SinPsi := Sin(psi); CosPsi := Cos(psi);
         END;
      IF SinTheta=0
         THEN PhiP := 0
         ELSE PhiP := ( Omega[1]*SinPsi + Omega[2]*CosPsi )/SinTheta;
      PsiP := Omega[3] - Costheta*PhiP;
      ThetaP := Omega[1]*CosPsi - Omega[2]*SinPsi;
      RK4VecP.Put(1,PhiP);
      RK4VecP.Put(2,ThetaP);
      RK4VecP.Put(3,PsiP);
     {+----------------------------------------------------------------}
     {| Now determine omegabodyP using Euler's Equations
     {|  -First determine the Torque components in the body frame.
     {|  -Then Euler's Equations can be implemented directly.
     {+----------------------------------------------------------------}
      CalcTorque;
      omegaP[1] := ( TorqueBody.Value(1) + (MomInert[2]-MomInert[3])*
                     Omega[2]*Omega[3] )/MomInert[1];
      omegaP[2] := ( TorqueBody.Value(2) + (MomInert[3]-MomInert[1])*
                     Omega[3]*Omega[1] )/MomInert[2];
      omegaP[3] := ( TorqueBody.Value(3) + (MomInert[1]-MomInert[2])*
                     Omega[1]*Omega[2] )/MomInert[3];
      FOR i := 1 to 3 DO RK4VecP.Put(3+i,omegaP[i]);
   END;

   PROCEDURE ConstantTorque(t,dt:Real);
  {+-------------------------------------------------------------------}
  {| The solution to the Euler Equations involves 6 variables:
  {|    Euler angles: (phi,theta,psi)
  {|    Body components of omega.
  {+-------------------------------------------------------------------}
   VAR
      i:Integer;
   BEGIN
      EulerMat.MultVector(OmegaBody,OmegaFixed);
      WITH CurrBodyAngle DO IF Sin(theta)=0 THEN
         BEGIN
            phi := phi + psi;
            IF OmegaFixed.Value(1)<>0
               THEN psi := ArcTan(-OmegaFixed.Value(2)/OmegaFixed.Value(1))
               ELSE psi := Pi/2;
            phi := phi - psi;
         END;
      RK4Vec.Put(1,CurrBodyAngle.phi);
      RK4Vec.Put(2,CurrBodyAngle.theta);
      RK4Vec.Put(3,CurrBodyAngle.psi);
      FOR i := 1 to 3 DO RK4Vec.Put(3+i,OmegaBody.Value(i));
      StepRK4(CalcRK4P,t,dt,RK4Vec);
      CurrBodyAngle.phi :=   RK4Vec.Value(1);
      CurrBodyAngle.theta := RK4Vec.Value(2);
      CurrBodyAngle.psi :=   RK4Vec.Value(3);
      FOR i := 1 to 3 DO OmegaBody.Put(i,RK4Vec.Value(3+i));
      GenRotMat(EulerMat,CurrBodyAngle);
      EulerMat.Transpose(InvEulerMat);
      EulerMat.MultVector(OmegaBody,OmegaFixed);
      GenVector(OmegaFixed,Origin,GraphOmega,OmegaScale);
   END;

   PROCEDURE CalcEulerP(t:Real; VAR EulerVec,EulerVecP:DVector); FAR;
  {+-------------------------------------------------------------------}
  {| This procedure provides derivatives for RK4 for constant omega problems
  {+-------------------------------------------------------------------}
   VAR
      PhiP,ThetaP,PsiP:Real;
      SinTheta,SinPsi,CosPsi:Real;
   BEGIN WITH CurrBodyAngle DO BEGIN
      phi    := EulerVec.Value(1);
      theta  := EulerVec.Value(2);
      psi    := EulerVec.Value(3);
      GenRotMat(EulerMat,CurrBodyAngle);
      EulerMat.Transpose(InvEulerMat);
      InvEulerMat.MultVector(OmegaFixed,OmegaBody);
      SinTheta := Sin(theta);
      SinPsi := Sin(psi); CosPsi := Cos(psi);
      IF SinTheta=0
         THEN PhiP := 0
         ELSE PhiP := ( OmegaBody.Value(1)*SinPsi +
                        OmegaBody.Value(2)*CosPsi )/SinTheta;
      PsiP := OmegaBody.Value(3) - Cos(theta)*PhiP;
      ThetaP := OmegaBody.Value(1)*CosPsi - OmegaBody.Value(2)*SinPsi;
      EulerVecP.Put(1,PhiP);
      EulerVecP.Put(2,ThetaP);
      EulerVecP.Put(3,PsiP);
   END; END;

   PROCEDURE ConstantOmega(t,dt:Real);
   BEGIN
      WITH CurrBodyAngle DO IF Sin(theta)=0 THEN
         BEGIN
            phi := phi + psi;
            IF OmegaFixed.Value(1)<>0
               THEN psi := ArcTan(-OmegaFixed.Value(2)/OmegaFixed.Value(1))
               ELSE psi := Pi/2;
            phi := phi - psi;
         END;
      EulerVec.Put(1,CurrBodyAngle.phi);
      EulerVec.Put(2,CurrBodyAngle.theta);
      EulerVec.Put(3,CurrBodyAngle.psi);
      StepRK4(CalcEulerP,t,dt,EulerVec);
      CurrBodyAngle.phi :=   EulerVec.Value(1);
      CurrBodyAngle.theta := EulerVec.Value(2);
      CurrBodyAngle.psi :=   EulerVec.Value(3);
      GenRotMat(EulerMat,CurrBodyAngle);
      EulerMat.Transpose(InvEulerMat);
   END;

   PROCEDURE GenMotion;
  {+-------------------------------------------------------------------}
  {| Test system.  Real program will not use Euler angles to generate
  {| motion.  New postions of points will be found and stored in
  {| graphobjects.  Euler angles will be calculated for use in generating
  {| other graphs.
  {+-------------------------------------------------------------------}
   VAR
      i:Integer;
   BEGIN
      Delay(TimeDelay);
      CASE Problem OF
         FixedRotation: ConstantOmega(t,dt);
         FixedTorque: ConstantTorque(t,dt);
         SpinningTop: ConstantTorque(t,dt);
      END; {CASE}
      t := t + dt;
      WITH CurrBodyAngle DO
         BEGIN
            IF theta > Pi THEN
               BEGIN
                  theta := PiT2 - theta;
                  Phi := Phi + Pi;
                  Psi := Psi + Pi;
               END
            ELSE IF theta < 0 THEN
               BEGIN
                  theta := -theta;
                  Phi := Phi + Pi;
                  Psi := Psi + Pi;
               END;
            IF      Phi > PiT2-0.0001  THEN Phi := Phi - PiT2
            ELSE IF Phi < -0.0002      THEN Phi := Phi + PiT2;
            IF      Psi > PiT2-0.0001  THEN Psi := Psi - PiT2
            ELSE IF Psi < -0.0002      THEN Psi := Psi + PiT2;
         END;
      IF IncludeAngMom THEN CalcAngularMomentum;
      IF IncludeTorque THEN CalcTorque;
      DrawScreen;
      Anim.Switch;
      DrawScreen;
      Anim.Switch;
   END;

{+--------------------------------------------------------------------------}
{| Input Screen Utilities
{+--------------------------------------------------------------------------}
   PROCEDURE PrintMomentofInertia;
   BEGIN
      CalcMomentofInertia;
      SelectViewPort(1);
      Rubout(1,1,12,DarkGray);
      Print(1,1,'Moment of Inertia - kg-m^2');
      Print(1,2,Concat(' Ix: ',Num2Str(MomInert[1],4)));
      Print(1,3,Concat(' Iy: ',Num2Str(MomInert[2],4)));
      Print(1,4,Concat(' Iz: ',Num2Str(MomInert[3],4)));
   END;

   PROCEDURE ChangeObjectDimensions;
   VAR
      SlideDone:Boolean;
      MomSlider:TSliders;
      xPos,dx,yPos,dy:Real;
      i:Integer;
      HoldFixedDraw:DrawFlagType;
      HoldTwoViews,HoldStereo:Boolean;
      HoldObjDim:VectorType;
      KeyHit:Byte;
   BEGIN
      Sliders := True;
      HoldTwoViews := TwoViews;
      HoldStereo := StereoGraphic;
      TwoViews := False;
      StereoGraphic := False;
      HoldFixedDraw := FixedDraw;
      WITH FixedDraw DO
         BEGIN Body := True; Omega:=False; Torque:=False; AngMom := False; END;
      OpenPlot;
      ResetSystem;
      Message('Adjust dimensions of the object - Enter when finished');
      dy := 0.08; yPos := 0.8; xPos := 0.66;
      MomSlider.init;
      MomSlider.Create(1,1,10,ObjDim[1],xPos,yPos,xPos+0.3,1,'','','xDim',False);
      yPos := yPos - dy;
      MomSlider.Create(2,1,10,ObjDim[2],xPos,yPos,xPos+0.3,1,'','','yDim',False);
      yPos := yPos - dy;
      MomSlider.Create(3,1,10,ObjDim[3],xPos,yPos,xPos+0.3,1,'','','zDim',False);
      MomSlider.DrawAll;
      ChangeObjDimKeys.display;
      SlideDone:=False;
      PrintMomentofInertia;
      HoldObjDim := ObjDim;
      REPEAT
         CheckForEvents;
         IF ChangeObjDimKeys.Pressed(KeyHit) THEN
         CASE KeyHit OF
            1: ShowHelp('Rotate.HLP','ChangeObjectDimensionsHelp');
            2: SlideDone := True;
            3: BEGIN SlideDone := True; ObjDim := HoldObjDim; END;
         END; {CASE}
         WITH MomSlider DO IF Changed THEN
            BEGIN
               ObjDim[LastAltered] := Value(LastAltered);
               SetUpObject;
               DrawScreen;
               PrintMomentofInertia;
            END;
      UNTIL SlideDone;
      SetUpObject;
      FixedDraw := HoldFixedDraw;
      TwoViews := HoldTwoViews;
      Sliders := False;
      OpenPlot;
      ResetSystem;
      MomSlider.done;
   END;

   Procedure SetUpHotKeys;
   BEGIN
      WITH HotKeys DO
         BEGIN
            Init(6);
            key[1] := 'F2-Run';
            key[2] := 'F3-Step';
            key[3] := 'F4-Reverse';
            key[4] := 'F5-Slow-Down';
            key[5] := 'F6-Speed-Up';
            key[6] := 'F10-Menu';
         END;
      WITH ChangeObjDimKeys DO
         BEGIN
            Init(3);
            key[1] := 'F1-Help';
            key[2] := 'Return-Done';
            key[3] := 'Esc-Quit';
         END;
   END;

   PROCEDURE SetUpMenu;
   BEGIN
      MainMenu.Init;
      WITH MainMenu DO
         BEGIN
            Column(1,'FILE');
               Row(1,1,'About CUPS');
               Row(1,2,'About Program - F1');
               Row(1,3,'Configuration');
               Row(1,4,'EXIT');
            Column(2,'Systems');
               Row(2,1,'Fixed Rotation Axis');
               Row(2,2,'Constant Torque');
               Row(2,3,'Spinning Top');
            Column(3,'Parameters');
               Row(3,1,'Specify Initial Conditions');
               Row(3,2,'Specify Body Dimensions');
            Column(4,'Display');
            Column(5,'Reset');
         END;
   END;

   PROCEDURE ProblemInit(NewProblem:ProblemTypes);
   BEGIN
      InitTorque.Fill(0);
      InitOmega.Fill(0);
      Problem := NewProblem;
      WITH FixedDraw DO
         BEGIN
            Body := True;   Omega := True;
            Torque := True; AngMom := True;
         END;
      WITH BodyDraw DO
         BEGIN
            Body := True;   Omega := True;
            Torque := True; AngMom := True;
         END;
      CASE Problem OF
         FixedRotation:
            BEGIN
               WITH InitBodyAngle DO
                  BEGIN Phi := 0; Theta := Pi/6; Psi := 0; END;
               InitOmega.Put(3,6);
               TorqueScale := 0.002; AngMomScale := 0.005; OmegaScale := 1;
               ObjDim[1] := 3; ObjDim[2] := 3; ObjDim[3] := 8;
            END;
         FixedTorque:
            BEGIN
               InitOmega.Put(3,5.5671);
               InitOmega.Put(2,-2.2377);
               WITH InitBodyAngle DO
                  BEGIN Phi := 0; Theta := Pi/6; Psi := 0; END;
               TorqueScale := 0.002; AngMomScale := 0.02; OmegaScale := 1;
               ObjDim[1] := 3; ObjDim[2] := 3; ObjDim[3] := 8;
            END;
         SpinningTop:
            BEGIN
               InitOmega.Put(1,20/sqrt(2));
               InitOmega.Put(2,-20/sqrt(2));
               TorqueMag := 200;
               WITH InitBodyAngle DO
                  BEGIN Phi := Pi/4; Theta := Pi/2; Psi := 0; END;
               TorqueScale := 0.01; AngMomScale := 0.005; OmegaScale := 0.5;
               ObjDim[1] := 3; ObjDim[2] := 3; ObjDim[3] := 8;
            END;
      END; {CASE}
   END;

   PROCEDURE HandleMenu;
   BEGIN
      IF MainMenu.Chosen THEN WITH MainMenu DO
         CASE colChosen OF
          1: CASE rowChosen OF                                  {FILE}
              1: aboutCUPS;
              2: ShowHelp('Rotate.HLP','General Help');
              3: Configuration;
              4: DoneFlag := TRUE;
             END; {CASE}
          2: BEGIN                                             {SYSTEMS}
                CASE rowChosen OF
                   1: ProblemInit(FixedRotation);
                   2: ProblemInit(FixedTorque);
                   3: ProblemInit(SpinningTop);
                END; {CASE}
                OpenPlot;
                ResetSystem;
             END;
          3: CASE rowChosen OF                                {InitCond}
              1: GetInitConds;
              2: ChangeObjectDimensions;
             END; {CASE}
          4: ChangeDisplays;
          5: ResetSystem;
         END; {CASE}
      ClearMkBuffers;
   END;

   PROCEDURE StartRun;
   BEGIN
      IF NOT RunFlag THEN
         BEGIN
            HotKeys.key[1] := 'F2-Pause';
            HotKeys.Display;
            Anim.Continue;
            HideMouse;
            HotKeys.Display;
            RunFlag:=True;
         END;
   END;

   PROCEDURE PauseRun;
   BEGIN
      IF RunFlag THEN
         BEGIN
            HotKeys.key[1] := 'F2-Run';
            HotKeys.Display;
            Anim.Pause;
            HotKeys.Display;
            ShowMouse;
            RunFlag := False;
         END;
   END;

    Procedure HandleHotKey(KeyHit:Byte);
   {+----------------------------------------------------------------}
   {| This procedure handles hot keys for animation
   {+----------------------------------------------------------------}
   BEGIN
     {+----------------------------------------------------------------}
     {| Determine the appropriate action for the hotkey choice
     {+----------------------------------------------------------------}
      CASE keyHit OF
         1: IF RunFlag THEN PauseRun
                       ELSE StartRun;
         2: BEGIN                    {F3 - STEP}
               Anim.Continue;
               IF NOT RunFlag THEN HideMouse;
               RunFlag := True;
               GenMotion;
               PauseRun;
            END;
         3: dt := -dt;                                {F4 - REVERSE}
         4: TimeDelay := 2*(TimeDelay+1);             {F5 - SLOW DOWN}
         5: TimeDelay := Trunc(TimeDelay/2);          {F6 - SPEED UP}
         6: BEGIN PauseRun; HandleMenu; END;          {F10 - MENU}
      END {CASE};
      ClearMkBuffers;
      CheckForEvents;
   END;

   PROCEDURE HandleMouse;
  {+-------------------------------------------------------------------}
  {| During Animation:
  {|    Mouse is hidden to improve animation speed.
  {|    Mouse click will pause animation.
  {| When animation is paused:
  {|    Click at top of screen is passed on to HandleMenu
  {|    Select MainMenu
  {+-------------------------------------------------------------------}
   BEGIN
      IF RunFlag
         THEN BEGIN PauseRun; ClearMkBuffers; CheckForEvents; END
         ELSE IF (Event.y<=RowHt) THEN HandleMenu;
   END;

   PROCEDURE EulerInit;
  {+------------------------------------------------------------------}
  {| This procedure initializes various global variables.
  {+------------------------------------------------------------------}
   VAR
      i,j:Integer;
      DrawAngles:EulerAngleType;
   BEGIN
      SetUpMenu;
      SetUpHotKeys;
      RunFlag := False;
      Sliders:= False;
     {+---------------------------------------------------------------}
     {| Initialize dynamical vectors and matrices.
     {+---------------------------------------------------------------}
      EulerVec.Init(3);
      RK4Vec.Init(6);
      rVec1.Init(3);
      rVec2.Init(3);
      Origin.Init(3);
      OmegaFixed.Init(3);
      OmegaBody.Init(3);
      TorqueFixed.Init(3);
      TorqueBody.Init(3);
      AngMomFixed.Init(3);
      AngMomBody.Init(3);
      InitOmega.Init(3);
      InitTorque.Init(3);
      Origin.Fill(0);
      DrawMat.Init(3,3);
      DrawMat2.Init(3,3);
      TempMat.Init(3,3);
      EulerMat.Init(3,3);
      InvEulerMat.Init(3,3);
     {+---------------------------------------------------------------}
     {| Set up program variables
     {+---------------------------------------------------------------}
      TwoViews := False;
      StereoGraphic := False;
      ProblemInit(FixedRotation);
     {+---------------------------------------------------------------}
     {| Set up constants and physics parameters
     {+---------------------------------------------------------------}
      t := 0; dt := 0.01;
      DegToRad := 2*Pi/360;
      RadToDeg := 1/DegToRad;
      PiT2 := Pi * 2;
      TimeDelay := 0;
      Mass := 1.0;
      CalcMomentofInertia;
     {+---------------------------------------------------------------}
     {| Generate perspective matrices.  The Euler angles:
     {| (-Pi/2, 0.57, 1.47) gives a nice orientation for DrawMat.
     {+---------------------------------------------------------------}
      WITH DrawAngles DO
         BEGIN
            Psi := -Pi/2;
            Theta := 0.57;
            Phi := +1.47;
         END;
      GenRotMat(DrawMat,DrawAngles);
      WITH DrawAngles DO BEGIN Psi := 0; Theta:=0; Phi:=-0.04; END;
      GenRotMat(TempMat,DrawAngles);
      DrawMat.MultMatrix(TempMat,DrawMat2);
      SetupGraphObjects;
   END;

BEGIN
   Anim.Init;
   CUPSinit;
   EulerInit;
   OpenPlot;
   ResetSystem;
   DoneFlag := FALSE;
   ShowHelp('Rotate.HLP','General Help');
      REPEAT
         CheckForEvents;
         IF event.mouseClicked THEN HandleMouse;
         IF HotKeys.Pressed(KeyHit) THEN HandleHotKey(KeyHit);
         IF Event.KeyPressed THEN PauseRun;
         IF Event.ExtendedKey AND (Event.ReadKey=';') THEN
            CASE Problem OF
               FixedRotation: ShowHelp('Rotate.HLP','FixedRotHelp');
               FixedTorque:   ShowHelp('Rotate.HLP','FixedTorHelp');
               SpinningTop:   ShowHelp('Rotate.HLP','SpinTopHelp');
            END; {CASE}


         IF RunFlag THEN GenMotion;
      UNTIL DoneFlag;
   CUPSdone;
END.


