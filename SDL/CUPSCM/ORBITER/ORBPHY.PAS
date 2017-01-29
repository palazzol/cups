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

	 {**********************************************}
	 {**********************************************}
	 {**            Unit OrbPhy.Pas               **}
	 {**          Program CentralF.Pas            **}
	 {**          Central Force Orbits            **}
	 {**        Written by Bruce Hawkins          **}
	 {**           January 21, 1992               **}
	 {**        Last change:  2/10/95             **}
	 {**             Version 1.00                 **}
	 {**                                          **}
	 {**      B. Hawkins Copyright @ 1992         **}
	 {**             CUPS Project                 **}
	 {**                                          **}
	 {**     Uses 12/95 MUPPET & CUPS Utilities   **}
	 {**********************************************}
	 {**********************************************}
{              Contains the Physics                      }
{   Does the main work of displaying the appropriate graphs    }
{        Initializes the parameters for each system           }

UNIT OrbPhy;

INTERFACE

USES CupsMupp, Graph, Cups, CupsFunc, CupsProc, CupsGui, CupsGrph, OrbPar;

{   Forces in form needed by Runge Kutta routine       }

PROCEDURE N_Body_Force(t: real; VAR Y,YP:DVector);
FUNCTION N_Body_Energy: Real;
PROCEDURE MildAnnounce(text : string);
Procedure ToFrame(WhichFrame : Integer; VAR BodyVector : DVector;
						Yin : DVector);
PROCEDURE Debounce;
Procedure ClearPlots(Temporary, ShowInit : Boolean); {Clears all the graphs}
Procedure Elements(Body, CenterBody : Integer;
						VAR Eccen, SemiMajor, Angle, ApAngle : Real;
						VAR Forwards : Boolean);
PROCEDURE SetNewPlace(XChosen, YChosen, VCx, VCy : Real; N : Integer);
PROCEDURE GetMass(Body : Integer);
PROCEDURE SetSystem;
PROCEDURE SetCartesianBodyParameters(Body, CenterBody : Integer;
				VAR Cancel : Boolean);
PROCEDURE SetBodyElements(Body, CenterBody : Integer; NewBody : Boolean;
				VAR Cancel : Boolean);
Procedure InitData;
Procedure ShowData(Erase : Boolean);
Procedure ThrustInit;
Procedure ShowInitialPositions;
PROCEDURE DoPoincare(XBodyN, YBodyN : Integer);
PROCEDURE DrawTrajectories;
PROCEDURE DrawBodies(VAR SightLineCount : Integer;
								 i3, i4, i5, i6, i12 : Integer);
Procedure DefineMyViewPort (Port : Integer; VPX1,VPX2,VPY1,VPY2: real);
Procedure SelectMyViewport(Port:Integer);
Procedure OpenMyViewPort (Port : Integer);
Procedure CloseMyViewPort (Port : Integer);
PROCEDURE ClearMyViewPort (Port : Integer);
PROCEDURE DrawCircle(x, y: Real; r: Integer; thisColor: Word);
Procedure ExtendVect(Vect : DVector); {Assumes length is an even number}
					  {Replicates first two values (X and Y) to rest of vector}
PROCEDURE ThreeBodyInit;
PROCEDURE GeneralInit;
PROCEDURE ReWrap;
PROCEDURE PhyInit;

VAR
	Eccentricity, ApAstron, PeriAstron, Period, ReducedM,
		Angular_Momentum : Real;

CONST   {Define ViewPort Numbers}

	{********  NOTE Ports with `Left' or `Right' in their names *********
						 Should not have their number changed without
						 taking note of the intialization section of the unit  }

	OneFull = 1; One = 2;
	LeftPort = 3; RightPort = 4;
	ChangePort = 5;
	ClearD1Port = 6; ClearD2Port = 7;
	ClearThrustPort = 8; AllPort = 9;
	SwapPort = 20;

	UpperLeft = 11; UpperMid = 12; UpperRight = 13;
	LowerLeft = 14; LowerMid = 15; LowerRight = 16;

	Mod1Port = 17; Mod2Port = 18;

	BigUpperLeft = 21; BigUpperRight = 22;
	BigLowerLeft = 23; BigLowerRight = 24;

	MidNamePort = 25; LowNamePort = 26; LowerNamePort = 27;
	ClearTimePort = 28; ClearVCircPort = 29;


VAR
	CofMPort, Body1Port, Body2Port, Body3Port, UnivPort, DataPort,
     PoincarePort, RotatePort, NamePort : Integer; {Viewports can change}

	MyViews : Array [21..30] of View;
   PlotList : Array [1..7] of Boolean;
   PortsUsed : Array[1..6] of Integer;
	ScaleUsed, XCenterUsed, YCenterUsed : Array[1..6] of Real;
   NamesUsed : Array[1..6] of String[35];
   FrameUsed : Array[1..6] of Integer;

   PortTable : Array[1..4] of Array[1..6] of Integer;

	Position0, Positions, SavePos : Array[1..7] of DVector;
	TempV, SaveY, InitY, RepInitY, NewInitY : DVector;

	t, SaveT : Real;
   key : Byte;
   TempColor : Integer;
   Y, YP, UnivVel : DVector;
   LightBodyEnergy : Real;

IMPLEMENTATION
CONST
	MyVersion = 1.00;

PROCEDURE N_Body_Force(t: real; VAR Y,YP:DVector);
VAR
   ihvy, j, Last : Integer;
   Xf, Yf : Real;
BEGIN
     {Velocities are 1st 2*NBodies derivatives}
	FOR j := 1 to N2Bodies Do YP.Put(j, Y.Value(j + N2Bodies));

   FOR ihvy := 1 to NHeavy DO
      BEGIN
      IF ihvy > NHeavyPrime THEN Last := NBodiesOld ELSE Last := NBodies;
      FOR j := 1 TO Last DO
        IF j <> ihvy THEN
        BEGIN
                  {Distances to Heavy Body ihvy from Body j}
				Xh[ihvy].Put(j, Y.Value(2*j-1)-Y.Value(2*ihvy-1));
				Yh[ihvy].Put(j, Y.Value(2 * j) - Y.Value(2 * ihvy));
				Rh_3[ihvy].Put(j, Pwr(Sqr(Xh[ihvy].Value(j)) 
								+ Sqr(Yh[ihvy].Value(j)), 1.5))
        END {For j}
      END; {For ihvy}
   Last := NHeavy;
	FOR j := 1 to NBodies DO
   BEGIN
      Xf:= 0.0; Yf := 0.0;
		IF j > NBodiesOld THEN Last := NHeavyPrime;
      FOR ihvy := 1 TO Last DO
      BEGIN
         IF (j <> ihvy) THEN
         BEGIN
				Xf := Xf - G * Xh[ihvy].Value(j) * M[ihvy] / Rh_3[ihvy].Value(j);
				Yf := Yf - G * Yh[ihvy].Value(j) * M[ihvy] / Rh_3[ihvy].Value(j)
         END
		END; {For j}
		YP.Put(N2Bodies + 2 * j - 1, Xf);
		YP.Put(N2Bodies + 2 * j, Yf);
   END; {For i}
END; {N_Body_Force}

FUNCTION N_Body_Energy: Real;
VAR
	MLight, MhEnergy : Double;
	ihvy, j : Integer;
BEGIN
	{Calculate total energy of system}
   MhEnergy := 0.0;
   FOR ihvy := 1 TO NHeavy DO
   BEGIN
      {Add KE of heavy bodies}
		MhEnergy := MhEnergy + M[ihvy] * (Sqr(Y.Value(N2Bodies + 2 * ihvy - 1))
						+ Sqr(Y.Value(N2Bodies + 2 * ihvy))) * 0.5;
      FOR j := 1 to ihvy-1 DO  {Add mutual PE of all heavy bodies}
				MhEnergy := MhEnergy - G * M[ihvy] * M[j]
								/ Sqrt(Sqr(Y.Value(2*ihvy-1)- Y.Value(2*j-1))
										+Sqr(Y.Value(2*ihvy)  - Y.Value(2*j)))
	END; {For ihvy}
	N_Body_Energy := MhEnergy;

   {Calculate energy of designated single body, KE first}
   IF ShowEnergyBody > 0 THEN
   BEGIN
		IF ShowEnergyBody > NHeavy THEN MLight := M[NHeavy]*0.001
			ELSE MLight := M[ShowEnergyBody];
	   LightBodyEnergy := MLight*(Sqr(Y.Value(N2Bodies + 2*ShowEnergyBody- 1))
						+ Sqr(Y.Value(N2Bodies + 2 * ShowEnergyBody))) * 0.5;
   	FOR j := 1 to NHeavy DO  {Add mutual PE of all heavy bodies}
      	IF j <> ShowEnergyBody THEN
				LightBodyEnergy := LightBodyEnergy - G * MLight * M[j]
						/ Sqrt(Sqr(Y.Value(2*ShowEnergyBody-1)- Y.Value(2*j-1))
						+Sqr(Y.Value(2*ShowEnergyBody)  - Y.Value(2*j)))
   END {Calculate single body energy}
END; {N_Body_Energy}

PROCEDURE MildAnnounce(text : string);
VAR
   dx,dy,cx,cy,a,n : integer;
   image           : TImage;
   z               : char;
   envir           : TEnvironment;
BEGIN
	envir.Save;
{	Message('Click or Press ENTER, then follow instructions.');  }
	envir.Standardize; 
	SetFillStyle(SolidFill,Brown);
	dx := TextWidth(text)+13; 
	dy := RowHt + 3; 
	cx := (GetMaxX-dx) DIV 2; 
	cy := (GetMaxY-dy) DIV 2; 
	HideCursor;
	Image.Get(cx,cy,cx+dx,cy+dy); 
	Bar3D(cx,cy+3,cx+dx-3,cy+dy,3,true);
	ClearMKBuffers;
	SetColor(White); 
	OutTextXY(cx+6,cy+8,Text);
	ShowCursor;
	DynamicPause;
	image.put(cx,cy);
	envir.Reset;  
END;

Procedure DefineMyViewPort (Port : Integer; VPX1,VPX2,VPY1,VPY2: real);
BEGIN
	IF Port < SwapPort THEN DefineViewPort (Port, VPX1,VPX2,VPY1,VPY2)
     ELSE
     BEGIN
     DefineViewPort (SwapPort, VPX1,VPX2,VPY1,VPY2);
	  MyViews[Port] := Views[SwapPort]
     END
END; {DefineMyViewPort}

Procedure SelectMyViewport(Port:Integer);
BEGIN
   IF Port < SwapPort THEN SelectViewPort (Port)
     ELSE
     BEGIN
     Views[SwapPort] := MyViews[Port];
     SelectViewPort (SwapPort)
     END
END; {SelectMyViewPort}

Procedure OpenMyViewPort (Port : Integer);
BEGIN
	IF Port < SwapPort THEN OpenViewPort (Port)
     ELSE
     BEGIN
	  Views[SwapPort] := MyViews[Port];
     OpenViewPort (SwapPort)
     END
END; {OpenMyViewPort}

Procedure CloseMyViewPort (Port : Integer);
BEGIN
	IF Port < SwapPort THEN
     BEGIN
       CloseViewPort (Port);
       SelectViewPort (Port)
	  END
     ELSE
     BEGIN
        Views[SwapPort] := MyViews[Port];
        CloseViewPort (SwapPort);
		  SelectViewPort (SwapPort) {Because we often want to clear a port and use it}
     END
END; {CloseMyViewPort}

PROCEDURE ClearMyViewPort (Port : Integer);
BEGIN
	SetColor(Blue);
	SetFillStyle(1,blue);
	SelectMyViewPort(Port);
	IF Port < SwapPort THEN
		WITH Views[Port] DO bar (0,0,Vx2-Vx1,Vy2-Vy1)
	ELSE WITH MyViews[Port] DO bar (0,0,Vx2-Vx1,Vy2-Vy1)
END;

PROCEDURE DrawCircle(x, y: Real; r: Integer; thisColor: Word);
VAR
   xSc, ySc, x2Sc, y2Sc: integer;
BEGIN
   Map(x,y, xsc,ysc);
   SetFillStyle(solidfill,ThisColor);
   FillEllipse(xSc,ySc,r,r);
END;

Procedure ExtendVect(Vect : DVector); {Assumes length is an even number}
VAR              {Replicates first two values (X and Y) to rest of vector}
	i, imax : Integer;
   V1, V2 :Real;
BEGIN
	I := 1; V1 := Vect.Value(i); i := 2; V2 := Vect.Value(i);
	i := 3; imax := Vect.Length;
	REPEAT
		Vect.Put(i, V1);
		Vect.Put(i + 1, V2);
		i := i + 2
	UNTIL i > imax
END; {ExtendVect}

Procedure ToRotFrame(VAR BodyVector : DVector; Yin : DVector);
VAR
	i, j, imax :integer;
   X21, Y21, R21, CosTh, SinTh : Real;
BEGIN
   i := 3; j := 1; {Necessary to fool compiler about DVectors}
	X21 := Yin.Value(i) - Yin.Value(j);
   i := 4; j := 2;
	Y21 := Yin.Value(i) - Yin.Value(j);
	R21 := Sqrt(Sqr(X21) + Sqr(Y21));
   CosTh := Y21/R21;
   SinTh := X21/R21;
	i := 1; imax :=  2 * Nbodies;
   REPEAT
		BodyVector.Put(i, Yin.Value(i) * SinTh + Yin.Value(i + 1) * CosTh);
		BodyVector.Put(i + 1, -Yin.Value(i) * CosTh + Yin.Value(i + 1) * SinTh);
		i := i + 2
		UNTIL i > imax
END; {ToRotFrame}

Procedure ToFrame(WhichFrame : Integer; VAR BodyVector : DVector;
						Yin : DVector);
VAR
	i1, i2 : Integer;
BEGIN
	i1 := 1; i2 := 2;
	Case WhichFrame of
		1 :     BodyVector.Equate(1.0, Yin);
		2,3,4 : BEGIN TempV.Put(i1, Yin.Value(2 * (WhichFrame - 1) - 1));
				  TempV.Put(i2, Yin.Value(2 * (WhichFrame - 1))) END;
		5 :     BEGIN
				  TempV.Put(1,t* UnivVel.Value(1));
				  TempV.Put(2,t* UnivVel.Value(2))
				  END;
		6 :     ToRotFrame(BodyVector, Yin);
		7 :     ToRotFrame(BodyVector, Yin)
	END; {Case WhichFrame}
	If (WhichFrame > 1) AND (WhichFrame < 6) THEN
      BEGIN
		ExtendVect(TempV); {Contains X and Y of body}
      BodyVector.Equate(1.0, Yin);   {Get current body positions}
		BodyVector.AddVector(-1.0, TempV) {Subtract position of given body}
      END {If WhichFrame ..}
END; {ToFrame}

PROCEDURE DrawScale(WhichFrame : Integer);
VAR
   ScaleMarker, ThisScale, ScaleEnd, CenterX, CenterY : Real;
   Xs, Ys, Size, Decimals : Integer;
BEGIN
	ThisScale := ScaleUsed[WhichFrame];
	ScaleMarker := 1.0e20;
	While ScaleMarker > ThisScale DO ScaleMarker := ScaleMarker/10.0;
	Size := 7; Decimals := -1;
	IF (ScaleMarker < 2000) AND (ScaleMarker >= 0.0009) THEN
	BEGIN
		Size := 6; Decimals := 0;
		IF ScaleMarker > 900.0 THEN Size := 5
		ELSE IF ScaleMarker > 90.0 THEN Size := 4
		ELSE IF ScaleMarker > 9.0 THEN Size := 3
		ELSE IF ScaleMarker > 0.9 THEN Size := 2
      ELSE IF ScaleMarker > 0.09 THEN Decimals := 1
      ELSE IF ScaleMarker > 0.009 THEN Decimals := 2
		ELSE IF ScaleMarker > 0.0009 THEN Decimals := 3
   END; {IfScaleMarker < 2000 ..}
	CenterX := XCenterUsed[WhichFrame];
	CenterY := YCenterUsed[WhichFrame];
   SelectScale(WhichFrame);
	ScaleEnd := ScaleMarker/2.0;
	SetColor(LightGray);
	PlotLine(-ScaleEnd+CenterX,-0.8*ThisScale+CenterY,
             -ScaleEnd+CenterX,-0.8*ThisScale+CenterY);
	PlotLine(-ScaleEnd+CenterX,-0.7*ThisScale+CenterY,
             -ScaleEnd+CenterX,-0.9*ThisScale+CenterY);
	PlotLine(ScaleEnd+CenterX,-0.7*ThisScale+CenterY,
             ScaleEnd+CenterX,-0.9*ThisScale+CenterY);
   Map(ScaleEnd+CenterX,-0.8*ThisScale+CenterY, Xs,Ys);
	OutTextXY(Xs,Ys,ConCat(NumStr(ScaleMarker,Size,Decimals),' ',LengthName))
END; {DrawScale}

PROCEDURE Debounce;
BEGIN
	REPEAT CheckForEvents UNTIL Not Event.MouseClicked;
END; {Debounce}

Procedure ClearPlots(Temporary, ShowInit : Boolean);
VAR
	i, Frame : Integer;
	ThisScale : Real;
BEGIN
	HideCursor;
   GraphBackColor := Blue;
   If Not Temporary THEN OpenMyViewPort (AllPort);
   GraphBackColor := Black;

   For i := 1 to Plots Do
	BEGIN
      SetColor(White);
		OpenMyViewPort(PortsUsed[i]);
      PutLabel(Inside,NamesUsed[i]);
		ThisScale := ScaleUsed[i];
		DefineScale(i, -Zf*ThisScale+XCenterUsed[i],
                             Zf*ThisScale+XCenterUsed[i],
				-ThisScale+YCenterUsed[i], ThisScale+YCenterUsed[i]);
      DrawScale(i)
   END; {For i}

   NColor := 2;
   IF Not Temporary THEN
	BEGIN
      N_Body_Force(0.0, Y, YP); {Initialize variables for energy calculation}
		InitData;
	   IF ThrustAllowed AND (Plots <= 2) THEN ThrustInit
   END;
   IF ShowInit THEN ShowInitialPositions;
   ShowCursor
END; {ClearPlots}

VAR
	EcCol, PeriCol, ApeCol, AngCol, EnCol, EnLCol, TCol, ErrCol,
	EcRow, PeriRow, ApeRow, AngRow, EnRow, EnLRow, TRow, ErrRow,
	NameCol, NameRow, ClrCol, ClrRow, TSizRow, TSizCol : Integer;
	Hcol, HRow, TimRow, TimCol : Integer;
	ClearPort : Integer;

Procedure Elements(Body, CenterBody : Integer;
						VAR Eccen, SemiMajor, Angle, ApAngle : Real;
						VAR Forwards : Boolean);
VAR
	Ener, Rzero, R, V, RTheta, VTheta, Dx, Dy, Mass : Real;
BEGIN
	If Body > NHeavy THEN Mass := 0.001*M[NHeavy]
		ELSE Mass := M[Body];
	Dx := Y.Value(2*Body-1)-Y.Value(2*CenterBody-1);
	Dy := Y.Value(2*Body)-Y.Value(2*CenterBody);
	R := Sqrt(Sqr(Dx) + Sqr(Dy));
	IF R = 0.0 THEN RTheta := 0.0
	ELSE RTheta := ArcTan2(Dx, Dy)*180/Pi;
	IF RTheta = 360.00 THEN RTheta := 0.0;
	Dx := Y.Value(N2Bodies+2*Body-1)-Y.Value(N2Bodies+2*CenterBody-1);
	Dy := Y.Value(N2Bodies+2*Body)-Y.Value(N2Bodies+2*CenterBody);
	V := Sqrt(Sqr(Dx) + Sqr(Dy));
	IF V = 0.0 THEN VTheta := 0.0
	ELSE VTheta := ArcTan2(Dx,Dy)*180/Pi;
	IF VTheta = 360.00 THEN VTheta := 0.0;
		ReducedM := M[CenterBody]*Mass/(m[CenterBody] + Mass);
   IF ReducedM = 0 THEN ReducedM := 0.0001;
	Ener := (ReducedM*Sqr(V)/2 - G*m[CenterBody]*Mass/R);
	SemiMajor := G*M[CenterBody]*Mass/(2*Abs(Ener));
	Eccen := Sqrt(1+(2*ReducedM*Ener*Sqr(R*V*sin((VTheta-RTheta)*Pi/180))
						/Sqr(G*M[CenterBody]*Mass)));
	Rzero := Abs(SemiMajor*(1-Eccen));
	Angle := ((Rzero/R)*(1+Eccen)-1)/Eccen;
	If Angle > 1.0 Then Angle := 1.0;
   If Angle <-1.0 Then Angle :=-1.0;
	Angle := ArcCos(Angle)*180/Pi;
	If Abs(VTheta-RTheta) > 90 THEN Angle := 360 - Angle;
	ApAngle := RTheta - Angle;
	IF (((VTheta < Rtheta) AND ((VTheta - RTheta) > -180))) OR
		(VTheta > (RTheta + 180)) THEN
		Forwards := False ELSE Forwards := True
END; {Elements}

PROCEDURE SetNewPlace(XChosen, YChosen, VCx, VCy : Real; N : Integer);
VAR      {Calculates Center of Mass Position & revises others}
	XCofM, YCofM, VxCofM, VyCofM, MCofM : Real;
	i : Integer;
BEGIN
	Y.Put(2*N-1,XChosen); Y.Put(2*n,YChosen);
	Y.Put(N2Bodies+2*N-1,VCx); Y.Put(N2Bodies+2*N,VCy);
	MCofM := 0.0; XCofM := 0.0; YCofM := 0.0; VxCofM := 0.0; VyCofM := 0.0;
	FOR i := 1 to NHeavy DO {Calculate CofM position & Velocity}
	BEGIN
		MCofM := MCofM + M[i];
		XCofM := XCofM + Y.Value(2*i-1)*M[i];
		YCofM := YCofM + Y.Value(2*i)*M[i];
		VxCofM := VxCofM + Y.Value(N2Bodies+2*i-1)*M[i];
		VyCofM := VyCofM + Y.Value(N2Bodies+2*i)*M[i]
	END; {FOR i}
	XCofM := XCofM/MCofM; YCofM := YCofM/MCofM;
	VxCofM := VxCofM/MCofM; VyCofM := VyCofM/MCofM;
	FOR i := 1 to NHeavy DO
	BEGIN
		Y.Put(2*i-1,Y.Value(2*i-1)-XCofM);
		Y.Put(2*i,Y.Value(2*i)-YCofM);
		Y.Put(N2Bodies+2*i-1,Y.Value(N2Bodies+2*i-1)-VxCofM);
		Y.Put(N2Bodies+2*i,Y.Value(N2Bodies+2*i)-VyCofM)
	END;
	NewInitY.Resize(4*NBodies); NewInitY.Equate(1.0, Y);
	UnivVel.put(1, VxCofM);
	UnivVel.put(2, VyCofM);
	ExtendVect(UnivVel);
END; {SetNewPlace}

PROCEDURE GetMass(Body : Integer);
VAR
	dataScreen    : TInputScreen;   { input screen               }
BEGIN
 WITH DataScreen DO
 BEGIN
	IF Body <= MaxHeavy THEN
	BEGIN
		Init;
		LoadLine ('      Mass of Next Body?');
		LoadLine ('');
		LoadLine (ConCat('  Mass: {            } ',MassName));
		SetNumber(1,M[Body]);
		LoadLine ('');
		LoadLine ('[  OK  ]    [Cancel]');
		DefineInputPort(0.1,0.5,0.55,0.8);
		AcceptScreen;
		IF Not Canceled THEN M[Body] := GetNumber(1);
      Done
	END
  END
END;

PROCEDURE SetSystem;
VAR
	SysScreen : TInputScreen;
   i : Integer;
BEGIN
	WITH SysScreen DO
	BEGIN
		Init;                                                          
		LoadLine(' System Name: "                              "');
		LoadLine (Concat(' Width of Plot=  {       }  ',LengthName));
		LoadLine ('       [  OK  ]    [Cancel]');
		SetNumber (2,AllNormScale);
		DefineInputPort(0.1,0.8,0.65,0.8);
		AcceptScreen;
		IF Not Canceled THEN
		BEGIN
			SysName := GetString(1);
			AllNormScale := GetNumber(2);
			ScaleTable[7] := AllNormScale;
			FOR i := 1 to 6 DO
			BEGIN
				ScaleUsed[i] := AllNormScale;
				ScaleTable[i] := AllNormScale;
			END {FOR i}
		END; {Not Canceled}
		Done;
	END
END; {SetSystem}

PROCEDURE SetCartesianBodyParameters(Body, CenterBody : Integer;
				VAR Cancel : Boolean);
VAR
	dataScreen    : TInputScreen;   { input screen               }
	R, RTheta, V, VTheta : Real;
	i : Integer;
	Name : String[35];
BEGIN
	WITH DataScreen DO
	BEGIN
		Init;
		R := Sqrt(Sqr(Y.Value(2*Body-1)) + Sqr(Y.Value(2*Body)));
		V := Sqrt(Sqr(Y.Value(N2Bodies+2*Body-1)) + Sqr(Y.Value(N2Bodies+2*Body)));
      IF R = 0.0 THEN RTheta := 0.0
      ELSE RTheta := ArcTan2(Y.Value(2*Body-1),Y.Value(2*Body))*180/Pi;
		IF RTheta = 360.00 THEN RTheta := 0.0;
		IF V = 0.0 THEN VTheta := 0.0
      ELSE VTheta := ArcTan2(Y.Value(N2Bodies+2*Body-1),
						 Y.Value(N2Bodies+2*Body))*180/Pi;
      IF VTheta = 360.00 THEN VTheta := 0.0;
		IF (Not AddingBody) AND (Body > NHeavy) AND (Body <= MaxHeavy) THEN
		   M[Body] := 0.0;
		Name := Nametable[Body];
		i := Length(Name);
      While Name[i] = ' ' DO
		BEGIN
			Delete(Name,i,1);
			i := Length(Name)
		END;
		DefineInputPort(0.02, 0.98, 0.35, 0.83);
		LoadLine (ConCat('       ',SysName));
		LoadLine (ConCat('       Parameters for Body ',NumStr(Body,2,0)));
		LoadLine (' ');
      LoadLine ('  Name: "                 "');
		LoadLine (' ');
		LoadLine ('        INITIAL VALUES: ');
		LoadLine (ConCat(' Distance from Center of Mass= {        } ',
					LengthName,'    at {      } degrees'));
		LoadLine (Concat ('  Velocity relative to ',NameTable[CenterBody],
								' = {      } ',
							LengthName,'/',TimeName,' at {      } degrees'));
		SetNumber(2,R); SetNumber(3,RTheta);
		SetNumber(4,V); SetNumber(5,VTheta);
		LoadLine ('');
		LoadLine ('       [  OK  ]    [Cancel]');
		SetString(1, Name);
		Message('Press <ENTER> to accept changes, <ESC> to leave unchanged');
		REPEAT
		AcceptScreen;
		If Canceled THEN Cancel := True
		ELSE
		BEGIN
			Name := GetString(1);
			While Name[Length(Name)] = ' ' DO Delete(Name, Length(Name), 1);
			Nametable[Body] := Name;
			R      := GetNumber(2);
			RTheta := GetNumber(3)*Pi/180;
			V      := GetNumber(4);
			VTheta := GetNumber(5)*Pi/180;
			Y.Put(N2Bodies+2*Body-1,V*Cos(VTheta));
			Y.Put(N2Bodies+2*Body,V*Sin(VTheta));
			Y.Put(2*Body-1,R*Cos(RTheta));
			Y.Put(2*Body,R*Sin(RTheta));
			SetNewPlace(Y.Value(2*Body-1), Y.Value(2*Body),
						Y.Value(N2Bodies+2*Body-1), Y.Value(N2Bodies+2*Body), Body);
			END; {IF Not Canceled}
			IF Name = '' THEN
			BEGIN
				IF Not Canceled THEN
				MildAnnounce(
					  'You MUST give the body a name! Click Mouse to go back.')
				ELSE
				BEGIN
					Name := 'X';
					Nametable[Body] := Name
				END
			END;
			Debounce
		UNTIL (Name <> '');
		Done;
	END; {With Screen}
	Debounce;
END; {SetCartesianBodyParameters}

PROCEDURE SetBodyElements(Body, CenterBody : Integer; NewBody : Boolean;
				VAR Cancel : Boolean);
VAR
	dataScreen    : TInputScreen;   { input screen               }
	R, RTheta, V, VTheta, Eccen, SemiMajor, Angle, ApAngle,
	 R0, Vtan, Vr, Energy, Mass : Real;
	i : Integer;
	Forw : Boolean;
	Name : String[35];
BEGIN
	WITH DataScreen DO
	BEGIN
		Init;
		R := Sqrt(Sqr(Y.Value(2*Body-1)) + Sqr(Y.Value(2*Body)));
		V := Sqrt(Sqr(Y.Value(N2Bodies+2*Body-1)) + Sqr(Y.Value(N2Bodies+2*Body)));
      IF R = 0.0 THEN RTheta := 0.0
      ELSE RTheta := ArcTan2(Y.Value(2*Body-1),Y.Value(2*Body))*180/Pi;
      IF RTheta = 360.00 THEN RTheta := 0.0;
      IF V = 0.0 THEN VTheta := 0.0
      ELSE VTheta := ArcTan2(Y.Value(N2Bodies+2*Body-1),
						 Y.Value(N2Bodies+2*Body))*180/Pi;
		IF VTheta = 360.00 THEN VTheta := 0.0;
		IF NOT NewBody THEN
			Elements(Body, CenterBody, Eccen, SemiMajor, Angle, ApAngle, Forw);
		IF (Not AddingBody) AND (Body > NHeavy) AND (Body <= MaxHeavy) THEN
			M[Body] := 0.0;
      Name := Nametable[Body];
      While Pos(' ',Name) > 0 DO Delete(Name,Pos(' ',Name),1);
		DefineInputPort(0.02, 0.98, 0.35, 0.83);
		LoadLine (ConCat('       ',SysName));
      LoadLine (ConCat('       Parameters for Body ',NumStr(Body,2,0)));
		LoadLine (' ');
		LoadLine (' Name: "                 "');
		LoadLine (ConCat(' Semimajor axis =  {        } ',
			LengthName,' relative to ',NameTable[CenterBody]));
		LoadLine (ConCat(' Eccentricity   = {       },',
			'    Angle at t=0 = {       } degrees.'));
		LoadLine (' Periapse (closest approach) at {        } degrees to x axis.');
		LoadLine (' #T Motion Counterclockwise');
		SetNumber(2,SemiMajor);
		SetNumber(3,Eccen); SetNumber(4,Angle);
		SetNumber(5,ApAngle);
		SetBoolean(6,Forw);
		LoadLine ('');
		LoadLine ('       [  OK  ]    [Cancel]');
		SetString(1, Name);
		Message('Press <ENTER> to accept changes, <ESC> to leave unchanged');
		REPEAT
		AcceptScreen;
		If Canceled THEN Cancel := True
		ELSE
		BEGIN
		Name := GetString(1);
		While Name[Length(Name)] = ' ' DO Delete(Name, Length(Name), 1);
		Nametable[Body] := Name;
		SemiMajor := GetNumber(2);
		Eccen := GetNumber(3);
		Angle := GetNumber(4);
		ApAngle := GetNumber(5);
		RTheta := Angle + ApAngle;
		R0 := SemiMajor*(1-Eccen);
		IF Eccen = 1 THEN R0 := SemiMajor; {anomalous case}
		R := R0*(1+Eccen)/(1+Eccen*Cos(Angle*Pi/180));
		Mass := M[Body];
		IF Mass =  0 THEN Mass := 0.001*M[NHeavy];
		IF Body <= MaxHeavy then
			ReducedM := M[CenterBody]*Mass/(M[CenterBody] + Mass);
		Energy := -G*M[CenterBody]*Mass/(2*SemiMajor);
		V := Sqrt(2*(Energy/ReducedM+G*M[CenterBody]*Mass/(R*ReducedM)));
		Vtan := (G*M[CenterBody]*Mass/R)
					*Sqrt((Sqr(Eccen)-1)/(2*Energy*ReducedM));
		Vr := Sqrt(Sqr(V)-Sqr(Vtan));
		IF Angle > 180 THEN Vr := - Vr;
		IF Not GetBoolean(6) THEN
		BEGIN Vtan := - Vtan END;
		VTheta := ArcTan2(Vr,Vtan); {Relative to R}
		RTheta := RTheta*Pi/180;
		VTheta := RTheta + VTheta;
		Y.Put(N2Bodies+2*Body-1,
					Vr*Cos(RTheta)-Vtan*Sin(RTheta)
					+Y.Value(N2Bodies+2*CenterBody-1));
		Y.Put(N2Bodies+2*Body,
					Vr*Sin(RTheta)+Vtan*Cos(RTheta)
					+Y.Value(N2Bodies+2*CenterBody));
		Y.Put(2*Body-1,R*Cos(RTheta)+Y.Value(2*CenterBody-1));
		Y.Put(2*Body,R*Sin(RTheta)+Y.Value(2*CenterBody));
		SetNewPlace(Y.Value(2*Body-1), Y.Value(2*Body),
				Y.Value(N2Bodies+2*Body-1), Y.Value(N2Bodies+2*Body), Body);
		END; {IF Not Canceled}
		IF Name = '' THEN
		BEGIN
			IF Not Canceled THEN
			MildAnnounce('You MUST give the body a name! Click Mouse to go back.')
			ELSE
			BEGIN
				Name := 'X';
				Nametable[Body] := Name
			END
		END;
		Debounce
		UNTIL (Name <> '');
		Done;
	END; {With Screen}
	Debounce;
END; {SetBodyElements}

Procedure InitData;
BEGIN
  If Not Zoomed THEN BEGIN
	Case Plots of
		1: BEGIN
			GraphBackColor := Blue;
			SetColor(Blue);
			OpenMyViewPort(ChangePort);
			DataPort := ChangePort;
			ClearPort := Mod2Port;
			EcRow := 3; PeriRow := 4; ApeRow := 5; AngRow := 6;
			EnRow := 11; EnLRow := 12; TRow := 7; TimRow := 10;
			EcCol := 1; PeriCol := 1; ApeCol := 1; AngCol := 1;
			EnCol := 1; EnLCol := 1; TCol := 1; TimCol := 1;
			NameRow := 1; NameCol := 1; ClrRow := 8; ClrCol := 1;
         TSizRow := 9; TSizCol := 1;
			END;
		2,4,6: BEGIN
			DataPort := AllPort;
			ClearPort := Mod1Port;
			IF ThrustAllowed THEN ClearPort := ClearThrustPort;
			NameRow := 1; NameCol := 30;
			ClrRow := 2; ClrCol := 55;
			EcRow := 1; PeriRow := 1; ApeRow := 1;
			AngRow := 2; EnRow := 21; EnLRow := 21;
			TRow := 2; TimRow := 21;
         TSizRow := 3; TSizCol := 55;
			EcCol := 5; PeriCol := 30; ApeCol := 55;
			AngCol := 5; EnCol := 30; EnLCol := 55;
			TCol := 30; TimCol := 5;
			END;
		END; {Case}
		IF Plots = 2 THEN NameRow := 3
	END {If Not Zoomed}
	ELSE BEGIN
		DataPort := AllPort;
		ClearPort := Mod1Port;
		NameRow := 1; NameCol := 30;
		EnRow := 21; EnLRow := 21;
		TRow := 2; TimRow := 21;
		EnCol := 30; EnLCol := 55;
		TCol := 30; TimCol := 5
	END; {ELSE Zoomed}
	IF Not AddingBody THEN ShowData(False)
END; {InitData}

Procedure ModData;
VAR X1s,Y1s,X2s,Y2s : Integer;
	 TempS : String[15];
    Count : Integer;
BEGIN
	GraphBackColor := Blue;
	SelectMyViewPort(DataPort);
	SetColor(White);
   Count := 0;
	SetColor(Blue);
   While Count < 2 DO
	BEGIN
		Print(EcCol+6,EcRow,Num2Str (SavT, 5));
		Print(TCol,TRow,ConCat(Chr(18),Num2str(SavVT,5),',  ',
									Chr(29),Num2str(SavHT,5)));
		SavVT := VThrust; SavHT := HThrust; SavT := THrust;
		SetColor(LightRed);
      Count := Count + 1
   END;
	SetColor(White)
END; {ModData}

Procedure ShowData(Erase : Boolean);
VAR
	NEn, NLEn, Nt,
	SemiMajor, Angle, ApAngle, V0, Mass, SavEccen : Real;
	Forw : Boolean;
	Count : Integer;
	TempS, TempS2 : String[15];
BEGIN
	GraphBackColor := Blue;
	SelectMyViewPort(DataPort);
	SetColor(White);
	NEn := SaveEnergy;
	NLEn := SaveLBodyEnergy;
	Nt := SaveNt;
	Count := 1;
	IF Not Erase THEN
	BEGIN  {Print the caption information}
		Print(NameCol,NameRow, SysName);
		If (Plots <= 2) AND Not Zoomed THEN
		IF NOT ThrustAllowed THEN
		BEGIN
			Print(EcCol,EcRow,'Eccent. = ');
			Print(PeriCol,PeriRow,ConCat(PeriName,' =         ',LengthName));
			Print(ApeCol,ApeRow,ConCat(ApeName,' =          ',LengthName));
			Print(AngCol,AngRow,'Ang.Mom.=  ');
			Print(TCol,TRow,ConCat('Period  =        ',TimeName));
			Print(ClrCol,ClrRow,Concat('Elements of ',NameTable[DataBody]))
		END; {If Plots < 2}
		Print(EnCol,EnRow,'Energy  = ');
		Print(EnLCol,EnLRow,ConCat(LightEnergyName,' = '));
		Print(TimCol,TimRow,ConCat('Time    =       ',TimeName))
	END; {not Erasing}
	IF ThrustAllowed THEN ModData
	ELSE
	IF (Plots <= 2) And (Not Zoomed) THEN
	BEGIN {Print old value in blue (erase), then new one in white}
		SavEccen := Eccentricity; {Do all calcluations first to reduce flicker}
		Elements(DataBody, DataCenterBody,
		Eccentricity, SemiMajor, Angle, ApAngle, Forw);
		SetColor(Blue);
		Print(EcCol+10,EcRow,Num2Str(SavEccen,6));
		SetColor(White);
		Print(EcCol+10,EcRow,Num2Str(Eccentricity,6));
			TempS := Num2Str(PeriAstron, 6);
		IF Eccentricity < 1 THEN
			PeriAstron := Abs(SemiMajor*(1-Eccentricity))
		ELSE PeriAstron := 1.0E30;
		TempS2 := Num2Str(PeriAstron, 6);
		SetColor(Blue);
		Print(PeriCol+11,PeriRow,TempS);
		SetColor(White);
		Print(PeriCol+11,PeriRow,TempS2);
		TempS := Num2Str(ApAstron, 6);
		IF Eccentricity <> 1 THEN
			ApAstron := Abs(SemiMajor*(1+Eccentricity))
		ELSE ApAstron := SemiMajor;
		TempS2 := Num2Str(ApAstron, 6);
		SetColor(Blue);
		Print(ApeCol+11,ApeRow,TempS);
		SetColor(White);
		Print(ApeCol+11,ApeRow,TempS2);
			TempS := Num2Str(Angular_Momentum, 6);
		V0 := Sqrt(G*M[DataCenterBody]*(Eccentricity+1)/ApAstron);
		Angular_Momentum := ApAstron*V0;
		TempS2 := Num2Str(Angular_Momentum, 6);
		SetColor(Blue);
		Print(AngCol+11,AngRow,TempS);
		SetColor(White);
		Print(AngCol+11,AngRow,TempS2);
			TempS := Num2Str(Period, 6);
		If DataBody > NHeavy THEN Mass := 0.001*M[NHeavy]
			ELSE Mass := M[DataBody];
		IF Eccentricity < 1 THEN
			Period := TwoPi*Pwr((ApAstron+PeriAstron)/2,1.5)
						/Sqrt(G*M[DataCenterBody]*Mass/ReducedM)
		ELSE Period := 1.0E30;
		TempS2 := Num2Str(Period, 6);
		SetColor(Blue);
		Print(TCol+10,TRow,TempS);
		SetColor(White);
		Print(TCol+10,TRow,TempS2);
	END; {If Plots < 2 & No Thrust}
	TempS := Num2Str(NEn, 10);
	NEn := N_Body_Energy;
	TempS2 := Num2Str(NEn, 10);
	SetColor(Blue);
	Print(EnCol+10,EnRow,TempS);
	SetColor(White);
	Print(EnCol+10,EnRow,TempS2);

	SetColor(Blue);
	TempS := Num2Str(NLEn,10);
	NLEn := LightBodyEnergy;
	TempS2 := Num2Str(NLEn,10);
	Print(EnLCol+13,EnLRow,TempS);
	SetColor(White);
	Print(EnLCol+13,EnLRow,TempS2);

	SetColor(Blue);
	Print(TimCol+10,TimRow,Num2Str(Nt,7));
	SetColor(White);
	Nt := t;
	Print(TimCol+10,TimRow,Num2Str(Nt,7));

	SaveEnergy := NEn;
	SaveLBodyEnergy := NLEn;
	SaveNt := Nt;
END; {ShowData}

Procedure ThrustInit;
BEGIN
	GraphBackColor := Blue;
	ClearMyViewPort(ClearPort);
	SelectMyViewPort(DataPort);
	SetColor(White);
	Print(EcCol,EcRow,'Apply        units of thrust ');
	Print(PeriCol+3,PeriRow,ConCat(' with ',Chr(24),Chr(25),Chr(26),Chr(27),' keys'));
	Print(ApeCol,ApeRow,Concat('direction of motion is ',Chr(24)));
	Print(AngCol,AngRow,'Thruster firing rate: ');
	Print(ClrCol,ClrRow,'[Home key clears]');
   Print(TSizCol,TSizRow,'Pg Up,Dn change thrust');
	ModData
END;

PROCEDURE DrawCofM(WhichFrame :Integer; Erase : Boolean);
VAR  Gx1,Gy1 :integer;
	  XCM, YCM : Real;
CONST Size = 2;
BEGIN  
   IF Erase THEN 
   BEGIN
     XCM := XCMSav[WhichFrame]; YCM := YCMSav[WhichFrame];
     SetColor(Black)
   END
   ELSE
   BEGIN
      XCM := 0.0; YCM := 0.0;
      Case WhichFrame of
			2,3,4 : BEGIN
							XCM := -Y.Value(2 * (WhichFrame - 1) - 1);
							YCM := -Y.Value(2 * (WhichFrame - 1))
					  END;
			5 : BEGIN
					 XCM := t*UnivVel.Value(1);
					 YCM := t*UnivVel.Value(2)
              END;
      END; {Case WhichFrame}
      SetColor(White);
   END; {ELSE}
   Map(XCM,YCM,Gx1,Gy1);
   Line(Gx1-Size,Gy1,Gx1+Size,Gy1);
   Line(Gx1,Gy1-Size,Gx1,Gy1+Size);
   XCMSav[WhichFrame] := XCM; YCMSav[WhichFrame] := YCM;
END;

Procedure ShowInitialPositions;
VAR
	iplot, iframe, i,j, k, n, SavDot : Integer;
	Name : String[16];
BEGIN
	TempV.Init(4*NBodies);
	For iplot := 1 to Plots DO
	BEGIN
      iframe := FrameUsed[iplot];
		IF iframe <> 7 THEN {Not in poincare}
		BEGIN
			SavePos[iframe].init(NBodies*4);
			ToFrame(iframe, SavePos[iframe],Y);
         SelectMyViewPort(PortsUsed[iplot]);
         SelectScale(iplot);
			IF ShowCofM THEN DrawCofM(iframe, {Erase=} False);
			i := 1; j := 1; k := 2;  {Used to minimize calculations in inner loop}
			REPEAT
			  SetColor(ColorB[i]);
			  SavDot := DotSize;
			  IF (SysName = 'Shuttle Docking') AND (i = 1) THEN
				DotSize := 78;
			  DrawCircle(SavePos[iframe].Value(j),
							SavePos[iframe].Value(k), DotSize, ColorB[i]);
			  DotSize := SavDot;
			  i := i + 1; j := j + 2; k := k+2
			UNTIL i > NBodies;
			IF (SysName = 'Shuttle Docking') AND (iframe = 6) THEN
			BEGIN
				SetColor(LightGreen);
				FOR i := 0 to 5 DO
					PlotLine(6356+i,-199, 6356+i,199)
			END;
			SavePos[iframe].free
		END; {For iplot}
	END; {For iplot}
	CASE Plots of
	  1 : IF Zoomed THEN NamePort := LowNamePort
				ELSE NamePort := LowerNamePort;
	  2 : NamePort := LowNamePort;
	  4,6 : NamePort := MidNamePort
	END; {Case}
	SelectMyViewPort(NamePort);
	n := 80 DIV NBodies; IF n > 16 THEN n := 16;
	j := 26-n*NBodies DIV 2;
	IF j < -n+2 THEN j := -n+2;
	FOR i := 1 to NBodies DO
		BEGIN
		  SetColor(ColorB[NameOrderTable[i]]);
		  Name := NameTable[NameOrderTable[i]];
        IF Name[n] <> ' ' THEN Name[n] := '.';
		  FOR k := n + 1 to 16 DO Name[k] := ' ';
		  Print(j+n*i,1, Name)
		END;
	TempV.Free
END; {ShowInitialPositions}

{--  Look for BodyN crossing x, vx in rotating plane to plot poincare points --}
PROCEDURE DoPoincare(XBodyN, YBodyN : Integer);
VAR
   Temp1, Temp2, Vx : Real;
	Gx,Gy, iframe, iplot : Integer;
BEGIN {Doing without the temp variables brought division by zero errors
       even though short circuit Boolean evaluation was turned on}
	temp1 := Positions[7].Value(YBodyN); {6 is rotating frame}
	temp2 := Position0[7].Value(YBodyN);
   IF ((temp1 = PCriterion) OR ((temp2-PCriterion)/(temp1-PCriterion) <=0)) THEN
   BEGIN
      SelectMyViewPort(PortsUsed[Plots]); {Poincare is always last}
      SelectScale(Plots);
		Vx := (Positions[7].Value(XBodyN) - Position0[7].Value(XBodyN))/TimeStep;
      temp1 := Positions[7].Value(XBodyN);  {Dummy for debugging}
		Map(Positions[7].Value(XBodyN), Vx*VPScale{/ 40}, Gx, Gy);
      PutPixel(Gx,Gy,White);
   	temp2 := gx; {Dummy for debugging}
      temp2 := gy; {Dummy for debugging}
	END; {Poincare Plot}
	FOR iplot := 1 to Plots Do
	BEGIN
      iframe := FrameUsed[iplot];
		Position0[iframe].Equate(1.0, Positions[iframe])
	END
END; {DoPoincare}

PROCEDURE DrawTrajectories;
VAR iplot, iframe, i, j, k : Integer;
BEGIN
	FOR iplot := 1 to Plots Do {Draw the trajectories}
	BEGIN
      iframe := FrameUsed[iplot];
      ToFrame(iframe, Positions[iframe],Y);
      IF Tracking AND (iframe <> 7) THEN {Not Poincare}
      BEGIN
         i := 1; j := 1; k := 2;  {Used to minimize calculations in inner loop}
         SelectMyViewPort(PortsUsed[iplot]);
         SelectScale(iplot);
			REPEAT
				SetColor(ColorB[i]);
				PlotLine(Position0[iframe].Value(j), Position0[iframe].Value(k),
                       Positions[iframe].Value(j), Positions[iframe].Value(k));
				i := i + 1; j := j + 2; k := k+2
			UNTIL i > NBodies
		END;
      IF Not PlotPoinCare THEN{Set equal to first half of Y (length of X0 governs}
			Position0[iframe].Equate(1.0, Positions[iframe]);
   END {FOR iplot}
END; {DrawTrajectories}

PROCEDURE DrawBodies(VAR SightLineCount : Integer;
								 i3, i4, i5, i6, i12 : Integer);
VAR i, j, k, iplot, iframe, DupCount : Integer;
BEGIN
	ShowData(True);
   IF ShowCofM THEN FOR Iplot := 1 to Plots DO
   BEGIN
      iframe := FrameUsed[iplot];
      IF iframe < 7 THEN {7 is poincare handled elsewhere}
      BEGIN
         SelectMyViewPort(PortsUsed[iplot]);
         SelectScale(iplot);
         DrawCofM(iframe, {Erase=} True);
         DrawCofM(iframe, {Erase=} False)
		END {iframe < 7}
   END; {IF ShowCofM}
   IF ShowBodies THEN
	BEGIN
	  DupCount := RotNumber;
     For iplot := 1 to Plots DO {Draw bodies themselves}
     BEGIN
		iframe := FrameUsed[iplot];
      IF iframe < 7 THEN {7 is poincare handled elsewhere}
      BEGIN
        IF iframe = 6 {Rotating} THEN DupCount := DupCount - 1;
        SelectMyViewPort(PortsUsed[iplot]);
        SelectScale(iplot);
        i := 1; j := 1; k := 2;  {Used to minimize calculations in inner loop}
        IF DrawSightLine THEN
        BEGIN
           SetColor(Black);
			  If SightLineCount > 3 THEN
           BEGIN
             SetColor(DarkGray);
				 IF iplot = Plots THEN SightLineCount := 0
			  END; {IF SIghtLineCount}
			  PlotLine(SavePos[iframe].Value(i3), SavePos[iframe].Value(i4),
               (0.1*SavePos[iframe].Value(i3) + SavePos[iframe].Value(i5)*0.9),
               (0.1*SavePos[iframe].Value(i4) + SavePos[iframe].Value(i6)*0.9));
			  IF iplot = Plots THEN SightLineCount := SightLineCount + 1
        END; {IF DrawSightLine}
        REPEAT
          SetColor(Black);
			 DrawCircle(SavePos[iframe].Value(j), SavePos[iframe].Value(k), 
					DotSize, Black);
          SetColor(ColorB[i]);
			 DrawCircle(Positions[iframe].Value(j), Positions[iframe].Value(k), 
					DotSize, ColorB[i]);
          i := i + 1; j := j + 2; k := k+2
        UNTIL i > NBodies;
        IF DrawSightLine THEN
				PlotLine(Positions[iframe].Value(i3), Positions[iframe].Value(i4),
				 (0.1*Positions[iframe].Value(i3)+Positions[iframe].Value(i5)*0.9),
				 (0.1*Positions[iframe].Value(i4)+Positions[iframe].Value(i6)*0.9));
		  IF (IFrame <> 6 {Rotating}) OR (DupCount = 0) THEN
				SavePos[iframe].Equate(1.0, Positions[iframe])
		 END; {For iplot}
	  END; {iframe < 7}
	END; {If ShowBodies}
END; {DrawBodies}

{ ---------------------- System initialization routines ------------------- }

{------------ Set up Center of Mass reference frame as only graph -------------}

PROCEDURE ReWrap;
VAR
   i : Integer;
BEGIN
   IF Wrapped THEN
   BEGIN
      Y.Equate(1.0, History[HistoryCount]);
      For i := N2Bodies+1 to 4*NBodies DO
         Y.Put(i, 0.0)  {Prevents (unneeded) energy calculation from blowing up}
   END {if Wrapped}
   ELSE IF Replicated THEN Y.Equate(1.0, RepInitY)
   ELSE IF NewPicked THEN Y.Equate(1.0, NewInitY)
   ELSE Y.Equate(1.0, InitY)
END; {ReWrap}

Procedure ThreeBodyInit;     {Physical parameters of Sun Earth Moon system}
VAR
   Temp : Real;
   i : Integer;
BEGIN
   t := 0.0;

   Xinit[1] := 0.0; Yinit[1] := 0.0;
	Xinit[2] := R12; Yinit[2] := 0.0;
   Xinit[3] := R12+R23; Yinit[3] := 0.0;
   VxInit[1] := 0.0; VyInit[1] := 0.0;
   VxInit[2] := 0.0; VyInit[2] := V12;
   VxInit[3] := 0.0; VyInit[3] := V12+V23;

   GeneralInit;

END; {ThreeBodyInit}

Procedure GeneralInit;     {Physical parameters of Sun Earth Moon system}
VAR
   i : Integer;
   Temp1, Temp2 : Real;
BEGIN
   t := 0.0;

   Temp1 := 0.0; Temp2 := 0.0;
   FOR i := 1 to NHeavy DO
   BEGIN Temp1 := Temp1 + Xinit[i]*M[i]; Temp2 := Temp2 + M[i] END;
   Temp1 := Temp1/Temp2; {X of C of M}
   FOR i := 1 TO NBodies DO Xinit[i] := Xinit[i] - Temp1;

   Temp1 := 0.0;
   FOR i := 1 to NHeavy DO Temp1 := Temp1 + Yinit[i]*M[i];
	Temp1 := Temp1/Temp2; {Y of C of M}
   FOR i := 1 TO NBodies DO Yinit[i] := Yinit[i] - Temp1;

   Temp1 := 0.0;
   FOR i := 1 to NHeavy DO Temp1 := Temp1 + Vxinit[i]*M[i];
   Temp1 := Temp1/Temp2; {Vx of C of M}
   FOR i := 1 TO NBodies DO Vxinit[i] := Vxinit[i] - Temp1;

	UnivVel.init(N2Bodies);
	UnivVel.put(1, Temp1);

   Temp1 := 0.0;
   FOR i := 1 to NHeavy DO Temp1 := Temp1 + VyInit[i]*M[i];
   Temp1 := Temp1/Temp2; {Vy of C of M}
   FOR i := 1 TO NBodies DO VyInit[i] := VyInit[i] - Temp1;

	UnivVel.put(2, Temp1);
	ExtendVect(UnivVel);

   Y.ReSize(4*NBodies); YP.ReSize(4*NBodies); InitY.ReSize(4*Nbodies);
   FOR i := 1 to NHeavy DO BEGIN
		Xh[i].Resize(NBodies); Yh[i].Resize(NBodies); Rh_3[i].Resize(NBodies) END;

   FOR i := 1 TO NBodies DO 
   BEGIN
      Y.Put(2*i-1, Xinit[i]); Y.Put(2*i, Yinit[i]);
      Y.Put(N2Bodies+2*i-1, VxInit[i]); Y.Put(N2Bodies+2*i, VyInit[i])
   END;

	InitY.Equate(1.0, Y)

END; {GeneralInit}

PROCEDURE PhyInit;
VAR i : Integer;
BEGIN {Unit initalization}
	IF MyVersion > Version THEN Version := Myversion;
	TwoPi   := 2.0*Pi;
	PlotCofM   := True;
	PlotBody1  := False;
	PlotBody2  := False;
	PlotBody3  := False;
	PlotPoincare := False;
	PlotUniverse := False;
	PlotRotating := True;
	Plots      := 2;
   Zoomed     := False; Zf := 1.0; ZFact := 10.0;
	Running    := False; ClearIt := False;
   Wrapped    := False; 
   Replicated := False;
   NewPicked  := False;
   AutoReplay := True;
   Tracking   := True;
   ShowBodies := True;
   ShowCofM   := False;
	DrawSightLine := False;
	ThrustAllowed := False;
	AutoNextReplay := True;
   Param1Exist:= False;

	PeriName := 'PeriAstr'; ApeName := 'ApoAstr';
	NColor := 1;
   TimeDirection := 1; 
	DotSize := 1;
	Thrust := 0.1;  SavT := Thrust;
   TimeStep := 0.1; TimeStepUsed := 0.1;
	MaxStep := 0.1;
   MultipleSpread := 0.01; {Useed as a factor}
   DotInterval := 10; DotGap := 2;
   RelativeError := 1.0e-6;
   AbsError := 1.0e-6;
   CofMNormScale := 0.0; Body1NormScale := 0.0;
   Body2NormScale := 0.0; Body3NormScale := 0.0;
   UnivNormScale := 0.0; DataNormScale := 0.0;
   PoincareNormScale := 0.0; RotateNormScale := 0.0;
   AllNormScale := 1.5;
   RotNormCenterX := 0.0; RotNormCenterY := 0.0; 
	UnivNormCenterX := 0.0; UnivNormCenterY := 0.0;
	DataBody := 2; DataCenterBody := 1;

   HistoryCount := 1;
	RotNumber := 1;

   FOR i := 1 to 5 DO BEGIN
      Xh[i].init(1); Yh[i].init(1); Rh_3[i].init(1) END;

	Y.init(12); YP.init(12); SaveY.init(12);
	InitY.init(12); RepInitY.init(12); NewInitY.init(12);
	For i := 1 to 7 DO
   BEGIN
      XCMSav[i] := 0.0;
      YCMSav[i] := 0.0
   END;

   {*** The following depends on the port numbers being in sequence ***}

   PortTable[2,1] := LeftPort; PortTable[2,2] := RightPort;
	For i := 1 to 4 DO PortTable[3,i] := 20+i; {BigUpperLeft is 21, etc.}
	For i := 1 to 6 DO PortTable[4,i] := 10+i; {UpperLeft is 11, etc.}
END;

END.