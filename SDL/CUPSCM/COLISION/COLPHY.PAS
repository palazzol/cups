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
	 {**            Unit ColPhy.Pas               **}
	 {**          Program Colision.Pas            **}
	 {**          Two Body Collisions             **}
	 {**        Written by Bruce Hawkins          **}
	 {**           March 22, 1993                 **}
	 {**        Last change:  2/ 1/95             **}
	 {**             Version 1.00                 **}
	 {**                                          **}
	 {**      B. Hawkins Copyright @ 1993         **}
	 {**             CUPS Project                 **}
	 {**                                          **}
	 {**     Uses 12/94 MUPPET & CUPS Utilities   **}
	 {**********************************************}
	 {**********************************************}
{                       Contains the Physics                      }
{      Does the main work of displaying the appropriate graphs    }
{            Initializes the parameters for each system           }

UNIT ColPhy;

INTERFACE

USES Crt, Dos, CupsMupp, Graph, Cups,
		CupsFunc, CupsProc, CupsGui, CupsGrph;

TYPE
	ForceType = (Coulomb, ShieldedC, TruncatedC, FPower,
					HardSphere, SoftSphere,  Yukawa, WSaxon, User);
	Arr2 = Array[1..2] of Real;
	AccelType = PROCEDURE(R_2 : Real; VAR XR, A1, A2 : Arr2);
	Vary_Type = (None, ImpactP, XSect, VEnergy,  FType, MassRatio,
					Strength, Range, Steepness);
Const
	NumForces = 9;

PROCEDURE SetAccel;
PROCEDURE Coulomb_Accel(R_2 : Real; VAR XR, A1, A2: Arr2);
PROCEDURE Truncated_Accel(R_2 : Real; VAR XR, A1, A2: Arr2);
PROCEDURE Shielded_Accel(R_2 : Real; VAR XR, A1, A2: Arr2);
PROCEDURE HardSphere_Accel(DeltaT : Real);
PROCEDURE SoftSphere_Accel(R_2 : Real; VAR XR, A1, A2: Arr2);
PROCEDURE Woods_Saxon_Accel(R_2 : Real; VAR XR, A1, A2: Arr2);
PROCEDURE Yukawa_Accel(R_2 : Real; VAR XR, A1, A2: Arr2);
PROCEDURE User_Accel(R_2 : Real; VAR XR, A1, A2: Arr2);
FUNCTION Coulomb_Potential(R : Real): Real;
FUNCTION Truncated_Potential(R : Real) : Real;
FUNCTION Shielded_Potential(R : Real) : Real;
FUNCTION HardSphere_Potential(R : Real): Real;
FUNCTION SoftSphere_Potential(R : Real): Real;
FUNCTION Woods_Saxon_Potential(R :Real): Real;
FUNCTION Yukawa_Potential(R :Real): Real;
FUNCTION User_Potential(R :Real): Real;
PROCEDURE CoulombParameters(ShowScreen : Boolean);
PROCEDURE TruncatedParameters(ShowScreen : Boolean);
PROCEDURE ShieldedParameters(ShowScreen : Boolean);
PROCEDURE PowerParameters;
PROCEDURE HardSphereParameters;
PROCEDURE SoftSphereParameters;
PROCEDURE Woods_Saxon_Parameters;
PROCEDURE Yukawa_Parameters;
PROCEDURE User_Parameters(Var NewConditions: Boolean);
PROCEDURE DisplayPotential(NotTutorial: Boolean);
PROCEDURE DisplayEffectivePotential;
PROCEDURE InitializeParameters;
Procedure NotYet;
FUNCTION PickImpactParam: Boolean;
PROCEDURE MildAnnounce(text : string);

VAR
	MainKeys, RunKeys : THotKeys;
	OldForce : ForceType;

PROCEDURE ClearThePlots(ShowInitialVector, NotTutorial : Boolean);
Procedure ClearPlots(NotTutorial: Boolean);
PROCEDURE SetPlots;
PROCEDURE DrawTrajectories(Y0 : Dvector; X0CM,Y0CM : Real);
PROCEDURE DrawXSection(RepCount : Integer);
PROCEDURE DrawBodies;
PROCEDURE DrawVectors(Tutorial : Boolean);
Procedure DefineMyViewPort (Port : Integer; VPX1,VPX2,VPY1,VPY2: real);
Procedure SelectMyViewport(Port:Integer);
Procedure OpenMyViewPort (Port : Integer);
Procedure CloseMyViewPort (Port : Integer);
PROCEDURE ClearMyViewPort (Port : Integer);
PROCEDURE DrawCircle(x, y: Real; r: Integer; thisColor: Word);
PROCEDURE Debounce;
PROCEDURE SetAllFalse;
PROCEDURE DoRunKeys (key : byte);
Procedure ShowOrbit(Var Angle : Real; NotTutorial : Boolean);
procedure SMALLFONTProc;
procedure TRIPLEXFONTProc;

CONST
	TwoPi   = 2.0*Pi;
	MaxXsects  = 10;
	CommentOn : Boolean = False;
   ShowEffective : Boolean = False;
VAR
	Version : Real;

	HistFile : Text;

	Vary_What : Vary_Type;
	PlotCofM, PlotLab, PlotCofmVectors, PlotLabVectors,
		Vectors, PlotXSection, Varying, Saving : Boolean;
	WhichForce, SecondForce : ForceType;
	TheAccel, SecondAccel : AccelType;
	ThePotential, SecondPotential : FunctionX;
   ForceNames : Array [1..10] of String[20];

	Plots, NumberOfReps,	SaveNReps, MaxTime : Integer;
	R_2, R_02, b, XCofM, YCofM, TotalM, DeltaB, DeltaMult, ProbNorm,
	VInit, Vx1, VCofM, X1, Beta, C, Energy, Power, PowerA: Real;
	LastTime : real;

	Zoomed, Running, Stepping, Quit, ShowCofM, ShowPaths, ShowImpactP, ErasePast,
	 CrossSection, Relativistic, ClearIt, TwoForces, Logs : Boolean;

	DotSize, DelayTime : Integer;

	TimeStep, TimeStepDef, TimeStepUsed, MaxStep	: Real;

	XSaved : Integer;
	XScalMaxLog, XScalMinLog,XScalMax,XScalMin : Real;
	SavProb,	SavAngle : Array [1..MaxXsects] of DVector;
	XDescription : Array [1..MaxXsects] of String[45];
   SavForceT : Array [1..MaxXsects] of ForceType;

	DotInterval, DotGap, DotIntDef : Integer;

	RelativeError, AbsError : Real;

CONST   {Define ViewPort Numbers}

	DemoPort = 1; One = 2;
	LeftPort = 3; RightPort = 4;
	BigUpperLeft = 5; BigUpperRight = 6;
	BigLowerLeft = 7; BigLowerRight = 8;
	AllPort = 9; AnglePort = 10; ParamPort = 11; XSectPort = 12;
	LTextPort = 13; RNamePort = 14; InitDistPort1 = 15; InitDistPort2 = 16;
	BigXS = 17; Name2Port = 18; Name4Port = 19;
	SwapPort = 20;

	HistoryLength = 100;
	XYSnum = 1; PSnum = 2; XSectSNum = 3; VSNum = 4; LnSNum = 5;

VAR
	CofMPort, LabPort, CofMVectorPort,
		LabVectorPort, XSectionPort, NamePort, ClearNamePort : Integer;
		{Viewports can change}

	MyViews : Array [21..30] of View;

	XScale, YScale, PxScale, PyScale  : Real;
	XScaleDef, YScaleDef, PxScaleDef, PyScaleDef : Real;

	HistoryCount, RotNumber : Integer;
	xHistory, yHistory, VxHistory,
		VyHistory  : Array[1..HistoryLength,1..2] of Real;
	tHistory : Array[1..HistoryLength] of Real;
	AngleHistory : Real;

	Position0, Positions, SavePos : Array[1..25] of DVector;

	LengthName : String[4]; ForceName : String[20];

	t : Real;
	key : Byte;
	Y, YP : DVector;

	M, Q : Array [1..2] of Real;
	kc,ks,kw,ky, k0, k00, a0, r00, a, R0, R0_2 : Real; {Constants in Force Laws}
	U1, U2, U3, U4 : Real; {Constants for user defined force law}

IMPLEMENTATION
{************************** Private Procedures *****************************

****************************************************************************}
VAR
	DataScreen : TInputScreen;
	i1, i2, i3, i4, i5, i6, i7, i8 : Integer;
	THist : Array [1..100] of Real;
	XHist, YHist, VxHist, VyHist : Array [1..100,1..2] of Real;
	iHist : Integer;
	Early : Boolean;

PROCEDURE Accelerate(t: real; VAR Y,YP:DVector); FAR;
VAR
	j : Integer;
	XR, A1, A2, A1P, A2P : Arr2;
BEGIN
	{$R-}
	{i1-i8 needed to  circumvent compiler restrictions on Dvector pointers}
	  {Velocities are 1st 2*NBodies derivatives}
	FOR j := 1 to 4 DO YP.VecPtr^[j] := Y.VecPtr^[j+4];
	XR[1] := Y.VecPtr^[i1]-Y.VecPtr^[i3];
	XR[2] := Y.VecPtr^[i2]-Y.VecPtr^[i4];
	R_2 := Sqr(XR[1]) + Sqr(XR[2]);
	TheAccel(R_2, XR, A1, A2);
	IF TwoForces THEN
		IF SecondForce <> HardSPhere THEN
		BEGIN
			SecondAccel(R_2, XR, A1P, A2P);
			FOR j := 1 to 2 DO
			BEGIN
				A1[j] := A1[j]+A1P[j];
				A2[j] := A2[j]+A2P[j]
			END
		END;
	YP.VecPtr^[i5] := A1[1];
	YP.VecPtr^[i6] := A1[2];
	YP.VecPtr^[i7] := A2[1];
	YP.VecPtr^[i8] := A2[2]
	{$R+}
END;

PROCEDURE Coulomb_Accel(R_2 : Real; VAR XR, A1, A2: Arr2);
VAR
	R_3, tret, treto, xret, yret  : Real;
	j, j1, i1, i2, ix, iy, id :Integer;
	XRel, YRel, VxRel, VyRel : Array [1..2] of Real; {1: Source particle}
	Nx, Ny, kappa_3, gamma, energy, beta_2, Ex, Ey, Bz, fract : Real;
BEGIN
	R_3 := Pwr(R_2,PowerA); {1/R^3}
	A1[1] :=    kc*XR[1]*R_3/M[1]; {K < 0 is attractive}
	A1[2] :=    kc*XR[2]*R_3/M[1];
	A2[1] :=  - kc*XR[1]*R_3/M[2];
	A2[2] :=  - kc*XR[2]*R_3/M[2];
END; {Coulomb_Accel}

PROCEDURE Truncated_Accel(R_2 : Real; VAR XR, A1, A2: Arr2);
VAR Rf : Real;
BEGIN
	IF R_2 > R0_2 THEN Coulomb_Accel(R_2, XR, A1, A2)
	ELSE
	BEGIN
		Rf := 1/Pwr(R0,3);
		A1[1] :=    kc*XR[1]*Rf/M[1];
		A1[2] :=    kc*XR[2]*Rf/M[1];
		A2[1] :=  - kc*XR[1]*Rf/M[1];
		A2[2] :=  - kc*XR[2]*Rf/M[1]
	END {Else}
END; {Truncated_Accel}

PROCEDURE Shielded_Accel(R_2 : Real; VAR XR, A1, A2: Arr2);
VAR R, factor : Real;
BEGIN
	Coulomb_Accel(R_2, XR, A1, A2);
	R := Sqrt(R_2);
	factor := R/R0;
	IF factor > 44 THEN Factor := 44;
	IF factor < -44 THEN Factor := -44;
	factor := (1+R/R0)*Exp(-factor);
	A1[1] := A1[1]*factor;
	A1[2] := A1[2]*factor;
	A2[1] := A2[1]*factor;
	A2[2] := A2[2]*factor
END; {Shielded_Accel}

PROCEDURE HardSphere_Accel(DeltaT : Real);
VAR
	j : Integer;
	X12, Y12, Vx12, Vy12, R, V, Theta, Temp : Real;
BEGIN
	{$R-}
	X12 := Y.VecPtr^[i1]-Y.VecPtr^[i3];
	Y12 := Y.VecPtr^[i2]-Y.VecPtr^[i4];
	R_2 := Sqr(X12) + Sqr(Y12);
	R := Sqrt(R_2);
	IF R <= R0 THEN
	BEGIN
		X12 := -Sqrt(Sqr(R0) - Sqr(Y12))+Y.VecPtr^[i3];
		Y.Put(i1,X12); {Move particle back to true edge, assumes Vy is zero}
		R := R0;
		Vx12 := Y.VecPtr^[i5]-Y.VecPtr^[i7];
		Vy12 := Y.VecPtr^[i6]-Y.VecPtr^[i8];
		V := Sqrt(Sqr(Vx12) + Sqr(Vy12));
		Theta := ArcTan2(X12,Y12);
		Temp := Abs((Vx12*X12+Vy12*Y12)/R); {Component of V along R}
		{And add it back in in reverse}
		Y.Put(i5,Y.VecPtr^[i5]+2*Cos(Theta)*Temp*M[2]/(M[1]+M[2]));
		Y.Put(i6,Y.VecPtr^[i6]+2*Sin(Theta)*Temp*M[2]/(M[1]+M[2]));
		Y.Put(i7,Y.VecPtr^[i7] - 2*Cos(Theta)*Temp*M[1]/(M[1]+M[2]));
		Y.Put(i8,Y.VecPtr^[i8] - 2*Sin(Theta)*Temp*M[1]/(M[1]+M[2]))
	END;
	FOR j := 1 to 4 DO
		Y.VecPtr^[j] :=  Y.VecPtr^[j] + Y.VecPtr^[j+4]*DeltaT
	{$R+}
END; {HardSphere_Accel}

PROCEDURE SoftSphere_Accel(R_2 : Real; VAR XR, A1, A2: Arr2);
VAR
	R : Real; j : Integer;
BEGIN
	R := Sqrt(R_2);
	IF R < R0 THEN
	BEGIN
		A1[1] :=  ks*(R0-R)*XR[1]*M[2]/(R*(M[1]+M[2]));{+ k is repulsive}
		A1[2] :=  ks*(R0-R)*XR[2]*M[2]/(R*(M[1]+M[2]));
		A2[1] := -ks*(R0-R)*XR[1]*M[1]/(R*(M[1]+M[2]));
		A2[2] := -ks*(R0-R)*XR[2]*M[1]/(R*(M[1]+M[2]))
	END {R <= R0}
	ELSE FOR j := 1 to 2 DO
		BEGIN a1[j] := 0.0; A2[j] := 0.0 END
END; {SoftSphere_Accel}


PROCEDURE Woods_Saxon_Accel(R_2 : Real; VAR XR, A1, A2: Arr2);
VAR
	r, ExpFactor : Real;
BEGIN
	R := Sqrt(R_2);
	ExpFactor := (R-R0)/a;
	IF ExpFactor > 44 THEN ExpFactor := 44;
	IF ExpFactor < -44 THEN ExpFactor := -44;
	ExpFactor := Exp(ExpFactor);
	ExpFactor := -kw*ExpFactor/(a*Sqr(1+ExpFactor));
{	V(r) = - Vo/(1+exp((r-Ro)/a))  Vo ~ 50 Mev, R0 ~ 10 fm, a ~ 0.5 - 0.7 fm}
	A1[1] :=    XR[1]*ExpFactor/M[1];
	A1[2] :=    XR[2]*ExpFactor/M[1];
	A2[1] :=  - XR[1]*ExpFactor/M[2];
	A2[2] :=  - XR[2]*ExpFactor/M[2]
END; {Woods_Saxon_Accel}

PROCEDURE Yukawa_Accel(R_2 : Real; VAR XR, A1, A2: Arr2);
VAR
	r, ExpFactor : Real;
BEGIN
	R := Sqrt(R_2);
	ExpFactor := -(R)/R0;
	IF ExpFactor < -80 THEN ExpFactor := -80;  {Prevent underflow}
	ExpFactor := Exp(ExpFactor);
	ExpFactor := -ky*ExpFactor*(1/(R_2)+1/(R0*R));
	A1[1] :=    XR[1]*ExpFactor/M[1];
	A1[2] :=    XR[2]*ExpFactor/M[1];
	A2[1] :=  - XR[1]*ExpFactor/M[2];
	A2[2] :=  - XR[2]*ExpFactor/M[2]
END; {Yukawa_Accel}

PROCEDURE User_Accel(R_2 : Real; VAR XR, A1, A2: Arr2);
BEGIN
	MildAnnounce('The user has not yet written Acceleration code ');{Remove this line}
	{This procedure is called by Accelerate which calculates values
	 for the vector XR: the position of object 2 relative to object 1
	 and its square R_2

	 you must use these to calculate values of the x and y components
	 of the acceleration of object 1 and of object 2 

	 The User input screen is set up to enter values for constants
	 U1, U2, U3, and U4, so use these as constants in your calculation
	 if you need more you will have to change the input screen}

	{ Any preliminary calculations needed go here }

{	A1[1] := expression for x component of object 1's acceleration
	A1[2] := expression for y component of object 1's acceleration
	A2[1] := expression for x component of object 2's acceleration
	A2[2] := expression for y component of object 2's acceleration}
END;

FUNCTION Coulomb_Potential(R :Real): Real;
BEGIN
	IF Power <> -1 THEN
		Coulomb_Potential := kc*Pwr(Abs(R),Power+1)
	ELSE
		Coulomb_Potential := -kc*Ln(Abs(R))
END; {Coulomb_Potential}

FUNCTION Truncated_Potential(R : Real) : Real;
BEGIN
	IF Abs(R) > R0 THEN Truncated_Potential := Coulomb_Potential(R)
	ELSE  Truncated_Potential := 1.5*(kc/R0)*(1-0.3333333333*R*R/R0_2)
END; {Truncated_Potential}

FUNCTION Shielded_Potential(R : Real) : Real;
BEGIN
	Shielded_Potential := Coulomb_Potential(R)*Exp(-Abs(R)/R0);
END; {Shielded_Potential}

FUNCTION HardSphere_Potential(R : Real): Real;
var temp : real;
BEGIN
	IF Abs(R) > R0 THEN HardSphere_Potential := 0.0
	ELSE
   BEGIN
   Temp := (Sqrt(Sqr(R0)-Sqr(R)));
   HardSphere_Potential := Temp END
END; {HardSphere_Potential}

FUNCTION SoftSphere_Potential(R : Real): Real;
BEGIN
	IF Abs(R) > R0 THEN SoftSphere_Potential := 0.0
	ELSE SoftSphere_Potential :=ks*Sqr(Abs(R)-R0)
END; {SoftSphere_Potential}

FUNCTION Woods_Saxon_Potential(R :Real): Real;
VAR
	Temp : Real;
BEGIN
	Temp := (Abs(R)-R0)/a;
	IF Temp > 80 THEN Temp := 80;  {Prevent Underflow}
	Woods_Saxon_Potential := -kw/(1+Exp(Temp));
{	Temp := kw/(1+Exp(Temp));}
	{Adding -kw makes potential at infinity zero}
END; {Woods_Saxon_Potential}

FUNCTION Yukawa_Potential(R :Real): Real;
VAR
	Temp : Real;
BEGIN
	Temp := -Abs(R)/R0;
	IF Temp < -80 THEN Temp := -80;  {Prevent Underflow}
	Yukawa_Potential := -ky*Exp(Temp)/Abs(R);
END; {Yukawa_Potential}

FUNCTION User_Potential(R : Real): Real;
BEGIN
	MildAnnounce('The user has not yet written Potential code');{Remove this line}

	{Upon entry the value of R, the distance between the two objects,
	 has been calculated.)
	{Insert calculations here if a single expression on next line is
	 insufficient.}
{	User_Potential := Some expression here}
END;

PROCEDURE FindCofM;
BEGIN
	XCofM := (M[1]*Y.Value(1) + M[2]*Y.Value(3))/TotalM;
	YCofM := (M[1]*Y.Value(2) + M[2]*Y.Value(4))/TotalM
END; {FindCofM}

PROCEDURE SetAccel;
BEGIN
	Case WhichForce OF
		Coulomb, FPower :
			BEGIN
				TheAccel := Coulomb_Accel;
				ThePotential := Coulomb_Potential
			END;
		HardSphere : ThePotential := HardSphere_Potential;
		SoftSphere :
			BEGIN
				TheAccel := SoftSphere_Accel;
				ThePotential := SoftSphere_Potential;
			END;
		ShieldedC :
			BEGIN
				TheAccel := Shielded_Accel;
				ThePotential := Shielded_Potential;
			END;
		TruncatedC :
			BEGIN
				TheAccel := Truncated_Accel;
				ThePotential := Truncated_Potential;
			END;
		Yukawa :
			BEGIN
				TheAccel := Yukawa_Accel;
				ThePotential := Yukawa_Potential;
			END;
		WSaxon :
			BEGIN
				TheAccel := Woods_Saxon_Accel;
				ThePotential := Woods_Saxon_Potential
			END;
		User :
			BEGIN
				TheAccel := User_Accel;
				ThePotential := User_Potential;
			END;
		END {Case}
END; {SetAccel}

PROCEDURE Woods_Saxon_Parameters;
BEGIN
	WhichForce := WSaxon;
	ForceName := 'Woods-Saxon';
	R0 := 0.5; r00 := 0.5;
	a := 0.1; a0 := 0.1;
	kw := 1.0;
	IF TwoForces THEN {This is the second force loaded}
	BEGIN R0 := 0.08; R00 := 0.08; kw:=20.0;k00:=6.324;a:=0.02;a0:=0.02 END
	ELSE k0 := 1.0;
	SetAccel;
	XScale := 1; PxScale := 1.8*M[2];
	Energy := 1;
	b := 0.2;
	DotInterval := 1; TimeStep := 0.1
END; {Woods_Saxon_Parameters}

PROCEDURE Yukawa_Parameters;
BEGIN
	WhichForce := Yukawa;
	ForceName := 'Yukawa';
	R0 := 0.1; r00 := 0.1;
	ky := 1.0;
	IF TwoForces THEN {This is the second force loaded}
	BEGIN R0 := 0.01; R00 := 0.01; ky := 0.5; k00 := 1.6 END
	ELSE k0 := 3.1623;
	SetAccel;
	XScale := 1; PxScale := 1.8*M[2];
	Energy := 1;
	b := 0.2;
	DotInterval := 1; TimeStep := 0.1
END; {Yukawa_Parameters}

PROCEDURE CoulombParameters(ShowScreen : Boolean);
VAR Screen : TInputScreen;
	 kSave : Real;
BEGIN
	AbsError := 1.0E-6; RelativeError := 1.0E-6;
	kc := 0.1; k0 := 0.1;
	ForceName := 'Coulomb';
	IF ShowScreen THEN WITH Screen DO
	BEGIN
		Init;
		LoadLine('  Coulomb Force Choices');
		LoadLine('');
		LoadLine('#1 Repulsive');
		LoadLine('#1 Attractive');
		LoadLine('');
		IF OldForce = Coulomb THEN
		BEGIN
			LoadLine('  Combine with:');
			LoadLine('#2 None');
			LoadLine('#2 Woods-Saxon');
			LoadLine('#2 Yukawa');
			LoadLine('#2 Hard Sphere');
			LoadLine('#2 Soft Sphere')
		END;
		LoadLine('');
		SetHelpFile('colision.hlp','Coulomb Choice Help');
		LoadLine ('[ OK ]  [Cancel] [Help]');
		IF OldForce = Coulomb THEN
			DefineInputPort (0.10,0.43,0.20,0.80)
			ELSE DefineInputPort (0.10,0.43,0.45,0.80);
		AcceptScreen;
		IF Not Canceled THEN
		BEGIN
			IF GetRadioButton('1') = 2 THEN
				BEGIN kc := -Abs(kc); k0 := -Abs(k0) END;
			kSave := kc;
			IF OldForce = Coulomb THEN
			BEGIN
				IF GetRadioButton('2') > 1 THEN TwoForces := True;
				CASE GetRadioButton('2') OF
					2 : Woods_Saxon_Parameters;
					3 : Yukawa_Parameters;
					4 : HardSphereParameters;
					5 : SoftSphereParameters;
				END; {Case}
			END; {OldForce = Coulomb}
			kc := kSave;
			IF OldForce = Coulomb THEN
			 IF GetRadioButton('2') > 1 THEN
			 BEGIN
				ForceName := Concat('Coulomb + ',Forcename);
				SecondForce := WhichForce;
				SecondAccel := TheAccel;
				SecondPotential := ThePotential;
				TwoForces := True
			 END;
		END; {Not Canceled}
		Done
	END; {ShowScreen With Screen}
	Q[2] := Sgn(kc)*Abs(Q[2]); {For relativistic}
	WhichForce := Coulomb;
	Power := -2;
	SetAccel;
	XScale := 1;  PxScale := 1.5*M[2];
	Energy := 1;
	b := 0.2;
	DotInterval := 1; TimeStep := 0.1
END; {CoulombParameters}

PROCEDURE TruncatedParameters(ShowScreen : Boolean);
BEGIN
	CoulombParameters(ShowScreen);
	ForceName := 'Truncated Coulomb';
	R0 := 0.1; r00 := 0.1;
	R0_2 := R0*R0;
	WhichForce := TruncatedC;
	SetAccel;
END; {TruncatedParameters}

PROCEDURE ShieldedParameters(ShowScreen : Boolean);
BEGIN
	CoulombParameters(ShowScreen);
	ForceName := 'Shielded Coulomb';
	R0 := 0.1; r00 := 0.1;
	WhichForce := ShieldedC;
	SetAccel
END; {ShieldedParameters}

Procedure NotYet;
BEGIN
	Announce ('Sorry, this is not yet implemented.');
END; {NotYet}

PROCEDURE PowerParameters;
BEGIN
	With DataScreen DO
	BEGIN
		Init;
		LoadLine ('  Power Law Force');
		LoadLine ('');
		LoadLine ('Use {     }  Power');
{		LoadLine ('#F Use Power SERIES');}
		LoadLine ('');
		LoadLine (' [ OK ]    [Cancel]');
		SetNumber (1,Power);
		SetNumberLimits (1,-100,-1);
		DefineInputPort (0.15,0.43,0.4,0.65);
		AcceptScreen;
		IF Canceled THEN Exit ELSE
		BEGIN
{		IF GetBoolean(2) THEN BEGIN NotYet; Exit END;}
			Power := GetNumber(1)
		END; {Not Canceled}
		Done
	END; {With DataScreen}
	WhichForce := FPower;
	ForceName := Concat(NumStr(Power,5,2),' Power Law');
	SetAccel;
	kc := 0.1; k0 := 0.1;
	XScale := 1;  PxScale := 1.5*M[2];
	Energy := 1;
	b := 0.2;
	AbsError := 1.0E-6; RelativeError := 1.0E-6;
	DotInterval := 1; TimeStep := 0.1
END; {PowerParameters}

PROCEDURE HardSphereParameters;
BEGIN
	WhichForce := HardSphere;
	ForceName := 'Hard Sphere';
	R0 := 0.2; R00 := 0.2;
	SetAccel;
	XScale := 1;  PxScale := 1.5*M[2];
	Energy := 1;
	b := 0.2/Sqrt(2);
	TimeStep := 0.001;
	DotInterval := 100;
	AbsError := 1.0E-6; RelativeError := 1.0E-6
END; {HardSphereParameters;}

PROCEDURE SoftSphereParameters;
BEGIN
	WhichForce := SoftSphere;
	ForceName := 'Soft Sphere';
	R0 := 0.5; R00 := 0.5;
	SetAccel;
	ks := 100;
	IF TwoForces THEN {This is the second force loaded}
	BEGIN R0 := 0.2; R00 := 0.2; k00 := 316.23 END
	ELSE k0 := 100.0;
	XScale := 1;  PxScale := 1.5*M[2];
	Energy := 1;
	b := R0/Sqrt(2);
	TimeStep := 0.01;
	DotInterval := 10;
	AbsError := 1.0E-3; RelativeError := 1.0E-3;
END; {SoftSphereParameters;}

PROCEDURE User_Parameters(Var NewConditions: Boolean);
BEGIN
	MildAnnounce('The user has not yet written code for this');
	NewConditions := False; {To keep conditions screen from coming up}
	Exit;  {Remove this line and the two above}

	WhichForce := User;
	SetAccel;

	{The 2 lines above must not be changed.
	THe lines below should be uncommented and changed to suit your needs
	ForceName labels the diagrams and can be changed to anything you want}

{	ForceName := 'User';
	R0 := 0.5;
	k := 0.1; k0 := 0.1; ks := 100;
	XScale := 1;  PxScale := 1.5*M[2];
	Energy := 1;
	b := R0/Sqrt(2);
	TimeStep := 0.01;
	DotInterval := 10;
	AbsError := 1.0E-6; RelativeError := 1.0E-6;}
END;

PROCEDURE Restart;
BEGIN
	PowerA := (Power-1)/2;
	Y.Fill(0.0);
	Y.Put(1,X1);
	Y.Put(2,b);
	Y.Put(5,Vx1);
	VCofM := Vx1*m[1]/(M[1]+M[2]);
	R_02 := Sqr(Vx1)+Sqr(b);
	TotalM := M[1]+M[2];
	FindCofM
END; {Restart}

PROCEDURE DrawImpactParameter;
BEGIN
	If PlotLab AND ShowImpactP THEN
	BEGIN
		SelectMyViewPort(LabPort);
		SelectScale(XYSNum);
		SetColor (Lightmagenta);
		PlotLine(-XScale,b,0.0,b);
		IF Plots < 4 THEN
		BEGIN
			PutLabel(Top,'____ impact parameter b             ');
			SetColor(White);
			IF ShowCofM THEN
				PutLabel(Top,'                       ; + C of Mass')
		END;
		SetColor(Yellow)
	END; {IF PlotLab}
END; {DrawImpactParameter}

PROCEDURE InitializeParameters;
BEGIN
	X1 := -3*XScale;
	Vx1 := Sqrt(2*Energy/M[1]);
	IF WhichForce in [WSaxon,HardSphere,SoftSphere] THEN
		X1 := -2*R0;
	IF X1 > -XScale THEN X1 := -XScale;
(*	PxScale := 1.5*M[2];*)
	IF Not Quit THEN DrawImpactParameter;
	Restart
END; {InitializeParameters}

{-------- Clears all the graphs ---------------}

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

PROCEDURE Debounce;
BEGIN
	REPEAT CheckForEvents UNTIL Not Event.MouseClicked;
END; {Debounce}

FUNCTION AxisRound(TickSize: Real; Sig : Integer): Real;
VAR
	RTick : Real;
	ITick : Integer;
BEGIN
	IF TickSize < Pwr(10.0,-Sig) THEN AxisRound := TickSize
	ELSE
	BEGIN
		Case Sig Of
			3 : If TickSize < 0.02 THEN ITick := 1000 ELSE ITick := 100;
			2 : If TickSize < 0.2 THEN ITick := 100;  {Set scaling}
		END; {Case}
		If TickSize >= 0.2 Then ITick := 10;
		If TickSize >= 2.0 Then ITick := 1;
		IF TickSize < 0.02 THEN ITick := 2 * ITick;
		RTick := Trunc(ITick*TickSize/2); {Reduces to 2 significant figures}
		RTIck := RTick/ITick;
		IF TickSize < 0.02 THEN RTick := 2 * RTick;
		AxisRound := Rtick
	END {Else}
END; {AxisRound}

PROCEDURE DisplayPotential(NotTutorial: Boolean);
VAR
	Temp, V,Vb,X,Xb : Real;
	i : Integer;
BEGIN
	SelectMyViewPort(LabPort);
	Temp := 2*ThePotential(0.01*XScale);
	IF Temp = 0 THEN Temp := YScale;
	IF WhichForce = HardSphere THEN Temp := YScale;
	IF WhichForce = SoftSphere THEN Temp := 0.5*YScale*Temp/R0;
	IF (Not Varying)
		OR ((WhichForce = SoftSphere) AND (Vary_What = Range)) THEN
		DefineScale(VSNum, -XScale, XScale, -Abs(Temp), Abs(Temp));
	SelectScale(VSNum);
	Xb := -XScale; Vb := ThePotential(Xb);
	SetColor(LightGreen);
	IF (Not (WhichForce in [HardSphere,SoftSphere]))
		AND (Not Varying) AND NotTutorial THEN
	IF (Plots < 4) THEN
      BEGIN
        Print (2,4, Numstr(Abs(Round(5*Temp))/10.0,3,1));
        Print (2,10, Numstr(-Abs(Round(5*Temp))/10.0,3,1))
		END
		ELSE
		BEGIN
		  Print (2,3, Numstr(Abs(Round(5*Temp))/10.0,3,1));
		  Print (2,7, Numstr(-Abs(Round(5*Temp))/10.0,3,1))
		END;
	IF NotTutorial OR (WhichForce <> HardSphere) THEN
	 FOR i := 1 TO 101 DO {Odd to avoid calculating V at r=0}
	 BEGIN
		X := Xb + XScale/50.5; V := ThePotential(X);
		IF TwoForces AND (SecondForce <> HardSPhere) THEN
			V := SecondPotential(X) + V;
		IF Abs(V) < 100*Abs(Temp) THEN PlotLine (Xb,Vb, X,V);
		Xb := X; Vb := V
	 END {For}
END; {DisplayPotential}

PROCEDURE DisplayEffectivePotential;
VAR
	Temp, V,Vb,X,Xb : Real;
	i : Integer;
BEGIN
	SelectMyViewPort(LabPort);
	SelectScale(VSNum);
	Temp := Abs(b*Sqrt(M[1]*Energy/2));
	Xb := -XScale; Vb := ThePotential(Xb)+Temp/(Xb*Xb);
	SetColor(LightBlue);
	FOR i := 1 TO 101 DO {Odd to avoid calculating V at r=0}
	BEGIN
		X := Xb + XScale/50.5; V := ThePotential(X)+Temp/(X*X);
		IF TwoForces AND (SecondForce <> HardSPhere) THEN
			V := SecondPotential(X) + V;
		PlotLine (Xb,Vb, X,V);
		Xb := X; Vb := V
	END {For}
END; {DisplayEffectivePotential}

VAR
	Px1, Py1, Px2, Py2, Pmx1, Pmy1, Pmx2, Pmy2 : Real;

PROCEDURE ClearThePlots(ShowInitialVector, NotTutorial : Boolean);
VAR
	xt,yt,xh,yh, Line : Integer;
BEGIN
	SetColor(White);
	GraphBackColor := DarkGray;
	SelectMyViewPort(ParamPort);
	IF Plots = 4 THEN Line := 1 ELSE Line := 2;
	Print(1,Line,Concat('Force: ', ForceName));
(*	SelectMyViewPort(NamePort);
	IF Plots = 1 THEN PutLabel(Right,ForceName)
		ELSE PutLabel(Top,ForceName);*)
	If PlotLab THEN
	BEGIN
		OpenMyViewPort(LabPort);
		PutLabel(Inside,'Lab System');
		PutLabel(Bottom,'x');
		IF ShowEffective AND (WhichForce <> HardSphere) THEN
		PutLabel(Left,'y,                    ')
			ELSE PutLabel(Left,'y,          ');
		SelectScale(XYSNum);
		Axis (0.0, 0.0, AxisRound(XScale,2), AxisRound(YScale,2));
		SetColor(LightCyan);
		DrawCircle(Y.Value(3),Y.Value(4),3,LightCyan);
		DisplayPotential(NotTutorial);
		IF WhichForce = HardSphere THEN PutLabel(Left, '   Shape    ')
		ELSE IF ShowEffective THEN
			BEGIN
				PutLabel(Left, '             Potential');
				SetColor(LightBlue);
				PutLabel(Left, '   Effective          ');
			END
			ELSE PutLabel(Left, '   Potential');
		SetColor(White); {DisplayPotential uses LightGreen}
	END; {PlotLab}
	If PlotCofM THEN
	BEGIN
		OpenMyViewPort(CofMPort);
		PutLabel(Inside,'Center of Mass System');
		PutLabel(Bottom,'x');
		PutLabel(Left,'y ');
		SelectScale(XYSNum);
		Axis (0.0, 0.0, AxisRound(XScale,2), AxisRound(YScale,2));
		SetColor(White)
	END; {PlotCofM}
	If PlotLabVectors THEN
	BEGIN
		OpenMyViewPort(LabVectorPort);
		PutLabel(Inside,'Lab System');
		PutLabel(Bottom,'x momentum');
		PutLabel(Left,'y momentum');
		SelectScale(PSNum);
		Axis (0.0, 0.0, AxisRound(PXScale,2), AxisRound(PYScale,2));
		SetColor(Yellow);
		Map(0.0,0.0,xt,yt);
		Px1 := Vx1*m[1]; Py1 := 0.0;
		Map(Vx1*m[1],0.0,xh,yh);
		IF ShowInitialVector THEN Vector(xt,yt,xh,yh);
		SetColor(White)
	END; {PlotLabVectors}
	If PlotCofMVectors THEN
	BEGIN
		OpenMyViewPort(CofMVectorPort);
		PutLabel(Inside,'Center of Mass System');
		PutLabel(Bottom,'x momentum');
		PutLabel(Left,'y momentum');
		SelectScale(PSNum);
		Axis (0.0, 0.0, AxisRound(XScale,2), AxisRound(YScale,2));
		SetColor(Yellow);
		Map(0.0,0.0,xt,yt);
		Pmx1 := (Vx1-VCofM)*M[1]; Pmy1 := 0.0;
		Pmx2 := - VCofM*M[2]; Pmy2 := 0.0;
		Map((Vx1-VCofM)*m[1],0.0,xh,yh);
		IF ShowInitialVector THEN Vector(xt,yt,xh,yh);
		SetColor(LightCyan);
		Map(-VCofM*M[2],0.0,xh,yh);
		IF ShowInitialVector THEN Vector(xt,yt,xh,yh);
		SetColor(White)
	END; {PlotCofMVectors}
	If PlotXSection THEN
	BEGIN
		OpenMyViewPort(XSectionPort);
		PutLabel(Inside,'Differential Cross Section');
		PutLabel(Bottom,'Scattering Angle');
		IF (Plots < 4) AND ((Plots > 1) OR (Not Logs)) THEN
			PutLabel(Left,'Relative Probability');
		IF Plots = 1 THEN
			IF Logs THEN PutLabel(Left,'Log Relative Probability');
		SetColor(LightMagenta);
		IF Plots > 1 THEN PutLabel (Right,'Logarithmic Scale');
		SelectScale(XSectSNum);
		Axis (0.0, 0.0,45,0.25);
	END; {PlotXSection}
END; {ClearThePlots}

PROCEDURE ClearPlots(NotTutorial: Boolean);
BEGIN
	SelectMyViewPort(AllPort);
	ClearViewPort;
	InitializeParameters;
	ClearThePlots(True, NotTutorial);
	IF NotTutorial AND (Not PlotXSection) And (Plots > 1) THEN
	BEGIN
		SelectMyViewPort(InitDistPort1);
		ClearViewPort;
		SetColor(White);
		Print(1,1,ConCat('Initial X posit. = ',
				NumStr(X1,6,1)));
	END;
END; {ClearPlots}

PROCEDURE SetPlots;
VAR i : Integer;
BEGIN
	i := 0;
	DefineScale(XSectSNum, 0.0, 180.0, 0.0, 1.0);
	CASE Plots OF
	 1 : BEGIN
			  YScale := XScale; PyScale := PxScale;
			  DefineScale(XYSNum, -XScale, XScale, -YScale, YScale);
			  DefineScale(PSNum, -PXScale, PXScale, -PYScale, PYScale);
			  NamePort := One; ClearNamePort := RNamePort;
			  IF PlotLab THEN LabPort := One;
			  IF PlotCofM THEN CofMPort := One;
			  IF PlotLabVectors THEN LabVectorPort :=One;
			  IF PlotCofMVectors THEN CofMVectorPort := One;
			  IF PlotXSection THEN XSectionPort := BigXS
		  END; {1}
	 2 : BEGIN
			  YScale := XScale; PyScale := PxScale;
			  DefineScale(XYSNum, -XScale, XScale, -YScale, YScale);
			  DefineScale(PSNum, -PXScale, PXScale, -PYScale, PYScale);
			  NamePort := RightPort; ClearNamePort := Name2Port;
			  IF PlotLab THEN BEGIN	LabPort := LeftPort; i := i + 1 END;
			  IF PlotCofM THEN
			  BEGIN
				  CofMPort := LeftPort;
				  i := i + 1;
				  IF i = Plots THEN BEGIN CofMPort := RightPort; Exit END
			  END;
			  IF PlotLabVectors THEN
			  BEGIN LabVectorPort := LeftPort; i := i + 1 END;
			  IF PlotCofMVectors THEN
			  BEGIN
				  CofMVectorPort  := LeftPort;
				  i := i + 1;
				  IF i = Plots THEN BEGIN CofMVectorPort := RightPort; Exit END
			  END;
			  IF PlotXSection THEN
			  BEGIN
				  XSectionPort := LeftPort;
				  i := i + 1;
				  IF i = Plots THEN BEGIN XSectionPort := XSectPort; Exit END
			  END;
			  IF PlotCofMVectors THEN
			  BEGIN
				  CofMVectorPort := LeftPort;
				  i := i + 1;
				  IF i = Plots THEN BEGIN CofMVectorPort:= RightPort; Exit END
			  END;
			  IF PlotLabVectors THEN
			  BEGIN
				  LabVectorPort := LeftPort;
				  i := i + 1;
				  IF i = Plots THEN BEGIN LabVectorPort := RightPort; Exit END
			  END;
		  END; {Case 2}
		4 : BEGIN
				  YScale := 0.73*XScale; PyScale := 0.73*PxScale;
				  DefineScale(XYSNum, -XScale, XScale, -YScale, YScale);
				  DefineScale(PSNum, -PXScale, PXScale, -PYScale, PYScale);
				  NamePort := BigUpperRight; ClearNamePort := Name4Port;
				  LabPort := BigUpperLeft;
				  CofMPort := BigUpperRight;
				  CofMVectorPort := BigLowerRight;
				  LabVectorPort := BigLowerLeft;
				  IF PlotXSection THEN
					  XSectionPort := BigUpperRight
			 END {Case 4}
	END; {Case Plots}
END; {SetPlots}

PROCEDURE DrawCofM;
VAR  Gx1,Gy1 :integer;
	  XCM, YCM : Real;
CONST Size = 2;
BEGIN
	Map(XCofM,YCofM,Gx1,Gy1);
	Line(Gx1-Size,Gy1,Gx1+Size,Gy1);
	Line(Gx1,Gy1-Size,Gx1,Gy1+Size);
END;

PROCEDURE DrawTrajectories(Y0 : Dvector; X0CM,Y0CM : Real);
BEGIN
	FindCofM;
	IF PlotLab THEN
	BEGIN
		SelectMyViewPort(LabPort);
		SelectScale(XYSNum);
		SetColor(Yellow);
		PlotLine (Y0.Value(1),Y0.Value(2),Y.Value(1),Y.Value(2));
		SetColor(LightCyan);
		PlotLine (Y0.Value(3),Y0.Value(4),Y.Value(3),Y.Value(4))
	END; {PlotLab}
	IF PlotCofM THEN
	BEGIN
		SelectMyViewPort(CofMPort);
		SelectScale(XYSNum);
		SetColor(Yellow);
		PlotLine (Y0.Value(1)-X0CM,Y0.Value(2)-Y0CM,
					 Y.Value(1)-XCofM,Y.Value(2)-YCofM);
		SetColor(LightCyan);
		PlotLine (Y0.Value(3)-X0CM,Y0.Value(4)-Y0CM,
					 Y.Value(3)-XCofM,Y.Value(4)-YCofM)
	END; {PlotCofM}
END; {DrawTrajectories}

VAR
	Prob0, LastAngle : Real;

PROCEDURE DrawXSection(RepCount : Integer);
VAR
	ScatteringAngle, Prob, Temp : Real;
BEGIN
	ScatteringAngle := ArcTan2(Y.Value(5),Y.Value(6));
   IF ScatteringAngle > Pi THEN ScatteringAngle := 2*Pi-ScatteringAngle;
	{First probability needs a factor of 2 in addition to setting last angle}
	Prob := b*DeltaB/Abs(Sin(ScatteringAngle));
	IF RepCount = 1 THEN
	BEGIN
		LastAngle := Pi; {For Plotting}
		IF M[1] = M[2] THEN LastAngle := Pi/2;
		DefineScale(LnSNum,0,180,
				Ln(Prob)-0.1*(Ln(ProbNorm)-Ln(Prob)),Ln(ProbNorm));
	END; {Repcount = 1}
	Prob := Prob/Abs(LastAngle-ScatteringAngle);
	IF (RepCount > 1) AND (Not Quit) THEN
	BEGIN
		SelectMyViewPort(XSectionPort);
		SelectScale(XSectSNum);
		PlotLine (LastAngle*180/Pi,Prob0/ProbNorm,
						ScatteringAngle*180/Pi,Prob/ProbNorm);

		SetColor(LightMagenta);
		SelectScale(LnSNum);
		IF Prob0 > 0.0 THEN PlotLine (LastAngle*180/Pi,Ln(Prob0),
						ScatteringAngle*180/Pi,Ln(Prob));
		SetColor(White);
		IF Saving THEN
		BEGIN
			SavProb[XSaved].Put(RepCount,Prob);
			SavAngle[XSaved].Put(RepCount,ScatteringAngle);
			IF Ln(Prob) > XScalMaxLog THEN XScalMaxLog := Ln(Prob);
         IF Ln(Prob) < XScalMinLog THEN XScalMinLog := Ln(Prob);
         IF Prob > XScalMax THEN XScalMax := Prob;
         IF Prob < XScalMin THEN XScalMin := Prob;
		END {Saving}
	END;
	Prob0 := Prob;
	LastAngle := ScatteringAngle
END; {DrawXSection}

VAR
	Bx1, By1, Bx2, By2 : Real;
PROCEDURE DrawBodies;
VAR
	Body1Color, Body2Color, CMColor, Count: Integer;
	Hour, Minute, Second, Sec100 : Word;
	ThisTime : real;
   TempTime : LongInt;
	DelayT : Integer;
BEGIN
	IF ErasePast THEN Count := 0 ELSE Count := 1;
	WHILE Count < 2 DO
	BEGIN
		IF Count = 0 THEN {Erase old body positions}
		BEGIN
			Body1Color := GraphBackColor; Body2Color := GraphBackColor;
			CMColor := GraphBackColor
		END
		ELSE
		BEGIN
			FindCofM;
			Bx1 := Y.Value(1); By1 := Y.Value(2);
			Bx2 := Y.Value(3); By2 := Y.Value(4);
			Body1Color := Yellow; Body2Color := LightCyan; CMColor := White
		END;
		IF PlotLab THEN
		BEGIN
			SelectMyViewPort(LabPort);
			SelectScale(XYSNum);
			IF ShowCofM THEN
			BEGIN
				SetColor(CMColor);
				DrawCofM
			END; {IF ShowCofM}
			SetColor(Body1Color); {Color of rim}
			DrawCircle (Bx1,By1,3,Body1Color);
			SetColor(Body2Color); {Color of rim}
			DrawCircle (Bx2,By2,3,Body2Color)
		END; {PlotLab}
		IF PlotCofM THEN
		BEGIN
			SelectMyViewPort(CofMPort);
			SelectScale(XYSNum);
			SetColor(Body1Color); {Color of rim}
			DrawCircle (Bx1-XCofM,By1-YCofM,3,Body1Color);
			SetColor(Body2Color); {Color of rim}
			DrawCircle (Bx2-XCofM,By2-YCofM,3,Body2Color)
		END; {PlotCofM}
		Count := Count + 1
	END; {While Count < 2}
	IF (DelayTime > 0) AND (Count = 2) THEN
	BEGIN
		GetTime(Hour, Minute, Second, Sec100);
		ThisTime := (Sec100+100.0*Second+6000.0*Minute+360000.0*Hour)*10.0; {ms}
		TempTime := trunc(ThisTime - LastTime);
		If TempTime < 0 THEN {presumably passed midnight}
			TempTime := TempTime + 360000*24;
		IF TempTime < DelayTime THEN DelayT := DelayTime-TempTime
		ELSE DelayT := 0;
		IF (TempTime > MaxTime) AND (TempTIme < 3000) THEN
			MaxTime := TempTime;
		Delay(DelayT);
		GetTime(Hour, Minute, Second, Sec100);
		LastTime := (Sec100+100.0*Second+6000.0*Minute+360000.0*Hour)*10 {ms}
	END {DelayTime > 0}
END; {DrawBodies}

PROCEDURE DrawVectors(Tutorial : Boolean);
VAR
	xt,yt,xh,yh,xh2,yh2 : Integer;
	Body1Color, Body2Color, Count: Integer;
BEGIN
	IF ErasePast OR ShowPaths THEN Count := 0 ELSE Count := 1;
	WHILE Count < 2 DO
	BEGIN
		IF Count = 0 THEN {Erase old body positions}
		BEGIN
			Body1Color := GraphBackColor; Body2Color := GraphBackColor
		END
		ELSE
		BEGIN
			FindCofM;
			Px1:= Y.Value(5)*M[1]; Py1 := Y.Value(6)*M[1];
			Px2 := Y.Value(5)*M[1]+Y.Value(7)*M[2];
			Py2 := Y.Value(6)*M[1]+Y.Value(8)*M[2];
			Pmx1 := (Y.Value(5)-VCofm)*M[1]; Pmy1 := Y.Value(6)*M[1];
			Pmx2 := (Y.Value(7)-VCofM)*M[2]; Pmy2 := Y.Value(8)*M[2];
			Body1Color := Yellow; Body2Color := LightCyan;
		END;
		IF PlotLabVectors THEN
		BEGIN
			IF Tutorial THEN
			BEGIN Body1Color := Yellow; Body2Color := Magenta END;
			SelectMyViewPort(LabVectorPort);
			SelectScale(PSNum);
			SetColor(Body1Color);
			Map (0.0,0.0, xt,yt);
			Map (Px1,Py1, xh, yh);
			Vector (xt,yt, xh,yh);
			SetColor(Body2Color);
			Map (Px2,Py2, xh2,yh2);
			Vector(xh,yh, xh2,yh2)
		END; {PlotLabVectors}
		IF PlotCofMVectors THEN
		BEGIN
			IF Tutorial THEN
			BEGIN Body1Color := LightCyan; Body2Color := LightRed END;
			SelectMyViewPort(CofMVectorPort);
			SelectScale(PSNum);
			SetColor(Body1Color);
			Map (0.0,0.0, xt,yt);
			Map (Pmx1,Pmy1, xh,yh);
			Vector(xt,yt,xh,yh);
			SetColor(Body2Color);
			Map (Pmx2,Pmy2, xh,yh);
			Vector(xt,yt,xh,yh)
		END; {PlotCofMVectors}
		Count := Count + 1
	END {While Count}
END; {DrawVectors}

{ ---------------------- System initialization routines ------------------- }


{------------ Set up Center of Mass reference frame as only graph -------------}

PROCEDURE SetAllFalse;
BEGIN
	PlotCofM := False;
	PlotLab := False;
	PlotCofmVectors := False;
	PlotLabVectors := False;
	Vectors := False;
	PlotXSection := False
END; {SetAllFalse}

PROCEDURE DoRunKeys (key : byte);
BEGIN
	CASE key OF
	 1 : BEGIN
			Running := False;
			Quit := True;
			Varying := False;
			MainKeys.Display;
	 END;
	 2 : BEGIN
			Stepping := True;
			Message ('Press any key or click mouse to step.');
		  END;
	3 : BEGIN
			 Stepping := False;
			 Message ('Press F3 to stop.');
			 Running := True
		 END
	END; {case}
	Debounce;
END; {DoRunKeys}

Procedure ShowOrbit(Var Angle : Real; NotTutorial : Boolean);
VAR
	count, IFlag : Integer;

	PROCEDURE CheckErrors;
	BEGIN
		Case IFlag of
			1 : ; {No error; continue}
         2 : BEGIN
             Message ('Bodies came too close; integration lost precision');
				 IF (WhichForce in [Coulomb, ShieldedC, TruncatedC, FPower])
					and (kc < 0.0) THEN
				  MildAnnounce (
				  'Happens for small impact parameters with attractive Coulomb forces.');
				 StaticPause;
				 Running := False;
				 END;
			3 : BEGIN
				 Message ('Illegal input values to integration routine');
				 StaticPause;
				 Running := False;
				 END;
			4 : BEGIN
				 Message ('Requested precison needs too small an integration step size');
				 StaticPause;
				 Running := False;
				 END;
			ELSE BEGIN
				  Message ('Undetermined error in integration routine');
				  StaticPause;
				  Running := False;
				  END;
		END; {Case}
	END; {CheckErrors}


VAR
	Inside, OutSide : Boolean;
	Y0 : DVector;
	X0CM, Y0CM  : Real;
	i : Integer;
BEGIN {ShowOrbit}
	IF Not Varying THEN RunKeys.Display;
	Y0.Init(8);
	VInit := Y.Value(5);
	Count := 0; 	LastTime := 0;
	HistoryCount := 1;
	Inside := False; Outside := False; Stepping := False;
	Y0.Equate(1,Y); X0CM := XCofM; Y0CM := YCofM;
	IF NotTutorial THEN
	BEGIN
		SelectMyViewPort(InitDistPort1);
		ClearViewPort;
		SetColor(White);
		Print(1,1,ConCat('Initial X posit. = ',
				NumStr(X1,6,1)));
	END;
	While Running DO
	BEGIN
		{$R-}
		tHistory[HistoryCount] := t;
		FOR i := 0 to 1 DO
		BEGIN
		xHistory[HistoryCount,i+1] := Y.VecPtr^[i1+2*i];
		yHistory[HistoryCount,i+1] := Y.VecPtr^[i2+2*i];
		VxHistory[HistoryCount,i+1] := Y.VecPtr^[i5+2*i];
		VyHistory[HistoryCount,i+1] := Y.VecPtr^[i6+2*i];
		END;
		{$R+}
		HistoryCount := 1 + HistoryCount Mod HistoryLength;
		IF TwoForces AND (SecondForce = HardSphere) THEN
			HardSphere_Accel(TimeStep);
		IF WhichForce = HardSphere THEN HardSphere_Accel(TimeStep) ELSE
		BEGIN
			StepRKF(Accelerate, t, Y, TimeStep, TimeStepUsed, MaxStep,
				  AbsError, RelativeError, IFlag);
			CheckErrors
		END;
		IF Stepping THEN
		BEGIN
			DynamicPause;
			CheckForEvents;
			If RunKeys.Pressed(key) THEN DoRunKeys(key)
		END;
		Count := Count + 1;
		If Count >= DotInterval THEN
		BEGIN
			Count := 0;
			IF ShowPaths THEN DrawTrajectories(Y0, X0CM,Y0CM) ELSE DrawBodies;
			Y0.Equate(1,Y); X0CM := XCofM; Y0CM := YCofM;
			IF Vectors THEN DrawVectors(Not NotTutorial)
		END;
		IF NotTutorial THEN
		BEGIN
		  CheckForEvents;
		  If RunKeys.Pressed(key) THEN DoRunKeys(key);
		END; {CheckEvents}
		IF (Not Inside) AND (Abs(Y.Value(1)) < XScale) THEN Inside := True;
		IF Inside
			AND (((Abs(Y.Value(1)) > XScale) OR (Abs(Y.Value(2)) > YScale))
				OR ((Abs(Y.Value(3)) > 5*XScale) OR (Abs(Y.Value(4)) > 5*YScale)))
			THEN Outside := True;
		IF Inside AND (b = 0)    {Head on collision m1 may stop inside}
			AND (((Abs(Y.Value(1)) > XScale) OR (Abs(Y.Value(2)) > YScale))
				OR ((Abs(Y.Value(3)) > XScale) OR (Abs(Y.Value(4)) > YScale)))
			THEN Outside := True;
		IF Outside OR (Y.Value(5) < -VInit) THEN
			IF R_2 > 4*R_02 THEN Running := False
	END; {While Running}
	Angle := ArcTan2(Y.Value(5),Y.Value(6));
	IF Angle > Pi THEN Angle := Angle - 2*Pi;
	AngleHistory := Angle;
	IF Erasepast then BEGIN InitializeParameters; ClearPlots(True) END;
	IF NotTutorial THEN
	BEGIN
		SelectMyViewPort(AnglePort);
		ClearViewPort;
		SetColor(White);
		Print(1,1,ConCat('Scattering angle = ',
				NumStr(Angle*180/Pi,7,2)));
		SelectMyViewPort(InitDistPort1);
		ClearViewPort;
		SetColor(White);
		Print(1,1,ConCat('Initial X posit. = ',
				NumStr(X1,6,1)));
		IF Not Varying THEN MainKeys.Display
	END;
	Y0.Free;
	IF CommentOn THEN
	BEGIN
		Comment (0.05,0.92, {Must be same length & position as original}
'                                                                        ');
		CommentOn := False
	END
END; {ShowOrbit}

FUNCTION PickImpactParam: Boolean;
VAR
  dy : Integer;
  y2 : Real;
BEGIN
	PickImpactParam := False;
	If PlotLab THEN
	WITH event DO WITH views[LabPort] DO {Look for events in lower right}
	IF mouseClicked AND (x>vx1) AND (x<vx2) AND (y>vy1) AND (y<vy2) THEN
	BEGIN
		dy := (vy2-vy1);
		y2:=(vy2-y) /dy;
		b := GraphScales[XYSNum].ymin +
						(GraphScales[XYSNum].Ymax-GraphScales[XYSNum].Ymin)*y2;
		PickImpactParam := True;
      ErasePast:= False;
		Running := True;
	END {If MouseClicked}
END; {IF PlotLab THEN WITH .. CASE }

PROCEDURE MildAnnounce(text : string);
VAR
   dx,dy,cx,cy,a,n : integer;
	image           : TImage;
	z               : char;
   envir           : TEnvironment;
BEGIN
	envir.Save;
	envir.Standardize;
	SetFillStyle(SolidFill,Brown);
	dx := TextWidth(text)+13;
   dy := RowHt + 3;
   cx := (GetMaxX-dx) DIV 2;
   cy := (GetMaxY-dy) DIV 2;
   Image.Get(cx,cy,cx+dx,cy+dy);
   HideCursor;
   Bar3D(cx,cy+3,cx+dx-3,cy+dy,3,true);
   ShowCursor;
	ClearMKBuffers;
	SetColor(white);
	HideCursor;
	OutTextXY(cx+6,cy+8,Text);
	ShowCursor;
	DynamicPause;
	image.put(cx,cy);
	envir.Reset;
END;

procedure SMALLFONTProc; external;
{$L LITT.OBJ }

procedure TRIPLEXFONTProc; external;
{$L TRIP.OBJ }

VAR i : Integer;
BEGIN {Unit initalization}

	Version := 1.00;

	Y.init(8); YP.init(8);
	M[1] := 1; M[2] := 1; q[1] := 1; q[2] := 1;
	TotalM := M[1]+M[2]; C := 1.0; beta := 0.9;
	HistoryCount := 1; XSaved := 0;
	XScalMaxLog := -1E6; XScalMinLog := 1E6;
	XScalMax := -1E6; XScalMin := 1E6;
	kc := 0.1;      {- is attractive, + repulsive}
	ShowCofM := True; Relativistic := False; TwoForces := False;
	ShowPaths := False; ShowImpactP := True; ErasePast := True;
	Zoomed := False;
	TimeStep := 0.1; MaxStep := 0.1; DelayTime := 0;
	AbsError := 1.0E-6; RelativeError := 1.0E-6;
	MaxTime := 100; {Estimated Delay Time for 386/33}
	i1 := 1; i2 := 2; i3 := 3; i4 := 4; i5 := 5; i6 := 6; i7 := 7; i8 := 8;
	For i := 1 to MaxXSects DO
	BEGIN
		SavProb[i].Init(1); {Will be resized before use}
		SavAngle[i].Init(1)
	END;

	ForceNames[1] := 'Coulomb';
	ForceNames[2] := 'Shielded Coulomb';
	ForceNames[3] := 'Truncated Coulomb';
	ForceNames[4] := 'Power';
	ForceNames[5] := 'HardSphere';
	ForceNames[6] := 'SoftSphere ';
	ForceNames[7] := 'Yukawa';
	ForceNames[8] := 'Woods-Saxon';
END.