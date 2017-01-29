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
	 {**            Unit AnhPhy.Pas               **}
	 {**           Program AnHarm.Pas             **}
	 {**      Non-Linear Oscillator Behaviour     **}
	 {**        Written by Bruce Hawkins          **}
	 {**           November 28, 1991              **}
	 {**        Last change:  2/15/95             **}
	 {**             Version 1.00                 **}
	 {**                                          **}
	 {**      B. Hawkins Copyright @ 1991         **}
	 {**             CUPS Project                 **}
	 {**                                          **}
	 {**     Uses 12/94 MUPPET & CUPS Utilities   **}
	 {**********************************************}
	 {**********************************************}

{            Contains all force and potential functions        }

{$F+}
UNIT AnhPhy;

INTERFACE

USES CupsProc, CupsFunc;

{$IFDEF CPU87}
	TYPE
		REAL = DOUBLE;
{$ELSE}
	TYPE
		SINGLE   = REAL;
		DOUBLE   = REAL;
		EXTENDED = REAL;
		COMP     = REAL;
{$ENDIF}

PROCEDURE Accel(t: real; VAR YF,YPF:DVector);
FUNCTION AsymmetricPotential(x:real): Real; {Spring between two walls}
FUNCTION AsymmetricAccel(x:real): Real;
FUNCTION ExactWallPotential(x:real): Real; {Spring between two walls}
FUNCTION ApproxWallPotential(x:real): Real; {4th order approx to above}
FUNCTION ExactWallAccel(x:real): Real;
FUNCTION ApproxWallAccel(x:real): Real;
FUNCTION ExactPendPotential(Theta:real): Real; {1 - Cos theta}
FUNCTION ApproxPendPotential(Theta:real): Real; {Fourth order approximation}
FUNCTION ExactPendAccel(Theta:real): Real;
FUNCTION ApproxPendAccel(Theta:real): Real;
FUNCTION SHOAccel(x : Real): Real;
FUNCTION SHOPotential(x:real): Real;
FUNCTION CubicAccel(x : Real): Real;
FUNCTION CubicPotential(x:real): Real;
FUNCTION VanDePolAccel(x : Real): Real;
PROCEDURE FindPotential (SetScale : Boolean); {Stores V values in an array}
PROCEDURE GetX(VAR X :Real; Choice : Real);
PROCEDURE GetX2(VAR X2 :Real; Choice : Real);
FUNCTION GetVel(Energy : Real): Real; {Returns velocity for pendulum energy above upside down}
PROCEDURE ShowTrajectory;
PROCEDURE SavePMap(FName : String);
PROCEDURE ReadPMap(FName : String; Flip : Boolean; VAR Error : Boolean;
				VAR FramesUsed, MapLength : Integer);
PROCEDURE Read2PMaps(FName1, FName2 : String);
PROCEDURE ReadSMap(FName : String);
PROCEDURE MakePeriodTable;
Procedure MakeResonanceTable;
PROCEDURE ChangePotential;
PROCEDURE MoveWall;
Procedure SetUpFourierTransform(VAR numPts : word;
									VAR Test, Windowed : Boolean; Clearit : Boolean;
									VAR Period : Real;
									VAR Shift, Points, Shape :Integer;
										 Color1 : Integer);
Procedure FastFourierTransform;
Procedure PlotFourierTransform(NumData, color1,color2 : word;
							VAR Test : Boolean;
							ZoomSet, ClearIt : Boolean; InitialZoom : Real);
PROCEDURE FourierTransform(NumData : word; IHist : Integer; Wrapped : Boolean);
PROCEDURE PickEnergy;
PROCEDURE FlipEm(Slow, Return : Boolean);

{**************************************************************}

VAR
	x,numdata : word;

	TwoPi : Real;

CONST
	BothV      : boolean = False;
	HoldVScale : boolean = False;
	FreezeEnergy : Boolean = False;
	LargePoincare : Boolean = True;
	TimeStep : Real = 0.1; DrivenTimeFactor : Real = 60;
	NPhases : Integer = 12;
	MapLength : Integer = 0;
{$IFDEF CPU87}
	HistoryCount = 1023; {2047}
	MaxSub=256;
	MaxSubX2=512;
{$ELSE}
	HistoryCount = 2047;
	MaxSub=512;
	MaxSubX2=1024;
{$ENDIF}

	{Physical parameters of system}
	Mass : Real = 1.0;
	koverm : Real = 1.0;
	C3overM : Real = 1.0;
	AsCoef3 : Real = 0.1;
	AsCoef4 : Real = 0.1;
	gOverL : Real = 9.8;
	PendL : Real = 1.0;
	DoverM : Real = 0.0;
	omega : Real = 1.0;
	F0overM : Real = 0.0;
	VdepX0 : Real = 1.0; {Van Der Pol oscillator limit cycle size}
	UserPar : Real = 1.0;
	SMapSize = 30;
	PMapSize = 2000;
	MaxPhases = 32;
	MapsUsed : Integer = 0;

	HalfWallSep : Real = 0.5; Restlength : Real = 0.4; {Several each}
	Energy : Real = 0.02;
	Emin : Real =0.0; EminA : Real = 0.0; EminX : Real = 0.0;
	XStart : Real = 0.02;

	MaxPos : Real = 0.5; XMinus : Real = -0.5;
	MaxEnergy : Real = 0.5;
	MaxVel : Real = 0.75;

	DResist : Real = 0.1; NResist : Integer = 2;
	DOmega : Real = 0.1; NOmega : Integer = 2;
	DDrive : Real = 0.1; NDrive : Integer = 2;

TYPE
	ForceType = (Asymmetric, Cubic, Pendulum, SHO, Walls, VanDePol);
	SMap = Array[1..SMapSize] of Integer;
	SMapRec = Record
		ID, NFrames, NPoints, Frame : Integer;
		Ener, Drag, Driv, Omeg : Real;
	 XMap, VMap : SMap;

		CASE Force : ForceType OF
			Walls, SHO, Cubic : (fk, fm, separation : Real);
			Asymmetric : (am, fak, k3, k4 : Real);
			Pendulum : (fg, Plength : Real);
			VanDePol : (vk, vm, VX0 : Real);
	END; {SMapRec}

	SFileTyp = File of SMapRec;
	Ibase = ARRAY[1..1] OF Integer;
	IbasePtr =^Ibase;

VAR
	WhichForce : ForceType;
	VCoef0, VCoefA, VCoefB, VCoefC, VCoefD : Real; {Approximate Potential Parameters}

	xpos, potential, xactpotential : DVector;
	XMapPtr, VMapPtr, XMap2Ptr, VMap2Ptr : Array[1..MaxPhases] of IbasePtr;

	Equilibrium, Period, t, TimeScale, TimeStepUsed : Real;
	Energies, Periods : Array[1..10] of DVector;
	Amplitudes, Frequencies : Array[1..5,1..3] of DVector;
	Xs, Vs, Ts : Array[0..HistoryCount] of Real;
	IHist : Integer;
	Wrapped : Boolean;
	NumberOfResPlots : Array[1..5] of Integer;

{Anharm and ANharmP}
	AccelP       : DFproc;

PROCEDURE SaveSMap(VAR SFile : SFileTyp; Count : Integer);

IMPLEMENTATION
{************************** Private Procedures *****************************
ShowTrajectory Contains
  Procedure DrawEnergyLine;  Puts trajectory energy on the potential graph
  Procedure RemoveCircle;    Erases springs and mass during animation
  Procedure NewCircles;      Draws the mass and springs during animation
  Procedure ExtendPhaseCurve;  Takes one time step and plots it on all the graphs
****************************************************************************}

USES Dos, Crt, Graph, Cups, CupsMupp, CupsGrph, CupsGui, AnhMenu, anhHelp;

CONST
	MyVersion : Real = 1.00;
	MaxMapLength = 2000;
	ResCrit : Real = 0.05;

TYPE
	PMap = Array[1..PMapSize] of Integer;

	PMapRec = Record
		ID, NFrames, NPoints, Frame : Integer;
		Ener, Drag, Driv, Omeg : Real;
	 XMap, VMap : PMap;

		CASE Force : ForceType OF
			Walls, SHO, Cubic : (fk, fm, separation : Real);
			Asymmetric : (am, fak, k3, k4 : Real);
			Pendulum : (fg, Plength : Real);
			VanDePol : (vk, vm, VX0 : Real);
	END; {PMapRec}

VAR
	vel, Amplitude, ThisAmp, LastAmp, EarlyAmp : Real;
	NPhase, FileID, NMap : Integer;
	Screen : TInputScreen;

{---------- Concerning springs between two walls ----------------------}
{ This assumes all forms of Duffing's equation, or rather the latter is
	an approximation to this }

FUNCTION AsymmetricPotential(x:real): Real; {Spring between two walls}
BEGIN
	AsymmetricPotential := kOverM*mass*Sqr(x)/2 + AsCoef3*mass*Pwr(x,3)
				+ AsCoef4*mass*Pwr(x,4)
END;

FUNCTION AsymmetricAccel(x:real): Real;
BEGIN
	AsymmetricAccel := -koverm*x - 3*AsCoef3*Sqr(x) - 4*AsCoef4*Pwr(x,3)
				  - DoverM*vel + F0overM*Sin(omega*t)
END;

FUNCTION ExactWallPotential(x:real): Real;
VAR  SLength : Real;
BEGIN
	SLength := Sqrt(Sqr(x)+Sqr(HalfWallSep));
	ExactWallPotential := kOverM*mass*Sqr(SLength - RestLength);
END; {ExactWallPotential}

FUNCTION ApproxWallPotential(x:real): Real;
BEGIN
	ApproxWallPotential := VCoef0 + VCoefA*Sqr(x)/2 + VCoefB*Pwr(x,4)/4
									{+ VCoefC*Pwr(x,6)/6 + VCoefD*Pwr(x,8)/8}
END; {ApproxWallPotential}

FUNCTION ExactWallAccel(x:real): Real;
VAR  SLength : Real;
BEGIN
	SLength := Sqrt(Sqr(x)+Sqr(HalfWallSep));
	ExactWallAccel := -2*koverm*(SLength - RestLength)*x/SLength
							-DoverM*vel+F0overM*Sin(omega*t)
END; {ExactWallAccel}

FUNCTION ApproxWallAccel(x:real): Real;
BEGIN
	ApproxWallAccel := -VCoefA*x - VCoefB*Pwr(x,3)
							 {- VCoefC*Pwr(x,5) - VCoefD*Pwr(x,7)}
							-DoverM*vel+F0overM*Sin(omega*t)
END; {ApproxWallAccel}

{----------------------- Concerning the Pendulum ----------------------}

FUNCTION ExactPendPotential(Theta:real): Real;
BEGIN
	ExactPendPotential := PendL*gOverL*(1-Cos(Theta))
END; {ExactPendPotential}

FUNCTION ApproxPendPotential(Theta:real): Real;
BEGIN
	ApproxPendPotential := PendL*gOverL*(0.5*Sqr(Theta) + 0.041666667*Pwr(Theta,4))
END; {ApproxPendPotential}

FUNCTION ExactPendAccel(Theta:real): Real;
BEGIN
	ExactPendAccel := -gOverL*Sin(Theta)
							-DoverM*vel+F0overM*Sin(omega*t)
END; {ExactPendAccel}

FUNCTION ApproxPendAccel(Theta:real): Real;
BEGIN
	ApproxPendAccel := -gOverL*(Theta + 0.16666667*Pwr(Theta,3))
							-DoverM*vel+F0overM*Sin(omega*t)
END; {ApproxPendAccel}

{------------------- Van Der Pol Oscillator (has no potential) -------------}

FUNCTION VanDePolAccel(x : Real): Real;
BEGIN
	VanDePolAccel := (-koverm*x + DoverM*(Sqr(VdepX0) - Sqr(x))*vel)
						  + F0overM*Sin(omega*t)
END; {VanDePolAccel}

FUNCTION SHOPotential(x:real): Real;
BEGIN
	SHOPotential := kOverM*mass*Sqr(x)/2
END; {SHOPotential}

FUNCTION SHOAccel(x : Real): Real;
BEGIN
	SHOAccel := -koverm*x - DoverM*vel + F0overM*Sin(omega*t)
END; {SHOAccel}

FUNCTION CubicPotential(x:real): Real;
BEGIN
	CubicPotential := kOverM*mass*x*x/2 + C3overM*Mass*x*x*x*x/4
END; {CubicPotential}

FUNCTION CubicAccel(x : Real): Real;
BEGIN
	CubicAccel := -koverm*x -C3overm*x*x*x - DoverM*vel + F0overM*Sin(omega*t)
END; {CubicAccel}

{-------------- Amplitude for any energy, and potential ------------------}

PROCEDURE ERoots(Fct:FUNCTIONX;a,b, Tol: REAL; VAR N, J: integer;
															 VAR M: DVector;
															 VAR Err : Boolean);
{Modified to return an error instead of halting on maxiter exceeded}
(* Roots divides the interval (a,b) into N subintervals AND searches
each FOR a root IF the sign OF the FUNCTION is different at the two
ends.  The search BEGINs WITH the regulus falsi AND moves TO the secant
algorithm as the root is approached.  Roots returns the number OF roots
found as the integer J AND places their values IN the DVector M. The
quantity Tol is the ABSOLUTE value OF the maximum difference between the
reported zero AND the value obtained on the previous iteration IF the
number OF iterations is less than 50. *)

CONST
   maxiter = 50;
VAR
  x,dx,xo,xol,fo,x1,f1,x2,f2: REAL;
  nf,i,iter: integer;

 PROCEDURE MOVES(VAR r,s,t,u,v,w,x,y : REAL);
 BEGIN
	r:=s; t:=u; v:=w; x:=y
 END;

BEGIN
	J := 0; nf := N-1;
   dx := (b-a)/N;
	FOR  i:=0 TO nf DO
		BEGIN
			Err := False;
			x := a + i*dx;
			f1:= Fct(x); f2 := Fct(x+dx);
			IF (f1*f2 < 0.0) THEN
         BEGIN
				iter :=0;
            x1 := x; f1 := Fct(x1); x2 := x + dx; f2 := Fct(x2);
            xo:= x1;
				REPEAT
					xol := xo; xo :=(x1*f2-x2*f1)/(f2-f1); fo := Fct(xo);
               iter := iter + 1;
					IF (ABS(xo-xol) > Tol)  THEN
						IF ABS(xo-x1) < 0.1*dx THEN
                     MOVES(x2,x1,f2,f1,x1,xo,f1,fo)
						ELSE
							IF ABS(xo-x2) < 0.1*dx THEN
                        MOVES(x1,x2,f1,f2,x2,xo,f2,fo)
                  ELSE
							IF f1*fo > 0
                        THEN
									MOVES(x1,xo,f1,fo,x2,x2,f2,f2)
                     ELSE
								MOVES(x2,xo,f2,fo,x1,x1,f1,f1);
				UNTIL ((iter =maxiter) OR (ABS(xo-xol)<Tol));
            IF (iter=maxiter) THEN
				BEGIN
					Err := True; M.Put(J+1, 0.0); Exit
				END
				ELSE
               BEGIN
						J := J + 1;
                  M.Put(J,xo)
               END
         END
      END
END; {ERoots}

VAR
	 RFn : FUNCTIONX;
	 Level : Real; {Global is only way I can see to pass the value}

FUNCTION RootFn(X : Real) : Real; far;
BEGIN
	RootFn := AsymmetricPotential(X) - Level
END;

PROCEDURE GetX(VAR X :Real; Choice : Real);
VAR Temp, TempX : Real;
	 Result : DVector;
	 n, j, iter : Integer;
	 Err : Boolean;

BEGIN
	Case WhichForce of
	  Asymmetric :
		  BEGIN
			  Level := Choice;
			  Rfn := RootFn;
			  n := 10;
			  Result.Init(20);
			  Temp := 1.0; Iter := 0;
			  REPEAT
				  ERoots(RFn,0.0, MaxPos*Temp, 0.00001, N, J, Result,Err);
				  IF J > 0 THEN TempX := Result.Value(J);
				  Temp := Temp * 2; Iter := iter + 1;
				  IF Err THEN Temp := Temp/4
			  UNTIL (J > 0) OR (Iter > 7);
			  IF J>0 THEN X := TempX ELSE X := 0.0;
			  Result.Free
		  END;
	  Walls :
		  IF Exact THEN
		  BEGIN
			  Temp := Sqrt(Choice/(kOverM*mass))+RestLength;
			  X := Sqrt(Abs(Sqr(Temp) - Sqr(HalfWallSep)))
		  END
		  ELSE
			  X := Sqrt((-VCoefA/2+
								Sqrt(Sqr(VCoefA/2)+VCoefB*(Choice-VCoef0)))/
							(VCoefB/2));
	  Pendulum :
		  BEGIN
		  IF Choice > 2*gOverL*PendL THEN X := -Pi
		  ELSE IF Exact THEN X := ArcCos(1-Choice/(gOverL*PendL))
		  ELSE X := Sqrt( (Sqrt(0.25 + Choice/(6*gOverL))-0.5)*12);
		  END;
	  VanDePol : X := Choice;
	  SHO   : X := Sqrt(2*(Choice)/(kOverM*mass));
	  Cubic : X := Sqrt(((-kOVerM*Mass/2)+
				Sqrt(Sqr(kOverM*Mass/2)+Choice*C3overM*Mass))*2/(C3overM*Mass))
	END; {Case}
END; {GetX}

{----- Calculates an array of potential values for chosen parameters -------}
{-----                and set up scales to display them              -------}

PROCEDURE FindPotential (SetScale : Boolean);
VAR
	i,j : integer;
	xx, MaxV, SDiff : real;
BEGIN
 CASE WhichForce of
 Walls, Pendulum, SHO, Cubic, Asymmetric :
	BEGIN
		SDiff := HalfWallSep - RestLength;
		VCoef0 := kOverM*Mass*Sqr(SDiff);

		VCoefA := kOverM*Mass*2*SDiff/HalfWallSep;

		VCoefB := kOverM*Mass*(1/Sqr(HalfWallSep)	- SDiff/Pwr(HalfWallSep,3));

		VCoefC:=kOverM*Mass*3*(1/Pwr(HalfWallSep,4)+SDiff/Pwr(HalfWallSep,5))/4;

		VCoefD:=kOverM*Mass*5*(1/Pwr(HalfWallSep,6)+SDiff/Pwr(HalfWallSep,7))/8;

		IF WhichForce <> Asymmetric THEN Xminus := -MaxPos
		ELSE 
		IF Exact THEN
			CASE WhichForce of
				Asymmetric : GetX2(Xminus, AsymmetricPotential(MaxPos));
				Cubic : GetX2(Xminus, CubicPotential(MaxPos));
				Pendulum : GetX2(Xminus, ExactPendPotential(MaxPos));
				SHO : GetX2(Xminus, SHOPotential(MaxPos));
				Walls : GetX2(Xminus, ExactWallPotential(MaxPos));
				VanDePol : {No potential};
			END {Case }
		ELSE
			Case WhichForce of
				Pendulum : GetX2(Xminus, ApproxPendPotential(MaxPos));
				Walls : GetX2(Xminus, ApproxWallPotential(MaxPos))
			{The other forces have no approximate expressions}
			END; {Case}

		FOR i := 1 TO 41 DO
		BEGIN
			xx := XMinus+ (i-1) * (MaxPos-Xminus)/40.0;
			xpos.put(i,xx);
			CASE WhichForce of
				Asymmetric :
            BEGIN
					xactpotential.put(i, AsymmetricPotential(xx));
					potential.put(i, AsymmetricPotential(xx))
            END;
				Cubic :
            BEGIN
					xactpotential.put(i, CubicPotential(xx));
					potential.put(i, CubicPotential(xx))
            END;
				Pendulum :
            BEGIN
					xactpotential.put(i, ExactPendPotential(xx));
					potential.put(i, ApproxPendPotential(xx))
            END;
				SHO :
            BEGIN
					xactpotential.put(i, SHOPotential(xx));
					potential.put(i, SHOPotential(xx))
            END;
				Walls :
            BEGIN
					xactpotential.put(i, ExactWallPotential(xx));
					potential.put(i, ApproxWallPotential(xx))
            END;
				VanDePol : {No potential};
			END {Case }
		END; {FOR ix}
		xactpotential.MinMax (j,Eminx,i,MaxV);
		potential.MinMax (j,EminA,i,xx); {Find Minimum Energy; xx and n are dummies}
		If Exact THEN Emin := EminX
		ELSE IF EMinx < EminA THEN Emin := EminX ELSE Emin := EminA;

		xactpotential.AddScalar(-Emin);     {Make lowest energy 0}
		potential.AddScalar(-Emin);     {Make lowest energy 0}
		IF Exact THEN
			xactpotential.MinMax (j,Eminx,i,MaxV)
		ELSE
			potential.MinMax (j,Eminx,i,MaxV);
		IF Not FreezeEnergy THEN MaxVel := 2 * Sqrt(MaxEnergy/Mass);
		IF Not NewEnergyScale THEN
		 If SetScale OR (Exact AND (WhichForce = Walls) AND (MaxV <= 1.01*MaxEnergy)) THEN
		 BEGIN
			IF Not FreezeEnergy THEN MaxVel := 2 * Sqrt(MaxV/Mass);
			MaxEnergy := MaxV;
			IF (WhichForce = Pendulum) AND (MaxPos >= Pi) THEN
				MaxEnergy := 1.5*MaxV;
			IF ((WhichForce = Pendulum) AND (MaxEnergy < 1.2*Energy))
				OR ((WhichForce = Walls) AND (MaxEnergy > 2*Energy)) THEN
					 MaxEnergy := 2*Energy;
			SelectMyViewPort (VPort);
			MyAutoScaling(VSNum, xpos, xactpotential); {Writes no legend unlike the CUPS routine}
		END
	END; {Forces with Potentials}
	VanDePol : ;
 END; {CASE}
END; {FindPotential}

{------ Double well has two turning points, this returns the inner one ------}

PROCEDURE GetX2(VAR X2 :Real; Choice : Real);
VAR Temp, TempX2 : Real;
	 N, J, iter : Integer;
	 Result : Dvector;
	 Err : Boolean;
BEGIN
	Case WhichForce of
	  Walls :
		  IF Exact THEN
		  BEGIN
			  Temp := -Sqrt(Choice/(kOverM*mass))+RestLength;
			  X2 := Sqrt(Abs(Sqr(Temp) - Sqr(HalfWallSep)))
		  END
		  ELSE
			  X2 := Sqrt(Abs((-VCoefA/2
									-Sqrt(Sqr(VCoefA/2)+VCoefB*(Choice-VCoef0)))
								/(VCoefB/2)));
	  Pendulum, VanDePol, SHO, Cubic :
		  X2 := 0.0;
	  Asymmetric :
		  BEGIN
			  Level := Choice;
			  Rfn := RootFn;
			  n := 10;
			  Result.Init(20);
			  Temp := 1.0; Iter := 0;
			  REPEAT
				  ERoots(RFn,0.0, -MaxPos*Temp, 0.00001, N, J, Result, Err);
				  IF J > 0 THEN TempX2 := Result.Value(J);
				  Temp := Temp * 2; Iter := iter + 1;
				  IF Err THEN Temp := Temp/4
			  UNTIL (J > 0) OR (Iter > 7);
			  IF J>0 THEN X2 := TempX2 ELSE X2 := 0.0;
			  Result.Free
		  END;

	END {Case}
END; {GetX2}

{----------- When the pendulum is upside down, its velocity is never zero -----}
{-------------     so an initial velocity is also needed    -------------------}

FUNCTION GetVel(Energy : Real): Real;
VAR Temp : Real;
BEGIN
	Case WhichForce of
	  Walls, VanDePol, SHO, Cubic, Asymmetric : GetVel := 0.0;
	  Pendulum :
		  IF Energy > 2*gOverL*PendL THEN
			  BEGIN
			  Temp:= Sqrt(2*(Energy-2*gOverL*PendL)/Mass);
			  If Not Plus THEN Temp := - Temp;
			  GetVel := Temp
			  END
		  ELSE
			 GetVel := 0.0
	END {Case}
END; {GetVel}

{---------------- Procedure required by CUPSMATH runge Kutta routine ---------}

PROCEDURE Accel(t: real; VAR YF,YPF:DVector);
BEGIN
	vel := YF.value(2);
	YPF.put(1,YF.value(2));
	If Exact THEN
	Case WhichForce of
		Asymmetric : YPF.put(2, AsymmetricAccel(YF.Value(1)));
		Cubic : YPF.put(2, CubicAccel(YF.Value(1)));
		Pendulum : YPF.put(2, ExactPendAccel(YF.Value(1)));
		SHO : YPF.put(2, SHOAccel(YF.Value(1)));
		Walls : YPF.put(2, ExactWallAccel(YF.Value(1)));
		VanDePol : YPF.put(2, VanDePolAccel(YF.Value(1)))
	END {Case}
	ELSE
	Case WhichForce of
		Pendulum : YPF.put(2, ApproxPendAccel(YF.Value(1)));
		Walls : YPF.put(2, ApproxWallAccel(YF.Value(1)))
		{The other forces have now approximate expressions}
	END {Case}
END; {Accel}

{ -------------------- Main Plotting loop ---------------------------}

Procedure ShowTrajectory;
CONST
	PCircleSize = 3; VCircleSize = 3;
VAR
	NCrossings : Integer;
	TNext, VBefore, Peak, Valley : Real;
	Xx : DVector;
	x0, xb, xPrev, x0p, x0sav, x0Energy, xquit,
	 v0, v0sav, vb, t0, t0i, tb, h, temp : Real;
	i, Count, AxisCrossed, MemBlock : Integer;
	ColorCount, ColorMaxCount, RCount, FCount : Integer;
	EnufMem, DoneResonating, FirstAmplitude : Boolean;

{------------- Puts trajectory energy on the potential graph ----------}

  Procedure DrawEnergyLine;
  VAR
	XminEnergy : Real;
  BEGIN
		If Exact THEN temp := xactpotential.Value(21)
			ELSE temp := potential.Value(21);
		IF WhichForce = Asymmetric THEN
		BEGIN
			GetX2(XminEnergy, Energy);
			IF XminEnergy = 0.0 THEN XminEnergy := XMinus;
			Plotline(XminEnergy,Energy,x0Energy,Energy)
		END
		ELSE
		BEGIN
			Xminus := -X0Energy;
			If (Energy >= temp) THEN
			  Plotline(-X0Energy,Energy,x0Energy,Energy) ELSE
			  BEGIN
				 IF Plus OR BothSides THEN Plotline(x0p,Energy,x0Energy,Energy);
				 IF BothSides OR Not Plus THEN  Plotline(-x0Energy,Energy,-x0p,Energy)
			  END
		END
  END; {DrawEnergyLine}

{----- Erases springs and mass during animation, uses Drawsprings ------}

  Procedure RemoveCircle;
  VAR i : Integer;
  BEGIN
	  IF PlotPhase AND CirclePresent THEN
	  BEGIN  {Remove old circle}
		  SelectMyViewPort (PhasePort); SelectScale(PhaseSNum);
		  SetColor(GraphBackColor);
		  DrawCircle(x0,v0,PCircleSize,GraphBackColor)
	  END;
	  SelectMyViewPort (WallPort); SelectScale(WallSNum);
	  CASE WhichForce OF
		  Pendulum : DrawPendulum (x0, Blue);
		  VanDePol : DrawCircle (x0, 0.0, 5, Red);
		  SHO :  DrawCircle (x0, 0.0, 5, Brown);
		  Cubic : DrawCircle (x0, 0.0, 5, Magenta);
		  Asymmetric: DrawCircle (x0, 0.0, 5, Blue)
	  END; {Case}
	  IF WhichForce <> VanDePol THEN
	  BEGIN
		  SelectMyViewPort (VPort); SelectScale(VSNum);
			IF Exact THEN
				CASE WhichForce of
					Asymmetric : DrawCircle(x0, 
						AsymmetricPotential(x0)-Emin,VCircleSize,GraphBackColor);
					Cubic : DrawCircle(x0, 
						CubicPotential(x0)-Emin,VCircleSize,GraphBackColor);
					Pendulum : DrawCircle(x0, 
						ExactPendPotential(x0)-Emin,VCircleSize,GraphBackColor);
					SHO : DrawCircle(x0, 
						SHOPotential(x0)-Emin,VCircleSize,GraphBackColor);
					Walls : DrawCircle(x0, 
						ExactWallPotential(x0)-Emin,VCircleSize,GraphBackColor);
					VanDePol : {No potential};
				END {Case}
			ELSE
				Case WhichForce of
					Pendulum : DrawCircle(x0, 
						ApproxPendPotential(x0)-Emin,VCircleSize,GraphBackColor);
					Walls : DrawCircle(x0, 
						ApproxWallPotential(x0)-Emin,VCircleSize,GraphBackColor);
				{The other forces have no approximate expressions}
				END; {Case}
		  SetColor(White);
		  PlotDVectors(xpos, xactpotential,1,41);
		  IF Not Exact THEN
		  BEGIN SetColor(LightCyan); PlotDVectors(xpos, potential,1,41)
		  END
	  END; {Not VanDePol}
	  IF WhichForce = Walls THEN {Put here to reduce flicker as much as poss}
	  BEGIN
		  SelectMyViewPort (WallPort); SelectScale(WallSNum);
		  DrawSprings (x0, Black, Black)
	  END;
	  CirclePresent := False
  END; {RemoveCircle}

{----------- Draws the mass and springs during animation -------------}

  Procedure NewCircles;
  VAR i : Integer;
		xx0, vv0 : Real;
  BEGIN
	  xx0 := xx.value(1); vv0 := xx.value(2);
	  IF WhichForce <> VanDePol THEN
	  BEGIN
		  SetColor(CurrentColor);
		  SelectMyViewPort (VPort); SelectScale(VSNum);
		  DrawEnergyLine;
			IF Exact THEN
				CASE WhichForce of
					Asymmetric : DrawCircle(xx0, 
						AsymmetricPotential(xx0)-Emin,VCircleSize,LightRed);
					Cubic : DrawCircle(xx0, 
						CubicPotential(xx0)-Emin,VCircleSize,LightRed);
					Pendulum : DrawCircle(xx0, 
						ExactPendPotential(xx0)-Emin,VCircleSize,LightRed);
					SHO : DrawCircle(xx0, 
						SHOPotential(xx0)-Emin,VCircleSize,LightRed);
					Walls : DrawCircle(xx0, 
						ExactWallPotential(xx0)-Emin,VCircleSize,LightRed);
					VanDePol : {No potential};
				END {Case}
			ELSE
				Case WhichForce of
					Pendulum : DrawCircle(xx0,
						ApproxPendPotential(xx0)-Emin,VCircleSize,LightRed);
					Walls : DrawCircle(xx0, 
						ApproxWallPotential(xx0)-Emin,VCircleSize,LightRed);
				{The other forces have no approximate expressions}
				END {Case}
	  END; {Not VanDePol}
	  IF PlotPhase THEN
	  BEGIN
		  SelectMyViewPort (PhasePort); SelectScale(PhaseSNum);
		  DrawCircle(xx0,vv0,PCircleSize,LightMagenta)
	  END;
	  SetColor(White);
	  SelectMyViewPort (WallPort); SelectScale(WallSNum);
	  CASE WhichForce OF
		  Walls :
			 BEGIN
			  If Abs(xx0) > Abs(Equilibrium) THEN TempColor := Green
			  ELSE TempColor := LightMagenta;
			  DrawSprings (xx0, TempColor, Red)
			 END;
		  Pendulum : DrawPendulum (xx0, White);
		  VanDePol, SHO, Cubic, Asymmetric : DrawCircle (xx0, 0.0, 5, White)
	END; {Case}
	CirclePresent := True
  END; {NewCircles}

  VAR TimeOffset, MaxA, MinA, xnew, vnew : Real;
		SaveCross : Array[1..100] of Real;
		IErase  : Integer;
		Erasing : Boolean;

		Gx, Gy, GxM, GyM : Integer;
		yM :integer;
		Sign : Real;
		iMap : Integer;
		NeedQuitMessage : Boolean;

  PROCEDURE DrawPoincare; {Special fast loop doesn't have to make tests or draw}
  BEGIN
	 SelectMyViewPort(PoincarePort);
	 SelectScale(PhaseSNum);
	 REPEAT
	 StepRK4(AccelP, t,h, Xx);
	 xb := x0; vb := v0; tb := t0;
	 x0 := Xx.value(1); v0 := Xx.value(2); t0 := t;
	 IF (WhichForce = Pendulum) AND (Abs(x0) > Pi) THEN
	 BEGIN
		 Sign := Abs(x0)/x0;
		 x0 := x0 - Sign*TwoPi;
		 xb := xb - Sign*TwoPi;
		 xx.put(1,x0);
	 END; {If WhichForce}
	 IF Abs(t - TNext) < h/2 THEN
	 BEGIN
		 IF NPhase = 0 THEN
		 BEGIN
			 MapLength := MapLength + 1;
			 iMap := MapLength Mod PMapSize;
			 IF iMap = 0 THEN
				Message('Poincar‚ circular buffer full; new points replace old ones:'+
						  NumStr(MapLength Div PMapSize,4,0)+' times over.');
			 Map(x0,v0,Gx,Gy);
			 IF LargePoincare THEN DrawCircle(x0,v0,1,White)
				ELSE PutPixel(Gx,Gy,White);
			 IF DemoPoincare THEN
				 DrawCircle(x0,v0,PCircleSize,LightRed);
		 END; {IF NPhase = 0}
		 Map(x0,v0,GxM,GyM); {Saved Maps are large screen}
		 {$R-}   {This pointer always generates a range check}
		 XMapPtr[NPhase+1]^[iMap] := GxM;
		 VMapPtr[NPhase+1]^[iMap] := GyM;
		 {$R+}
		 NPhase := (NPhase+1) MOD NPhases;
		 TNext := TNext + TwoPi/(Omega*NPhases)
	 END; {IF t - TNEXT}
	 CheckForEvents;
	 If Event.ReadKey in ['C','c'] THEN
	 BEGIN
		 SetColor(White);
		 ClearPhasePort;
		 SetMyColor(rainbow[0].colors[NColor]);
		 Event.KeyPressed := False
	 END;
	 If Event.ReadKey in ['P','p'] THEN
	 BEGIN
		 Message ('Click or press any key to continue.');
		 StaticPause;
		 Debounce;
		 NeedQuitMessage := True
	 END;
	 IF NeedQuitMessage THEN
	 BEGIN
		Message('Click Mouse or Press any Key to Stop, C to Clear & Continue, P to Pause');
		NeedQuitMessage := False;
	 END
	 UNTIL Event.KeyPressed OR Event.MouseClicked
			 OR (Searching AND (MapLength >= SMapSize))
  END; {DrawPoincare}

{---------- Takes one time step and plots it on all the graphs -------------}

  Procedure ExtendPhaseCurve;
  VAR
	  IFlag, i, n : Integer;
  BEGIN
	  If Animating THEN
		  RemoveCircle;
	  StepRK4(AccelP, t,h, Xx);
	  If Animating THEN
	  BEGIN
		  NewCircles;
		  Delay(DelayTime)  END;
	  SetColor(CurrentColor);
	  IF PlotPhase THEN
	  BEGIN
		  SelectMyViewPort (PhasePort); SelectScale(PhaseSNum);
		  BEGIN
			  PlotLine (xb,vb, X0, v0);
			  PlotLine (x0,v0, Xx.value(1),Xx.value(2))
		  END
	  END; {IF PlotPhase}
	  IF PlotVel THEN
	  BEGIN
		  SelectMyViewPort (VelocityPort); SelectScale(VelSNum);
		  If Erasing THEN
		  BEGIN
			  SetColor(GraphBackColor);
			  PlotLine (Ts[(IErase) MOD (HistoryCount+1)],
							Vs[(IErase) MOD (HistoryCount+1)],
							Ts[(IErase+1) MOD (HistoryCount+1)],
							Vs[(IErase+1) MOD (HistoryCount+1)]);
			  SetColor(CurrentColor);
		  END; {Erasing}
		  PlotLine (tb-TimeOffset,vb, t0-TimeOffset, v0);
		  PlotLine (t0-TimeOffset,v0, t-TimeOffset, Xx.Value(2))
	  END;
	  IF PlotTime THEN
	  BEGIN
		  SelectMyViewPort (TimePort); SelectScale(TimeSNum);
		  IF TimePlotInverted THEN
		  BEGIN
			  PlotLine (xb,tb-TimeOffset,x0, t0-TimeOffset);
			  PlotLine (x0,t0-TimeOffset,Xx.value(1), t-TimeOffset)
		  END
		  ELSE BEGIN
			  If Erasing THEN
			  BEGIN
				  SetColor(GraphBackColor);
				  PlotLine (Ts[(IErase) MOD (HistoryCount+1)],
								Xs[(IErase) MOD (HistoryCount+1)],
								Ts[(IErase+1) MOD (HistoryCount+1)],
								Xs[(IErase+1) MOD (HistoryCount+1)]);
				  SetColor(CurrentColor);
			  END; {Erasing}
			  PlotLine (tb-TimeOffset,xb, t0-TimeOffset, x0);
			  PlotLine (t0-TimeOffset,x0, t-TimeOffset, Xx.value(1))
		  END {TimePlotInverted ELSE}
	  END; {PlotTime}
	  xb := x0; vb := v0; tb := t0;
	  x0 := Xx.value(1); v0 := Xx.value(2); t0 := t;
	  IF DoErase OR Resonating THEN
	  BEGIN
		  IErase := (IErase + 1) MOD (HistoryCount+1);
		  IF Resonating THEN
		  BEGIN
			  IF x0 > Peak THEN Peak := x0; IF x0 < Valley THEN Valley := x0
		  END
	  END; {DoErase}
	  IHist := IHist + 1;
	  IF IHist > HistoryCount THEN
	  BEGIN
			Ihist := 0;
			IF Ts[HistoryCount] < TimeScale THEN DoErase := False;{would foul up}
			Wrapped := True
	  END;
	  Xs[IHist] := x0; Vs[IHist] := v0; Ts[IHist] := t0-TimeOffset;
	  IF HistFileOpen Then Writeln (HistFile,t0,', ', x0,', ', v0);
	  IF TimePlotInverted THEN Delay(DelayTime);
	  IF ((vb = 0.0) OR (v0/vb < 0.0))
			AND ((NOT Resonating) OR (x0/xPrev < 0.0)) THEN
	  BEGIN
			 AxisCrossed := ((AxisCrossed) MOD 100) + 1;
			 xPrev := x0; IF xPrev = 0.0 THEN xPrev := -xPrev;
			 IF v0 = vb THEN v0 := v0 + 1.0e-12; {prevent division by zero}
			 SaveCross[AxisCrossed] := tb+Abs(vb)*(t-tb)/Abs(v0-vb);
		 IF Resonating AND (x0 < 0.0) THEN
			 BEGIN
				 ThisAmp := Peak-Valley;
				 IF (Abs((ThisAmp)/2 -Amplitude)/Amplitude <= ResCrit)
					OR ((ThisAmp+LastAmp)/Amplitude <= ResCrit) THEN
						 RCount := Rcount + 1  {Resonance Criterion met}
				 ELSE
					 RCount := RCount - 1;
				 LastAmp := ThisAmp;
				 IF RCount < 0 THEN RCount := 0;
				 IF RCount >= 5 THEN
					 DoneResonating := True
				 ELSE IF FirstAmplitude THEN
					BEGIN
						Amplitude := ThisAmp/2;
						EarlyAmp := Amplitude;
						IF FCount < 3 THEN FCount := FCount + 1
						ELSE FirstAmplitude := False
					END
					ELSE
					Amplitude := (5*Amplitude + (ThisAmp)/2)/6;
				 Message('Average Amplitude: '+Numstr(Amplitude,4,2)
							+', Latest: '+Numstr((Peak-Valley)/2,4,2)
							+'; Press Q to quit, G to go to next point.');
				 Temp := Peak; Peak := Valley; Valley := Temp;
			 END; {Resonating AND}
			 ColorCount := ColorCount + 1;
			 If ColorCount > ColorMaxCount THEN
			 BEGIN {This only has an effect if continuous is true}
				 SetMyColor (NColor);
				 NColor := NColor + 1;
				 If NColor > 15 Then NColor := 9;
				 If rainbow[0].colors[NColor] = GraphBackColor THEN NColor := NColor + 1;
				 ColorCount := 0
			 END {If ColorCount}
	  END {Axis Crossed}
  END; {ExtendPhaseCurve}

{---Inner Loop till time to save Poincar‚ points or cycle time graphs -----}

  Procedure DrawPhaseCurve;
  BEGIN
	 REPEAT
		 ExtendPhaseCurve;
		 IF (WhichForce = Pendulum) AND (Abs(x0) > Pi) THEN
		 BEGIN
			 Sign := Abs(x0)/x0;
			 If Animating THEN RemoveCircle;
			 x0 := x0 - Sign*TwoPi;
			 xb := xb - Sign*TwoPi;
			 xx.put(1,x0);
			 AxisCrossed := (AxisCrossed MOD 100)+ 1
		 END; {If WhichForce}
		 IF Abs(t - TNext) < h/2 THEN
		 BEGIN
			 If T-TimeOffset > TimeScale THEN
			 BEGIN 
				 TimeOffset := TimeOffset + TimeScale;
				 IF NOT Erasing THEN
					IErase := 1;
				 If DoErase THEN Erasing := True;
			 END; {TimeOffset >}
			 IF NPhase = 0 THEN
			 BEGIN
				 MapLength := MapLength + 1;
				 iMap := MapLength Mod PMapSize;
				 IF PlotPoincare THEN
				 BEGIN
					 SelectMyViewPort(PoincarePort);
					 SelectScale(PhaseSNum);
					 Map(x0,v0,Gx,Gy);
					 IF LargePoincare THEN DrawCircle(x0,v0,1,White)
						ELSE PutPixel(Gx,Gy,White);
					 IF DemoPoincare THEN
 	             BEGIN
 						 DrawCircle(x0,v0,PCircleSize,LightRed);
						 IF PlotPhase THEN
						 BEGIN
							 SelectMyViewPort(PhasePort);
							 DrawCircle(x0,v0,PCircleSize,LightRed);
							 Delay(10*DelayTime)
	                END {PlotPhase}
					 END {DemoPoincare}
				 END;
			 END; {IF NPhase = 0}
			 SelectMyViewPort({Left:} 1);
			 SelectScale(PhaseSNum);
			 Map(x0,v0,GxM,GyM); {Saved Maps are large screen}
			 {$R-}
			 XMapPtr[NPhase+1]^[iMap] := GxM;
			 VMapPtr[NPhase+1]^[iMap] := GyM;   {$R+}
			 NPhase := (NPhase+1) MOD NPhases;
			 If PlotReturn THEN
			 BEGIN
				 SelectMyViewPort(ReturnPort);
				 SelectScale(ReturnSNum);
				 Map(vbefore,v0,Gx,Gy);
				 IF NPhase = 0 THEN
					 IF LargePoincare THEN DrawCircle(x0,v0,1,White)
						ELSE PutPixel(Gx,Gy,White)
			 END;
			 SetColor(CurrentColor);
			 VBefore := v0;
			 TNext := TNext + TwoPi/(Omega*NPhases)
		 END; {IF t - TNEXT}
		 CheckForEvents;
		 If Event.ReadKey in ['C','c'] THEN
		 BEGIN
			 SetColor(White);
			 ClearPhasePort;
			 SetMyColor(rainbow[0].colors[NColor]);
			 Event.KeyPressed := False
		 END;
		 If Event.ReadKey in ['P','p'] THEN
		 BEGIN
			 Message ('Click or press any key to continue.');
			 StaticPause;
			 Debounce;
			 NeedQuitMessage := True
		 END;
		 IF NeedQuitMessage AND (AxisCrossed >= 3) AND (Not InTutorial) THEN
		 BEGIN
			Message('Click Mouse or Press any Key to Stop, C to Clear & Continue, P to Pause');
			NeedQuitMessage := False;
		 END
	 UNTIL (Continuous AND
				((Event.KeyPressed OR Event.MouseClicked) AND (Not Resonating)))
		 OR (Resonating AND (Event.ReadKey in ['G','g','Q','q']))
		 OR (Not Continuous
				  AND (((AxisCrossed >= NCrossings) AND Not TimeLimit)
						  OR (Searching AND (MapLength >= SMapSize))))
		 OR (TimeLimit AND (T >= TimeScale))
		 OR ((Event.ReadKey in ['Q','q','G','g',Chr(27)]) AND (Not Resonating))
		 OR DoneResonating;
			 {First axis crossing occurs at beginning}
	 ExtendPhaseCurve;  {Insure closure of curve}
  END; {DrawPhaseCurve}

{---------------- Outer loop, initialization, closing all graphs ---------}

BEGIN {ShowTrajectory}
	Debounce;
	FirstAmplitude := True; FCount := 0;
	NeedQuitMessage := False;
	If Continuous AND (Not InTutorial) THEN
		Message('Click Mouse or Press any Key to Stop, C to Clear & Continue, P to pause.');
	{$IFDEF test} Message('Testing: test is defined'); {$ENDIF}
	If PlotTime OR PlotVel Then NCrossings := 2*ShowPeriods + 1
		ELSE NCrossings := 3;
	IF Searching THEN NCrossings := 30000; {Then MapLength controls}
	x0 := Equilibrium; If Animating THEN RemoveCircle;
	Xx.init(2);
	DoneResonating := False; Peak := 0.0; Valley := 0.0;
	Amplitude := 0.01; RCount := 0;
	MemBlock:= PMapSize*SizeOf(Integer);
	EnufMem := True;
	EnufMem := MaxAvail >= 3*MemBlock;
	While EnufMem AND (MapsUsed < NPhases) AND (MapsUsed < MaxPhases) DO
	BEGIN
		MapsUsed := MapsUsed + 1;     {$R-}
		GetMem(XMapPtr[MapsUsed], MemBlock);
		GetMem(VMapPtr[MapsUsed], MemBlock);  {$R+}
		EnufMem := MaxAvail >= 3*MemBlock;
	END; {While}
	If MapsUsed < NPhases THEN
	BEGIN
		NPhases := MapsUsed;
		NeedQuitMessage := Continuous;
		If Not InTutorial THEN
		 Message('Number of frames in cycle reduced to '+NumStr(NPhases,2,0)
					+' to fit memory available.')
	END;
	NMap := MapsUsed;
	If NPhases < NMap THEN NMap := NPhases; {In case another instance used more}
		{MapsUsed keeps track of memory usage
		 NPhases controls the calculation
		 NMap is used for flipping later
			  and could differ if NPhases read from a file}
	If NPhases < 1 Then NPhases := 1;
	t := 0; TimeOffset := 0;
	h := TimeStep; {Time step}
	IF F0overM > 0 THEN
		h := TwoPi/(Omega*NPhases*(Trunc(DrivenTimeFactor/NPhases) + 1));
	TimeStepUsed := h;
	ColorCount := 0;
	ColorMaxCount := 20;
	If WhichForce = VanDePol THEN x0:= XStart
		ELSE GetX(x0, Energy+Emin);
	x0Energy := x0;
	IF (WhichForce = Asymmetric) AND (Not Plus) THEN
	BEGIN
		GetX2(x0, Energy+Emin);
		x0 := - x0 {inverts again later}
	END;
	GetX2(x0p, Energy+EMin);
	IF x0 = 0.0 THEN x0 := x0p; {Happens with asymmetric force only local min}
	v0 := GetVel(Energy+Emin);
	t0i := 0.0;
	IF Resonating OR PhasePicked THEN {Res:Simulate adiabatic change with omega}
		BEGIN t0i := t0Old; x0 := x0Old; v0 := v0Old; Plus := True END;
	x0sav := x0; xPrev := x0;
	v0sav := v0;
	t0 := 0.0;
	Erasing := False;
	IErase := 0; IHist := 0; Wrapped := False;
	Xs[IHist] := x0; Vs[IHist] := v0; Ts[IHist] := t0;
	SelectMyViewPort(VPort);
	SelectScale(VSNum);
	SetColor(CurrentColor);
	If (WhichForce <> VanDePol) AND (Not Resonating) THEN
		DrawEnergyLine; {Must precede following choice to set temp}
	If BothSides AND (Energy < temp) THEN
		BEGIN Count := 0; Plus := True END
			ELSE Count := 1;
	REPEAT
	 t0 := 0; t := 0; tb := 0;
	 TNext := t+TwoPi/(Omega*NPhases);
	 NPhase := 0;
	 MapLength := 0;
	 x0 := x0sav;
	 v0 := v0sav;
	 VBefore := v0;
	 If Not Plus THEN x0 := - x0;
	 xquit := x0;
	 Xx.put(1,x0); Xx.put(2,v0);
	 xb := x0; vb := v0;
	 SetColor(CurrentColor); 
	 AxisCrossed := 0;
	 IF (Plots = 1) AND PlotPoincare AND (Not Animating) THEN
		DrawPoincare
	 ELSE	
		DrawPhaseCurve;
	 If (Not Continuous) OR Resonating THEN
	 BEGIN
		 Period := t/(ShowPeriods);
		 If (AxisCrossed > 1) THEN
			 Period := 2*(SaveCross[AxisCrossed] - SaveCross[1])/
							(AxisCrossed-1);
		 If (WhichForce = Pendulum) AND (Energy > 2*gOverL) THEN
			 Period := Period*0.4;
		 DisplayPeriod(Period)
	 END;
	 IF HistFileOpen THEN BEGIN Close(HistFIle);HistFileOpen := False END;
	 If Animating THEN RemoveCircle;
	 Count := Count + 1;
	 If BothSides AND (Energy < temp) THEN Plus := Not Plus
	UNTIL Count > 1;
	xx.free;
	If Animating THEN
	BEGIN
		SelectMyViewPort (WallPort); SelectScale(WallSNum);
		CASE WhichForce OF
			Walls :
			  BEGIN
				If Equilibrium = 0.0 THEN TempColor := Green
				ELSE TempColor := LightMagenta;
				DrawSprings (Equilibrium, TempColor, Red)
			  END;
			Pendulum : DrawPendulum (0.0, White);
			VanDePol, SHO, Cubic, Asymmetric : DrawCircle (0.0, 0.0, 5, White)
		END {Case}
	END; {If Animating}
	IF Resonating THEN  {Simulates adiabtic change with omega}
		BEGIN x0Old := x0; v0Old := v0;
			t0Old := t-Trunc(omega*t/TwoPi)*twoPi/omega
		END;
	IF MapLength > MaxMapLength THEN MapLength := MaxMapLength;
	If (Not Searching) AND ShowMenu THEN
	BEGIN
		DisplayMainMenu;
		ShowMouse
	END {Not Searching}
END; {ShowTrajectory}

PROCEDURE SavePMap(FName : String);
VAR
	i, j : Integer;
	PFile : File of PMapRec;
	PMap : PMapRec;
BEGIN
	IF FName = 'ABORT' THEN BEGIN Message ('File not written'); Exit END;
	Assign(PFile, FName);
	ReWrite(PFile);
	With PMap DO
	BEGIN
		ID := 0;
		Force := WhichForce;
		CASE WhichForce OF
			Walls, SHO, Cubic : BEGIN fk := koverm; fm := mass; Separation := HalfWallSep END;
			Pendulum : BEGIN fg := gOverL; PLength := PendL END;
			VanDePol : BEGIN vm := mass; vk := koverm; vX0 := VDepX0 END;
			Asymmetric : BEGIN am := mass; fak := koverm; k3 := AsCoef3; k4 := AsCoef4 END;
		END; {Case WhichForce}
		NFrames := NPhases;
		NPoints := MapLength;
		Ener := Energy;
		Drag := DoverM;
		Driv := F0overM;
		Omeg := Omega;
		For i := 1 to NPhases DO
		{$R-}
		BEGIN
			Frame := i;
			FOR j := 1 to MapLength DO
			BEGIN
				XMap[j] := XMapPtr[i]^[j];
				VMap[j] := VMapPtr[i]^[j]
			END;
			FOR j := MapLength+1 TO PMapSize DO
			BEGIN
				XMap[j] := 0;
				VMap[j] := 0;
			END;
			Write (PFile, PMap)
		END {For i}
		{$R+}
	END; {With PMap}
	Close(PFile);
	Continuous := True;
	ClearPhasePort;
	DisplayMainMenu;
	ShowMouse
END; {SavePMap}

PROCEDURE ReadPMap(FName : String; Flip : Boolean; VAR Error : Boolean;
				VAR FramesUsed, MapLength : Integer);
VAR
	i, j, OtherMapsUsed, MemBlock : Integer;
	temp : Real;
	PFile : File of PMapRec;
	PMap : PMapRec;
	EnufMem : Boolean;
BEGIN
	IF FName = '' THEN Exit;
	Error := False;
	Assign(PFile, FName);
	ReSet(PFile);
	IF Eof(Pfile) THEN
	BEGIN
		Message('File is empty. Click or ENTER.');
		Error := True;
		StaticPause;
		Exit
	END
	ELSE Read (Pfile, PMap);
	NPhases := PMap.NFrames;
	MemBlock:= PMapSize*SizeOf(Integer);
	EnufMem := True;
	EnufMem := MaxAvail >= 3*MemBlock;
	OtherMapsUsed := FramesUsed;
	While EnufMem AND ((MapsUsed-FramesUsed) < NPhases)
		AND (MapsUsed < MaxPhases) DO
	BEGIN
		MapsUsed := MapsUsed + 1;
		GetMem(XMapPtr[MapsUsed], MemBlock);
		GetMem(VMapPtr[MapsUsed], MemBlock);
		IF (MapsUsed-FramesUsed < NPhases) AND (MapsUsed < MaxPhases) THEN
			EnufMem := MaxAvail >= 3*MemBlock;
		IF Not ENufMem THEN
		BEGIN
			Message('Not enough Memory');
			Close(PFile);
			StaticPause; ClearPhasePort; Error := True; Exit
		END
	END; {While}
	Error := False;
	NMap := NPhases;
	IF MapsUsed-FramesUsed < NMap THEN NMap := MapsUsed-FramesUsed;
	With PMap DO
	BEGIN
		MainMenu.RowChecked(4,Ord(WhichForce)+1, False);
		WhichForce := Force;
		MainMenu.RowChecked(4,Ord(WhichForce)+1, True);
		CASE WhichForce OF
			Walls, SHO, Cubic : BEGIN
				koverm := fk; mass := fm; HalfWallSep := Separation END;
			Pendulum : BEGIN gOverL := fg; PendL := PLength END;
			VanDePol : BEGIN mass := vm; koverm := vk; VDepX0 := VX0 END;
			Asymmetric : BEGIN koverm := fak; AsCoef3 := k3; AsCoef4 := k4; Mass := am END;
		END; {Case WhichForce}
		InTutorial := True; {turn off inquiry about automation, etc}
		ChooseSystem({Clear: } False);
		InTutorial := False;
		MapLength := NPoints;
		Energy := Ener;
		DoverM := Drag;
		F0overM := Driv;
		Omega := Omeg;
		For i := FramesUsed + 1 to FramesUsed + NMap DO
		{$R-}
		BEGIN
			FOR j := 1 to MapLength DO
			BEGIN
				XMapPtr[i]^[j] := XMap[j];
				VMapPtr[i]^[j] := VMap[j]
			END;
			IF i-FramesUsed < NMap THEN
				 Read (PFile, PMap) {Otherwise reads an extra record}
		END {For i}
		{$R+}
	END; {With PMap}              
	Close(PFile);
	FramesUsed := NMap;
	IF Flip THEN               
	BEGIN
		DrawSystem;
		GetX(temp, Energy+Emin);
		IF temp = 0.0 THEN {Happens if asymmetric potentialhas only local minimum}
		BEGIN
			GetX2(temp, Energy+Emin);
			temp := Abs(temp)
		END;
		FindPotential(True);
		DisplayPotential(False, False);
		DisplayChoice;
		DisplayDissip;
		Continuous := True;
		FlipEm({Slow:} False, {Return:} False);
		Continuous := True;
		Event.ReadKey := Chr(0); {Otherwise event persists}
		ShowMouse
	END {IF Flip}
END; {ReadPMap}

Procedure FlipTwo(MapLen1, MapLen2, FUsed1 ,FUsed2: Integer);
VAR
	i, ib, ibOld, j, DTime, Frames, MLen :Integer;
	Done, Pausing : Boolean;
	Key : Byte;
BEGIN
	ShowPoinAlone;  {Sets Scales properly}
	OpenTwoPoincare;
	SetUpFlipKeys;
	Message ('Use F keys to control as indicated.');
	DTime := 0; Done := False; Pausing := False;
	Frames := FUsed1; IF FUsed2 < FUsed1 THEN Frames := FUsed2;
	MLen := MapLen1; IF MapLen2 < MapLen1 THEN MLen := MapLen2;
	ib := 1; i := 1;
	REPEAT {Until Done}
		 CloseMyViewPort(PoincarePort);
		 For j := ib to MLen DO
		 BEGIN
			{$R-}
			 PutPixel(XMapPtr[i+FUsed1]^[j], (VMapPtr[i+FUsed1]^[j]-150) DIV 2+225, White);
			 PutPixel(XMap2Ptr[i]^[j], (VMap2Ptr[i]^[j]-150) DIV 2+75, White);
			{$R+}
	 END; {For j}
		 ShowMouse;
		 Delay(DTime);
		 CheckForEvents;
		 IF Pausing THEN 
		 BEGIN
			 Message ('Pause. To step one frame, Press any key or click mouse.');
			 DynamicPause
		 END;
  		 ibold := ib;
		 IF FlipKeys.Pressed(key) THEN DoFlipKeys(key,ib,DTime,Done,Pausing);
		 IF DTime < 0 THen DTime := 0;
		 IF ib > MapLength - 10 THEN ib := MapLength - 10;
		 IF (ib = ibold) THEN i := i + 1;
		 IF i > Frames THEN i := 1
	UNTIL Done;
	DisplayMainMenu
END; {FlipTwo}

PROCEDURE Read2PMaps(FName1, FName2 : String);
CONST Allocated : Boolean = False;
VAR i, MapLen1, MapLen2, FUsed1, FUsed2 : Integer;
	 Error : Boolean;
	 temp :Real;
	 FramesUsed : Integer;
BEGIN
	IF (FName1 = '') OR (FName2 = '') THEN Exit;
	FUsed1 := 0;
	ReadPMap(FName1, {Flip=} False, Error, FUsed1, Maplen1);
	If Error THEN Exit;
	If Not Allocated THEN
	BEGIN
		For i := 1 to FUsed1 DO
		BEGIN
			XMap2Ptr[i] := XMapPtr[i];
			VMap2Ptr[i] := VMapPtr[i];
		END;
	END; {If Not ALlocated}
	FUsed2 := FUsed1; {Initial value needed, will be changed}
	ReadPMap(FName2, {Flifp=} False, Error, FUsed2, MapLen2);
	If Error THEN Exit;
	Allocated := True;
	FlipTwo(MapLen1, MapLen2, FUsed1 ,FUsed2);
	DrawSystem;
	GetX(temp, Energy+Emin);
	IF temp = 0.0 THEN {Happens if asymmetric potentialhas only local minimum}
	BEGIN
		GetX2(temp, Energy+Emin);
		temp := Abs(temp)
	END;
	IF Temp > 0.0 THEN MaxPos := Temp;
	FindPotential(True);
	DisplayPotential(False, False);
	DisplayChoice;
	DisplayDissip;
	Continuous := True;
	ClearPhasePort
END; {Read2PMaps}

PROCEDURE SaveSMap(VAR SFile : SFileTyp; Count : Integer);
VAR
	i, j : Integer;
	SMap : SMapRec;
BEGIN
	With SMap DO
	BEGIN
		ID := Count;
		Force := WhichForce;
		CASE WhichForce OF
			Walls, SHO, Cubic : 
				BEGIN fk := koverm; fm := mass; Separation := HalfWallSep END;
			Pendulum : BEGIN fg := gOverL; PLength := PendL END;
			VanDePol : BEGIN vm := mass; vk := koverm; vX0 := VDepX0 END;
			Asymmetric : BEGIN am := mass; fak := koverm; k3 := AsCoef3; k4 := AsCoef4 END;
		END; {Case WhichForce}
		NFrames := 1;
		NPoints := MapLength;
		Ener := Energy;
		Drag := DoverM;
		Driv := F0overM;
		Omeg := Omega;
		i := 1; {Only store one map}
		Frame := i;
		{$R-}
		FOR j := 1 to MapLength DO
		BEGIN
			XMap[j] := XMapPtr[i]^[j];
			VMap[j] := VMapPtr[i]^[j]
		END;
		FOR j := MapLength+1 TO SMapSize DO
		BEGIN
			XMap[j] := 0;
			VMap[j] := 0;
		END;
		{$R+}
		Write (SFile, SMap)
	END; {With SMap}
END; {SaveSMap}

PROCEDURE ReadSMap(FName : String);
CONST Maps = 20;
TYPE SMapPtr = ^SMapRec;
VAR
	i, j, MemBlock, WhichMap, NMaps, BufWhere, Back,
		First, Last, Len, CountOut : Integer;
	Temp : Real;
	FName1 : String;
	SFile, SFile1 : File of SMapRec;
	SMapN, Dummy : SMapRec;
	SMapPtrs : Array[1..Maps] of SMapPtr;
	EnufMem, UseBuffer, BufFull, Open, FExists, AllIn, Finished : Boolean;
	DirInfo : SearchRec;
	SearchKeys : THotKeys;
	key: Byte;

	PROCEDURE SetKeysRunning;
	BEGIN
		WITH SearchKeys DO
		BEGIN
			Init(3);
			key[1] := 'F1-Help';
			key[2] := 'F10-Quit';
			key[3] := 'UP-Back';
			Display;
	  END
	END; {SetKeysRunning}

	PROCEDURE SetKeysPausing;
	BEGIN
		WITH SearchKeys DO
		BEGIN
			Init(10);
			key[1] := 'F1-Help';
			key[2] := 'F2-More';
			key[3] := 'F3-Save';
         key[4] := 'F4-Run';
			key[5] := 'F10-Quit';
         key[6] := 'UP-Back';
			key[7] := 'DOWN-Forward';
			key[8] := 'PGUP-1st Half';
			key[9] := 'PGDN-2nd Half';
			key[10]:= 'HOME-All';
			Display
	  END
	END; {SetKeysPausing}


BEGIN
	If FName = '' Then Exit;
	FName1 := '';
	Assign(SFile, FName);
	ReSet(SFile);
	F0overM := 0.1; {So Poincare Map will display}
	Continuous := True; {with proper scale}
	Finished := False;
	Plots := 1;
	SetAllFalse;
	PlotPoincare := True;
	SetPlots;
	MemBlock:= SizeOf(SMapN);
	EnufMem := MaxAvail >= 1.5*MemBlock;
   ReadSearch := True;
	AllIn := False;
	WhichMap := 0;
	While EnufMem AND (WhichMap < Maps) DO
	BEGIN
		WhichMap := WhichMap + 1;
		GetMem(SMapPtrs[WhichMap],MemBlock);
		EnufMem := MaxAvail >= 1.5*MemBlock;
	END; {While}
	NMaps := Maps;
	IF WhichMap < NMaps THEN NMaps := WhichMap;
	WhichMap := 0;
	Open := False; Back := 1; UseBuffer := False; BufFull := False;
	SetKeysRunning;
	First := 1; Last := SMapSize;
	IF Eof(Sfile) THEN
	BEGIN Message('File is empty. Click or ENTER.'); StaticPause END
	ELSE With SMapN DO
	REPEAT {Until Eof(SFile)}
		Back := 0;
		CheckForEvents;
		IF  Event.Readkey = ' ' THEN 
		BEGIN
			Back := 0;
			UseBuffer := True;
			SearchKeys.Done;
			SetKeysPausing
		END;
		IF SearchKeys.Pressed(key) THEN
		Case Key of
			1 : BEGIN
				 SearchKeys.Clear; Message ('');
				 IF UseBuffer THEN
					ShowHelp('anharm.hlp','SearchPauseHelp;')
					ELSE ShowHelp('anharm.hlp','SearchFileHelp;');
				 StaticPause;
				 CloseMyViewPort(AllPort);
				 GraphBackColor := darkGray;
				 NewSystem({Clear:} True);
				 SearchKeys.Display;
				 DisplayChoice
				 END;
			2 : If UseBuffer THEN
					BEGIN
						IF Not AllIn THEN
						BEGIN
							UseBuffer := False;
							SearchKeys.Clear;
							SearchKeys.done;
							SetKeysRunning
						END
					END
					ELSE Finished := True;
			5 : Finished := True;
			6 : BEGIN Back := -1; UseBuffer := True END;
			7 : BEGIN Back :=  1; UseBuffer := True END;
			8 : If UseBuffer THEN
					BEGIN First := 1; Last := MapLength DIV 2 END;
			9 : If UseBuffer THEN
					BEGIN First := MapLength DIV 2; Last := MapLength end;
		  10 : BEGIN First := 1; Last := SMapSize END;
			4 : BEGIN
						 SearchKeys.Clear;
						 ShowFour({Return :} True);
						 SetPlots;
						 ClearPhasePort;
						 NPhases := 12;
						 Continuous := True; Animating := False;
						 ShowTrajectory;
						 FlipEm({Slow:} False, {Return:} False);
						 ShowPoinAlone;
						 SearchKeys.Display
						 END;
			3 : IF NOT UseBuffer THEN
					BEGIN
						Back := -1;
						UseBuffer := True;
						SearchKeys.Done;
						SetKeysPausing
					END
					ELSE
					BEGIN
					 If Not Open THEN
					 BEGIN
						 IF FName1 = '' THEN Fname1 := FName;
						 While Pos ('\',FName1) > 0 DO
							Delete (FName1, 1, Pos ('\',FName1));
						 Len := Length(FName1);
						 IF (Len < 12) OR (FName1[Len-4] = 'Z') THEN
							 Insert ('A', FName1, Len-3)
							 ELSE FName1[Len-4] := Succ(FName1[Len-4]);
						 If Length(FName1) = 13 THEN Delete(FName1,8,1);
						 CountOut := 1;
						 FExists := False;
						 FindFirst (FName1, AnyFile, DirInfo);
						 IF DosError = 0 THEN FExists := True;
						 Assign(SFile1, FName1);
						 IF FExists THEN
						 BEGIN
							 ReSet(SFile1);
							 REPEAT
								 CountOut := CountOut + 1;
								 Read(SFile1, Dummy)
							 UNTIL Eof(SFile1)
						 END ELSE
							 ReWrite(SFile1);
						 Open := True;
					 END; {Not Open}
					 ID := CountOut;
					 Write (SFile1, SMapN);
					 CountOut := CountOut + 1;
					 Back := 0;
				 END; {Case 3 IF Usebuffer}
		END; {Case}
		IF Not UseBuffer THEN
			BEGIN
				WhichMap := WhichMap + 1;
				IF WhichMap > Maps THEN
					BEGIN WhichMap := 1; BufFull := True END;
				IF Not Eof(SFile) THEN Read (SFile, SMapN);
				{$R-}
				SMapPtrs[WhichMap]^ := SMapN;
				{$R+}
				BufWhere := WhichMap;
				Message ('Press SPACEBAR to Pause. Index: '+NumStr(ID,3,0)+
							' Press ALT-y to slow down.');
			END
			ELSE BEGIN
			 BufWhere := BufWhere + Back;
			 IF BufWhere < 1 THEN BufWhere := Maps;
			 If (BufWhere > Maps) OR ((Not BufFull) AND (BufWhere > WhichMap))
				 THEN IF Back = -1 THEN BufWhere := WhichMap
				 ELSE BufWhere := 1;
			{$R-}
			 SMapN := SMapPtrs[Bufwhere]^;
			{$R+}
			Message ('Press F2 to Continue. Index: '+NumStr(ID,3,0));
		END;
		MainMenu.RowChecked(4,Ord(WhichForce)+1, False);
		WhichForce := Force;
		MainMenu.RowChecked(4,Ord(WhichForce)+1, True);
		CASE WhichForce OF
			Walls : BEGIN
				koverm := fk; mass := fm; HalfWallSep := Separation; ChooseWalls END;
			Pendulum : BEGIN gOverL := fg; PendL := PLength; ChoosePendulum END;
			VanDePol : BEGIN mass := vm; koverm := vk; VDepX0 := VX0 END;
			Asymmetric : BEGIN koverm := fak; AsCoef3 := k3; AsCoef4 := k4; Mass := am END;
		END; {Case WhichForce}
		NPhases := NFrames;
		MapLength := NPoints;
		Energy := Ener;
		DoverM := Drag;
		F0overM := Driv;
		Omega := Omeg;
		If ID = 1 THEN
		BEGIN
			NewSystem({Clear: } True);
			GetX(temp, Energy+Emin);
			IF temp = 0.0 THEN {Happens if asymmetric potentialhas only local minimum}
			BEGIN
				GetX2(temp, Energy+Emin);
				temp := Abs(temp)
			END;
			IF temp > 0.0 THEN MaxPos := Temp;
			FindPotential(False);
			DisplayPotential(False, False);
			SearchKeys.Display
		END;
		DisplayChoice;
		DisplayDissip;
		i := 1 ;
		CloseMyViewPort(PoincarePort);
		For j := First to Last
			DO PutPixel(XMap[j], VMap[j], White);
		IF Eof(SFile) THEN
		BEGIN
			UseBuffer := True;
			SearchKeys.Done;
			SetKeysPausing;
			AllIn := True;
			Message ('All maps read in.' +Numstr(ID,3,0));
		END; {If Eof}
		IF UseBuffer AND Not Finished THEN DynamicPause;
	UNTIL Finished;
	Close(SFile);
	If Open THEN Close(SFile1);
	SetColor(White);
	GetX(temp, Energy+Emin);
	IF temp = 0.0 THEN {Happens if asymmetric potentialhas only local minimum}
	BEGIN
		GetX2(temp, Energy+Emin);
		temp := Abs(temp)
	END;
	IF Temp > 0.0 THEN MaxPos := Temp;
	SearchKeys.Clear; SearchKeys.done;
	FindPotential(True);
	DisplayPotential(False, True);
	DisplayChoice;
	DisplayDissip;
	Continuous := True;
   ReadSearch := False;
   SetupSystemKeys;
	NPhases := 12;
	Event.ReadKey := Chr(0);
	ShowMouse
END; {ReadSMap}

{ Calculates period as a function of energy for several wall separations or g's}

Procedure MakePeriodTable;
VAR
	Quit, SaveAnimate, SaveContinuous : Boolean;
	Pstr : String[24];
	i, j, h  : Integer;
	E0, P0, PMax, gsav, DSav, RSav, SepSav, ksav, VdepX0Sav, C3Sav : Real;

	PROCEDURE PeriodParams;
	BEGIN
		Quit := False;
		WITH Screen DO
		BEGIN
			Init;
			Loadline ('        Period Plot Options');
			Loadline ('');
			Loadline ('#T Pause after each curve');        {Field 1}
			Loadline ('');
			Loadline ('      Plot period');
			IF WhichForce = VanDePol THEN     {Field 2 & 3}
				Loadline ('Against Drag  from {      } to  {      }')
			ELSE
				Loadline ('Against Energy from {      } to  {      }');
			Loadline ('in {    } Steps'); {Field 4}
			Loadline ('');
			Loadline ('Plot {   } curves, ');    {Field 5}
			Case WhichForce OF  {Field 6 & 7}
				Walls : BEGIN
					LoadLine ('Varying Wall Separation');
					Loadline ('        from {     } to {     }')
				END;
				SHO : LoadLine ('Varying k/m from {     } to {     }');
				VanDePol : LoadLine ('Varying X0 from {     } to {     }');
				Cubic : LoadLine ('Varying C3 from {     } to {     }');
				Pendulum : LoadLine ('Varying g/l from {     } to {     }');
				Asymmetric : LoadLine ('Varying A2 from  {     } to {     }');
			END; {Case}
			Loadline ('');
			Loadline ('Cancel if you need to set other variables');
			Loadline ('Defaults were carefully chosen, try them');
			Loadline ('  before you change them.');
			Loadline ('');
			Loadline ('   [ OK ]     [Cancel]     [Help]');
			SetBoolean (1,PeriodPause);
			SetNumber (2,EOne); SetNumber (3,EOne + DeltaE*(ESteps-1));
			SetNumber (4, ESteps);
			SetNumber (5, NPerCurves);
			SetNumber (6,BaseVal); SetNumber (7,BaseVal+(NPerCurves-1)*DeltaW);
			SetNumberLimits (5, 1, 10);
			SetHelpFile('anharm.hlp','PeriodParHelp');
			DefineInputPort (0.07, 0.62, 0.14, 0.9);
			AcceptScreen;
			IF Canceled THEN
			BEGIN
				Quit := True;
				DisplayMainMenu
			END
			ELSE
			BEGIN
				PeriodPause := GetBoolean (1);
				EOne   := GetNumber (2);
				ESteps := Trunc(GetNumber (4));
				DeltaE := (GetNumber (3)-EOne)/(ESteps-1);
				NPerCurves := Trunc(GetNumber (5));
				BaseVal := GetNumber (6);
				IF NPerCurves > 1 THEN
					DeltaW :=  (GetNumber (7) - BaseVal)/(NPerCurves-1)
				ELSE
					DeltaW := 0.0;
			END;
			done
		END
  END; {PeriodParams}

	PROCEDURE PerReScale(NewScale : Real; RePlot : Boolean);
	VAR ii : Integer;
	BEGIN
		CloseMyViewPort(ClearURY);
		OpenMyViewPort(UpperRight);
		DefineScale (PeriodSNum, 0.0, EScale, 0.0, NewScale);
		Axis (0.0, 0.0, EScale/2, NewScale/2);
		IF WhichForce = VanDePol THEN PutLabel(Inside,'Period vs. Drag D')
			ELSE PutLabel(Inside,'Period vs. Energy');
		IF RePlot THEN FOR ii := 2 to j DO
			PlotLine (Energies[i].value(ii-1),Periods[i].value(ii-1),
					  Energies[i].value(ii),Periods[i].value(ii))
	END; {PerReScale}

BEGIN
	IF WhichForce = Pendulum THEN
		Pstr := ' Plotting Energy '+Char(246)+' (g/l)' ELSE Pstr := '';
	Message ('Press Q to quit.'+Pstr);
	SetColor(White);
	HoldVScale := False; Quit := False;
	IF HavePerTabSpace THEN
		For i := 1 TO NPerCurves DO
			BEGIN
			Energies[i].free; Periods[i].free;
			HavePerTabSpace := False
			END;
	SaveAnimate := Animating; SaveContinuous := Continuous;
	Animating := False;  {Would take far too much time}
	Continuous := False;
	If WhichForce = VanDePol THEN
		BEGIN Continuous := True; Resonating := True END;
	ShowPeriods := 1; ShowMenu := False;
	DSav := F0overM; RSav := DoverM; C3Sav := C3overM; ksav := koverm;
	F0overM := 0.0; DoverM := 0.0; C3overM :=  1.0; SepSav := HalfWallSep;
	IF WhichForce = SHO THEN koverm := 2.0;
	HalfWallSep := 0.2;
	gsav := gOverL; VdepX0Sav := VdepX0;
	gOverL := 2; VdepX0 := 0.4;
	Case WhichForce OF
		Walls :
			BEGIN AScale := 25.0; MaxPos := 0.5 END;
		SHO :
			BEGIN AScale := 5.0; MaxPos := 0.5;
				TimeScale := 6; FreezeTimeScale := True;
			END;
		VanDePol :
			BEGIN AScale := 16.0; MaxPos := 4.0; END;
		Cubic :
			BEGIN AScale := 26.0;MaxPos := 1.0 END;
		Pendulum :
			BEGIN
				AScale := Trunc(1+Sqrt(PendL/gOverL)*TwoPi*2);
				MaxPos := Pi
			END;
		Asymmetric :
			BEGIN AScale := 25.0; MaxPos := 3.0;
				TimeScale := 20; FreezeTimeScale := True
			END;
	END; {Case}
	PeriodParams; IF Quit THEN Exit;
	EScale := EOne + DeltaE*(ESteps-1);
	For i := 1 TO NPerCurves DO
	BEGIN
		Energies[i].init(ESteps); Periods[i].init(ESteps);
		HavePerTabSpace := True
	END;
	CloseMyViewPort(MidRight);
	PerReScale(AScale, False);
	PMax := 0.0;
	i := 1;
	Case WhichForce OF  {Field 6 & 7}
		Walls : HalfWallSep := 0.5*BaseVal;
		SHO : kOverM := BaseVal;
		VanDePol : VdepX0 := BaseVal;
		Cubic : C3overM := BaseVal;
		Pendulum : gOverL := BaseVal;
		Asymmetric : AsCoef3 := BaseVal;
	END; {Case}
	HideMouse;
	REPEAT {NPerCurves times}
		IF WhichForce = Pendulum THEN
		BEGIN
			EOne := gOverL*EOne;
			DeltaE := gOverL*DeltaE
		END;
		FindPotential(True);
		DisplayPotential(Not Exact, HoldVScale);
		ClearPhasePort;
		j := 1;
		Energy := EOne;
		ClearDissipPort;
		SetColor(White);
		h := TextHeight('O')+4;
		CASE WhichForce OF
			Walls : OutTextXY(35,5+h,'Wall Separation: '+ NumStr(2*HalfWallSep,5,2));
			SHO :  OutTextXY(35,5+h,'k: '+ NumStr(kOverm*Mass,5,2));
			Pendulum :  OutTextXY(35,5+h,'g/l: '+ NumStr(gOverL,5,2));
			Cubic : OutTextXY(35,5+h, 'C3 = '+numStr(C3OverM*Mass,5,2));
			VanDePol : OutTextXY(35,5+h, 'Xo = '+numStr(VdePX0,5,2));
			Asymmetric : OutTextXY(35,5+h, 'A2 = '+numStr(AsCoef3,5,2));
		END; {Case}
		REPEAT {Until ESteps steps or Quit}
			SelectMyViewPort(Vport);
			BothSides := False;
			SetMyColor(rainbow[0].colors[NColor]);
			NColor := NColor + 1;
			If NColor > 15 Then NColor := 9;
			If rainbow[0].colors[NColor] = GraphBackColor THEN NColor := NColor + 1;
			IF WhichForce = VanDePol THEN
			BEGIN
				XStart := 0.1;
				DoverM := Energy;
			END; {VanDePol}
			ShowTrajectory;
			Plus := Not Plus; {So well used alternates when double wells present}
			IF WhichForce = Pendulum THEN
				Energies[i].Put(j,Energy/gOverL)
			ELSE
				Energies[i].Put(j,Energy);
			Periods[i].put(j,Period);
			IF Period > AScale THEN PerReScale(1.2*Period,{RePlot:} True);
			IF Period > PMax THEN Pmax := Period;
			If j > 1 THEN
			BEGIN
				SelectScale (PeriodSNum);
				SelectMyViewPort(UpperRight);
				SetColor(rainbow[0].colors[((i-1) Mod 6)+10]);
				IF WhichForce = Pendulum THEN
					PlotLine(E0/gOverL,P0, Energy/gOverL,Period)
				ELSE
					PlotLine(E0,P0, Energy,Period);
			END;
			IF Event.ReadKey in ['Q','q'] THEN Quit := True;{Event checked already}
			IF NOT Quit THEN BEGIN E0 := Energy; P0 := Period END;
			j := j + 1;
			Energy := Energy + DeltaE
		UNTIL (j > ESteps) OR Quit;
		IF Quit OR (WhichForce in [Sho, VanDePol]) THEN
			FOR j := j-1 TO ESteps DO
			 BEGIN Energies[i].Put(j,E0); Periods[i].put(j,P0) END;
		i := i + 1;
		IF WhichForce = SHO THEN koverm := koverm + 2.0;
		IF WhichForce = Pendulum THEN
		BEGIN
			EOne := EOne/gOverL;
			DeltaE := DeltaE/gOverL
		END;
		Case WhichForce OF  {Field 6 & 7}
			Walls : HalfWallSep := HalfWallSep + 0.5*DeltaW;
			SHO : koverM := kOverM + DeltaW;
			VanDePol : VdepX0 := VdepX0 + DeltaW;
			Cubic : C3overM := C3overM + DeltaW;
			Pendulum : gOverL := gOverL + DeltaW;
			Asymmetric : AsCoef3 := Ascoef3 + DeltaW;
		END; {Case}
		IF PeriodPause and (Not Quit) THEN
		BEGIN
			Message ('Click or press ENTER; press X to avoid this pause in future.');
			DynamicPause;
			Message ('Press Q to quit.');
			CheckforEvents;
			IF (event.Readkey in ['X','x']) Then PeriodPause := False;
			IF Event.ReadKey in ['Q','q'] THEN Quit := True;
		END;
	UNTIL (i > NPerCurves) OR Quit;
	HalfWallSep := SepSav;
	PerReScale(1.2* PMax,{RePlot:} False);
	SetColor(White);
	DrawSystem;
	HavePeriods := True;
	ShowPeriod('P');
	FindPotential(True);
	DisplayPotential(BothV, HoldVScale);
	RunInstruct;
	NColor := 10;
	If DoverM > 0.0 THEN ShowPeriods := 20 ELSE ShowPeriods := 2;
	ShowMenu := True;
	gOverL := gsav; koverm := ksav;
	F0overM := DSav; DoverM := RSav; VdepX0 := VdepX0Sav; C3overM := C3Sav;
	Continuous := AlwaysContinuous; Resonating := False;
	FreezeTimeScale := False;
	Animating := SaveAnimate; Continuous := SaveContinuous;
	DisplayMainMenu;
	ShowMouse
END; {MakePeriodTable}

CONST Fast : Boolean = True;

Procedure MakeResonanceTable;
CONST NFine = 10;
VAR
	SaveAnimate, SaveContinuous, SaveErase,
		Quit, DoneFlag, Extending, DoPlot : Boolean;
	i, j, k, h, Tries : Integer;
	F0, A0, AMax, ARes, DeltaF, DF, gsav, XMem : Real;
	omegaSav, DSav, RSav, VdepX0Sav : Real;
	xLow, xHi, xStart, x0Sav, v0Sav, t0Sav : Real;
	Mult : Real;
	ResKeys : THotKeys;
	key1 : Byte;

	PROCEDURE GetResKeyHelp;
	BEGIN
		ShowHelp('anharm.hlp','ResKeyHelp;');  StaticPause;
		GraphBackColor := Black; HideMouse;
		SelectMyViewPort(allPort); ClearViewPort; ShowMouse;
		GraphBackColor := DarkGray;
		DoneFlag := False
	END; {GetResKeyHelp}

	PROCEDURE LookForJumps(VAR i, j, k : Integer);
	VAR ii : Integer;
	BEGIN
		IF (((Amplitude/A0 > 2.0) OR (Amplitude > ARes/2)) AND (k = 2))
			OR ((A0/Amplitude > 2.0) AND (k = 1)) THEN
		BEGIN
			DF := DeltaF;
			IF Extending THEN
			BEGIN
			  IF Tries < 4 THEN {Take a finer look}
			  BEGIN
				Omega := Omega - Mult*DeltaF*ResStepSize;
				Mult := Mult/3;
				x0Old := -A0; v0Old := 0.0;
				Tries := Tries + 1;
				j := j - 1; {Write over the saved data}
				DoPlot := False;
				Exit
			  END
			  ELSE
			  BEGIN
				  Omega := Omega - DeltaF*Mult*ResStepSize;
				  DeltaF := - DF; {Encountered hysteresis, go plot repulsor}
				  IF k = 2 THEN Doneflag := True;
				  FOR ii := j TO NResPts[k] DO
				  BEGIN
						Amplitudes[i,k].Put(ii,A0);
						Frequencies[i,k].put(ii,F0)
				  END;
				  NumberOfResPlots[i] := 2;
				  j := 0; k := 3; Extending := False
			  END
			END
			ELSE
			BEGIN {Found break, start from high end}
			  For ii := j-1  TO NResPts[k] DO
			  BEGIN
					Amplitudes[i,k].Put(ii,A0);
					Frequencies[i,k].put(ii,F0)
			  END;
			  ARes := A0;
			  Omega := Omega0 + OmegaRangeUp+DF*ResStepSize;
			  A0 := Amplitude;
			  Extending := True; j := 0; k := 2; Mult := 1.0;
			  DeltaF := - DeltaF; Tries := 1;
			END
		END {Amplitude/A0}
		ELSE
		BEGIN
			SelectScale (ResonanceSNum);
			SelectMyViewPort(UpperRight);
			SetColor(rainbow[0].colors[i+9]);
			PlotLine(F0,A0, Omega, Amplitude);
			F0 := Omega; A0 := Amplitude;
		END; {ELSE}
	END; {LookForJumps}

	PROCEDURE ResReScale(NewScale : Real; RePlot : Boolean);
	VAR ii : Integer;
	BEGIN
		CloseMyViewPort(ClearURY);
		OpenMyViewPort(UpperRight);
		DefineScale (ResonanceSNum, Omega0-Abs(OmegaRangeDown),
							Omega0+Abs(OmegaRangeUp),0.0, NewScale);
		Axis (0.0, 0.0, Abs(OmegaRange)/2,
								AxisRound(NewScale/2,2));
		PutLabel(Inside,'Amplitude vs. Freq.');
		AScale := NewScale;
		IF RePlot THEN FOR ii := 2 to j DO
			PlotLine (Frequencies[i,k].value(ii-1),Amplitudes[i,k].value(ii-1),
					  Frequencies[i,k].value(ii),Amplitudes[i,k].value(ii))
	END; {ResReScale}

	PROCEDURE FindResPoint(VAR i, j, k : Integer);
	BEGIN
			DisplayDissip;
			SelectMyViewPort(Vport);
			BothSides := False;
			SetColor(White);
			ClearPhasePort;
			SetMyColor(rainbow[0].colors[NColor]);
			NColor := NColor + 1;
			If NColor > 15 Then NColor := 9;
			If rainbow[0].colors[NColor] = GraphBackColor THEN NColor := NColor + 1;
			Plus := True;
			x0Sav := x0Old; v0Sav := v0Old; t0Sav := t0Old;
			HideMouse;
			ShowTrajectory; ShowMouse;
			IF Event.Readkey in ['G','g'] THEN
				Announce('G invalidates the curve, be patient, read caution carefully next time.');
			IF J <= NResPts[k] THEN
			BEGIN
				Amplitudes[i,k].Put(j,Amplitude);
				Frequencies[i,k].put(j,Omega)
			END
			ELSE Comment(0.0,0.05,
			'Display Memory Full, points no longer saved, use ''Extra'' for these parameters');
			IF Amplitude > AScale THEN ResReScale(1.2*Amplitude,{RePlot:} True);
			IF Amplitude > AMax THEN Amax := Amplitude;
			DoPlot := True;
			If j > 1 THEN LookForJumps(i, j, k) {Also plots the curve}
			ELSE BEGIN F0 := Omega; A0 := Amplitude END;
			IF (k = 1) AND (Abs(Omega-Omega0) > 0.1*OmegaRange) THEN
				Mult := Abs(Omega-Omega0)/OmegaRange;
			Omega := Omega + Mult*DeltaF*ResStepSize;
			j := j + 1;
			IF j > NResPts[k] THEN ;
			IF (Omega > Omega0+OmegaRangeUp) AND (k = 1) THEN Doneflag := True;
			IF (Omega < Omega0-OmegaRangeDown) THEN Doneflag := True;
			IF Event.ReadKey in ['Q','q'] THEN Quit := True
	END; {FindResPoint}

	PROCEDURE PlotRepulsor(VAR i, j, k : Integer);
	VAR jj, kk : Integer;
		 Lower, XInc, xPlot, xLowest, MidAmp, FRange, D1, D2 : Real;
		 FirstTry : Boolean;
	BEGIN
		IF Fast THEN
		BEGIN
			SelectScale (ResonanceSNum);
			SelectMyViewPort(UpperRight);
			SetColor(rainbow[0].colors[i]+1);
			PlotLine(Frequencies[i,2].Value(NResPts[2]),
						Amplitudes[i,2].Value(NResPts[2]),
						Frequencies[i,1].Value(NResPts[1]),
						Amplitudes[i,1].Value(NResPts[1]))
		END
		ELSE
		BEGIN {Find Unstable branch}
			Comment(0.05,0.05,'Searching for unstable branch: moving up till amplitude increases.');
			NumberOfResPlots[i] := 3;
			xLow := Amplitudes[i,2].Value(NResPts[2]);
			xHi  := Amplitudes[i,1].Value(NResPts[1]);
			xLowest := xLow;
			FRange := Abs(Frequencies[i,1].Value(NResPts[1])
							 -Frequencies[i,2].Value(NResPts[2]));
			DF := 0.1*FRange;
			XInc := 0.025*(XHi-XLow);
			F0 := Frequencies[i,2].Value(NResPts[2]); {Low w in unstable branch}
			A0 := xLow; Omega := F0;
			DF := (Frequencies[i,1].Value(NResPts[1])-F0)/10.0;
			Frequencies[i,3].Put(1,F0);
			Amplitudes[i,3].Put(1,A0);
			Continuous := False; TimeLimit := False; ShowPeriods := 20;
			xPlot := xLow;
			j := 2;
			REPEAT
				DisplayDissip;
				IF Omega < Frequencies[i,1].Value(NResPts[1]) THEN {Below peak}
				BEGIN
					jj := 1;
					While (jj >= NResPts[1])
						AND (Omega >= Frequencies[i,1].Value(jj)) DO jj := jj + 1;
					IF jj < NResPts[1] THEN
					BEGIN
						xLow := xPlot;
						xHi  := Amplitudes[i,1].Value(jj)
					END
				END;
				MidAmp := (xLow + xHi)/2;
				XStart := XPlot+Trunc(ResStepSize-1)*XInc; {Will be incremented}
				Lower := XStart; {In case the first trial is above}
				FirstTry := True;
				kk := 1;
				REPEAT
					XStart := xStart + XInc;
					x0Old := -xStart; {This phase gives better results}
					v0Old := 0.0;
					ClearPhasePort;
					ShowTrajectory;
					IF Amplitude > MidAmp THEN
					BEGIN
						IF FirstTry THEN xStart := xStart - 2*XInc {Try lower}
						ELSE {Amplitude High and not first try}
							kk := kk + 1
					END
					ELSE
					BEGIN
						FirstTry := False;
						Lower := XStart;
						kk := 1
					END;
					IF xStart < xLowest THEN
					BEGIN
						Lower := xLowest-0.5*XInc;
						kk := 3
					END;
					IF Event.ReadKey in ['Q','q'] THEN Quit := True
				UNTIL (kk > 2) OR Quit OR (Event.ReadKey in ['G','g']);
								{Twoincreasing runs in a row}
				xPlot := Lower+ 0.5*XInc; {Unstable boundary is about halfway}
				SelectScale (ResonanceSNum);
				SelectMyViewPort(UpperRight);
				SetColor(rainbow[0].colors[i]+9);
				PlotLine(F0,A0, Omega,xPlot);
				IF j <= NResPts[3] THEN
				BEGIN
					Amplitudes[i,3].Put(j,xPlot);
					Frequencies[i,3].put(j,Omega);
				END
				ELSE Comment(0.0,0.05,
				'Display Memory Full, points no longer saved, use ''Extra'' for these parameters');
				F0 := Omega; A0 := xPlot;
				Mult := 1.0;
				D1 := Abs(Omega-Frequencies[i,1].Value(NResPts[1]));
				IF D1 < 0.1*FRange THEN Mult := 10.0*D1/FRange;
				D2 := Abs(Omega-Frequencies[i,2].Value(NResPts[2]));
				IF D2 < 0.2*ResStepSize*FRange THEN Mult := 5.0*D2/FRange;
				IF Mult < 0.01 THEN Mult := 0.1;
				Omega := Omega + DF*ResStepSize*Mult;
				j := j + 1;
			UNTIL (Omega > Frequencies[i,1].Value(NResPts[1])) OR Quit;
			SetColor(rainbow[0].colors[i]+9);
			PlotLine(F0,A0,
				Frequencies[i,1].Value(NResPts[1]),
				Amplitudes[i,1].Value(NResPts[1]));
			FOR jj := j to NResPts[3] DO
			BEGIN
				Amplitudes[i,3].Put(jj,Amplitudes[i,1].Value(NResPts[1]));
				Frequencies[i,3].Put(jj,Frequencies[i,1].Value(NResPts[1]))
			END
		END; {ELSE Find Unstable Branch}
	END; {PlotRepulsor}

	PROCEDURE SelectDetail;
	BEGIN
		WITH Screen DO
		BEGIN
			Init;
			Loadline ('       Resonance Plot Options');
			Loadline ('');
			Loadline (' If hysteresis is present:');
			Loadline ('#1 Fast Straight Line 3rd Branch'); {Field 1}
			Loadline ('#1 Slow & Careful Third Branch');   {Field 2}
			Loadline ('');
			Loadline ('#T Pause after each curve');        {Field 3}
			Loadline ('');
			Loadline ('Plot vs. omega from {      } to  {     }'); {Field 4 & 5}
			Loadline ('Expect Resonance at about omega= {     }'); {Field 6}
			Loadline ('Plot {   } curves, ');                      {Field 7}
			Loadline ('Fo = {     } ');
			Loadline (' varying Drag/M from {     } to  {     }'); {Field 8 & 9}
			Loadline ('Step Size {      }   Sensitivity {     }');
			Loadline ('#F 20% Extra Memory   #F 40% Extra Memory');
			Loadline ('');
			Loadline ('Cancel if you need to set other variables');
			Loadline ('');
			Loadline ('   [ OK ]     [Cancel]     [Help]');
			SetHelpFile('anharm.hlp','ResKeyHelp;');
			DefineInputPort (0.07, 0.62, 0.12, 0.92);
			If Not Fast THEN SetRadioButton('1',2);
			SetBoolean(3, PeriodPause);
			SetNumber (4, Omega0 - OmegaRangeDown);
			SetNumber (5, Omega0 + OmegaRangeUp);
			SetNumber (6, Omega0);
			SetNumber (7, NResCurves);
			SetNumber (8, F0overM);
			SetNumber (9, DBeg);
			SetNumber (10, DBeg + (NResCurves-1)*DInc);
			SetNumber (11, ResStepSize);
			SetNumber (12, 0.05/ResCrit);
			SetNumberLimits(7,1,5);
			SetNumberLimits(11,0.1,6);
			SetNumberLimits(12,0.1,10.0);
			AcceptScreen;
			IF Canceled THEN Quit := True
			ELSE
			BEGIN
				Fast := GetBoolean(1);
				PeriodPause := GetBoolean(3);
				Omega0 := GetNumber(6);
				F0overM := GetNumber(8);
				ResStepSize := GetNumber (11);
				ResCrit := 0.05/GetNumber (12);
				NResCurves := Trunc(GetNumber(7));
				DoverM := GetNumber(9); DBeg := DOverM;
				IF GetBoolean(13) Then XMem := 1.2 ELSE XMem := 1.0;
				IF GetBoolean(14) THEN XMem := 1.4;
				IF NResCurves = 1 THEN DInc := 0
					ELSE DInc := (GetNumber(10) - DoverM)/(NResCurves-1);
				IF GetNumber(4) > GetNumber(5) THEN
				BEGIN
					Announce(
						'Omega upper limit must be greater than lower limit.');
					Quit := True;
					Exit
				END;
				OmegaRangeDown := Abs(Omega0 - GetNumber(4));
				OmegaRangeUp   := Abs(Omega0 - GetNumber(5));
			END;
			done
		END
  END; {SelectDetail}

	PROCEDURE ResSetup;
	VAR ii : Integer;
		 MemStep : Real;
	BEGIN
		DisplayPotential(BothV, HoldVScale);
		HoldVScale := False; Quit := False;
		IF HaveResTabSpace THEN
			For ii := 1 TO NResCurves DO
				BEGIN
				Amplitudes[ii,1].free; Frequencies[ii,1].free;
				Amplitudes[ii,2].free; Frequencies[ii,2].free;
				Amplitudes[ii,3].free; Frequencies[ii,3].free;
				HaveResTabSpace := False
				END;
		HaveResonances := False;
		MainMenu.RowActivate(1,12,False);
		DSav := F0overM; RSav := DoverM; omegaSav := Omega;
		F0overM := 0.1;
		gsav := gOverL; VdepX0Sav := VdepX0;
		DoErase := True;
		Case WhichForce OF
			Walls, SHO : AScale := 1.2;
			Asymmetric: AScale := 1.0;
			VanDePol : AScale := 3.0;
			Cubic : BEGIN AScale := 3.0; F0overM := 0.5;
						  Omega0 := 1.5; OmegaRange := 1.2
					  END;
			Pendulum :
				BEGIN
					AScale := 2.0;
					F0overM := 1.0;
					Omega0 := 0.1*Trunc(10*Sqrt(gOverL));
					OmegaRange := 0.1*Trunc(5*Omega0);
				END;
		END; {Case}
		SelectDetail; IF Quit THEN Exit;
		Message('Press Q to quit, G to go on to next point in emergencies.');
		Animating := False;  {Would take far too much time}
		Continuous := True; Resonating := True;
		ShowMenu := False;
		OmegaRange := 0.5*(OmegaRangeDown + OmegaRangeUp);
		MemStep := 1.0/ResStepSize;
		If MemStep < 0.33 THEN MemStep := 0.33;
		NResPts[1] := Trunc(3.5*NResPtsInit*XMem*MemStep);
		NResPts[2] := Trunc(NResPtsInit*XMem*MemStep);
		NResPts[3] := Trunc(1.50*NResPtsInit*XMem*MemStep);
		ii := 1;
		While ii <=  NResCurves DO
		BEGIN
			IF 2*(NResPts[1]+NResPts[2]+NResPts[3])*SizeOf(Real)+1600 < Maxavail THEN
			BEGIN
			Amplitudes[ii,1].init(NResPts[1]); Frequencies[ii,1].init(NResPts[1]);
			Amplitudes[ii,2].init(NResPts[2]); Frequencies[ii,2].init(NResPts[2]);
			Amplitudes[ii,3].init(NResPts[3]); Frequencies[ii,3].init(NResPts[3]);
			HaveResTabSpace := True
			END
			ELSE
			BEGIN
				Announce('Only Memory for '+NumStr(ii-1,2,0)
					+' curve(s), reduce # of Movie Frames to get more');
				NResCurves := ii-1
			END;
			ii := ii + 1
		END;
		IF NREsCurves < 1 THEN
		BEGIN
			Quit := True;
			Resonating := False;
			ShowMenu := True;
			Exit
		END;
		Help(ResScreen);
		CloseMyViewPort(MidRight);
		ResReScale(AScale,{RePlot:} False);
		AMax := 0.0;
		i := 1;
		XStart := 0.01;
		Energy := 0.01
	END; {ResSetup}

BEGIN {MakeResonanceTable}
	SaveAnimate := Animating; SaveContinuous := Continuous;
	SaveErase := DoErase;
	ResSetup; IF Quit THEN Exit;
	REPEAT {UNTIL (DoverM > DEnd)}
		NumberOfResPlots[i] := 1;
		Comment(0.05,0.05,'Determining amplitude at each frequency.');
		DeltaF := 0.1*OmegaRange;
		DoneFlag := False; Extending := False;
		j := 1; k := 1;
		Mult := 1.0;
		{$IFDEF test}
		Omega := Omega0 - 0.2*OmegaRangeDown;
		{$ELSE}
		Omega := Omega0 - OmegaRangeDown;
		{$ENDIF}
		REPEAT FindResPoint(i, j, k) UNTIL Quit OR DoneFlag;
		IF (NumberOfResPlots[i] = 1) OR Quit THEN
		  For j := j-1  TO NResPts[k] DO
			 BEGIN Amplitudes[i,k].Put(j,A0); Frequencies[i,k].put(j,F0) END;
		IF (k = 3) AND (NOT Quit) THEN
			PlotRepulsor(i, j, k);
		i := i + 1;
		DoverM := DoverM + DInc;
		IF PeriodPause and (Not Quit) AND
			((DoverM <= 0.2) OR (i <= NResCurves)) THEN
		BEGIN
			Comment(0.1,0.5,
			  'Click or press ENTER; press X to avoid this pause in future.');
			DynamicPause;
			Comment(0.1,0.5,
			  '                                                            ');
			Message ('Press Q to quit.');
			CheckforEvents;
			IF event.Readkey in ['X','x'] Then PeriodPause := False;
			IF Event.ReadKey in ['Q','q'] THEN Quit := True;
		END;
	UNTIL (i > NResCurves) OR Quit;
	SetColor(White);
	DrawSystem;
	HaveResonances := True;
	MainMenu.RowActivate(1,12,True);
	{Adjust Scale }
	IF 1.2*AMax < AScale THEN ResReScale(1.2* AMax,{RePlot:} False);
	ShowPeriod('R');
	DisplayChoice;
	RunInstruct;
	NColor := 10;
	ShowMenu := True;
	gOverL := gsav; 
	F0overM := DSav; DoverM := RSav; VdepX0 := VdepX0Sav; Omega := omegaSav;
	If DoverM > 0.0 THEN ShowPeriods := 20 ELSE ShowPeriods := 2;
	Continuous := SaveContinuous; Resonating := False; DoErase := SaveErase;
	Animating := SaveAnimate;
	DisplayMainMenu; SetupSystemKeys;
	ShowMouse
END; {MakeResonanceTable}

	{--- Recalculates and redraws potential when wall separation or g is changed --}

Procedure ChangePotential;
VAR
  dy : Integer;
  y2, temp : Real;
BEGIN
	DoSystemKeys;
	IF (WhichForce <> VanDePol) AND (F0overM = 0.0)
		AND ((Energy > 0.9*MaxEnergy) OR (Energy < 0.1*MaxEnergy)) THEN
	BEGIN {Rescale Energy}
		GetX(temp, 2*Energy+Emin);
		IF temp = 0.0 THEN {Happens if asymmetric potentialhas only local minimum}
		BEGIN
			GetX2(temp, 2*Energy+Emin);
			temp := Abs(temp)
		END;
		IF temp > 0.0 THEN {Otherwise don't rescale, fouls up}
		BEGIN
			MaxPos := temp;
			FindPotential(True);
			HideMouse;
			DisplayPotential(BothV, HoldVScale);
			ShowMouse
		END
	END; {Rescale Energy}
END; {ChangePotential}

{------ Uses mouse to select the wall separation or initial position ----------}

Procedure MoveWall;
VAR
  dy : Integer;
  y2 : Real;
  xnow, ynow, button: Integer;
BEGIN
 CASE WhichForce OF
  Walls, VanDePol :
  BEGIN
	SelectMyViewPort(WallPort);
	SelectScale(WallSNum);
	WITH event DO WITH views[WallPort] DO {Look for events in WallPort}
	IF (mouseClicked AND (x>vx1) AND (x<vx2) AND (y>vy1) AND (y<vy2)) THEN
	 BEGIN
	  HoldVScale := False;
	  ynow := y;
	  REPEAT {Drag Mouse}
		MouseGlobalPosn(xnow, ynow, button);
		dy := (vy2-vy1);
		y2:=(vy2-ynow) /dy;
		WITH GraphScales[WallSNum] DO
		 CASE WhichForce of
			Walls :
			BEGIN
				HalfWallSep := ymin + 0.04 + (Ymax-Ymin)*y2/2;
				IF HalfWallSep < 0.01 THEN HalfWallSep := 0.01;
				IF HalfWallSep > 0.50 THEN HalfWallSep := 0.50;
				VCoefA := kOverM*mass*(HalfWallSep - RestLength)/HalfWallSep;
				VCoefB := kOverM*mass*RestLength/(4*Pwr(HalfWallSep,3));
				MaxPos := 0.5; {Reset to Default}
				FindPotential(True);
				HideMouse;
				DrawSystem;
				DisplayPotential(BothV, HoldVScale);
			END; {Walls}
			VanDePol :
			BEGIN
				XStart := xmin + (xmax-xmin)*(xnow-vx1)/(vx2-vx1);
				Plus := True;
				HideMouse;
				ShowTrajectory;
				NColor := NColor + 1;
				If NColor > 15 Then NColor := 9;
				If rainbow[0].colors[NColor] = GraphBackColor THEN
					NColor := NColor + 1;
				SetMyColor(NColor);
				ShowMouse
			END {VanDePol}
		END; {WITH and CASE}
		DisplayChoice;
		RunInstruct;
		ShowMouse
	 UNTIL (Button = 0) OR (WhichFOrce = VanDePol); {Mouse is released}
	END {If MouseClicked}
  END {Walls, VanDePol}
 END {CASE WhichForce}
END; {MoveWall}

Function FlatWindow(X,N : Integer): Real; {Cannot be inside another block}
BEGIN
	FlatWindow := 1.0
END;

Function SinWindow(X,N : Integer): Real; {Cannot be inside another block}
BEGIN
	SinWindow := Sin(Pi*X/N)
END;

Function Sin2Window(X,N : Integer): Real; {Cannot be inside another block}
BEGIN
	Sin2Window := Sqr(Sin(Pi*X/N))
END;

Function TriWindow(X,N : Integer): Real; {Cannot be inside another block}
BEGIN
	IF X < N DIV 2 THEN TriWindow := 2.0*X/N
	ELSE TriWindow := 2 - 2.0*X/N
END;

TYPE
	FunctionI = Function(i, n : Integer) : Real;
const
	FourierSnum = 11;
	FFTMagnitude : Boolean = True;
VAR
	Data : ARRAY[1..MaxSubX2] of REAL;
	A,B : ARRAY[0..MaxSub] OF REAL;
	Xconst,xr,yr : REAL;
	Max : Real;
	Window : FunctionI;
	Test : Boolean;

	nnn,isign:integer;

PROCEDURE FFTType(VAR Test, Windowed : Boolean);
BEGIN
	WITH Screen DO
	BEGIN
		Init;
		LoadLine ('Fourier Display:');
		Loadline ('  #1 Magnitude and Phase');
		Loadline ('  #1 Real+Imaginary');
		Loadline ('  #F Windowed');
		LoadLine ('  #F Test Function');
		LoadLine ('    [  OK  ]    [Cancel]');
		IF NOT FFTMagnitude Then SetRadioButton('1',2);
		SetBoolean (3, Windowed);
		DefineInputPort (0.10, 0.60, 0.575, 0.875);
		Accept;
		IF Not Canceled THEN
		BEGIN
			FFTMagnitude := GetBoolean(1);
			Windowed := GetBoolean(3);
			Test := GetBoolean(4)
		END;
		Done;
	END {With Screen}
END; {FFTType}

PROCEDURE TestParams(VAR Period : Real; VAR Shift, Points, Shape : Integer;
							VAR Windowed : Boolean);
BEGIN
	WITH Screen DO
	BEGIN
		Init;
		LoadLine ('Fourier Test Parameters');
		Loadline ('        Period: {      }');
		Loadline ('Number of Points: {    }');
		LoadLine ('           Shift: {    } Points');
		LoadLine ('  #1 Sine  #1 Square  #1 Triangle');
		Loadline ('  #F Windowed');
		LoadLine ('    [  OK  ]    [Cancel]');
		SetNumber (1,Period);
		SetNumber (2,Points);
		SetNumber (3,Shift);
		SetRadioButton ('1',Shape);
		SetBoolean (7, Windowed);
		SetNumberLimits (1, 2*TimeStepUsed, 10000*TimeStepUsed);
		SetNumberLimits (2, 16, 512);
		SetNumberLimits (3, -512, 512);
		DefineInputPort (0.10, 0.60, 0.575, 0.875);
		Accept;
		IF Not Canceled THEN
		BEGIN
			Period := GetNumber(1);
			Points := Trunc(GetNumber(2));
			Shift := Trunc(GetNumber(3));
			Shape := GetRadioButton('1');
			Windowed := GetBoolean(7)
		END;
		Done
	END {With Screen}
END; {TestParams}

PROCEDURE WindowType(VAR Window : FunctionI);
VAR i : integer;
BEGIN
	WITH Screen DO
	BEGIN
		Init;
		LoadLine ('Fourier Window Type');
		Loadline ('  #1 Sine');
		Loadline ('  #1 Sine Squared');
		LoadLine ('  #1 Triangle');
		LoadLine ('    [  OK  ]    [Cancel]');
		DefineInputPort (0.10, 0.60, 0.575, 0.875);
		Accept;
		IF Not Canceled THEN
		BEGIN
			i := GetRadioButton('1');
			Case i OF
				1 : Window := SinWindow;
				2 : Window := Sin2Window;
				3 : Window := TriWindow;
			END {Case}
		END
		ELSE Window := TriWindow; {Default Case}
		Done;
	END {With Screen}
END; {WindowType}

Function SinShape (i, Shift :Integer; Period : Real) : Real;
BEGIN
	SinShape := Sin(2*Pi*(i+Shift)*TimeStepUsed/Period)
END;

Function SquareShape (i, Shift :Integer; Period : Real) : Real;
VAR P : Integer;
	 S : Real;
BEGIN
	P := Trunc(Period/TimeStepUsed);
	S := 1.0*(((i + Shift) MOD P) - P DIV 2);
	IF S = 0 THEN SquareShape := 1
	ELSE SquareShape := S/Abs(S)
END;

Function TriangleShape (i, Shift :Integer; Period : Real) : Real;
VAR P : Integer;
	 S : Real;
BEGIN
	P := Trunc(Period/TimeStepUsed);
	i := (i + Shift) MOD P;
	IF i < P DIV 2 THEN S := 4.0*i/P
	ELSE S := 4 - 4.0*i/P;
	TriangleShape := S - 1.0
END;

Procedure SetUpFourierTransform(VAR numPts : word;
									VAR Test, Windowed : Boolean; ClearIt : Boolean;
									VAR Period : Real;
									VAR Shift, Points, Shape : Integer;
										 Color1 : Integer);

{Proc 'four1' Replaces data by its discrete/inverse discrete
 Fourier Transform if isign is	1/  -1.  nnn must be power of 2
 Data must be twice as long as NumPts}
VAR
	i, idiff : Integer;
BEGIN
	IF (Not InTutorial) AND (Not Test) THEN
	BEGIN
		FFTType(Test, Windowed); {Can set Test true}
	END;
	IF Test AND (Not Intutorial) THEN
		TestParams(Period, Shift, Points, Shape, Windowed);
	IF Windowed THEN
		IF (Not InTutorial) THEN WindowType(Window)
		ELSE Window := SinWindow;
	If NOT Windowed THEN Window := FlatWindow; {Default}
	isign:= 1; {discrete Fourier Transform rather than inverse}
	nnn := 2; While nnn < NumPts DO nnn := nnn*2;
	FOR i := 0 TO MaxSub DO
	BEGIN A[i] := 0.0; B[i] := 0.0 END;
	IF Not Test THEN
	BEGIN
	IF WhichForce = Pendulum THEN
		Message ('Analyzing velocity because angle is discontinuous.')
	ELSE
		Message ('');
	IF NOT Wrapped THEN {Points start at zero}
		IF WhichForce = Pendulum THEN
			FOR i := 0 TO NumPts-1 DO
				A[i] := Vs[i]
		ELSE
			FOR i := 0 TO NumPts-1 DO
				A[i] := Xs[i]
	ELSE IF (HistoryCount - IHist) < NumPts THEN {points are split}
	BEGIN
		IF WhichForce = Pendulum THEN
			FOR i := iHist to HistoryCount-1 DO
				A[i-IHist] := Vs[i+1]
		ELSE
			FOR i := iHist to HistoryCount-1 DO
				A[i-IHist] := Xs[i+1];
		idiff := HistoryCount - iHist;
		IF WhichForce = Pendulum THEN
			FOR i := IDiff to NumPts DO
				A[i] := Vs[i-IDiff]
		ELSE
			FOR i := IDiff to NumPts DO
				A[i] := Xs[i-IDiff];
	END {wrapped and split}
	ELSE {points begin at IHist+1 and are not split}
	IF WhichForce = Pendulum THEN
		FOR i := 0 to NumPts-1 DO
			A[i] := Vs[i+iHist+1]
	ELSE
		FOR i := 0 to NumPts-1 DO
			A[i] := Xs[i+iHist+1];
	IF Windowed THEN
		FOR i := 0 TO NumPts-1 DO
			A[i] := Window(i,NumPts)*A[i];
	END {Not Test}
	ELSE
	{Test Function:}
	BEGIN
		NumPts := Points; {Otherwise type mismatch}
		For i := 0 to NumPts DO
		 CASE Shape OF
			1: A[i] := Window(i,NumPts)*SinShape(i,Shift,Period);
			2: A[i] := Window(i,NumPts)*SquareShape(i,Shift,Period);
			3: A[i] := Window(i,NumPts)*TriangleShape(i,Shift,Period)
		 END; {Case}
		nnn := 2; While nnn < NumPts DO nnn := nnn*2;
		IF ClearIt THEN
		BEGIN
			CloseMyViewPort (ClearLLX);
			CloseMyViewPort (ClearLY);
			OpenViewPort(LowerLeft)
		END;
		SelectMyViewPort(LowerLeft);
		DefineScale (FourierSNum, 0.0, nnn*TimeStepUsed, -1.1,1.1);
		SelectScale(FourierSnum);
		SetColor(White);
		Axis (0.0, 0.0, AxisRound(nnn*TimeStepUsed/5,2), AxisRound(0.5,3));
		HideMouse; {To avoid flickering}
		SetColor(Color1);
		FOR i := 1 to MaxSub DO
			PlotLine ((i-1)*TimeStepUsed,A[i-1],i*TimeStepUsed,A[i]);
		ShowMouse
	END
END; {SetUpFourierTransform}

Procedure FastFourierTransform;

{PROCEDURE four1}
VAR
	 ii,jj,n,mmax,m,j,i,istep:integer;
	 wtemp,wr,wi,wpr,wpi, theta:real;
	 tempr,tempi, wrs,wis: real;
BEGIN

{Pack data}
	ii:=(nnn DIV 2) - 1;
	jj:=ii + 2;
	FOR n:=1 TO jj DO
	 BEGIN
		data[n*2 - 1]:= A[ii + n];
		data[n*2]:=B[ii + n];
	END;
	FOR n:=jj TO nnn DO
	BEGIN
		data[n*2 - 1]:=A[n - jj];
		data[n*2]:=B[n - jj];
	 END;
{end of pack data}

	 n:=2*nnn;
	 j:=1;
	 {Bit Reversal}
	FOR ii:= 1 TO nnn DO BEGIN
		i:=2*ii - 1;
		IF j > i THEN BEGIN
				{Exchange the two complex numbers}
		 tempr:=data[j];		tempi:=data[j+1];
		 data[j]:=data[i];  data[j+1]:=data[i+1];
		 data[i]:=tempr;		data[i+1]:=tempi;
			END;
	 m:=n DIV 2;
	 WHILE (m>=2) AND (j>m) DO BEGIN
					j:=j-m;
			       m:=m DIV 2;
		END;
		j:=j+m;
	 END;
	 mmax:=2;
	 WHILE n>mmax DO BEGIN       {log nnn (base 2) times}
			istep:= 2*mmax;
			{Initialise for trig recurrence}
			theta:=6.28318530717959/(isign * mmax);
			wpr:=-2.0 * sqr(sin(0.5*theta)); 	wpi:=sin(theta);
			wr:=1.0;	wi:=0.0;
			FOR ii:=1 TO mmax DIV 2 DO BEGIN
		{nested inner loops}
		m:=2 * ii - 1;
		wrs:=wr;	wis:=wi;
		FOR jj:=0 TO (n-m) DIV istep DO BEGIN
			 {Danielson-Lanczos formula}
			 i:=m + jj*istep;
			 j:=i +mmax;
			 tempr:=wrs * data[j] - wis * data[j+1];
			 tempi:=wrs * data[j+1] + wis * data[j];
			 data[j]:=data[i] - tempr;
			 data[j+1]:=data[i+1] - tempi;
			 data[i] := data[i]  +  tempr;
			 data[i+1]:= data[i+1]+ tempi;
		END;
		wtemp:=wr;	{Trig recurrence}
		wr:=wr*wpr - wi*wpi   + wr;
		wi:= wi*wpr + wtemp*wpi +wi;
		END;
		mmax:= istep;
		END;

{Unpack data}
	ii:= (nnn DIV 2) - 1;
	 jj:= ii + 2;
	FOR n:=1 TO jj DO
	BEGIN
		A[ii + n]:= data[n*2 - 1];
		B[ii + n]:= data[n*2];
	END;
	FOR n:=jj TO nnn DO
	BEGIN
		A[n - jj]:= data[n*2 - 1];
		B[n - jj]:= data[n*2];
	END;

END; {FastFourierTransform}

Function GetZoom(Zoom : Real; VAR Test : Boolean) : Real;
VAR i : Integer;
	 NewZoom : Real;
BEGIN
	WITH Screen DO
	BEGIN
		Init;
		Loadline ('#1 Zoom in 2X');
		IF Zoom < 1.0 THEN LoadLine ('#1 Zoom out 2X') ELSE Loadline('');
		LoadLine ('#1 No Zoom : Go On ');
		LoadLine ('[OK]      [Cancel]');
		IF Zoom < 1.0 THEN SetRadioButton('1',3) ELSE SetRadioButton('1',2);
		DefineInputPort (0.7, 0.99, 0.425, 0.635);
		AcceptScreen;
		i := GetRadioButton('1');
		CASE i OF
			1 : NewZoom := Zoom/2;
			2 : IF Zoom < 1.0 THEN NewZoom := 2*Zoom ELSE NewZoom := 0.0;
			3 : NewZoom := 0.0
		END;
		Done;
		GetZoom := NewZoom;
		IF Test AND (NewZoom = 0.0) THEN
		BEGIN
			Init;
			LoadLine ('#T Another test function');
			Loadline ('');
			LoadLine ('[OK]      [Cancel]');
			DefineInputPort (0.7, 0.99, 0.425, 0.635);
			AcceptScreen;
			IF Canceled THEN Test := False
			ELSE Test := GetBoolean(1);
			Done
		END {If Test}
	END {With Screen}
END; {GetZoom}

Procedure PlotFourierTransform(NumData, color1,color2 : word; 
							VAR Test : Boolean;
							ZoomSet, ClearIt : Boolean; InitialZoom : Real);
VAR
	xrange, MaxA, Zoom : REAL;
	ip, i : integer;
	lastX,lastY,lastA,lastB,newX,newY,newA,newB : word;
Begin
	Zoom := InitialZoom;
	Max := 0.0; MaxA := 0.0;
	FOR i := 1 TO nnn DO
	BEGIN
		Data[i] := Sqrt(Sqr(A[i]) + Sqr(B[i])); {Magnitudes}
		IF Data[i] = 0.0 THEN Data[i+MaxSub] := Data[i+MaxSub-1] {Protect ArcTan}
		ELSE Data[i+MaxSub] := ArcTan2(A[i],B[i]); {Phases}
		IF FFTMagnitude THEN
		BEGIN
			IF Abs(Data[i]) > Max THEN Max := Abs(Data[i]);
			IF Abs(Data[i+MaxSub]) > MaxA THEN MaxA := Abs(Data[i+MaxSub]);
		END
		ELSE
		BEGIN
			IF Abs(A[i]) > Max THEN Max := Abs(A[i]);
			IF Abs(B[i]) > Max THEN Max := Abs(B[i])
		END
	END;
	IF Max = 0.0 THEN
	BEGIN
		MildAnnounce('For some reason, all transform values are zero.');
		Exit
	END;
	REPEAT
	IF ClearIt THEN
	BEGIN
		CloseMyViewPort (ClearLMidX);
		CloseMyViewPort (ClearLUY);
		OpenViewPort(UpperLeft)
	END;
	SelectMyViewPort (UpperLeft);
	IF FFTMagnitude THEN
	BEGIN
		DefineScale (FourierSNum,
			0.0, Zoom*0.5/TimeStepUsed, 0.0,1.1*Max);
		DefineScale (FourierSNum+1,
			0.0, Zoom*0.5/TimeStepUsed, -1.1*MaxA,1.1*MaxA)
	END
	ELSE
		DefineScale (FourierSNum,
			0.0, Zoom*0.5/TimeStepUsed, -1.1*Max,1.1*Max);
	SelectScale(FourierSnum);
	SetColor(White);
	Axis (0.0, 0.0,
		AxisRound({Sqrt(}Zoom*0.2/TimeStepUsed,2), AxisRound(Max/2,3));
	PutLabel(Bottom,'Frequency (Hz)'); PutLabel(Left,'Amplitude');
	WITH GraphScales [FourierSnum] DO Xconst  := 2*(xmax - xmin) /(Zoom* nnn);
	xrange:=0.5/TimeStepUsed;
	IF FFTMagnitude THEN
	BEGIN
		WITH GraphScales [FourierSnum] DO lastA:=MapY(Data[1]);
		WITH GraphScales [FourierSnum+1] DO lastB:=MapY(Data[MaxSub+1]);
		Setcolor(color1);
		Print(1,1,'Magnitude    '+NumStr(NumData,3,0)+' points');
		Setcolor(color2);
		Print(36,1,'Phase')
	END
	ELSE
	BEGIN
		lastA:=MapY(A[0]);
		lastB:=MapY(B[0]);
		Setcolor(color1);
		Print(4,2,'Real transform');
		Setcolor(color2);
		Print(26,2,'Imaginary transform')
	END;
	lastX:=MapX(0.0);
	HideMouse;
	FOR ip:= nnn DIV 2 TO nnn-1 DO
	BEGIN
		newX:=MapX(ip*Xconst - xrange);
		IF FFTMagnitude THEN
		BEGIN
			WITH GraphScales [FourierSnum] DO newA:=MapY(Data[ip]);
			WITH GraphScales [FourierSnum+1] DO newB:=MapY(Data[MaxSub+ip])
		END
		ELSE
		BEGIN
			newA:=MapY(A[ip]);
			newB:=MapY(B[ip])
		END;
		Setcolor(color2);
		line(lastX,lastB,newX,newB);
		SetColor(color1);
		line(lastX,lastA,newX,newA);
		lastX:=newX;
		lastA:=newA;
		lastB:=newB;
	END;
	ShowMouse;
	Message('');
	If NOT ZoomSet THEN Zoom := GetZoom(Zoom, Test);
	UNTIL (Zoom = 0.0) OR InTutorial;
	IF NOT InTutorial THEN DisplayMainMenu
END; {PlotFourierTransform}

PROCEDURE FourierTransform(NumData : word; IHist : Integer; Wrapped : Boolean);
CONST
	Windowed : Boolean = False;
	Period : Real = 5.12;
	Shift : Integer = 0;
	Shape : Integer = 1; {Sine curve}
VAR
	TestPoints : Integer;
BEGIN {FourierTransform}
	Test := False;
   TestPoints := MaxSub;
	REPEAT
		SetUpFourierTransform
			(NumData,Test,Windowed,{Clear=} True,
			 Period,Shift,TestPoints,Shape,White);
		FastFourierTransform;
		PlotFourierTransform(NumData, yellow,green, Test,
									{ZoomSet=} False, {Clear=} True, {InitialZoom=} 1.0);
	UNTIL Not Test;
	ClearPhasePort
END; {FourierTransform}

{- Uses mouse or arrow (& Enter) keys to choose energy & launch a trajectory -}

Procedure PickEnergy;
VAR
  dx, dy, EFactor : Integer;
  x2, y2, temp : Real;
BEGIN
	WITH event DO WITH views[VPort] DO {Look for events in lower right}
	BEGIN {With}
	  IF Ord(ReadKey)=13 Then  {Enter Key}
	  BEGIN
		SelectMyViewPort(VPort);
		SelectScale(VSnum);
		SetMyColor(rainbow[0].colors[NColor]);
		NColor := NColor + 1;
		If NColor > 15 Then NColor := 9;
		If rainbow[0].colors[NColor] = GraphBackColor THEN NColor := NColor + 1;
		HideMouse;
		ShowTrajectory;
		ShowMouse;
	  END; {If Key 13}
 CASE WhichForce of
  Walls, Pendulum, SHO, Cubic, Asymmetric :
	IF mouseClicked AND (x>vx1) AND (x<vx2) AND (y>vy1) AND (y<vy2) THEN
	BEGIN {Look for energy chosen}
		dy := (vy2-vy1);
		y2:=(vy2-y) /dy;
		Energy := GraphScales[VSNum].ymin +
						(GraphScales[VSNum].Ymax-GraphScales[VSNum].Ymin)*y2;
		IF Energy < 0 {EminA} THEN
		BEGIN
			Announce ('Energy below minimum is invalid.');
			Energy := EminA;
			Exit
		END;
		IF ((Energy > 0.9*MaxEnergy) OR (Energy < 0.1*MaxEnergy))
			AND (F0overM = 0.0) THEN  {Rescale Energy}
		BEGIN
			GetX(temp, 2*Energy+Emin);
			IF (Not Exact) AND (WhichForce = Walls) THEN ;
			IF temp = 0.0 THEN {Happens if asymmetric potential has only local
										minimum}
			BEGIN
				GetX2(temp, Energy+Emin);
				temp := Abs(temp)
			END;
         IF temp < -Pi/2.0 THEN temp := Pi;{Pendulum energy -> swing over top}
			IF temp > 0.0 THEN {Otherwise don't rescale, it fouls up}
			BEGIN {temp 0 fouls up, F0 > 0 rescale is undesirable}
				MaxPos := temp;
				FindPotential(True);
				IF (Energy + Emin) < 0.0 THEN {emin is negative}
					Energy := Energy - Emin;
				HideMouse;
				HoldVScale := False; {If set True by SelectEnergyScale}
				DisplayPotential(BothV, HoldVScale);
				ShowMouse
			END
		END; {Rescale Energy}
		DisplayChoice;
		RunInstruct;
		SelectMyViewPort(VPort);
		SelectScale(VSnum);
		SetMyColor(rainbow[0].colors[NColor]);
		NColor := NColor + 1;
		If NColor > 15 Then NColor := 9;
		If rainbow[0].colors[NColor] = GraphBackColor THEN NColor := NColor + 1;
		HideMouse;
		If x > (vx1+vx2)/2 THEN Plus := True ELSE Plus := False; {For Phase Plot initial condition}
		ShowTrajectory;
      NewEnergyScale := False;
		ShowMouse;
	END; {Look for energy chosen}
	END; {CASE WhichForce}
 END; {With and Forces with Potentials}
 IF PlotPhase THEN
 WITH event DO WITH views[PhasePort] DO {Look for events in lower right}
	IF mouseClicked AND (x>vx1) AND (x<vx2) AND (y>vy1) AND (y<vy2) THEN
	BEGIN
		dy := (vy2-vy1);
		y2:=(vy2-y) /dy;
		dx := (vx2-vx1);
		x2:=(x-vx1) /dx;
		x0Old := GraphScales[PhaseSNum].xmin +
						(GraphScales[PhaseSNum].Xmax-GraphScales[PhaseSNum].Xmin)*x2;
		v0Old := GraphScales[PhaseSNum].ymin +
						(GraphScales[PhaseSNum].Ymax-GraphScales[PhaseSNum].Ymin)*y2;
		PhasePicked := True; Plus := True;
		IF Exact THEN
			CASE WhichForce of
				Asymmetric : Energy := AsymmetricPotential(X0Old);
				Cubic : Energy := CubicPotential(X0Old);
				Pendulum : Energy := ExactPendPotential(X0Old);
				SHO : Energy := SHOPotential(X0Old);
				Walls : Energy := ExactWallPotential(X0Old);
				VanDePol : {No potential};
			END {Case Ignore i}
		ELSE
			Case WhichForce of
				Pendulum : Energy := ApproxPendPotential(X0Old);
				Walls : Energy := ApproxWallPotential(X0Old)
			{The other forces have no approximate expressions}
			END; {Case}

		Energy := Energy + 0.5*Mass*Sqr(v0Old) - Emin;
		SetMyColor(rainbow[0].colors[NColor]);
		NColor := NColor + 1;
		If NColor > 15 Then NColor := 9;
		If rainbow[0].colors[NColor] = GraphBackColor THEN NColor := NColor + 1;
		HideMouse;
		RunInstruct;
		ShowTrajectory;
		PhasePicked := False;
		ShowMouse;
 END {IF PlotPhase THEN WITH .. CASE Walls}
END; {PickEnergy}

Procedure FlipEm(Slow, Return : Boolean);
VAR
	i, jb, jbOld, j, DTime :Integer;
	Done, Pausing : Boolean;
	Key : Byte;
	Plotsav : Integer;
	PSavTime, PSavVel, PSavPhase, PSavPoincare, PSavReturn : Boolean;
BEGIN
	PSavTime := PLotTime; PSavVel := PLotVel; PSavPhase := PLotPhase;
	PSavPoincare := PLotPoincare; PSavReturn := PLotReturn; PlotSav := Plots;
	SetAllFalse; Plots := 1; PlotPoincare := True; SetPlots;
	SetFillStyle(solidfill,White);
	SetUpFlipKeys;
	DTime := 0; Done := False;
	If Slow THEN DTime := 200; Pausing := False;
	IF Return THEN jb := 2 ELSE jb := 1;
	i := 1;
	REPEAT {Until Done}
		 jbold := jb;
		 CloseMyViewPort(PoincarePort);
		 If Not Pausing THEN
			 Message ('Use F- keys as indicated. '+Numstr(MapLength,3,0)
					 +' points. Phase: '+Numstr(i,2,0)+' Pi/'+Numstr(NMap,2,0));
		 {$R-}
		 For j := jb to MapLength DO
			 IF Return THEN
				 PutPixel(VMapPtr[i]^[j], VMapPtr[i]^[j-1], White)
			 ELSE
				 PutPixel(XMapPtr[i]^[j], VMapPtr[i]^[j], White);
		 {$R+}
		 Delay(DTime);
		 If Pausing THEN
		 BEGIN
			 Message ('Pause. To step one frame, Press any key or click mouse.'
						 +' Phase: '+Numstr(i,2,0)+' Pi/'+Numstr(NMap,2,0));
			 DynamicPause
		 END;
		 CheckForEvents;
	    If Event.ReadKey = Chr(27) THEN Done := True;
		 IF FlipKeys.Pressed(key) THEN
			 DoFlipKeys(key,jb,DTime,Done,Pausing);
		 IF DTime < 0 THen DTime := 0;
		 IF jb > MapLength - 10 THEN jb := MapLength - 10;
		 IF (Not Pausing) OR (jb = jbold) THEN i := i + 1;
		 IF i > NMap THEN i := 1
	UNTIL Done;
	SetUpSystemKeys;
	DisplayMainMenu;
	If Not InTutorial THEN
	BEGIN
		PLotTime := PSavTime; PLotVel := PSavVel; PLotPhase := PSavPhase;
		PLotPoincare := PSavPoincare; PLotReturn := PSavReturn; Plots := PlotSav;
		SetPlots;
      ClearPhaseport
	END;
END; {FlipEm}

BEGIN {Always set Version to that of the unit with the largest Version number}
	AsCoef3 := 0.1; AsCoef4 := 0.1;
	If MyVersion > Version THEN Version := MyVersion
END. {UNIT AnhPhy}
