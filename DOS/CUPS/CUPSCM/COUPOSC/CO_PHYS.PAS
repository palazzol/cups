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
{| PHYSICS PROCEDURES FOR COUPLED OSCILLATORS
{|    SIM2 for MECHANICS for CUPS
{| Version D
{| Author:  Randall Jones
{| Uses (1-14-93) CUPS utilities
{| Date:  {1/93}
{+-------------------------------------------------------------------}

UNIT CO_PHYS;
INTERFACE
USES Graph, Crt, Dos, CUPS, CUPSMUPP, CUPSgui, CupsGrph, CUPSFunc, CUPSProc;

TYPE
   AnimObj = Array[1..100] OF Integer;
   MassTypes = (Inf,mA,mB,mC);
   MassInfo = RECORD
         Mass:Real;
         Radius:Word;
         Color:Word;
         MassImage:AnimObj;
      END;
   MassInstance = RECORD
         xEquilPos,yEquilPos:Real;
         Class:MassTypes;
      END;
   SpringTypes = (sA,sB,sC,sD);
   SpringInfo = RECORD
         SpringConst:Real;
         Width:Real;
         nDiv:Word;
         Color:Word;
      END;
   SpringInstance = RECORD
         End1,End2:Word;
         Class:SpringTypes;
      END;
   HowCalc = (NormalModeCalc,NumericalCalc);
   Str12 = String[12];
   NumParmType = RECORD
         Damping: Real;
         DriveForce,CurExtForce,DFAngle,XComp,YComp:Real;
         DFMax,ExtFreq,EFMax:Real;
         AppliedMass:Word;
         TxtX,TxtY:Real;
         ExtForceLabel:String;
      END;

CONST
   MaxMass = 10;
   MaxSprings = 50;
   MassCode: ARRAY [MassTypes] OF STRING[3] = ('INF','A  ','B  ','C  ');
   SpringCode: ARRAY [SpringTypes] OF STRING[4] =
               ('A   ','B   ','C   ','D   ');
   NumBarDiv = 10;
   BarChr:CHAR = CHR(219);

TYPE
   ConfigType = RECORD
         Description:StrING;
         NumMasses,NumFreeMasses,NumInfMasses,NumSprings:Integer;
         NumModes:Integer;
         MassVals:ARRAY [MassTypes] OF MassInfo;
         SpringVals:ARRAY [SpringTypes] OF SpringInfo;
         Masses:ARRAY [1..MaxMass] OF MassInstance;
         Springs:ARRAY [1..MaxSprings] OF SpringInstance;
         Evecs,InvEvecs,DynMat: DMatrix;
         MassScale:DVector;
         EVals,Amplitude,Phase,Displacements,OldDisp,Velocities:DVector;
         TwoDim,EVecDone,EquilDrawn,xVERSUSt:Boolean;
         CalcMethod:HowCalc;
         SaveData,FileOpen:Boolean;
         FileName:String;
         MaxModeEnergy:Real;
         NumParms:NumParmType;
      END;
   ModePutType = RECORD
         AspectFactor:Real;
         xInitPhase,PhaseRadiusX,PhaseRadiusY:Word;
         h,w,dx,dy,yInit,xInitBar,xInitStar:Word;
      END;
   NumBarCharType = ARRAY[1..20] OF Word;
VAR
   Config:ConfigType;
   DiskAmplitude,DiskPhase:ARRAY[1..20] OF Real;
   Work:DVector;
   Work2:Dvector;
   PosVel,PosVelP:Dvector;
   DegToRad:Real;
   dtAnim,dtAnimMax,t,dt:Real;
   ModeUpDate:Boolean;
   ModePutInfo:ModePutType;

   PROCEDURE SortMasses(VAR Config:ConfigType);
   PROCEDURE GetEvecs(VAR Config:ConfigType);
   PROCEDURE GenDynMat(VAR Config:ConfigType);
   PROCEDURE GetPosNumerical(VAR Config:ConfigType; t,dt,dtAnim:Real);
   PROCEDURE SetUpRK4(VAR Config:ConfigType);
   PROCEDURE GenDispVel(VAR Config:ConfigType; t:Real);
   PROCEDURE GetModes(VAR Config:ConfigType; t:Real);


IMPLEMENTATION
   PROCEDURE SortMasses(VAR Config:ConfigType);
  {+-------------------------------------------------------------------}
  {| This procedure moves the masses around to put the infinite masses
  {| at the end of the list.
  {+-------------------------------------------------------------------}
   VAR
     OldMasses:ARRAY[1..MaxMass] OF MassInstance;
     NewMassPositions:ARRAY[1..MaxMass] OF Byte;
     NextInf,NextFree,i:Integer;
   BEGIN
      WITH Config DO
         BEGIN
            FOR i := 1 to NumMasses DO OldMasses[i] := Masses[i];
            NextInf := NumMasses;
            NextFree := 1;
            FOR i := 1 to NumMasses DO
               IF OldMasses[i].Class = Inf THEN
                  BEGIN
                     NewMassPositions[i] := NextInf;
                     Masses[NextInf] := OldMasses[i];
                     NextInf := NextInf - 1;
                  END
               ELSE
                  BEGIN
                     NewMassPositions[i] := NextFree;
                     Masses[NextFree] := OldMasses[i];
                     NextFree := NextFree + 1;
                  END;
            NumFreeMasses := NextFree - 1;
            NumInfMasses := NumMasses - NumFreeMasses;
           {+----------------------------------------------------------}
           {| Change Mass numbers for Spring definitions
           {+----------------------------------------------------------}
            If NumInfMasses <> 0 THEN FOR i := 1 to NumSprings DO
               BEGIN
                  Springs[i].End1 := NewMassPositions[Springs[i].End1];
                  Springs[i].End2 := NewMassPositions[Springs[i].End2];
               END;
         END;
   END;

   PROCEDURE AddTerm(VAR DynMat:DMatrix; i,j:Word; term:Real);
   BEGIN
      DynMat.Put(i,j,term+DynMat.Value(i,j));
      IF i<>j THEN
         DynMat.Put(j,i,term+DynMat.Value(j,i));
   END;

   PROCEDURE GenDynMat(VAR Config:ConfigType);
  {+-------------------------------------------------------------------}
  {| This procedure determines the dynamical matrix.
  {+-------------------------------------------------------------------}
   VAR
      CoordPos,InfPos,i,j:Integer;
      Mass1,Mass2,HoldMass,x1Coord,x2Coord,y1Coord,y2Coord:Word;
      xDist,yDist,xxTerm,yyTerm,xyTerm,EquilSQR,k:Real;
      MassFact:Real;
   BEGIN
      WITH Config DO
         BEGIN
           {+----------------------------------------------------------}
           {| Determine number of dynamical coordinates and initialize
           {| dynamical variables.
           {+----------------------------------------------------------}
            IF TwoDim THEN NumModes := 2*NumFreeMasses
                      ELSE NumModes := NumFreeMasses;
            EVecs.Resize(NumModes,NumModes);
            InvEVecs.Resize(NumModes,NumModes);
            DynMat.Resize(NumModes,NumModes); DynMat.Fill(0.0);
            MassScale.Resize(NumModes);
            EVals.Resize(NumModes);
            Amplitude.Resize(NumModes);
            Phase.Resize(NumModes);
            OldDisp.Resize(NumModes);
            Displacements.Resize(NumModes);
            Velocities.Resize(NumModes);
            Work.Resize(NumModes);
            Work2.Resize(NumModes);
            PosVel.Resize(2*NumModes);
            PosVelP.Resize(2*NumModes);
           {+----------------------------------------------------------}
           {| Generate Dynamical Matrix
           {+----------------------------------------------------------}
            FOR i := 1 to NumSprings DO
               BEGIN
                  Mass1 := Springs[i].End1;
                  Mass2 := Springs[i].End2;
                  k := SpringVals[Springs[i].Class].SpringConst;
                  IF Inf=Masses[Mass1].Class THEN
                     BEGIN
                        HoldMass := Mass1;
                        Mass1 := Mass2;
                        Mass2 := HoldMass;
                     END;
                  IF Inf<>Masses[Mass1].Class THEN
                     BEGIN
                        x1Coord := Mass1;
                        x2Coord := Mass2;
                        IF NOT TwoDim
                           THEN xxTerm := k
                           ELSE
                              BEGIN
                                 xDist := Masses[Mass2].xEquilPos -
                                          Masses[Mass1].xEquilPos;
                                 yDist := Masses[Mass2].yEquilPos -
                                          Masses[Mass1].yEquilPos;
                                 EquilSQR := SQR(xDist) + SQR(yDist);
                                 xxTerm := k*SQR(xDist)/EquilSQR;
                                 yyTerm := k*SQR(yDist)/EquilSQR;
                                 xyTerm  :=k*xDist*yDist/EquilSQR;
                                 y1Coord := NumFreeMasses + Mass1;
                                 y2Coord := NumFreeMasses + Mass2;
                              END;
                       {+----------------------------------------------}
                       {| Insert terms for this spring into DynMat.
                       {+----------------------------------------------}
                        IF Inf=Masses[Mass2].Class
                           THEN
                              BEGIN
                                 AddTerm(DynMat,x1Coord,x1Coord,xxTerm);
                                 IF TwoDim THEN
                                    BEGIN
                                       AddTerm(DynMat,y1Coord,y1Coord,yyTerm);
                                       AddTerm(DynMat,x1Coord,y1Coord,xyTerm);
                                    END;
                              END
                           ELSE
                              BEGIN
                                 AddTerm(DynMat,x1Coord,x1Coord,xxTerm);
                                 AddTerm(DynMat,x2Coord,x2Coord,xxTerm);
                                 AddTerm(DynMat,x1Coord,x2Coord,-xxTerm);
                                 IF TwoDim THEN
                                    BEGIN
                                       AddTerm(DynMat,y1Coord,y1Coord,yyTerm);
                                       AddTerm(DynMat,y2Coord,y2Coord,yyTerm);
                                       AddTerm(DynMat,x1Coord,y1Coord,xyTerm);
                                       AddTerm(DynMat,x2Coord,y2Coord,xyTerm);
                                       AddTerm(DynMat,x1Coord,y2Coord,-xyTerm);
                                       AddTerm(DynMat,x2Coord,y1Coord,-xyTerm);
                                       AddTerm(DynMat,y1Coord,y2Coord,-yyTerm);
                                    END;
                              END;
                     END;
               END; {i Loop}
           {+----------------------------------------------------------}
           {| The mass of the objects are taken care of by scaling the
           {| coordinates - see text
           {+----------------------------------------------------------}
            FOR i := 1 TO NumFreeMasses DO
               BEGIN
                  MassFact := 1.0/SQRT(MassVals[Masses[i].Class].Mass);
                  MassScale.Put(i,MassFact);
                  IF TwoDim THEN MassScale.Put(NumFreeMasses+i,MassFact);
               END;
            FOR i := 1 to NumModes DO
               FOR j := 1 to NumModes DO
                  DynMat.Put(i,j,DynMat.Value(i,j)*MassScale.Value(i)*
                                 MassScale.Value(j));
         END;

   END;

PROCEDURE Jacobi(VAR a:DMatrix; n:Integer; VAR d:DVector;
                 VAR v:DMatrix; nRot:Integer);
LABEL 99;
VAR
   j,iq,ip,i:Integer;
   tresh,theta,tau,t,sm,s,h,g,c:Real;
   hold1,hold2:Real;
   b,z:DVector;
BEGIN
   b.Init(n);
   z.Init(n);
   v.Fill(0.0);
   FOR ip := 1 TO n DO
      BEGIN
         v.Put(ip,ip,1.0);
         hold1 := a.Value(ip,ip);
         b.Put(ip,hold1);
         d.Put(ip,hold1);
      END;
   nRot := 0;
   FOR i := 1 TO 50 DO
      BEGIN
        {| Sum of absolute value of off-diagonal elements|}
        {| Normal return relies on machine underflow |}
         sm := 0.0;
         FOR ip := 1 TO n-1 DO
            FOR iq := ip+1 TO n DO
               sm := sm + abs(a.Value(ip,iq));
         IF sm=0.0 THEN GOTO 99;
         IF i<4 THEN tresh := 0.2*sm/sqr(n)
                ELSE tresh := 0.0;
         FOR ip := 1 to n-1 DO
            BEGIN
               FOR iq := ip+1 TO n DO
                  BEGIN
                     g := 100.0*abs(a.Value(ip,iq));
                     Hold1 := abs(d.Value(ip));
                     Hold2 := abs(d.Value(iq));
                     IF (i>4) AND (Hold1+g=Hold1) AND (Hold2+g=Hold2)
                        THEN a.Put(ip,iq,0.0)
                        ELSE IF abs(a.Value(ip,iq))>tresh THEN
                           BEGIN
                              h := d.Value(iq) - d.Value(ip);
                              IF abs(h)+g = abs(h)
                                 THEN t := a.Value(ip,iq)/h
                                 ELSE
                                    BEGIN
                                       theta := 0.5*h/a.Value(ip,iq);
                                       t := 1.0/(abs(theta)+
                                                 sqrt(1+sqr(theta)));
                                       IF theta<0.0 THEN t := -t;
                                    END;
                              c := 1.0/sqrt(1+sqr(t));
                              s := t*c;
                              tau := s/(1.0+c);
                              h := t*a.Value(ip,iq);
                              z.Put(ip,z.Value(ip)-h);
                              z.Put(iq,z.Value(iq)+h);
                              d.Put(ip,d.Value(ip)-h);
                              d.Put(iq,d.Value(iq)+h);
                              a.Put(ip,iq,0.0);
                              FOR j := 1 TO ip-1 DO
                                 BEGIN
                                    g := a.Value(j,ip);
                                    h := a.Value(j,iq);
                                    a.Put(j,ip,g-s*(h+g*tau));
                                    a.Put(j,iq,h+s*(g-h*tau));
                                 END;
                              FOR j := ip+1 TO iq-1 DO
                                 BEGIN
                                    g := a.Value(ip,j);
                                    h := a.Value(j,iq);
                                    a.Put(ip,j,g-s*(h+g*tau));
                                    a.Put(j,iq,h+s*(g-h*tau));
                                 END;
                              FOR j := iq+1 TO n DO
                                 BEGIN
                                    g := a.Value(ip,j);
                                    h := a.Value(iq,j);
                                    a.Put(ip,j,g-s*(h+g*tau));
                                    a.Put(iq,j,h+s*(g-h*tau));
                                 END;
                              FOR j := 1 TO n DO
                                 BEGIN
                                    g := v.Value(j,ip);
                                    h := v.Value(j,iq);
                                    v.Put(j,ip,g-s*(h+g*tau));
                                    v.Put(j,iq,h+s*(g-h*tau));
                                 END;
                              nRot := nRot + 1;
                           END;
                  END;
            END;
         FOR ip := 1 TO n DO
            BEGIN
               b.Put(ip,b.Value(ip)+z.Value(ip));
               d.Put(ip,b.Value(ip));
               z.Put(ip,0.0);
            END;
      END;
   Message('ERROR - EIGENVECTOR ROUTINE required 50 iterations');
   StaticPause;
99:
   z.Free;
   b.Free;
END;

   PROCEDURE EigSort(VAR d:DVector;
                     VAR v:DMatrix;
                         n:Integer);
   VAR
      k,j,i:Integer;
      p:Real;
   BEGIN
      FOR i := 1 to n-1 DO
         BEGIN
            k := i;
            p := d.Value(i);
            FOR j := i+1 to n DO
               BEGIN
                  IF d.Value(j) >= p THEN
                     BEGIN
                        k := j;
                        p := d.Value(j);
                     END;
               END;
            IF k <> i THEN
               BEGIN
                  d.Put(k,d.Value(i));
                  d.Put(i,p);
                  FOR j := 1 TO n DO
                     BEGIN
                        p := v.Value(j,i);
                        v.Put(j,i,v.Value(j,k));
                        v.Put(j,k,p);
                     END;
               END;
         END;
   END;

   PROCEDURE GetEvecs(VAR Config:ConfigType);
  {+-------------------------------------------------------------------}
  {| This procedure generates the eigenvector and eigenvalue matrix.
  {+-------------------------------------------------------------------}
   VAR
      mScale:Real;
      nRot:Integer;
      i,j:Integer;
      EVal:Real;
   BEGIN
      WITH Config DO
         BEGIN
            Jacobi(DynMat,NumModes,EVals,EVecs,nRot);
            EigSort(EVals,EVecs,NumModes);
            FOR i := 1 to NumModes DO
               IF EVals.Value(i)>=0.001
                  THEN EVals.Put(i,SQRT(EVals.Value(i)))
                  ELSE EVals.Put(i,0.0);
           {+----------------------------------------------------------}
           {| Note that DynMat is changed by Jacobi to be zero in the
           {| upper diagonal.  This must be changed back for the
           {| force calculation required for the numerical analysis.
           {+----------------------------------------------------------}
            FOR i := 2 to NumModes DO
               FOR j := 1 TO i-1 DO
                  DynMat.Put(j,i,DynMat.Value(i,j));
           {+----------------------------------------------------------}
           {| Rescale Evecs to account for the masses and store the
           {| inverse of Evecs.  Note that this is not simply the
           {| the transpose because of the mass scaling.
           {+----------------------------------------------------------}
            FOR i := 1 to NumModes DO
               BEGIN
                  MScale := MassScale.Value(i);
                  FOR j := 1 to NumModes DO
                     BEGIN
                        InvEvecs.Put(j,i,Evecs.Value(i,j)/MScale);
                        EVecs.Put(i,j,Evecs.Value(i,j)*MScale);
                     END;
               END;
            EVecDone := True;
         END;
   END;

   PROCEDURE GetAccel(VAR Config:ConfigType; t:Real;
                      VAR PosVel,Accel:DVector);
  {+-------------------------------------------------------------------}
  {| This procedure calculates the acceleration.  Note that it also
  {| updates the dynamic vector, Displacements.
  {+-------------------------------------------------------------------}
   VAR
      i,iycomp:Integer;
      DampAccel,Mass:Real;
   BEGIN
      WITH Config DO
         BEGIN
            FOR i := 1 to NumModes DO
               Displacements.Put(i,PosVel.Value(i));
            DynMat.MultVector(Displacements,Accel);
            Accel.MultScalar(-1.0);
            IF NumParms.Damping<>0 THEN WITH NumParms DO
               FOR i := 1 to NumFreeMasses DO
                  BEGIN
                     Mass := MassVals[Masses[i].Class].Mass;
                     DampAccel := Damping*PosVel.Value(NumModes+i)/Mass;
                     Accel.Put(i,Accel.Value(i)-DampAccel);
                     IF TwoDim THEN
                        BEGIN
                           iycomp := i + NumFreeMasses;
                           DampAccel := Damping*PosVel.Value(NumModes+iyComp)/Mass;
                           Accel.Put(iyComp,Accel.Value(iyComp)-DampAccel);
                        END;
                     END;
            IF NumParms.DriveForce<>0 THEN WITH NumParms DO
               BEGIN
                  Mass := MassVals[Masses[AppliedMass].Class].Mass;
                  CurExtForce := DriveForce*COS(t*ExtFreq*2*Pi);
                  Accel.Put(AppliedMass,Accel.Value(AppliedMass) +
                               CurExtForce*xComp/Mass);
                  IF TwoDim THEN
                     Accel.Put(AppliedMass+NumFreeMasses,
                               Accel.Value(AppliedMass+NumFreeMasses) +
                               CurExtForce*yComp/Mass);
               END;
         END;
   END;

   PROCEDURE RK4Deriv(t:Real; VAR PosVel,PosVelP:DVector);FAR;
  {+-------------------------------------------------------------------}
  {| This procedure inserts the derivative of PosVel into PosVelP.
  {|    The first NumModes elements of PosVel are the Positions
  {|    The next  NumModes elements of PosVel are the Velocities
  {+-------------------------------------------------------------------}
   VAR
      i:Integer;
   BEGIN
      WITH Config DO
         BEGIN
            GetAccel(Config,t,PosVel,Work);   {Work will hold the accelerations}
            FOR i := 1 to NumModes DO
               BEGIN
                  PosVelP.Put(i,PosVel.Value(NumModes+i));
                  PosVelP.Put(NumModes+i,Work.Value(i));
               END;
         END;
   END;

   PROCEDURE GetPosNumerical(VAR Config:ConfigType; t,dt,dtAnim:Real);
  {+-------------------------------------------------------------------}
  {| This procedure uses the RK4 method to generate the next value
  {| of the position and velocity.
  {| Note that the positions and velocities are stored in the dynamic
  {| vector PosVel since RK4 needs all the dynamic variables in one
  {| array.  Positions are also written into Displacements by GetAccel.
  {+-------------------------------------------------------------------}
   VAR
      i,NumIter: integer;
      tEnd: Real;
   BEGIN
      tEnd := t + dtAnim;
      NumIter:=Trunc(Abs(dtAnim)/dt);
      if (dtAnim<0) then dt:=-dt;
      For i:=1 TO NumIter DO StepRK4(RK4Deriv,t,dt,PosVel);
      {Include remaining interval}
      dt:=tEnd-t;
      IF dt<>0.0 THEN StepRk4(RK4Deriv,t,dt,PosVel);
   END;

   PROCEDURE GenDispVel(VAR Config:ConfigType; t:Real);
  {+-------------------------------------------------------------------}
  {| This procedure determines the positions and velocities at time t
  {| from the normal mode amplitudes and phases.  Values are stored in
  {| dynamical arrays Displacements and Velocities, and in PosVel.
  {| Time dependence can be dropped from this.
  {+-------------------------------------------------------------------}
   VAR
      i:Integer;
      NormVal:Real;
   BEGIN
      WITH Config DO
         BEGIN
            FOR i:= 1 to NumModes DO
               BEGIN
                  NormVal := Amplitude.Value(i) * 
                                COS(EVals.Value(i)*t + Phase.Value(i));
                  Work.Put(i,NormVal);
                  NormVal := - Amplitude.Value(i) * EVals.Value(i)*
                          SIN(EVals.Value(i)*t + Phase.Value(i));
                  Work2.Put(i,NormVal);
               END;
            EVecs.MultVector(Work,Displacements);
            EVecs.MultVector(Work2,Velocities);
            FOR i := 1 to NumModes DO
               BEGIN
                  PosVel.Put(i,Displacements.Value(i));
                  PosVel.Put(i+NumModes,Velocities.Value(i));
               END;
       END;
    END;

   PROCEDURE SetUpRK4(VAR Config:ConfigType);
  {+-------------------------------------------------------------------}
  {| This procedure sets up variables for numerical integration.
  {+-------------------------------------------------------------------}
   VAR
      i:Integer;
   BEGIN
      WITH Config DO
         BEGIN
            NumParms.CurExtForce := NumParms.DriveForce;
         END;
      GenDispVel(Config,0.0);
   END;

   PROCEDURE GetModes(VAR Config:ConfigType; t:Real);
  {+-------------------------------------------------------------------}
  {| This procedure determines the occupation (amplitude and phase) of
  {| the normal modes given the positions and velocities of the objects
  {| at a time, t.  Position values stored in Displacements are used,
  {| Velocity values stored in PosVel are used and written to Velocities.
  {| This is because GetPosNumerical updates Displacements but not
  {| Velocities.
  {| Zero frequency modes are now calculated as well.
  {+-------------------------------------------------------------------}
   VAR
      i:Integer;
      Amp,Phi,Omega,Arg:Real;
   BEGIN WITH Config DO
      BEGIN
         FOR i := 1 to NumModes DO
            Velocities.Put(i,PosVel.Value(NumModes+i));
         InvEVecs.MultVector(Displacements,Work);
         InvEvecs.MultVector(Velocities,Work2);
         FOR i := 1 to NumModes DO
            BEGIN
               Omega := Evals.Value(i);
               IF Omega <= 0.0
                  THEN
                     BEGIN
                        {Amplitude.Put(i,Work.Value(i));}
                        Amplitude.Put(i,0);
                        Phase.Put(i,0.0);
                     END
                  ELSE
                     BEGIN
                        Work2.Put(i,Work2.Value(i)/Omega);
                        Amp := SQRT(SQR(Work.Value(i)) +
                                    SQR(Work2.Value(i)));
                        Amplitude.Put(i,Amp);
                        IF (Amp>0.0) THEN
                           BEGIN
                              Arg := (Work2.Value(i)*Cos(Omega*t) +
                                      Work.Value(i)*Sin(Omega*t))/Amp;
                              Phi := -ArcSin(Arg);
                              IF ABS(Work.Value(i)-Amp*Cos(Omega*t+Phi))
                                 > 0.1*ABS(Work.Value(i))
                                 THEN Phi := Pi - Phi;
                              Phase.Put(i,Phi);
                           END
                           ELSE Phase.Put(i,0);
                     END;
            END;
      END;
   END;

END.
