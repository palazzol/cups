                 (**********************************************)
                 (**********************************************)
                 (**                                          **)
                 (**   Unit CUPSproc - Numerical Procedures   **)
                 (**   Written by William M. MacDonald        **)
                 (**   for CUPS Project                       **)
                 (**   Department of Physics                  **)
                 (**   University of Maryland                 **)
                 (**   College Park, Maryland 20742           **)
                 (**   (c) 1994 by William M. MacDonald       **)
                 (**   Originated:  91/02/14                  **)
                 (**   Version: 1.3 (94/11/14)                **)
                 (**                                          **)
                 (**********************************************)
                 (**********************************************)

{$O+,F+,R-}
UNIT CUPSproc;

INTERFACE
USES  CUPS, CUPSfunc;


(**** DECLARATION OF OBJECT TO PROVIDE DYNAMICALLY SIZED VECTORS ****)
TYPE
  Dbase = ARRAY[1..1] OF REAL;
  DbasePtr =^Dbase;

  DVector = OBJECT
    VecPtr   : DbasePtr;               {This points to the DVector}
    Size     : INTEGER;                {This is the number of elements used}
    MemBlock : LongInt;
    PROCEDURE Init( NumberElements: INTEGER);
    PROCEDURE Free;                                {This releases the memory}
    PROCEDURE ReSize( NumberElements: INTEGER);
    FUNCTION  Length: INTEGER;                     {This returns Size}
    PROCEDURE Put( Index: INTEGER;     X: REAL);   {Store X at Index}
    PROCEDURE Get( Index: INTEGER; VAR X: REAL);   {Recall value X at Index}
    FUNCTION  Value( Index: INTEGER) : REAL;       {Returns value at Index}
    PROCEDURE MinMax( VAR IMin: INTEGER; VAR Min: REAL;
                      VAR IMax: INTEGER; VAR Max: REAL);
    PROCEDURE Fill( FillValue : REAL);             {Fill with a value}
    PROCEDURE Equate( s: REAL; VectB : DVector);   {Equate  to s*VectB}
    PROCEDURE AddScalar( Scalar: REAL);            {Add a scalar}
    PROCEDURE MultScalar(Scalar: REAL);            {Multiply by a scalar}
    PROCEDURE AddVector( s: REAL; VectB: DVector); {Add s* VectB}
    PROCEDURE SumOfVectors( s: REAL; VAR VectA: DVector;
                            t: REAL; VAR VectB: DVector);
    FUNCTION Projection( VectB: DVector) : REAL;   {Scalar product}
    FUNCTION Simpson(min,max: Integer): Real;
    FUNCTION dVdx( i: integer; h: real) : real;
  END;


  DMatrix = OBJECT
    MatPtr       : DbasePtr;
    NRows, NCols : INTEGER;                    {dimensions of DMatrix}
    MemBlock     : LongInt;
    Determinant  : REAL;
    FUNCTION  Loc( Row,Col: INTEGER): INTEGER;
    PROCEDURE InvLoc( VecIndex: INTEGER; VAR Row,Col: INTEGER);
    PROCEDURE Init( NumRows,NumCols: INTEGER);
    PROCEDURE Free;
    PROCEDURE ReSize( NumRows,NumCols: INTEGER);
    PROCEDURE GetSize( VAR NumRows,NumCols: INTEGER);
    PROCEDURE Put( Row,Col: INTEGER; X: REAL);
    PROCEDURE Get( Row,Col: INTEGER; VAR X: REAL);
    FUNCTION  Value( Row,Col: INTEGER): REAL;
    PROCEDURE MinMax( VAR MinRow,MinCol: INTEGER; VAR Min: REAL;
                      VAR MaxRow,MaxCol: INTEGER; VAR Max: REAL);
    PROCEDURE Fill( FillValue: REAL);
    PROCEDURE Equate( InMat: DMatrix);
    PROCEDURE AddScalar(  Scalar: REAL);
    PROCEDURE MultScalar( Scalar: REAL);
    PROCEDURE MultVector( InVect: DVector; VAR OutVect: DVector);
    PROCEDURE MultMatrix( InMat : DMatrix; VAR OutMat : DMatrix);
    PROCEDURE Transpose(  VAR OutMat: DMatrix);
    FUNCTION  dMdx( row,col: integer; h: real): real;
    FUNCTION  dMdy( row,col: integer; h: real): real;
    FUNCTION  d2Mdx2( row,col: integer; h: real): real;
    FUNCTION  d2Mdy2( row,col: integer; h: real): real;
    FUNCTION  Interpolate( row,col: real): real;
  END;

  IDbase = ARRAY[1..1]  OF INTEGER;
  IDbasePtr = ^IDbase;
  IDVector = OBJECT
    IVecPtr  : IDbasePtr;
    Size     : INTEGER;                {This is the number of elements used}
    MemBlock : LongInt;
    PROCEDURE Init( NumberElements: INTEGER);
    PROCEDURE Free;                                  {This releases memory}
    PROCEDURE ReSize( NumberElements: INTEGER);
    FUNCTION  Length: INTEGER;                       {This returns Size}
    PROCEDURE Put( Index: INTEGER;     X: integer);  {Store X at Index}
    PROCEDURE Get( Index: INTEGER; VAR X: integer);  {Recall value X at Index}
    FUNCTION  Value( Index: INTEGER) : integer;      {Returns value at Index}
    PROCEDURE Equate( s: integer; VectB : IDVector); {Equate  to s*VectB}
  END;

(*******DECLARATION OF NUMERICAL PROCEDURES ******************************)

(*** PROCEDURAL VARIABLES TYPES FOR PASSING FUNCTION NAMES ***********)
TYPE
  FUNCTIONX   = FUNCTION(x: REAL): REAL;
  FUNCTIONXY  = FUNCTION(x,y: REAL): REAL;

  DFproc      = PROCEDURE(t: REAL; VAR Y,YP: DVector);

PROCEDURE Findmacheps(VAR macheps: REAL);
PROCEDURE FindZero(Fct: FUNCTIONX; a,b,Tol: REAL;
               VAR Zero: REAL; VAR iter: integer);
FUNCTION  Fmin( Fct: FUNCTIONX; a,b,Tol: REAL): REAL;
PROCEDURE Roots(Fct: FUNCTIONX; a,b,Tol: REAL;
                VAR N, J: integer; VAR M: DVector);
PROCEDURE Numerov( ibeg, iend: Integer; h: REAL; VAR A,y: DVector);
FUNCTION  NumerovDeriv(ideriv: Integer; h: REAL; VAR A,y: DVector): REAL;
PROCEDURE StepRK4(FYP: DFproc; VAR t,h: REAL; VAR Y: DVector);
PROCEDURE StepRKF(FYP: DFproc; VAR t: REAL; VAR Y:DVector; dt: REAL;
                  VAR h: REAL; hmax,Aerr,Rerr: REAL; VAR Iflag: integer);
FUNCTION Urand(VAR idum: INTEGER): REAL;
PROCEDURE ChebCoef(Fct: FUNCTIONX; a,b: REAL; VAR coeff: DVector;
                   m: INTEGER);
FUNCTION Chebeval(coeff: DVector; x: REAL; errorflag: BOOLEAN): REAL;

(*********************MATRIX ROUTINES********************************)

PROCEDURE LUsolve(VAR A: DMatrix; VAR Y: DVector; VAR IPvt: IDVector);
PROCEDURE LUdecomp(VAR A:DMatrix;VAR RCond:REAL;VAR IPvt:IDVector);
PROCEDURE LUinverse(VAR A,AI:DMatrix;VAR IPvt:IDVector);
PROCEDURE SYM2TRID(N : integer; VAR a: DMatrix; VAR d,e :DVector; ReturnTransform: BOOLEAN);
PROCEDURE Tridiag(N : Integer;  VAR d,e: DVector; FindVectors: BOOLEAN; VAR evecs: DMatrix);
(********************************************************************)

IMPLEMENTATION
USES graph;

PROCEDURE FatalError(errorStr : string);
VAR i : integer;
BEGIN
   ErrorStr := 'Error: '+ErrorStr;
   i := GetGraphMode;
   IF GraphResult=0 THEN announce(errorStr) ELSE
   begin writeln(errorStr); StaticPause; end;
   CupsDone;
   halt;
END;

(****************** INITIALIZATION ROUTINES FOR CUPS ***************)

PROCEDURE Findmacheps(VAR macheps: REAL);
BEGIN
  macheps := 1.0;
  REPEAT
    macheps := macheps/2 ;
  UNTIL ((macheps + 1.0) = 1.0 )
END;

(*********  BEGIN DYNAMIC DVector PROCEDURES *************************)

PROCEDURE DVector.Init (NumberElements: INTEGER);
 VAR
    i: INTEGER;
    MemReq,MaxBlock: String[10];
BEGIN
   Size := NumberElements;
   MemBlock:= longint(Size)*SizeOf(REAL);
   IF ((MemBlock > MaxAvail) OR (MemBlock >= 65521)) THEN
   BEGIN
     Str(MemBlock:0,MemReq);
     Str(MaxAvail:0,MaxBlock);
     Error('DVector.Init: Request for '+ MemReq +
               ' exceeds free memory ' + MaxBlock + 'or 65521');
     MemBlock := 0;
     VecPtr   := NIL;
     Size     := 0;
   END
   ELSE
     IF size=0 THEN VecPtr:=nil ELSE
     BEGIN
       GetMem(VecPtr,MemBlock);
       FOR i:=1 TO Size DO VecPtr^[i]:=0.0
     END
 END;

PROCEDURE DVector.Free;
BEGIN
   FreeMem(VecPtr,MemBlock);
   MemBlock := 0;
   VecPtr := NIL;
   Size:= 0;
END;

PROCEDURE DVector.ReSize(NumberElements: INTEGER);
VAR
   temp : DVector;
   i,num : integer;
BEGIN
   temp.init(size);
   temp.equate(1,self);
   Free;
   Init(NumberElements);
   IF size<temp.size THEN num:=size ELSE num:=temp.size;
   FOR i:=1 TO num DO VecPtr^[i]:= temp.VecPtr^[i];
   temp.free;
END;


FUNCTION DVector.Length: INTEGER;
BEGIN
    Length:= Size;
END;

PROCEDURE DVector.Put(Index   : INTEGER;  {input}
                         X     : REAL   {input});
BEGIN
{$IFOPT D+}
   IF (Index>Size) OR (Index<1) THEN
     FatalError('DVector.Put: Index exceeds length of the vector');
{$ENDIF}
   VecPtr^[Index]:=X;
END;

PROCEDURE DVector.Get(Index: INTEGER; VAR X: REAL);
BEGIN
{$IFOPT D+}
   IF (Index>Size) OR (Index<1) THEN
     FatalError('DVector.Get: Index exceeds length of the vector');
{$ENDIF}
   X:=VecPtr^[Index]
END;

FUNCTION  DVector.Value(Index: INTEGER): REAL;
BEGIN
{$IFOPT D+}
   IF (Index>Size) OR (Index<1) THEN
     FatalError('DVector.Value: Index exceeds length of the vector');
{$ENDIF}
   Value:=VecPtr^[Index]
END;

PROCEDURE DVector.MinMax(VAR Imin: INTEGER; VAR Min: REAL;
                 VAR Imax: INTEGER; VAR Max: REAL);
VAR
   i: INTEGER;
   tval: REAL;
BEGIN
   Imin:=1; Imax:=1;
   Min:=VecPtr^[1]; Max:=VecPtr^[1];
   FOR  i:=2 TO Size DO
      BEGIN
         tval:=VecPtr^[i];
         IF tval<Min THEN
            BEGIN
               Imin:=i; Min:=tval
            END;
         IF tval>Max THEN
            BEGIN
               Imax:=i; Max:=tval
            END
      END
END;


PROCEDURE DVector.Fill(FillValue: REAL);
VAR
   i: INTEGER;
BEGIN
   FOR i:= 1 TO Size DO
       VecPtr^[i]:= FillValue;
END;

PROCEDURE DVector.Equate(s: REAL; VectB: DVector {input});
VAR
  i: INTEGER;
BEGIN
   IF size<>VectB.size THEN
    FatalError('DVector.Equate: equating two vectors of different size');
   FOR i:=1 TO Size DO
       VecPtr^[i]:= s*VectB.VecPtr^[i]
END;


PROCEDURE DVector.AddScalar(Scalar: REAL {input});
VAR
   i: INTEGER;
BEGIN
   FOR i:= 1 TO Size DO
      VecPtr^[i]:= VecPtr^[i] + Scalar
END;

PROCEDURE DVector.MultScalar(Scalar: REAL {input});
VAR
  i: INTEGER;
BEGIN
   FOR i:= 1 TO Size DO
      VecPtr^[i]:= VecPtr^[i]*Scalar
END;

PROCEDURE DVector.AddVector(s: REAL; VectB: DVector {input});
VAR
  i: INTEGER;
BEGIN
   IF size<>VectB.size THEN
    FatalError('DVector.AddVector: adding two vectors of different size');
   FOR i:=1 TO VectB.Size DO
      VecPtr^[i]:= VecPtr^[i] + s*VectB.VecPtr^[i]
END;

PROCEDURE DVector.SumOfVectors(s: REAL; VAR VectA: DVector;
                               t: REAL; VAR VectB: DVector);
VAR i: INTEGER;
BEGIN
   IF (size<>VectB.size) OR (size<>VectA.size) THEN
    FatalError('DVector.SumOfVector: summing two vectors of different size');
   FOR i := 1 TO Size DO
      VecPtr^[i] := s*VectA.VecPtr^[i] + t*VectB.VecPtr^[i]
END;


FUNCTION DVector.Projection(VectB: DVector {input}): REAL;
VAR
  i: INTEGER;
  Sum: REAL;
BEGIN
   IF (size<>VectB.size) THEN
    FatalError('DVector.Projection: projecting vector has incorrect size');
   Sum:=0;
   FOR i:= 1 TO Size DO
      Sum:= Sum + VecPtr^[i]*VectB.VecPtr^[i];
   Projection:= Sum
END;

FUNCTION DVector.Simpson(min,max: Integer): Real;
VAR
  i,k,nsteps,sign: INTEGER;
  sum            : REAL;
BEGIN
  sign:=1;
  IF min>max THEN
    BEGIN
      sign:=-1; k:=min; min:=max; max:=min;
    END
  ELSE IF max=min THEN Simpson:=0
  ELSE
    BEGIN
      nsteps:= (max-min) DIV 2;
      IF nsteps = 0 THEN
        sum:=(VecPtr^[min]+VecPtr^[max])/2
      ELSE
        BEGIN
          k:=min;
          sum:=VecPtr^[min];
          FOR i:=1 TO nsteps DO
          BEGIN
             k:=k+2;
             sum:=sum + 4*VecPtr^[k-1] + 2*VecPtr^[k];
          END;
          sum:=(sum-VecPtr^[k])/3;
          IF k <max THEN
          sum:=sum + (VecPtr^[max-3]-5*VecPtr^[max-2]
                  +19*VecPtr^[max-1]+9*VecPtr^[max])/24
        END;
      Simpson:=sign*sum
    END
END;

FUNCTION DVector.dVdx( i: integer; h: real) : real;
VAR
   x : real;
BEGIN
{$IFOPT D+}
   IF (i>Size) OR (i<1) THEN
     FatalError('DVector.dVdx: Index exceeds length of the vector');
{$ENDIF}
   x := 0;
   IF (i>2) AND (i<size-1) THEN    {accuracy 5}
    x := (VecPtr^[i-2]-8*VecPtr^[i-1]+8*VecPtr^[i+1]-VecPtr^[i+2])/(12*h)
   ELSE IF (i=1) THEN
    x := (-25*VecPtr^[i]+48*VecPtr^[i+1]-36*VecPtr^[i+2]+16*VecPtr^[i+3]-3*VecPtr^[i+4])/(12*h)
   ELSE IF (i=2) THEN
    x := (-3*VecPtr^[i-1]-10*VecPtr^[i]+18*VecPtr^[i+1]-6*VecPtr^[i+2]+VecPtr^[i+3])/(12*h)
   ELSE IF (i=size-1) THEN
    x := (-VecPtr^[i-3]+6*VecPtr^[i-2]-18*VecPtr^[i-1]+10*VecPtr^[i]+3*VecPtr^[i+1])/(12*h)
   ELSE IF (i=size) THEN
    x := (3*VecPtr^[i-4]-16*VecPtr^[i-3]+36*VecPtr^[i-2]-48*VecPtr^[i-1]+25*VecPtr^[i])/(12*h);
   dVdx := x;
END;


(**************** END DYNAMIC DVector PROCEDURES ********************)

(*************** BEGIN DYNAMIC DMatrix PROCEDURES********************)

FUNCTION DMatrix.Loc(Row,Col: INTEGER): INTEGER;
BEGIN
   Loc:= (Row -1)*NCols + Col
END;

PROCEDURE DMatrix.InvLoc(VecIndex: INTEGER; VAR Row,Col: INTEGER);
BEGIN
{$IFOPT D+}
   IF VecIndex>NRows*NCols THEN
     FatalError('DMatrix.InvLoc: VecIndex exceeds NRows*NCols');
{$ENDIF}
   Row:= ((VecIndex-1) DIV NCols)+1;
   Col:= ((VecIndex-1) MOD NCols)+1
END;

PROCEDURE DMatrix.Init (NumRows, NumCols: INTEGER);
VAR
   Row,Col,pos: INTEGER;
   MemReq, MaxBlock: String[10];
BEGIN
   NRows:= NumRows; NCols:= NumCols;
   MemBlock := longint(NCols)*longint(NRows)*SizeOf(REAL);
   IF ((MemBlock > MaxAvail) OR (MemBlock >=65521)) THEN
     BEGIN
     Str(MemBlock:0,MemReq);
     Str(MaxAvail:0,MaxBlock);
     Error('DMatrix: Request for '+ MemReq +
                ' exceeds free memory ' + MaxBlock + ' or 65521');
     MemBlock := 0;
     MatPtr := NIL;
     NRows:= 0; NCols:= 0;
     END
   ELSE
     IF (NRows*NCols=0) THEN MatPtr := NIL ELSE
     BEGIN
     GetMem(MatPtr,MemBlock);
     FOR pos:=1 TO NRows*NCols DO MatPtr^[pos] := 0.0
     END
END;

PROCEDURE DMatrix.Free;
BEGIN
   FreeMem(MatPtr,MemBlock);
   MemBlock := 0;
   MatPtr := NIL;
   NRows:= 0; NCols:= 0;
END;

PROCEDURE DMatrix.ReSize (NumRows, NumCols: INTEGER);
VAR
   temp : DMatrix;
   row,col,i,j : integer;
BEGIN
   temp.init(NRows,NCols);
   temp.equate(self);
   Free;
   Init(NumRows,NumCols);
   IF NRows<Temp.NRows THEN row:=NRows ELSE row:=temp.NRows;
   IF NCols<Temp.NCols THEN col:=NCols ELSE Col:=temp.NCols;
   FOR i:=1 TO col DO
    FOR j:=1 TO row DO
     MatPtr^[(j-1)*NCols+i]:= temp.MatPtr^[(j-1)*temp.NCols+i];
   temp.free;
END;

PROCEDURE  DMatrix.GetSize(VAR NumRows,NumCols: INTEGER);
BEGIN
   NumRows:= NRows; NumCols:= NCols;
END;

PROCEDURE DMatrix.Put(Row,Col: INTEGER; X: REAL);
BEGIN
{$IFOPT D+}
   IF (Row>NRows) OR (Row<1) THEN
     FatalError('DMatrix.Put: Row exceeds number of rows in the matrix')
   ELSE IF (Col>NCols) OR (Col<1) THEN
     FatalError('DMatrix.Put: Col exceeds number of columns in the matrix');
{$ENDIF}
   MatPtr^[(Row -1)*NCols + Col]:= X
END;

PROCEDURE DMatrix.Get(Row, Col: INTEGER; VAR X: REAL);
BEGIN
{$IFOPT D+}
   IF (Row>NRows) OR (Row<1) THEN
     FatalError('DMatrix.Get: Row exceeds number of rows in the matrix')
   ELSE IF (Col>NCols) OR (Col<1) THEN
     FatalError('DMatrix.Get: Col exceeds number of columns in the matrix');
{$ENDIF}
   X:= MatPtr^[(Row -1)*NCols + Col]
END;

FUNCTION DMatrix.Value(Row,Col: INTEGER): REAL;
BEGIN
{$IFOPT D+}
   IF (Row>NRows) OR (Row<1) THEN
     FatalError('DMatrix.Value: Row exceeds number of rows in the matrix')
   ELSE IF (Col>NCols) OR (Col<1) THEN
     FatalError('DMatrix.Value: Col exceeds number of columns in the matrix');
{$ENDIF}
   Value:= MatPtr^[(Row -1)*NCols + Col]
END;

PROCEDURE DMatrix.MinMax(VAR MinRow,MinCol: INTEGER; VAR Min: REAL;
                         VAR MaxRow,MaxCol: INTEGER; VAR Max: REAL);
VAR
   IMin,IMax: INTEGER;
   NElts,Elt: INTEGER;
   tval: REAL;
BEGIN
   NElts:=NRows*NCols;
   Imin:=1; Imax:=1;
   Min:=MatPtr^[1]; Max:=MatPtr^[1];
   FOR  Elt:=2 TO NElts DO
      BEGIN
         tval:=MatPtr^[Elt];
         IF tval<Min THEN
            BEGIN
               Imin:=Elt; Min:=tval
            END;
         IF tval>Max THEN
            BEGIN
               Imax:=Elt; Max:=tval
            END
      END;
   InvLoc(IMin,MinRow,MinCol);
   InvLoc(IMax,MaxRow,MaxCol)
END;


PROCEDURE DMatrix.Fill(FillValue: REAL);
VAR
  pos: INTEGER;
BEGIN
   FOR pos:= 1 TO NRows*NCols DO MatPtr^[pos]:= FillValue;
END;

PROCEDURE DMatrix.Equate(InMat: DMatrix);
VAR
  pos: INTEGER;
BEGIN
   IF (NCols<>InMat.NCols) OR (NRows<>InMat.NRows) THEN
    FatalError('DMatrix.Equate: equating two matrixes of different size');
   FOR pos:= 1 TO NRows*NCols DO MatPtr^[pos] := InMat.MatPtr^[pos] ;
END;

PROCEDURE DMatrix.AddScalar(Scalar: REAL);
VAR
   pos: INTEGER;
BEGIN
   FOR pos:= 1 TO NRows*NCols DO MatPtr^[pos] := MatPtr^[pos] + Scalar ;
END;

PROCEDURE DMatrix.MultScalar(Scalar: REAL);
VAR
   pos: INTEGER;
BEGIN
   FOR pos:= 1 TO NRows*NCols DO MatPtr^[pos] := MatPtr^[pos] * Scalar ;
END;

PROCEDURE DMatrix.MultVector(InVect : DVector;VAR  OutVect: DVector);
VAR
  Row,Col: INTEGER;
  Sum : REAL;
BEGIN
   IF (InVect.size<>NCols) OR (OutVect.size<>NRows) THEN
   FatalError('DMatrix.MultVector: Input or Output vector has incorrect size');
   FOR Row:= 1 TO NRows DO
      BEGIN
         Sum:=0.0;
         FOR Col:= 1 TO NCols DO
            Sum:= Sum + MatPtr^[(Row -1)*NCols + Col]*InVect.VecPtr^[Col];
         OutVect.VecPtr^[Row] := Sum
      END
END;

PROCEDURE DMatrix.MultMatrix(InMat: DMatrix; VAR OutMat: DMatrix);
VAR
   Row,Col,k: INTEGER;
   Sum: REAL;
BEGIN
   IF (NRows<>OutMat.NRows) OR (InMat.NCols<>OutMat.NCols) THEN
    FatalError('DMatrix.MultMatrix: Output Matrix has incorrect size.');
   IF (NCols<>InMat.NRows) THEN
    FatalError('MultMatrix: NRows of first matrix <> NCols of second matrix');
   FOR Row:=1 TO NRows DO
    FOR Col:= 1 TO NCols DO
     BEGIN
        Sum:=0.0;
        FOR k:= 1 TO NCols DO
           Sum:= Sum + MatPtr^[(Row -1)*NCols + k]*InMat.Value(k,Col);
        OutMat.MatPtr^[OutMat.Loc(Row,Col)]:=Sum;
     END
END;

PROCEDURE DMatrix.Transpose(VAR OutMat : DMatrix);
VAR
  Row,Col: INTEGER;
BEGIN
   IF ((NCols<>OutMat.NRows) AND (NRows<>OutMat.NCols)) THEN
     FatalError('DMatrix.Transpose: Transpose of different dimensions')
   ELSE
   FOR Row:=1 TO NRows DO  FOR Col:=1 TO NCols DO
      OutMat.MatPtr^[OutMat.Loc(Col,Row)]:=MatPtr^[(Row -1)*NCols + Col]
END;

FUNCTION DMatrix.dMdx( row,col : integer; h : real) : real;
VAR
   p : integer;
   x : real;
BEGIN
{$IFOPT D+}
   IF (Row>NRows) OR (Row<1) THEN
     FatalError('DMatrix.dMdx: Row exceeds number of rows in the matrix')
   ELSE IF (Col>NCols) OR (Col<1) THEN
     FatalError('DMatrix.dMdx: Col exceeds number of columns in the matrix');
{$ENDIF}
   p := (row-1)*NCols+col;
   x := 0;
   IF (col>2) AND (col<Ncols-1) THEN    {accuracy 5}
    x := (MatPtr^[p-2]-8*MatPtr^[p-1]+8*MatPtr^[p+1]-MatPtr^[p+2])/(12*h)
   ELSE IF (col=1) THEN
    x := (-25*MatPtr^[p]+48*MatPtr^[p+1]-36*MatPtr^[p+2]+16*MatPtr^[p+3]-3*MatPtr^[p+4])/(12*h)
   ELSE IF (col=2) THEN
    x := (-3*MatPtr^[p-1]-10*MatPtr^[p]+18*MatPtr^[p+1]-6*MatPtr^[p+2]+MatPtr^[p+3])/(12*h)
   ELSE IF (col=NCols-1) THEN
    x := (-MatPtr^[p-3]+6*MatPtr^[p-2]-18*MatPtr^[p-1]+10*MatPtr^[p]+3*MatPtr^[p+1])/(12*h)
   ELSE IF (col=NCols) THEN
    x := (3*MatPtr^[p-4]-16*MatPtr^[p-3]+36*MatPtr^[p-2]-48*MatPtr^[p-1]+25*MatPtr^[p])/(12*h);
   dMdx := x;
END;


FUNCTION DMatrix.dMdy( row,col : integer; h : real) : real;
VAR
   p,c : integer;
   y   : real;
BEGIN
{$IFOPT D+}
   IF (Row>NRows) OR (Row<1) THEN
     FatalError('DMatrix.dMdy: Row exceeds number of rows in the matrix')
   ELSE IF (Col>NCols) OR (Col<1) THEN
     FatalError('DMatrix.dMdy: Col exceeds number of columns in the matrix');
{$ENDIF}
   c := NCols;
   p := (row-1)*NCols+col;
   y := 0;
   IF (row>2) AND (row<NRows-1) THEN    {accuracy 5}
    y := (MatPtr^[p-2*c]-8*MatPtr^[p-c]+8*MatPtr^[p+c]-MatPtr^[p+2*c])/(12*h)
   ELSE IF (row=1) THEN
    y := (-25*MatPtr^[p]+48*MatPtr^[p+c]-36*MatPtr^[p+2*c]+16*MatPtr^[p+3*c]-3*MatPtr^[p+4*c])/(12*h)
   ELSE IF (row=2) THEN
    y := (-3*MatPtr^[p-c]-10*MatPtr^[p]+18*MatPtr^[p+c]-6*MatPtr^[p+2*c]+MatPtr^[p+3*c])/(12*h)
   ELSE IF (row=NRows-1) THEN
    y := (-MatPtr^[p-3*c]+6*MatPtr^[p-2*c]-18*MatPtr^[p-c]+10*MatPtr^[p]+3*MatPtr^[p+c])/(12*h)
   ELSE IF (row=NRows) THEN
    y := (3*MatPtr^[p-4*c]-16*MatPtr^[p-3*c]+36*MatPtr^[p-2*c]-48*MatPtr^[p-c]+25*MatPtr^[p])/(12*h);
   dMdy := y;
END;

FUNCTION DMatrix.d2Mdx2( row,col : integer; h : real) : real;
VAR
   p : integer;
   x : real;
BEGIN
{$IFOPT D+}
   IF (Row>NRows) OR (Row<1) THEN
     FatalError('DMatrix.dMdx2: Row exceeds number of rows in the matrix')
   ELSE IF (Col>NCols) OR (Col<1) THEN
     FatalError('DMatrix.d2Mxd2: Col exceeds number of columns in the matrix');
{$ENDIF}
   p := (row-1)*NCols+col;
   x := 0;
   IF (col>2) AND (col<Ncols-1) THEN    {accuracy 5}
    x := (-MatPtr^[p-2]+16*MatPtr^[p-1]-30*MatPtr^[p]+16*MatPtr^[p+1]-MatPtr^[p+2])/(12*h*h)
   ELSE IF (col=1) THEN
    x := (35*MatPtr^[p]-104*MatPtr^[p+1]+114*MatPtr^[p+2]-56*MatPtr^[p+3]+11*MatPtr^[p+4])/(12*h*h)
   ELSE IF (col=2) THEN
    x := (11*MatPtr^[p-1]-20*MatPtr^[p]+6*MatPtr^[p+1]+4*MatPtr^[p+2]-MatPtr^[p+3])/(12*h*h)
   ELSE IF (col=NCols-1) THEN
    x := (-MatPtr^[p-3]+4*MatPtr^[p-2]+6*MatPtr^[p-1]-20*MatPtr^[p]+11*MatPtr^[p+1])/(12*h*h)
   ELSE IF (col=NCols) THEN
    x := (11*MatPtr^[p-4]-56*MatPtr^[p-3]+114*MatPtr^[p-2]-104*MatPtr^[p-1]+35*MatPtr^[p])/(12*h*h);
   d2Mdx2 := x;
END;


FUNCTION DMatrix.d2Mdy2( row,col : integer; h : real) : real;
VAR
   p,c : integer;
   y   : real;
BEGIN
{$IFOPT D+}
   IF (Row>NRows) OR (Row<1) THEN
     FatalError('DMatrix.d2Mdy2: Row exceeds number of rows in the matrix')
   ELSE IF (Col>NCols) OR (Col<1) THEN
     FatalError('DMatrix.d2Mdy2: Col exceeds number of columns in the matrix');
{$ENDIF}
   c := NCols;
   p := (row-1)*NCols+col;
   y := 0;
   IF (row>2) AND (row<NRows-1) THEN    {accuracy 5}
    y := (-MatPtr^[p-2*c]+16*MatPtr^[p-c]-30*MatPtr^[p]+16*MatPtr^[p+c]-MatPtr^[p+2*c])/(12*h*h)
   ELSE IF (row=1) THEN
    y := (35*MatPtr^[p]-104*MatPtr^[p+c]+114*MatPtr^[p+2*c]-56*MatPtr^[p+3*c]+11*MatPtr^[p+4*c])/(12*h*h)
   ELSE IF (row=2) THEN
    y := (11*MatPtr^[p-c]-20*MatPtr^[p]+6*MatPtr^[p+c]+4*MatPtr^[p+2*c]-MatPtr^[p+3*c])/(12*h*h)
   ELSE IF (row=NRows-1) THEN
    y := (-MatPtr^[p-3*c]+4*MatPtr^[p-2*c]+6*MatPtr^[p-c]-20*MatPtr^[p]+11*MatPtr^[p+c])/(12*h*h)
   ELSE IF (row=NRows) THEN
    y := (11*MatPtr^[p-4*c]-56*MatPtr^[p-3*c]+114*MatPtr^[p-2*c]-104*MatPtr^[p-c]+35*MatPtr^[p])/(12*h*h);
   d2Mdy2 := y;
END;

FUNCTION DMatrix.Interpolate(row,col : real) : real;

   FUNCTION Int6Pt( x,y, v1,v2,v3,v4,v5,v6 : real) : real;
   VAR i : real;
   BEGIN
      i := ( y*(y-1)*v1 +y*(y-2*x+1)*v5  +2*(1+x*y-x*x-y*y)*v3
            +x*(x-1)*v2 +x*(x-2*y+1)*v4  +2*x*y*v6 )/2;
      Int6Pt := i;
   END;

VAR
   dx,dy   : real;
   p,c,x,y : integer;
BEGIN
   dx := frac(col);   dy := frac(row);
   x  := trunc(col);  y  := trunc(row);
   c := NCols;
   p := (y-1)*NCols+x;
   IF (p<1) OR (p>NCols*NRows) THEN Interpolate := 0 ELSE
   IF ((dx+dy<1) AND (x>1) AND (y>1)) OR (x=NCols) OR (y=NRows) THEN
    Interpolate := Int6Pt( dx,dy, MatPtr^[p-c], MatPtr^[p-1], MatPtr^[p],
                   MatPtr^[p+1], MatPtr^[p+c], MatPtr^[p+c+1])
   ELSE
    Interpolate := Int6Pt( 1-dx,1-dy, MatPtr^[p+2*c+1], MatPtr^[p+2+c],
                   MatPtr^[p+c+1], MatPtr^[p+c], MatPtr^[p+1], MatPtr^[p]);
END;

(*************** END DYNAMIC DMatrix PROCEDURES *********************)

(******************************* IDVector ********************************)

PROCEDURE IDVector.Init(NumberElements: INTEGER);

VAR
    i: INTEGER;
    MemReq,MaxBlock: String[10];
BEGIN
   Size := NumberElements;
   MemBlock:= longInt(Size)*SizeOf(INTEGER);
   IF ((MemBlock > MaxAvail) OR (MemBlock >= 65521)) THEN
     BEGIN
     Str(MemBlock:0,MemReq);
     Str(MaxAvail:0,MaxBlock);
     Error('IDVector.Init: Request for '+ MemReq +
               ' exceeds free memory ' + MaxBlock + 'or 65521');
     MemBlock := 0;
     IVecPtr := NIL;
     Size:= 0;
     END
   ELSE
     IF size=0 THEN IVecPtr:=nil ELSE
     BEGIN
        GetMem(IVecPtr,MemBlock);
        FOR i:=1 TO Size DO IVecPtr^[i]:=0
     END
 END;

PROCEDURE IDVector.Put(Index : INTEGER; X : INTEGER);
BEGIN
{$IFOPT D+}
   IF (Index>Size) OR (Index<1) THEN
     FatalError('DVector.Put: Index exceeds length of the vector');
{$ENDIF}
   IVecPtr^[Index] := X;
END;

PROCEDURE IDVector.Get(Index : INTEGER; VAR X: INTEGER);
BEGIN
{$IFOPT D+}
   IF (Index>Size) OR (Index<1) THEN
     FatalError('IDVector.Get: Index exceeds length of the vector');
{$ENDIF}
   X := IVecPtr^[Index]
END;

FUNCTION IDVector.Value(Index: INTEGER) : INTEGER;
BEGIN
{$IFOPT D+}
   IF (Index>Size) OR (Index<1) THEN
     FatalError('IDVector.Value: Index exceeds length of the vector');
{$ENDIF}
   Value := IVecPtr^[Index]
END;

PROCEDURE IDVector.Free;
BEGIN
   FreeMem(IVecPtr,MemBlock);
   MemBlock := 0;
   IVecPtr := NIL;
   Size:= 0;
END;

PROCEDURE IDVector.ReSize(NumberElements: INTEGER);
VAR
   temp : IDVector;
   i,num : integer;
BEGIN
   temp.init(size);
   temp.equate(1,self);
   Free;
   Init(NumberElements);
   IF size<temp.size THEN num:=size ELSE num:=temp.size;
   FOR i:=1 TO num DO IVecPtr^[i]:= temp.IVecPtr^[i];
   temp.free;
END;


FUNCTION IDVector.Length: INTEGER;
BEGIN
    Length:= Size;
END;

PROCEDURE IDVector.Equate(s: integer; VectB: IDVector {input});
VAR
  i: INTEGER;
BEGIN
   IF size<>VectB.size THEN
    FatalError('IDVector.Equate: equating two vectors of different size');
   FOR i:=1 TO Size DO
       IVecPtr^[i]:= s*VectB.IVecPtr^[i]
END;


(**************** BEGIN NUMERICAL PROCEDURES ************************)

PROCEDURE FindZero(Fct: FUNCTIONX; a, b, Tol: REAL;
               VAR zero: REAL; VAR iter: integer);

(* This is an adaptation by James A. Hummel of the Algol
  procedure of Richard Brent ("Algorithms FOR Minimization without
  Derivatives", Prentice Hall, 1973).  Revised by William M. MacDonald to
  find the zero of the FUNCTION Fct (which must  be a FUNCTION declared in
  the main program with the FAR directive) between a AND b.

                                    --J. A. Hummel    9/15/88
                                    --W. M. MacDonald 2/18/91

  b is the most recently found approximation to the zero.
  a is the next most recent.
  c is the most recent FOR which Fct(b) AND Fct(c) have opposite signs.
  d = b - a;  xm = 0.5*(c - b); AND  e= previous d OR  xm               *)

 CONST

    MaxIter = 100;

 VAR
      c,d,e,eps,FA,FB,FC,Tol1,xm,p,q: REAL;

 PROCEDURE CtoA; {Moves c to a}
 BEGIN
    c  := a; FC := FA;
    d  := b - a;
    e  := d;
 END;

 PROCEDURE BCset;
 {Resets variables if c is a better approximation than b}
 BEGIN
    IF (ABS(FC) < ABS(FB)) THEN
       BEGIN
          a := b;   b := c;   c := a;
          FA := FB; FB := FC; FC := FA
       END;
    Tol1 := 2.0*eps*ABS(b) + 0.5*Tol;
    xm := 0.5*(c-b);
 END;

 PROCEDURE Bisect(VAR d,e: REAL);
 BEGIN
   d := xm;
   e := d;
 END;

 PROCEDURE LinearInterp(VAR p,q: REAL);
 VAR
    s: REAL;
 BEGIN
    s := FB/FA;         {Linear Interpolation}
    p := 2.0*xm*s;
    q := 1.0 - s;
 END;

 PROCEDURE InvQuadInterp(VAR p,q: REAL);
 VAR
    r,s: REAL;
 BEGIN
    q := FA/FC;         {Inverse quadratic interpolation}
    r := FB/FC;
    s := FB/FA;
    p := s*(2.0*xm*q*(q - r) - (b - a)*(r - 1.0));
    q := (q - 1.0)*(R - 1.0)*(s - 1.0);
 END;

BEGIN    {Main Program}

   eps:=macheps;
   iter:=0;
   {Initialize}
   FA := Fct(a); FB := Fct(b); FC := FB;
   CtoA;
   BCset;
   {-------Check for zero or Halt the entire program------------}
   IF (FA*FB)>0 THEN
      FatalError('FindZero: Function has same sign at '
             +numStr(a,5,2)+' AND b '+NumStr(b,5,2))
   ELSE
      IF Abs(FA)<macheps THEN
        zero := a
   ELSE
      IF Abs(FB)<macheps THEN
        zero := b

   {-------BEGIN search for the zero----------------------------}
   ELSE
      WHILE ((ABS(xm) > Tol1) AND (FB<>0) AND (iter<100)) DO
         BEGIN
            iter:=iter+1;
            IF (ABS(e) < Tol1) OR (ABS(FA) <= ABS(FB)) THEN
               Bisect(d,e)
            ELSE
               BEGIN
                  IF  a = c  THEN
                     LinearInterp(p,q)
                  ELSE
                     InvQuadInterp(p,q);
                   IF (p > 0.0) THEN             {Adjust signs}
                     q := -q
                  ELSE
                     p := -p;   
                   IF ((2.0*p) >= (3.0*xm*q - ABS(Tol1*q)))
                                   OR (p >= ABS(0.5*e*q))  THEN
                     Bisect(d,e)
                  ELSE
                     BEGIN
                        e := d;
                        d := p/q;
                     END
               END;
            a := b;
            FA := FB;
            IF (ABS(d) > Tol1) THEN
               b := b + d
            ELSE
               IF (xm>0) THEN
                  b := b + Tol1
               ELSE
                  b := b - Tol1;
            FB := Fct(b);
            IF (FB*(FC/ABS(FC)) > 0.0 ) THEN
               CtoA;
            BCset;
            Zero:= b;
         END;
END;  {FindZero}


PROCEDURE Roots(Fct:FUNCTIONX;a,b, Tol: REAL; VAR N, J: integer;
                                              VAR M: DVector);
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
         x := a + i*dx;
         f1:= Fct(x); f2 := Fct(x+dx);
         IF (f1*f2 < 0.0) THEN
         BEGIN
            iter :=0;
            x1 := x;  x2 := x + dx;
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
               FatalError('FindZero: no convergence to root between '
                        +numStr(a,5,2)+' AND  '+NumStr(b,5,2))
            ELSE
               BEGIN
                  J := J + 1;
                  if J<=M.size then M.Put(J,xo)
               END
         END
      END
END;

{---------------------NUMEROV Procedures-----------------------------}

PROCEDURE Numerov(ibeg, iend: Integer; h: REAL; VAR A,y: DVector);
(* Numerov solves the homogeneous second order ODE OF the form
                y'' = F(x) y  .
   The dynamic DVector A holds the values OF F(x)*h^2/12.0 from the
   starting point TO the last.  The spacing between points is uniform
   AND equal TO h.  The solution DVector y must initially contain the
   value OF the wave FUNCTION at the first AND second points. The
   solution at the mesh points is returned IN  y.

   FOR the radial Schrodinger equation the values FOR A[1] AND y[1] must be
   chosen so that the starting value FOR Z = (1 - F(r)*sqr(h)/12.0)*u(r)
   is correct, where x -> r AND y -> u = R(r) r IN the conventional
   notation FOR the radial Schrodinger equation.  This requires
   evaluating Z IN the limit as r -> 0.  FOR non-singular potentials the
   result leads TO the choice A[1] = 0 FOR all angular momenta, WHILE
   u[1] = 0.0 FOR angular momentum 0,2,...  but u[1] = - C h^2/6.0 where
   u(r) -> C r^(l+1) as r -> 0.   The value OF C merely sets the
   scale OF the wave FUNCTION AND is therefore arbitrary but NONZERO.

   This is the summed Numerov method which is discussed by J.L. Friar,
   Journal OF Computational Physics 28, 426 (1978).
                                  --William M. MacDonald 2/18/91  *)
  (****************************************************************
   This Numerov is a modified version that allows FOR
   forwards OR backwards integration between arbitrary mesh
   points.   It is assumed that the calling PROGRAM ensures
   that the inequalities  1 <= ibeg, iend <= Npoints  are
   satisfied.  Npoints is the max length OF the DVectors.
                                  -- R.J. Philpott 8/19/91       *)
  (****************************************************************
      NPts  = number OF meshpoints = number OF steps + 1
      A_i   = F(xi)*h^2/12.0
      y_i   = solution OF differential equation
  *****************************************************************)
 VAR
   i1, i2, di, ii, i : Integer;
   B, Z, T, F, s     : REAL;
BEGIN
   IF (a.size<> y.size) THEN
     FatalError('Numerov: Vectors A and y must be the same length');
   IF (ibeg < 1) OR (iend < 1) OR (ibeg > y.size) OR (iend > y.size)
      OR (ibeg = iend) THEN
     FatalError('Numerov: Subinterval markers are out of range');
   IF ibeg < iend THEN
     BEGIN
       i1 := ibeg;  i2 := iend;  di := 1;
     END
    ELSE
      BEGIN
        i1 := iend;  i2 := ibeg;  di := -1;
      END;
    s  := 0.0;
    B := 1 - A.VecPtr^[ibeg];
    i := ibeg;
    Z := B*y.VecPtr^[ibeg];
    B := 1 - A.VecPtr^[ibeg + di];
    F := B*y.VecPtr^[ibeg + di] - Z;
    FOR ii := i1 + 1 TO i2 DO
       BEGIN
          i := i + di;
          Z := Z + F;
          B := 1 - A.VecPtr^[i];
          T := 12.0*(1 - B)/B;
          F := F + T*Z;
          y.VecPtr^[i] := Z/B;
       END
END;

FUNCTION NumerovDeriv(ideriv: Integer; h: REAL; VAR A, y: DVector): REAL;
{ Numerical derivative following a Numerov integration.
  ideriv must lie within the range specified by ibeg AND
  iend, (see the Numerov PROCEDURE) AND NOT CLOSER THAN
  TWO POINTS away from either END OF the range.
                                    -- R.J. Philpott 9/25/91  }
CONST
   Coef1 : ARRAY[1..5] OF REAL = (   44, -448, 0,  448, -44 );
   Coef2 : ARRAY[1..5] OF REAL = ( -128,  448, 0, -448, 128 );
VAR
   sum, term : REAL;
   i, j      : Integer;
BEGIN
      sum := 0;
      i := ideriv - 3;
      FOR j := 1 TO 5 DO
         BEGIN { Finite difference evaluation }
            i := i + 1;
            term  := (Coef1[j] + Coef2[j]*A.VecPtr^[i])*y.VecPtr^[i];
            sum   := sum + term;
         END;  { Finite difference evaluation }
      NumerovDeriv := sum/(720*h);
END;


PROCEDURE StepRK4(FYP: DFproc; VAR t,h: REAL; VAR Y: DVector);
(* This PROCEDURE solves a system OF ANY NUMBER OF first order
differential equations  FOR one time step using the Runge-Kutta method
FOR fourth order. The main PROGRAM must declare a DVector that contains
the values OF the dependent variables at time t which is passed as the
variable Y. The parameter FYP passes TO RK4 the NAME OF a PROCEDURE IN
the main PROGRAM that calculates the first derivative OF the vector Y OF
the dependent variables AND stores the result IN a DVector YP, i.e.
dY/dt = YP.  (The actual names chosen FOR these DVector's is NOT
important.) This PROCEDURE must be declared OF TYPE FAR as IN the
following statement.

   PROCEDURE ANYNAME(t: REAL; VAR Y,YP:DVector): FAR ;

 Input variables:
   t    - the inital value OF the independent variable
   h   -  the desired increment, that is, the solution is TO be calculated
          at  t + h. This variable may be negative
   Y    - the inital values OF the dependent variables

 Output variables:
   t    - the value OF t after the time step
   Y    - the vector OF computed solutions at the new t              *)

VAR
   neqns,i,j: INTEGER;
   tt,hb2 : REAL;
   Yt, k1,k2,k3,k4: DVector;
   w1,w2: REAL;
BEGIN
   w1 := 1.0/6*h; w2 := 2*w1;
   hb2:= h/2.0;
   neqns := Y.Length;                      {Determine number of equations}
   Yt.Init(neqns);                         {Initialize storage of Y}
   k1.Init(neqns); k2.Init(neqns); k3.Init(neqns); k4.Init(neqns);
   FYP(t,Y,k1);
   t:=t+0.5*h;
   Yt.SumOfVectors(1.0,Y,hb2,k1); FYP(t,Yt,k2);
   Yt.SumOfVectors(1.0,Y,hb2,k2); FYP(t,Yt,k3); 
   t:=t+0.5*h;
   Yt.SumOfVectors(1.0,Y,h,k3); FYP(t,Yt,k4);
 { Add Y to a weighted average of increments in Y
     given by k1..k4 TO get Y at t + h }
   FOR i:=1 TO neqns DO
     Y.VecPtr^[i] := Y.VecPtr^[i] + w1*k1.VecPtr^[i] + w2*k2.VecPtr^[i] +
                                    w2*k3.VecPtr^[i] + w1*k4.VecPtr^[i];
   Yt.Free;
   k1.Free; k2.Free; k3.Free; k4.Free;
 END;

PROCEDURE StepRKF(FYP: DFproc; VAR t: REAL; VAR Y:DVector; dt: REAL;
              VAR h: REAL;
              hmax,Aerr,Rerr: REAL;
              VAR Iflag: integer);

(* This PROCEDURE solves a system OF ANY NUMBER OF first order
differential equations FOR one time step using the Runge-Kutta-Fehlberg
method. The RKF algorithm requires six FUNCTION evaluations
TO give TO give a fifth-order result.  Four OF the evaluations are used
TO give a fourth-order result which is compared WITH the fifth order
result TO control the step size. The main PROGRAM must declare a DVector
that contains the values OF the dependent variables at time t which is
passed as the variable Y. The parameter FYP passes TO RKF the NAME OF a
PROCEDURE IN the main PROGRAM that calculates the first derivative OF
the vector Y OF the dependent variables AND stores the result IN a
DVector YP, i.e. dY/dt = YP.  (The actual names chosen FOR these
DVector's is NOT important.) This PROCEDURE must be declared OF TYPE FAR
as IN the following statement.

   PROCEDURE ANYNAME(t: REAL; VAR Y,YP:DVector): FAR ;

 Input variables:
   t    - The inital value OF the independent variable.
   dt   - The desired increment. That is, the solution is TO be calculated
          at  t + dt. This variable may be negative
   h    - The initial increment (step size) OF t TO be tried. May be negative.
   hmax - The maximum allowable step size. Must be positive.
   Y    - The inital values OF the dependent variables.
   Aerr - Desired local ABSOLUTE AND relative
   Rerr -   errors relative TO a UNIT change IN t. The code attempts
            TO make each component OF  Y  IN a step OF length  h  pass the test:
              abs(estimated local error) <= abs(h)*(Rerr*abs(Y(I) + Aerr)

 Output variables:
   t    - The last value at which the solution was successfully computed.
   h    - Optimal step size currently being used by code.
   Y    - The vector OF computed solutions at the new t.
   Iflag = 1  FOR normal return. Reached  t + dt.
         = 2  more than 2000 FUNCTION evaluations were required.
         = 3  illegal input values.
         = 4  required step size is too small FOR accuracy OF REAL numbers
              IN the computer.
                                                                          *)

VAR
  U,Ratio,Tout,S,SR,Ttemp,hkeep: REAL;
  I,J,L,Kount: integer;
  ReachedGoal:boolean;
  G:array[1..7,1..6] OF REAL;
  K:array[1..6] OF DVector;
  GH:array[1..5] OF REAL;
  Ytemp,R: DVector;
  Neqn: INTEGER;

BEGIN
  Neqn := Y.Length;
  Ytemp.Init(Neqn);
  R.Init(Neqn);
  FOR i:=1 TO 6 DO
     K[i].Init(Neqn);
  U := Macheps;
  {----Initialize parameters---------}
  Iflag := 0;
  Ratio := 0.0;
  Kount := 0;
  ReachedGoal := False;
  {----Check input parameters--------}
  IF (Rerr < 0.0) OR (Aerr < 0.0) OR
      (Rerr + Aerr = 0.0) OR (hmax <= 0.0) THEN
    Iflag := 3;
  {-------Initialize variables-------}
  Tout := t + dt;  S := abs(Tout);
  IF abs(t) > S THEN S := abs(t);
  IF abs(dt) <= 13.0*U*S THEN Iflag := 4;        {Step size too small}
  IF abs(dt) < hmax THEN hmax := abs(dt);
  IF abs(h) <= 13.0*U*abs(t) THEN  h := hmax;
  {-----Load Fehlberg constants------}
  GH[1] := 0.25; GH[2] := 0.375; GH[3] := 12.0/13.0;
  GH[4] := 1.0; GH[5] := 0.5;
  G[1,1] := 0.25;
  G[2,1] := 0.09375; G[2,2] := 0.28125; 
  G[3,1] := 1932.0/2197.0; G[3,2] := -7200.0/2197.0;
  G[3,3] := 7296.0/2197.0; 
  G[4,1] := 439.0/216.0; G[4,2] := -8.0; G[4,3] := 3680.0/513.0;
  G[4,4] := -845.0/4104.0; 
  G[5,1] := -8.0/27.0; G[5,2] := 2.0; G[5,3] := -3544.0/2565.0;
  G[5,4] := 1859.0/4104.0; G[5,5] := -11.0/40.0; 
  G[6,1] := 25.0/216.0; G[6,2] := 0.0; G[6,3] := 1408.0/2565.0;
  G[6,4] := 2197.0/4104.0; G[6,5] := -0.2;
  G[7,1] := 1.0/360.0; G[7,2] := 0.0; G[7,3] := -128.0/4275.0;
  G[7,4] := -2197.0/75240.0; G[7,5] := 0.02; G[7,6] := 2.0/55.0;
  {-----Do calculation if all parameters are OK-------}
  WHILE  Iflag = 0 DO
    BEGIN
      ReachedGoal := False;
      IF Ratio <= 1.0 THEN
        BEGIN
          h := abs(h);
          IF hmax < abs(h) THEN h := hmax;
          IF dt <0 THEN h:= -h;
          IF abs(Tout - T) <= 1.25*abs(h) THEN
            BEGIN
              ReachedGoal := True;
              hkeep := h;
              h := Tout - t
            END;
          FYP(t,Y,K[1]);
          Kount := Kount + 1;
        END;

      FOR  L := 1 TO 5 DO                 {Make one RKF step}
        BEGIN
          Ytemp.Equate(1.0,Y);
          FOR J :=1 TO L DO
             Ytemp.AddVector(h*G[L,J],K[J]);
          Ttemp := T + GH[L]*dt;
          FYP(Ttemp,Ytemp,K[L+1]);
        END;

      Ytemp.Equate(1.0,Y);
      FOR J := 1 TO 5 DO
         Ytemp.AddVector(h*G[6,J],K[J]);  {Ytemp is tentative result}
      R.Equate(G[7,1],K[1]);
      FOR J := 2 TO 6 DO
         R.AddVector(G[7,J],K[J]);   {R is the estimated local error}

      Ratio := 0.0;                      {Test for accuracy}
      FOR I := 1 TO Neqn DO
        BEGIN
          S := abs(R.VecPtr^[I])/(Rerr*abs(Ytemp.VecPtr^[I]) + Aerr);
          IF S > Ratio THEN
            Ratio := S
        END;

      IF Ratio <= 1.0 THEN
         BEGIN
            Y.Equate(1,Ytemp);      {Accept result of step}
            t := t + h;
         END;

      IF (Ratio <= 1.0) AND ReachedGoal THEN      {Finished}
        BEGIN
          Iflag := 1;
          h := hkeep
        END
      ELSE
        BEGIN                            {Adjust Ratio to limit change.}
          IF Ratio < 6.5536E-4 THEN
            Ratio := 6.5536E-4
          ELSE IF Ratio > 4096.0 THEN
            Ratio := 4096.0;
          h := 0.8*h/sqrt(sqrt(Ratio));
          Kount := Kount + 5;
          IF abs(h) <= 13.0*U*abs(t) THEN
            Iflag := 4           {Required step too small for machine.}
          ELSE IF Kount >= 1995 THEN
            Iflag := 2;          {More than 2000 FUNCTION evaluations.}
        END;
    END;    {WHILE}
   {Free the working DVector's}
   Ytemp.Free;  R.Free;
   FOR i:=1 TO 6 DO
      K[i].Free;
  END;      {StepRKF}

FUNCTION Fmin(Fct:FUNCTIONX; a,b,Tol: REAL): REAL;

(*  This routine searches FOR an approximation TO the point x on the
  interval [a,b] at which a REAL FUNCTION  Fct  attains its minimum.
  This routine is an adaptation OF the PROGRAM by Richard Brent
  (Algorithms FOR Minimization without Derivatives,
  Prentice - Hall 1973).

  TO use, include the statment
            USES MATHPROC;
  at the BEGINning OF the PROGRAM AND define

            FUNCTION Fct(x: REAL): REAL; FAR;
            BEGIN
               FUNCTION body
            END;

  IN the main PROGRAM.
                                              - J. A. Hummel
                                                April 1988          *)

VAR   C,d,e,Prev,r,p,q,u,v,w,x,Fu,Fv,Fw,Fx : REAL;
      Eps,Tol1,Tol2,xm : REAL;
      Acceptable : boolean;

PROCEDURE AdjustTol;
BEGIN
   xm := 0.5*(a + b);
   Tol1 := Eps*Abs(x) + Tol/3.0;
   Tol2 := 2.0*Tol1;
END;

PROCEDURE Initialize;
BEGIN
  C := 0.5*(3.0 - Sqrt(5.0));       {Squared inverse of Golden Ratio.}
  Eps := Sqrt(Macheps);                 {Square root of Machine epsilon.}
  IF a > b THEN
     BEGIN                         {Fix incorrect input order}
        x := a; a := b; b := x;
     END;
  IF Tol < Eps THEN
     Tol := Eps;                    {In case min is at x = 0}
  v := a + C*(b - a);               {Initialization of points}
  w := v; x := v;
  e := 0.0;
  Fx := Fct(x);
  Fv := Fx;  Fw := Fx;
  AdjustTol;
END;

PROCEDURE FitParabola;
BEGIN
   r := (x - w)*(Fx - Fv);           {Fit parabola.}
   q := (x - v)*(Fx - Fw);
   p := (x - v)*q - (x - w)*r;
   q := 2.00*(q - r);              
   IF (q > 0.0) THEN
      p := -p
   ELSE
      q :=  Abs(q);               {Min is at x + p/q}
   Prev := e;
   e := d;

  IF (Abs(p) >= Abs(0.5*q*Prev))  {Want Step < one half previous step}
     OR (p <= q*(a - x))          {Want new point inside interval}
     OR (p >= q*(b - x)) THEN
        Acceptable := False
  ELSE
    Acceptable := True;
END;

PROCEDURE ParabolicInterpolation;
BEGIN
   d := p/q;                 {Parabolic Interpolation step}
   u := x + d;
   IF ((u - a) < Tol2) THEN  {F must NOT be evaluated too close to a or b}
      d := Sign(Tol1, xm - x)
   ELSE IF ((b - u) < Tol2) THEN
      d := Sign(Tol1, xm - x);
END;

PROCEDURE GoldenSection;
BEGIN
   IF (x >= xm) THEN
      e := a - x
   ELSE
      e := b - x;
   d := C*e;                      {Golden section step}
END;

PROCEDURE Update;
{Have interval [a,b], points v, w, x, and new point u <> x. Want to
 update so [a,b] is smaller, Fx is smallest OF values Fx, Fu, Fv, Fw,
 AND (IF possible) Fv > Fw > Fx, but IN any CASE always END WITH
 Fv >= Fx AND Fw >= Fx.}
BEGIN
  IF Fu <= Fx THEN
     BEGIN
        IF  u >= x  THEN
           a := x
        ELSE  b := x;
        v := w; Fv := Fw;          {Discard previous v}
        w := x; Fw := Fx;
        x := u; Fx := Fu;          {Fx is the smallest value found yet}
     END
  ELSE
     BEGIN
        IF u < x THEN
           a := u
        ELSE  b := u;
        IF (Fu <= Fw) OR (w = x) THEN
           BEGIN
              v := w; Fv := Fw;        {Discard previous v}
              w := u; Fw := Fu;        {Previous Fx is smallest found yet}
           END
        ELSE
           IF (Fu <= Fv) OR (v = x) OR (v = w) THEN
              BEGIN
                 v := u;               {Discard previous v}
                 Fv := Fu;             
               END;
       {There remains the case in which Fu > Fw, Fu > Fv,  w <> x, v <> x,
        AND w <> v.  IN this CASE continue WITH same x, v, w but smaller
        interval [a,b].  At least one step OF Golden section will follow.}
     END;
END;

BEGIN                               {Start of Main program}
  Initialize;
  WHILE (Abs(x - xm) > (Tol2 - 0.5*(b - a))) DO
    BEGIN                           {Main Loop}
       IF (Abs(e) > Tol1) THEN
          BEGIN
             FitParabola;
             IF Acceptable THEN
                ParabolicInterpolation
             ELSE GoldenSection;
          END
       ELSE
         GoldenSection;

                {F must NOT be evaluated too close to x.}
       IF (Abs(d) >= Tol1) THEN
          u := x + d
       ELSE  u := x + Sign(Tol1, d);
       Fu := Fct(u);

       Update;
       AdjustTol;
    END;     {of main loop}

  Fmin := x;

END;    {of Fmin} 
(*********** UNIFORM RANDOM DEVIATE GENERATOR ***************************)
CONST
   RanOY: REAL = 2;
VAR
   RanOV: ARRAY[1..97] OF REAL;

FUNCTION Urand(VAR idum: integer):REAL;
VAR
   j: integer;
   dum : real;
BEGIN
   IF (idum < 0) OR (RanOY<0) OR (RanOY>=1) THEN
      BEGIN
         RandSeed := Abs(idum);
         idum :=1;
         FOR j :=1 TO 97 DO
            dum := Random;
         FOR j := 1 TO 97 DO
            RanOV[j] := Random;
         RanOY := Random;
      END;
   j := 1 + Trunc(97.0*RanOY);
   RanOY := RanOV[j];
   Urand := RanOY;
   RanOV[j] := Random
END;

PROCEDURE ChebCoef(Fct: FUNCTIONX; a,b: REAL; VAR coeff: DVector;
                  m: INTEGER);
{ The RANGE [a,b] is stored in the LAST TWO positions and the         }
{ Chebyshev coefficients for the function Fct in the first m-2 places.}

VAR
 n, i,j: INTEGER;
 norm, halfwidth, midpoint, sum: REAL;
 fv: DVector;

BEGIN
  IF coeff.size < m THEN FatalError(
    'ChebCoef: coefficient vector size must be at least '+num2Str(m,4));
  IF a >= b THEN
    FatalError('ChebCoef: must have a <= b');
  coeff.Put(m-1,a); coeff.Put(m,b);
  n := m-2;
  fv.Init(n);
  halfwidth := 0.5*(b-a);
  midpoint  := 0.5*(a+b);
  FOR i:=1 TO n DO
     fv.Put(i, Fct(cos(pi*(i-0.5)/n)*halfwidth + midpoint) );
  norm := 2.0/n;
  FOR i:=1 TO n DO
    BEGIN
    sum:= 0.0;
    FOR j:=1 TO n DO
      sum := sum + fv.Value(j)*cos( pi*(i-1)*(j-0.5)/n );
    coeff.Put(i, norm*sum)
    END;
  fv.Free;
END;

FUNCTION Chebeval(coeff: DVector; x: REAL; errorflag: BOOLEAN): REAL;
VAR
  a,b, y, y2, d1,d2,d3: REAL;
  m,n,i: INTEGER;

BEGIN
  m := coeff.Length;
  a := coeff.Value(m-1);
  b := coeff.Value(m);
  n := m-2;
  IF ( (b >= a) AND ((b-x)*(x-a) >=0) ) THEN
    BEGIN
      errorflag := FALSE;
      y := (2.0*x - a - b)/(b-a);
      y2 := 2.0*y;
      d1 := 0;
      d2 := 0;
      FOR i:= n DOWNTO 2 DO
        BEGIN
        d3 := d1;
        d1 := y2*d1 - d2 + coeff.Value(i);
        d2 := d3;
        END;
      chebeval := y*d1 - d2 + 0.5*coeff.Value(1);
    END
  ELSE
    errorflag := TRUE
END;


PROCEDURE LUdecomp(VAR A:DMatrix; VAR RCond:REAL; VAR IPvt:IDVector);
VAR  Singular : boolean;
     N,i,k,m : INTEGER;
     ANorm,WNorm,ZNorm,Cond : REAL;
     W : DVector;

   FUNCTION Norm(VAR A:DMatrix):REAL;   {one norm of a DMatrix}
   VAR  i,j : INTEGER;
        Max,Sum : REAL;
   BEGIN
     Max := 0.0;
     FOR  j := 1  TO  N  DO
       BEGIN
       Sum := 0.0;
       FOR  i:= 1  TO  N  DO  Sum := Sum + abs(A.Value(i,j));
       IF  Sum > Max  THEN   Max := Sum;
       END;
     Norm := Max;
   END;

   FUNCTION Pivot(k:INTEGER):INTEGER;
   VAR  i,m : INTEGER;
        max : REAL;
   BEGIN
     m := k;  max := Abs(A.Value(k,k)/W.VecPtr^[k]);
     FOR  i := k+1  TO  N  DO
       IF  abs(A.Value(i,k)/W.VecPtr^[i]) > Max  THEN
         BEGIN
         m := i;
         max := abs(A.Value(i,k)/W.VecPtr^[i]);
         END;
     IPvt.IVecPtr^[k] := m;
     Pivot := m;
   END;

   PROCEDURE Interchange(k,m:INTEGER);
   VAR  temp1,temp2 : REAL;
        j : INTEGER;
   BEGIN
     IPvt.IVecPtr^[N] := -IPvt.IVecPtr^[N];
     FOR  j := k  TO  N  DO
       BEGIN
          temp1 := A.Value(k,j); temp2 := A.Value(m,j);
          A.Put(k,j,temp2);
          A.Put(m,j,temp1)
       END
   END;

   PROCEDURE RowReduce(i,k:INTEGER);
   VAR   mult,temp : REAL;
            j : INTEGER;
   BEGIN
     mult := -A.Value(i,k)/A.Value(k,k);
     A.Put(i,k, mult);                  {Store multiplier in A[i,k]}
     IF  mult <> 0.0  THEN
       FOR  j := k+1  TO  N  DO
         BEGIN
           temp:=A.Value(i,j) + A.Value(k,j)*mult;
           A.Put(i,j, temp)
         END
   END;

   PROCEDURE FindWeights(VAR W:DVector);
   VAR  i,j : INTEGER;
        max : REAL;
   BEGIN                       {The weight is the row max.}
     FOR i := 1 TO N DO
       IF NOT Singular THEN
         BEGIN
         Max := 0.0;
         FOR j := 1 TO N DO
           IF abs(A.Value(i,j)) > max THEN max := abs(A.Value(i,j));
         W.VecPtr^[i] := Max;
         IF W.VecPtr^[i] = 0.0 THEN
            BEGIN Singular := True; RCond := 0.0; END;
         END
   END;

   PROCEDURE Construct(VAR Y:DVector);
   VAR  i,k,seed : INTEGER;
        Sum,Temp,E : REAL;
   BEGIN
     seed:=-1;
     REPEAT Temp := Urand(seed) UNTIL ABS(Temp)>0.01;
     FOR  k :=1 TO N DO Y.VecPtr^[k] := Urand(seed)/Temp;
     FOR  k := 1  TO  N  DO
       BEGIN
       Sum := 0.0;
       IF  k > 1  THEN
          FOR  i := 1  TO  k-1  DO
           Sum := Sum + A.Value(i,k)*Y.VecPtr^[i];
       IF  Sum < 0.0  THEN  E := -1.0  ELSE  E := 1.0;
       Y.VecPtr^[k] := -(E + Sum)/A.Value(k,k);
       END;
     FOR  k := N-1  DOWNTO  1  DO
       BEGIN
       Sum := 0.0;
       FOR  i := k+1  TO  N   DO
          BEGIN
          Sum := Sum + A.Value(i,k)*Y.VecPtr^[k];
          END;
       Y.VecPtr^[k] := Sum;
       m := IPvt.IVecPtr^[k];
       IF  m <> k  THEN
         BEGIN
         Temp := Y.VecPtr^[m];  Y.VecPtr^[m] := Y.VecPtr^[k];
         Y.VecPtr^[k] := Temp;
         END
       END
   END;
BEGIN    {Start of main PROCEDURE LUdecomp}
  N := A.NRows;
  IF (A.NCols <> N) THEN
    FatalError('LUDecomp: Input matrix is not a square matrix');
  IF (Ipvt.size<N) THEN
    FatalError('LUDecomp: Ipvt vector must have a length >= matrix size');
  W.Init(N);
  ANorm := Norm(A);
  Singular := False;
  FindWeights(W);                 {IF any W is zero, DMatrix is singular.}
  IPvt.IVecPtr^[N] := 1;          {Counts parity of row interchanges.}
  IF  NOT Singular THEN
    IF N = 1 THEN RCond:= 1.0
  ELSE
    BEGIN
    FOR  k := 1  TO  N-1  DO
        BEGIN
        m := Pivot(k);
        IF  m <> k  THEN  Interchange(k,m);
        IF  A.Value(k,k) <> 0.0  THEN  FOR  i := k+1  TO N  DO
           RowReduce(i,k)
        ELSE Singular := True;
        END;
    IF A.Value(N,N)= 0.0 THEN Singular := True;
    IF NOT Singular THEN
        BEGIN
        {W becomes a working vector to find RCond}
        {W is initialized to a set of random numbers}
        Construct(W);                 {Solve Transpose(A)*W := E}
        WNorm := 0.0;
        FOR  i := 1  TO  N  DO  WNorm := WNorm + abs(W.VecPtr^[i]);
        LUsolve(A,W,IPvt);                 {Solve  A*Z := Y  }
        ZNorm := 0.0;
        FOR  i := 1  TO  N  DO  ZNorm := ZNorm + abs(W.VecPtr^[i]);
        Cond := ANorm*ZNorm/WNorm;
        IF  Cond < 1.0  THEN  Cond := 1.0;
        RCond := 1.0/Cond;
        END
    ELSE
       BEGIN
         W.Free;
         FatalError('LUdecomp: Input matrix is singular')
       END;
    END;
  W.Free;
END;

PROCEDURE LUinverse(VAR A,AI:DMatrix;VAR IPvt:IDVector);
VAR N,i,k : INTEGER;
    b : DVector;
BEGIN
  N := A.NRows;
  IF (AI.NRows <> N) OR (AI.NCols <> N) THEN
    FatalError('LUinverse: Input and output matrices must be of the same length');
  b.Init(N);
  FOR  k := 1  TO  N  DO
  BEGIN
    FOR  i := 1  TO  N  DO  b.VecPtr^[i] := 0.0;
    b.VecPtr^[k] := 1.0;                        {Unit Vector}
    LUsolve(A,b,IPvt);
    FOR  i := 1  TO  N  DO  AI.Put(i,k, b.VecPtr^[i]);
  END;
  b.Free;
END;


PROCEDURE LUsolve(VAR A: DMatrix; VAR Y: DVector; VAR IPvt: IDVector);
VAR N,i,k,m : INTEGER;
    Temp  : REAL;
BEGIN
   N := A.NRows;
   IF (Y.size < N) THEN
     FatalError('LUsolve: Vector Y must be as long as the matrix');
   IF   N > 1  THEN
     BEGIN
        FOR  k := 1  TO  N-1  DO         {Forward elimination}
          BEGIN
          m := IPvt.IVecPtr^[k];
          Temp := Y.VecPtr^[m];
          Y.VecPtr^[m] := Y.VecPtr^[k];
          Y.VecPtr^[k] := Temp;
          FOR  i := k+1 TO N  DO
             Y.VecPtr^[i] := Y.VecPtr^[i] + A.Value(i,k)*Temp;
          END;
        FOR  k := N  DOWNTO 2 DO          {Back substitution}
          BEGIN
          Y.VecPtr^[k] := Y.VecPtr^[k]/A.Value(k,k);
          Temp := - Y.VecPtr^[k];
          FOR  i := 1  TO  k-1 DO
             Y.VecPtr^[i] := Y.VecPtr^[i] + A.Value(i,k)*Temp;
          END
     END;
   Y.VecPtr^[1] := Y.VecPtr^[1]/A.Value(1,1);
END;


PROCEDURE SYM2TRID(N: INTEGER; VAR a: DMatrix; VAR d,e :DVector;
                   ReturnTransform: BOOLEAN);
{a is a real symmetric matrix of order N. On output a contains the
orthogonal matrix Q which effects the transformation IF eigenvectors
are wanted, otherwise NOT. Always d contains the diagonal elements, AND
e contains the subdiagonal elements WITH e[n]=0}
VAR
  i,l,k,j          : INTEGER;
  scale,h,hh,f,g   : REAL;

BEGIN
  IF (a.NCols<>a.NRows) OR (a.NCols<>d.size) OR (a.NCols<>e.size) THEN
   FatalError('SYM2TRID: Mismatch of matrix and/or vector dimensions');
  IF (N>d.size) THEN
   FatalError('SYM2TRID: Specified dimension > Matrix dimensions.');
  FOR i:=N DOWNTO 2 DO   (*Loop 1*)
    BEGIN
    l:=i-1;
    h:=0.0;
    scale:=0.0;
    IF l>1 THEN          (*Loop 2*)
      BEGIN
      FOR k:=1 TO l DO
        scale:=scale+abs(a.Value(i,k));
      IF scale=0.0 THEN
        e.Put(i,a.Value(i,l))
      ELSE
        BEGIN
        FOR k:=1 TO l DO
          BEGIN
          a.Put(i,k,a.Value(i,k)/scale);
          h:=h+sqr(a.Value(i,k))
          END;
        f:=a.Value(i,l);
        g:=-Sign(sqrt(h),f);
        e.Put(i,scale*g);
        h:=h-f*g;
        a.Put(i,l,f-g);
        f:=0.0;
        FOR j:=1 TO l DO
          BEGIN
          IF ReturnTransform THEN a.Put(j,i,a.Value(i,j)/h);
          g:=0.0;
          FOR k:=1 TO j DO
            g:=g+a.Value(j,k)*a.Value(i,k);
          FOR k:=j+1 TO l DO
            g:=g+a.Value(k,j)*a.Value(i,k);
          e.Put(j,g/h);
          f:=f+e.Value(j)*a.Value(i,j)
          END;
        hh:=f/(h+h);
        FOR j:=1 TO l DO
          BEGIN
          f:=a.Value(i,j);
          g:=e.Value(j)-hh*f;
          e.Put(j,g);
          FOR k:=1 TO j DO
            a.Put(j,k,a.Value(j,k)-f*e.Value(k) - g*a.Value(i,k));
          END
        END
      END
    ELSE
      e.Put(i,a.Value(i,l));
    d.Put(i,h);
    END;
    IF ReturnTransform THEN
      d.Put(1,0.0);
    e.Put(1,0.0);
    FOR i:=1 TO N DO
      BEGIN
      IF ReturnTransform THEN
        BEGIN
        l:=i-1;
        IF d.Value(i)<>0.0 THEN
          BEGIN
          FOR j:=1 TO l DO
            BEGIN
            g:=0.0;
            FOR k:=1 TO l DO
              g:=g+a.Value(i,k)*a.Value(k,j);
            FOR k:=1 TO l DO
              a.Put(k,j,a.Value(k,j)-g*a.Value(k,i))
            END
          END
        END;
      d.Put(i,a.Value(i,i));
      a.Put(i,i,1.0);
      IF ReturnTransform THEN
        FOR j:=1 TO l DO
          BEGIN
          a.Put(i,j,0.0);
          a.Put(j,i,0.0);
          END
      END;
    FOR i:=1 TO N-1 DO
     e.Put(i,e.Value(i+1));
    e.Put(n,0.0);
END;

PROCEDURE Tridiag(N: INTEGER; VAR d,e: DVector;
                  FindVectors: BOOLEAN; VAR evecs: DMatrix);
{This procedure finds the eigenvalues and, if FindVectors=TRUE, the     }
{eigenvectors of a symmetric tridiagonal matrix.  Here e[i] is the      }
{element BELOW the diagonal element d[i] with e[n]=0. The eigenvalues   }
{are returned in d and the eigenvectors are columns of evecs.           }
VAR
  iter, splitindex  : INTEGER;
  l,i,k             : INTEGER;
  dd,b,c,r,s,f,g,p  : REAL;

FUNCTION Split(L: INTEGER): INTEGER;
VAR
  i: INTEGER;
  SmallElt: BOOLEAN;
BEGIN
  SmallElt:=FALSE;
  i:=L;
  WHILE ((NOT SmallElt) AND (i<N)) DO
  BEGIN
    dd := abs(d.Value(i)) + abs(d.Value(i+1));
    SmallElt := (abs(e.Value(i)) + dd = dd);
    inc(i);
  END;
  IF SmallElt THEN split := i-1 ELSE split := N;
END;

BEGIN
  IF (d.size<>e.size) THEN
   FatalError('Tridiag: Mismatch of vector dimensions');
  IF (N>d.size) THEN
   FatalError('Tridiag: Specified dimension > vector dimensions.');
  FOR L:=1 TO N DO
    BEGIN
    iter:=0;
    splitindex:=Split(L);
    WHILE (splitindex <> L) DO
      BEGIN
      iter:=iter +1;
      g:=(d.Value(L+1) - d.Value(L))/(2.0*e.Value(L));
      r:=Sqrt(Sqr(g) + 1.0);
      g:=d.Value(splitindex)-d.Value(L)+e.Value(L)/(g + sign(r,g));
      s:=1.0; c:=1.0; p:=0.0;
      FOR i:=splitindex-1 DOWNTO L DO
        BEGIN
          f:=s*e.Value(i);
          b:=c*e.Value(i);
          IF abs(f) >= abs(g) THEN
            BEGIN
            c:=g/f;
            r:=Sqrt(Sqr(c)+1.0);
            e.Put(i+1,f*r);
            s:=1.0/r;
            c:=c*s
            END
          ELSE
            BEGIN
            s:=f/g;
            r:=Sqrt(Sqr(s)+1.0);
            e.Put(i+1,g*r);
            c:=1.0/r;
            s:=s*c
            END;
          g:=d.Value(i+1)-p;
          r:=(d.Value(i)-g)*s+2.0*c*b;
          p:=s*r;
          d.Put(i+1,g+p);
          g:=c*r-b;
          IF FindVectors THEN
            FOR k:=1 TO N DO
              BEGIN
              f:=evecs.Value(k,i+1);
              evecs.Put(k,i+1,s*evecs.Value(k,i)+c*f);
              evecs.Put(k,i,c*evecs.Value(k,i)-s*f);
              END
        END;
      d.Put(L,d.Value(L)-p);
      e.Put(L,g);
      e.Put(splitindex,0.0);
      splitindex:=Split(L);
      END
    END
END;

END.{CUPSproc}
