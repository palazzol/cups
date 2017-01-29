{Unit of PC-specific routines used by simulations authored by Hiller }
{     One-dimensional scattering states (Scattr1D).                  }
{     Three-dimensional scattering states (Scattr3D).                }
{     Cylindrically symmetric bound states (CylSym).                 }
{     Hydrogenic atoms and the H2+ molecule (Hatom).                 }
{                                                                    }
{Author: John R. Hiller                                              }
{Version: 1.0               7/20/94                                  }
{Copyright (c) 1995 John Wiley & Sons, Inc.                          }
{                                                                    }
UNIT Sc3DMach;

INTERFACE

USES Graph,CUPS,CUPSmupp,CUPSfunc,CUPSproc,CUPSgui,CUPSgrph;

CONST
   Escape=#27;
   HeightFactor=2;
   WheelRadius=35;
   WheelX=0.4;
   WheelY=0.592;

TYPE
   RfileType=file OF real;
   Cbase=array[1..1] OF complex;
   CbasePtr=^Cbase;
   CVector=object                       {complex vector}
      {Public}
         VecPtr   :CbasePtr;
         Size,     
         MemBlock :longint;
         PROCEDURE Init(NumElts :integer);
         PROCEDURE Free;
         PROCEDURE Put(index :integer; cRe,cIm :real);
         PROCEDURE Get(index :integer; VAR c :complex);
         FUNCTION AbsVal(index :integer) :real;
         FUNCTION ArgVal(index :integer) :real;
      END; {CVector}

   CMatrix=object                        {complex matrix}
      {Public}
         MatPtr      :CbasePtr;
         NRows,NCols :integer;
         MemBlock    :longint;
         PROCEDURE Init(NumRows,NumCols :integer);
         PROCEDURE Free;
         PROCEDURE Put(Row,Col :integer; cRe,cIm :real);
         PROCEDURE Get(Row,Col :integer; VAR c :complex);
         PROCEDURE MultVec(VAR InVec,OutVec :CVector);
      END; {CMatrix}

   IDMatrix=object                        {complex matrix}
      {Public}
         IMatPtr     :IDbasePtr;
         NRows,NCols :integer;
         MemBlock    :longint;
         PROCEDURE Init(NumRows,NumCols :integer);
         PROCEDURE Free;
         PROCEDURE Put(Row,Col :integer; iVal :integer);
         FUNCTION Value(Row,Col :integer) :integer;
         Procedure Fill(iVal :integer);
      END; {IDMatrix}

   DCircBuf=object(DVector)                  {circular buffer}
         PutPos,GetPos      :integer;
         Filled,BeganToRead :boolean;
         PROCEDURE Init(NumElts :integer);
         PROCEDURE Reset;
         PROCEDURE Rewrite;
         PROCEDURE PutNext(x :real);
         PROCEDURE AddNext(x :real);
         FUNCTION GetNext :real;
         PROCEDURE Rotate(nSteps :integer);
         PROCEDURE Strip(nStrip :integer);
         FUNCTION Left :boolean;
         PROCEDURE MinMax(VAR iMin :integer; VAR Min :real;
                          VAR iMax :integer; VAR Max :real);
      END; {DCircBuf}

   DVectorExt=object(DVector)                {extension of DVector}
      {Public}
         PROCEDURE AddMultVec(N :integer; s :real; VAR Vec1,Vec2 :DVector);
         PROCEDURE DivVec2(N :integer; s :real; VAR Vec1,Vec2 :DVector);
      END; {DVectorExt}

   DMatrixExt=object(DMatrix) {extension of DMatrix}
      {Public}
         PROCEDURE EquateRow(Row,RowSize :integer; VAR InVec :DVector);
         PROCEDURE ExtractRow(Row,RowSize :integer; VAR OutVec :DVector);
         PROCEDURE EquateColumn(Col,ColSize :integer; VAR InVec :DVector);
         PROCEDURE ExtractColumn(Col,ColSize :integer; VAR OutVec :DVector);
         FUNCTION Norm(drho :real) :real;
         PROCEDURE Normalize;
         FUNCTION Projection(VAR Mat :DMatrixExt; drho :real) :real;
         PROCEDURE AddMatrix(s :real; VAR Mat :DMatrixExt);
         PROCEDURE WriteF(VAR Rfile :RfileType);
         PROCEDURE ReadF(VAR Rfile :RfileType);
      END; {DMatrixExt}

{--------Routines for DMatrices------------------------------------------}

PROCEDURE MagSqr (VAR Mat1, Mat2, MatOut: DMatrix);

{------File access routines---------------------------------------------}

PROCEDURE GetFileName(VAR FileName :string; VAR WishToAbort :boolean);
PROCEDURE ConnectTextFile(VAR f :text; filename :string;
                          VAR FileExists :boolean);
PROCEDURE ConnectRealFile(VAR f :RfileType; filename :string;
                          VAR FileExists :boolean);

{------Special plotting routines----------------------------------------}

PROCEDURE SetVGAHi;
PROCEDURE SetVGAMed;
FUNCTION bxor(Color1,Color2 :word) :word;
PROCEDURE LineTP(x1,y1,x2,y2 :word);
PROCEDURE PlotTrapezoid(sx1,sx2,sy0,sy1,sy2 :integer);

IMPLEMENTATION

{--------Methods for complex objects-----------------------------------}

{Patterned after DVector and DMatrix objects written by W.M. MacDonald.}

PROCEDURE CVector.Init(NumElts :integer);
VAR
   i: integer;
   MemReq,MaxBlock: string[10];
BEGIN
   Size:=NumElts;
   MemBlock:= Size*SizeOf(Cbase);
   IF ((MemBlock>MaxAvail) OR (MemBlock>=65521)) THEN
      BEGIN
         Str(MemBlock :10,MemReq);
         Str(MaxAvail :10,MaxBlock);
         Error(Concat('CVector:Init: Request for ',MemReq,
               ' exceeds free memory ',MaxBlock,'or 65521'));
         MemBlock:=0; Size:= 0;
         VecPtr:=Nil;
      END
   ELSE IF Size=0 THEN VecPtr:=Nil
   ELSE
      BEGIN
         GetMem(VecPtr,MemBlock);
         FOR i:=1 TO Size DO
            BEGIN
               VecPtr^[i].Re:=0.0;
               VecPtr^[i].Im:=0.0;
            END;
      END;
END; {CVector.Init}

PROCEDURE CVector.Free;
BEGIN
   FreeMem(VecPtr,MemBlock);
   MemBlock:=0;
   VecPtr:=Nil;
   Size:=0;
END; {CVector.Free}

PROCEDURE CVector.Put(index :integer; cRe,cIm :real);
BEGIN
{$IFOPT D+}
   IF index>Size THEN
      Error('CVector.Put: Index exceeds length of the vector');
{$ENDIF}
   VecPtr^[index].Re:=cRe;
   VecPtr^[index].Im:=cIm;
END;

PROCEDURE CVector.Get(index :integer; VAR c :complex);
BEGIN
{$IFOPT D+}
   IF index>Size THEN
      Error('CVector.Get: Index exceeds length of the vector');
{$ENDIF}
   c.Re:=VecPtr^[index].Re;
   c.Im:=VecPtr^[index].Im;
END;

FUNCTION CVector.AbsVal(index :integer) :real;
BEGIN
{$IFOPT D+}
   IF index>Size THEN
      Error('CVector.AbsVal: Index exceeds length of the vector');
{$ENDIF}
   AbsVal:=CABS(VecPtr^[index]);
END;

FUNCTION CVector.ArgVal(index :integer) :real;
BEGIN
{$IFOPT D+}
   IF index>Size THEN
      Error('CVector.ArgVal: Index exceeds length of the vector');
{$ENDIF}
   ArgVal:=Arg(VecPtr^[index]);
END;

PROCEDURE CMatrix.Init(NumRows,NumCols :integer);
VAR
   Row,Col :integer;
   Size :longint;
   MemReq, MaxBlock: string[10];
BEGIN
   NRows:=NumRows; NCols:=NumCols; Size:=NCols*NRows;
   MemBlock:=Size*SizeOf(Cbase);
   IF ((MemBlock>MaxAvail) OR (MemBlock>=65521)) THEN
      BEGIN
         Str(MemBlock :10,MemReq);
         Str(MaxAvail :10,MaxBlock);
         Error(ConCat('CMatrix: Request for ',MemReq,
               ' exceeds free memory ',MaxBlock,' or 65521'));
         MemBlock:=0; NRows:=0; NCols:=0;
         MatPtr:=Nil;
      END
   ELSE IF Size=0 THEN MatPtr:=Nil
   ELSE
      BEGIN
         GetMem(MatPtr,MemBlock);
         FOR Row:=1 TO NRows DO FOR Col:=1 TO NCols DO
            BEGIN
               MatPtr^[(Row-1)*NCols+Col].Re:=0.0;
               MatPtr^[(Row-1)*NCols+Col].Im:=0.0;
            END;
      END;
END; {CMatrix.Init}

PROCEDURE CMatrix.Free;
BEGIN
   FreeMem(MatPtr,MemBlock);
   MemBlock:=0;
   MatPtr:=Nil;
   NRows:=0; NCols:=0;
END; {CMatrix.Free}

PROCEDURE CMatrix.Put(Row,Col :integer; cRe,cIm :real);
BEGIN
{$IFOPT D+}
   IF (Row>NRows) THEN
      Error('CMatrix.Put: Row exceeds number of rows in the matrix')
   ELSE IF (Col>NCols) THEN
      Error('CMatrix.Put: Col exceeds number of columns in the matrix');
{$ENDIF}
   MatPtr^[(Row-1)*NCols+Col].Re:=cRe;
   MatPtr^[(Row-1)*NCols+Col].Im:=cIm;
END;

PROCEDURE CMatrix.Get(Row,Col :integer; VAR c :complex);
BEGIN
{$IFOPT D+}
   IF (Row>NRows) THEN
      Error('CMatrix.Get: Row exceeds number of rows in the matrix')
   ELSE IF (Col>NCols) THEN
      Error('CMatrix.Get: Col exceeds number of columns in the matrix');
{$ENDIF}
   c.Re:=MatPtr^[(Row-1)*NCols+Col].Re;
   c.Im:=MatPtr^[(Row-1)*NCols+Col].Im;
END;

PROCEDURE CMatrix.MultVec(VAR InVec,OutVec :CVector);
VAR
   Row,Col :integer;
   Sum,tempC : complex;
BEGIN
{$IFOPT D+}
   IF (InVec.size<>NCols) OR (OutVec.size<>NRows) THEN
      Error('CMatrix.MultVec: Input or Output vector has incorrect size');
{$ENDIF}
   FOR Row:= 1 TO NRows DO
      BEGIN
         Sum.Re:=0.0; Sum.Im:=0.0;
         FOR Col:= 1 TO NCols DO
            BEGIN
               CxOp(MatPtr^[(Row-1)*NCols+Col],
                    '*',InVec.VecPtr^[Col],tempC);
               CxOp(Sum,'+',tempC,Sum)
            END;
         OutVec.VecPtr^[Row].Re:=Sum.Re;
         OutVec.VecPtr^[Row].Im:=Sum.Im;
      END;
END; {CMatrix.MultVec}

{--------Methods for integer matrix------------------------------------}

{Patterned after DVector and DMatrix objects written by W.M. MacDonald.}

PROCEDURE IDMatrix.Init(NumRows,NumCols :integer);
VAR
   Row,Col :integer;
   Size :longint;
   MemReq, MaxBlock: string[10];
BEGIN
   NRows:=NumRows; NCols:=NumCols; Size:=NCols*NRows;
   MemBlock:=Size*SizeOf(Cbase);
   IF ((MemBlock>MaxAvail) OR (MemBlock>=65521)) THEN
      BEGIN
         Str(MemBlock :10,MemReq);
         Str(MaxAvail :10,MaxBlock);
         Error(ConCat('CMatrix: Request for ',MemReq,
               ' exceeds free memory ',MaxBlock,' or 65521'));
         MemBlock:=0; NRows:=0; NCols:=0;
         IMatPtr:=Nil;
      END
   ELSE IF Size=0 THEN IMatPtr:=Nil
   ELSE
      BEGIN
         GetMem(IMatPtr,MemBlock);
         FOR Row:=1 TO NRows DO FOR Col:=1 TO NCols DO
            IMatPtr^[(Row-1)*NCols+Col]:=0;
      END;
END; {IDMatrix.Init}

PROCEDURE IDMatrix.Free;
BEGIN
   FreeMem(IMatPtr,MemBlock);
   MemBlock:=0;
   IMatPtr:=Nil;
   NRows:=0; NCols:=0;
END; {IDMatrix.Free}

PROCEDURE IDMatrix.Put(Row,Col :integer; iVal :integer);
BEGIN
{$IFOPT D+}
   IF (Row>NRows) THEN
      Error('IDMatrix.Put: Row exceeds number of rows in the matrix')
   ELSE IF (Col>NCols) THEN
      Error('IDMatrix.Put: Col exceeds number of columns in the matrix');
{$ENDIF}
   IMatPtr^[(Row-1)*NCols+Col]:=iVal;
END;

FUNCTION IDMatrix.Value(Row,Col :integer) :integer;
BEGIN
{$IFOPT D+}
   IF (Row>NRows) THEN
      Error('IDMatrix.Value: Row exceeds number of rows in the matrix')
   ELSE IF (Col>NCols) THEN
      Error('IDMatrix.Value: Col exceeds number of columns in the matrix');
{$ENDIF}
   Value:=IMatPtr^[(Row-1)*NCols+Col];
END;

PROCEDURE IDMatrix.Fill(iVal :integer);
VAR
  pos: INTEGER;
BEGIN
   FOR pos:= 1 TO NRows*NCols DO IMatPtr^[pos]:= iVal;
END; {IDMatrix.Fill}

{--------Methods for DCircBuf object-----------------------------------}

PROCEDURE DCircBuf.Rewrite;
{Prepare buffer for input.}
BEGIN
   PutPos:=0; GetPos:=0;
   Filled:=False; BeganToRead:=False;
END; {DCircBuf.Rewrite}

PROCEDURE DCircBuf.Init(NumElts :integer);
{Initialize space for buffer and prepare for input.}
BEGIN
   DVector.Init(NumElts);
   DCircBuf.Rewrite;
END; {DCircBuf.Init}

PROCEDURE DCircBuf.Reset;
{Reset buffer for output.}
BEGIN
   IF Filled THEN GetPos:=PutPos ELSE GetPos:=0;
   BeganToRead:=False;
END; {DCircBuf.Reset}

PROCEDURE DCircBuf.PutNext(x :real);
{Put x in next location.}
BEGIN
   PutPos:=PutPos+1;
   IF PutPos=size+1 THEN PutPos:=1;
   IF PutPos=size THEN Filled:=True;
   Put(PutPos,x);
END; {DCircBuf.PutNext}

PROCEDURE DCircBuf.AddNext(x :real);
{Put x plus value in current location into next location.}
VAR
   oldVal :real;
BEGIN
   IF PutPos=0 THEN oldVal:=0.0 ELSE Get(PutPos,oldVal);
   PutNext(x+OldVal);
END; {DCircBuf.PutNext}

FUNCTION DCircBuf.GetNext :real;
{Get value from next location.}
BEGIN
   GetPos:=GetPos+1;
   IF GetPos=size+1 THEN GetPos:=1;
   GetNext:=Value(GetPos);
   BeganToRead:=True;
END; {DCircBuf.GetNext}

PROCEDURE DCircBuf.Rotate(nSteps :integer);
{Rotate buffer by nSteps steps, if buffer full.}
VAR
   i,j  :integer;
   temp :real;
BEGIN
   IF Filled THEN
      FOR i:=1 TO nSteps DO
         BEGIN
            temp:=Value(size);
            FOR j:=size DOWNTO 2 DO Put(j,Value(j-1));
            Put(1,temp);
            PutPos:=PutPos+1; GetPos:=GetPos+1;
         END;
END; {DCircBuf.Rotate}

PROCEDURE DCircBuf.Strip(nStrip :integer);
{Strip nStrip entries from buffer.}
BEGIN
   Rotate(size-PutPos);
   PutPos:=PutPos-nStrip;
   IF GetPos>PutPos THEN GetPos:=PutPos;
   Filled:=False;
   if PutPos=0 then BeganToRead:=False;
END; {DCircBuf.Strip}

FUNCTION DCircBuf.Left :boolean;
{True if unread value reamins.}
BEGIN
   IF BeganToRead AND (GetPos=PutPos) THEN Left:=False ELSE Left:=True;
END; {DCircBuf.Left}

PROCEDURE DCircBuf.MinMax(VAR iMin :integer; VAR Min :real;
                          VAR iMax :integer; VAR Max :real);
{Find minimum and maximum values and locations.}
VAR
   i,iStop: integer;
   tval: real;
BEGIN
   IF PutPos>0 THEN
      BEGIN
         IF Filled THEN iStop:=size ELSE iStop:=PutPos;
         iMin:=1; iMax:=1;
         Min:=VecPtr^[1]; Max:=VecPtr^[1];
         FOR  i:=2 TO iStop DO
            BEGIN
               tval:=VecPtr^[i];
               IF tval<Min THEN
                  BEGIN
                     iMin:=i; Min:=tval;
                  END
               ELSE IF tval>Max THEN
                  BEGIN
                     iMax:=i; Max:=tval;
                  END;
            END;
      END
   ELSE
      BEGIN
         iMin:=0; iMax:=0; Min:=0.0; Max:=0.0;
      END;
END; {DCircBuf.MinMax}

{--------Methods for DVectorExt object---------------------------------}

PROCEDURE DVectorExt.AddMultVec(N :integer; s :real;
                                VAR Vec1,Vec2 :DVector);
{Add s*Vec1[i]*Vec2[i].}
VAR
   Index :integer;
BEGIN
{$IFOPT D+}
   IF N>Size THEN
      Error('DVectorExt.AddMultVec: Input vectors have incorrect size');
{$ENDIF}
   FOR Index:=1 TO N DO
       VecPtr^[Index]:=VecPtr^[Index]
                       +s*Vec1.VecPtr^[Index]*Vec2.VecPtr^[Index];
END; {DVectorExt.AddMultVec}

PROCEDURE DVectorExt.DivVec2(N :integer; s :real;
                             VAR Vec1,Vec2 :DVector);
{Add s*Vec2[i]/sqr(Vec1[i]).}
VAR
   Index :integer;
BEGIN
{$IFOPT D+}
   IF N>Size THEN
      Error('DVectorExt.DivVec2: Input vectors have incorrect size');
{$ENDIF}
   FOR Index:=1 TO N DO
      VecPtr^[Index]:=VecPtr^[Index]
                      +s*Vec2.VecPtr^[Index]/sqr(Vec1.VecPtr^[Index]);
END; {DVectorExt.DivVec2}

{--------Methods for DMatrixExt object---------------------------------}

PROCEDURE DMatrixExt.EquateRow(Row,RowSize :integer;
                               VAR InVec :DVector);
{Insert row in matrix.}
VAR
   Col,Index :integer;
BEGIN
{$IFOPT D+}
   IF RowSize>NCols THEN
      Error('DMatrixExt.EquateRow: Input vector has incorrect size');
{$ENDIF}
   FOR Col:=1 TO RowSize DO
      BEGIN
         Index:=(Row-1)*NCols+Col;
         MatPtr^[Index]:=InVec.VecPtr^[Col];
      END;
END; {DMatrixExt.EquateRow}

PROCEDURE DMatrixExt.ExtractRow(Row,RowSize :integer;
                                VAR OutVec :DVector);
{Obtain row from matrix.}
VAR
   Col,Index :integer;
BEGIN
{$IFOPT D+}
   IF RowSize>NCols THEN
      Error('DMatrixExt.ExtractRow: Input vector has incorrect size');
{$ENDIF}
   FOR Col:=1 TO RowSize DO
      BEGIN
         Index:=(Row-1)*NCols+Col;
         OutVec.VecPtr^[Col]:=MatPtr^[Index];
      END;
END; {DMatrixExt.ExtractRow}

PROCEDURE DMatrixExt.EquateColumn(Col,ColSize :integer;
                                  VAR InVec :DVector);
{Insert column in matrix.}
VAR
   Row,Index :integer;
BEGIN
{$IFOPT D+}
   IF ColSize>NRows THEN
      Error('DMatrixExt.EquateColumn: Input vector has incorrect size');
{$ENDIF}
   FOR Row:=1 TO ColSize DO
      BEGIN
         Index:=(Row-1)*NCols+Col;
         MatPtr^[Index]:=InVec.VecPtr^[Row];
      END;
END; {DMatrixExt.EquateColumn}

PROCEDURE DMatrixExt.ExtractColumn(Col,ColSize :integer;
                                   VAR OutVec :DVector);
{Obtain column from matrix.}
VAR
   Row,Index :integer;
BEGIN
{$IFOPT D+}
   IF ColSize>NRows THEN
      Error('DMatrixExt.ExtractColumn: Input vector has incorrect size');
{$ENDIF}
   FOR Row:=1 TO ColSize DO
      BEGIN
         Index:=(Row-1)*NCols+Col;
         OutVec.VecPtr^[Row]:=MatPtr^[Index];
      END;
END; {DMatrixExt.ExtractColumn}

FUNCTION DMatrixExt.Norm(drho :real) :real;
VAR
   Index,Row,Col :integer;
   tempNorm,rho  :real;
BEGIN
   tempNorm:=0.0;
   FOR Row:=1 TO NRows DO
      BEGIN
         rho:=(Row-0.5)*drho;
         FOR Col:=1 TO NCols DO
            BEGIN
               Index:=(Row-1)*NCols+Col;
               tempNorm:=tempNorm+rho*sqr(MatPtr^[Index]);
            END;
      END;
   Norm:=sqrt(tempNorm);
END; {Norm}

PROCEDURE DMatrixExt.Normalize;
{Normalize matrix elements to be no more than one in absolute value.}
VAR
   MinRow,MinCol,MaxRow,MaxCol :integer;
   Min,Max :real;
BEGIN
   DMatrix.MinMax(MinRow,MinCol,Min,MaxRow,MaxCol,Max);
   IF abs(Min)>Max THEN Max:=abs(Min);
   IF Max=0.0 THEN Max:=1.0;
   Dmatrix.MultScalar(1/Max);
END; {DMatrixExt.Normalize}

FUNCTION DMatrixExt.Projection(VAR Mat :DMatrixExt; drho :real) :real;
VAR
   Index,Row,Col :integer;
   tempProj,rho  :real;
BEGIN
{$IFOPT D+}
   IF (NRows<>Mat.NRows) OR (NCols<>Mat.NCols) THEN
      Error('DMatrixExt.Projection: Input matrix has incorrect size');
{$ENDIF}
   tempProj:=0.0;
   FOR Row:=1 TO NRows DO
      BEGIN
         rho:=(Row-0.5)*drho;
         FOR Col:=1 TO NCols DO
            BEGIN
               Index:=(Row-1)*NCols+Col;
               tempProj:=tempProj+rho*MatPtr^[Index]*Mat.MatPtr^[Index];
            END;
      END;
   Projection:=tempProj;
END; {Projection}

PROCEDURE DMatrixExt.AddMatrix(s :real; VAR Mat :DMatrixExt);
VAR
   Index,Row,Col    :integer;
BEGIN
{$IFOPT D+}
   IF (NRows<>Mat.NRows) OR (NCols<>Mat.NCols) THEN
      Error('DMatrixExt.AddMatrix: Input matrix has incorrect size');
{$ENDIF}
   FOR Row:=1 TO NRows DO
      FOR Col:=1 TO NCols DO
         BEGIN
            Index:=(Row-1)*NCols+Col;
            MatPtr^[Index]:=MatPtr^[Index]+s*Mat.MatPtr^[Index];
         END;
END; {AddMatrix}

PROCEDURE DMatrixExt.WriteF(VAR Rfile :RfileType);
VAR
   Index,Row,Col :integer;
BEGIN
   FOR Row:=1 TO NRows DO
      FOR Col:=1 TO NCols DO
         BEGIN
            Index:=(Row-1)*Ncols+Col;
            write(Rfile,MatPtr^[Index]);
         END;
END; {DMatrixExt.WriteF}

PROCEDURE DMatrixExt.ReadF(VAR Rfile :RfileType);
VAR
   Index,Row,Col :integer;
BEGIN
   FOR Row:=1 TO NRows DO
      FOR Col:=1 TO NCols DO
         BEGIN
            Index:=(Row-1)*Ncols+Col;
            read(Rfile,MatPtr^[Index]);
         END;
END; {DMatrixExt.ReadF}

{--------Routines for DMatrices----------------------------------------}

PROCEDURE MagSqr(VAR Mat1,Mat2,MatOut :DMatrix);
VAR
   Row,Col,Index :integer;
BEGIN
   WITH MatOut DO
      BEGIN
{$IFOPT D+}
         IF (NRows<>Mat1.NRows) OR (NCols<>Mat1.NCols)
            OR (NRows<>Mat2.NRows) OR (NCols<>Mat2.NCols) THEN
               Error('DMatrixExt.MagSqr: An input matrix has incorrect size');
{$ENDIF}
         FOR Row:=1 TO NRows DO
            FOR Col:=1 TO NCols DO
               BEGIN
                  Index:=(Row-1)*Ncols+Col;
                  MatPtr^[Index]:=sqr(Mat1.MatPtr^[Index])
                                  +sqr(Mat2.MatPtr^[Index]);
               END;
      END;
END; {DMatrixExt.MagSqr}

{------File access routines---------------------------------------------}

PROCEDURE GetFileName(VAR FileName :string; VAR WishToAbort :boolean);
VAR
   tempFN   :string;
   Count,n  :integer;
   InputScr :TInputScreen;
BEGIN
   tempFN:=FileName;
   WITH InputScr DO
      BEGIN
         Init;
         DefineInputPort(0.12,0.82,0.4,0.65);
         LoadLine('');
         LoadLine(Concat(' File name: "',tempFN,'    "'));
         LoadLine('');
         LoadLine('    [  OK  ]    [Cancel]');
         SetString(1,tempFN);
         AcceptScreen;
         IF NOT Canceled THEN
            BEGIN
               FileName:=GetString(1);
               n:=Length(FileName);
               WHILE (n>0) AND (FileName[n]=' ') DO n:=n-1;
               IF n=0 THEN WishToAbort:=True
               ELSE
                  BEGIN
                     Count:=Length(FileName)-n;
                     Delete(FileName,n+1,Count);   {strip excess blanks}
                     WishToAbort:=False
                  END;
            END
         ELSE WishToAbort:=True;
         Done;
      END;
END; {GetFileName}

PROCEDURE ConnectTextFile(VAR f :text; filename :string;
                          VAR FileExists :boolean);
BEGIN
   Assign(f,filename);
   {$I-}
   reset(f);
   {$I+}
   IF IOresult=0 THEN FileExists:=True
   ELSE FileExists:=False;
END; {ConnectTextFile}

PROCEDURE ConnectRealFile(VAR f :RfileType; filename :string;
                          VAR FileExists :boolean);
BEGIN
   Assign(f,filename);
   {$I-}
   reset(f);
   {$I+}
   IF IOresult=0 THEN FileExists:=True
   ELSE FileExists:=False;
END; {ConnectRealFile}

{------Special plotting routines----------------------------------------}

PROCEDURE SetVGAHi;
BEGIN
   GraphMode:=VGAHi;
   SetGraphMode(GraphMode);
END;

PROCEDURE SetVGAMed;
BEGIN
   SetGraphMode(GraphMode);
END;

FUNCTION bxor(Color1,Color2 :word) :word;
BEGIN
   bxor:=Color1 xor Color2;
END;

PROCEDURE LineTP(x1,y1,x2,y2 :word);
BEGIN
   Line(x1,y1,x2,y2);
END;

PROCEDURE PlotTrapezoid(sx1,sx2,sy0,sy1,sy2 :integer);
VAR
   Trapezoid :array [1..4] OF PointType;
BEGIN
   Trapezoid[1].x:=sx1; Trapezoid[1].y:=sy0;
   Trapezoid[2].x:=sx1; Trapezoid[2].y:=sy1;
   Trapezoid[3].x:=sx2; Trapezoid[3].y:=sy2;
   Trapezoid[4].x:=sx2; Trapezoid[4].y:=sy0;
   FillPoly(4,Trapezoid);
END; {PlotTrapezoid}

END. {Sc3DMach}
