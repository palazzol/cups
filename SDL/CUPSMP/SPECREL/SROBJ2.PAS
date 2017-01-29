           (*************************************************)
           (*************************************************)
           (**                                             **)
           (**  Consortium of Upper-level Physics Software **)
           (**              ( CUPS Project )               **)
           (**      ( c) 1994 by John Wiley & Sons         **)
           (**  Compiled with Utilities Ver. 1.9 (95/09/10)**)
           (**                                             **)
           (*************************************************)
           (*************************************************)

UNIT srobj2;

INTERFACE
USES Crt, Dos, Graph, CUPSmupp, CUPS, CUPSgrph, CUPSfunc,CUPSproc, CUPSgui,
        Objects, srminkx, srobj1;

CONST
      normal = false;
      inverse = true;

      nonsymmetric = 1;
      symmetric = 2;
      antisymmetric = 3;




TYPE

TEntity = Object
   EntityType: Integer;
END;


   TScalar = Object(TEntity)

   Field: TField;
   Region: TRegion;
   FieldSelected: Boolean;
   CONSTRUCTOR Init(tx,ty,tLength,tHeight: Real);
   PROCEDURE Transform(Beta, Rotation: TSpaceVector);
   PROCEDURE Display;
   PROCEDURE SelectRegion(kPoint: Point);

   END;

   TVector = Object(TEntity)
   Field: ARRAY [0..3] OF TField;
   Region: TRegion;
   FieldSelected: Integer;
   CONSTRUCTOR Init(tx,ty,tLength,tHeight: Real);
   PROCEDURE Transform(ttype: Boolean;Beta, Rotation: TSpaceVector; VAR z: TVector);
   PROCEDURE Display;
   PROCEDURE SelectRegion(kPoint: Point);
   {PROCEDURE Graph(GraphNum,GraphType: Integer);}
   END;

   TTensor = Object(TEntity)

   Field: ARRAY [0..3,0..3] OF TField;
   Region: TRegion;
   Old: ARRAY [0..3,0..3] OF Real;
   FieldSelected: ARRAY [1..2] OF Integer;
   CONSTRUCTOR Init(tx,ty,tLength,tHeight: Real);
   PROCEDURE Transform(ttype: Boolean;Beta, Rotation: TSpaceVector; VAR z: TTensor);
   PROCEDURE DIsplay;
   PROCEDURE SelectRegion(kPoint: Point);
   PROCEDURE Graph(GraphNum,GraphType: Integer);
   PROCEDURE Symmeterize(symmetry: Byte);
   END;

IMPLEMENTATION

PROCEDURE TScalar.Transform(Beta,Rotation: TSpaceVector);

   BEGIN
   END;

PROCEDURE TVector.Transform(ttype: Boolean;Beta,Rotation: TSpaceVector; VAR z: TVector);

   VAR
      Gamma, Beta2, c1, Cxx, Cyy, Czz, Cxy, Cxz, Cyz: Real;
      j: Integer;

   BEGIN
      IF ttype = inverse THEN
        BEGIN
          Beta.x := -Beta.x;
          Beta.y := -Beta.y;
          Beta.z := -Beta.z;
        END;
      Beta2 := Sqr(beta.x) + Sqr(Beta.y) + Sqr(Beta.z);
      Gamma := 1/Sqrt(1-Beta2);
      IF Beta2 <> 0 THEN c1 := (Gamma-1)/Beta2 ELSE c1 := 0;
      Cxx := 1 + c1*Sqr(Beta.x);
      Cyy := 1 + c1*Sqr(Beta.y);
      Czz := 1 + c1*Sqr(Beta.z);
      Cxy := c1 * Beta.x*beta.y;
      Cxz := c1 * Beta.x*beta.z;
      Cyz := c1 * Beta.y*beta.z;
      z.Field[0].ChangeValue(Gamma*(Field[0].value - Beta.x*Field[1].value
                           -Beta.y*Field[2].value-Beta.z*Field[3].value));
      z.Field[1].ChangeValue(-Gamma*Beta.x*Field[0].value+Cxx*Field[1].value
                                +Cxy*Field[2].value+Cxz*Field[3].value);
      z.Field[2].ChangeValue(-Gamma*Beta.y*Field[0].value+Cxy*Field[1].value
                               +Cyy*Field[2].value+Cyz*Field[3].value);
      z.Field[3].ChangeValue(-Gamma*Beta.z*Field[0].value+Cxz*Field[1].value
                                +Cyz*Field[2].value+Czz*Field[3].value);
    END;

PROCEDURE TTensor.Transform(ttype: Boolean;Beta,rotation: TSpaceVector; VAR z: TTensor);

   VAR
     q,r,s: Real;
      Gamma, Beta2, c1, Cxx, Cyy, Czz, Cxy, Cxz, Cyz: Real;
      t,tt, xx, yy, zz, xxt, yyt, zzt: Real;
      i,j: Integer;
      x: ARRAY [0..3,0..3] OF Real;
   BEGIN
      IF ttype = inverse THEN
        BEGIN
          Beta.x := -Beta.x;
          Beta.y := -Beta.y;
          Beta.z := -Beta.z;
        END;
      Beta2 := Sqr(beta.x) + Sqr(Beta.y) + Sqr(Beta.z);
      Gamma := 1/Sqrt(1-Beta2);
      IF Beta2 <> 0 THEN c1 := (Gamma-1)/Beta2 ELSE c1 := 0;
      Cxx := 1 + c1*Sqr(Beta.x);
      Cyy := 1 + c1*Sqr(Beta.y);
      Czz := 1 + c1*Sqr(Beta.z);
      Cxy := c1 * Beta.x*beta.y;
      Cxz := c1 * Beta.x*beta.z;
      Cyz := c1 * Beta.y*beta.z;
      FOR j := 0 TO 3 DO
        BEGIN
          x[j,0] := Gamma*(Field[j,0].value - Beta.x*Field[j,1].value
                     -Beta.y*Field[j,2].value-Beta.z*Field[j,3].value);
          x[j,1] := -Gamma*Beta.x*Field[j,0].value+Cxx*Field[j,1].value
                     +Cxy*Field[j,2].value+Cxz*Field[j,3].value;
          x[j,2] := -Gamma*Beta.y*Field[j,0].value+Cxy*Field[j,1].value
                     +Cyy*Field[j,2].value+Cyz*Field[j,3].value;
          x[j,3] := -Gamma*Beta.z*Field[j,0].value+Cxz*Field[j,1].value
                     +Cyz*Field[j,2].value+Czz*Field[j,3].value;
         END;
      FOR j := 0 TO 3 DO
        BEGIN
          z.Field[0,j].ChangeValue(Gamma*(x[0,j] - Beta.x*x[1,j]
                                             -Beta.y*x[2,j]-Beta.z*x[3,j]));
          z.Field[1,j].ChangeValue(-Gamma*Beta.x*x[0,j]+Cxx*x[1,j]
                                             +Cxy*x[2,j]+Cxz*x[3,j]);
          z.Field[2,j].ChangeValue(-Gamma*Beta.y*x[0,j]+Cxy*x[1,j]
                                             +Cyy*x[2,j]+Cyz*x[3,j]);
          z.Field[3,j].ChangeValue(-Gamma*Beta.z*x[0,j]+Cxz*x[1,j]
                                             +Cyz*x[2,j]+Czz*x[3,j]);
         END;
   END;

PROCEDURE TTensor.Symmeterize(symmetry: Byte);

   VAR i,j: Integer;

   BEGIN
      CASE symmetry OF
      Symmetric:
         BEGIN
            FOR i := 0 TO 3 DO
              FOR j := i+1 TO 3 DO
                Field[j,i].ChangeValue(Field[i,j].Value);
         END;
      Antisymmetric:
         BEGIN
            FOR i := 0 TO 3 DO
              FOR j := i+1 TO 3 DO
                Field[j,i].ChangeValue(-Field[i,j].Value);
            FOR i := 0 TO 3 DO
               Field[i,i].ChangeValue(0);
         END;
      END;
   END;

{***************************************************************************

   TScalar Methods

***************************************************************************}

CONSTRUCTOR TScalar.Init(tx,ty,tLength,tHeight: Real);

   BEGIN
   WITH Region DO
   BEGIN
      x := tx;
      y := ty;
      Length := tLength;
      Height := tHeight;
      Field.value := 0;
      Field.Decimals := 2;
      Field.x1 := x + 1;
      Field.y1 := y + 1;
      Field.x2 := x + Length - 1;
      Field.y2 := y + Height - 1;
      WITH Field DO init(x1,y1,x2,y2,'',0,3);
   END;
   END;


PROCEDURE TScalar.Display;


   BEGIN
      WITH Field DO
      BEGIN
      HideCursor;
      Draw3DBox(Region);
      NumberString := Num2Str(Value,5);
      SetColor(MBlack);
         OutTextXY(MapX(x2)-TextWidth(NumberString),
                   MapY(y1)-TextHeight(NumberString),Num2Str(Value,5));
      ShowCursor;
      END;
   END;

PROCEDURE TScalar.SelectRegion(kPoint: Point);

   BEGIN
   END;

CONSTRUCTOR TVector.Init(tx,ty,tLength,tHeight: Real);

   VAR
      Deltax, Deltay: Real;
      i: Integer;

   BEGIN
      WITH Region DO
      BEGIN
      x := tx;
      y := ty;
      Length := tLength;
      Height := tHeight;
      Deltax := (Length-2);
      Deltay := (Height-2)/4;
      FOR i := 0 TO 3 DO
         WITH Field[3-i] DO
            BEGIN
               Value := 0;
               x1 := x + 1;
               y1 := y + 1 + i*Deltay;
               x2 := x1 + Deltax;
               y2 := y1 + Deltay;
               Init(x1,y1,x2,y2,'',0,3);
            END;
        Field[1].value := 1;
       END;
    END;


PROCEDURE TVector.Display;

   VAR i: Integer;

   BEGIN
      HideCursor;
      Draw3DBox(Region);
      SetLineStyle(SolidLn,0,NormWidth);
      SetColor(MBlack);
      FOR i := 0 TO 3 DO
        BEGIN
         PlotLine(Field[i].x1,Field[i].y1,Field[i].x2,Field[i].y1);
         PlotLine(Field[i].x2,Field[i].y1,Field[0].x2,Field[i].y2);
         PlotLine(Field[i].x2,Field[i].y2,Field[0].x1,Field[i].y2);
         PlotLine(Field[i].x1,Field[i].y2,Field[0].x1,Field[i].y1);
        END;
      FOR i := 0 TO 3 DO WITH Field[i] DO
         BEGIN
         NumberString := Num2Str(Value,5);
         SetColor(MBlack);
         OutTextXY(MapX(x2)-TextWidth(NumberString),
                   MapY(y1)-TextHeight(NumberString),Num2Str(Value,5));
         END;
      SetLineStyle(SolidLn,0,NormWidth);
      ShowCursor;
   END;

PROCEDURE TVector.SelectRegion(kPoint: Point);

   BEGIN
   END;

PROCEDURE TTensor.Graph(GraphNum,GraphType: Integer);

   BEGIN
     SelectViewPort(GraphNum);
     SelectScale(5);
   END;

CONSTRUCTOR TTensor.Init(tx,ty,tLength,tHeight: Real);

   VAR
      Deltax, Deltay: Real;
      i,j: Integer;

   BEGIN
      SelectViewPort(1);
      SelectScale(1);
      WITH Region DO
      BEGIN
      x := tx;
      y := ty;
      Length := tLength;
      Height := tHeight;
      Deltax := (Length-2)/4;
      Deltay := (Height-2)/4;
      FOR i := 0 TO 3 DO
      FOR j := 0 TO 3 DO
         WITH Field[3-j,i] DO
            BEGIN
               Init(x+1+i*Deltax,y+1+j*Deltay,x+1+(i+1)*Deltax,
                                                      y+1+(j+1)*Deltay,'',0,3);
               NumberString := '';
            END;
       END;
    END;

PROCEDURE TTensor.Display;

   VAR i,j: Integer;

   BEGIN
      HideCursor;
      Draw3DBox(Region);
      SetLineStyle(SolidLn,0,NormWidth);
      SetColor(MBlack);
      SelectViewPort(1);
      SelectScale(1);
      FOR i := 0 TO 3 DO
       FOR j := 0 TO 3 DO
        BEGIN
         PlotLine(Field[i,j].x1,Field[i,j].y1,Field[i,j].x2,Field[i,j].y1);
         PlotLine(Field[i,j].x2,Field[i,j].y1,Field[i,j].x2,Field[i,j].y2);
         PlotLine(Field[i,j].x2,Field[i,j].y2,Field[i,j].x1,Field[i,j].y2);
         PlotLine(Field[i,j].x1,Field[i,j].y2,Field[i,j].x1,Field[i,j].y1);
        END;
      FOR i := 0 TO 3 DO
         FOR j := 0 TO 3 DO WITH Field[i,j] DO
            BEGIN
            NumberString := Num2Str(Value,5);
            SetColor(MBlack);
            OutTextXY(MapX(x2)-TextWidth(NumberString),
                   MapY(y1)-TextHeight(NumberString),NumberString);
            END;
      SetLineStyle(SolidLn,0,NormWidth);
      ShowCursor;
   END;

PROCEDURE TTensor.SelectRegion(kPoint: Point);

   VAR
      Oldi, Oldj: Integer;
      i,j: Integer;

   BEGIN
      Oldi := FieldSelected[1];
      Oldj := FieldSelected[2];
      FOR i := 0 TO 3 DO
         FOR j := 0 TO 3 DO
            IF Field[i,j].ScrnReg.PtInRegion(kPoint) THEN
               BEGIN
                  FieldSelected[1] := i;
                  FieldSelected[2] := j;
               END;
      IF (Oldi <> FieldSelected[1]) OR (Oldj <> FieldSelected[2]) THEN
         BEGIN
            Field[Oldi,Oldj].HighLight(False);
            Field[FieldSelected[1],FieldSelected[2]].HighLight(True);
         END;
   END;

END.