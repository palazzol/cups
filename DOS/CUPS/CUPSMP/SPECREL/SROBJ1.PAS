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

UNIT srobj1;

INTERFACE
USES Crt, Dos, Graph, CUPSmupp, CUPS, CUPSgrph, CUPSfunc,CUPSproc, CUPSgui,
        Objects, srminkx;

CONST
      MaxLength = 5;

      Graph1 = 5;
      Graph2 = 6;

      Maxt = 192;
      MinBright = 0.06;
      BrightRange = 0.2;
      RTheta = Pi/6;
      sinRTheta = 0.5;
      cosRTheta = 0.8660254;
      tanRTheta = 0.5773503;
      RAlpha = Pi/6;
      cosRAlpha = 0.8660254;
      sinRAlpha = 0.5;
      tanRAlpha = sinRAlpha/cosRAlpha;

      MBlack = 0;
      MCyan = 1;
      MMagenta = 2;
      MDarkGray = 3;
      MYellow = 11;
      MBlue = 7;
      MLightCyan = 8;
      MLightGray = 4;
      MLightMagenta = 12;
      MWhite = 15;




TYPE

FourVector = Record
     t,x,y,z: Real;
END;

InitRec = Record
   NeedInput: Boolean;
   kColor,kTubeColor: Integer;
   Pos: FourVector;
   V: TSpaceVector;
   Phi: TSpaceVector;
   Size: TSpaceVector;
   kVertices: Integer;
   END;

   TField = Object
   Value: Real;
   NumberString: String;
   Decimals: Byte;
   CursorPos: Integer;
   x1,y1,x2,y2: Real;
   ScrnReg: Region;
   BackColor: Integer;
   Name: String[14];
   NO_DECIMAL, NO_E: Boolean;
   PROCEDURE Init(ix1,iy1,ix2,iy2: Real;IName: String;IValue: Real;
                                               IDecimals:Byte);
   PROCEDURE HighLight(On: Boolean);
   PROCEDURE Edit(IChar: Char);
   PROCEDURE ChangeValue(NewVal: Real);
   PROCEDURE Draw;
   PROCEDURE IncValue(increment: Real);
   END;

   ReferenceFrame = Record
   betax,betay,betaz,phi,eta,theta: Real;
   END;

   BytePoint = Record
     x,y: Byte;
   END;

TFlashPoints1 = Record
      Points: ARRAY [1..192,1..21] OF BytePoint;
   END;

   TFlashPoints2 = Record
      Points: ARRAY [1..192,1..42] OF Point;
   END;

   TFlashRecord1 = Object
      PointsPtr: ^TFlashPoints1;
      CONSTRUCTOR Init;
   END;

   TFlashRecord2 = Object
      PointsPtr: ^TFlashPoints2;
      CONSTRUCTOR Init;
   END;

ScreenTrajectory = RECORD
   x: ARRAY[1..Maxt] OF Integer;
   y: ARRAY[1..Maxt] OF Integer;
   END;

TPointTrajectory = Object

   XYZ,XYT,XY: ScreenTrajectory;
   Length: Integer;
   PROCEDURE Init(points: Integer);
   PROCEDURE SetUp(x,y,z: DVector);
   PROCEDURE Transform(Frame: ReferenceFrame);
   PROCEDURE Plot(Color: Integer);
   PROCEDURE Done;
   END;

GObject = Object
   Color,TubeColor: Integer;
   DefiningFrame: ReferenceFrame;
   ObjectName: String;
   ObjectType: Byte;
   GInitRec: InitRec;
   NumberInfoRegions: Byte;
   InfoRegions: ARRAY [1..18] OF TField;
   InfoMin,InfoMax: Byte;
   CurrentRegion: Integer;
   CONSTRUCTOR Init(Initial: InitRec);
   DESTRUCTOR Done; VIRTUAL;
   PROCEDURE DrawCurrent(t,lastt: Integer; GraphType: Integer); VIRTUAL;
   PROCEDURE Transform(Beta: TSpaceVector); VIRTUAL;
   PROCEDURE DrawWorldTube(GraphNum,GraphType: Integer);VIRTUAL;
   PROCEDURE Change; VIRTUAL;
   PROCEDURE DrawInfo; VIRTUAL;
   PROCEDURE Update(RegionChanged: Integer; VAR BadVelocity: Boolean); VIRTUAL;
END;

PObject = Object(GObject)
   Vertices: Integer;
   Path, PathPrime: ARRAY [1..6] OF TPointTrajectory;
   XT,XTPrime: ScreenTrajectory;
   lx,ly: Real;
   Vx,Vy,Vz: Real;
   U1,U2: FourVector;
   X0,Y0,Z0: Real;
   X20,Y20,Z20: Real;
   OffsetX,OffsetY,OffsetZ: ARRAY [1..6] OF Real;
   PROCEDURE DrawWorldTube(GraphNum,GraphType: Integer);VIRTUAL;
   PROCEDURE DrawCurrent(t,lastt: Integer; GraphType: Integer); VIRTUAL;
   PROCEDURE SetPathFunctions;
   PROCEDURE Change; VIRTUAL;
   PROCEDURE DefineVertices; VIRTUAL;
   PROCEDURE Update(RegionChanged: Integer; VAR BadVelocity: Boolean); VIRTUAL;
   CONSTRUCTOR Init(Initial: InitRec);
   PROCEDURE Transform(Beta: TSpaceVector); VIRTUAL;
   DESTRUCTOR Done; VIRTUAL;
   END;

TRocket = Object(PObject)

  { InfoRegions: ARRAY [1..15] OF TField;}
   PROCEDURE DefineVertices; VIRTUAL;
   CONSTRUCTOR Init(Initial: InitRec);
   PROCEDURE DrawInfo; VIRTUAL;
   END;

TRocketPtr = ^TRocket;

TRectangle = Object(PObject)
   CONSTRUCTOR Init(Initial: InitRec);
   PROCEDURE DefineVertices; VIRTUAL;
   PROCEDURE DrawInfo; VIRTUAL;
   DESTRUCTOR Remove; VIRTUAL;
   END;

TRectanglePtr = ^TRectangle;

TFlash = Object(GObject)
   XT,XTPrime,XY,XYT,XYZ,XYPrime,XYTPrime,XYZPrime: Point;
   Location,LocationPrime: FourVector;
   HasBeenDrawn: Boolean;
   TStart,TStartPrime: Integer;
  { InfoRegions: ARRAY [1..8] OF TField;}
   PROCEDURE DrawInfo; VIRTUAL;
   PROCEDURE Update(RegionChanged: Integer; VAR BadVelocity: Boolean); VIRTUAL;
   PROCEDURE Transform(Beta: TSpaceVector); VIRTUAL;
   PROCEDURE DrawCurrent(i,lastt: Integer; GraphType: Integer); VIRTUAL;
   PROCEDURE DrawWorldTube(GraphNum,GraphType: Integer);VIRTUAL;
   CONSTRUCTOR Init(Initial: InitRec);
   DESTRUCTOR Done; VIRTUAL;

   END;

TFlashPtr = ^TFlash;


TRod = Object(PObject)

   PROCEDURE DefineVertices; VIRTUAL;
   CONSTRUCTOR Init(Initial: InitRec);
   PROCEDURE DrawInfo; VIRTUAL;
   {DESTRUCTOR Remove; VIRTUAL;}
  { PROCEDURE SetPathFunctions;}
   END;

TRodPtr = ^TRod;

TBall = Object(PObject)
   PROCEDURE DrawWorldTube(GraphNum,GraphType: Integer); VIRTUAL;
   PROCEDURE DrawCurrent(t,lastt: Integer; GraphType: Integer); VIRTUAL;
   CONSTRUCTOR Init(Initial: InitRec);
   PROCEDURE DrawInfo; VIRTUAL;
   DESTRUCTOR Remove; VIRTUAL;
   PROCEDURE DefineVertices; VIRTUAL;
   END;

TBallPtr = ^TBall;

TBarn = Object(PObject)
   PROCEDURE DefineVertices; VIRTUAL;
   CONSTRUCTOR Init(Initial: InitRec);
   PROCEDURE DrawInfo; VIRTUAL;
   DESTRUCTOR Remove;
   END;

TBarnPtr = ^TBarn;

VAR
      Beta: TSpaceVector;
      tstep: Real;
      FlashXY,FlashXYZ: TFlashRecord1;
      FlashXYT: TFlashRecord2;
      tbottom,ttop,tleft,tright,lasti: Integer;

IMPLEMENTATION

{****************************************************************************

   GObject Methods

****************************************************************************}

CONSTRUCTOR GObject.Init(Initial: InitRec);

   BEGIN
      CurrentRegion := Infomin;
      GInitRec := Initial;
   END;

DESTRUCTOR GObject.Done;

   BEGIN
   END;

PROCEDURE GObject.Change;

   BEGIN
   END;

PROCEDURE GObject.DrawCurrent;

   BEGIN
   END;

PROCEDURE GObject.Transform(Beta: TSpaceVector);

   BEGIN
   END;

PROCEDURE GObject.DrawWorldTube(GraphNum,GraphType: Integer);

   BEGIN
   END;

PROCEDURE GObject.DrawInfo;

   BEGIN
      SelectViewPort(1);
      SelectScale(1);
      SetColor(MWhite);
      OutTextxy(Mapx(13),MapY(39),ObjectName);
   END;

PROCEDURE GObject.Update(RegionChanged: Integer; VAR BadVelocity: Boolean);

   BEGIN
   END;

{****************************************************************************

   PObject Methods

****************************************************************************}

CONSTRUCTOR PObject.Init(Initial: InitRec);

   VAR
      i: Integer;
      t: Real;

   BEGIN
      WITH Initial DO
         BEGIN
           GObject.Init(Initial);
           x0 := pos.x;
           y0 := Initial.pos.y;
           z0 := Initial.pos.z;
           vx := Initial.v.x;
           vy := Initial.v.y;
           vz := Initial.v.z;
           lx := Initial.size.x;
           ly := Initial.size.y;
           U1.t := 1/sqrt(1-sqr(Vx)+Sqr(Vy)+Sqr(Vz));
           U1.x := Vx*U1.t;
           U1.y := Vy*U1.t;
           U1.z := Vz*U1.t;
           LorentzTransform(U1.t,U1.x,U1.y,U1.z,U2.t,U2.x,U2.y,U2.z,Beta);
           LorentzTransform(0,x0,y0,z0,t,x20,y20,z20,Beta);
         FOR i := 1 TO Vertices DO Path[i].Init(maxt);
         FOR i := 1 TO Vertices DO PathPrime[i].Init(maxt);
           DefineVertices;
           SetPathFunctions;
       END;
       Infomin := 1;
       NumberInfoRegions := 15;
       InfoRegions[1].Init(45,36.5,52,39.5,'Proper Length',lx,3);
       InfoRegions[2].Init(22,29,30,32,'Length',lx*(1-Sqr(U1.x)/(U1.t*(U1.t+1))),3);
       InfoRegions[3].Init(22,25,30,28,'x(t=0)',x0,3);
       InfoRegions[4].Init(22,22,30,25,'y(t=0)',y0,3);
       InfoRegions[5].Init(22,19,30,22,'z(t=0)',z0,3);
       InfoRegions[6].Init(22,15,30,18,'Vx',U1.x/U1.t,3);
       InfoRegions[7].Init(22,12,30,15,'Vy',U1.y/U1.t,3);
       InfoRegions[8].Init(22,9,30,12,'Vz',U1.z/U1.t,3);
       InfoRegions[9].Init(42.5,29,50.5,32,'Length',lx*(1-Sqr(U2.x)/(U2.t*(U2.t+1))),3);
       InfoRegions[10].Init(42.5,25,50.5,28,'x(t=0)',x20,3);
       InfoRegions[11].Init(42.5,22,50.5,25,'y(t=0)',y20,3);
       InfoRegions[12].Init(42.5,19,50.5,22,'z(t=0)',z20,3);
       InfoRegions[13].Init(42.5,15,50.5,18,'Vx',U2.x/U2.t,3);
       InfoRegions[14].Init(42.5,12,50.5,15,'Vy',U2.y/U2.t,3);
       InfoRegions[15].Init(42.5,9,50.5,12,'Vz',U2.z/U2.t,3);
       CurrentRegion := Infomin;
   END;

DESTRUCTOR PObject.Done;

   VAR
      i: Integer;

   BEGIN
      FOR i := 1 TO Vertices DO Path[i].Done;
      FOR i := 1 TO Vertices DO PathPrime[i].Done;
   END;

PROCEDURE PObject.Change;

   BEGIN
      DefineVertices;
      SetPathFunctions;
      Transform(Beta);
   END;


PROCEDURE PObject.DrawWorldTube(GraphNum, GraphType: Integer);

   VAR
      t,i,j,k: Integer;
      Cosine,Slope: Real;
      Brightness: Real;

   BEGIN
      SelectViewPort(GraphNum);
      SelectScale(5);
      HideCursor;
      IF GraphType = XYTGraph THEN
      BEGIN
      FOR i := 1 TO Vertices DO
      BEGIN
      FOR t := 1 TO Maxt DO
         BEGIN
            j := (i MOD Vertices)+1;
            SetColor(Black);
           { Line(Path[i].XYT.x[t],Path[i].XYT.y[t],
                   Path[j].XYT.x[t],Path[j].XYT.y[t]);}
            Cosine := (1.0*Path[j].XYT.x[t]-Path[i].XYT.x[t])/
                          SQRT(Sqr(Path[j].XYT.x[t]-Path[i].XYT.x[t])+
                               Sqr(Path[j].XYT.y[t]-Path[i].XYT.y[t]));
            IF Cosine < 0 THEN Cosine := ABS(Cosine);
            IF Path[j].XYT.x[t] <> Path[i].XYT.x[t] THEN Slope :=
                                      (Path[j].XYT.y[t]-Path[i].XYT.y[t])/
                                         (Path[j].XYT.x[t]-Path[i].XYT.x[t])
            ELSE Slope := 1000000;
            Brightness := MinBright + BrightRange*Cosine;
            FOR k := Path[i].XYT.x[t] TO Path[j].XYT.x[t] DO
               IF Random < Brightness THEN
                  PutPixel(k,Trunc((k-Path[i].XYT.x[t])*Slope+Path[i].XYT.y[t])
                                           ,TubeColor);
         END;
       END;
       {SetColor(Color);
       SetLineStyle(DottedLn,1,1);
       FOR j := 1 TO Vertices DO
       FOR i := 1 TO 99 DO
             Line(Path[j].XYT.x[i],Path[j].XYT.y[i],
                      Path[j].XYT.x[i+1],Path[j].XYT.y[i+1]);
       SetLineStyle(SolidLn,0,2);}
       END;
      IF GraphType = XYTPrimeGraph THEN
      BEGIN
      FOR i := 1 TO Vertices DO
      BEGIN
      FOR t := 1 TO Maxt DO
         BEGIN
            j := (i MOD Vertices)+1;
            SetColor(Black);
           { Line(Path[i].XYT.x[t],Path[i].XYT.y[t],
                   Path[j].XYT.x[t],Path[j].XYT.y[t]);}
            Cosine := (1.0*PathPrime[j].XYT.x[t]-PathPrime[i].XYT.x[t])/
                          SQRT(Sqr(PathPrime[j].XYT.x[t]-PathPrime[i].XYT.x[t])+
                               Sqr(PathPrime[j].XYT.y[t]-PathPrime[i].XYT.y[t]));
            IF Cosine < 0 THEN Cosine := ABS(Cosine);
            IF PathPrime[j].XYT.x[t] <> PathPrime[i].XYT.x[t] THEN Slope :=
                                      (PathPrime[j].XYT.y[t]-PathPrime[i].XYT.y[t])/
                                         (PathPrime[j].XYT.x[t]-PathPrime[i].XYT.x[t])
            ELSE Slope := 1000000;
            Brightness := MinBright + BrightRange*Cosine;
            FOR k := PathPrime[i].XYT.x[t] TO PathPrime[j].XYT.x[t] DO
               IF Random < Brightness THEN
                  PutPixel(k,Trunc((k-PathPrime[i].XYT.x[t])*Slope+PathPrime[i].XYT.y[t])
                                           ,TubeColor);
         END;
       END;
       {SetColor(Color);
       SetLineStyle(DottedLn,1,1);
       FOR j := 1 TO Vertices DO
       FOR i := 1 TO 99 DO
             Line(PathPrime[j].XYT.x[i],PathPrime[j].XYT.y[i],
                      PathPrime[j].XYT.x[i+1],PathPrime[j].XYT.y[i+1]);
       SetLineStyle(SolidLn,0,2);}
       END;
       IF GraphType = XTGraph THEN
         FOR t := 1 TO Maxt DO
          BEGIN
            Brightness := MinBright + BrightRange;
            FOR k := XT.x[t] TO XT.y[t] DO
              IF Random < Brightness THEN
                  PutPixel(k,192-t,TubeColor);
         END;
       IF GraphType = XTPrimeGraph THEN
         FOR t := 1 TO Maxt DO
          BEGIN
            Brightness := MinBright + BrightRange*0.5;
            FOR k := XTPrime.x[t] TO XTPrime.y[t] DO
              IF Random < Brightness THEN
                  PutPixel(k,192-t,TubeColor);
         END;
       ShowCursor;
   END;

PROCEDURE Pobject.DefineVertices;

   BEGIN
   END;

PROCEDURE PObject.DrawCurrent(t,lastt: Integer; GraphType: Integer);

   VAR
      i,j: Integer;
      Max: Integer;

   BEGIN
      SetWriteMode(XORPut);
      SetColor(Color);
      SetLineStyle(SolidLn,0,1);
      IF Vertices > 2 THEN max := Vertices ELSE Max := 1;
      HideCursor;
      FOR i := 1 TO Max DO
         BEGIN
            j := (i MOD Vertices) + 1;
            CASE GraphType OF
               XYTGraph:
                 BEGIN
                  IF lastt>0 THEN Line(Path[i].XYT.x[lastt],Path[i].XYT.y[lastt],
                         Path[j].XYT.x[lastt],Path[j].XYT.y[lastt]);
                  Line(Path[i].XYT.x[t],Path[i].XYT.y[t],
                         Path[j].XYT.x[t],Path[j].XYT.y[t]);
                 END;
               XYGraph:
                 BEGIN
                  IF lastt>0 THEN Line(Path[i].XY.x[lastt],Path[i].XY.y[lastt],
                         Path[j].XY.x[lastt],Path[j].XY.y[lastt]);
                  Line(Path[i].XY.x[t],Path[i].XY.y[t],
                         Path[j].XY.x[t],Path[j].XY.y[t]);
                 END;
               XYZGraph:
                 BEGIN
                  IF lastt > 0 THEN Line(Path[i].XYZ.x[lastt],Path[i].XYZ.y[lastt],
                         Path[j].XYZ.x[lastt],Path[j].XYZ.y[lastt]);
                  Line(Path[i].XYZ.x[t],Path[i].XYZ.y[t],
                         Path[j].XYZ.x[t],Path[j].XYZ.y[t]);
                 END;
               XYTPrimeGraph:
                 BEGIN
                  IF lastt > 0 THEN Line(PathPrime[i].XYT.x[lastt],PathPrime[i].XYT.y[lastt],
                         PathPrime[j].XYT.x[lastt],PathPrime[j].XYT.y[lastt]);
                  Line(PathPrime[i].XYT.x[t],PathPrime[i].XYT.y[t],
                         PathPrime[j].XYT.x[t],PathPrime[j].XYT.y[t]);
                 END;
               XYPrimeGraph:
                 BEGIN
                  IF lastt > 0 THEN Line(PathPrime[i].XY.x[lastt],PathPrime[i].XY.y[lastt],
                         PathPrime[j].XY.x[lastt],PathPrime[j].XY.y[lastt]);
                  Line(PathPrime[i].XY.x[t],PathPrime[i].XY.y[t],
                         PathPrime[j].XY.x[t],PathPrime[j].XY.y[t]);
                 END;
               {XTPrimeGraph:
                 BEGIN
                  IF lastt > 0 THEN Line(PathPrime[i].XT.x[lastt],PathPrime[i].XT.y[lastt],
                         PathPrime[j].XT.x[lastt],PathPrime[j].XT.y[lastt]);
                  Line(PathPrime[i].XT.x[t],PathPrime[i].XT.y[t],
                         PathPrime[j].XT.x[t],PathPrime[j].XT.y[t]);
                 END;}
               XYZPrimeGraph:
                 BEGIN
                  IF lastt > 0 THEN Line(PathPrime[i].XYZ.x[lastt],PathPrime[i].XYZ.y[lastt],
                         PathPrime[j].XYZ.x[lastt],PathPrime[j].XYZ.y[lastt]);
                  Line(PathPrime[i].XYZ.x[t],PathPrime[i].XYZ.y[t],
                         PathPrime[j].XYZ.x[t],PathPrime[j].XYZ.y[t]);
                 END;
            END;
         END;
         CASE GraphType OF
            XTGraph:
              BEGIN
               IF lastt > 0 THEN Line(XT.x[lastt],192-lastt,
                                                 XT.y[lastt],192-lastt);
               Line(XT.x[t],192-t,XT.y[t],192-t);
              END;
             XTPrimeGraph:
              BEGIN
               IF lastt > 0 THEN Line(XTPrime.x[lastt],192-lastt,
                     XTPrime.y[lastt],192-lastt);
               Line(XTPrime.x[t],192-t,XTPrime.y[t],192-t);
              END;
         END;
      ShowCursor;
      SetLineStyle(SolidLn,0,1);
      SetWriteMode(CopyPut);
   END;

PROCEDURE PObject.SetPathFunctions;

   VAR
     t: Real;
     x,y,z,xmin,xmax: DVector;
     i,j: Integer;

   BEGIN
      x.init(Maxt);
      y.init(maxt);
      z.init(maxt);
      xmin.init(maxt);
      xmax.init(maxt);
      FOR i := 1 TO Vertices DO
        BEGIN
         FOR j := 1 TO maxt DO
            BEGIN
               t := j*tstep+tmin;
               x.put(j, x0 + Vx*t + OffsetX[i]);
               y.put(j, y0 + Vy*t + OffsetY[i]);
               z.put(j, z0 + Vz*t + OffsetZ[i]);
               IF i = 1 THEN
                  BEGIN
                     xmin.put(j,x.value(j));
                     xmax.put(j,x.value(j));
                  END;
               IF xmin.value(j) > x.value(j) THEN xmin.put(j,x.value(j));
               IF xmax.value(j) < x.value(j) THEN xmax.put(j,x.value(j));
            END;
         Path[i].SetUp(x,y,z);
        END;
        SelectViewPort(Graph1);
        SelectScale(Graph1);
        FOR i := 1 TO maxt DO
          BEGIN
            XT.x[i] := MAPX(xmin.value(i));
            XT.y[i] := MapX(xmax.value(i));
          END;
      x.free;
      y.free;
      z.free;
      xmin.free;
      xmax.free;
   END;

PROCEDURE PObject.Update(RegionChanged: Integer; VAR BadVelocity: Boolean);

   VAR
      tt,xx,yy,zz: Real;
      dummy: Boolean;

   FUNCTION CheckVelocity(vx,vy,vz: Real): Boolean;

      VAR
         c: Real;

      BEGIN
         IF (sqr(vx)+Sqr(Vy)+Sqr(Vz)) >= 1 THEN CheckVelocity := False
         ELSE CheckVelocity := true;
      END;

   PROCEDURE EditV;
      BEGIN
           IF CheckVelocity(InfoRegions[6].Value,InfoRegions[7].Value,
                               InfoRegions[8].Value) THEN
             BEGIN
               IF BadVelocity THEN DisplayPrompt(2);
               U1.t := Sqrt(1/(1-Sqr(InfoRegions[6].Value)+Sqr(InfoRegions[7].value)
                           +Sqr(InfoRegions[8].Value)));
               U1.y := InfoRegions[7].value*U1.t;
               {InfoRegions[2].ChangeValue(InfoRegions[1].value/U1.t);}
               U1.x := InfoRegions[6].value*U1.t;
               U1.z := InfoRegions[8].value*U1.t;
               Vx := InfoRegions[6].value;
               Vy := InfoRegions[7].value;
               Vz := InfoRegions[8].value;
               Update(0,dummy);
               Update(1,dummy);
               BadVelocity := False;
             END
           ELSE BadVelocity := True;
         END;

   PROCEDURE ChangePosition(direction: Byte);
      BEGIN
         IF direction = 1 THEN
          BEGIN
            LorentzTransform(0,x0,y0,z0,tt,x20,y20,z20,Beta);
            x20 := x20-tt*U2.x/U2.t;
            y20 := y20-tt*U2.y/U2.t;
            z20 := z20-tt*U2.z/U2.t;
            InfoRegions[10].ChangeValue(x20);
            InfoRegions[11].ChangeValue(y20);
            InfoRegions[12].ChangeValue(z20);
          END
         ELSE
           BEGIN
            InvLorentzTransform(0,x20,y20,z20,tt,x0,y0,z0,Beta);
            x0 := x0-tt*U1.x/U1.t;
            y0 := y0-tt*U1.y/U1.t;
            z0 := z0-tt*U1.z/U1.t;
            InfoRegions[3].ChangeValue(x0);
            InfoRegions[4].ChangeValue(y0);
            InfoRegions[5].ChangeValue(z0);
           END;
        END;

   BEGIN
      CASE RegionChanged OF
       0:BEGIN
          LorentzTransform(U1.t,U1.x,U1.y,U1.z,U2.t,U2.x,U2.y,U2.z,Beta);
          InfoRegions[13].ChangeValue(U2.x/U2.t);
          InfoRegions[14].ChangeValue(U2.y/U2.t);
          InfoRegions[15].ChangeValue(U2.z/U2.t);
          InfoRegions[9].ChangeValue(InfoRegions[1].Value*(1-Sqr(U2.x)/(U2.t*(U2.t+1))));
          LorentzTransform(0,X0,Y0,Z0,tt,xx,yy,zz,Beta);
          InfoRegions[10].ChangeValue(xx-U2.x/U2.t*tt);
          InfoRegions[11].ChangeValue(yy-U2.y/U2.t*tt);
          InfoRegions[12].ChangeValue(zz-U2.z/U2.t*tt);
          END;
       1:IF infomin < 3 THEN
         BEGIN
          Lx := InfoRegions[1].value;
          InfoRegions[2].ChangeValue(Lx*(1-Sqr(U1.x)/(U1.t*(U1.t+1))));
          InfoRegions[9].ChangeValue(Lx*(1-Sqr(U2.x)/(U2.t*(U2.t+1))));
         END;
       2:IF infomin < 3 THEN
         BEGIN
          Lx := InfoRegions[2].value/(1-Sqr(U1.x)/(U1.t*(U1.t+1)));
          InfoRegions[1].ChangeValue(Lx);
          InfoRegions[9].ChangeValue(Lx*(1-Sqr(U2.x)/(U2.t*(U2.t+1))));
         END;
       3:IF infomin < 3 THEN
         BEGIN
          x0 := InfoRegions[3].value;
          ChangePosition(1);
         END;
       4:BEGIN
          y0 := InfoRegions[4].value;
          ChangePosition(1);
         END;
       5:BEGIN
          z0 := InfoRegions[5].value;
          ChangePosition(1);
         END;
       6,7,8: EditV;
       9:BEGIN
          Lx := InfoRegions[9].value/(1-Sqr(U2.x)/(U2.t*(U2.t+1)));
          InfoRegions[1].ChangeValue(Lx);
          InfoRegions[2].ChangeValue(Lx*(1-Sqr(U1.x)/(U1.t*(U1.t+1))));
         END;
       10:BEGIN
          x20 := InfoRegions[10].value;
          ChangePosition(2);
         END;
       11:BEGIN
          y20 := InfoRegions[11].value;
          ChangePosition(2);
         END;
       12:BEGIN
          z20 := InfoRegions[12].value;
          ChangePosition(2);
         END;
       13,14,15:BEGIN
           IF CheckVelocity(InfoRegions[13].Value,InfoRegions[14].Value,
                               InfoRegions[15].Value) THEN
             BEGIN
               IF BadVelocity THEN DisplayPrompt(2);
               U2.t := Sqrt(1/(1-Sqr(InfoRegions[13].Value)+Sqr(InfoRegions[14].value)
                           +Sqr(InfoRegions[15].Value)));
               U2.y := InfoRegions[14].value*U1.t;
               {InfoRegions[2].ChangeValue(InfoRegions[1].value/U1.t);}
               U2.x := InfoRegions[13].value*U1.t;
               U2.z := InfoRegions[15].value*U1.t;
               InvLorentzTransform(U2.t,U2.x,U2.y,U2.z,U1.t,U1.x,U1.y,U1.z,Beta);
               InfoRegions[6].ChangeValue(U1.x/U1.t);
               InfoRegions[7].ChangeValue(U1.y/U1.t);
               InfoRegions[8].ChangeValue(U1.z/U1.t);
               InfoRegions[2].ChangeValue(InfoRegions[1].Value/U1.t);
               LorentzTransform(0,InfoRegions[10].Value,InfoRegions[11].Value,
                             InfoRegions[12].Value,tt,xx,yy,zz,Beta);
               InfoRegions[3].ChangeValue(xx-U1.x/U1.t*tt);
               InfoRegions[4].ChangeValue(yy-U1.y/U1.t*tt);
               InfoRegions[5].ChangeValue(zz-U1.z/U1.t*tt);
               x0 := InfoRegions[3].value;
               y0 := InfoRegions[4].value;
               z0 := InfoRegions[5].Value;
               BadVelocity := False;
             END
           ELSE BadVelocity := True;
         END;
      END;
      GInitRec.pos.x := x0;
      GInitRec.pos.y := y0;
      GInitRec.pos.x := z0;
      GinitRec.V.x := vx;
      GinitRec.V.y := vy;
      GinitRec.V.z := vz;
      GINitRec.Size.x := Lx;
   END;

PROCEDURE PObject.Transform( Beta: TSpaceVector);

   VAR
      Gamma, Beta2, c1, Cxx, Cyy, Czz, Cxy, Cxz, Cyz: Real;
      t,tt, xx, yy, zz, xxt, yyt, zzt: Real;
      i,j: Integer;
      x,y,z,xmin,xmax: DVector;

   BEGIN
      x.init(maxt);
      xmin.init(maxt);
      xmax.init(maxt);
      y.init(maxt);
      z.init(maxt);
      Beta2 := Sqr(beta.x) + Sqr(Beta.y) + Sqr(Beta.z);
      Gamma := 1/Sqrt(1-Beta2);
      IF Beta2 <> 0 THEN c1 := (Gamma-1)/Beta2 ELSE c1 := 0;
      Cxx := 1 + c1*Sqr(Beta.x);
      Cyy := 1 + c1*Sqr(Beta.y);
      Czz := 1 + c1*Sqr(Beta.z);
      Cxy := c1 * Beta.x*beta.y;
      Cxz := c1 * Beta.x*beta.z;
      Cyz := c1 * Beta.y*beta.z;
      FOR j := 1 TO Vertices DO
      BEGIN
      FOR i := 1 TO maxt DO
      BEGIN
         WITH Path[j] DO
         BEGIN
            t:= (i*tstep+tmin)/Gamma;
            tt := (t + Beta.x*(X0+OffsetX[j])+Beta.y*(Y0+OffsetY[j])+
                       Beta.z*(Z0+OffsetZ[j]))/
                            (1 - Beta.x*Vx - Beta.y*Vy - Beta.z*Vz);
            xxt := x0 + Vx*tt + OffsetX[j];
            yyt := y0 + Vy*tt + OffsetY[j];
            zzt := z0 + Vz*tt + OffsetZ[j];
            xx := Gamma*(-Beta.x*tt) + Cxx*xxt + Cxy*yyt + Cxz*zzt;
            yy := Gamma*(-Beta.y*tt) + Cxy*xxt + Cyy*yyt + Cyz*zzt;
            zz := Gamma*(-Beta.z*tt) + Cxz*xxt + Cyz*yyt + Czz*zzt;
         END;
         x.put(i,xx);
         y.put(i,yy);
         z.put(i,zz);
         IF j = 1 THEN
            BEGIN
              xmin.put(i,x.value(i));
              xmax.put(i,x.value(i));
            END;
         IF xmin.value(i) > x.value(i) THEN xmin.put(i,x.value(i));
         IF xmax.value(i) < x.value(i) THEN xmax.put(i,x.value(i));
      END;
      PathPrime[j].SetUp(x,y,z);
      END;
      SelectViewPort(Graph1);
      SelectScale(Graph1);
      FOR i := 1 TO maxt DO
        BEGIN
          XTPrime.x[i] := MapX(xmin.value(i));
          XTPrime.y[i] := MapX(xmax.value(i));
        END;
      x.free;
      xmin.free;
      xmax.free;
      y.free;
      z.free;
   END;

{****************************************************************************

   TRod Methods

****************************************************************************}

PROCEDURE TRod.DefineVertices;

   VAR
      xfactor,yfactor: Real;

   BEGIN
      xfactor := 1-Sqr(u1.x)/(U1.t*(1+U1.t));
      yfactor := 1-Sqr(u1.y)/(U1.t*(1+U1.t));
      OffsetX[1] := Lx/2*xfactor;
      OffsetY[1] := 0;
      OffsetZ[1] := 0;
      OffsetX[2] := -Lx/2*xfactor;
      OffsetY[2] := 0;
      OffsetZ[2] := 0;
   END;

CONSTRUCTOR TRod.Init(Initial: InitRec);

   VAR
     i: Integer;

   BEGIN
      ObjectName := 'Stick';
      ObjectType := 1;
      Vertices := 2;
      Color := MLightCyan+1;
      TubeColor := MCyan;
      infoMin := 1;
      PObject.Init(Initial);
   END;

PROCEDURE TRod.DrawInfo;

   VAR
     i: Integer;
   BEGIN
      SelectViewPort(1);
      SelectScale(1);
      SetColor(MBlack);
      HideCursor;
      PlotLine(12.1,36,52.9,36);
      PlotLine(32.5,36,32.5,3.2);
      OutTextxy(MapX(13),MapY(35),'Frame 1');
      OutTextXy(MapX(33.5),MapY(35),'Frame 2');
      OutTextxy(Mapx(13),MapY(39),ObjectName);
      SetColor(Color);
      PlotLine(21,38,27,38);
      FOR i := 1 TO 15 DO
         BEGIN
         InfoRegions[i].Draw;
         SetTextJustify(RightText,TopText);
         SetColor(MBlack);
         OuttextXY(MapX(InfoRegions[i].x1)-2,MapY(InfoRegions[i].y2-1),
                        InfoRegions[i].Name);
         SetTextJustify(LeftText,TopText);
         END;
      ShowCursor;
   END;

{****************************************************************************

   TRocket Methods

****************************************************************************}

PROCEDURE TRocket.DrawInfo{(VAR RegionList: RegionPtrList)};

   VAR
      i: Integer;
   BEGIN
      SelectViewPort(1);
      SelectScale(1);
      SetColor(MBlack);
      HideCursor;
      PlotLine(12.1,36,52.9,36);
      PlotLine(32.5,36,32.5,3.2);
      OutTextxy(MapX(13),MapY(35),'Frame 1');
      OutTextXy(MapX(33.5),MapY(35),'Frame 2');
      OutTextxy(Mapx(13),MapY(39),ObjectName);
      {WriteName;}
      {DrawSymbol;}
      SetColor(Color);
      {FOR i := 1 TO 8 DO
         BEGIN
            theta := pi*i/4;
            sinTheta := sin(theta);
            costheta := cos(theta);
            PlotLine(22+0.5*costheta,38+0.5*sintheta,
                       22+1.5*costheta,38+1.5*sintheta);
         END;}
      PlotLine(27,38,26,39);
      PlotLine(26,39,21,39);
      PlotLine(21,39,21,37);
      {PlotLine(22,38,21,37);}
      PlotLine(21,37,26,37);
      PlotLine(26,37,27,38);
      FOR i := 1 TO 15 DO
         BEGIN
         InfoRegions[i].Draw;
         SetTextJustify(RightText,TopText);
         SetColor(MBlack);
         OuttextXY(MapX(InfoRegions[i].x1)-2,MapY(InfoRegions[i].y2-1),
                        InfoRegions[i].Name);
         SetTextJustify(LeftText,TopText);
         END;
{      RegionList := ^Info;
      RegionList.Display;}
      ShowCursor;
   END;

PROCEDURE TRocket.DefineVertices;

   VAR
      xfactor,yfactor: Real;

   BEGIN
      xfactor := 1-Sqr(u1.x)/(U1.t*(1+U1.t));
      yfactor := 1-Sqr(u1.y)/(U1.t*(1+U1.t));
      OffsetX[1] := 0;
      OffsetY[1] := 0;
      OffsetZ[1] := 0;
      OffsetX[2] := -lx/5*xfactor;
      OffsetY[2] := -ly/2*yfactor;
      OffsetZ[2] := 0;
      OffsetX[3] := -lx*xfactor;
      OffsetY[3] := -ly/2*yfactor;
      OffsetZ[3] := 0;
      {OffsetX[4] := -0.8*lx*xfactor;
      OffsetY[4] := 0;
      OffsetZ[4] := 0;}
      OffsetX[4] := -lx*xfactor;
      OffsetY[4] := ly/2*yfactor;
      OffsetZ[4] := 0;
      OffsetX[5] := -lx/5*xfactor;
      OffsetY[5] := ly/2*yfactor;
      OffsetZ[5] := 0;
   END;

CONSTRUCTOR TRocket.Init(Initial: InitRec);

   VAR
      i: Integer;
      t: Real;

   BEGIN
      ObjectName := 'Rocket';
      ObjectType := 2;
      Vertices := 5;
      Color := MLightMagenta+2;
      TubeColor := MMagenta;
      infoMin := 1;
      PObject.init(Initial);
   END;

{****************************************************************************

   TBarn Methods

****************************************************************************}

CONSTRUCTOR TBarn.Init;

   VAR
      i: Integer;

   BEGIN
      ObjectName := 'Barn';
      ObjectType := 4;
      Vertices := 6;
      Color := MLightMagenta+2;
      TubeColor := MMagenta;
      infoMin := 1;
      PObject.init(Initial);
   END;

PROCEDURE TBarn.DefineVertices;

   VAR
      xfactor,yfactor: Real;

   BEGIN
      ly := 0.5;
      xfactor := 1-Sqr(u1.x)/(U1.t*(1+U1.t));
      yfactor := 1-Sqr(u1.y)/(U1.t*(1+U1.t));
      OffsetX[1] := lx/2*xfactor;
      OffsetY[1] := -ly/4*yfactor;
      OffsetZ[1] := 0;
      OffsetX[2] := lx/2*xfactor;
      OffsetY[2] := ly/4*yfactor;
      OffsetZ[2] := 0;
      OffsetX[3] := lx/4*xfactor;
      OffsetY[3] := 3*ly/4*yfactor;
      OffsetZ[3] := 0;
      OffsetX[4] := -lx/4*xfactor;
      OffsetY[4] := 3*ly/4*yfactor;
      OffsetZ[4] := 0;
      OffsetX[5] := -lx/2*xfactor;
      OffsetY[5] := ly/4*yfactor;
      OffsetZ[5] := 0;
      OffsetX[6] := -lx/2*xfactor;
      OffsetY[6] := -ly/4*yfactor;
      OffsetZ[6] := 0;
   END;

PROCEDURE TBarn.DrawInfo;

   VAR
      i: Integer;

   BEGIN
      SelectViewPort(1);
      SelectScale(1);
      SetColor(MBlack);
      HideCursor;
      PlotLine(12.1,36,52.9,36);
      PlotLine(32.5,36,32.5,3.2);
      OutTextxy(MapX(13),MapY(35),'Frame 1');
      OutTextXy(MapX(33.5),MapY(35),'Frame 2');
      OutTextxy(Mapx(13),MapY(39),ObjectName);
      SetColor(Color);
      PlotLine(21.6,39,22.4,39);
      PlotLine(22.4,39,23,38);
      PlotLine(23,38,23,37);
      PlotLine(23,37,21,37);
      PlotLine(21,37,21,38);
      PlotLine(21,38,21.6,39);
      FOR i := 1 TO 15 DO
         BEGIN
         InfoRegions[i].Draw;
         SetTextJustify(RightText,TopText);
         SetColor(MBlack);
         OuttextXY(MapX(InfoRegions[i].x1)-2,MapY(InfoRegions[i].y2-1),
                        InfoRegions[i].Name);
         SetTextJustify(LeftText,TopText);
         END;
      ShowCursor;
   END;

DESTRUCTOR TBarn.Remove;

   BEGIN
   END;

{****************************************************************************

   TRectangle Methods

****************************************************************************}

PROCEDURE TRectangle.DefineVertices;

   VAR
      xfactor,yfactor: Real;

   BEGIN
      ly := 0.5;
      xfactor := 1-Sqr(u1.x)/(U1.t*(1+U1.t));
      yfactor := 1-Sqr(u1.y)/(U1.t*(1+U1.t));
      OffsetX[1] := Lx/2*xfactor;
      OffsetY[1] := Ly/2*yfactor;
      OffsetZ[1] := 0;
      OffsetX[2] := -Lx/2*xfactor;
      OffsetY[2] := Ly/2*yfactor;
      OffsetZ[2] := 0;
      OffsetX[3] := -Lx/2*xfactor;
      OffsetY[3] := -Ly/2*yfactor;
      OffsetZ[3] := 0;
      OffsetX[4] := Lx/2*xfactor;
      OffsetY[4] := -Ly/2*yfactor;
      OffsetZ[4] := 0;
   END;

PROCEDURE TRectangle.DrawInfo;

   VAR
      i: Integer;

   BEGIN
      SelectViewPort(1);
      SelectScale(1);
      SetColor(MBlack);
      HideCursor;
      PlotLine(12.1,33,52.9,33);
      PlotLine(32.5,33,32.5,3.2);
      OutTextxy(MapX(13),MapY(32),'Frame 1');
      OutTextXy(MapX(33.5),MapY(32),'Frame 2');
      OutTextxy(Mapx(13),MapY(39),ObjectName);
      SetColor(Color);
      PlotLine(25,39,27,39);
      PlotLine(27,39,27,37);
      PlotLine(27,37,25,37);
      PlotLine(25,37,25,39);
      FOR i := 1 TO NumberInfoRegions DO
         BEGIN
         InfoRegions[i].Draw;
         SetTextJustify(RightText,TopText);
         SetColor(MBlack);
         OuttextXY(MapX(InfoRegions[i].x1)-2,MapY(InfoRegions[i].y2-1),
                        InfoRegions[i].Name);
         SetTextJustify(LeftText,TopText);
         END;
      ShowCursor;
   END;

CONSTRUCTOR TRectangle.Init;

   VAR
      i: Integer;

   BEGIN
      ObjectName := 'Rectangle';
      ObjectType := 7;
      Vertices := 4;
      Color := MLightMagenta+2;
      TubeColor := MMagenta;
      infoMin := 1;
      PObject.init(Initial);
       NumberInfoRegions := 18;
       InfoRegions[1].Init(45,36.5,52,39.5,'ProperLength',lx,3);
       InfoRegions[16].Init(45,33.5,52,36.5,'Proper Width',ly,3);
       InfoRegions[2].Init(22,26,30,29,'Length',lx*(1-Sqr(U1.x)/(U1.t*(U1.t+1))),3);
       InfoRegions[17].Init(22,23,30,26,'Width',ly*(1-Sqr(U1.y)/(U1.t*(U1.t+1))),3);
       InfoRegions[3].Init(22,19,30,22,'x(t=0)',x0,3);
       InfoRegions[4].Init(22,16,30,19,'y(t=0)',y0,3);
       InfoRegions[5].Init(22,13,30,16,'z(t=0)',z0,3);
       InfoRegions[6].Init(22,9,30,12,'Vx',U1.x/U1.t,3);
       InfoRegions[7].Init(22,6,30,9,'Vy',U1.y/U1.t,3);
       InfoRegions[8].Init(22,3,30,6,'Vz',U1.z/U1.t,3);
       InfoRegions[9].Init(42.5,26,50.5,29,'Length',lx*(1-Sqr(U2.x)/(U2.t*(U2.t+1))),3);
       InfoRegions[18].Init(42.5,23,50.5,26,'Width',ly*(1-Sqr(U2.y)/(U2.t*(U2.t+1))),3);
       InfoRegions[10].Init(42.5,19,50.5,22,'x(t=0)',x20,3);
       InfoRegions[11].Init(42.5,16,50.5,19,'y(t=0)',y20,3);
       InfoRegions[12].Init(42.5,13,50.5,16,'z(t=0)',z20,3);
       InfoRegions[13].Init(42.5,9,50.5,12,'Vx',U2.x/U2.t,3);
       InfoRegions[14].Init(42.5,6,50.5,9,'Vy',U2.y/U2.t,3);
       InfoRegions[15].Init(42.5,3,50.5,6,'Vz',U2.z/U2.t,3);
       Currentregion := Infomin;
   END;

DESTRUCTOR TRectangle.Remove;

   BEGIN
   END;
{****************************************************************************

   TBall Methods

****************************************************************************}


PROCEDURE TBall.DrawInfo;

   VAR
     i: Integer;
   BEGIN
      SelectViewPort(1);
      SelectScale(1);
      SetColor(MBlack);
      HideCursor;
      PlotLine(12.1,36,52.9,36);
      PlotLine(32.5,36,32.5,3.2);
      OutTextxy(MapX(13),MapY(35),'Frame 1');
      OutTextXy(MapX(33.5),MapY(35),'Frame 2');
      OutTextxy(Mapx(13),MapY(39),ObjectName);
      SetColor(Color);
      PutPixel(Mapx(23),MapY(38),Color);
      PutPixel(Mapx(23)+1,MapY(38),Color);
      PutPixel(Mapx(23),MapY(38)+1,Color);
      PutPixel(Mapx(23)+1,MapY(38)+1,Color);
      FOR i := infomin TO 15 DO
        IF i <> 9 THEN
         BEGIN
         InfoRegions[i].Draw;
         SetTextJustify(RightText,TopText);
         SetColor(MBlack);
         OuttextXY(MapX(InfoRegions[i].x1)-2,MapY(InfoRegions[i].y2-1),
                        InfoRegions[i].Name);
         SetTextJustify(LeftText,TopText);
         END;
      ShowCursor;
   END;

PROCEDURE Tball.DrawWorldTube;

   VAR
      i : Integer;
      Brightness: Real;

   BEGIN
      SelectViewPort(GraphNum);
      SelectScale(5);
      HideCursor;
      Brightness := MinBright + BrightRange/2;
     IF GraphType = XYTGraph THEN
     FOR i := 1 TO 192 DO
      IF Random > Brightness THEN
        BEGIN
           PutPixel(Path[1].XYT.x[i],Path[1].XYT.y[i],TubeColor);
           PutPixel(Path[1].XYT.x[i]+1,Path[1].XYT.y[i],TubeColor);
           PutPixel(Path[1].XYT.x[i],Path[1].XYT.y[i]+1,TubeColor);
           PutPixel(Path[1].XYT.x[i]+1,Path[1].XYT.y[i]+1,TubeColor);
        END;
     IF GraphType = XYTPrimeGraph THEN
     FOR i := 1 TO 192 DO
      IF Random > Brightness THEN
        BEGIN
        PutPixel(PathPrime[1].XYT.x[i],PathPrime[1].XYT.y[i],TubeColor);
        PutPixel(PathPrime[1].XYT.x[i]+1,PathPrime[1].XYT.y[i],TubeColor);
        PutPixel(PathPrime[1].XYT.x[i],PathPrime[1].XYT.y[i]+1,TubeColor);
        PutPixel(PathPrime[1].XYT.x[i]+1,PathPrime[1].XYT.y[i]+1,TubeColor);
        END;
     IF GraphType = XTGraph THEN
        FOR i := 1 TO Maxt DO
          BEGIN
              IF Random > Brightness THEN
                  PutPixel(XT.x[i],192-i,TubeColor);
         END;
       IF GraphType = XTPrimeGraph THEN
         FOR i := 1 TO Maxt DO
          BEGIN
              IF Random > Brightness THEN
                  PutPixel(XTPrime.x[i],192-i,TubeColor);
         END;
      ShowCursor;
   END;

PROCEDURE TBall.DrawCurrent(t,lastt: Integer; GraphType: Integer);

   BEGIN
     CASE GraphType OF
       XTGraph:
         BEGIN
          IF lastt > 0 THEN
          PutPixel(XT.x[lastt],192-lastt,Color XOR getPixel(XT.x[lastt],192-lastt));
          PutPixel(XT.x[t],192-t,Color XOR getPixel(XT.x[t],192-t));
          IF lastt > 0 THEN
          PutPixel(XT.x[lastt]+1,192-lastt,Color XOR getPixel(XT.x[lastt]+1,192-lastt));
          PutPixel(XT.x[t]+1,192-t,Color XOR getPixel(XT.x[t]+1,192-t));
          IF lastt > 0 THEN
          PutPixel(XT.x[lastt]+1,193-lastt,Color XOR getPixel(XT.x[lastt]+1,193-lastt));
          PutPixel(XT.x[t]+1,193-t,Color XOR getPixel(XT.x[t]+1,193-t));
          IF lastt > 0 THEN
          PutPixel(XT.x[lastt],193-lastt,Color XOR getPixel(XT.x[lastt],193-lastt));
          PutPixel(XT.x[t],193-t,Color XOR getPixel(XT.x[t],193-t));
          {IF lastt > 0 THEN
          PutPixel(XT.x[lastt],192-lastt,Color XOR getPixel(XT.x[lastt],192-lastt));
          PutPixel(XT.x[t],192-t,Color XOR getPixel(XT.x[t],192-t));
          IF lastt > 0 THEN
          PutPixel(XT.x[lastt],192-lastt,Color XOR getPixel(XT.x[lastt],192-lastt));
          PutPixel(XT.x[t],192-t,Color XOR getPixel(XT.x[t],192-t));
          IF lastt > 0 THEN
          PutPixel(XT.x[lastt],192-lastt,Color XOR getPixel(XT.x[lastt],192-lastt));
          PutPixel(XT.x[t],192-t,Color XOR getPixel(XT.x[t],192-t));
           }
        END;
       XYTGraph:
        BEGIN
         IF lastt >= 0 THEN
         PutPixel(Path[1].XYT.x[lastt],Path[1].XYT.y[lastt],
            Color XOR GetPixel(Path[1].XYT.x[lastt],Path[1].XYT.y[lastt]));
         PutPixel(Path[1].XYT.x[t],Path[1].XYT.y[t],
            Color XOR GetPixel(Path[1].XYT.x[t],Path[1].XYT.y[t]));
         IF lastt >= 0 THEN
         PutPixel(Path[1].XYT.x[lastt]+1,Path[1].XYT.y[lastt],
            Color XOR GetPixel(Path[1].XYT.x[lastt]+1,Path[1].XYT.y[lastt]));
         PutPixel(Path[1].XYT.x[t]+1,Path[1].XYT.y[t],
            Color XOR GetPixel(Path[1].XYT.x[t]+1,Path[1].XYT.y[t]));
         IF lastt >= 0 THEN
         PutPixel(Path[1].XYT.x[lastt]+1,Path[1].XYT.y[lastt]+1,
            Color XOR GetPixel(Path[1].XYT.x[lastt]+1,Path[1].XYT.y[lastt]+1));
         PutPixel(Path[1].XYT.x[t]+1,Path[1].XYT.y[t]+1,
            Color XOR GetPixel(Path[1].XYT.x[t]+1,Path[1].XYT.y[t]+1));
         IF lastt >= 0 THEN
         PutPixel(Path[1].XYT.x[lastt],Path[1].XYT.y[lastt]+1,
            Color XOR GetPixel(Path[1].XYT.x[lastt],Path[1].XYT.y[lastt]+1));
         PutPixel(Path[1].XYT.x[t],Path[1].XYT.y[t]+1,
            Color XOR GetPixel(Path[1].XYT.x[t],Path[1].XYT.y[t]+1));
        END;
       XYZGraph:
        BEGIN
         IF lastt >= 0 THEN
         PutPixel(Path[1].XYZ.x[lastt],Path[1].XYZ.y[lastt],
            Color XOR GetPixel(Path[1].XYZ.x[lastt],Path[1].XYZ.y[lastt]));
         PutPixel(Path[1].XYZ.x[t],Path[1].XYZ.y[t],
            Color XOR GetPixel(Path[1].XYZ.x[t],Path[1].XYZ.y[t]));
         IF lastt >= 0 THEN
         PutPixel(Path[1].XYZ.x[lastt]+1,Path[1].XYZ.y[lastt],
            Color XOR GetPixel(Path[1].XYZ.x[lastt]+1,Path[1].XYZ.y[lastt]));
         PutPixel(Path[1].XYZ.x[t]+1,Path[1].XYZ.y[t],
            Color XOR GetPixel(Path[1].XYZ.x[t]+1,Path[1].XYZ.y[t]));
         IF lastt >= 0 THEN
         PutPixel(Path[1].XYZ.x[lastt]+1,Path[1].XYZ.y[lastt]+1,
            Color XOR GetPixel(Path[1].XYZ.x[lastt]+1,Path[1].XYZ.y[lastt]+1));
         PutPixel(Path[1].XYZ.x[t]+1,Path[1].XYZ.y[t]+1,
            Color XOR GetPixel(Path[1].XYZ.x[t]+1,Path[1].XYZ.y[t]+1));
         IF lastt >= 0 THEN
         PutPixel(Path[1].XYZ.x[lastt],Path[1].XYZ.y[lastt]+1,
            Color XOR GetPixel(Path[1].XYZ.x[lastt],Path[1].XYZ.y[lastt]+1));
         PutPixel(Path[1].XYZ.x[t],Path[1].XYZ.y[t]+1,
            Color XOR GetPixel(Path[1].XYZ.x[t],Path[1].XYZ.y[t]+1));
        END;
       XYGraph:
        BEGIN
         IF lastt >= 0 THEN
         PutPixel(Path[1].XY.x[lastt],Path[1].XY.y[lastt],
            Color XOR GetPixel(Path[1].XY.x[lastt],Path[1].XY.y[lastt]));
         PutPixel(Path[1].XY.x[t],Path[1].XY.y[t],
            Color XOR GetPixel(Path[1].XY.x[t],Path[1].XY.y[t]));
         IF lastt >= 0 THEN
         PutPixel(Path[1].XY.x[lastt]+1,Path[1].XY.y[lastt],
            Color XOR GetPixel(Path[1].XY.x[lastt]+1,Path[1].XY.y[lastt]));
         PutPixel(Path[1].XY.x[t]+1,Path[1].XY.y[t],
            Color XOR GetPixel(Path[1].XY.x[t]+1,Path[1].XY.y[t]));
         IF lastt >= 0 THEN
         PutPixel(Path[1].XY.x[lastt]+1,Path[1].XY.y[lastt]+1,
            Color XOR GetPixel(Path[1].XY.x[lastt]+1,Path[1].XY.y[lastt]+1));
         PutPixel(Path[1].XY.x[t]+1,Path[1].XY.y[t]+1,
            Color XOR GetPixel(Path[1].XY.x[t]+1,Path[1].XY.y[t]+1));
         IF lastt >= 0 THEN
         PutPixel(Path[1].XY.x[lastt],Path[1].XY.y[lastt]+1,
            Color XOR GetPixel(Path[1].XY.x[lastt],Path[1].XY.y[lastt]+1));
         PutPixel(Path[1].XY.x[t],Path[1].XY.y[t]+1,
            Color XOR GetPixel(Path[1].XY.x[t],Path[1].XY.y[t]+1));
        END;
       XTPrimeGraph:
         BEGIN
          IF lastt > 0 THEN
          PutPixel(xtprime.x[lastt],192-lastt,Color XOR getPixel(xtprime.x[lastt],192-lastt));
          PutPixel(xtprime.x[t],192-t,Color XOR getPixel(xtprime.x[t],192-t));
          IF lastt > 0 THEN
          PutPixel(xtprime.x[lastt]+1,192-lastt,Color XOR getPixel(xtprime.x[lastt]+1,192-lastt));
          PutPixel(xtprime.x[t]+1,192-t,Color XOR getPixel(xtprime.x[t]+1,192-t));
          IF lastt > 0 THEN
          PutPixel(xtprime.x[lastt]+1,193-lastt,Color XOR getPixel(xtprime.x[lastt]+1,193-lastt));
          PutPixel(xtprime.x[t]+1,193-t,Color XOR getPixel(xtprime.x[t]+1,193-t));
          IF lastt > 0 THEN
          PutPixel(xtprime.x[lastt],193-lastt,Color XOR getPixel(xtprime.x[lastt],193-lastt));
          PutPixel(xtprime.x[t],193-t,Color XOR getPixel(xtprime.x[t],193-t));
          {IF lastt > 0 THEN
          PutPixel(XT.x[lastt],192-lastt,Color XOR getPixel(XT.x[lastt],192-lastt));
          PutPixel(XT.x[t],192-t,Color XOR getPixel(XT.x[t],192-t));
          IF lastt > 0 THEN
          PutPixel(XT.x[lastt],192-lastt,Color XOR getPixel(XT.x[lastt],192-lastt));
          PutPixel(XT.x[t],192-t,Color XOR getPixel(XT.x[t],192-t));
          IF lastt > 0 THEN
          PutPixel(XT.x[lastt],192-lastt,Color XOR getPixel(XT.x[lastt],192-lastt));
          PutPixel(XT.x[t],192-t,Color XOR getPixel(XT.x[t],192-t));
           }
        END;
       XYTPrimeGraph:
        BEGIN
         IF lastt >= 0 THEN
         PutPixel(pathprime[1].XYT.x[lastt],pathprime[1].XYT.y[lastt],
            Color XOR GetPixel(pathprime[1].XYT.x[lastt],pathprime[1].XYT.y[lastt]));
         PutPixel(pathprime[1].XYT.x[t],pathprime[1].XYT.y[t],
            Color XOR GetPixel(pathprime[1].XYT.x[t],pathprime[1].XYT.y[t]));
         IF lastt >= 0 THEN
         PutPixel(pathprime[1].XYT.x[lastt]+1,pathprime[1].XYT.y[lastt],
            Color XOR GetPixel(pathprime[1].XYT.x[lastt]+1,pathprime[1].XYT.y[lastt]));
         PutPixel(pathprime[1].XYT.x[t]+1,pathprime[1].XYT.y[t],
            Color XOR GetPixel(pathprime[1].XYT.x[t]+1,pathprime[1].XYT.y[t]));
         IF lastt >= 0 THEN
         PutPixel(pathprime[1].XYT.x[lastt]+1,pathprime[1].XYT.y[lastt]+1,
            Color XOR GetPixel(pathprime[1].XYT.x[lastt]+1,pathprime[1].XYT.y[lastt]+1));
         PutPixel(pathprime[1].XYT.x[t]+1,pathprime[1].XYT.y[t]+1,
            Color XOR GetPixel(pathprime[1].XYT.x[t]+1,pathprime[1].XYT.y[t]+1));
         IF lastt >= 0 THEN
         PutPixel(pathprime[1].XYT.x[lastt],pathprime[1].XYT.y[lastt]+1,
            Color XOR GetPixel(pathprime[1].XYT.x[lastt],pathprime[1].XYT.y[lastt]+1));
         PutPixel(pathprime[1].XYT.x[t],pathprime[1].XYT.y[t]+1,
            Color XOR GetPixel(pathprime[1].XYT.x[t],pathprime[1].XYT.y[t]+1));
        END;
       XYZPrimeGraph:
        BEGIN
         IF lastt >= 0 THEN
         PutPixel(pathprime[1].XYZ.x[lastt],pathprime[1].XYZ.y[lastt],
            Color XOR GetPixel(pathprime[1].XYZ.x[lastt],pathprime[1].XYZ.y[lastt]));
         PutPixel(pathprime[1].XYZ.x[t],pathprime[1].XYZ.y[t],
            Color XOR GetPixel(pathprime[1].XYZ.x[t],pathprime[1].XYZ.y[t]));
         IF lastt >= 0 THEN
         PutPixel(pathprime[1].XYZ.x[lastt]+1,pathprime[1].XYZ.y[lastt],
            Color XOR GetPixel(pathprime[1].XYZ.x[lastt]+1,pathprime[1].XYZ.y[lastt]));
         PutPixel(pathprime[1].XYZ.x[t]+1,pathprime[1].XYZ.y[t],
            Color XOR GetPixel(pathprime[1].XYZ.x[t]+1,pathprime[1].XYZ.y[t]));
         IF lastt >= 0 THEN
         PutPixel(pathprime[1].XYZ.x[lastt]+1,pathprime[1].XYZ.y[lastt]+1,
            Color XOR GetPixel(pathprime[1].XYZ.x[lastt]+1,pathprime[1].XYZ.y[lastt]+1));
         PutPixel(pathprime[1].XYZ.x[t]+1,pathprime[1].XYZ.y[t]+1,
            Color XOR GetPixel(pathprime[1].XYZ.x[t]+1,pathprime[1].XYZ.y[t]+1));
         IF lastt >= 0 THEN
         PutPixel(pathprime[1].XYZ.x[lastt],pathprime[1].XYZ.y[lastt]+1,
            Color XOR GetPixel(pathprime[1].XYZ.x[lastt],pathprime[1].XYZ.y[lastt]+1));
         PutPixel(pathprime[1].XYZ.x[t],pathprime[1].XYZ.y[t]+1,
            Color XOR GetPixel(pathprime[1].XYZ.x[t],pathprime[1].XYZ.y[t]+1));
        END;
       XYPrimeGraph:
        BEGIN
         IF lastt >= 0 THEN
         PutPixel(pathprime[1].XY.x[lastt],pathprime[1].XY.y[lastt],
            Color XOR GetPixel(pathprime[1].XY.x[lastt],pathprime[1].XY.y[lastt]));
         PutPixel(pathprime[1].XY.x[t],pathprime[1].XY.y[t],
            Color XOR GetPixel(pathprime[1].XY.x[t],pathprime[1].XY.y[t]));
         IF lastt >= 0 THEN
         PutPixel(pathprime[1].XY.x[lastt]+1,pathprime[1].XY.y[lastt],
            Color XOR GetPixel(pathprime[1].XY.x[lastt]+1,pathprime[1].XY.y[lastt]));
         PutPixel(pathprime[1].XY.x[t]+1,pathprime[1].XY.y[t],
            Color XOR GetPixel(pathprime[1].XY.x[t]+1,pathprime[1].XY.y[t]));
         IF lastt >= 0 THEN
         PutPixel(pathprime[1].XY.x[lastt]+1,pathprime[1].XY.y[lastt]+1,
            Color XOR GetPixel(pathprime[1].XY.x[lastt]+1,pathprime[1].XY.y[lastt]+1));
         PutPixel(pathprime[1].XY.x[t]+1,pathprime[1].XY.y[t]+1,
            Color XOR GetPixel(pathprime[1].XY.x[t]+1,pathprime[1].XY.y[t]+1));
         IF lastt >= 0 THEN
         PutPixel(pathprime[1].XY.x[lastt],pathprime[1].XY.y[lastt]+1,
            Color XOR GetPixel(pathprime[1].XY.x[lastt],pathprime[1].XY.y[lastt]+1));
         PutPixel(pathprime[1].XY.x[t],pathprime[1].XY.y[t]+1,
            Color XOR GetPixel(pathprime[1].XY.x[t],pathprime[1].XY.y[t]+1));
        END;
      END;
    END;

CONSTRUCTOR TBall.Init;

   BEGIN
      ObjectName := 'Ball';
      ObjectType := 5;
      Vertices := 1;
      Color := MLightCyan+1;
      TubeColor := MCyan;
      InfoMin := 3;
      PObject.Init(Initial);
   END;

PROCEDURE TBall.DefineVertices;

   BEGIN
      OffsetX[1] := 0;
      OffsetY[1] := 0;
      OffsetZ[1] := 0;
      OffsetX[2] := 0;
      OffsetY[2] := 0;
      OffsetZ[2] := 0;
   END;


DESTRUCTOR TBall.Remove;

   BEGIN
   END;

{****************************************************************************

   TFlash Methods

****************************************************************************}

PROCEDURE TFlash.DrawCurrent(i,lastt:Integer;GraphType: Integer);

   VAR
      k,lasti,t,temp,mm,m,maxpoint: Integer;
      Trajectory : TFlashPoints1;
      Trajectory2: TFlashPoints2;
      StartPoint : Point;

   BEGIN
      IF GraphType < 5 THEN
         BEGIN
            t := i-tstart;
            lasti := lastt-tstart;
         END
       ELSE
         BEGIN
            t := i-tstartprime;
            lasti := lastt-tstartprime;
         END;
      IF t > 192 THEN t := -1;
      IF lasti > 192 THEN lasti := -1;
      HideCursor;
      CASE GraphType OF
         XYGraph: BEGIN
                    StartPoint := XY;
                    Trajectory := FlashXY.pointsPtr^;
                  END;
         XYTGraph: BEGIN
                    StartPoint := XYT;
                    Trajectory2 := FlashXYT.pointsPtr^;
                  END;
         XYZGraph: BEGIN
                    StartPoint := XYZ;
                    Trajectory := FlashXYZ.pointsPtr^;
                  END;
         XYPrimeGraph: BEGIN
                    StartPoint := XYPrime;
                    Trajectory := FlashXY.pointsPtr^;
                  END;
         XYTPrimeGraph: BEGIN
                    StartPoint := XYTPrime;
                    Trajectory2 := FlashXYT.pointsPtr^;
                  END;
         XYZPrimeGraph: BEGIN
                    StartPoint := XYZPrime;
                    Trajectory := FlashXYZ.pointsPtr^;
                  END;
      END;
      CASE GraphType OF
        XYGraph,XYZGraph,XYPrimeGraph,XYZPrimeGraph:
            FOR k := 1 TO 21 DO
               BEGIN
               IF lasti > 0 THEN
                 BEGIN
                   PutPixel(Trajectory.points[lasti,k].x+ Startpoint.x,
                          Trajectory.points[lasti,k].y+StartPoint.y,
                          Color XOR GetPixel(Trajectory.points[lasti,k].x+ StartPoint.x,
                          Trajectory.points[lasti,k].y+StartPoint.y));
                  END;
                IF t >0 THEN
                   PutPixel(Trajectory.points[t,k].x+ StartPoint.x,
                          Trajectory.points[t,k].y+StartPoint.y,
                          Color XOR GetPixel(Trajectory.points[t,k].x+ StartPoint.x,
                          Trajectory.points[t,k].y+StartPoint.y))
                   ELSE FOR mm := 1 TO 175 DO m := t;
               IF lasti > 0 THEN
                 BEGIN
                   PutPixel(-Trajectory.points[lasti,k].x+ Startpoint.x,
                          Trajectory.points[lasti,k].y+StartPoint.y,
                          Color XOR GetPixel(-Trajectory.points[lasti,k].x+ StartPoint.x,
                          Trajectory.points[lasti,k].y+StartPoint.y));
                  END;
                IF t >0 THEN
                   PutPixel(-Trajectory.points[t,k].x+ StartPoint.x,
                          Trajectory.points[t,k].y+StartPoint.y,
                          Color XOR GetPixel(-Trajectory.points[t,k].x+ StartPoint.x,
                          Trajectory.points[t,k].y+StartPoint.y))
                   ELSE FOR mm := 1 TO 175 DO m := t;
               IF lasti > 0 THEN
                 BEGIN
                   PutPixel(-Trajectory.points[lasti,k].x+ Startpoint.x,
                          -Trajectory.points[lasti,k].y+StartPoint.y,
                          Color XOR GetPixel(-Trajectory.points[lasti,k].x+ StartPoint.x,
                          -Trajectory.points[lasti,k].y+StartPoint.y));
                  END;
                IF t >0 THEN
                   PutPixel(-Trajectory.points[t,k].x+ StartPoint.x,
                          -Trajectory.points[t,k].y+StartPoint.y,
                          Color XOR GetPixel(-Trajectory.points[t,k].x+ StartPoint.x,
                          -Trajectory.points[t,k].y+StartPoint.y))
                   ELSE FOR mm := 1 TO 175 DO m := t;
               IF lasti > 0 THEN
                 BEGIN
                   PutPixel(Trajectory.points[lasti,k].x+ Startpoint.x,
                          -Trajectory.points[lasti,k].y+StartPoint.y,
                          Color XOR GetPixel(Trajectory.points[lasti,k].x+ StartPoint.x,
                          -Trajectory.points[lasti,k].y+StartPoint.y));
                  END;
                IF t >0 THEN
                   PutPixel(Trajectory.points[t,k].x+ StartPoint.x,
                          -Trajectory.points[t,k].y+StartPoint.y,
                          Color XOR GetPixel(Trajectory.points[t,k].x+ StartPoint.x,
                          -Trajectory.points[t,k].y+StartPoint.y))
                   ELSE FOR mm := 1 TO 175 DO m := t;
               END;
         XYTGraph,XYTPrimeGraph:
            FOR k := 1 TO 42 DO
               BEGIN
               IF lasti > 0 THEN
                 BEGIN
                   PutPixel(Trajectory2.points[lasti,k].x+ Startpoint.x,
                          Trajectory2.points[lasti,k].y+StartPoint.y,
                          Color XOR GetPixel(Trajectory2.points[lasti,k].x+ StartPoint.x,
                          Trajectory2.points[lasti,k].y+StartPoint.y));
                  END;
                IF t >0 THEN
                   PutPixel(Trajectory2.points[t,k].x+ StartPoint.x,
                          Trajectory2.points[t,k].y+StartPoint.y,
                          Color XOR GetPixel(Trajectory2.points[t,k].x+ StartPoint.x,
                          Trajectory2.points[t,k].y+StartPoint.y))
                   ELSE FOR mm := 1 TO 175 DO m := t;
               IF lasti > 0 THEN
                 BEGIN
                   PutPixel(-Trajectory2.points[lasti,k].x+ Startpoint.x,
                          Trajectory2.points[lasti,k].y+StartPoint.y,
                          Color XOR GetPixel(-Trajectory2.points[lasti,k].x+ StartPoint.x,
                          Trajectory2.points[lasti,k].y+StartPoint.y));
                  END;
                IF t >0 THEN
                   PutPixel(-Trajectory2.points[t,k].x+ StartPoint.x,
                          Trajectory2.points[t,k].y+StartPoint.y,
                          Color XOR GetPixel(-Trajectory2.points[t,k].x+ StartPoint.x,
                          Trajectory2.points[t,k].y+StartPoint.y))
                   ELSE FOR mm := 1 TO 175 DO m := t;
             END;
         XTGraph:
            BEGIN
               IF (lasti > 0) AND HasBeenDrawn THEN
                 BEGIN
                    PutPixel(XT.x+lasti,192-lastt,Color XOR GetPixel(XT.x+lasti,
                          192-lastt));
                    PutPixel(XT.x-lasti,192-lastt,Color XOR GetPixel(XT.x-lasti,
                          192-lastt));
                    PutPixel(XT.x+lasti+1,192-lastt,Color XOR GetPixel(XT.x+lasti+1,
                          192-lastt));
                    PutPixel(XT.x-lasti-1,192-lastt,Color XOR GetPixel(XT.x-lasti-1,
                          192-lastt));
                    PutPixel(XT.x+lasti+1,193-lastt,Color XOR GetPixel(XT.x+lasti+1,
                          193-lastt));
                    PutPixel(XT.x-lasti-1,193-lastt,Color XOR GetPixel(XT.x-lasti-1,
                          193-lastt));
                    PutPixel(XT.x+lasti,193-lastt,Color XOR GetPixel(XT.x+lasti,
                          193-lastt));
                    PutPixel(XT.x-lasti,193-lastt,Color XOR GetPixel(XT.x-lasti,
                          193-lastt));
                 END;
               IF (t > 0)  THEN
                 BEGIN
                    PutPixel(XT.x+t,192-i,Color XOR GetPixel(XT.x+t,192-i));
                    PutPixel(XT.x-t,192-i,Color XOR GetPixel(XT.x-t,192-i));
                    PutPixel(XT.x+t+1,192-i,Color XOR GetPixel(XT.x+t+1,192-i));
                    PutPixel(XT.x-t-1,192-i,Color XOR GetPixel(XT.x-t-1,192-i));
                    PutPixel(XT.x+t+1,193-i,Color XOR GetPixel(XT.x+t+1,193-i));
                    PutPixel(XT.x-t-1,193-i,Color XOR GetPixel(XT.x-t-1,193-i));
                    PutPixel(XT.x+t,193-i,Color XOR GetPixel(XT.x+t,193-i));
                    PutPixel(XT.x-t,193-i,Color XOR GetPixel(XT.x-t,193-i));
                 END;
            END;
         XTPrimeGraph:
            BEGIN
               IF (lasti > 0) AND HasBeenDrawn THEN
                 BEGIN
                    PutPixel(xtprime.x+lasti,192-lastt,Color XOR GetPixel(xtprime.x+lasti,
                          192-lastt));
                    PutPixel(xtprime.x-lasti,192-lastt,Color XOR GetPixel(xtprime.x-lasti,
                          192-lastt));
                    PutPixel(xtprime.x+lasti+1,192-lastt,Color XOR GetPixel(xtprime.x+lasti+1,
                          192-lastt));
                    PutPixel(xtprime.x-lasti-1,192-lastt,Color XOR GetPixel(xtprime.x-lasti-1,
                          192-lastt));
                    PutPixel(xtprime.x+lasti+1,193-lastt,Color XOR GetPixel(xtprime.x+lasti+1,
                          193-lastt));
                    PutPixel(xtprime.x-lasti-1,193-lastt,Color XOR GetPixel(xtprime.x-lasti-1,
                          193-lastt));
                    PutPixel(xtprime.x+lasti,193-lastt,Color XOR GetPixel(xtprime.x+lasti,
                          193-lastt));
                    PutPixel(xtprime.x-lasti,193-lastt,Color XOR GetPixel(xtprime.x-lasti,
                          193-lastt));
                 END;
               IF (t > 0)  THEN
                 BEGIN
                    PutPixel(xtprime.x+t,192-i,Color XOR GetPixel(xtprime.x+t,192-i));
                    PutPixel(xtprime.x-t,192-i,Color XOR GetPixel(xtprime.x-t,192-i));
                    PutPixel(xtprime.x+t+1,192-i,Color XOR GetPixel(xtprime.x+t+1,192-i));
                    PutPixel(xtprime.x-t-1,192-i,Color XOR GetPixel(xtprime.x-t-1,192-i));
                    PutPixel(xtprime.x+t+1,193-i,Color XOR GetPixel(xtprime.x+t+1,193-i));
                    PutPixel(xtprime.x-t-1,193-i,Color XOR GetPixel(xtprime.x-t-1,193-i));
                    PutPixel(xtprime.x+t,193-i,Color XOR GetPixel(xtprime.x+t,193-i));
                    PutPixel(xtprime.x-t,193-i,Color XOR GetPixel(xtprime.x-t,193-i));
                 END;
            END;
       END;
       ShowCursor;
       HasBeenDrawn := True;
    END;

CONSTRUCTOR TFlash.Init(Initial: InitRec);

   VAR
      xx,yy,zz,tt: Real;

   BEGIN
      GObject.Init(Initial);
      ObjectName := 'Flash';
      ObjectType := 3;
      Color := MYellow;
      Location := Initial.Pos;
      SelectViewPort(Graph1);
      SelectScale(Graph1);
      HasBeenDrawn := False;
      WITH Location DO
      BEGIN
       XT.x := MapX(x);
       XY.x := MapX(x);
       XY.y := MapY(y);
       XYT.x := MapX(x*cosRTheta+sinRTheta*y);
       XYT.y := MapY(t)+MapY((y*cosRTheta-x*sinRTheta)*tanRAlpha)-MapY(0);
       XYZ.x := MapX(x*cosRTheta + y*sinRTheta);
       XYZ.y := MapY(z*cosRAlpha-x*sinRtheta*sinRAlpha+y*cosRTheta*
                               sinRAlpha);
       tstart := tbottom-MapY(t);
       SelectViewPort(1);
       SelectScale(1);
       InfoMin := 1;
       NumberInfoRegions := 8;
       InfoRegions[1].init(19,21,27,24,'t',t,3);
       InfoRegions[2].init(19,18,27,21,'x',x,3);
       InfoRegions[3].init(19,15,27,18,'y',y,3);
       InfoRegions[4].init(19,12,27,15,'z',z,3);
       LorentzTransform(t,x,y,z,tt,xx,yy,zz,Beta);
       InfoRegions[5].init(39.5,21,47.5,24,'t',tt,3);
       InfoRegions[6].init(39.5,18,47.5,21,'x',xx,3);
       InfoRegions[7].init(39.5,15,47.5,18,'y',yy,3);
       InfoRegions[8].init(39.5,12,47.5,15,'z',zz,3);
      END;
      CurrentRegion := Infomin;
   END;

DESTRUCTOR TFlash.Done;
  VAR x: Integer;
  BEGIN
   x:=1;
  END;
PROCEDURE TFlash.Transform(Beta: TSpaceVector);

   VAR
      Gamma, Beta2, c1, Cxx, Cyy, Czz, Cxy, Cxz, Cyz: Real;
      tt, xx, yy, zz: Real;


   BEGIN
      Beta2 := Sqr(beta.x) + Sqr(Beta.y) + Sqr(Beta.z);
      Gamma := 1/Sqrt(1-Beta2);
      IF Beta2 <> 0 THEN c1 := (Gamma-1)/Beta2 ELSE c1 := 0;
      Cxx := 1 + c1*Sqr(Beta.x);
      Cyy := 1 + c1*Sqr(Beta.y);
      Czz := 1 + c1*Sqr(Beta.z);
      Cxy := c1 * Beta.x*beta.y;
      Cxz := c1 * Beta.x*beta.z;
      Cyz := c1 * Beta.y*beta.z;
      WITH Location DO
        BEGIN
          tt := Gamma*(t - Beta.x*x-Beta.y*y-Beta.z*z);
          xx := Gamma*(-Beta.x*t) + Cxx*x + Cxy*y + Cxz*z;
          yy := Gamma*(-Beta.y*t) + Cxy*x + Cyy*y + Cyz*z;
          zz := Gamma*(-Beta.z*t) + Cxz*x + Cyz*y + Czz*z;
        END;
      SelectViewPort(Graph1);
      SelectScale(Graph1);
      XTPrime.x := MapX(xx);
      XYPrime.x := MapX(xx);
      XYPrime.y := MapY(yy);
      XYTPrime.x := MapX(xx*cosRTheta+sinRTheta*yy);
      XYTPrime.y := MapY(tt)+MapY((yy*cosRTheta-xx*sinRTheta)*tanRAlpha)-MapY(0);
      XYZPrime.x := MapX(xx*cosRTheta + yy*sinRTheta);
      XYZPrime.y := MapY(zz*cosRAlpha-xx*sinRtheta*sinRAlpha+yy*cosRTheta*
                               sinRAlpha);
      tstartprime := tbottom-MapY(tt);
   END;

PROCEDURE TFlash.DrawInfo{(VAR RegionList: RegionPtrList)};

   VAR
      theta,sintheta,costheta: Real;
      i: Integer;

   BEGIN
      SelectViewPort(1);
      SelectScale(1);
      SetColor(MWhite);
      PlotLine(12,36,53,36);
      PlotLine(32.5,36,32.5,3);
      OutTextxy(MapX(13),MapY(35),'Frame 1');
      OutTextXy(MapX(33.5),MapY(35),'Frame 2');
      OutTextxy(Mapx(13),MapY(39),ObjectName);
      OutTextXY(MapX(13),MapY(32),'Location:');
      OutTextXY(MapX(33.5),MapY(32),'Location:');
      {WriteName;}
      {DrawSymbol;}
      SetColor(MYellow);
      FOR i := 1 TO 8 DO
         BEGIN
            theta := pi*i/4;
            sinTheta := sin(theta);
            costheta := cos(theta);
            PlotLine(22+0.5*costheta,38+0.5*sintheta,
                       22+1.5*costheta,38+1.5*sintheta);
         END;
      FOR i := 1 TO 8 DO
         BEGIN
         InfoRegions[i].Draw;
         SetTextJustify(RightText,TopText);
         SetColor(MBlack);
         OuttextXY(MapX(InfoRegions[i].x1)-2,MapY(InfoRegions[i].y2-1),
                        InfoRegions[i].Name);
         SetTextJustify(LeftText,TopText);
         END;

{      RegionList := ^Info;
      RegionList.Display;}
   END;

PROCEDURE TFlash.DrawWorldTube(GraphNum, GraphType: Integer);

   CONST
      BrightRange = 0.15;

   VAR
      t,k: Integer;

   BEGIN
      SelectViewPort(GraphNum);
      SelectScale(Graph1);
      SetColor(MDarkGray);
      HideCursor;
      FOR t := 1 TO 192 DO
      FOR k := 1 TO 42 DO
      CASE GraphType OF
         XYTGraph: BEGIN
                    IF Random < Brightrange THEN
                     PutPixel(FlashXYT.pointsPtr^.points[t,k].x+ XYT.x,
                          FlashXYT.pointsPtr^.points[t,k].y+XYT.y,MDarkGray);
                    IF Random < Brightrange THEN
                     PutPixel(-FlashXYT.pointsPtr^.points[t,k].x+ XYT.x,
                          FlashXYT.pointsPtr^.points[t,k].y+XYT.y,MDarkGray);
                   END;
         XYTPrimeGraph: BEGIN
                      IF Random < Brightrange THEN
                          PutPixel(FlashXYT.pointsPtr^.points[t,k].x+XYTprime.x,
                          FlashXYT.pointsPtr^.points[t,k].y+XYTprime.y,MDarkGray);
                      IF Random < Brightrange THEN
                          PutPixel(-FlashXYT.pointsPtr^.points[t,k].x+XYTprime.x,
                          FlashXYT.pointsPtr^.points[t,k].y+XYTprime.y,MDarkGray);
                     END;
      END;
      CASE GraphType OF
         XTGraph:
            BEGIN
               SetLineStyle(DottedLn,0,NormWidth);
               Line(XT.x,192-tstart,XT.x-192,-tstart);
               Line(XT.x,192-tstart,XT.x+192,-tstart);
               SetLineStyle(SolidLn,0,NormWidth);
            END;
         XTPrimeGraph:
            BEGIN
               SetLineStyle(DottedLn,0,NormWidth);
               Line(XTPrime.x,192-tstartprime,XTPrime.x-192,-tstartprime);
               Line(XTPrime.x,192-tstartprime,XTPrime.x+192,-tstartprime);
               SetLineStyle(SolidLn,0,NormWidth);
            END;
       END;
       ShowCursor;
    END;

PROCEDURE TFlash.Update(RegionChanged: Integer; VAR BadVelocity: Boolean);

   VAR
      tt,xx,yy,zz: Real;
      dummy: Boolean;

   PROCEDURE frame2changed;
      BEGIN
        InvLorentzTransform(InfoRegions[5].value,InfoRegions[6].value,
        InfoRegions[7].value,InfoRegions[8].value,Location.t,
        Location.x,Location.y,Location.z,Beta);
        InfoRegions[1].ChangeValue(Location.t);
        InfoRegions[2].ChangeValue(Location.x);
        InfoRegions[3].ChangeValue(Location.y);
        InfoRegions[4].ChangeValue(Location.z);
      END;

   BEGIN
      BadVelocity := False;
      CASE RegionChanged OF
       0:BEGIN
         WITH Location DO
          BEGIN
            LorentzTransform(t,x,y,z,tt,xx,yy,zz,Beta);
            InfoRegions[5].ChangeValue(tt);
            InfoRegions[6].ChangeValue(xx);
            InfoRegions[7].ChangeValue(yy);
            InfoRegions[8].ChangeValue(zz);
          END;
        END;
       1: BEGIN
            Location.t := InfoRegions[1].value;
            Update(0,dummy);
          END;
       2: BEGIN
            Location.x := InfoRegions[2].value;
            Update(0,dummy);
          END;
       3: BEGIN
            Location.y := InfoRegions[3].value;
            Update(0,dummy);
          END;
       4: BEGIN
            Location.z := InfoRegions[4].value;
            Update(0,dummy);
          END;
       5:BEGIN
            InvLorentzTransform(InfoRegions[5].value,InfoRegions[6].value,
            InfoRegions[7].value,InfoRegions[8].value,Location.t,
            Location.x,Location.y,Location.z,Beta);
            InfoRegions[1].ChangeValue(Location.t);
            InfoRegions[2].ChangeValue(Location.x);
            InfoRegions[3].ChangeValue(Location.y);
            InfoRegions[4].ChangeValue(Location.z);
         END;
       6: Frame2Changed;
       7: Frame2Changed;
       8: Frame2Changed;
      END;
      SelectViewPort(Graph1);
      SelectScale(Graph1);
      WITH Location DO
      BEGIN
       GInitRec.pos.x := x;
       GInitRec.pos.y := y;
       GInitRec.pos.z := z;
       GInitRec.pos.t := t;
       XT.x := MapX(x);
       XY.x := MapX(x);
       XY.y := MapY(y);
       XYT.x := MapX(x*cosRTheta+sinRTheta*y);
       XYT.y := MapY(t)+MapY((y*cosRTheta-x*sinRTheta)*tanRAlpha)-MapY(0);
       XYZ.x := MapX(x*cosRTheta + y*sinRTheta);
       XYZ.y := MapY(z*cosRAlpha-x*sinRtheta*sinRAlpha+y*cosRTheta*
                               sinRAlpha);
       tstart := tbottom-MapY(t);
       Transform(Beta);
       SelectViewPort(1);
       SelectScale(1);
      END;
   END;

{****************************************************************************

   TField Methods

****************************************************************************}
PROCEDURE TField.Init(ix1,iy1,ix2,iy2: Real;IName: String;IValue: Real;
                         IDecimals: Byte);

   BEGIN
      SelectViewPort(1);
      SelectScale(1);
      x1 := ix1;
      y1 := iy1;
      x2 := ix2;
      y2 := iy2;
      BackColor := MWhite;
      Decimals := IDecimals;
      Name := IName;
      Value := IValue;
      NumberString := NumStr(Value,5,Decimals);
      CursorPos := Length(NumberString)+1;
      ScrnReg.UpLeft.x := Mapx(x1);
      ScrnReg.UpLeft.y := Mapy(y2)+30;
      ScrnReg.LowRt.x := Mapx(x2);
      ScrnReg.LowRt.y := MapY(y1)+30;

   END;


PROCEDURE TField.Edit(IChar: Char);

   VAR
      OldNumberString: String;
      code: Integer;

   BEGIN
      CursorPos := Length(NumberString)+1;
      IF IChar IN EditChars THEN
         BEGIN
            OldNumberString := NumberString;
            IF (IChar IN Digits) AND (Length(NumberString)<MaxLength) THEN
               BEGIN
                  Insert(IChar,NumberString,CursorPos);
                  CursorPos := CursorPos + 1;
               END
            ELSE IF ((IChar = decimal) AND NOT Boolean(Pos('.',NumberString)))
                    AND NOT Boolean(Pos('e',NumberString))
                    AND (Length(NumberString)<MaxLength) THEN
               BEGIN
                  Insert(IChar,NumberString,CursorPos);
                  CursorPos := CursorPos + 1;
               END
            ELSE IF IChar = minus THEN
              BEGIN
                IF NOT Boolean(Pos('e',NumberString)) THEN
                BEGIN
                IF NumberString[1] = '-' THEN
                   BEGIN
                      Delete(NumberString,1,1);
                      CursorPos := CursorPos - 1;
                   END
                ELSE
                   BEGIN
                      IF (Length(NumberString)<MaxLength) THEN
                        BEGIN
                           NumberString := '-'+ NumberString;
                           CursorPos := CursorPos + 1;
                        END;
                   END
                END
                ELSE
                   BEGIN
                    IF NumberString[Pos('e',NumberString)+1] = '-' THEN
                      BEGIN
                         Delete(NumberString,Pos('e',NumberString)+1,1);
                         CursorPos := CursorPos-1;
                      END
                    ELSE
                       BEGIN
                          IF  (Length(NumberString)<MaxLength) THEN
                             BEGIN
                                Insert('-',NumberString,Pos('e',NumberString)+1);
                                CursorPos := CursorPos+1;
                             END;
                       END
                   END
              END
            ELSE IF ((IChar = 'E') OR (IChar = 'e')) AND NOT Boolean(Pos('E',NumberString))
                       AND NOT Boolean(Pos('e',NumberString))
                        AND (Length(NumberString)<MaxLength) THEN
                    BEGIN
                       Insert(IChar,NumberString,CursorPos);
                       CursorPos := CursorPos + 1;
                    END
            ELSE IF (IChar = RightArrow) AND (CursorPos <= Length(NumberString))
               THEN CursorPos := CursorPos + 1
            ELSE IF (IChar = LeftArrow) AND (CursorPos > 0)
               THEN CursorPos := CursorPos - 1
            ELSE IF (IChar = BackSpace) AND (CursorPos > 0) THEN
               BEGIN
                  Delete(NumberString,CursorPos-1,1);
                  CursorPos := CursorPos - 1;
               END
            ELSE IF IChar = Del THEN Delete(NumberString,CursorPos,1);
            IF ((NumberString <> '0') AND (NumberString[1] = '0') AND
                    (NumberString[2] <> '.')) THEN
               BEGIN
                  Delete(NumberString,1,1);
                  CursorPos := CursorPos - 1;
               END
            ELSE IF (Pos('-0',NumberString) = 1) AND (NumberString <> '-0') THEN
               BEGIN
                 Delete(NumberString,2,1);
                 CursorPos := CursorPos-1;
              END;
            IF NumberString = '' THEN
               BEGIN
                  NumberString := '0';
                  CursorPos := 2;
               END;
            SetColor(MYellow);
            HideCursor;
            OutTextXY(MapX(x2)-TextWidth(OldNumberString),
                   MapY(y1)-TextHeight(OldNumberString),OldNumberString);
            SetColor(MBlack);
            OutTextXY(MapX(x2)-TextWidth(NumberString),
                   MapY(y1)-TextHeight(NumberString),NumberString);
            ShowCursor;
            Val(NumberString,value,code);
        END;
   END;

PROCEDURE TField.ChangeValue(NewVal: Real);

   BEGIN
      SelectviewPort(1);
      selectScale(1);
      SetColor(BackColor);
      HideCursor;
      OutTextXY(MapX(x2)-TextWidth(NumberString),
             MapY(y1)-TextHeight(NumberString),NumberString);
      Value := NewVal;
      NumberString := MyNum2Str(value,5);
      {NumberString := NumStr(Value,5,Decimals);}
      SetColor(MBlack);
      OutTextXY(MapX(x2)-TextWidth(NumberString),
                   MapY(y1)-TextHeight(NumberString),NumberString);
      ShowCursor;
   END;

PROCEDURE TField.IncValue(increment: Real);

   BEGIN
      ChangeValue(Value + increment);
   END;

PROCEDURE TField.Draw;

   BEGIN
      HideCursor;
      SetColor(MDarkGray);
      PlotLine(x1,y1,x2,y1);
      PlotLine(x2,y1,x2,y2);
      PlotLine(x2,y2,x1,y2);
      PlotLine(x1,y2,x1,y1);
      SetFillStyle(1,MWhite);
      FloodFill(Mapx((x1+x2)/2),MapY((y1+y2)/2),MDarkGray);
      SetColor(MBlack);
      OutTextXY(MapX(x2)-TextWidth(NumberString),
                   MapY(y1)-TextHeight(NumberString),NumberString);
      ShowCursor;
   END;

PROCEDURE TField.HighLight(On: Boolean);

   BEGIN
      HideCursor;
      IF On THEN
         BEGIN
            BackColor := MYellow;
            SetColor(BackColor);
            Line(MapX(x1)+1,MapY(y1)-1,MapX(x2)-1,MapY(y1)-1);
            Line(MapX(x2)-1,MapY(y1)-1,MapX(x2)-1,MapY(y2)+1);
            Line(MapX(x2)-1,MapY(y2)+1,MapX(x1)+1,MapY(y2)+1);
            Line(MapX(x1)+1,MapY(y2)+1,MapX(x1)+1,MapY(y1)-1);
            SetFillStyle(1,MYellow);
            FloodFill(Mapx((x1+x2)/2),MapY((y1+y2)/2),MYellow);
            SetColor(MBlack);
            OutTextXY(MapX(x2)-TextWidth(NumberString),
                   MapY(y1)-TextHeight(NumberString),NumberString);
         END
      ELSE
         BEGIN
            BackColor := MWhite;
            SetColor(BackColor);
            Line(MapX(x1)+1,MapY(y1)-1,MapX(x2)-1,MapY(y1)-1);
            Line(MapX(x2)-1,MapY(y1)-1,MapX(x2)-1,MapY(y2)+1);
            Line(MapX(x2)-1,MapY(y2)+1,MapX(x1)+1,MapY(y2)+1);
            Line(MapX(x1)+1,MapY(y2)+1,MapX(x1)+1,MapY(y1)-1);
            SetFillStyle(1,MWhite);
            FloodFill(Mapx((x1+x2)/2),MapY((y1+y2)/2),MWhite);
            {SetColor(MWhite);
            PlotLine(x1,y1,x2,y1);
            PlotLine(x2,y1,x2,y2);
            PlotLine(x2,y2,x1,y2);
            PlotLine(x1,y2,x1,y1);
            SetLineStyle(DashedLn,0,NormWidth);
            SetColor(MLightGray);
            PlotLine(x1,y1,x2,y1);
            PlotLine(x2,y1,x2,y2);
            PlotLine(x2,y2,x1,y2);
            PlotLine(x1,y2,x1,y1);
            SetLineStyle(SolidLn,0,NormWidth);}
            SetColor(MBlack);
            OutTextXY(MapX(x2)-TextWidth(NumberString),
                   MapY(y1)-TextHeight(NumberString),NumberString);
         END;
      ShowCursor;
   END;

{****************************************************************************

   TFlashRecord Methods

****************************************************************************}
CONSTRUCTOR TFlashRecord1.Init;

   BEGIN
      New(PointsPtr);
   END;

CONSTRUCTOR TFlashRecord2.Init;

   BEGIN
      New(PointsPtr);
   END;

{****************************************************************************

   TPointTrajectory Methods

****************************************************************************}

PROCEDURE TPointTrajectory.Plot(Color: Integer);

   VAR
      i: Integer;

   BEGIN
      {HideCursor;
      SetColor(Color);
      FOR i := 1 TO Length-1 DO
          Line(XI[i],YI[i],XI[i+1],YI[i+1]);
      ShowCursor;}
   END;

PROCEDURE TPointTrajectory.SetUp(x,y,z: DVector);

   VAR
      i: Integer;
      c1,c2: Real;

   BEGIN
      
      c1 := cos(RTheta);
      c2 := sin(RTheta);
      SelectViewPort(Graph1);
      SelectScale(Graph1);
      FOR i := 1 TO maxt DO
         BEGIN
            XYT.x[i] := MapX(x.value(i)*cosRTheta+y.value(i)*sinRTheta);
            XYT.y[i] := tbottom-i+MapY((y.value(i)*cosRTheta-x.value(i)*sinRTheta)*sinRAlpha)-MapY(0);
            XY.x[i] := MapX(x.value(i));
            XY.y[i] := MapY(y.value(i));
            {XT.x[i] := XY.x[i];
            XT.y[i] := 171-i-MapY(0);}
            XYZ.x[i] := XYT.x[i];
            XYZ.y[i] := MapY(z.value(i)*cosRAlpha+y.value(i)*cosRTheta*sinRAlpha
                           -x.value(i)*sinRTheta*SinRAlpha);
         END;
   END;

PROCEDURE TPointTrajectory.Init(points: Integer);
   BEGIN
      {x.init(points);
      y.init(points);
      z.init(points);}
   END;

PROCEDURE TPointTrajectory.Done;
   BEGIN
      {x.free;
      y.free;
      z.free;}
   END;

PROCEDURE TPointTrajectory.Transform(Frame: ReferenceFrame);
   BEGIN
      {FOR i := 1 TO Maxt DO
         BEGIN
            dt := Gamma(t)*deltat;
            xtemp[i] := x(i*dt);
            ytemp[i] := y(i*dt);
            ztemp[i] := z(i*dt);
            x
         END;}
    END;


END.