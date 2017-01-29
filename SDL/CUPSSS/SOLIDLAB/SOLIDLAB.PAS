           (*************************************************)
           (*************************************************)
           (**                                             **)
           (**  Consortium of Upper-level Physics Software **)
           (**              ( CUPS Project )               **)
           (**      ( c) 1994 by John Wiley & Sons         **)
           (**  Compiled with Utilities Ver. 2.0 (95/10/10)**)
           (**                                             **)
           (*************************************************)
           (*************************************************)

PROGRAM SolidLab;


USES  crt, dos, graph, cupsMUPP, CUPS, CUPSpars, CUPSfunc, CUPSproc,
      CUPSgrph, CUPSgui;

CONST
   INITXSIZE = 40;   { INITXSIZE cells across }
   INITYSIZE = 1;    { INITYSIZE cells down   }
   SOLIDVIEWPORT = 1;
   CLOCKVIEWPORT = 2;
   ONEDPLOTPORT = 3;
   INITTFACTOR = 0.1;
   INITMIDDENS = 12.0;

   SOLIDX1 = 0.15;  { Solid 'Window' }
   SOLIDX2 = 0.85;
   SOLIDY1 = 0.2;
   SOLIDY2 = 0.8;

   ONEDX1 = 0.25;
   ONEDX2 = 0.80;
   ONEDY1 = 0.25;
   ONEDY2 = 0.80;

   SLIDERX = 0.05;
   SLIDERY = 0.2;
   SLIDERH = 0.7;

   CLOCKX1 = 0.90;
   CLOCKY1 = 0.20;
   CLOCKX2 = 0.98;
   CLOCKY2 = 4*0.08/3 + CLOCKY1;

   SELFPOTENTIAL = 5.0; { Later we'll implement a real SELFPOTENTIAL calc }

   MAXCELLGROUPS = 3;

TYPE
   mobilityType = (constantMobility, variableMobility);
   densityType = (fixedDensity, variableDensity);
   solidViewType = (NDensity2D, NDensity3D, PDensity2D, PDensity3D, Current2D,
                   Potential3D);
   clockViewType = (analogClockView, digitalClockView);
   currentViewType = (analogCurrentView, digitalCurrentView );
   keyboardFocusType = (sliderFocus, solidFocus, groupFocus );

   carrierType = (nType, pType);
   REALPtr = ^REAL;
   cellLinkPtr  = ^cellLink;   { atom for linked list of cells }
   cellGroupPtr = ^cellGroup; { pointer to a group of cells   }
   cellPtr      = ^cellObj; { pointer needed later... }

   RectType = RECORD
      x1, x2, y1, y2 : INTEGER;  { Useful for drawing/ checking }
   END;

   cellLink = RECORD
      whichCell : cellPtr; { pointer to cell assoc. with link }
      next      : cellLinkPtr;
   END;

   clockObj = OBJECT                { the clock }
      whichView : clockViewType;
      ticks     : LONGINT;
      center    : PointType;
      radius    : WORD;

      PROCEDURE Init;
      PROCEDURE Draw;
      PROCEDURE DrawChange;
      PROCEDURE XORTime;
   END;

   batteryObj = OBJECT              { the battery }
      slider     : TSliders;        { potential of battery }
      currGroup  : cellGroupPtr;    { which cell group are we connected to? }

      PROCEDURE Init;
      PROCEDURE Done;
   END;

   cellGroup = OBJECT   { a ( linked list ) group of cells }
      start        : cellLinkPtr;
      potential    : REAL;
      cellsInGroup : INTEGER;

      CONSTRUCTOR Init;
      DESTRUCTOR Done;
      PROCEDURE AddCell( cellToAdd : cellPtr);
      FUNCTION  InGroup( cellToCheck : cellPtr ): BOOLEAN ;
      PROCEDURE ShowMember( thisCell : cellPtr; onOff : BOOLEAN);
      PROCEDURE ShowAllMembers( onOff : BOOLEAN );
   END;


(**********************  The Cell Object *****************************)


 cellObj  = OBJECT

{
   The cell object is the heart of the solid data structure. Each cell
   has Physical properties and Graphical properties. First the physical:
}
   physicalData : RECORD
      density : ARRAY [nType..pType,0..2] OF REAL; { carrier conc}
{
   density[(n|p)Type] are arrays that represent the electron and
   hole density of the cell. The 0 and 1 elements of the
   arrays are used to keep track of the 'old' and 'new'
   values of density. Which is 'old' and which is 'new'
   is determined by the old property of solidObj. The
   '2' element of the array is used as an intermediate value in
   the ADI alogorithm.
}

      n0Density : REAL; { equilibrium electron density }
      p0Density : REAL; { equilibrium hole density }
      doping : REAL;    { doping level in cell }
      npProd : REAL;    { mass action product }
      whichDensity : densityType; { is density fixed? }

      mobility : record
      CASE whichType : mobilityType OF
         constantMobility : ( value : REAL );
         variableMobility : ( mFunc : string );
                            {expression giving mobility}
      END;

{
   If a cell has a fixed potential, it needs to have a value, and
   a 'Defining Cell'. Basically for each fixed potential, there will
   be a group of cells defined that share the same fixed potential.
   This way, when the user wants to adjust the potential on an
   'electrode', she can adjust all the cells in the group at the
   same time.
}
      potential : REAL;  { potential at cell. }

      Bz : REAL;  { Magnetic field in cell }
   END; { PhysicalData }

{
   Also, each cell has graphical properties. These are next.
   The bounding rectangle is used to tell WHERE the object is.
}
   position : PointType;
   inCellGroup : BOOLEAN; { Is it in a cell Group? }

   lastDensVal : INTEGER;  { last rendered density value }

   boundingRect : RectType;

{
 The methods for the cell object are also divided into physical
 and graphical methods. First the Physics!
}
   FUNCTION Charge( old : INTEGER) : REAL;
{
 Now the graphics.
}
   PROCEDURE Init(x, y : INTEGER; pData : cellPtr);
   PROCEDURE DrawDensity( whichType : carrierType ); { Draw this cell in 2D-Density mode }
   FUNCTION NShade(cType : carrierType) : INTEGER; { shade to render cell }
END;

(**********************************************************)

{
   The initCellObj exists to allow easy initialization of
   the cell class. It is exactly the same as the cellObj
   except that it is initialized differently. The only instantiation
   of this class is a member of the solidObject's data structure
   called 'defaultCell' use to keep the default data around.
}

   initCellObj = OBJECT (cellObj)   { sub-classs of cellObj }
      PROCEDURE Init( old : INTEGER );
                                  { just like cellObj except for init }
   END;


(**************** Cellbase stuff **************************)

{
   The following definitions are a required 'trick' for Pascal since
   it doesn't know how to do pointers well. I declare these 1..100
   so that the debugger will let me look at elements of the arrays.
}
   cellBase = ARRAY [1..100] of cellObj;
   CBasePtr = ^cellBase;
   CBaseArray = ARRAY [1..100] of CBasePtr;
   CBaseArrayPtr = ^CBaseArray;

{
   In order to allocate many cells, each of which is fairly large,
   we have to jump through hoops. A single array
   cannot exceed approx 65Kbytes. So the cells will be stored in
   arrays of arrays. Like this:

solidPtr-->  [1] --> row1Ptr -->  (cell 1,1), (cell 1,2), (cell 1,3), ...
             [2] --> row2Ptr -->  (cell 2,1), (cell 2,2), (cell 2,3), ...
             [3] ...
             .
             .
             .

   This way we can have many many cells! This has the side benefit
   that accessing an element doesn't require any arithmetic. (see
   the PosToPtr method.)

}


(**************** The Solid Object ************************)
{
   The solid object keeps track of data/methods for the entire
   solid.
}

   solidObj = OBJECT
      xCells, yCells : INTEGER; { dimension of solid, number of cells }
      pDimX, pDimY   : REAL;     { physical size of cells }
      tFactor        : REAL;     { factor for size of current time step }
      solidPtr : CBaseArrayPtr;  { Ptr to cells   }
      GLookUp :  DMatrix;        { Look up table for Geometry Matrix.
                                   This is also highly useful for
                                   calculating the potential at any
                                   time. It contains the reciprocal
                                   distance between x,y pairs on the
                                   'solid' grid. }
      geom : DMatrix;            { goemetry matrix for calc fixed pot }
      geomPvt: IDVector;         { pivot vector for Geom }

      defaultCell : initCellObj; { Cell with default assignments }
      old     : INTEGER;         { Which density element is old? }
      howToDraw : SolidViewType; { Which view type?  }
      nMin, nFactor : REAL;     { minimum n and nFactor}

      selection : RECORD  { the 'selected' cell }
         validCell : Boolean;
         reference : PointType;
         clipboard : cellObj;
      END;
      calcRunning : BOOLEAN;     { is the calculation running? }

      cellGroups  : ARRAY [1..MAXCELLGROUPS] OF cellGroupPtr;
      battery     : batteryObj;
      thePlot3D   : TPlot3D;
      thePlotDensity : DMatrix;  { DMatrix for various plots }
      theLastPlotDensity : DMatrix; { Ditto }

      FUNCTION PosToPtr( x, y : INTEGER ) : cellPtr;
      PROCEDURE Init( xSize, ySize : INTEGER );
      PROCEDURE HandleMouseClick( thisEvent : eventTp);
      PROCEDURE Draw; {Draw the entire solid in any mode }
      PROCEDURE Copy;
      PROCEDURE Paste;
      PROCEDURE SelectCell( Ix, Iy : INTEGER);
      PROCEDURE UnSelect;
      PROCEDURE Up;
      PROCEDURE Down;
      PROCEDURE Right;
      PROCEDURE Left;
      PROCEDURE GetTheDensity;
      PROCEDURE GetTheProperties;
      PROCEDURE GetTheMobility;
      PROCEDURE GetTheDoping;
      FUNCTION CellPotential( x, y : INTEGER;
               includeCellGroups : BOOLEAN ) : REAL ;
      PROCEDURE UpdatePotential;
      FUNCTION  Ex( x, y : INTEGER) : REAL;
      FUNCTION  Ey( x, y : INTEGER) : REAL;
      PROCEDURE DoADIStep(cType : carrierType);
      PROCEDURE DoDriftStep( cType : carrierType);
      PROCEDURE UpdateFixedDensities;
      PROCEDURE UpdateFixedPotentials;

      PROCEDURE SetDensityScale( cType : carrierType);
      PROCEDURE DrawChanges;
      FUNCTION  GetDensity( x, y, which : INTEGER;
                               cType : carrierType ) : REAL;
      PROCEDURE SetDensity( x, y, which : INTEGER;
                               cType : carrierType ;
                               Val : REAL);
      PROCEDURE GetNewSize( VAR newX, newY :  INTEGER);
      PROCEDURE Free;
      PROCEDURE GetTFactor;
      FUNCTION  InAnyGroup( thisCell : cellPtr ) : BOOLEAN;
      FUNCTION  NumCells : INTEGER;  { number of cells in groups }
      PROCEDURE SetUpGeom; { prepare geometry matrix }
      PROCEDURE DoGeomCol(i : INTEGER; rowCellPtr : cellPtr);
      PROCEDURE SetView( theView : SolidViewType );
      PROCEDURE Draw3DDensity( whichType : carrierType;
                               checkForChange : BOOLEAN );
      PROCEDURE DrawCurrent( checkForChange : BOOLEAN );
      PROCEDURE PlotPotential3D( checkForChange : BOOLEAN );
      FUNCTION UpdatePlotDensity( whichType : carrierType ) : BOOLEAN;
      FUNCTION PotentialToPlotDensity : BOOLEAN;
      PROCEDURE Render3DPlotDensity( theLabel : String);
      PROCEDURE DoASingleStep;      { Perform a single Step }
   END;

(**************** Variables ******************************)

{
   A few globals. (Plus those defined in the Units )
}

VAR
   myMenu   : TMenu; { the currently displayed menu }
   quit     : Boolean;
   solid    : solidObj; { the solid! }
   mes      : STRING[50];
   focus    : keyBoardFocusType;
   clock    : clockObj; { the clock }
   hotKeys  : THotKeys;
   kNum     : BYTE;  { hotkey pressed ... }
   autoHelp : BOOLEAN; { is autoHelp on? }

PROCEDURE DisplayMainScreen;  FORWARD;
procedure DrawFieldVectors (potential : DMatrix); FORWARD;
PROCEDURE myHelp (helpScr : HelpScrType; numLines : INTEGER); FORWARD;
PROCEDURE DoSlider; FORWARD;
PROCEDURE DefineCellGroup( group : BYTE); FORWARD;

(********************************************************************
  Methods of the 'Solid' object.
*********************************************************************)


PROCEDURE solidObj.Init( xSize, ySize: integer);

{
   Initialize solid to default values.
   Return failure if there is not enough memory to support
   the requested size.
}

VAR
   x, y : INTEGER;          { Position within solid }
   stillOK : BOOLEAN;       { We can continue initializing... }
   group : INTEGER;         { group index }

BEGIN

{
   Establish size of model.
}
   xCells := xSize;
   yCells := ySize;
   pDimX := xCells/(xCells*yCells);
   pDimY := yCells/(xCells*yCells);
{
   Initialize to no selection. Set up Viewport.
}
   selection.validCell := FALSE;

   DefineScale( SOLIDVIEWPORT, 0.0, xCells, 0.0, yCells);
   SelectViewPort( SOLIDVIEWPORT );
{
   Allocate memory for solid data and Initialize cells.
}
   solidPtr := NIL;                                 { Init to NIL }
   stillOK := MaxAvail > yCells*SizeOf(CBasePtr);   { enough memory? }

   IF stillOK THEN
   BEGIN
      GetMem( solidPtr, yCells*SizeOf(CBasePtr));

      stillOK := TRUE;
      FOR y := 1 TO ySize DO
      BEGIN
         solidPtr^[y] := NIL;    { Initialize to NIL }
         IF stillOK THEN
         BEGIN
            stillOK := MaxAvail > xCells*SizeOf( CellObj );
            IF stillOK THEN
               GetMem( solidPtr^[y], xCells*SizeOf(CellObj));
         END;
      END;
   END
   ELSE
      Announce('Not enough memory for this model');


   If stillOK THEN
   BEGIN
      GLookUp.Init(xCells, yCells);
      old := 0;
      defaultCell.Init( old );
      FOR x := 1 TO xCells DO
         FOR y := 1 TO yCells DO
         BEGIN
            PosToPtr(x, y)^.Init(x, y, @defaultCell);
            IF (x=1) AND (y=1) THEN
               GLookUp.Put(x,y,SELFPOTENTIAL)
            ELSE
               GLookUp.Put(x,y,1/sqrt((x-1)*(x-1) + (y-1)*(y-1)));
         END;

{
   Initialize the type of view.
}
      howToDraw := NDensity2D;
      tFactor := INITTFACTOR;
      calcRunning := FALSE;

      FOR group := 1 TO MAXCELLGROUPS DO
         cellGroups[group] := NIL;

      battery.Init;
      geom.Init(1,1);
      geomPvt.Init(1);
      thePlot3D.Init( SOLIDVIEWPORT);
      thePlotDensity.Init( xCells, yCells );
      theLastPlotDensity.Init( xCells, yCells );

   END
   ELSE
   BEGIN
      Announce('Insufficient memory for this model');

      IF solidPtr <> NIL THEN    { pointer array was allocated }
      BEGIN
        FOR y := 1 to ySize DO
           IF solidPtr^[y] <> NIL THEN
              FreeMem( solidPtr^[y], xCells*SizeOf(CellObj));
                                     { clean up the mess }

        FreeMem( solidPtr, yCells*SizeOf(CBasePtr));
      END;
      Init(1,1);
   END;
END;

PROCEDURE solidObj.Free;

VAR
   y : INTEGER ;
   group : INTEGER;

BEGIN
   IF solidPtr <> NIL THEN    { pointer array was allocated }
   BEGIN
      FOR y := 1 to yCells DO
         IF solidPtr^[y] <> NIL THEN
            FreeMem( solidPtr^[y], xCells*SizeOf(CellObj));
                                     { clean up the mess }

      FreeMem( solidPtr, yCells*SizeOf(CBasePtr));
   END;
   xCells := 0;
   yCells := 0;
   GLookUp.Free;
   battery.done;                { bye bye battery?...}
   thePlot3D.Done;
   thePlotDensity.Free;
   thelastPlotDensity.Free;
   geom.free;
   geompvt.free;
   FOR group := 1 TO MAXCELLGROUPS DO
      IF cellGroups[group] <> NIL THEN
         dispose( cellGroups[group], Done);   { group disposal }

END;

FUNCTION solidObj.PosToPtr( x, y : INTEGER) : cellPtr;

{
   Get Ptr to cell at position specified. This is required
   since PASCAL doesn't support pointer arithmetic.
}

BEGIN

   { check for bounds, this produces the desired boundary conditions.}

   IF x < 1 THEN x := 1;
   IF x > xCells THEN x := xCells;
   IF y < 1 THEN y := 1;
   IF y > yCells THEN y := yCells;

   PosToPtr := Addr(solidPtr^[y]^[x])

END;

PROCEDURE solidObj.Copy;
BEGIN
     WITH selection DO
        IF ValidCell THEN
           clipboard := PosToPtr( reference.x, reference.y )^;
END;

PROCEDURE solidObj.Paste;
BEGIN
     WITH selection DO
        IF ValidCell THEN
        BEGIN
           PosToPtr( reference.x, reference.y )^.PhysicalData :=
                                             clipboard.PhysicalData;
           case howToDraw of
                NDensity2D :
                   PosToPtr( reference.x, reference.y )^.DrawDensity(nType);
                PDensity2D :
                   PosToPtr( reference.x, reference.y )^.DrawDensity(pType);
           END;
        END;
END;


PROCEDURE solidObj.Draw;

VAR
 x, y : INTEGER;

BEGIN

   case howToDraw of
     NDensity2D:
      BEGIN
           SetDensityScale(nType);
           FOR x := 1 TO xCells DO
               FOR y := 1 TO yCells DO
                   PosToPtr(x, y)^.DrawDensity( nType );
      END;
     PDensity2D:
      BEGIN
           SetDensityScale(pType);
           FOR x := 1 TO xCells DO
               FOR y := 1 TO yCells DO
                   PosToPtr(x, y)^.DrawDensity( pType );
      END;
     NDensity3D: Draw3DDensity( nType, FALSE);
     PDensity3D: Draw3DDensity( pType, FALSE);
     Current2D: DrawCurrent( FALSE );
     Potential3D : PlotPotential3D( FALSE );
   END;

   battery.slider.DrawAll;

END;

PROCEDURE solidObj.Render3DPlotDensity( theLabel : string);

VAR
  oneDVector : DVector;
  axisDVector : DVector;
  i, color : integer;

BEGIN
     HideCursor;
     CloseViewPort( SOLIDVIEWPORT );
     if theLabel='p-Carrier Density' then Color:=lightRed;
     if theLabel='n-Carrier Density' then Color:=lightGreen;
     if theLabel='Potential' then Color:=lightBlue;

     IF (yCells = 1) THEN
     BEGIN                     { we have a one dim model }
        oneDVector.Init(xCells);
        axisDVector.Init(xCells);

        FOR i := 1 to xCells DO
        BEGIN
             oneDVector.Put(i,thePlotDensity.Value(i,1));
             axisDVector.Put(i,i);
        END;

        OpenViewPort( SOLIDVIEWPORT );
        GraphBackColor := black;
        OpenViewPort( ONEDPLOTPORT );
        GraphBackColor := DarkGray;
        SelectScale( ONEDPLOTPORT );
        autoScaling( ONEDPLOTPORT, axisDVector, oneDVector);
        PutLabel(top, thelabel);
        SetColor(color);
        PlotDVectors( axisDVector, oneDVector, 1, xCells );
        oneDVector.Free;
        axisDVector.Free;
        SelectScale( SOLIDVIEWPORT );
     END
     ELSE IF (xCells = 1) THEN
     BEGIN
        oneDVector.Init(yCells);
        axisDVector.Init(yCells);

        FOR i := 1 to yCells DO
        BEGIN
           oneDVector.Put(i,thePlotDensity.Value(1,i));
           axisDVector.Put(i,i);
        END;

        OpenViewPort( SOLIDVIEWPORT );
        GraphBackColor := black;
        OpenViewPort( ONEDPLOTPORT );
        GraphBackColor := DarkGray;
        SelectScale( ONEDPLOTPORT );
        autoScaling( ONEDPLOTPORT, axisDVector, oneDVector);
        PutLabel(top, thelabel);
        SetColor(color);
        PlotDVectors( axisDVector, oneDVector, 1, yCells );
        oneDVector.Free;
        axisDVector.Free;
        SelectScale( SOLIDVIEWPORT );
     END
     ELSE
     BEGIN
        SelectViewPort( SOLIDVIEWPORT );
        thePlot3D.SurfaceColor := color-8;
        thePlot3D.PlotMatrix( thePlotDensity, theLabel);
     END;
     theLastPlotDensity.Equate( thePlotDensity );
     ShowCursor;
END;

PROCEDURE solidObj.Draw3DDensity( whichType : carrierType;
                           checkForChange : BOOLEAN );

VAR
  changed : BOOLEAN;

BEGIN

{
   Draw the 3D plot if density.
}

   changed := UpdatePlotDensity( whichType );

   IF (checkForChange and changed) or (not checkForChange) THEN
      IF whichType = nType THEN
         Render3DPlotDensity( 'n-Carrier Density')
      ELSE
         Render3DPlotDensity( 'p-Carrier Density');

END;

PROCEDURE solidObj.DrawCurrent( checkForChange : BOOLEAN );

VAR
   changed : BOOLEAN;
   xPose : DMatrix;

BEGIN
   changed := PotentialToPlotDensity;

   if (checkForChange and changed) or (not checkForChange) THEN
   BEGIN
      SelectViewPort( SOLIDVIEWPORT );
      xPose.Init(yCells,xCells);
      thePlotDensity.Transpose(xPose);
      DrawFieldVectors(xPose);
      xPose.Free;
   END;
END;

PROCEDURE solidObj.PlotPotential3D( checkForChange : BOOLEAN );

VAR changed : BOOLEAN;
    i           : integer;

BEGIN
   changed := PotentialToPlotDensity;

   if (checkForChange and changed) or (not checkForChange) THEN
   BEGIN
      SelectViewPort( SOLIDVIEWPORT );
      Render3DPlotDensity('Potential');
   END;
END;

FUNCTION solidObj.UpdatePlotDensity( whichType : carrierType) : BOOLEAN;
VAR

 x,y : integer;
 thisDens : REAL;
 lastDens : REAL;
 foundChange : BOOLEAN;
 minRow, minCol, maxRow, maxCol : INTEGER;
 minVal, maxVal : REAL;

BEGIN

   foundChange := FALSE;
   thePlotDensity.MinMax( minRow, minCol, minVal, maxRow, maxCol, maxVal);
   for x := 1 to xCells DO
       for y := 1 to yCells DO
          BEGIN
            thisDens := GetDensity(x,y,old,whichType);
            if not foundChange THEN
            BEGIN
               lastDens := theLastPlotDensity.Value(x,y);
               foundChange := abs((thisDens-minVal)*0.05) < (abs(thisDens - lastDens));
            END;

            thePlotDensity.Put(x,y, thisDens);
          END;

   UpdatePlotDensity := foundChange;
END;

FUNCTION solidObj.PotentialToPlotDensity: BOOLEAN;
VAR

 x,y : integer;
 thisDens : REAL;
 lastDens : REAL;
 foundChange : BOOLEAN;
 minRow, minCol, maxRow, maxCol : INTEGER;
 minVal, maxVal : REAL;

BEGIN

   foundChange := FALSE;
   thePlotDensity.MinMax( minRow, minCol, minVal, maxRow, maxCol, maxVal);

   for x := 1 to xCells DO
       for y := 1 to yCells DO
          BEGIN
            thisDens := CellPotential(x,y,FALSE);
            if not foundChange THEN
            BEGIN
               lastDens := theLastPlotDensity.Value(x,y);
               foundChange := abs((thisDens-minVal)*0.05) < (abs(thisDens - lastDens));
            END;

            thePlotDensity.Put(x,y, thisDens);
          END;

   PotentialToPlotDensity := foundChange;
END;

PROCEDURE solidObj.DrawChanges;

VAR
 x, y : INTEGER;
   thisCell : cellPtr;

BEGIN
   SelectViewPort( SOLIDVIEWPORT );
   SelectScale( SOLIDVIEWPORT );

   case howToDraw of
    NDensity2D:
     BEGIN
      SetDensityScale(nType);
      FOR x := 1 TO xCells DO
          FOR y := 1 TO yCells DO
              BEGIN
                   thisCell := PosToPtr(x, y);
                   IF thisCell^.lastDensVal <> thisCell^.NShade(nType) THEN
                      thisCell^.DrawDensity(nType);
              END;
     END;
    PDensity2D:
     BEGIN
      SetDensityScale(pType);
      FOR x := 1 TO xCells DO
          FOR y := 1 TO yCells DO
              BEGIN
                   thisCell := PosToPtr(x, y);
                   IF thisCell^.lastDensVal <> thisCell^.NShade(pType) THEN
                      thisCell^.DrawDensity(pType);
              END;
     END;
    NDensity3D : Draw3DDensity( nType, TRUE );
    PDensity3D : Draw3DDensity( pType, TRUE );
    Current2D : DrawCurrent( TRUE );
    Potential3D : PlotPotential3D( TRUE );
   END;
END;

PROCEDURE solidObj.HandleMouseClick(thisEvent : eventTp);
VAR
   Rx, Ry: REAL;
   Ix, Iy : INTEGER;
   Inside : BOOLEAN;
BEGIN
     MousePosn(Rx, Ry, SOLIDVIEWPORT, SOLIDVIEWPORT, Inside);
     IF Inside THEN { Mouse is in Solid! }
     BEGIN
       focus := solidFocus;
       case howToDraw of
        NDensity3D, PDensity3D, Potential3D : thePlot3D.Check;
        NDensity2D, PDensity2D :
          BEGIN
            Ix := trunc(Rx + 1.0);
            Iy := trunc(Ry + 1.0);
            SelectCell( Ix, Iy);
               IF thisEvent.doubleClicked THEN
               BEGIN
                    GetTheProperties;
                    DisplayMainScreen;
               END;
          END;
        END;
     END
     ELSE
     BEGIN
       focus := sliderFocus;
       DoSlider;
     END;

END;

PROCEDURE solidObj.UnSelect;
BEGIN

     IF selection.ValidCell THEN
     BEGIN
          selection.ValidCell := False;
          WITH selection.reference DO
               CASE howToDraw OF
                 NDensity2D : PosToPtr(x, y)^.DrawDensity(nType);
                 PDensity2D : PosToPtr(x, y)^.DrawDensity(pType);
               END

     END;
END;

PROCEDURE solidObj.SelectCell( Ix, Iy : INTEGER);
BEGIN

     UnSelect;

     WITH selection DO
     BEGIN
          validCell := TRUE;
          reference.x := Ix;
          reference.y := Iy;
          CASE howToDraw OF
                 NDensity2D : PosToPtr(reference.x,reference.y)^.DrawDensity(nType);
                 PDensity2D : PosToPtr(reference.x,reference.y)^.DrawDensity(pType);
               END
     END;
END;

PROCEDURE solidObj.Up;
BEGIN
     IF NOT selection.ValidCell THEN
        SelectCell(1,1)
     ELSE
        WITH selection DO
             IF reference.y < yCells THEN
                SelectCell( reference.x, reference.y + 1);
END;

PROCEDURE solidObj.Down;
BEGIN
     IF NOT selection.ValidCell THEN
        SelectCell(1,1)
     ELSE
        WITH selection DO
             IF reference.y > 1 THEN
                SelectCell( reference.x, reference.y - 1);
END;

PROCEDURE solidObj.Right;
BEGIN
     IF NOT selection.ValidCell THEN
        SelectCell(1,1)
     ELSE
        WITH selection DO
             IF reference.x < xCells THEN
                SelectCell( reference.x + 1, reference.y);
END;

PROCEDURE solidObj.Left;
BEGIN
     IF NOT selection.ValidCell THEN
        SelectCell(1,1)
     ELSE
        WITH selection DO
             IF reference.x > 1 THEN
                SelectCell( reference.x - 1, reference.y);
END;

PROCEDURE solidObj.GetTheDensity;
VAR

   DensityScreen : TInputScreen;
   cellX, cellY : string;
   currCellPtr  : cellPtr;

BEGIN
{
   Get density for the selected cell.
}
   IF NOT selection.ValidCell THEN
      SelectCell(1,1);

   currCellPtr := PosToPtr( selection.reference.x, selection.reference.y);

   with DensityScreen do
   BEGIN
        cellX := Num2Str( selection.reference.x, 3);
        cellY := Num2Str( selection.reference.y, 3);

        Init;
        DefineInputPort(0.2,0.8,0.3,0.9);
        LoadLine('');
        LoadLine(' Input Density for Cell (' + cellX +
                      ' , ' + cellY + ' )');
        LoadLine('For a fixed density cell highlight the fixed');
        LoadLine('  Density'' button below. (And set the value)');
        LoadLine('  Otherwise the density is allowed to ''float.''');
        LoadLine('');
        LoadLine('Density Type');
        LoadLine('      Fixed Density  #F');
        LoadLine(' nType density ... Value:{    }');
        LoadLine(' pType density ... Value:{    }');
        LoadLine('    [  Ok  ]   [ Cancel ] ');

        WITH currCellPtr^.PhysicalData DO
        BEGIN
           SetBoolean(1, whichDensity = fixedDensity);
           SetNumber(2, density[ntype,1]);
           SetNumber(3, density[pType,1]);
           SetNumberLimits(2, 0, 1e10);
           SetNumberLimits(3, 0, 1e10);
           AcceptScreen;
           if not canceled then
           BEGIN

                IF GetBoolean(1) THEN { fixed Density}
                   whichDensity := fixedDensity
                ELSE { floating potential }
                     whichDensity := variableDensity;

                density[nType,1] := GetNumber(2);
                density[nType,0] := GetNumber(2);
                density[pType,1] := GetNumber(3);
                density[pType,0] := GetNumber(3);
           END;
        END;
        Done;
   END;

END;

PROCEDURE solidObj.GetTheDoping;
VAR

   DopingScreen : TInputScreen;
   cellX, cellY : string;

BEGIN
{
   Get doping for the selected cell.
}
   IF NOT selection.ValidCell THEN
      SelectCell(1,1);

   cellX := Num2Str( selection.reference.x, 3);
   cellY := Num2Str( selection.reference.y, 3);

   with DopingScreen DO
   BEGIN
        Init;
        DefineInputPort(0.2, 0.8, 0.3, 0.6);
        LoadLine('');
        LoadLine(' Input Doping for Cell (' + cellX +
                                          ' , ' + cellY + ' )');
        LoadLine('');
        LoadLine(' Doping Level : {     }');
        LoadLine('');
        LoadLine('     [  Ok  ]  [Cancel]');

        WITH selection DO
             WITH PosToPtr(reference.x, reference.y)^.PhysicalData DO
             BEGIN

                  SetNumber(1, doping);
                  AcceptScreen;
                  doping := GetNumber(1);
             END;
        Done;
   END;

END;

PROCEDURE solidObj.GetTheProperties;
VAR

   DensityScreen : TInputScreen;
   cellX, cellY : string;
   currCellPtr  : cellPtr;

BEGIN
{
   Get density for the selected cell.
}
   IF NOT selection.ValidCell THEN
      SelectCell(1,1);

   currCellPtr := PosToPtr( selection.reference.x, selection.reference.y);

   with DensityScreen do
   BEGIN
        cellX := Num2Str( selection.reference.x, 3);
        cellY := Num2Str( selection.reference.y, 3);

        Init;
        DefineInputPort(0.2,0.8,0.1,0.9);
        LoadLine('');
        LoadLine(' Input Properties for Cell (' + cellX +
                      ' , ' + cellY + ' )');
        LoadLine('For a fixed density cell highlight the fixed');
        LoadLine('  Density'' button below. (And set the value)');
        LoadLine('');
        LoadLine('Density Type');
        LoadLine('      Fixed Density  #F');
        LoadLine(' nType density ... Value={    }');
        LoadLine(' pType density ... Value={    }');
        LoadLine('');
        LoadLine(' Doping Level   = {      }');
        LoadLine('');
        LoadLine('    #1 Fixed Mobility ');
        LoadLine('    #1 Variable Mobility ');
        LoadLine('');
        LoadLine(' Mobility Value = {      } m^2/Vs ');
        LoadLine('');
        LoadLine('   [  Ok  ]  [ Cancel ] ');

        WITH currCellPtr^.PhysicalData DO
        BEGIN
           IF whichDensity = fixedDensity THEN
              SetBoolean(1, TRUE)
           ELSE
              SetBoolean(1, FALSE);

           SetNumber(2, density[ntype,1]);
           SetNumber(3, density[pType,1]);
           SetNumberLimits(2, 0, 1e10);
           SetNumberLimits(3, 0, 1e10);
           SetNumber(4, doping);

           IF mobility.whichType = constantMobility THEN
              SetRadioButton('1',1)
           ELSE
              SetRadioButton('1',2);

           SetNumber(7, mobility.value);


           AcceptScreen;

           if not canceled then
           BEGIN

                IF GetBoolean(1) THEN { fixed Density}
                   whichDensity := fixedDensity
                ELSE { floating potential }
                     whichDensity := variableDensity;

                density[nType,1] := GetNumber(2);
                density[nType,0] := GetNumber(2);
                density[pType,1] := GetNumber(3);
                density[pType,0] := GetNumber(3);
                doping := GetNumber(4);

                IF GetBoolean(5) THEN
                   mobility.whichType := constantMobility
                ELSE
                   mobility.whichType := variableMobility;

                mobility.value := GetNumber(7);
           END;
        END;
        Done;
   END;

END;

PROCEDURE solidObj.GetNewSize( VAR newX, newY : INTEGER);
VAR

   SizeScreen : TInputScreen;
   MemNeeded,MaxSize, x, y  : real;
   Ok : boolean;
BEGIN
{
   Get new size for the model.
}

   with SizeScreen DO
   BEGIN
        Init;
        DefineInputPort(0.25, 0.75, 0.3, 0.6);

        LoadLine('       Input New model Size');
        LoadLine('');
        LoadLine(' X-Size (number of cells) = {     }');
        LoadLine(' Y-Size (number of cells) = {     }');
        LoadLine('');
        LoadLine('       [  Ok  ]    [Cancel]');

        SetNumber(1, xCells);
        SetNumber(2, yCells);
        SetNumberLimits(1, 1, 1e5);
        SetNumberLimits(2, 1, 1e5);
        repeat
           AcceptScreen;
           x := GetNumber(1);
           y := GetNumber(2);
           MemNeeded := y*(SizeOf(CBasePtr) + x*SizeOf(CellObj))
                        + 3*x*y*SizeOf(real);
           MaxSize   := x*y*SizeOf(real);
           OK := (MemNeeded < MaxAvail) and (MaxSize < 65500);
           if not OK then
            announce('Not enought memory for the model, please try again.');
        until OK;
        newX := round(x);
        newY := round(y);
        Done;
   END;

END;

PROCEDURE solidObj.GetTFactor;
VAR

   TfactorScreen : TInputScreen;

BEGIN
{
   Get new timestep parameter.
}
   with TfactorScreen DO
   BEGIN
        Init;
        LoadLine(' Input New Time-Step Parameter');
        LoadLine('');
        LoadLine('     time-step = {    }');
        LoadLine('');
        LoadLine('     [  Ok  ]  [Cancel]');

        DefineInputPort( 0.3, 0.7, 0.3, 0.7);

        SetNumber(1, tFactor);
        SetNumberLimits(1, 1e-10, 100);
        AcceptScreen;

        tFactor := GetNumber(1);
   END;
END;


PROCEDURE solidObj.GetTheMobility;
VAR

   MobilityScreen : TInputScreen;
   cellX, cellY : string;

BEGIN
{
   Get mobility for the selected cell.
}
   IF NOT selection.ValidCell THEN
      SelectCell(1,1);

   cellX := Num2Str( selection.reference.x, 3);
   cellY := Num2Str( selection.reference.y, 3);

   
   with MobilityScreen DO
   BEGIN
        Init;
        LoadLine('');
        LoadLine(' Input Mobility for Cell (' + cellX +
                                          ' , ' + cellY + ' )');
        LoadLine('');
        LoadLine('    #1  Fixed Mobility ');
        LoadLine('    #1  Variable Mobility ');
        LoadLine('');
        LoadLine('  Mobility Value : {      } m^2/Vs ');
        LoadLine('');
        LoadLine('    [  Ok  ]   [Cancel]');

        DefineInputPort( 0.3, 0.8, 0.3, 0.7);

        WITH selection DO
             WITH PosToPtr(reference.x, reference.y)^.PhysicalData.mobility DO
             BEGIN
                  IF whichType = constantMobility THEN
                       SetRadioButton('1',1)
                  ELSE
                       SetRadioButton('1',2);

                  SetNumber(3, value);

                  AcceptScreen;

                  IF not canceled THEN
                  BEGIN
                       IF GetBoolean(1) THEN
                          whichType := constantMobility
                       ELSE
                          whichType := variableMobility;

                       value := GetNumber(3);
                  END;

             END;
   END;
END;

FUNCTION solidObj.CellPotential( x, y : INTEGER;
                                 includeCellGroups : BOOLEAN ) : REAL ;

VAR
   i,j         : INTEGER;
   potential   : REAL;
   xIndex, yIndex : INTEGER;
   recipDistance : REAL;
   thisCell, thatCell : cellPtr;

{
   The potential the ith cell is essentially:

       potential := 0.0;

       FOR i,j := 1 TO NUMCELLS DO
           IF i=j THEN
              potential := SELFPOTENTIAL
           ELSE
              potential := Sum[q(i)/r(i,j)];

   But we have to get into the cell/solid data structure to do this.
}

BEGIN
{
   first fix up boundaries.
}
     thisCell := PosToPtr( x, y);
     potential := 0.0;
        FOR i := 1 TO xCells DO
            FOR j := 1 TO yCells DO
            BEGIN
                 thatCell := PosToPtr(i,j);
                 IF includeCellGroups OR (NOT thatCell^.inCellGroup) THEN
                 BEGIN
                    xIndex := Abs(x - thatCell^.position.x) + 1;
                    yIndex := Abs(y - thatCell^.position.y) + 1;
                    { Indices into GLookUp array }

                    recipDistance := GLookUp.Value(xIndex, yIndex);
                    potential := potential + thatCell^.Charge(old)*recipDistance;
                 END;
            END;

     CellPotential := potential;
END;

PROCEDURE solidObj.UpdatePotential;

VAR
   x,y : INTEGER;
   thisCell : cellPtr;

BEGIN

     FOR x := 1 TO xCells DO
         FOR y := 1 TO yCells DO
         BEGIN
            thisCell := PosToPtr(x,y);
            thisCell^.physicalData.potential := CellPotential(x,y,
                                          (NOT thisCell^.inCellGroup));
         END;
END;
{
   Ex calculates the electric field as - dV/dx. It assumes the
   potential of all the cells is 'current'. You have to be a little
   bit careful about the edges here!
}

FUNCTION solidObj.Ex( x, y : INTEGER ) : REAL;
VAR
   thisCell, rightCell, leftCell : cellPtr;

BEGIN
     IF xCells <= 1 THEN
        Ex := 0
     ELSE IF x = 0 THEN
        BEGIN
           thisCell := PosToPtr(x, y);
           rightCell := PosToPtr( x+1, y);
           Ex := xCells*(thisCell^.physicalData.potential -
                    rightCell^.physicalData.potential)/pDimX;
        END
     ELSE IF x = xCells THEN
        BEGIN
           thisCell := PosToPtr(x, y);
           leftCell := PosToPtr(x-1, y);
           Ex := xCells*(leftCell^.physicalData.potential -
                    thisCell^.physicalData.potential)/pDimX;
        END
     ELSE
        BEGIN
           rightCell := PosToPtr(x+1, y);
           leftCell := PosToPtr(x-1, y);
           Ex := xCells*(leftCell^.physicalData.potential -
                    rightCell^.physicalData.potential)/(2.0*pDimX);
        END
END;

FUNCTION solidObj.Ey( x, y : INTEGER ) : REAL;
VAR
   thisCell, upCell, downCell : cellPtr;

BEGIN
     IF yCells <= 1 THEN
        Ey := 0
     ELSE IF y = 0 THEN
        BEGIN
           thisCell := PosToPtr(x, y);
           upCell := PosToPtr( x, y+1);
           Ey := yCells*(thisCell^.physicalData.potential -
                    upCell^.physicalData.potential)/pDimY;
        END
     ELSE IF y = yCells THEN
        BEGIN
           thisCell := PosToPtr(x, y);
           downCell := PosToPtr(x, y-1);
           Ey := yCells*(downCell^.physicalData.potential -
                    thisCell^.physicalData.potential)/pDimY;
        END
     ELSE
        BEGIN
           upCell := PosToPtr(x, y+1);
           downCell := PosToPtr(x, y-1);
           Ey := yCells*(downCell^.physicalData.potential -
                    upCell^.physicalData.potential)/(2.0*pDimY);
        END
END;

FUNCTION solidObj.GetDensity( x, y, which : INTEGER;
                              cType : carrierType ) : REAL;

{
   Get the "which'th" n type carrier density for the cell at x,y.
}
VAR
   thisCell : cellPtr;

BEGIN
     { take care of boundaries first }

     GetDensity := (PosToPtr(x,y)^.physicalData.density[cType,which]);
END;

PROCEDURE solidObj.SetDensity( x, y, which : INTEGER;
                              cType : carrierType;
                              Val: REAL);

{
   Get the "which'th" n type carrier density for the cell at x,y.
}
BEGIN
     PosToPtr(x,y)^.physicalData.density[cType,which] := Val;
END;


PROCEDURE solidObj.DoDriftStep(cType : carrierType);
{
   integrate dn/dt = mu*n* Laplacian(V) for one time step.
   For now use Euler's Algorithm. Might improve later...
}
VAR
   x,y : INTEGER;
   thisCell : cellPtr;
   laplacian : REAL;
   deltaN, deltaT, deltaX, deltaY : REAL;

   FUNCTION getPot(x,y : INTEGER) : REAL;
   BEGIN
        getPot := PosToPtr(x,y)^.physicalData.potential;
   END;

BEGIN
   deltaX := pDimX/xCells;
   deltaY := pDimY/yCells;
   deltaT := deltaX*deltaY*tFactor/100;

   FOR x := 1 TO xCells DO
      FOR y := 1 to yCells DO
      BEGIN
         thisCell := PosToPtr(x,y);
         laplacian := (getPot(x+1,y) + getPot(x-1,y) - 2.0*getPot(x,y))/
                                                     sqr(deltaX) +
                      (getPot(x,y+1) + getPot(x,y-1) - 2.0*getPot(x,y))/
                                                     sqr(deltaY);

         WITH thisCell^.physicalData DO
         BEGIN
            deltaN := density[cType,old] * mobility.value * laplacian * deltaT;

            IF cType = nType THEN
               deltaN := - deltaN;  { deal with negative charge carriers }

            density[cType,1-old] := density[cType,1-old] + deltaN;
         END;
      END;
END;

PROCEDURE solidObj.DoADIStep(cType : carrierType);

{
   Perform one step in the ADI algorithm.
}

VAR
   rho1, rho2 : REAL;    { 2(1+1/tFact) }
   D    : REAL;   {diffusion constant for solid}
   diag : REAL;   {size of diagonal element of diff. matrix }


   I,J : INTEGER; { counter for x,y position }
   gamma, rhs : DVector; { working vectors for tri-diag sol}

BEGIN
   D := 1;  { actually ought to be cell by cell D := mu*k*T/q, improve later }
   rho1 := 2.0*(1.0 + 1.0/tFactor);
   rho2 := 2.0*(1.0/tFactor - 1.0);
   rhs.Init(xCells);
   gamma.Init(xCells);
{
   first fill in rhs = rightHandSide.
}

   FOR J := 1 TO yCells DO
   BEGIN
      FOR I := 1 TO xCells DO
         rhs.VecPtr^[I] := GetDensity(I,J-1,old,cType) +
                           GetDensity(I,J+1,old,cType) +
                           rho2*GetDensity(I,J,old,cType);
{
   Now use the tridiagonal matrix LU dcomp/solution to get the
   intermediate '2' density values.
}

      diag := rho1 - 1;
      SetDensity(1,J,2,cType,rhs.VecPtr^[1]/diag);

      FOR I := 2 TO xCells - 1 DO
      BEGIN
         gamma.VecPtr^[I] := -1.0/diag;
         diag := rho1 + gamma.VecPtr^[I];
         SetDensity(I,J,2,cType,(rhs.VecPtr^[I] +
                                     GetDensity(I-1,J,2,cType))/diag);
      END;

      gamma.VecPtr^[xCells] := -1.0/diag;
      diag := rho1 + gamma.VecPtr^[xCells] - 1.0;
      SetDensity(xCells,J,2,cType,(rhs.VecPtr^[xCells] +
                                  GetDensity(xCells-1,J,2,cType))/diag);

      FOR I := xCells - 1 DOWNTO 1 DO
      BEGIN
           SetDensity(I,J,2,cType,GetDensity(I,J,2,cType) -
               gamma.VecPtr^[I+1]*GetDensity(I+1,J,2,cType));
      END;
   END;

   rhs.Free;
   gamma.Free;

   rhs.Init(yCells);
   gamma.Init(yCells);
{
   Now sweep in the other direction.
}

   FOR I := 1 TO xCells DO
   BEGIN
      FOR J := 1 TO yCells DO
         rhs.VecPtr^[J] := GetDensity(I-1,J,2,cType) +
                           GetDensity(I+1,J,2,cType) +
                           rho2*GetDensity(I,J,2,cType);
{
   Now use the tridiagonal matrix LU dcomp/solution to get the
   intermediate density values.
}

      diag := rho1 - 1;
      SetDensity(I,1,1-old,cType, rhs.VecPtr^[1]/diag);

      FOR J := 2 TO yCells DO
      BEGIN
         gamma.VecPtr^[J] := -1.0/diag;
         diag := rho1 + gamma.VecPtr^[J];
         SetDensity(I,J,1-old,cType,(rhs.VecPtr^[J] +
                GetDensity(I,J-1,1-old,cType))/diag);
      END;

      gamma.VecPtr^[yCells] := -1.0/diag;
      diag := rho1 + gamma.VecPtr^[yCells] - 1.0;
      SetDensity(I,yCells,1-old,cType,(rhs.VecPtr^[yCells] +
                         GetDensity(I,yCells-1,1-old,cType))/diag);


      FOR J := yCells - 1 DOWNTO 1 DO
      BEGIN
           SetDensity(I,J,1-old,cType,GetDensity(I,J,1-old,cType) -
               gamma.VecPtr^[J+1]*GetDensity(I,J+1,1-old,cType));
      END;
   END;

   rhs.Free;
   gamma.Free;

END;

PROCEDURE solidObj.UpdateFixedDensities;

{
   Force cells with fixed densities back to the proper density.
   This implies and injection of charge and needs to be accounted for
   in the current tracking code.
}

VAR
   x,y : INTEGER;
   thisCell : cellPtr;

BEGIN
   FOR x := 1 TO xCells DO
      FOR y := 1 TO yCells DO
      BEGIN
         WITH PosToPtr(x,y)^.physicalData DO
         IF whichDensity = fixedDensity THEN
         BEGIN
            density[nType,old] := n0Density;
            density[pType,old] := p0Density;
         END;
      END;
END;

PROCEDURE solidObj.UpdateFixedPotentials;

VAR
   deltaV         : DVector;  { changes in voltage and charge   }
   numberOfCells  : INTEGER;  { number of cells at fixed potential }
   group          : INTEGER;  { which cell group are we on now? }
   i              : INTEGER;  { index into vectors              }
   currLink       : cellLinkPtr; { pointer to current link }

{
   Force cells with fixed potentials to their proper potential
   by injecting the appropriate amount of charge.


   The idea here is to use the geometry of the solid to calculate
   the amount of charge required in each of the cells with a constant
   potential to get them back where they belong.

   Basically we have


             delta V1      sp     1/r12   1/r13   1/r14  ...     delta Q1

             delta V2      1/r21   sp     1/r23   1/r24  ...     delta Q2

             delta V3   =   .      .        .                    delta Q3
               .            .      .        .                 *
               .            .      .        .                        .
               .                                                     .
                                                                     .


   Calculating the deltaVs is straightforward. Then get the delta
   Q's by LU docomposition.


   All this book-keeping has to be done on the fly. We define
   fixed potential 'groups' and demand that all members of the group
   are kept at the same potential. This makes it easier for the
   student to adjust the potential on an 'electrode' for example
   while watching the current.

}
BEGIN

   {
     First count the number of cells in the model that are fixed
     potential
   }

   numberOfCells := NumCells;

   IF numberOfCells <> 0 THEN
   BEGIN
      deltaV.Init( numberOfCells );
      i := 0;
      FOR group := 1 TO MAXCELLGROUPS DO
      BEGIN
         IF solid.cellGroups[ group ] <> NIL THEN
            WITH solid.cellGroups[ group ]^ DO
            BEGIN                                           { do group }
               currLink := solid.cellGroups[group]^.start;
               WHILE currLink <> NIL DO
                  WITH currLink^.whichCell^ DO
                  BEGIN
                     i := i+1;
{
   All these structures are a little confusing. The group potential
   is solid.cellGroups[ group ]^.potential. The current potential
   at the cell is currLink^.whichCell^.physicalData.potential.
}
                     deltaV.Put(i, potential - physicalData.potential);
                     currLink := currLink^.next;
                  END;
            END;
      END; { FOR group }

      LUSolve( geom, deltaV, geomPvt); { solve! }
                                       { deltaV comes back as density }

      i := 0;

      FOR group := 1 TO MAXCELLGROUPS DO
      BEGIN
         IF solid.cellGroups[ group ] <> NIL THEN
            WITH solid.cellGroups[ group ]^ DO
            BEGIN                                           { do group }
               currLink := solid.cellGroups[group]^.start;
               WHILE currLink <> NIL DO
                  WITH currLink^.whichCell^ DO
                  BEGIN
                     i := i+1;
                     physicalData.density[nType,old] :=
                        physicalData.density[nType,old] - deltaV.Value(i);
                     currLink := currLink^.next;
                  END;
            END;
      END; { FOR group }
      deltaV.free;
   END;

END;

PROCEDURE solidObj.SetUpGeom;

VAR
   group     : INTEGER;
   currLink  : cellLinkPtr;
   numberOfCells : INTEGER;
   i             : INTEGER;
   RCond         : REAL;

BEGIN

   numberOfCells := numCells;

   IF numberOfCells <> 0 THEN
   BEGIN
      geom.Free;
      geomPvt.Free;
      geom.Init( numberOfCells, numberOfCells);
      geomPvt.Init( numberOfCells );

      i := 0;

      FOR group := 1 TO MAXCELLGROUPS DO
      BEGIN
         IF solid.cellGroups[ group ] <> NIL THEN
            WITH solid.cellGroups[ group ]^ DO
            BEGIN                                           { do group }
               currLink := solid.cellGroups[group]^.start;
               WHILE currLink <> NIL DO
                  WITH currLink^.whichCell^ DO
                  BEGIN
                     i := i+1;
                     DoGeomCol(i, currLink^.whichCell);
                     currLink := currLink^.next;
                  END;
            END;
      END; { FOR group }

      { geometry matrix is filled. }

      LUDecomp( geom, RCond, geomPvt);
   END;
END;

PROCEDURE solidObj.DoGeomCol(i : INTEGER; rowCellPtr : cellPtr);
{
   calculate the column of the geometry matrix.
}
VAR
   group     : INTEGER;
   currLink  : cellLinkPtr;
   j         : INTEGER;
   x1, x2, y1, y2 : INTEGER;  { positions of cells }
   xIndex, yIndex : INTEGER;

BEGIN

   j := 0;

   FOR group := 1 TO MAXCELLGROUPS DO
   BEGIN
      IF solid.cellGroups[ group ] <> NIL THEN
         WITH solid.cellGroups[ group ]^ DO
         BEGIN                                           { do group }
            currLink := solid.cellGroups[group]^.start;
            WHILE currLink <> NIL DO
               WITH currLink^.whichCell^ DO
               BEGIN
                  j := j+1;
                  x1 := position.x;
                  y1 := position.y;
                  x2 := rowCellPtr^.position.x;
                  y2 := rowCellPtr^.position.y;
                  xIndex := Abs(x1 - x2) + 1;
                  yIndex := Abs(y1 - y2) + 1;
                  geom.Put(i,j,GLookUp.Value(xIndex, yIndex));
                  currLink := currLink^.next;
               END;
         END;
   END; { FOR group }
END;

PROCEDURE solidObj.SetDensityScale( cType : carrierType);
{
   Get scale factors to shade density properly.
}

VAR
   nMax : REAL;
   I,J : INTEGER;
   thisCell : cellPtr;

BEGIN
     nMin := PosToPtr(1,1)^.physicalData.density[cType,old];
     nMax := nMin;

     FOR I := 1 TO xCells DO
         FOR J := 1 TO yCells DO
         BEGIN
             thisCell := PosToPtr(I,J);
             IF nMin > thisCell^.physicalData.density[cType,old] THEN
                nMin := thisCell^.physicalData.density[cType,old]
             ELSE IF nMax < thisCell^.physicalData.density[cType,old] THEN
                nMax := thisCell^.physicalData.density[cType,old];
         END;

     IF nMax = nMin THEN
        nFactor := 0.0
     ELSE
        nFactor := 1/(nMax - nMin);
END;

FUNCTION  solidObj.InAnyGroup( thisCell : cellPtr ) : BOOLEAN;
VAR
   group : INTEGER;
   found : BOOLEAN;

BEGIN

   found := FALSE;
   group := 1;

   WHILE (NOT found) AND (group <= MAXCELLGROUPS) DO
   BEGIN
      IF cellGroups[ group ] <> NIL THEN
         found := cellGroups[ group ]^.InGroup( thisCell);
      group := group + 1;
   END;

   InAnyGroup := found;
END;

FUNCTION  solidObj.NumCells : INTEGER;  { number of cells in groups }
VAR
   numberOfCells : INTEGER;
   group         : INTEGER;
BEGIN
   numberOfCells := 0;
   FOR group := 1 TO MAXCELLGROUPS DO
      IF solid.cellGroups[ group ] <> NIL THEN
         numberOfCells := numberOfCells +
                          solid.cellGroups[ group ]^.cellsInGroup;

   NumCells := numberOfCells;
END;

PROCEDURE solidObj.SetView( theView : SolidViewType );
BEGIN
     howToDraw := theView;
END;

PROCEDURE solidObj.DoASingleStep;
BEGIN

     DoADIStep(nType);
     DoDriftStep(nType);
     DoADIStep(pType);
     DoDriftStep(pType);
     UpdatePotential;
     old := 1 - old;            { update 'old' }
     UpdateFixedPotentials;
     UpdateFixedDensities;
     DrawChanges;               { Draw density changes }
     clock.DrawChange;
END;


(***************************************************************
  Methods of the 'cell' object
****************************************************************)

PROCEDURE initCellObj.Init( old : INTEGER );

VAR
   xScreen, yScreen : INTEGER;
BEGIN

     WITH PhysicalData DO
     BEGIN
          doping := -1.0;
          npProd := 0.1;
          IF (doping > 0.0) THEN
             BEGIN
               density[pType,0] := doping;
               density[pType,1] := doping;
               density[nType,0] := npProd / doping;
               density[nType,1] := npProd / doping;
             END
          ELSE
             BEGIN
               density[nType,0] := -doping;
               density[nType,1] := -doping;
               density[pType,0] := - npProd / doping;
               density[pType,1] := - npProd / doping;
             END;
          p0Density := density[pType, 0];
          n0Density := density[nType, 0];
          whichDensity := variableDensity;
          mobility.whichType := constantMobility;
          mobility.value := 1.0;
          potential := 0.0;
          Bz := 0.0;
     END;
END;

PROCEDURE cellObj.Init(x, y: INTEGER; pData : cellPtr);

VAR
   xScreen, yScreen : INTEGER;
BEGIN

     PhysicalData := pData^.PhysicalData; { copy data from a default cell }

     position.x := x;
     position.y := y;
     inCellGroup := FALSE;

     WITH boundingRect DO
     BEGIN
        Map( x - 1, y - 1 , xScreen, yScreen);
        x1 := xScreen;
        y1 := yScreen;
        Map( x, y, xScreen, yScreen);
        x2 := xScreen;
        y2 := yScreen;
     END;
END;

{
   Render the cell using the appropriate density.
}

PROCEDURE cellObj.DrawDensity( whichType : carrierType );
VAR
   fillColor : Word;
   densVal : INTEGER;
BEGIN
     SelectViewPort( SOLIDVIEWPORT);
     SelectScale( SOLIDVIEWPORT );
     SetWriteMode( NormalPut);

     CASE whichType OF
        nType : fillColor := green;
        pType : fillColor := red;
     END;

     HideCursor;
     IF solid.selection.validCell AND
        ( solid.selection.reference.x = position.x ) AND
        ( solid.selection.reference.y = position.y ) THEN
        BEGIN
          SetWriteMode(XORput);        { XOR mode }
          fillColor := 15;
        END;

     WITH boundingRect DO
     WITH solid DO
     BEGIN
          densVal := NShade(whichType);
          SetFillPattern(shade[densVal], fillColor);
          Bar( x1, y2, x2, y1);
          SetColor(white);
          Rectangle( x1, y2, x2, y1);
     END;
     SetWriteMode( NormalPut );
     ShowCursor;
     lastDensVal := densVal;

END;



FUNCTION cellObj.NShade(cType : carrierType) : INTEGER;

BEGIN
     WITH solid DO
        NShade := trunc((physicalData.density[cType,old] - nMin)*
                    7.0*nFactor) + 1;
END;

FUNCTION cellObj.Charge( old : INTEGER ): REAL;
BEGIN
     WITH physicalData DO
        Charge := density[pType,old] - p0Density -
                                     density[nType,old] + n0Density;
END;

(***************************************************************
            Methods of cellGroup
****************************************************************)

PROCEDURE cellGroup.AddCell( cellToAdd : cellPtr);
VAR
   newLink : cellLinkPtr;

BEGIN
   new( newLink );   { get a new link }
   newLink^.whichCell := cellToAdd;
   newLink^.next := start;
   start := newLink;
   cellsInGroup := cellsInGroup + 1;
   cellToAdd^.inCellGroup := TRUE;
END;

FUNCTION cellGroup.InGroup( cellToCheck : cellPtr ) : BOOLEAN;
VAR
   currLink : cellLinkPtr;
   found    : BOOLEAN;

BEGIN
   found := FALSE;
   currLink := start;

   WHILE ( NOT found) AND (currLink <> NIL) DO
   BEGIN
      found := currLink^.whichCell = cellToCheck; { anybody home? }
      IF NOT found THEN
         currLink := currLink^.next;
   END;
   InGroup := found;
END;

PROCEDURE cellGroup.ShowMember( thisCell : cellPtr; onOff : BOOLEAN );
BEGIN

   SelectViewPort( SOLIDVIEWPORT);
   HideCursor;

   IF onOff THEN
      SetColor( red )
   ELSE
      SetColor( white);

   WITH thisCell^.boundingRect DO
      Rectangle(x1, y2, x2, y1);

   ShowCursor;

END;

PROCEDURE cellGroup.ShowAllMembers( onOff : BOOLEAN);
VAR
   currLink : cellLinkPtr;

BEGIN
   currLink := start;

   WHILE currLink <> NIL DO
   BEGIN
      ShowMember( currLink^.whichCell, onOff );
      currLink := currLink^.next;
   END;
END;

CONSTRUCTOR cellGroup.Init;
BEGIN
   start := NIL;
   potential := 0.0;
   cellsInGroup := 0;
END;

DESTRUCTOR cellGroup.Done;
VAR
   currCellLink : cellLinkPtr;
   lastCellLink : cellLinkPtr;

BEGIN
   currCellLink := start;

   WHILE currCellLink <> NIL DO
   BEGIN
      lastCellLink := currCellLink;
      currCellLink := currCellLink^.next;
      lastCellLink^.whichCell^.inCellGroup := FALSE;
      dispose( lastCellLink );
   END;

END;

(*******************  Methods of the clock Object   *********************)

PROCEDURE clockObj.Init;
VAR
   x1, y1, x2 : INTEGER;
BEGIN
   ticks := 0;
   DefineScale( CLOCKVIEWPORT, -1.0, 1.0, -1.0, 1.0);
   SelectScale( CLOCKVIEWPORT);
   SelectViewPort( CLOCKVIEWPORT );

   Map(0, 0, x1, y1);
   Map(1, 0, x2, y1);
   center.x := x1;
   center.y := y1;
   radius  := x2 - x1;

END;

PROCEDURE clockObj.Draw;

BEGIN
   SelectViewPort( CLOCKVIEWPORT );
   ClearViewPort;
   SelectScale( CLOCKVIEWPORT );
   SetWriteMode( CopyPut);
   HideCursor;
   SetColor( white );
   Circle( center.x, center.y, radius);
   ShowCursor;
   XORTime;

END;

PROCEDURE clockObj.XORTime;
VAR
   x, y : REAL;
   ix, iy : INTEGER;

BEGIN
   SelectViewPort( CLOCKVIEWPORT );
   SelectScale( CLOCKVIEWPORT );
   SetWriteMode( XORPut );
   x := SIN(2.0*Pi*( ticks mod 10 )/10);
   y := COS(2.0*Pi*( ticks mod 10 )/10);
   Map(x, y, ix, iy );
   HideCursor;
   SetColor( white );
   Line(center.x, center.y, ix, iy);
   ShowCursor;
   SetWriteMode( CopyPut);
END;

PROCEDURE clockObj.DrawChange;
BEGIN
   XORTime;
   ticks := ticks + 1;
   XORTime;
END;



(*******************  Methods of the battery Object *********************)

PROCEDURE batteryObj.Init;
BEGIN
     currGroup := NIL;
     slider.Init;
     slider.create(1,0.0,10.0,0.0,SLIDERX,SLIDERY,SLIDERH,2,'min','max',
                            'battery',true);

END;

PROCEDURE batteryObj.Done;
BEGIN
   Slider.done;
END;

(***************************************************************
    Utility Procedures
****************************************************************)

PROCEDURE myHelp (helpScr : HelpScrType; numLines : INTEGER);
VAR
   oldPort : ViewPortType;
   oldLine : LineSettingsType;
   oldText : TextSettingsType;
   x,y,dx,dy,h,i,size,color : word;

BEGIN
   color := GetColor;
   GetViewSettings (oldPort);
   GetLineSettings (oldLine);
   GetTextSettings (oldText);
   SetFillStyle (SolidFill, Green);
   SetLineStyle (solidLn, 0,thickWidth);
   SetTextStyle (DefaultFont, HorizDir, 1);
   h := 3 * TextHeight('O') DIV 2;
   dx:= round(0.63 *GetMaxX);    dy:= numLines*h + 5;
   x := (GetMaxX-dx) div 2;      y := (GetMaxY-dy) div 2;
   SetViewPort (x, y, GetMaxX-x, GetMaxY-y, clipOff);
   HideCursor;
   Bar (0,0,dx,dy);
   rectangle (1,1,dx-1,dy-1);
   FOR i := 1 TO numlines DO
      OutTextXY (5,(i-1)*h + 5, helpScr[i]);
   ShowCursor;
   SetColor (color);
   WITH oldPort DO SetViewPort (x1,y1,x2,y2,clip);
   WITH oldLine DO SetLineStyle (LineStyle, Pattern, Thickness);
   WITH oldText DO SetTextStyle (Font, Direction, CharSize);
END;

PROCEDURE DescribeSolidLab;
CONST
   InitHelpScreen   : HelpScrType =
   ('                                                 ',
    '     Inhomogeneous Semiconducor Laboratory       ',
    '                                                 ',
    '       Written by Steve Spicklemire              ',
    '  Deparment of Physics and Earth-Space Sciences  ',
    '           University of Indianapolis,           ',
    '            Indianapolis, IN 46227               ',
    '                                                 ',
    '         Copyright (C) John Wiley & Sons   ',
    '                                                 ',
    '  This program simulates many of the physical    ',
    '  processes that occur in semiconductor devices. ',
    '  The user can ''cut'' and ''paste'' the dopants ',
    '  from one part of the device to another in      ',
    '  order to build up any arbitrary two dimensional',
    '  structure on a fixed grid of semiconductor   ',
    '  cells. Some geometries are pre-set for demos  ',
    '  and as starting points for various investiga- ',
    '  tions.   ',
    '    ',
    '    ',
    '    ',
    '    ',
    '    ',
    '    ');
BEGIN
     help(InitHelpScreen);
END;

PROCEDURE SetUpMainMenu( var aMenu : TMenu) ;
BEGIN
   WITH aMenu DO
   BEGIN
        init;
        column(1, 'File');
                  row(1,1, 'About Cups');
                  row(1,2, 'About Program');
                  row(1,3, 'Configure ');
                  row(1,4, '-------------------');
                  row(1,5, 'Exit Program  alt-x');
        column(2, 'Build');
                  row(2,1, 'Copy    ctrl-c');
                  row(2,2, 'Paste   ctrl-v');
                  row(2,3, '--------------');
                  row(2,4, 'Properties...');
                  row(2,5, 'Dimensions');
                  row(2,6, '--------------');
                  row(2,7, 'Electrode 1');
                  row(2,8, 'Electrode 2');
                  row(2,9, 'Electrode 3');
        column(3, 'Simulation');
                  row(3,1, 'Time Factor');
                  row(3,2, 'Go      alt-g');
                  row(3,3, 'Stop    alt-h');
        column(4, 'View');
                  row(4,1, 'N-Density 2-D');
                  row(4,2, 'N-Density');
                  row(4,3, 'P-Density 2-D');
                  row(4,4, 'P-Density');
                  row(4,5, 'Current');
                  row(4,6, 'Potential');

        RowChecked(4,1,TRUE);
        RowActivate(1,4,FALSE);
        RowActivate(4,5,FALSE);
        RowActivate(2,3,FALSE);
        RowActivate(2,6,FALSE);
        ColSize := 11;
   END;
END;

PROCEDURE HandleMenu( var aMenu : TMenu);

VAR
   helpText : HelpScrType;
   newX, newY : INTEGER;   { new values for model size }

BEGIN
   WITH aMenu DO
      CASE colChosen OF
      1 : { File menu }
         CASE rowChosen OF

            1: AboutCUPS;

            2:          { About SolidLab }
              BEGIN
                   DescribeSolidLab;
                   DisplayMainScreen;
              END;

            3: Configuration;

            5 : quit := true;

         END; { CASE rowChosen }

      2 : { Build Menu }
	BEGIN
         CASE rowChosen OF
            1 : solid.Copy;
            2 : solid.Paste;
            4 : solid.GetTheProperties;

            5: BEGIN
                  solid.GetNewSize( newX, newY );
                  solid.Free;
                  solid.Init( newX, newY);
                  WITH myMenu DO
                  BEGIN
                       IF (newX = 1) or (newY = 1) THEN
                       BEGIN
                          RowActivate(4,5,FALSE);
                          row(4,2,'N-Density');
                          row(4,4,'P-Density');
                       END
                       ELSE
                       BEGIN
                           RowActivate(4,5,TRUE);
                           row(4,2,'N-Density 3-D');
                           row(4,4,'P-Density 3-D');
                       END;

                       RowChecked(4,1,TRUE);
                       RowChecked(4,2,FALSE);
                       RowChecked(4,3,FALSE);
                       RowChecked(4,4,FALSE);
                       RowChecked(4,5,FALSE);
                       RowChecked(4,6,FALSE);
                  END;

                  DisplayMainScreen;
               END;

            7: DefineCellGroup( 1 );
            8: DefineCellGroup( 2 );
            9: DefineCellGroup( 3 );
         END; { CASE rowChosen }
         DisplayMainScreen;
	END;
      3 : { simulation menu }
         CASE rowChosen OF
            1: solid.GetTFactor;

            2:
              BEGIN
                   solid.calcRunning := TRUE;
                   solid.UnSelect;
              END;

            3: solid.calcRunning := FALSE;


         END;
      4 : { View Menu }
        BEGIN
            WITH myMenu DO
            BEGIN
                 RowChecked(4,1,FALSE);
                 RowChecked(4,2,FALSE);
                 RowChecked(4,3,FALSE);
                 RowChecked(4,4,FALSE);
                 RowChecked(4,5,FALSE);
                 RowChecked(4,6,FALSE);
                 RowChecked(4,rowChosen,TRUE);
            END;

            CASE rowChosen OF
               1: solid.SetView( NDensity2D );
               2: solid.SetView( NDensity3D );
               3: solid.SetView( PDensity2D );
               4: solid.SetView( PDensity3D );
               5: solid.SetView( Current2D );
               6: solid.SetView( Potential3D );
            END;

            CASE rowChosen OF
               1,3 : BEGIN
                        RowActivate(2,5,TRUE);
                        RowActivate(2,6,TRUE);
                        RowActivate(2,7,TRUE);
                     END;
               2,4,5,6 : BEGIN
                        RowActivate(2,5,FALSE);
                        RowActivate(2,6,FALSE);
                        RowActivate(2,7,FALSE);
                     END;

            END;
            DisplayMainScreen;
         END;
      END;
END;


PROCEDURE DoSlider;
BEGIN
   WITH solid DO
      IF battery.slider.changed THEN
         IF battery.currGroup <> NIL THEN
            battery.currGroup^.potential := battery.slider.value(1);

END;


PROCEDURE HandleKeybd( thisEvent : eventTp );

CONST

      (* Numeric keypad *)
      extUP    = 72;  extDOWN  = 80;  extLEFT  = 75;  extRIGHT = 77;
      extPGUP  = 73;  extPGDN  = 81;  extHOME  = 71;  extEND   = 79;
      extINS   = 82;  extDEL   = 83;

      extCTRL_PRTSC = 114;
      extCTRL_LEFT  = 115; extCTRL_RIGHT = 116;
      extCTRL_PGUP  = 132; extCTRL_PGDN  = 118;
      extCTRL_HOME  = 119; extCTRL_END  = 117;

      NullKey = 3;  extSH_Tab = 15;

      (* ALT+letter keys *)
      extALT_A = 30; extALT_B = 48; extALT_C = 46; extALT_D = 32;
      extALT_E = 18; extALT_F = 33; extALT_G = 34; extALT_H = 35;
      extALT_I = 23; extALT_J = 36; extALT_K = 37; extALT_L = 38;
      extALT_M = 50; extALT_N = 49; extALT_O = 24; extALT_P = 25;
      extALT_Q = 16; extALT_R = 19; extALT_S = 31; extALT_T = 20;
      extALT_U = 22; extALT_V = 47; extALT_W = 17; extALT_X = 45;
      extALT_Y = 21; extALT_Z = 44;

      (* extALT+number keys *)
      extALT_1 = 120; extALT_2 = 121; extALT_3 = 122;
      extALT_4 = 123; extALT_5 = 124; extALT_6 = 125;
      extALT_7 = 126; extALT_8 = 127; extALT_9 = 128;
      extALT_0 = 129;

      extALT_minus = 130;  extALT_equals = 131;
      CtrlZ = 26; CtrlX = 24; CtrlC = 03; CtrlV = 22; CtrlA = 1;

BEGIN
   IF thisEvent.extendedKey THEN
      CASE thisEvent.readKey OF
            chr(extALT_Q) : quit := true;
            chr(extALT_X) : quit := true;
            chr(extALT_G) :
               BEGIN
                    solid.calcRunning := TRUE;
                    solid.UnSelect;
               END;
            chr(extALT_H) : solid.calcRunning := FALSE;

  END
  ELSE
     CASE thisEvent.readKey OF
                   chr(9) : BEGIN
                               IF focus = solidFocus THEN
                                  focus := sliderFocus
                               ELSE IF focus = sliderFocus THEN
                                  focus := solidFocus;
                            END;
     END;

  IF (focus = solidFocus) then
   if thisEvent.extendedKey THEN
     CASE thisEvent.readKey OF
            chr(extUP)    : solid.Up;
            chr(extDOWN)  : solid.Down;
            chr(extLEFT)  : solid.Left;
            chr(extRIGHT) : solid.Right;
     END
   else
   if thisEvent.KeyPressed then
     CASE thisEvent.readKey OF
            chr(CtrlC) : solid.Copy;
            chr(CtrlV) : solid.Paste;
     END

  ELSE IF focus = sliderFocus THEN
     DoSlider;
        { Nothing };

END;

PROCEDURE SelectCellGroup( group : BYTE );
VAR
   groupString : STRING[02];
   help        : HelpScrType;

BEGIN

   WITH solid DO
   BEGIN
      IF (battery.currGroup <> NIL) AND
         ((solid.howToDraw = NDensity2D) OR
          (solid.howToDraw = PDensity2D)) THEN
         battery.currGroup^.ShowAllMembers( FALSE );

      battery.currGroup := cellGroups[ group ];

      IF cellGroups[ group ] <> NIL THEN
      BEGIN
         IF ((solid.howToDraw = NDensity2D) OR
          (solid.howToDraw = PDensity2D)) THEN
             cellGroups[ group ]^.ShowAllMembers( TRUE );

         battery.slider.delete(1);
         battery.slider.create(1,
                                 0.0,10.0,
                                 cellGroups[ group ]^.potential,
                                 SLIDERX,SLIDERY,SLIDERH,2,'min','max',
                                 'battery',true);
         battery.slider.DrawAll;
      END;

      selection.validCell := FALSE;
   END;

END;

procedure DrawFieldVectors (potential : DMatrix);
{ Draws field vectors in the current view-port. stand-alone }
VAR
   i,j,MaxCol,MaxRow,pos,maxPos,color : integer;
   maxE,E,Ex,Ey,disX,disY     : real;
begin
   color := GetColor;
   HideCursor; ClearViewPort; ShowCursor;
   potential.GetSize(MaxRow,MaxCol);
   with views[viewPortNumber] do
   begin
      disX := (vx2-vx1) / (maxCol-1);    {horiz. grid spacing}
      disY := (vy2-vy1) / (maxRow-1);    {vert.  grid spacing}
   end;
   maxE :=0;
   with potential do
   FOR i := 2 TO maxCol-1 DO             {find max E on the grid}
   FOR j := 2 TO maxRow-1 DO
    begin
       pos := (j-1)*maxCol+i;
       E := sqr(MatPtr^[pos-maxCol]-matPtr^[pos+maxCol]) +
          + sqr(MatPtr^[pos-1]-matPtr^[pos+1])+0.0001;
       if E>maxE then maxE:=e;
    END;
   maxE:=sqrt(maxE);
   with potential do
   FOR i := 2 TO maxCol-1 DO
    FOR j := 2 TO maxRow-1 DO
     begin
        pos := (j-1)*maxCol+i;
        Ex := MatPtr^[pos-1] - MatPtr^[pos+1];
        Ey := MatPtr^[pos-maxCol] - MatPtr^[pos+maxCol];
        E  := sqrt(sqr(Ex)+sqr(Ey))+0.00001;
        SetColor(rainbow[1].colors[trunc(11*E/maxE)+1]);
        E  := 2*E;
        vector(round(disX*(i-1-Ex/E)), round(disY*(j-1-Ey/E)),
               round(disX*(i-1+Ex/E)), round(disY*(j-1+Ey/E)));
     END;
   SetColor(color);
end;

PROCEDURE DefineCellGroup( group : BYTE);
VAR
    help          : HelpScrType;
    groupString   : STRING[2];
    escaped       : BOOLEAN;
    Ix, Iy        : INTEGER;
    Rx, Ry        : REAL;
    currCell      : CellPtr;
    Inside        : BOOLEAN;

BEGIN

     str(group:2, groupString);
     help[01] := '';
     help[02] := '   Click on all the cells you want in';
     help[03] := '   the cell group for electrode' + groupString + '.';
     help[04] := '   Hit ''escape'' end cell group definition.';
     help[05] := '';
     ClearMKBuffers;
     myHelp( help, 5);
     StaticPause;
     DisplayMainScreen;
     CheckForEvents;

     escaped := FALSE;

     IF solid.CellGroups[ group ] <> NIL THEN
     BEGIN
          Dispose( solid.CellGroups[ group ] , Done); { out w/the old }
          solid.CellGroups[ group ] := NIL;           { be safe }
     END;

     New( solid.CellGroups[ group ] , Init );       { in w/the new! }

     Message(' Group '+ groupString+ ' Definition Mode (Escape to exit)');

     WITH solid.CellGroups[ group ]^ DO
     WHILE NOT escaped DO
       BEGIN
           CheckForEvents;
           IF event.keypressed THEN
              escaped := event.readKey = chr(27)
           ELSE if event.mouseClicked THEN
           BEGIN

              MousePosn(Rx, Ry, SOLIDVIEWPORT, SOLIDVIEWPORT, Inside);
              IF Inside THEN { Mouse is in Solid! }
              BEGIN
                 Ix := trunc(Rx + 1.0);
                 Iy := trunc(Ry + 1.0);
                 currCell := solid.PosToPtr( Ix, Iy);
                 IF NOT solid.InAnyGroup( currCell ) THEN
                 BEGIN
                    AddCell( currCell );
                    ShowMember( currCell, TRUE );
                 END;
              END;
           END;
        END;
     solid.SetUpGeom;
     DisplayMainScreen;
     SelectCellGroup( group );
END;


PROCEDURE HandleHotKey( kNum : BYTE; thisEvent : eventTp );
BEGIN
    CASE kNum OF
     6:
       IF myMenu.Chosen THEN
          HandleMenu( myMenu );
     1,2,3:
       IF thisEvent.doubleClicked THEN
          DefineCellGroup( kNum )
       ELSE
          SelectCellGroup( kNum );
     5: IF solid.CalcRunning THEN
               solid.CalcRunning := FALSE
           ELSE
           BEGIN
                solid.CalcRunning := TRUE;
                solid.Unselect;
           END;
     4: BEGIN
           solid.CalcRunning := FALSE;  { Stop the clock }
           solid.DoASingleStep;
        END;
    END;
END;

PROCEDURE DisplayMainScreen;
BEGIN
   HideCursor;
   SelectMuppetPort;
   ClearMuppetPort;
   myMenu.Display;
   solid.Draw;
   clock.Draw;
   hotKeys.Display;
   ShowCursor;
END;


PROCEDURE Initialize;

BEGIN

   quit := false;          { Don't quit yet. }
   focus := solidFocus;    { init focus on solid }
   HaltIfError := true ;   { stop if error }
   ErrorFound := false;    { no errors yet }

   DefineViewPort( SOLIDVIEWPORT, SOLIDX1, SOLIDX2, SOLIDY1, SOLIDY2);
   DefineViewPort( CLOCKVIEWPORT, CLOCKX1, CLOCKX2, CLOCKY1, CLOCKY2);
   DefineViewPort( ONEDPLOTPORT, ONEDX1, ONEDX2, ONEDY1, ONEDY2 );

   solid.Init(INITXSIZE, INITYSIZE);
   solid.PosToPtr(1,1)^.physicalData.whichDensity := fixedDensity;
   solid.PosToPtr(INITXSIZE,1)^.physicalData.whichDensity := fixedDensity;
   solid.PosToPtr(INITXSIZE div 2,1)^.physicalData.density[nType,0] := INITMIDDENS;
   solid.PosToPtr(INITXSIZE div 2,1)^.physicalData.density[nType,1] := INITMIDDENS;
   clock.Init;

   hotKeys.Init(6);
   WITH hotKeys DO
   BEGIN
      key[1] := 'F2-GRP1';
      key[2] := 'F3-GRP2';
      key[3] := 'F4-GRP3';
      key[4] := 'F5-STEP';
      key[5] := 'F6-RUN/S';
      key[6] := 'F10-Menu';
   END;

   SetUpMainMenu( myMenu);
   DisplayMainScreen;
   DescribeSolidLab;
   autoHelp := TRUE;


END;


BEGIN
   CUPSinit;
   Initialize;
   WHILE NOT quit DO
   BEGIN
      CheckForEvents;

      IF event.keypressed THEN            { keyboard .... }
         IF hotKeys.Pressed( kNum ) THEN
            HandleHotKey( kNum, event )
         ELSE
            HandleKeybd( event )

      ELSE IF MyMenu.Activated THEN       { menu ... }
            HandleMenu( myMenu )

      ELSE IF event.mouseClicked THEN     { mouse ... }
      BEGIN
            solid.HandleMouseClick( event );
            IF hotKeys.Pressed( kNum ) THEN
               HandleHotKey( kNum, event );
      END;

      if focus <> solidFocus THEN
         DoSlider;

      IF solid.CalcRunning THEN
         solid.DoASingleStep;

   END;
   mymenu.done;
   solid.free;
   CUPSdone;
END.
