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

PROGRAM LCAOWorkbench;
{
   LCAO Workbench allows the user to interactively adjust
   the parameters of some 'atoms' and directly see the
   resulting effects on the molecular orbitals calculated
   using the Linear Combination of Atomic Orbitals (LCAO)
   approach.
}

USES  crt, dos, graph, CupsMUPP, CUPSfunc, CUPS, CUPSpars, CUPSproc,
      CUPSgrph, CUPSgui;

CONST
   ATOMVIEWPORT = 1;
   ATOMSX1 = 0.10;  { Atom 'Window' }
   ATOMSX2 = 0.40;
   ATOMSY1 = 0.15;
   ATOMSY2 = 0.85;

   ENERGYVIEWPORT = 2;
   ENERGYX1 = 0.55;
   ENERGYX2 = 0.90;
   ENERGYY1 = 0.15;
   ENERGYY2 = 0.85;

   DEFAULTRADIUS = 2;     (* Default atomic 'dimensions' *)
   DEFAULTDEPTH = 1;

   XSELECTBOX = 6;
   YSELECTBOX = 6;

   MAXNATOMIC = 2;      { maximum allowed atomic principal quantum number }
                        { number of atomic states = (MAXNATOMIC+1)^2      }
   MINRADIUS = 0.5000;
   MINDEPTH = 0.1000;
   DELTAPOS = 0.01;

TYPE
   REALPtr = ^REAL;

(**********************  The Atom Object *****************************)

   realPointType = RECORD
         x , y : REAL;        { coordinates of the point }
      END;

   atomicStatePtr = ^atomicState;

   atomicState = RECORD
         energy      : REAL;  { energy of this atomic state       }
         m           : INTEGER; { angular momentum q.n.           }
         A, B        : REAL;  { coefficients of the wave function }
         alpha, beta : REAL;  { wave numbers                      }
         nextState   : atomicStatePtr;
      END;

 atomPtr     = ^atomObj; { pointer needed later... }

 atomObj  = OBJECT
{   Atoms are very simple in this model of a 2-D solid. They
   cylindrical 'Square Wells' with a radius and a depth.
   This keeps the Quantum Mechanics of these atoms very simple
   and the whole problem tractable.}
      radius : REAL;    { The radius of the atom }
      depth  : REAL;    { The depth of the atom }
      states : atomicStatePtr;  { Pointer to wave function information }
      position     : RealPointType;   { position of atom }
      numElectrons : INTEGER; { number of electrons in the atom }
      maxN         : INTEGER; { maximum principle quantum number (q.n.) }
      mCurr        : INTEGER; { current angular momentum q.n.   }
      nextAtom     : atomPtr; { next atom in list }
{  Graphical Parameters }
      drawingMode : INTEGER;        { current drawing mode }
      color       : INTEGER;        { current color        }
      bSelected   : BOOLEAN;        { is atom selected ?   }
      sizeB1, sizeB2 : PointType;   { position of size box }
{   Creation and destruction}
      CONSTRUCTOR Init( r, V0, x, y: REAL );
      DESTRUCTOR  Done;
{   Physics}
      FUNCTION DiffSlope( currEnergy : REAL ) : REAL;
      PROCEDURE FindEnergy;
      PROCEDURE Normalize;
      FUNCTION WaveFunc( r : REAL ) : REAL;
      PROCEDURE ScaleWaveFunc;
      PROCEDURE DrawWaveFunc( rescale : BOOLEAN );
      FUNCTION  MatrixElement( otherAtom : atomPtr ): REAL;
{   Graphics stuff.}
      PROCEDURE draw;
      PROCEDURE MoveAtom( newX, newY : REAL );
      FUNCTION ClickInAtom( x, y : REAL ) : BOOLEAN;
 END; { atomObj }

(**********************  The Model Object *****************************)

   calcModeType = ( clusterMode, solidMode );
   energyViewType = ( atomicWaveFuncView, clusterEnergyView  );
   atomViewType   = ( noWaveFuncs, WaveFuncs, clusterFuncView );

   clusterStatesType = RECORD
      energies     : DVector;
      stateVectors : DMatrix;
      whichState   : INTEGER;    { which state to display }
   END;

   modelObj  = OBJECT
{   The model is the set of all the atoms in the project, and the
   'mode' of the simulation (e.g., solidMode, clusterMode etc.).}
      atomList : atomPtr;             { Linked list of all the atoms in
                                                           the model }
      clipBoard : atomPtr;            { Atom last 'Cut' or 'Copied' }
      selection : atomPtr;
      calcMode : calcModeType;        { current Mode of calculation }
      mouseOffset : RealPointType;    { Amount to offset mouse for selection}
      xSize, ySize : REAL;            { Size of model View. }
      clusterStates: clusterStatesType; { state of cluster }
      energyView   : energyViewType;  { Which type of energy view? }
      atomView     : atomViewType;    { Which type of atom view    }
      configChanged : BOOLEAN;        { has the atomic config changed? }
      PROCEDURE Init;
      PROCEDURE Draw;
      PROCEDURE HandleMouseClick( event : eventTp);
      PROCEDURE AddAtom( x, y : REAL );
      PROCEDURE CleanUp;
      FUNCTION  InAtom( x, y : REAL ) : atomPtr;
      PROCEDURE SelectAtom( thisAtom : atomPtr);
      PROCEDURE UnselectAtom;
      PROCEDURE Cut;
      PROCEDURE DragSelection;
      PROCEDURE MoveSelection( dx, dy : REAL );
      FUNCTION  OnSizeBox( Rx, Ry : REAL ) : BOOLEAN;
      PROCEDURE SizeSelection;
      PROCEDURE GetSelectionData;
      PROCEDURE DrawAtomAxis;
      PROCEDURE AdjustYSize;
      PROCEDURE SelectSomething;
      PROCEDURE SelectNextAtom;
      PROCEDURE SetClipboardData( dataPtr : atomPtr);
      PROCEDURE Copy;
      PROCEDURE Paste;
      PROCEDURE Clear;
      FUNCTION  CountAtoms : INTEGER;
      PROCEDURE CreateHamiltonian( ham : DMatrix );
      PROCEDURE CalcClusterState;
      PROCEDURE DrawEnergyView( openPort : BOOLEAN);
      PROCEDURE DrawAtomView( openPort : BOOLEAN );
   END; { ModelObj }


(***************** The BesselCalc Object ************)

   besselObjPtr = ^besselObj;

   besselObj = OBJECT
{   The besselCalc Object is a collection of data/routines
   designed to speed the calculation of bessel functions
   for this program. The Init method fills an array
   with some sampled points calculated using the
   recursion relation among the bessel functions.
   Later calls to the J(n,z) and K(n,z) Entry points
   attemtp to use the sample points (plus Tayler's
   theorem!) to estimate the actual function. If the
   requested order/argument is out of range, another
   method is used to return a value.}
      BessJData   : DMatrix; { sample J data }
      BessKData   : DMatrix; { sample K data }
      nMax        : INTEGER;      { Maximum order to store }
      zMax        : REAL;         { Maximum z to store }
      deltaZ      : REAL;         { delta between z's  }
      numSamples  : INTEGER;      { number of samples for each n }
      CONSTRUCTOR Init( nMaxI, numSamplesI : INTEGER; zMaxI : REAL );
      DESTRUCTOR  Done;
      FUNCTION J(n : INTEGER; z : REAL) : REAL; { Bessel Function 1st kind }
      FUNCTION dJdz(n : INTEGER; z : REAL): REAL; { dJ/dz }
      FUNCTION K(n : INTEGER; z : REAL) : REAL; { Modifies Bess. Func      }
      FUNCTION dKdz(n : INTEGER; z : REAL) : REAL; { dK/dz }
      PROCEDURE JKFill;                  { fill in J,K samples }
      FUNCTION K0( z : REAL) : REAL;     { K0 polynomial     }
      FUNCTION I0( z : REAL) : REAL;     { I0 polynomial     }
      FUNCTION K1( z : REAL) : REAL;     { K0 polynomial     }
      FUNCTION I1( z : REAL) : REAL;     { I0 polynomial     }
      FUNCTION BessJVal( n, sample : INTEGER) : REAL;
      FUNCTION BessKVal( n, sample : INTEGER) : REAL;
      PROCEDURE BessJPut( n, sample : INTEGER; val : REAL);
      PROCEDURE BessKPut( n, sample : INTEGER; val : REAL);
   END; { besselObj }


PROCEDURE DisplayMainScreen;  FORWARD;
FUNCTION GlueDiffSlope( energy : REAL ) : REAL; FAR; FORWARD;
FUNCTION GlueNormFunc( r : REAL ) : REAL; FAR; FORWARD;
FUNCTION Simpson( func : FUNCTIONX ; a, b : REAL;
                  samples : INTEGER ): REAL; FORWARD;
PROCEDURE tred2( a : DMatrix; d, e : DVector ); FORWARD;
PROCEDURE tqli( d, e : DVector; z : DMatrix ); FORWARD;
PROCEDURE MyClearAxis( viewPort : INTEGER ); FORWARD;
FUNCTION myMousePosn(VAR X,Y:REAL; VNum,SNum:integer; VAR Inside:boolean) : WORD; FORWARD;

(**************** Variables ******************************)

{ A few globals. (Plus those defined in the Units )}

VAR
   myMenu      : TMenu; { the currently displayed menu }
   HotKeys     : THotKeys;
   quit        : Boolean;
   mes         : STRING[50];
   model       : modelObj;    { the actual model }
   bessel      : besselObjPtr;{ the bessel function object }
   glueAtomPtr : atomPtr;     { global atom pointer to get to
                                global functions }
   myContour   : TContour;    { general countour object }

(************* Implementation of the Atom Object *************)

PROCEDURE atomObj.draw;
CONST
     NUMSIDES = 20;
VAR
   xScreen, yScreen, rScreen : INTEGER;
   xPos, yPos, index : INTEGER;
   Xfactor, Yfactor, theta : REAL;
BEGIN
{   Select correct viewport and scale.}
     SelectViewPort( ATOMVIEWPORT );
     SelectScale( ATOMVIEWPORT );
     Map( position.X, position.Y, xScreen, yScreen);
     WITH Views[ViewPortNumber] do
     BEGIN
       WITH GraphScales[ScaleNumber] do
       BEGIN
{   Get scaling stuff.}
          Xfactor := (Vx2-Vx1)/(xmax-xmin);
          Yfactor := (Vy2-Vy1)/(ymax-ymin);
{   Set up Graphics mode for drawing this atom.}
          SetWriteMode( drawingMode );
          SetColor( color );
          IF (bSelected) THEN
             SetLineStyle(SolidLn, 0, ThickWidth)
          ELSE
             SetLineStyle(SolidLn, 0, NormWidth);
{   Draw the 'circle' for the atom.}
          rScreen := ROUND(Xfactor * radius);
          HideCursor;
          MoveTo( xScreen + rScreen, yScreen);
          FOR index := 1 TO NUMSIDES DO
          BEGIN
             theta := index * 2.0 * PI / NUMSIDES;
             xPos := xScreen + ROUND(rScreen*Cos(theta));
             yPos := yScreen - ROUND(rScreen*(Yfactor/Xfactor)*Sin(theta));
             LineTo( xPos, yPos);
          END;
          IF bSelected THEN
          BEGIN
{   set up & draw size box.}
             SetLineStyle( SolidLn, 0, NormWidth);
             sizeB1.x := xScreen + rScreen;
             sizeB1.y := yScreen - YSELECTBOX div 2;
             sizeB2.x := xScreen + rScreen + XSELECTBOX;
             sizeB2.y := yScreen + YSELECTBOX div 2;
             Rectangle( sizeB1.x, sizeB1.y, sizeB2.x, sizeB2.y);
          END;
          SetWriteMode( CopyPut);
          SetColor( white );
          ShowCursor;
       END;
     END;
END;

CONSTRUCTOR atomObj.Init( r, V0, x, y : REAL );
BEGIN
     radius := r;
     depth := V0;
     position.X := x;
     position.Y := y;
     numElectrons := 0;
     maxN := 0;
     nextAtom := NIL;
     drawingMode := CopyPut;
     color := white;
     bSelected := FALSE;
     states := NEW( atomicStatePtr );
     IF states = NIL THEN
     BEGIN
        Error('No memory for state in DiffSlope!');
        Exit;
     END
     ELSE
     BEGIN
        states^.nextState := NIL;
        states^.m := 0;            { only s states for now }
     END;
     FindEnergy;  { find energy, fill in state information }
END;

DESTRUCTOR atomObj.Done;
BEGIN
     { Nothing yet };
END;

FUNCTION atomObj.DiffSlope( currEnergy : REAL ) : REAL;
VAR
   slope1      : REAL;  { slope of inner function  }
   slope2      : REAL;  { slope of outer function  }
{   phiInner = A * J(m, alpha * r);
   phiOuter = B * K(m, beta * r);
   BTW:  energy is a negative quantity (for bound states) but
   depth is positive.}

BEGIN
   IF states = NIL THEN
   BEGIN
      Error('No state defined in memory for state in DiffSlope!');
      Exit;
   END;
   WITH bessel^ DO
   WITH states^ DO
   BEGIN
      energy := currEnergy;
      alpha := sqrt( energy + depth);
      beta := sqrt( -energy );
      A := 1.0;
      B := A * J( m, alpha * radius )/K( m, beta * radius );
      slope1 := A * alpha * dJdz(m, alpha*radius);
      slope2 := B * beta * dKdz(m, beta*radius);
   END;
   DiffSlope := slope1 - slope2;
END;

FUNCTION atomObj.WaveFunc( r : REAL ) : REAL;
BEGIN
   IF states = NIL THEN
   BEGIN
      Error('No wave function defined yet in WaveFunc!');
      WaveFunc := 0.0;
   END
   ELSE
      WITH bessel^ DO
      WITH states^ DO
      BEGIN
         IF r < radius THEN
         BEGIN
            WaveFunc := A * J(m, alpha * r);
         END
         ELSE
         BEGIN
            WaveFunc := B * K(m, beta * r);
         END;
      END;
END;

PROCEDURE atomObj.Normalize;
{   Set coeficients A and B based on normalization...}
CONST
   NUMSAMPLES = 21;
VAR
   integral : REAL;     { value of normalization integral }
BEGIN
{   Integrate[ r Psi(r^2),(*r,0,Infinity)]

   First integrate region r < radius.}
   glueAtomPtr := Addr(self);  { point global atom ptr to me }
   integral := Simpson( GlueNormFunc,
                        0, 2*(radius + 1.0/(states^.beta + 1/(100*radius))),
                        NUMSAMPLES);
{   psi := A * J(m, alpha * r ) (r <  radius )
          B * K(m, beta *  r ) (r >= radius )

      _ r = Infinity
     /
     |
 ==> |  2 * Pi * r * psi(r)^2 * dr  = 1
     |
    _/
    r = 0
             1
 ==>  A =  -----            B =  A * J(m, alpha * R)/K(beta * R)
            2Pi Integral}
   WITH states^ DO
   WITH bessel^ DO
   BEGIN
      A := 1.0/( 2.0 * PI * integral );
      B := A * J(m, alpha * radius)/K(m, beta * radius );
   END;
END;

PROCEDURE atomObj.FindEnergy;
VAR
   result : REAL;
   iterations : INTEGER;
   minE, maxE : REAL;
BEGIN
{   I can't declare an Object method as FAR for some reason...
   This trick forces a Far pointer to a 'glue' function.}
   maxE := sqr(2.404/radius) - depth;
   IF maxE > 0.0 THEN
      maxE := 0;         { just a guess }
   minE := -depth;
   glueAtomPtr := Addr(self);  { get address for glue }
   FindZero(GlueDiffSlope, minE, maxE, 0.001, result, iterations);
   WITH states^ DO
   BEGIN
      energy := result;
      alpha := sqrt( depth + energy );
      beta := sqrt( - energy );
      Normalize;               { set coeficients }
   END;
END;

PROCEDURE atomObj.ScaleWaveFunc;
VAR
   xScale : REAL;
BEGIN
   with states^ DO
   BEGIN
      xScale := 2.0*(radius + 1/(beta + 1/(100*radius)));
      DefineScale(ENERGYVIEWPORT, -0.2*xScale, xScale, -0.2*A, 1.2*A);
      SelectViewPort( ENERGYVIEWPORT);
      ClearAxis;
      OpenViewPort( ENERGYVIEWPORT);
      SelectScale( ENERGYVIEWPORT );
      Axis(0, 0, 0, 0);
   END;
END;

PROCEDURE atomObj.DrawWaveFunc( rescale : BOOLEAN );
CONST
   NUMSAMPLES = 40;
VAR
   waveFuncVals : DVector;
   rVals        : DVector;
   index        : INTEGER;
   xVal         : REAL;
   yVal         : REAL;
BEGIN
   FindEnergy;
   IF rescale THEN
      ScaleWaveFunc;
   waveFuncVals.Init( NUMSAMPLES );
   rVals.Init( NUMSAMPLES );
   FOR index := 0 TO NUMSAMPLES - 1 DO
   BEGIN
      xVal := index*2.0*(radius + 1/(states^.beta + 1/(100*radius)))/
                                                  (NUMSAMPLES - 1);
      rVals.Put( index + 1, xVal);
      yVal := WaveFunc( xVal );
      waveFuncVals.put( index + 1, yVal);
   END;
   SelectViewPort( ENERGYVIEWPORT );
   SelectScale( ENERGYVIEWPORT );
   SetWriteMode( drawingMode );
   SetColor( color );
   PlotDVectors( rVals, waveFuncVals, 1, NUMSAMPLES);
   rVals.Free;
   waveFuncVals.Free;
END;

PROCEDURE atomObj.MoveAtom( newX, newY : REAL );
BEGIN
     IF ((position.X <> newX) OR (position.Y <> newY)) THEN
     BEGIN
{   Assume atom has been drawn at it's current position.
   (Usually at this point drawing mode will be XORPut.)}
         Draw;
{   Now move it.}
         position.X := newX;
         position.Y := newY;
{   Now draw again.}
         Draw;
     END;
END;

FUNCTION atomObj.MatrixElement( otherAtom : atomPtr) : REAL;
{   Calculate the matrix element between self and otherAtom.}
VAR
   r, theta    : REAL;  { variables of the integral }
   r1, r2      : REAL;  { distance from center of atom to edge of other }
   otherR      : REAL;
   result      : REAL;
   rIndex, tIndex : REAL;
BEGIN
{   For now just make a fake (but more or less reasonable) estimate
   of the integral ...
       -
      /
      |
      | psi_1(x) * (V1(x) + V2(x))* psi_2(x) d^2x
      /
     -
     All x

     One thing we do know is that the only contributions to
     this integral happen *within* the atoms, where V1 and
     V2 are <> 0, so we estimate

     Integral =~ PI*(psi_1(x1-x2)*V2*psi_2(0) r2^2 +
                     psi_1(0)*V1*psi_2(x2-x1) r1^2)

     for the case self <> otherAtom and
     E for the case self = otherAtom;}
   IF otherAtom = Addr(self) THEN
      result := states^.energy
   ELSE
      BEGIN
         r := sqrt(  sqr(position.x - otherAtom^.position.x) +
                  sqr(position.y - otherAtom^.position.y));
         r1 := r - radius;
         IF r1 < 0.0 THEN
            r1 := 0.0;
         r2 := r - otherAtom^.radius;
         IF r2 < 0.0 THEN
            r2 := 0.0;
         result := PI*(
                        -WaveFunc(r)*otherAtom^.depth*
                           otherAtom^.states^.A*sqr(otherAtom^.radius) +
                          -otherAtom^.WaveFunc(r)*depth*
                                   states^.A*sqr(radius)
                        );
      END;
   MatrixElement := result;
END;


FUNCTION atomObj.ClickInAtom( x, y : REAL) : BOOLEAN;
BEGIN
     ClickInAtom := sqr(x - position.X) +
                    sqr(y - position.Y) < radius*radius;
END;

(************* Implementation of the Model Object *************)

PROCEDURE modelObj.Init;
BEGIN
     atomList := NIL;            (* Initially Empty Atom List *)
     selection := NIL;           (* No selection Initially    *)
     clipboard := NIL;
     calcMode := clusterMode;    (* Start in Cluster Mode     *)
     clusterStates.energies.Init(0);    (* no cluster states yet  *)
     clusterStates.stateVectors.Init(0,0);
     clusterStates.whichState := 0;
     energyView := clusterEnergyView;  (* show cluster energies *);
     atomView := noWaveFuncs;
     configChanged := TRUE;       { No changes yet. }
END;

PROCEDURE modelObj.Draw;
CONST
     XMESH = 30;
     YMESH = 30;
VAR
   currAtom : atomPtr;
   i, j     : INTEGER;
   drawMatrix : DMatrix;
   sum, rX, rY, r : REAL;
   whichAtom : INTEGER;
BEGIN
     SelectViewPort( ATOMVIEWPORT );
     SelectScale( ATOMVIEWPORT );
     IF CountAtoms <> 0 THEN
     BEGIN
        IF atomView = clusterFuncView THEN { draw cluster function atoms }
        BEGIN
           Message('Please Stand By......');
           SelectViewPort( ATOMVIEWPORT );
           SelectScale( ATOMVIEWPORT );
           CalcClusterState;
           drawMatrix.Init( XMESH, YMESH );
           FOR i := 1 TO XMESH DO
              FOR j := 1 TO YMESH DO
              BEGIN
                 sum := 0.0;
                 whichAtom := 1;
                 currAtom := atomList;
                 WHILE currAtom <> NIL DO
                 WITH GraphScales[ ATOMVIEWPORT ] DO
                 WITH clusterStates DO
                 BEGIN
                    rX := xMin + (i-1)*(xMax - xMin)/(XMESH-1);
                    rY := yMax - (j-1)*(yMax - yMin)/(YMESH-1);
                    r := sqrt(sqr(rX - currAtom^.position.x) +
                           sqr(rY - currAtom^.position.y));
                    sum := sum + stateVectors.Value(whichAtom, whichState)*
                                 currAtom^.WaveFunc(r);
                    currAtom := currAtom^.nextAtom;
                    whichAtom := whichAtom + 1;
                 END;
                 drawMatrix.put(j,i,sum);
              END;
           HideCursor;
           myContour.DrawMatrix( drawMatrix );
           myMenu.Display;
           ShowCursor;
           drawMatrix.Free;
        END;
        currAtom := atomList;  (* start at beginning *)
        WHILE currAtom <> NIL DO
        BEGIN
           currAtom^.Draw;                   (* draw the current atom *)
           currAtom := currAtom^.nextAtom;   (* get next atom *)
        END;
     END;
     WITH Views[ ATOMVIEWPORT ] DO
     BEGIN
          Rectangle( 0, 0, Vx2 - Vx1, Vy2 - Vy1);
          DrawAtomAxis;
     END;
END;

PROCEDURE modelObj.HandleMouseClick( event : eventTp);
VAR
   whichAtom : atomPtr;   { pointer to atom under mouse! }
   Rx, Ry    : REAL;      { position of mouse }
   OldRx, OldRy : REAL;
   Inside    : BOOLEAN;   { flag for testing mouse position }
   x, y, button : WORD;   { Global Mouse Position arguments }
   mouseEnergy : REAL;    { energy corresponding to mouse posn }
   Emin, Emax, ESpan : REAL;
   i, minDex, maxDex : INTEGER; { various indices }
   dLookup : DVector;     { lookup vector to find closest }
   moved : BOOLEAN;
BEGIN
     button := myMousePosn(OldRx, OldRy, ATOMVIEWPORT, ATOMVIEWPORT, Inside);
     IF Inside THEN
     BEGIN
        IF OnSizeBox( OldRx, OldRy ) THEN
           SizeSelection
        ELSE
        BEGIN
           whichAtom := InAtom( OldRx, OldRy ); { Get atom under mouse }
           IF whichAtom <> NIL THEN
           BEGIN
              IF selection <> whichAtom THEN
                 SelectAtom( whichAtom );
              moved := FALSE;
              WHILE (NOT moved) AND (button <> 0) DO
              BEGIN
                 button := myMousePosn(Rx, Ry, ATOMVIEWPORT, ATOMVIEWPORT, Inside);
                 moved := (Rx <> OldRx) OR (Ry <> OldRy);
              END;
              IF moved THEN
                 DragSelection;
              IF event.doubleClicked THEN
                 GetSelectionData;
           END
           ELSE   { Not in an atom... deal with it. }
           BEGIN
               UnselectAtom;  { If anything is selected, unselect. }
               IF event.doubleClicked THEN
               BEGIN
                  AddAtom( OldRx, OldRy );
               END;
           END;
        END;
     END
     ELSE IF energyView = clusterEnergyView THEN
     BEGIN
        SelectScale( ENERGYVIEWPORT );
        button := myMousePosn( Rx, Ry, ENERGYVIEWPORT, ENERGYVIEWPORT, Inside );
        IF Inside THEN  { Ry is an energy! }
           WITH clusterStates DO
           WITH GraphScales[ ENERGYVIEWPORT ] DO
           BEGIN
              energies.MinMax( minDex, Emin, maxDex, Emax);
              ESpan := Emax - Emin;
              if (Rx<0.5) then mouseEnergy := Ry * ESpan + Emin
              else mouseEnergy := Ry - 1.0;
              dLookup.Init( energies.Size );
              FOR i := 1 TO energies.Size DO
                 dLookup.Put(i, abs(mouseEnergy - energies.Value(i)));
              dLookup.MinMax( minDex, Emin, maxDex, Emax);
              dLookup.Free;
              IF whichState <> minDex THEN
              BEGIN
                 whichState := minDex;
                 DrawEnergyView( TRUE );
                 Draw;
              END;
           END;
     END;
END;

PROCEDURE modelObj.GetSelectionData;
VAR
   AtomScreen   : TInputScreen;
BEGIN
{   Get mobility for the selected cell.}
   SelectSomething;
   with AtomScreen do
   BEGIN
        Init;
        DefineInputPort(0.26,0.74,0.6,0.9);
        LoadLine('');
        LoadLine(' Atomic Data for Selected Atom');
        LoadLine('');
        LoadLine('  Radius     = {      } (0.5 to 10)');
        LoadLine('  Well Depth = {      } (0.1 to 10)');
        LoadLine('');
        LoadLine('      [  Ok  ]  [Cancel]   ');
        SetNumber(1,selection^.radius);
        SetNumber(2,selection^.depth);
        SetNumberLimits(1,MinRadius,10);
        SetNumberLimits(1,MinDepth ,10);
        AcceptScreen;
        IF NOT canceled THEN
        BEGIN
             selection^.radius := GetNumber(1);
             selection^.depth  := GetNumber(2);
             selection^.FindEnergy;
             CalcClusterState;
        END;
        DisplayMainScreen;   END;
END;

PROCEDURE modelObj.DragSelection;
{   Move selected Atom using the Mouse!}
VAR
   Rx, Ry    : REAL;      { position of mouse }
   OldRx, OldRy : REAL;
   Inside    : BOOLEAN;   { flag for testing mouse position }
   x, y, button : WORD;   { Global Mouse Position arguments }
   xInit, yInit : WORD;   { Initial x and y }
   drawBare     : BOOLEAN;{ don't update  energy view while dragging }
BEGIN
     WITH Views[ ATOMVIEWPORT ] DO
          MouseBorders( Vx1, Vy1, Vx2, Vy2 );
     button := myMousePosn( OldRx, OldRy, ATOMVIEWPORT, ATOMVIEWPORT, Inside);
     IF selection <> NIL THEN
        WITH selection^ DO
        BEGIN
           drawBare := (energyView = AtomicWaveFuncView);
           color := GraphBackColor;          { first erase it }
           Draw; { selection }
           color := white;
           DrawAtomAxis;
           drawingMode := XORPut;   { set XOR drawing mode }
           Draw;                    { get in sync }
           IF NOT drawBare THEN
           BEGIN
              ClearAxis;
    {          SetWriteMode( XORPut );
              DrawEnergyView( FALSE );}
           END;
           REPEAT
              button := myMousePosn( Rx, Ry, ATOMVIEWPORT, ATOMVIEWPORT, Inside);
              IF ((oldRx <> Rx) OR (oldRy <> Ry)) THEN
              BEGIN
                 OldRx := Rx;
                 OldRy := Ry;
                 IF NOT drawBare THEN
                 BEGIN
                    SetWriteMode( XORPut );
                    DrawEnergyView( FALSE );
                 END;
                 MoveAtom( Rx + mouseOffset.x, Ry + mouseOffset.y );
                 IF NOT drawBare THEN
                 BEGIN
                    SetWriteMode( XORPut );
                    DrawEnergyView( FALSE );
                 END;
              END;
           UNTIL button = 0;
           drawingMode := CopyPut;
           MouseBorders( 0, 0, GetMaxX, GetMaxY);
        END;
        Draw;  { model }
        IF NOT drawBare THEN
           DrawEnergyView( TRUE );
END;

PROCEDURE modelObj.SelectSomething;
{   This is for operations that need SOMETHING to be selected.
   Select something makes *&^% sure that SOMETHING intellegent
   is in fact selected. If it can't find an atom to select,
   it constructs one.}
BEGIN
     IF selection = NIL THEN
     BEGIN
        IF atomList = NIL THEN
        BEGIN
           AddAtom(0, 0);
           IF atomList = NIL THEN
              Error('Can''t add a SINGLE atom!')
        END;
        SelectAtom( atomList );
     END;
END;

PROCEDURE modelObj.MoveSelection( dx, dy : REAL );
{   Move selected Atom specified fraction of xSize.}
BEGIN
     SelectSomething;
     IF selection <> NIL THEN
        WITH selection^ DO
        BEGIN
           color := GraphBackColor;          { first erase it }
           Draw; { selection }
           drawingMode := XORPut;   { set XOR drawing mode }
           color := white;          { now XOR it }
           Draw;                    { get in sync }
           MoveAtom( position.x + dx*xSize , position.y + dy*ySize );
           drawingMode := CopyPut;
        END
     ELSE
        Error('selection error in MoveSelection.');
     Draw;  { model }
END;


PROCEDURE modelObj.SizeSelection;
VAR
   Rx, Ry    : REAL;      { position of mouse }
   Inside    : BOOLEAN;   { flag for testing mouse position }
   x, y, button : WORD;   { Global Mouse Position arguments }
   xScreen, yScreen : INTEGER;
   oldRadius : REAL;      { remembered radius }
   newRadius : REAL;      { new value for radius }
BEGIN
     Map( selection^.position.x, selection^.position.y,
          xScreen, yScreen); { get x,y }
     WITH Views[ ATOMVIEWPORT ] DO
          MouseBorders( xScreen+2, Vy1, Vx2, Vy2 );
     WITH selection^ DO
     BEGIN
          color := GraphBackColor;          { first erase it }
          Draw;
          IF energyView <> AtomicWaveFuncView THEN
             MyClearAxis( ENERGYVIEWPORT );
          drawingMode := XORPut;   { set XOR drawing mode }
          color := white;          { now XOR it in ... }
          Draw;                    { get in sync }
          DrawEnergyView( energyView = AtomicWaveFuncView);
          oldRadius := radius;
          REPEAT
             button := myMousePosn( Rx, Ry, ATOMVIEWPORT, ATOMVIEWPORT, Inside);
             newRadius := Rx - position.X;
             IF newRadius < MINRADIUS THEN
                newRadius := MINRADIUS;
             IF newRadius <> oldRadius THEN
             BEGIN
                Draw;   { erase old stuff }
                SetWriteMode( XORPut);
                DrawEnergyView( FALSE );
                FindEnergy;
                radius := newRadius;
                oldRadius := radius;
                Draw;     { XOR in new! }
                SetWriteMode( XORPut);
                DrawEnergyView( FALSE );
             END;
          UNTIL button = 0;
          drawingMode := CopyPut;
         
     END;
     MouseBorders( 0, 0, GetMaxX, GetMaxY);
     Draw;
     DrawEnergyView( TRUE );
END;

PROCEDURE modelObj.SelectAtom( thisAtom : atomPtr);
VAR
   xScale : REAL;
BEGIN
     IF thisAtom <> NIL THEN
     BEGIN
        UnselectAtom;
        WITH thisAtom^ DO
        BEGIN
           bSelected := TRUE;
           Draw;
        END;
           
        selection := thisAtom;
        IF energyView = AtomicWaveFuncView THEN
           DrawEnergyView( TRUE );
     END
     ELSE
         Error('NIL selection!');
END;

PROCEDURE modelObj.DrawEnergyView( openPort : BOOLEAN);
VAR
   stateNumber : INTEGER;
   ESpan, Emin, Emax : REAL;
   i, j, col         : INTEGER;
BEGIN
   SelectViewPort( ENERGYVIEWPORT );
   case model.energyView OF
     atomicWaveFuncView :
      IF selection <> NIL THEN
         WITH GraphScales[ENERGYVIEWPORT] DO
         WITH selection^ DO
         BEGIN
            IF openPort THEN
            BEGIN
               ClearAxis;
               OpenViewPort( ENERGYVIEWPORT );
               PlotLine(0.0,ymin,0.0,ymax);
               PlotLine(xmin,0.0,xmax,0.0);
            END;
            DrawWaveFunc( (ymax < states^.A) OR openPort );
            SetTextJustify( CenterText, CenterText);
            RubOutXY((ENERGYX1 + ENERGYX2)/2, ENERGYY1-0.05,15,black);
            PrintXY((ENERGYX1 + ENERGYX2)/2, ENERGYY1-0.05,'Wave Function');
            PutLabel(Left, 'Psi');
         END;
    clusterEnergyView :
       IF CountAtoms <> 0 THEN
       BEGIN
          CalcClusterState;
          WITH clusterStates DO
          BEGIN
             energies.MinMax( i, Emin, j, Emax);
             ESpan := Emax - Emin;
             IF ESpan = 0.0 THEN ESpan := 1;
             DefineScale( ENERGYVIEWPORT, 0, 1, -0.1, 1.1);
             SelectViewPort( ENERGYVIEWPORT);
             SelectScale( ENERGYVIEWPORT);
             IF openPort THEN
             BEGIN
                ClearAxis;
                OpenViewPort( ENERGYVIEWPORT );
                Axis( 0, 100, 0, 0);
             END;
             FOR i := 1 TO energies.Size DO
             BEGIN
                if selection^.drawingMode=XORput then
                     col := GraphBackColor
                else col := black;
                IF (i = whichState) THEN
                     SetColor( lightred xor col )
                ELSE SetColor( white    xor col );
                PlotLine(0, (energies.Value(i)-Emin)/ESpan,
                         0.45, (energies.Value(i)-Emin)/ESpan);
                PlotLine(0.45, (energies.Value(i)-Emin)/ESpan,
                         0.55,(energies.Value(i) + 1.0)/1.0);
                PlotLine(0.55,(energies.Value(i) + 1.0)/1.0,
                         1.00,(energies.Value(i) + 1.0)/1.0);
             END;
             SetColor( white );
             IF (whichState > 0) and (whichState <= energies.Size) THEN
             BEGIN
                SetTextJustify( LeftText, CenterText);
                RubOutXY(ENERGYX1, ENERGYY1-0.08,30,BLACK);
                PrintXY(ENERGYX1, ENERGYY1-0.08,
                    'Selected Energy = '+ Num2Str( energies.Value(whichState),5));
             END;
          END;
          SetTextJustify( CenterText, CenterText);
          RubOutXY((ENERGYX1 + ENERGYX2)/2, ENERGYY1-0.05,15,Black);
          PrintXY((ENERGYX1 + ENERGYX2)/2, ENERGYY1-0.05,'Energy Diagram');
          PutLabel(Left, 'E');
       END;
   END;
END;


PROCEDURE modelObj.DrawAtomView( openPort : BOOLEAN);
BEGIN
   SelectViewPort( ATOMVIEWPORT );
   IF openPort THEN OpenViewPort( ATOMVIEWPORT );
   Draw;
END;
     
PROCEDURE modelObj.UnselectAtom;
BEGIN
     IF selection <> NIL THEN
     BEGIN
          WITH selection^ DO
          BEGIN
               color := GraphBackColor;
               Draw;
               bSelected := FALSE;
               color := white;
               drawingMode := CopyPut;
               Draw;
          END;
          selection := NIL;
     END;
END;

PROCEDURE modelObj.SelectNextAtom;
{   For tab support. Select next atom in chain.}
BEGIN
     SelectSomething;
     IF selection^.nextAtom <> NIL THEN
        SelectAtom( selection^.nextAtom)
     ELSE IF atomList <> NIL THEN
        SelectAtom( atomList )
     ELSE
        Error('Selection error in SelectNextAtom');
END;


FUNCTION modelObj.InAtom( x, y : REAL ) : atomPtr;
{   check for click inside an atom.}
VAR
   currAtom : atomPtr;
   found    : BOOLEAN;
BEGIN
     currAtom := atomList;    { start at the beginning }
     found    := FALSE;       { not found yet!         }
     WHILE (NOT found) AND (currAtom <> NIL) DO
     BEGIN
          found := currAtom^.ClickInAtom( x, y );
          IF NOT found THEN
             currAtom := currAtom^.nextAtom
          ELSE
          BEGIN
               mouseOffset.X := currAtom^.position.X - x;
               mouseOffset.Y := currAtom^.position.Y - y;
          END;
     END;
     InAtom := currAtom;
END;

FUNCTION modelObj.OnSizeBox( Rx, Ry : REAL ): BOOLEAN;
{   check for size box of selection.}
VAR
   xScreen, yScreen : INTEGER;
BEGIN
   IF selection <> NIL THEN
      WITH selection^ DO
      BEGIN
         SelectViewPort( ATOMVIEWPORT );
         SelectScale( ATOMVIEWPORT );
         Map( Rx, Ry, xScreen, yScreen);
         OnSizeBox := (xScreen <= sizeB2.x) AND (xScreen >= sizeB1.x) AND
                      (yScreen >= sizeB1.y) AND (yScreen <= sizeB2.y);
      END
   ELSE
      OnSizeBox := FALSE;
END;

PROCEDURE modelObj.AddAtom(x, y : REAL);
{   Add an atom to the atom list for the model.}
VAR
   newPtr : atomPtr;
BEGIN
     newPtr := NEW( atomPtr, Init(DEFAULTRADIUS, DEFAULTDEPTH, x, y));
     IF newPtr <> NIL THEN
     BEGIN
          newPtr^.nextAtom := atomList;   (* add ot beginning of list *)
          atomList := newPtr;
          SelectAtom( atomList );
          IF energyView <> AtomicWaveFuncView THEN
             DrawEnergyView( TRUE );
          Draw;
     END
     ELSE
         Error('Cannot Allocate more memory for atom!');
END;

PROCEDURE modelObj.CleanUp;
{   Delete all atoms in model.}
VAR
   currPtr : atomPtr;   (* pointer to 'current' atom *)
   nextPtr : atomPtr;   (* pointer to 'next' atom *)
BEGIN
     currPtr := atomList;
     atomList := NIL;
     WHILE currPtr <> NIL DO
     BEGIN
          nextPtr := currPtr^.nextAtom;
          DISPOSE( currPtr, Done );
          currPtr := nextPtr;
     END;
END;

PROCEDURE modelObj.Clear;
{   Clear out all atoms. Start over.}
BEGIN
     CleanUp;
     OpenViewPort( ATOMVIEWPORT );
     Draw;
END;

PROCEDURE modelObj.Cut;
VAR
   currPtr, lastPtr : atomPtr;
   found            : BOOLEAN;
BEGIN
     IF selection <> NIL THEN
     BEGIN
          found := FALSE;
          currPtr := atomList;
          lastPtr := NIL;     { use lastPtr to keep last atom }
{   look through list for selection.}
          WHILE (NOT found) AND (currPtr <> NIL) DO
          BEGIN
               found := selection = currPtr;
               IF NOT found THEN
               BEGIN
                  lastPtr := currPtr;
                  currPtr := currPtr^.nextAtom;
               END;
          END;
          IF (found) THEN
          BEGIN
               IF lastPtr = NIL THEN
                    atomList := currPtr^.nextAtom
               ELSE
                   lastPtr^.nextAtom := currPtr^.nextAtom;
               UnselectAtom;
               SetClipboardData(currPtr); { Cut to clipboard }
               DrawAtomView( TRUE );
               DrawEnergyView( TRUE );
          END;
     END;
END;

PROCEDURE modelObj.Copy;
{   Copy contents of currently selected atom, to clipboard.}
VAR
   copyPtr : atomPtr;
BEGIN
     IF selection <> NIL THEN
        WITH selection^ DO
        BEGIN
           copyPtr := NEW( atomPtr, Init(radius, depth, position.x, position.y));
           IF copyPtr <> NIL THEN
           BEGIN
              copyPtr^.numElectrons := numElectrons;
              copyPtr^.maxN := maxN;
              SetClipboardData( copyPtr);
           END
           ELSE
              Error('Cannot Create Copy in Copy');
        END;
END;

PROCEDURE modelObj.SetClipboardData( dataPtr : atomPtr);
{   Set data into clipboard. Ownership of pointer is
   transferred to clipboard to deal with.}
BEGIN
     IF clipboard <> NIL THEN
        DISPOSE( clipboard, Done);
     clipBoard := dataPtr;
END;

PROCEDURE modelObj.Paste;
{   Paste clipboard data into selection.}
VAR
   pastePtr : AtomPtr;
BEGIN
     IF clipboard <> NIL THEN
     BEGIN
        IF selection <> NIL THEN    { Paste into selection }
           pastePtr := selection
        ELSE                        
        BEGIN                       { Create new atom & paste into it. }
           pastePtr := NEW( atomPtr, INIT(DEFAULTRADIUS,
                                          DEFAULTDEPTH,
                                          clipboard^.position.x,
                                          clipboard^.position.y ));
           IF pastePtr <> NIL THEN
           BEGIN
              IF atomList <> NIL THEN
              BEGIN
                 pastePtr^.nextAtom := atomList; { pre-pend to list }
              END;
              atomList := pastePtr;
           END;
        END;
        pastePtr^.radius := clipboard^.radius;
        pastePtr^.depth := clipboard^.depth;
        pastePtr^.maxN := clipboard^.maxN;
        pastePtr^.numElectrons := clipboard^.numElectrons;
        OpenViewPort( ATOMVIEWPORT );
        Draw;
     END;
END;


PROCEDURE modelObj.DrawAtomAxis;
BEGIN
     SelectViewPort(ATOMVIEWPORT);
     SelectScale(ATOMVIEWPORT);
     HideCursor;
     Axis(0, 0, 4, 4);
     ShowCursor;
END;

PROCEDURE modelObj.AdjustYSize;
{   Adjust ySize for correct aspect ratio.}
VAR
   Xasp, Yasp : Word;   { arguments for aspect ratio }
BEGIN
   GetAspectRatio( Xasp, Yasp);
   ySize := (ATOMSY2 - ATOMSY1) * GetMaxY /
            ((Xasp/Yasp)*(ATOMSX2 - ATOMSX1)* GetMaxX) * xSize;
   DefineScale( ATOMVIEWPORT, -xSize/2, xSize/2, -ySize/2, ySize/2);
END;

FUNCTION modelObj.CountAtoms : INTEGER;
VAR
   currAtom : atomPtr;
   numAtoms : INTEGER;
BEGIN
   currAtom := atomList;
   numAtoms := 0;
   WHILE currAtom <> NIL DO
   BEGIN
      numAtoms := numAtoms + 1;
      currAtom := currAtom^.nextAtom;
   END;

   CountAtoms := numAtoms;
END;

PROCEDURE modelObj.CreateHamiltonian( ham : DMatrix );
VAR
   rowAtom, colAtom : atomPtr;
   row, col         : INTEGER;
BEGIN
   IF atomList <> NIL THEN
   BEGIN
      colAtom := atomList;
      col := 1;
      WHILE colAtom <> NIL DO
      BEGIN
         row := col;
         rowAtom := colAtom;
         WHILE rowAtom <> NIL DO { Get the matrix element
                                       between row & column atoms }
         BEGIN
            ham.Put(row, col,rowAtom^.MatrixElement( colAtom ));
            ham.Put(col, row, ham.Value( row, col)); { symmetry }
            rowAtom := rowAtom^.nextAtom;
            row := row + 1;
         END;
         colAtom := colAtom^.nextAtom;
         col := col+1;
      END;
   END;
END;

PROCEDURE modelObj.CalcClusterState;
{   Calculate the Cluster state.}
VAR
   modelSize : INTEGER;    { number of atomic states }
   offDiag   : DVector;    { off diagonal elements   }
   i         : INTEGER;    { index for stateVectors  }
BEGIN
   WITH clusterStates DO
   BEGIN
      modelSize := CountAtoms;
      stateVectors.ReSize( modelSize, modelSize );
                                        { Find eigenvalues, vectors }
      CreateHamiltonian( stateVectors );
                                        { create the cluster hamiltonian }
      energies.ReSize( modelSize );     { get vectors for diag  }
      offDiag.Init( modelSize );        { and off diag elements }
      tred2( stateVectors, energies, offDiag);
                                        { tri-diagonalize!      }
      tqli( energies, offDiag, stateVectors);
      IF whichState > modelSize THEN whichState := 1;
   END;
   offDiag.Free;
END;

(***************** Methods of besselObj ********************)

FUNCTION besselObj.K(n : INTEGER; z : REAL) : REAL;
CONST
     BIGRESULT = 1.7e38;
VAR
   sample   : INTEGER;
   nearestZ : REAL;
   kPrime   : REAL;              { Estimate of K' at sample point }
   i        : INTEGER;           { loop variable for recursion relation }
   kVal, bessN1, bessN2 : REAL;  { recursion relation stuff }
BEGIN
   sample := round(z/deltaZ);
{   This does something more or less reasonable...}
   IF (z = 0.0) THEN
      K := BIGRESULT
   ELSE
   IF (n <= nMax) AND (sample > 0) AND (sample < numSamples) THEN 
   BEGIN
      nearestZ := sample*deltaZ;                { we can 'interpolate' }
      IF n = 0 THEN { use one sided relation }
         kPrime := -BessKVal( n+1, sample)
      ELSE IF n = nMax THEN
         kPrime := -(BessKVal(n-1, sample) + (n/z)*BessKVal(n, sample))
      ELSE
         kPrime := -(BessKVal(n-1, sample) + BessKVal(n+1, sample))/2;
      {          Use  K(z) := K(z-z0) + K'(z0)(z-z0);
      }
      K := BessKVal(n, sample) + kPrime*(z - nearestZ);
   END
   ELSE
   BEGIN
      {         Should't be out this far often , but sometimes we will!
         Use polynomial or recursion relation expansion...
      }
      IF n = 0 THEN
         K := K0( z )
      ELSE IF n = 1 THEN
         K := K1( z )
      ELSE
      BEGIN
         bessN1 := K0(z);
         bessN2 := K1(z);
         FOR i := 2 TO n DO
         BEGIN
            kVal := 2*(n - 1)*bessN2/z + bessN1;
            bessN1 := bessN2;
            bessN2 := kVal;
         END;
         K := kVal;
      END;
   END;
END;

FUNCTION besselObj.dKdz(n : INTEGER; z : REAL) : REAL;
VAR
   kPrime : REAL;
BEGIN
   IF n = 0 THEN { use one sided relation }
      kPrime := -K( n+1, z)
   ELSE IF n = nMax THEN
      kPrime := -(K(n-1, z) + (n/z)*K(n, z))
   ELSE
      kPrime := -(K(n-1, z) + K(n+1, z))/2;
   dKdz := kPrime;
END;

FUNCTION besselObj.dJdz(n : INTEGER; z : REAL ) : REAL;
{   Use recursion relation to evaluate dJ/dz.}
VAR
   jPrime : REAL;
BEGIN
   IF n = 0 THEN { use one sided relation }
      jPrime := -J( n+1, z)
   ELSE IF n = nMax THEN
      jPrime := J(n-1, z) - (n/z)*J(n, z)
   ELSE
      jPrime := (J(n-1, z) - J(n+1, z))/2;
   dJdz := jPrime;
END;


FUNCTION besselObj.J(n : INTEGER; z : REAL) : REAL;
VAR
   sample   : INTEGER;
   nearestZ : REAL;
   jPrime   : REAL;  { Estimate of J' at sample point }
   jVal     : REAL;  { result }
BEGIN
   sample := round(z/deltaZ);
   IF z = 0.0 THEN
      IF n = 0 THEN
         jVal := 1.0
      ELSE
         jVal := 0.0
   ELSE IF (n <= nMax) AND (sample < numSamples) THEN { we can 'interpolate' }
   BEGIN
      nearestZ := sample*deltaZ;
      IF n = 0 THEN { use one sided relation }
         jPrime := -BessJVal( n+1, sample)
      ELSE IF n = nMax THEN
         jPrime := BessJVal(n-1, sample) - (n/z)*BessJVal(n, sample)
      ELSE
         jPrime := (BessJVal(n-1, sample) - BessJVal(n+1, sample))/2;
      {          Use  J(z) := J(z0) + J'(z0)(z-z0);
      }
      jVal := BessJVal(n, sample) + jPrime*(z - nearestZ);
   END
   ELSE
   BEGIN
      {         Should't be out this far, but just in case...
         Use Asymptotic expansion...
      }
      jVal := Sqrt(2.0/(PI*z))*cos(z - PI*(n/2 - 0.25));
   END;
   J := jVal;
END;

FUNCTION besselObj.BessKVal( n, sample : INTEGER) : REAL;
BEGIN
   BessKVal := BessKData.Value( n + 1, sample + 1 );
END;

FUNCTION besselObj.BessJVal( n, sample : INTEGER) : REAL;
BEGIN
   BessJVal := BessJData.Value( n + 1, sample + 1 );
END;

PROCEDURE besselObj.BessKPut( n, sample : INTEGER; Val: REAL);
BEGIN
   BessKData.Put( n + 1, sample + 1 , Val);
END;

PROCEDURE besselObj.BessJPut( n, sample : INTEGER; Val: REAL);
BEGIN
   BessJData.Put( n + 1, sample + 1 , Val);
END;


CONSTRUCTOR besselObj.Init( nMaxI, numSamplesI : INTEGER; zMaxI : REAL );
{   Initialize the besselObj & fill in tables.
   Be careful. Since DMatrix uses 1,1 as origin, BessJData[1,1] is
   really J(0,0) etc.}
BEGIN
   IF numSamplesI = 0 THEN
      Error('To Few Samples in besselObj.Init ');
   nMax := nMaxI;
   numSamples := numSamplesI;
   zMax := zMaxI;
   BessJData.Init( nMax + 1, numSamples ); { Allocate room for tables }
   BessKData.Init( nMax + 1, numSamples );
   JKFill;
END;

FUNCTION BesselObj.I0( z : REAL) : REAL;
{   See Abr & Stegun, p378, 9.8.1}
VAR
   t,t2,tfac : REAL;
   I0val     : REAL;
BEGIN
{   valid for -3.75 < z < 3.75 only}
   t := z/3.75;
   t2 := t*t;     { t2 == t^2 }
   tfac := t2;
   I0val := 1.000 +  3.5156229*tfac; tfac := tfac*t2; { tfac = t^4 }
   I0val := I0val +  3.0899424*tfac; tfac := tfac*t2; { tfac = t^6 }
   I0val := I0val +  1.2067492*tfac; tfac := tfac*t2; { tfac = t^8 }
   I0val := I0val +  0.2659732*tfac; tfac := tfac*t2; { tfac = t^10 }
   I0val := I0val +  0.036076800*tfac; tfac := tfac*t2; { tfac = t^12 }
   I0val := I0val +  0.004581300*tfac;
   I0 := I0val;
END;

FUNCTION BesselObj.I1( z : REAL) : REAL;
{   See Abr & Stegun, p378, 9.8.1}
VAR
   t,t2,tfac : REAL;
   I1val     : REAL;
BEGIN
{   valid for -3.75 < z < 3.75 only}
   t := z/3.75;
   t2 := t*t;     { t2 == t^2 }
   tfac := t2;
   I1val := 0.500 +  0.87890594*tfac; tfac := tfac*t2; { tfac = t^4 }
   I1val := I1val +  0.51498869*tfac; tfac := tfac*t2; { tfac = t^6 }
   I1val := I1val +  0.15084934*tfac; tfac := tfac*t2; { tfac = t^8 }
   I1val := I1val +  0.02658733*tfac; tfac := tfac*t2; { tfac = t^10 }
   I1val := I1val +  0.00301532*tfac; tfac := tfac*t2; { tfac = t^12 }
   I1val := I1val +  0.00032411*tfac;
   I1val := I1val * z;
   I1 := I1val;
END;


FUNCTION   BesselObj.K0( z : REAL) : REAL;
{   See Abr & Stegun, p379, 9.8.5, 9.8.6}
VAR
   t, t2, tfac, K0val : REAL;
BEGIN
     IF z <= 2.0 THEN
     BEGIN
          t := z/2.0; t2 := t*t;
          K0val := -Ln(t) * I0(z) - 0.57721566;
          K0val := K0val + 0.42278420*t2; tfac := t2*t2;      { tfac = t^4 }
          K0val := K0val + 0.23069756*tfac; tfac := tfac*t2;  { tfac = t^6 }
          K0val := K0val + 0.03488590*tfac; tfac := tfac*t2;  { tfac = t^8 }
          K0val := K0val + 0.00262698*tfac; tfac := tfac*t2;  { tfac = t^10 }
          K0val := K0val + 0.00010750*tfac; tfac := tfac*t2;  { tfac = t^12 }
          K0val := K0val + 0.00000740*tfac;
     END
     ELSE
     BEGIN
          t := 2.0/z;
          K0val := 1.25331414 - 0.07832358*t;      tfac := t*t; {tfac = t^2}
          K0val := K0val +      0.02189568*tfac; tfac := tfac*t;{tfac = t^3}
          K0val := K0val -      0.01062446*tfac; tfac := tfac*t;{tfac = t^4}
          K0val := K0val +      0.00587872*tfac; tfac := tfac*t;{tfac = t^5}
          K0val := K0val -      0.00251540*tfac; tfac := tfac*t;{tfac = t^6}
          K0val := K0val +      0.00053208*tfac;
          K0val := K0val/(exp(z)*sqrt(z));
     END;
     K0 := K0val;
END;


FUNCTION   BesselObj.K1( z : REAL) : REAL;
{   See Abr & Stegun, p379, 9.8.5, 9.8.6}
VAR
   t, t2, tfac, K1val : REAL;
BEGIN
     IF z <= 2.0 THEN
     BEGIN
          t := z/2.0; t2 := t*t;
          K1val := z*Ln(t) * I1(z) + 1.0;
          K1val := K1val + 0.15443144*t2; tfac := t2*t2;      { tfac = t^4 }
          K1val := K1val - 0.67278579*tfac; tfac := tfac*t2;  { tfac = t^6 }
          K1val := K1val - 0.18156897*tfac; tfac := tfac*t2;  { tfac = t^8 }
          K1val := K1val - 0.01919402*tfac; tfac := tfac*t2;  { tfac = t^10 }
          K1val := K1val - 0.00110404*tfac; tfac := tfac*t2;  { tfac = t^12 }
          K1val := K1val - 0.00004686*tfac;
          K1val := K1val/z;
     END
     ELSE
     BEGIN
          t := 2.0/z;
          K1val := 1.25331414 + 0.23498619*t;      tfac := t*t; {tfac = t^2}
          K1val := K1val -      0.03655620*tfac; tfac := tfac*t;{tfac = t^3}
          K1val := K1val +      0.01504268*tfac; tfac := tfac*t;{tfac = t^4}
          K1val := K1val -      0.00780353*tfac; tfac := tfac*t;{tfac = t^5}
          K1val := K1val +      0.00325614*tfac; tfac := tfac*t;{tfac = t^6}
          K1val := K1val -      0.00068245*tfac;
          K1val := K1val/(exp(z)*sqrt(z));
     END;
     K1 := K1val;
END;



DESTRUCTOR BesselObj.Done;
BEGIN
   BessJData.Free;
   BessKData.Free;
END;

PROCEDURE besselObj.JKFill;
CONST
   SMALL = 1.0e-20;   { small value to calulate recursion }
VAR
   zCurr  : REAL;     { current z     }
   nCurr  : INTEGER;  { current order }
   sample : INTEGER;  { current sample number }
   bessN1, bessN2 : REAL;{ sample bessJs }
   bessCurr : REAL;   { current un-normalized Bessel func }
   bessSum : REAL;    { sum of 2*J(n,z) for normalization }
BEGIN
{   First do special case... z = 0.}
   deltaZ := zMax/(numSamples - 1);
   sample := 0;
   FOR nCurr := nMax DOWNTO 0 DO
   BEGIN
      IF nCurr <> 0 THEN
         BessJPut(nCurr, sample, 0.0)
      ELSE
         BessJPut(nCurr, sample, 1.0);
      BessKPut(nCurr, sample, -999.0);  { Error. Should never see this! }
   END;
{   Start at highst order + 5, then recurse down to zero order;}
   FOR sample := 1 TO numSamples - 1 DO
   BEGIN
{   For each value of z, sweep throught orders of J,K  start
   by assuming J(n+6,z) = 0 and J(n+5,z) = 1. Normalize later.
   First DO J.}
      bessN1 := 0.0;
      bessN2 := SMALL;
      bessSum := 0.0;
      zCurr := sample*deltaZ;
{   Start nCurr high enough to ensure accuracy.}
      FOR nCurr := nMax + round(zCurr) + 5 DOWNTO 0 DO
      BEGIN
         bessCurr := 2*(nCurr + 1)*bessN2/zCurr - bessN1;
         IF NOT odd(nCurr) THEN
            IF (nCurr <> 0) THEN
               bessSum := bessSum + 2.0 * bessCurr
            ELSE
               bessSum := bessSum + bessCurr;
         IF nCurr <= nMax THEN
            BessJPut( nCurr, sample, bessCurr);
         bessN1 := bessN2;    { update ... }
         bessN2 := bessCurr;
      END;
{   Now normalize ...}
      FOR nCurr := 0 TO nMax DO
      BEGIN
         BessJPut( nCurr, sample, BessJVal( nCurr, sample)/bessSum);
      END;
{   Now do K... by foreward recursion...}
      bessN1 := K0( zCurr );
      BessKPut( 0, sample, bessN1);
      bessN2 := K1( zCurr );
      BessKPut( 1, sample, bessN2);

      FOR nCurr := 2 TO nMax DO
      BEGIN
         bessCurr := 2*(nCurr - 1)*bessN2/zCurr + bessN1;
         BessKPut( nCurr, sample, bessCurr);
         bessN1 := bessN2;    { update ... }
         bessN2 := bessCurr;
      END;
   END;   { sample }
END;


(**************** non- oBject procedures *********************)

(**************** 'Glue' procedures. *************************)

{   These are really just dummy procedures/functions that call
   on object methods or non-global procedures. They are required
   because TP doesn't allow FAR object methods.}

FUNCTION GlueDiffSlope( energy : REAL ) : REAL;
BEGIN
   GlueDiffSlope := glueAtomPtr^.DiffSlope( energy );
END;

FUNCTION GlueNormFunc( r : REAL ) : REAL;
BEGIN
     GlueNormFunc := r * sqr(glueAtomPtr^.WaveFunc(r));
END;

{   Clear Viewport + Axis...}
PROCEDURE MyClearAxis( viewPort : INTEGER );
BEGIN
   SelectViewPort( viewPort );
   WITH Views[viewPort] DO
      SetViewPort( Vx1 - 2*TextWidth('00000'), Vy1 - 6,
           Vx2 + 2*TextWidth('00000'), Vy2 + TextHeight('0')+6, True);
   HideMouse;
   ClearViewPort;
   ShowMouse;
   SelectViewPort( viewPort );
   SetColor(white);
   with views[ViewPort] do
    rectangle(0,0,vx2-vx1,vy2-vy1);
END;

{   Simple simpson rule integration.}

FUNCTION Simpson( func : FUNCTIONX ;         { function }
                  a, b : REAL;               { limits   }
                  samples : INTEGER ): REAL; { number of samples }
VAR
   h     : REAL;    { stepsize }
   sum   : REAL;    { simpson's sum }
   i     : INTEGER; { loop control }
BEGIN
   sum := func(a) + func(b);              { initialize sum }
   IF NOT ODD( samples ) THEN
      samples := samples + 1;             { must be odd }
   IF (samples > 1) AND (b > a) THEN
   BEGIN
      h := (b - a)/(samples - 1);         { calculate h }
      FOR i := 1 TO samples - 2 DO
         IF ODD(i) THEN
            sum := sum + 4.0*func(i*h)
         ELSE
            sum := sum + 2.0*func(i*h);
   END;
   Simpson := sum*h/3.0;
END;

PROCEDURE tred2( a : DMatrix; d, e : DVector );
{   From Numerical Recipies. (Actually translated from
                                Numerical Recipies in C!)
   The (terribly un-imaginative) variable names are due to
   Press et. al.. No need to apologize.

   Performs a Householder transformation to get a into
   tri-diagonal form.}
VAR
   l,k,j,i           : INTEGER;   { counters }
   scale, hh, h,g,f  : REAL;      { misc factors }
BEGIN
   FOR i := a.NRows DOWNTO 2 DO
   BEGIN
      l := i - 1;
      scale := 0.0;
      h := 0.0;
      IF l > 1 THEN
      BEGIN
         FOR k := 1 TO l DO
            scale := scale + abs( a.Value(i,k));
         IF scale = 0.0 THEN                    { skip transformation }
            e.VecPtr^[i] := a.Value(i,l)
         ELSE
         BEGIN
            FOR k := 1 TO l DO
            BEGIN
               a.Put(i,k,a.Value(i,k)/scale);
               h := h + sqr(a.Value(i,k));
            END;
            f := a.Value(i,l);
            IF f > 0 THEN g := -sqrt(h) ELSE g := sqrt(h);
            e.VecPtr^[i] := scale*g;
            h := h - f*g;
            a.Put(i,l,f - g);
            f := 0.0;
            FOR j := 1 TO l DO
            BEGIN
               a.Put(j,i,a.Value(i,j)/h);
               g := 0.0;
               FOR k := 1 TO j DO
                 g := g + a.Value(j,k)*a.Value(i,k);
               FOR k := j+1 TO l DO
                  g := g + a.Value(k,j)*a.Value(i,k);
               e.VecPtr^[j] := g/h;
               f := f + e.Value(j)*a.Value(i,j);
            END;
            hh := f/(h + h);
            FOR j := 1 TO l DO
            BEGIN
              f := a.Value(i,j);
              g := e.Value(j) - hh*f;
              e.VecPtr^[j] := g;
              FOR k := 1 TO j DO
                 a.Put(j,k,a.Value(j,k) -
                    ( f*e.Value(k) + g*a.Value(i,k)));
            END;
         END
      END
      ELSE
         e.VecPtr^[i] := a.Value(i,l);
      d.VecPtr^[i] := h;
   END;
   d.VecPtr^[1] := 0.0;
   e.VecPtr^[1] := 0.0;
   FOR i := 1 TO a.NRows DO
   BEGIN
      l := i-1;
      IF d.Value(i) <> 0.0 THEN
      BEGIN
      FOR j :=1 TO l DO
         BEGIN
            g := 0.0;
            FOR k := 1 TO l DO
               g := g + a.Value(i,k)*a.Value(k,j);
            FOR k := 1 TO l DO
               a.Put(k,j,a.Value(k,j) - g*a.Value(k,i));
         END;
      END;
      d.VecPtr^[i] := a.Value(i,i);
      a.Put(i,i,1.0);
      FOR j := 1 TO l DO
      BEGIN
         a.Put(i,j,0.0);
         a.Put(j,i,0.0);
      END;
   END;
END;

FUNCTION Sign( a, b : REAL ) : REAL;
{   Equivalent to C macro :
   #define SIGN(a,b) ((b)<0 ? -fabs(a) : fabs(a))}
BEGIN
   IF b < 0.0 THEN
      Sign := -abs(a)
   ELSE
      Sign := abs(a);
END;

PROCEDURE tqli( d, e : DVector; z : DMatrix );
{   tqli from Numerical Recipies in C, converted to CUPs pascal.
   Evaluates the eigenvalues and eigenvactors of a symmetric
   tri-diagonal matrix converted by tred2. The (terribly un-imaginative)
   variable names are due to Press et. al.. No need to apologize.}
VAR
   m,l,iter,i,k      : INTEGER;
   s,r,p,g,f,dd,c,b  : REAL;
   found             : BOOLEAN;
BEGIN
   FOR i := 2 TO d.Size DO e.VecPtr^[i-1] := e.Value(i);
   e.VecPtr^[d.Size] := 0.0;
   FOR l := 1 TO d.Size DO
   BEGIN
      iter := 0;
      REPEAT
(*
   I had to change this part quite a lot to get the same effect as:
   for (m=l;m<=n-1;m++) {    dd=fabs(d[m])+fabs(d[m+1]);
    if (fabs(e[m])+dd == dd) break;
   }
*)
         found := FALSE;
         m := l;
         WHILE (NOT found) AND ( m <= d.Size - 1) DO
         BEGIN
            dd := abs(d.VecPtr^[m]) + abs(d.VecPtr^[m+1]);
            found := (abs(e.VecPtr^[m]) + dd = dd);
            IF NOT found THEN m := m + 1;
         END;
         IF m <> l THEN
         BEGIN
            iter := iter + 1;
            IF iter = 300 THEN
            BEGIN
               Error('Too many iterations in TQLI');
               Exit;
            END;
            g := (d.VecPtr^[l+1]-d.VecPtr^[l])/(2.0*e.VecPtr^[l]);
            r := sqrt((g*g)+1.0);
            g := d.VecPtr^[m]-d.VecPtr^[l]+e.VecPtr^[l]/(g+Sign(r,g));
            c := 1.0;
            s := c;
            p := 0.0;
            FOR i := m-1 DOWNTO l DO
            BEGIN
               f := s*e.VecPtr^[i];
               b := c*e.VecPtr^[i];
               IF (abs(f) >= abs(g)) THEN
               BEGIN
                  c := g/f;
                  r := sqrt((c*c)+1.0);
                  e.VecPtr^[i+1] := f*r;
                  s := 1.0/r;
                  c := c*s;
               END
               ELSE
               BEGIN
                  s := f/g;
                  r := sqrt((s*s)+1.0);
                  e.VecPtr^[i+1] := g*r;
                  c := 1.0/r;
                  s := s*c;
               END;
               g := d.VecPtr^[i+1]-p;
               r := (d.VecPtr^[i]-g)*s+2.0*c*b;
               p := s*r;
               d.VecPtr^[i+1] := g+p;
               g := c*r-b;
               FOR k := 1 TO d.Size DO
               BEGIN
                  f := z.Value(k,i+1);
                  z.Put(k,i+1,s*z.Value(k,i)+c*f);
                  z.Put(k,i,c*z.Value(k,i)- s*f);
               END;
            END;
            d.VecPtr^[l] := d.VecPtr^[l]-p;
            e.VecPtr^[l] := g;
            e.VecPtr^[m] := 0.0;
         END;
      UNTIL m = l;
   END;
END;

FUNCTION myMousePosn(VAR X,Y:REAL; VNum,SNum:integer; VAR Inside:boolean) : WORD;
VAR XFactor,YFactor: REAL;
    xGlobal,yGlobal,button: integer;
BEGIN MouseGlobalPosn(xGlobal,yGlobal,button);
      WITH Views[VNum] DO
           WITH GraphScales[SNum] DO
                BEGIN xFactor := (xmax-xmin)/(Vx2-Vx1);
                      X := (xFactor*(xGlobal-Vx1)) + xmin;
                      yFactor := (ymax-ymin)/(Vy2-Vy1);
                      Y := -(yFactor*(yGlobal-Vy1)) + ymax;
                      Inside := false;
                      IF (xGlobal>=Vx1) AND (xGlobal<=Vx2)
                         AND (yGlobal>=Vy1) AND (yGlobal<=Vy2)
                         THEN Inside := true;
                END;
      myMousePosn := button;
END;


(***************************************************************
    Utility Procedures
****************************************************************)

PROCEDURE DescribeLCAO;
CONST
   InitHelpScreen   : HelpScrType =
   ('                                                 ',
    '                 LCAO Workbench                  ',
    '                                                 ',
    '         Written by Steve Spicklemire            ',
    '  Deparment of Physics and Earth-Space Sciences  ',
    '          University of Indianapolis             ',
    '            Indianapolis, IN 46227               ',
    '        Copyright (C) John Wiley & Sons          ',
    '                                                 ',
    ' This program provides an interactive means of   ',
    ' investigating the Linear Combination of Atomic  ',
    ' Orbitals (LCAO) as an approach to the study of  ',
    ' solid state physics.  The user can create       ',
    ' ''clusters'' of very simple one electron single ',
    ' state 2D atoms. These then combine according to ',
    ' the principles of Quantum Mechanics. The user   ',
    ' can interactively adjust the position, size,    ',
    ' and number of atoms in the system. By selecting ',
    ' a particular cluster state energy, the student  ',
    ' can then view the cluster state wave-function,  ',
    ' and the total cluster energy. Atoms are created ',
    ' by simply double-clicking in the Atom-View      ',
    ' window. States are selected in the EnergyView   ',
    ' window.                                         ',
    '                                                 ');
BEGIN
     Help(InitHelpScreen);
END;

PROCEDURE HelpScreen;
var
   HelpScr : HelpScrType;
BEGIN            { 12345678901234567890123456789012345678901234567890}
   HelpScr[01] := '';
   HelpScr[02] := ' Mouse operations in Atom-Window (left):';
   HelpScr[03] := '  clicking on atom - selects it';
   HelpScr[04] := '  double clicking on background - creates atom at  ';
   HelpScr[05] := '     the given position';
   HelpScr[06] := '  double clicking on atom - gives you atom ';
   HelpScr[07] := '     properties screen';
   HelpScr[08] := '  clicking and draging atom - moves it';
   HelpScr[09] := '  clicking and draging square on the right side ';
   HelpScr[10] := '     of the atom - changes radius';
   HelpScr[11] := ' ';
   HelpScr[12] := ' Mouse operations in Energy-Window (right):';
   HelpScr[13] := '  clicking on energy line - selects it';
   HelpScr[14] := '';
   HelpScr[15] := ' Keyboard operations:';
   HelpScr[16] := '  Tab - togles between all atoms selecting them';
   HelpScr[17] := '  Ctrl-a - adds atom at the origin';
   HelpScr[18] := '  Enter - gives you properties screen of selected ';
   HelpScr[19] := '     atom ';
   HelpScr[20] := '  Arrow Keys - move selected atom';
   HelpScr[21] := '';
   HelpScr[22] := ' See also options in "Build" menu.';
   HelpScr[23] := '';
   HelpScr[24] := ' For more information about the program see  ';
   HelpScr[25] := ' "File/About Program" option. ';
   Help(HelpScr);
END;

PROCEDURE SetUpHotKeys;
BEGIN
   With HotKeys do
   begin
      init(2);
      key[1] := 'F1-Help';
      key[2] := 'F10-Menu';
   end;
END;

PROCEDURE SetUpMainMenu( var aMenu : TMenu) ;
BEGIN
   aMenu.Init;
   WITH aMenu DO
   BEGIN
      column(1,'File');
       row(1,1, 'About CUPS');
       row(1,2, 'About Program');
       row(1,3, 'Configure     ');
       row(1,4, '-------------------');
       row(1,5, 'Exit Program  alt-x');
      column(2, 'Build');
       row(2,1, 'Copy                ctrl-c');
       row(2,2, 'Cut                 ctrl-x');
       row(2,3, 'Paste               ctrl-v');
       row(2,4, 'Clear               ctrl-z');
       row(2,5, '--------------------------');
       row(2,6, 'Add Atom at origin  ctrl-a');
       row(2,7, 'Adjust Atom...     <Enter>');
      column(3, 'View');
       row(3,1, '---- Left  ViewPort ---------');
       row(3,2, 'Bare Atoms');
       row(3,3, 'Cluster State Wave Function');
       row(3,4, '---- Right ViewPort ---------');
       row(3,5, 'Selected Atom Wave Function');
       row(3,6, 'Cluster Energies');
      RowActivate(1,4,FALSE);
      RowActivate(2,5,FALSE);
      RowActivate(3,1,FALSE);
      RowActivate(3,4,FALSE);
      RowChecked(3,2,TRUE);
      RowChecked(3,6,TRUE);
   END;
END;

PROCEDURE HandleMenu( var aMenu : TMenu);
VAR
   helpText : HelpScrType;
BEGIN
   WITH aMenu DO
   CASE colChosen OF
   1 : { File menu }
     CASE rowChosen OF
            1: AboutCUPS;
            2:          { About LCAO }
              BEGIN
                   DescribeLCAO;
{                   DisplayMainScreen;  }
              END;
            3: Configuration;
            5: quit := true;
         END; { CASE rowChosen }
 2 : { Edit Menu }
         CASE rowChosen OF
            1    : model.Copy;
            2    : model.Cut;
            3    : model.Paste;
            4    : model.Clear;
            6    : model.AddAtom( 0, 0); { Add an atom }
            7    : model.GetSelectionData;
         END;
 3 : { View Menu }
         CASE rowChosen OF
            2 :  BEGIN
                    model.atomView   := noWaveFuncs;
                    model.DrawAtomView( TRUE );
                    RowChecked(3,2,TRUE);
                    RowChecked(3,3,FALSE);
                 END;
            3 :  BEGIN
                    model.atomView   := clusterFuncView;
                    model.clusterStates.whichState := 1;
                    model.DrawEnergyView( TRUE );
                    model.DrawAtomView( TRUE );
                    RowChecked(3,3,TRUE);
                    RowChecked(3,2,FALSE);
                 END;
            5:
              BEGIN
                    model.energyView := atomicWaveFuncView;
                    model.SelectSomething;
                    model.DrawEnergyView( TRUE );
                    RowChecked(3,5,TRUE);
                    RowChecked(3,6,FALSE);
               END;
            6 :
               BEGIN
                    model.energyView := clusterEnergyView;
                    model.clusterStates.whichState := 1;
                    model.DrawEnergyView( TRUE );
                    RowChecked(3,6,TRUE);
                    RowChecked(3,5,FALSE);
               END;
         END;
     END;
END;


Procedure HandleHotKeys(key : byte);
begin
   case key of
    1: HelpScreen;
    2: IF MyMenu.Chosen THEN HandleMenu( myMenu );
   end;
end;

PROCEDURE HandleKeybd( thisEvent : EventTp );
CONST
      extUP    = 72;  extDOWN  = 80;  extLEFT  = 75;  extRIGHT = 77;
      extALT_X = 45;
      CtrlZ = 26; CtrlX = 24; CtrlC = 03; CtrlV = 22; CtrlA = 1;
BEGIN
   IF thisEvent.KeyPressed THEN
      CASE thisEvent.readKey OF
           chr(CtrlA) : model.AddAtom(0, 0);
           chr(CtrlX) : model.Cut;
           chr(CtrlV) : model.Paste;
           chr(CtrlC) : model.Copy;
           chr(CtrlZ) : model.Clear;
      END;
   IF thisEvent.extendedKey THEN
      CASE thisEvent.readKey OF
           chr(extALT_X) : begin
                              quit := true;
                              thisEvent.readKey := chr(1)
                           end;
           chr(extUP) :
              BEGIN
                   model.MoveSelection(0,DELTAPOS);
                   if model.energyView = clusterEnergyView THEN
                      model.DrawEnergyView(TRUE);
              END;
           chr(extDOWN)  :
              BEGIN
                   model.MoveSelection(0,-DELTAPOS);
                   if model.energyView = clusterEnergyView THEN
                      model.DrawEnergyView(TRUE);
              END;
           chr(extRIGHT) :
              BEGIN
                   model.MoveSelection(DELTAPOS,0);
                   if model.energyView = clusterEnergyView THEN
                      model.DrawEnergyView(TRUE);
              END;
           chr(extLEFT)  :
              BEGIN
                   model.MoveSelection(-DELTAPOS,0);
                   if model.energyView = clusterEnergyView THEN
                      model.DrawEnergyView(TRUE);
              END;
      END
   ELSE
      CASE thisEvent.readKey OF
            chr(9)  : model.SelectNextAtom;  { Tab to next atom }
            chr(13) : model.GetSelectionData;
      END;
END;


PROCEDURE DisplayMainScreen;
BEGIN
   HideCursor;
   SelectMuppetPort;
   ClearMuppetPort;
   myMenu.Display;
   HotKeys.Display;
   OpenViewPort( ATOMVIEWPORT );
   model.DrawEnergyView( TRUE );
   model.draw;
   ShowCursor;
END;


PROCEDURE Initialize;
VAR
   helpText : HelpScrType;
BEGIN
   quit := false;      { Don't quit yet. }
   HaltIfError := false;   { keep going if error }
   ErrorFound := false;    { no errors yet }
   DefineViewPort( ATOMVIEWPORT, ATOMSX1, ATOMSX2, ATOMSY1, ATOMSY2);
   DefineViewPort( ENERGYVIEWPORT, ENERGYX1, ENERGYX2, ENERGYY1, ENERGYY2);
   DefineScale( ENERGYVIEWPORT, -DEFAULTRADIUS/3, 2.0*DEFAULTRADIUS,
                                -0.2, 1.2);
   myContour.Init;
   model.Init;
{   We want atoms to be round. We want the scale in the ATOMVIEWPORT
   to be set up so that x and y distance are the same on the screen.}
   model.xSize := 10*DEFAULTRADIUS;
   model.AdjustYSize;
   bessel := NEW( besselObjPtr , Init( MAXNATOMIC + 1, 512, 20.0 ));
   IF bessel = NIL THEN
      Error('Not enough memory for bessel functions!');
   SetUpMainMenu( myMenu);
   SetUpHotKeys;
   DisplayMainScreen;
   DescribeLCAO;
END;

VAR
   key : byte;
BEGIN
   CupsInit;      (* Init the Cups (and MUPPET) stuff ...      *)
   Initialize;
   WHILE NOT quit DO
   BEGIN
      CheckForEvents;
      IF event.keypressed THEN HandleKeybd( event );
      IF HotKeys.Pressed(key) then HandleHotKeys(key);
      IF MyMenu.Activated THEN HandleMenu( myMenu ) ELSE
      IF event.mouseClicked THEN model.HandleMouseClick( event );
   END;
   HideCursor;
   model.CleanUp;
   CUPSdone;
END.
