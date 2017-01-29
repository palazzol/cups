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

PROGRAM GM_RaceTrack;
{+-------------------------------------------------------------------}
{| Description: RaceTrack
{|    A race car must be manuevered around a race track without running
{|    of the road.  Arrow keys are used to generate acceleration
{| Author:  Randall Jones
{| Date:  {5/24/94}
{+-------------------------------------------------------------------}
{| Uses: Motion Generator from MECHANICS section of CUPS.
{|       (1-14-93) CUPS utilities
{+-------------------------------------------------------------------}

USES Graph,Crt,CUPSMUPP,CUPS,CUPSgui,CupsGrph,CUPSFunc,GMUtil1,GMUtil2;
VAR
   xOld:Real;   {Used to watch for finish-line crossing}
{+---------------------------------------------------------------------}
{| This procedure is called once to define the names and units of the
{|    Dynamical variables,
{|    Parameters that will be used to describe the force,
{|    User-defined dynamical functions.
{+---------------------------------------------------------------------}
PROCEDURE DefForceParms;FAR;
   BEGIN
      ForceDef.ForceTitle := 'A day at the races       (Do not use F3, F4, F5 or F6)';
      ForceDef.ForceDesc := 'Fnet = (Fx - c1 vx)i + (Fy - c1 vy)j ';
     {+----------------------------------------------------------------}
     {| Specify name and units of "time" variable.
     {+----------------------------------------------------------------}
                 {Name }  {Units     }
      GenTimeVar('t',     's'         );
     {+----------------------------------------------------------------}
     {| Specify name and units of Dynamical Variables (Maximum of 3)
     {+----------------------------------------------------------------}
                {Name } {Vel   } {Acc  }  {Units     }
      GenDynVar('x',    'vx',    'ax',    'm'         );
      GenDynVar('y',    'vy',    'ay',    'm'         );
     {+----------------------------------------------------------------}
     {| Specify name and units of Force Parameters (Maximum of 10)
     {+----------------------------------------------------------------}
                   {Name } {Units     }  {Description         }
      GenForceParm('m',    'kg',         'Mass'               );
      GenForceParm('Fx',   'N',          'x Applied Force'    );
      GenForceParm('Fy',   'N',          'y Applied Force'    );
      GenForceParm('c',    'N/(m/s)',    'Linear Damping'     );
     {+----------------------------------------------------------------}
     {| Specify name and units of User-defined Dynamical Functions
     {|    (Maximum of 10)
     {+----------------------------------------------------------------}
                {Name } {Units     }  {Description         }
      GenDynFun('EK',   'J',          'Kinetic Energy'     );
     {+----------------------------------------------------------------}
     {| Specify name of start-up configuration file
     {+----------------------------------------------------------------}
      ForceDef.ConfigFileName := 'GMRace.PRM';
    END;



{+---------------------------------------------------------------------}
{| The four procedures/functions below include the following variables
{| as arguments:
{|    t,                                  {time variable}
{|    x,vx,ax,                            {1st dynamical variable}
{|    y,vy,ay,                            {2nd dynamical variable}
{|    z,vz,az,                            {3rd dynamical variable}
{|    P1,P2,P3,P4,P5,P6,P7,P8,P9,P10      {Force Parameters}
{| The names of these variables may be changed to match the definitions
{| given above in PROCEDURE DefForce, but the order must remain as above.
{| A number of global variables such as initial values of the dynamical
{| variables and limits for graph variables are also available to these
{| routines.  Refer to the list of global variables in GenMot.pas or
{| in the text chapter.
{+---------------------------------------------------------------------}

{+---------------------------------------------------------------------}
{| This procedure is called to generate the current values of the
{| accelerations.  It is called by the numerical integration
{| procedures and by the graphing procedures.  Only the acceleration
{| variables should be modified by this routine.
{+---------------------------------------------------------------------}

   PROCEDURE CalcAccel(VAR
                       t,                      {time variable}
                       x,vx,ax,                {x-dynamical variable}
                       y,vy,ay,                {Not Used}
                       z,vz,az,                {Not Used}
                       m,Fx,Fy,c,              {Force Parameters}
                       P5,P6,P7,P8,P9,P10      {Not Used}
                                         :Real );FAR;
   VAR
      FNetx,FNety:Real;
   BEGIN
      FNetx := Fx - c*vx;
      FNety := Fy - c*vy;
      ax := FNetx/m;
      ay := FNety/m
   END;

{+---------------------------------------------------------------------}
{| This procedure is called by the graphics routines to generate the
{| current value of the User-defined dynamical function specified by
{| FunName.  FunName will match one of the function names defined in
{| PROCEDURE DefForceParms.
{| Note: None of the FUNCTION variables should be changed by this routine.
{+---------------------------------------------------------------------}
   FUNCTION CalcDynFun(FunName:String5; VAR
                       t,                   {time variable}
                       x,vx,ax,             {x-dynamical variable}
                       y,vy,ay,             {Not used}
                       z,vz,az,             {Not Used}
                       m,Fx,Fy,c,              {Force Parameters}
                       P5,P6,P7,P8,P9,P10      {Not Used}
                              :Real ):Real; FAR;
   BEGIN
      IF FunName='EK' THEN
         CalcDynFun := m*(vx*vx+vy*vy)/2;
   END;


{+---------------------------------------------------------------------------}
{| Graphics Routines: These can be modified to generate graphics:
{|   InitWindow     Called each time a window is opened. Used to draw
{{                  initial graphics in windows.
{|   AnimateWindow  Called each time a window is updated.  Used to
{|                  generate animations.
{| These routines must be present, even if they are not used.
{|
{|      Note: yAxisVar,xAxisVar:String[5] identify window being drawn.
{|
{|      Useful Animation Procedures:
{|          PlotLine(x1,y1,x2,y2)
{|          PutCircle(x,y)
{|          DrawSpring(x1,y1,x2,y2,width)
{|          DrawVector(x,y,Vy,Vx)          { (Vx,Vy) at point (x,y) }
{|          CalcDynFun(FunName...)         {Returns user-defined fun val}
{+---------------------------------------------------------------------------}
   PROCEDURE InitWindow(yAxisVar,xAxisVar:String5; Erase:Boolean; VAR
                       t,                      {time variable}
                       x,vx,ax,                {x-dynamical variable}
                       y,vy,ay,                {Not Used}
                       z,vz,az,                {Not Used}
                       m,Fx,Fy,c,              {Force Parameters}
                       P5,P6,P7,P8,P9,P10      {Not Used}
                                         :Real ); FAR;
   CONST
      TrackWidth=50; Length=50; Width=80;
   VAR
      RaceCourse:ARRAY[1..44] OF PointType;
      i,j,sign,radius:Integer;
      xPos,yPos,xOld,yOld:Real;
      Envir:TEnvironment;
   BEGIN;
      IF (NOT Erase) AND (yAxisVar='y') AND (xAxisVar='x') THEN
         BEGIN
            Fx := 0;
            Fy := 0;
            xOld := 0;
            SetWriteMode(NormalPut);
            WITH RaceCourse[1] DO Map(-200,-150,x,y);
            WITH RaceCourse[2] DO Map( 200,-150,x,y);
            WITH RaceCourse[3] DO Map( 200, 150,x,y);
            WITH RaceCourse[4] DO Map(-200, 150,x,y);
            SetFillStyle(xHatchFill,Green); FillPoly(4,RaceCourse);
            FOR j := 1 to 2 DO
               BEGIN
                  IF j=1
                     THEN BEGIN SetFillStyle(SolidFill,Black); radius := Width+TrackWidth; END
                     ELSE BEGIN SetFillStyle(xHatchFill,Green); radius := Width; END;
                  WITH RaceCourse[1] DO Map(-Length,Radius,x,y);
                  WITH RaceCourse[2] DO Map( Length,Radius,x,y);
                  For i := 1 to 20 DO WITH RaceCourse[2+i] DO
                     Map(Length+radius*SIN(i*Pi/20),radius*Cos(i*Pi/20),x,y);
                  WITH RaceCourse[23] DO Map( Length,-Radius,x,y);
                  WITH RaceCourse[24] DO Map(-Length,-Radius,x,y);
                  For i := 1 to 20 DO WITH RaceCourse[24+i] DO
                     Map(-Length-radius*SIN(i*Pi/20),-radius*Cos(i*Pi/20),x,y);
                  DrawPoly(44,RaceCourse);
                  FillPoly(44,RaceCourse);
                  SetFillStyle(SolidFill,Black);
               END;
            PlotLine(0,-Width,0,-Width-TrackWidth);
         END;

   END;

   PROCEDURE AnimateWindow(yAxisVar,xAxisVar:String5; Erase:Boolean; VAR
                       t,                      {time variable}
                       x,vx,ax,                {x-dynamical variable}
                       y,vy,ay,                {Not Used}
                       z,vz,az,                {Not Used}
                       m,Fx,Fy,c,              {Force Parameters}
                       P5,P6,P7,P8,P9,P10      {Not Used}
                                         :Real ); FAR;
   BEGIN
      IF (yAxisVar='y') AND (xAxisVar='x') THEN
         BEGIN
            IF (NOT Erase) THEN
               BEGIN
                  IF (x<-190) OR (x>190) OR (y<-145) OR (y>145) THEN
                     BEGIN
                        ANNOUNCE('Clear Graphs and try again');
                        RunFlag := False;
                     END;
                  IF (xOld<0) AND (X>0) THEN
                     BEGIN
                        BEEP;
                        Announce('YOU''VE REACHED THE FINISH LINE');
                        RunFlag := False;
                     END;
                  xOld := x;
                  IF Event.ExtendedKey THEN
                     CASE Event.ReadKey OF
                        'H': Fy := Fy + 0.2;
                        'P': Fy := Fy - 0.2;
                        'M': Fx := Fx + 0.2;
                        'K': Fx := Fx - 0.2;
                     END; {CASE}
               END;
            IF NOT((Fx=0) AND (Fy=0)) THEN DrawVector(x,y,20*Fx,20*Fy);
         END;
   END;

{+--- This is the main routine.  RunMotSim is found in SMUtil.pas ----+}
BEGIN
   RunMotSim(DefForceParms,CalcAccel,CalcDynFun,InitWindow,AnimateWindow);
{+---------------------------------------------------------------------------}
{| Note: If compile error appears here, make certain each procedure defined
{|       above has the correct number of arguments and is followed by "FAR".
{+---------------------------------------------------------------------------}
END.