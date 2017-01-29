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

PROGRAM GM_1DGrav;
{+-------------------------------------------------------------------}
{| Description:  Motion in 1-D with gravity and air resistance
{| Author:  Randall Jones
{| Date:  {5/24/93}
{+-------------------------------------------------------------------}
{| Uses: Motion Generator from MECHANICS section of CUPS.
{|       (1-04-93) CUPS utilities
{+-------------------------------------------------------------------}

USES Graph,Crt,CUPSMUPP,CUPS,CUPSgui,CupsGrph,CUPSFunc,GMUtil1,GMUtil2  ;
{+---------------------------------------------------------------------}
{| This procedure is called once to define the names and units of the
{|    Dynamical variables,
{|    Parameters that will be used to describe the force,
{|    User-defined dynamical functions.
{+---------------------------------------------------------------------}
PROCEDURE DefForceParms;FAR;
   BEGIN
      ForceDef.ForceTitle := '1-D Motion with Force of Gravity and Air Resistance';
      ForceDef.ForceDesc := 'F = -mg - c1 v - c2 v |v|';
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
     {+----------------------------------------------------------------}
     {| Specify name and units of Force Parameters (Maximum of 10)
     {+----------------------------------------------------------------}
                   {Name } {Units     }  {Description         }
      GenForceParm('m',    'kg',         'Mass'               );
      GenForceParm('g',    'N/kg',       'Grav. Const'        );
      GenForceParm('c1',   'N/(m/s)',    'Linear Damping'     );
      GenForceParm('c2',   'N/(m/s)^2',  'Quadratic Damping'  );
     {+----------------------------------------------------------------}
     {| Specify name and units of User-defined Dynamical Functions
     {|    (Maximum of 10)
     {+----------------------------------------------------------------}
                  {Name } {Units     }  {Description         }
      GenDynFun('EK',   'J',     'Kin. Energy'               );
      GenDynFun('U',    'J',     'Pot. Energy'               );
      GenDynFun('Etot', 'J',     'Tot. Energy'               );
     {+----------------------------------------------------------------}
     {| Specify name of start-up parameter file
     {+----------------------------------------------------------------}
      ForceDef.ConfigFileName := 'GM1dGrav.PRM';
    END;

{+---------------------------------------------------------------------}
{| The four procedures/functions below include the following variables
{| as arguments:
{|    t,                                  {time variables}
{|    x,vx,ax,                            {1st dynamical variable}
{|    y,vy,ay,                            {2nd dynamical variable}
{|    z,vz,az,                            {3rd dynamical variable}
{|    P1,P2,P3,P4,P5,P6,P7,P8,P9,P10      {Force Parameters}
{| The names of these variables may be changed to match the definitions
{| given above in PROCEDURE DefForce, but the order must remain as above.
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
                       m,g,c1,c2,              {Force Parameters}
                       P5,P6,P7,P8,P9,P10      {Not Used}
                                         :Real );FAR;
   VAR
      Force:Real;
   BEGIN
      Force := -m*g - c1*vx - c2*vx*ABS(vx);
      ax := Force/m;
   END;

{+---------------------------------------------------------------------}
{| This procedure is called by the graphics routines to generate the
{| current value of the User-defined dynamical function specified by
{| FunName.  FunName will match one of the function names defined in
{| PROCEDURE DefForceParms.
{| Note: None of the function variables should be changed by this routine.
{+---------------------------------------------------------------------}
   FUNCTION CalcDynFun(FunName:String5; VAR
                       t,                   {time variable}
                       x,vx,ax,             {x-dynamical variable}
                       y,vy,ay,             {Not used}
                       z,vz,az,             {Not Used}
                       m,g,c1,c2,           {Force Parameters}
                       P5,P6,P7,P8,P9,P10   {Not Used}
                              :Real ):Real; FAR;
   BEGIN
      IF FunName='EK' THEN
         CalcDynFun := m*(vx*vx)/2
      ELSE IF FunName='U' THEN
         CalcDynFun := m*g*x
      ELSE IF FunName='Etot' THEN
         CalcDynFun := m*SQR(vx)/2 + m*g*x;
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
                       m,g,c1,c2,              {Force Parameters}
                       P5,P6,P7,P8,P9,P10      {Not Used}
                              :Real ); FAR;
   BEGIN;   END;

   PROCEDURE AnimateWindow(yAxisVar,xAxisVar:String5; Erase:Boolean; VAR
                       t,                      {time variable}
                       x,vx,ax,                {x-dynamical variable}
                       y,vy,ay,                {Not Used}
                       z,vz,az,                {Not Used}
                       m,g,c1,c2,              {Force Parameters}
                       P5,P6,P7,P8,P9,P10      {Not Used}
                              :Real ); FAR;
   BEGIN
   END;

{+--- This is the main routine.  RunMotSim is found in GMUtil.pas ----+}
BEGIN
   RunMotSim(DefForceParms,CalcAccel,CalcDynFun,InitWindow,AnimateWindow);
{+---------------------------------------------------------------------------}
{| Note: If compile error appears here, make certain each procedure defined
{|       above has the correct number of arguments and is followed by "FAR".
{+---------------------------------------------------------------------------}
END.