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

PROGRAM GM_Generic1D;
{+-------------------------------------------------------------------}
{| Description:  Generic force for many problems:
{| Author:  Randall Jones
{| Date:  {5/24/93}
{+-------------------------------------------------------------------}
{| Uses: Motion Generator from MECHANICS section of CUPS.
{|       (1-14-93) CUPS utilities
{| Date:  {1/12/93}
{+-------------------------------------------------------------------}

USES Graph,Crt,CUPSMUPP,CUPS,CUPSgui,CupsGrph,CUPSFunc,GMUtil1,GMUtil2;

{+---------------------------------------------------------------------}
{| This procedure is called once to define the names and units of the
{|    Dynamical variables,
{|    Parameters that will be used to describe the force,
{|    User-defined dynamical functions.
{+---------------------------------------------------------------------}
PROCEDURE DefForceParms;FAR;
   BEGIN
      ForceDef.ForceTitle := 'Generic Motion in 1-D';
      ForceDef.ForceDesc :=
'F = -mg - c1 v - c2 v |v| - k0 sign(x) - k1 x - k2 x^3 + a1 t + F0cos(wt)';
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
      GenForceParm('g',    'N/kg',       'Grav. Constant'     );
      GenForceParm('c1',   'N/(m/s)',    'Linear Damping'     );
      GenForceParm('c2',   'N/(m/s)^2',  'Quadratic Damping'  );
      GenForceParm('k0',   'N',          'Constant Restoring' );
      GenForceParm('k1',   'N/m',        'Linear Restoring'   );
      GenForceParm('k2',   'N/m^3',      'Cubic Restoring F'  );
      GenForceParm('a1',   'N/s',        'Linear time depend' );
      GenForceParm('F0',   'N',          'Periodic Driving F' );
      GenForceParm('w',    'rad/s',      'Frequency of F0'    );
     {+----------------------------------------------------------------}
     {| Specify name and units of User-defined Dynamical Functions
     {|    (Maximum of 10)
     {+----------------------------------------------------------------}
                {Name } {Units     }  {Description         }
      GenDynFun('EK',   'Joules',     'Kinetic Energy (J)'  );
      GenDynFun('U',    'Joules',     'Potential Energy (J)');
      GenDynFun('Etot', 'Joules',     'Total Energy (J)'    );
     {+----------------------------------------------------------------}
     {| Specify name of start-up configuration file
     {+----------------------------------------------------------------}
      ForceDef.ConfigFileName := 'GMGener.PRM';
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
                       t,                                {time variable}
                       x,vx,ax,                   {x-dynamical variable}
                       y,vy,ay,                               {Not Used}
                       z,vz,az,                               {Not Used}
                       m,g,c1,c2,k0,k1,k2,a1,F0,w     {Force Parameters}
                                              :Real );FAR;
   VAR
      Force:Real;
   BEGIN
      Force := -m*g - c1*vx - c2*vx*ABS(vx) - k1*x - k2*x*x*x +
               a1*t  +F0*COS(w*t);
      IF k0<>0 THEN IF x>0 THEN Force := Force - k0
                           ELSE Force := Force + k0;
      ax := Force/m;
   END;

{+---------------------------------------------------------------------}
{| This procedure is called by the graphics routines to generate the
{| current value of the User-defined dynamical function specified by
{| FunName.  FunName will match one of the function names defined in
{| PROCEDURE DefForceParms.
{| Note: None of the FUNCTION variables should be changed by this routine.
{+---------------------------------------------------------------------}
   FUNCTION CalcDynFun(FunName:String5; VAR
                       t,                                {time variable}
                       x,vx,ax,                   {x-dynamical variable}
                       y,vy,ay,                               {Not Used}
                       z,vz,az,                               {Not Used}
                       m,g,c1,c2,k0,k1,k2,a1,F0,w     {Force Parameters}
                              :Real ):Real; FAR;
   BEGIN
      IF FunName ='EK' THEN
         CalcDynFun := m*(vx*vx)/2
      ELSE IF FunName='U' THEN
         CalcDynFun := m*g*x + k0*Abs(x) + k1*x*x/2 + k2*Pwr(x,4)/4
      ELSE IF FunName='Etot' THEN
         CalcDynFun := m*(vx*vx)/2 + m*g*x + k0*Abs(x) + k1*x*x/2
                                           + k2*Pwr(x,4)/4
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
                       t,                                {time variable}
                       x,vx,ax,                   {x-dynamical variable}
                       y,vy,ay,                               {Not Used}
                       z,vz,az,                               {Not Used}
                       m,g,c1,c2,k0,k1,k2,a1,F0,w     {Force Parameters}
                              :Real ); FAR;
   BEGIN;
   END;

   PROCEDURE AnimateWindow(yAxisVar,xAxisVar:String5; Erase:Boolean; VAR
                       t,                                {time variable}
                       x,vx,ax,                   {x-dynamical variable}
                       y,vy,ay,                               {Not Used}
                       z,vz,az,                               {Not Used}
                       m,g,c1,c2,k0,k1,k2,a1,F0,w     {Force Parameters}
                              :Real ); FAR;
   BEGIN
   END;

{+--- This is the main routine.  RunMotSim is found in SMUtil.pas ----+}
BEGIN
   RunMotSim(DefForceParms,CalcAccel,CalcDynFun,InitWindow,AnimateWindow);
{+---------------------------------------------------------------------------}
{| Note: If compile error appears here, make certain each procedure defined
{|       above has the correct number of arguments and is followed by "FAR".
{+---------------------------------------------------------------------------}
END.