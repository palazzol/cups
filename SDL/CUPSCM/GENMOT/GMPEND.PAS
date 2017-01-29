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

PROGRAM GM_Pendulum;
{+-------------------------------------------------------------------}
{| Description:  Pendulum with periodic driving torque and
{|               resistive torque
{| Author:  Randall Jones
{| Date:  {5/24/93}
{+-------------------------------------------------------------------}
{| Uses: Motion Generator from MECHANICS section of CUPS.
{|       (1-04-93) CUPS utilities
{| Date:  {5/24/93}
{+-------------------------------------------------------------------}

USES Graph,Crt,CUPSMUPP,CUPS,CUPSgui,CupsGrph,CUPSFunc,GMUtil1,GMUtil2;

{+---------------------------------------------------------------------}
{| This procedure is called once to define the names and units of the
{|    Dynamical variables,
{|    Parameters that will be used to describe the force,
{|    User-defined dynamical functions.
{| Note: CHR(233) is 'Theta', CHR(224) is 'alpha'
{+---------------------------------------------------------------------}
PROCEDURE DefForceParms;FAR;
   BEGIN
      ForceDef.ForceTitle := 'Motion of a damped, driven pendulum';
      ForceDef.ForceDesc := Concat('m L^2 ',CHR(224),' = -mgL sin',
                                   CHR(233),' - c1 w - T_d cos(w_d t)');
     {+----------------------------------------------------------------}
     {| Specify name and units of "time" variable.
     {+----------------------------------------------------------------}
                 {Name }  {Units     }
      GenTimeVar('t',     's'         );
     {+----------------------------------------------------------------}
     {| Specify name and units of Dynamical Variables (Maximum of 3)
     {+----------------------------------------------------------------}
                {Name } {Vel   } {Acc  }  {Units     }
      GenDynVar(CHR(233),'w',CHR(224),    'rad'       );
     {+----------------------------------------------------------------}
     {| Specify name and units of Force Parameters (Maximum of 10)
     {+----------------------------------------------------------------}
                   {Name } {Units     }  {Description         }
      GenForceParm('m',    'kg',         'Mass'               );
      GenForceParm('g',    'N/kg',       'Grav. Constant'     );
      GenForceParm('c ',   'N/(rad/s)',  'Linear Damping'     );
      GenForceParm('L',    'm',          'Pendulum Length'    );
      GenForceParm('T_d',  'N-m',        'Drive Torque Magn.' );
      GenForceParm('w_d',  'rad/s',      'Drive Torque Freq.' );
     {+----------------------------------------------------------------}
     {| Specify name and units of User-defined Dynamical Functions
     {|    (Maximum of 10)
     {+----------------------------------------------------------------}
                  {Name } {Units     }  {Description         }
      GenDynFun('EK',   'J',          'Kinetic Energy'      );
      GenDynFun('Etot', 'J',          'Total Energy'        );
      GenDynFun('xPos', 'm',          'x position'          );
      GenDynFun('yPos', 'm',          'y position'          );
     {+----------------------------------------------------------------}
     {| Specify name of start-up configuration file
     {+----------------------------------------------------------------}
      ForceDef.ConfigFileName := 'GMpend.PRM';
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
                       t,                         {time variable}
                       theta,omega,alpha,         {x-dynamical variable}
                       y,vy,ay,                   {Not Used}
                       z,vz,az,                   {Not Used}
                       m,g,c,L,T_d,w_d,           {Force Parameters}
                       P7,P8,P9,P10               {Not Used}
                                   :Real );FAR;
   VAR
      Torque,MomentOfInertia:Real;
   BEGIN
      MomentOfInertia := m*L*L;
      Torque := - m*g*L*SIN(theta) - c*omega + T_d*cos(w_d*t);
      alpha := Torque/MomentOfInertia;
   END;

{+---------------------------------------------------------------------}
{| This procedure is called by the graphics routines to generate the
{| current value of the User-defined dynamical function specified by
{| FunName.  FunName will match one of the function names defined in
{| PROCEDURE DefForceParms.
{| Note: None of the FUNCTION variables should be changed by this routine.
{+---------------------------------------------------------------------}
   FUNCTION CalcDynFun(FunName:String5; VAR
                       t,                         {time variable}
                       theta,omega,alpha,         {x-dynamical variable}
                       y,vy,ay,                   {Not Used}
                       z,vz,az,                   {Not Used}
                       m,g,c,L,T_d,w_d,           {Force Parameters}
                       P7,P8,P9,P10               {Not Used}
                              :Real ):Real; FAR;
   BEGIN
      IF FunName='EK' THEN
         CalcDynFun := m*Pwr(omega*L,2)/2
      ELSE IF FunName='Etot' THEN
         CalcDynFun := -m*g*L*COS(theta) + m*Pwr(omega*L,2)/2
      ELSE IF FunName='xPos' THEN
         CalcDynFun := L*SIN(theta)
      ELSE IF FunName='yPos' THEN
         CalcDynFun := -L*COS(theta);
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
                       t,                         {time variable}
                       theta,omega,alpha,         {x-dynamical variable}
                       y,vy,ay,                   {Not Used}
                       z,vz,az,                   {Not Used}
                       m,g,c,L,T_d,w_d,           {Force Parameters}
                       P7,P8,P9,P10               {Not Used}
                                   :Real ); FAR;
   BEGIN;   END;

   PROCEDURE AnimateWindow(yAxisVar,xAxisVar:String5; Erase:Boolean; VAR
                       t,                         {time variable}
                       theta,omega,alpha,         {x-dynamical variable}
                       y,vy,ay,                   {Not Used}
                       z,vz,az,                   {Not Used}
                       m,g,c,L,T_d,w_d,           {Force Parameters}
                       P7,P8,P9,P10               {Not Used}
                                   :Real ); FAR;
   VAR
      xLoc,yLoc:Real;
      x1,x2,y1:Real;
   BEGIN
      IF (yAxisVar='yPos') AND (xAxisVar='xPos') THEN
         BEGIN
            SetColor(2);
            xLoc := L*sin(theta);
            yLoc := -L*cos(theta);
            PlotLine(0,0,xLoc,yLoc);
            IF T_d<>0 THEN
               BEGIN
                  x1 := xLoc/8;
                  y1 := yLoc/8;
                  x2 := 1.2 + 0.2*COS(w_d*t);
                  DrawSpring(x1,y1,x2,y1,0.15);
                  PlotLine(x2,0.2,x2,-0.2);
               END;
         END;
   END;

{+--- This is the main routine.  RunMotSim is found in SMUtil ------+}
BEGIN
   RunMotSim(DefForceParms,CalcAccel,CalcDynFun,InitWindow,AnimateWindow);
{+---------------------------------------------------------------------------}
{| Note: If compile error appears here, make certain each procedure defined
{|       above has the correct number of arguments and is followed by "FAR".
{+---------------------------------------------------------------------------}
END.