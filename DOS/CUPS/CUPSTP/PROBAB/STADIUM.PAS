           (*************************************************)
           (*************************************************)
           (**                                             **)
           (** Consortium of Upper-level Physics Software  **)
           (**             ( CUPS Project )                **)
           (**     (c) 1994 by John Wiley & Sons           **)
           (** Compiled with Utilities Ver. 1.6 (95/05/17) **)
           (**                                             **)
           (*************************************************)
           (*************************************************)

     {*************************************************}
      {**              Program STADIUM                **}
      {**                                             **}
      {** This program models a ball which is confined**}
      {** to move in a stadium-shaped container.  Its **}
      {** purpose is to illustrate a system whose     **}
      {** equation of motion is solvable but whose    **}
      {** behavior is not predictable because of its  **}
      {** extreme sensitivity to initial conditions   **}
      {** (i.e., its chaotic nature).                 **}
      {**                                             **}
      {** Because of this chaotic behavior, it is     **}
      {** appropriate to use a statistical description**}
      {** of this system.                             **}
      {**                                             **}
      {** The program has two parts: a one ball model **}
      {** and a two ball model.                       **}
      {**                                             **}
      {** The purpose of the one ball model is to     **}
      {** show that the chaotic behavior is not due   **}
      {** to the numerical inaccuracies of the        **}
      {** computer's arithmetic methods (i.e., finite **}
      {** arithmetic ... ).  It does this by allowing **}
      {** the ball to bounce a prescribed number of   **}
      {** times (n), then reversing the ball's        **}
      {** momentum and allowing the ball to bounce n  **}
      {** more times.  If the ball retraces its       **}
      {** original path and returns to its starting   **}
      {** location, any chaotic behavior is not a     **}
      {** result of the computer's arithmetic methods.**}
      {** If the ball does not return to its initial  **}
      {** location, nothing can be said about its     **}
      {** behavior.                                   **}
      {**                                             **}
      {** The two ball model illustrates the          **}
      {** difference between a well defined           **}
      {** trajectory and a chaotic one.  The concept  **}
      {** of "Predictability Horizon" is introduced.  **}
      {**                                             **}
      {** The "Predictability Horizon" is defined as  **}
      {** the time beyond which nearest neighbor      **}
      {** trajectories diverge randomly.              **}
      {**                                             **}
      {** "Nearest neighbor" trajectories are         **}
      {** trajectories whose initial conditions differ**}
      {** by one unit in the last (right-most)        **}
      {** decimal place.                              **}
      {**                                             **}
      {** For a chaotic system, decreasing the initial**}
      {** distance between nearest neighbors by       **}
      {** increasing the accuracy of the measurements **}
      {** or the precision of the computer's          **}
      {** arithmetic only changes the "Predictability **}
      {** Horizon" slightly.                          **}
      {**                                             **}
      {** While the ball's equation of motion is      **}
      {** solvable, because of its chaotic nature,    **}
      {** the position of the ball cannot be          **}
      {** determined.  This is demonstrated in the    **}
      {** two-ball model where the paths of two balls **}
      {** with the same initial momentum but initial  **}
      {** positions differing by one unit in the last **}
      {** decimal place, are drawn. Their differences **}
      {** in phase space as a function of the number  **}
      {** of bounces and the initial phase space      **}
      {** separation are plotted.  If the trajectory  **}
      {** is chaotic, the paths usually begin to      **}
      {** diverge within 20 to 50 bounces             **}
      {**                                             **}
      {** The Stadium Model demonstrates the need for **}
      {** a statistical description even for systems  **}
      {** whose equations of motion are solvable.     **}
      {**                                             **}
      {**       Written by L. B. Spornick             **}
      {**       Applied Physics Laboratory            **}
      {**      The Johns Hopkins University           **}
      {**                                             **}
      {**           March 5, 1992                     **}
      {**      Version 1.0  July, 1993                **}
      {**      Version 1.1  Nov, 1993                 **}
      {**      Version 1.2  Jan, 1994                 **}
      {**      Version 1.3  May, 1994                 **}
      {**      Version 1.4  Apr, 1995                 **}
      {**                                             **}
      {*************************************************}
      {*************************************************}

PROGRAM Stadium;

USES
   Graph, Crt, DOS, CUPSMUPP, CUPS, CUPSgui;

CONST
   deltat:
   INTEGER = 750;
   STEP:
   BOOLEAN = false;
   quitf:
   BOOLEAN = false;
   restart:
   BOOLEAN = false;
   shape:
   FillPatternType = ( $FF , $FF , $FF , $FF , $FF , $FF , $FF , $FF );
   first:
   BOOLEAN = true;
   Stadium_Output: STRING
   = 'Stadium.dat';

TYPE
   doubler = ARRAY[1..2] OF EXTENDED;
   doublei = ARRAY[1..2] OF INTEGER;
   distarray = ARRAY[1..4, 1..200] OF REAL;

VAR
   AL, AR, d, dmax, delta, dsize, r2d, sd, smax, thetair, thetal, sr: REAL;
   alpha, p, s, thetai, thetar, xi, xt, yi, yt: doubler;
   dist: distarray;
   axstart, axend, ax1, ax2, aystartl, ayendl, ayr, ay1 : integer;
   ay2, ay11, ay12, ay21, ay22, error, ib, iph, iiph, nb : integer;
   lmaxx, lmaxy, x, y, thetaid, rev: INTEGER;
   st: STRING;
   ch1: STRING[5];
   intersect, intersect_new, xd1, xd2, yd1, yd2: doublei;
   i, i1, n: LONGINT;
   ch: CHAR;
   backcolor: WORD;
   MyMenuout: TMenu;
   HotKeys: THotKeys;
   key: BYTE;

PROCEDURE HELP1;
   VAR
      helpScr: HelpScrType;
BEGIN
   helpScr[1] := '                                                  ';
   helpScr[2] := '            The One-Ball Stadium Model            ';
   helpScr[3] := '                                                  ';
   helpScr[4] := '                                                  ';
   helpScr[5] := '    The screen is split:                          ';
   helpScr[6] := '                                                  ';
   helpScr[7] := '    The left side displays the Stadium Model      ';
   helpScr[8] := '                                                  ';
   helpScr[9] := '    The white lines are the initial trajectory    ';
   helpScr[10] := '    of the ball, the red lines are the            ';
   helpScr[11] := '    reversed trajectory.                          ';
   helpScr[12] := '                                                  ';
   helpScr[13] := '    The right side displays a plot of p vs s      ';
   helpScr[14] := '    (p = momentum, s = position relative to       ';
   helpScr[15] := '    to the curve).                                ';
   helpScr[16] := '                                                  ';
   helpScr[17] := '    The white dots are the initial trajectory     ';
   helpScr[18] := '    and the red are the reversed trajectory.      ';
   helpScr[19] := '                                                  ';
   helpScr[20] := '    At the conclusion, the program will output    ';
   helpScr[21] := '    the final position of the ball.  If it is     ';
   helpScr[22] := '    equal to (or close to) the initial position,  ';
   helpScr[23] := '    then the chaotic behavior is not a function   ';
   helpScr[24] := '    of the numeric inaccuracy of the computer.    ';
   helpScr[25] := '                                                  ';
   HELP(helpScr);
END;  { PROCEDURE HELP1 }

PROCEDURE HELP2;
   VAR
      helpScr: HelpScrType;
BEGIN
   helpScr[1] := '                                                  ';
   helpScr[2] := '            The Two-Ball Stadium Model            ';
   helpScr[3] := '                                                  ';
   helpScr[4] := '                                                  ';
   helpScr[5] := '    The screen is split:                          ';
   helpScr[6] := '                                                  ';
   helpScr[7] := '    The left side displays the Stadium Model      ';
   helpScr[8] := '                                                  ';
   helpScr[9] := '    The white lines are the trajectory of the     ';
   helpScr[10] := '    first ball, the red lines are the             ';
   helpScr[11] := '    trajectory of the second ball.                ';
   helpScr[12] := '                                                  ';
   helpScr[13] := '    The right side displays plots of the          ';
   helpScr[14] := '    differences in phase space as a function of   ';
   helpScr[15] := '    the number of bounces and the initial         ';
   helpScr[16] := '    nearest neighbor separation.                  ';
   helpScr[17] := '                                                  ';
   helpScr[18] := '                                                  ';
   helpScr[19] := '                                                  ';
   helpScr[20] := '                                                  ';
   helpScr[21] := '                                                  ';
   helpScr[22] := '                                                  ';
   helpScr[23] := '                                                  ';
   helpScr[24] := '                                                  ';
   helpScr[25] := '                                                  ';
   HELP(helpScr);
END;  { PROCEDURE HELP2  }

PROCEDURE ABOUTPROGRAM;
   VAR
      helpScr: HelpScrType;
BEGIN
   helpScr[1] := '                                                  ';
   helpScr[2] := '               The Stadium Model                  ';
   helpScr[3] := '                                                  ';
   helpScr[4] := '                       by                         ';
   helpScr[5] := '                                                  ';
   helpScr[6] := '                 L.B. Spornick                    ';
   helpScr[7] := '           Applied Physics Laboratory             ';
   helpScr[8] := '          The Johns Hopkins University            ';
   helpScr[9] := '                                                  ';
   helpScr[10] := '                  April 1995                      ';
   helpScr[11] := '               Copyright (c) 1995                 ';
   helpScr[12] := '                                                  ';
   helpScr[13] := '   This program models a ball which is confined   ';
   helpScr[14] := '   to move in a stadium-shaped container.  Its    ';
   helpScr[15] := '   purpose is to illustrate a system whose        ';
   helpScr[16] := '   equation of motion is solvable but whose       ';
   helpScr[17] := '   behavior is not predictable because of its     ';
   helpScr[18] := '   extreme sensitivity to initial conditions      ';
   helpScr[19] := '   (i.e., its chaotic nature).  There are two     ';
   helpScr[20] := '   options: the one ball model which shows that   ';
   helpScr[21] := '   the chaotic behavior is not due to computer    ';
   helpScr[22] := '   numeric computations and the two ball model    ';
   helpScr[23] := '   which shows the chaotic behavior.              ';
   helpScr[24] := '                                                  ';
   helpScr[25] := '                                                  ';
   HELP(helpScr);
END;  { PROCEDURE ABOUTPROGRAM  }

PROCEDURE ReadData;
   VAR
      screen: TInputScreen;
      nr, error: INTEGER;
BEGIN
   WITH screen DO
   BEGIN
      init;
      DefineInputPort(0.1, 0.87, 0.1, 0.95);
      LoadLine('                 The Stadium Model                  ');
      LoadLine('                                                    ');
      LoadLine(' Select the Model:                                 ');
      LoadLine('  #1 One Ball                                       ');
      LoadLine('  #1 Two Ball                                       ');
      LoadLine('                                                    ');
      LoadLine(' Number of Bounces: {     } (1 to 200)            ');
      LoadLine('                                                   ');
      LoadLine(' Length of Straight Portion of the Stadium (AL):    ');
      LoadLine('  (0 -5) 0 forms a circle,5 a long thin oval {    } ');
      LoadLine('                                                    ');
      LoadLine(' The Starting Location of the Ball: {     } (0 to 2*(Pi+AL))');
      LoadLine('                                                   ');
      LoadLine(' The Initial Angle WRT the x axis:  {     } (0 to 179)');
      LoadLine('                                                   ');
      LoadLine('  Output File Name:  "                         "');
      LoadLine('                                                   ');
      LoadLine('              [   Ok   ]   [Cancel]               ');
      SetRadioButton('1', nb);
      SetNumber(3, n);
      SetNumberLimits(3, 1, 200);
      SetNumber(4, AL);
      SetNumberLimits(4, 0, 5);
      SetNumber(5, sr);
      SetNumber(6, thetair);
      SetNumberLimits(5, 0, 179);
      SetString(7, Stadium_Output);
      REPEAT
         error := 0;
         Accept;
         nb := GetRadioButton('1');
         n := Trunc(GetNumber(3));
         AL := GetNumber(4);
         sr := GetNumber(5);
         IF ((sr < 0) OR (sr > 2 * (Pi + AL))) THEN
         BEGIN
            Announce('Error in Starting Location');
            error := 1;
         END;
         thetair := GetNumber(6);
         Stadium_Output := GetString(7);
      UNTIL (error = 0);
      IF (canceled) THEN
         quitf := true;
      DONE;
   END;
END;  { PROCEDURE ReadData }

PROCEDURE ReadData1;
   VAR
      screen: TInputScreen;
      nr, error: INTEGER;
BEGIN
   WITH screen DO
   BEGIN
      init;
      DefineInputPort(0.1, 0.87, 0.1, 0.95);
      LoadLine('                 The Stadium Model                  ');
      LoadLine('                                                    ');
      LoadLine(' Select the Model:                                 ');
      LoadLine('  #1 One Ball                                       ');
      LoadLine('  #1 Two Ball                                       ');
      LoadLine('                                                    ');
      LoadLine(' Number of Bounces: {     } (1 to 200)            ');
      LoadLine('                                                   ');
      LoadLine(' Length of Straight Portion of the Stadium (AL):    ');
      LoadLine('  (0 -5) 0 forms a circle,5 a long thin oval {    } ');
      LoadLine('                                                    ');
      LoadLine(' The Starting Location of the Ball: {     } (0 to 2*(Pi+AL))');
      LoadLine('                                                   ');
      LoadLine(' The Initial Angle WRT the x axis:  {     } (0 to 179)');
      LoadLine('                                                   ');
      LoadLine('              [   Ok   ]   [Cancel]               ');
      SetRadioButton('1', nb);
      SetNumber(3, n);
      SetNumberLimits(3, 1, 200);
      SetNumber(4, AL);
      SetNumberLimits(4, 0, 5);
      SetNumber(5, sr);
      SetNumber(6, thetair);
      SetNumberLimits(5, 0, 179);
      REPEAT
         error := 0;
         Accept;
         nb := GetRadioButton('1');
         n := Trunc(GetNumber(3));
         AL := GetNumber(4);
         sr := GetNumber(5);
         IF ((sr < 0) OR (sr > 2 * (Pi + AL))) THEN
         BEGIN
            Announce('Error in Starting Location');
            error := 1;
         END;
         thetair := GetNumber(6);
      UNTIL (error = 0);
      IF (canceled) THEN
         quitf := true;
      DONE;
   END;
END;  { PROCEDURE ReadData1 }

PROCEDURE SetUpMyMenu;
BEGIN
   MyMenuOut.Init;
   WITH MyMenuOut DO
   BEGIN
      column(1, 'File');
      row(1, 1, 'About Program');
      row(1, 2, 'About CUPS');
      row(1, 3, 'Configuration');
      row(1, 4, '---------------');
      row(1, 5, 'Restart Program');
      row(1, 6, '---------------');
      row(1, 7, 'Exit Program');
      rowactivate(1,4,false);
      rowactivate(1,6,false);
   END;
   MyMenuOut.Display;
END;  { PROCEDURE SetUpMyMenu }

PROCEDURE HandleMenu (VAR restart, quitf: BOOLEAN);
BEGIN
   WITH MyMenuOut DO
   BEGIN
      CASE rowChosen OF
         1: ABOUTPROGRAM;
         2: ABOUTCUPS;
         3: configuration;
         5: restart := true;
         7: quitf := NOT quitf;
      END; { CASE }
   END;  { WITH MyMenuOut DO }
END; { PROCEDURE HandleMenu }

PROCEDURE SetUpHotKeys (VAR HotKeys: THotKeys);
BEGIN
   WITH HotKeys DO
   BEGIN
      init(5);
      key[1] := 'F1-Help';
      key[2] := 'F3-Step';
      key[3] := 'F5-Slower';
      key[4] := 'F6-Faster';
      key[5] := 'F10-Menu';
   END;
END;  { PROCEDURE SetUpHotKeys }

PROCEDURE HandleHotKeys (key: BYTE; nb: INTEGER; VAR deltat: INTEGER;
          VAR STEP: BOOLEAN);
BEGIN
   CASE key OF
      1:
         IF (nb = 1) THEN
            Help1
         ELSE
            Help2;
      2:
         STEP := NOT STEP;  { F3 - Step }
      3:
      BEGIN      { F5 - Slower }
         deltat := round(1.3*deltat);
         if (deltat = 0 ) then deltat := 10;
         IF (deltat > 1500) THEN begin
            deltat := 1500;
            beep;
         end;
      END;
      4:
      BEGIN      { F6 - Faster }
         deltat := Round(deltat/1.3);
         IF (deltat < 10) THEN begin
            deltat := 0;
            beep;
         end;
      END;
      5:
      BEGIN
         MouseBorders(0, 0, GetMaxX, GetMaxY);
         IF MyMenuOut.chosen THEN
            HandleMenu(restart, quitf);
      END;
   END;  { CASE }
END;  { PROCEDURE HandleHotKeys }

PROCEDURE PlotDistance (ax1, ax2, ay11, ay12, ay21, ay22, iiph, i,
                        n: INTEGER; d, dmax: REAL);

   { ****************************************************** }
   {                          }
   { Procedure PlotDistance plots the phase space distances }
   {                          }
   { ****************************************************** }

   CONST
      shape:
      FillPatternType = ( $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);

   VAR
      i1: WORD;
      admax, ad, x, y, lax1, lax2 : longint;
      lay11, lay12, lay21, lay22, li, lnn: LONGINT;
      color : integer;
BEGIN
   lax1 := ax1;
   lax2 := ax2;
   lay11 := ay11;
   lay12 := ay12;
   lay21 := ay21;
   lay22 := ay22;
   li := i;
   lnn := n;
   ad := Trunc ( 100.E0
   * d );
   admax := Trunc ( dmax * 100.E0
   );
   y := lay12 + (((lay11 - lay12) * ad) DIV admax);
   x := lax1 + ((li * (lax2 - lax1)) DIV lnn);
   case iiph of
    1: color := 15;
    2: color := 13;
    3: color := 3;
    4: color := 10;
    5: color := 2;
    6: color := 12;
    ELSE color := 14;
   end;
   SetFillStyle(solidfill, color);
   setColor(color);
   FillEllipse(x, y, 2, 2);
END;  { PROCEDURE PlotDistance }

PROCEDURE Initialize_Stadium (AL, delta, AR, smax, thetair: REAL;
s, thetai: doubler; dist: distarray; iiph: INTEGER; n: LONGINT;
nb: INTEGER; VAR dmax: REAL; VAR p, xi, yi: doubler;
VAR axend, axstart, ax1, ax2, ayendl, aystartl, ayr, ay1, ay2,
ay11, ay12, ay21, ay22, lmaxx, lmaxy: INTEGER; VAR intersect: doublei);

{ *********************************************************** }
{                            }
{ Initialize_Stadium inputs the AL,n,s and thetai:         }
{                            }
{  AL - the ratio of the stadium's non-curved portion to the  }
{    radius of the curved portion,                }
{                            }
{ delta - a constant that is used to compenstate for numerical}
{      impercsions                    }
{                            }
{ n  - the number of bounces                }
{                            }
{ s  - the starting location of the ball along the stadium }
{      outline                     }
{                            }
{ thetai - the angle the ball makes with the x axis        }
{                            }
{ calls StadiumOutline which draws the stadium outline and    }
{ and computes maxX,maxY, axl,axendl,axstartl,axr,ayl,ayendl, }
{ aystart,ayr, ir and lengthl                  }
{                            }
{ and computes                     }
{                            }
{ (xi,yi) - the starting location in Cartesian coordinates    }
{                            }
{ p - the balls momentum = cos(alpha) where alpha is the      }
{     angle between the foward stadium tangent and the ball's }
{     trajectory                      }
{                            }
{ intersect - the stadium section in which the ball initially }
{          intersects the stadium outline            }
{                            }
{ dmax - the maximum separation distance in phase space       }
{     between two balls                  }
{                            }
{ smax - half the stadium's circumference                     }
{                            }
{ *********************************************************** }

   VAR
      alpha, AL2, Pi2, ratio, r2d, theta, thetat: REAL;
      axr, i, i1, ir, one_fourtith, x, y: INTEGER;
      st: STRING;
      ch: STRING[5];

   PROCEDURE StadiumOutline (AL: REAL; VAR axend, axstart, axr, ayendl,
   aystartl, ayr, ir, lmaxx, lmaxy, one_fourtith: INTEGER;
   VAR ratio: REAL);

{ ************************************************************ }
{                        }
{ this procedure draws the stadium outline and outputs axl,  }
{ axendl,axstartl,axr,ayl,ayendl,aystartl,ayl and ir, lengthl  }
{ which define the stadium outline in screen coordinates  }
{                        }
{ ************************************************************ }

      VAR
         l, FontSize, FontType, Xasp, Yasp: WORD;
         axl, axendl, axstartl, ayl, lengthl: INTEGER;
         lmaxxl, lmaxyl, lir, ll: LongInt;
         TextOptions: TextSettingsType;
         ArcCoords: ArcCoordsType;
         axasp, ayasp: REAL;

   BEGIN
      GetAspectRatio(Xasp, Yasp);
      axasp := Xasp;
      ayasp := Yasp;
      ratio := ayasp / axasp;

      lmaxx := GetMaxX;
      lmaxy := GetMaxY;

      lmaxxl := lmaxx;
      lmaxyl := lmaxy;
      ll := round(100.0 * AL);

      IF (AL < 1.0) THEN
         one_fourtith := lmaxxl DIV 20
      ELSE
         one_fourtith := lmaxxl DIV 40;

{ determine the radius of the circle (relative to the screen size) }

      IF (AL < 1.0) THEN
         lir := (lmaxyl * 5) DIV 20
      ELSE
         lir := (lmaxyl * 6) DIV 20;

      IF lir > (300 * lmaxxl) DIV ((200 + ll) * 5) THEN
         lir := (300 * lmaxxl) DIV ((200 + ll) * 5);

   { draw the left semi-circle }

      ir := lir;
      axl := (one_fourtith + Trunc(ir * ratio));
      ayl := lmaxyl DIV 2;

      GetTextSettings(TextOptions);

      WITH TextOptions DO
      BEGIN
         FontType := Font;
         FontSize := CharSize * 2;
      END;

      SetTextStyle(FontType, HorizDir, FontSize);
      IF nb = 1 THEN
         OutTextXY(lmaxx DIV 8, lmaxy DIV 25,
                   'Stadium Model with One Ball')
      ELSE
         OutTextXY(lmaxx DIV 8, lmaxy DIV 25,
                   'Stadium Model with Two Balls');
      SetTextStyle(FontType, HorizDir, Fontsize DIV 2);


      Arc(axl, ayl, 90, 270, ir);

      GetArcCoords(ArcCoords);

      WITH ArcCoords DO
      BEGIN
         axstartl := Xstart;
         aystartl := Ystart;
         axendl := Xend;
         ayendl := Yend;
      END;

      lengthl := Trunc((ll * lir * ratio) / 100.0E0);

 { Draw the upper and lower lines }

      Line(axstartl, aystartl, axstartl + lengthl, aystartl);
      Line(axendl, ayendl, axendl + lengthl, ayendl);

 { Draw the right semi-circle }

      axr := axl + lengthl;
      ayr := ayl;

      Arc(axr, ayr, 270, 90, ir);

      Arc(axl, ayl, 90, 180, ir);
      GetArcCoords(ArcCoords);
      WITH ArcCoords DO
      BEGIN
         axstart := Xend;
      END;

      Arc(axr, ayr, 0, 90, ir);
      GetArcCoords(ArcCoords);
      WITH ArcCoords DO
      BEGIN
         axend := Xstart;
      END;

   END;    { PROCEDURE StadiumOutline }

   PROCEDURE GraphPhase (axr, ayr, ir, lmaxx, lmaxy, one_fourtith:
   INTEGER; ratio, smax: REAL; VAR ax1, ax2, ay1, ay2: INTEGER);

{ *********************************************** }
{                     }
{ This procedure draws the axes for the p-s graph }
{                     }
{ The outputs ax1,ax2,ay1,ay2 are the screen     }
{ coordinates for the axes           }
{                     }
{ *********************************************** }

      VAR
         length1, i: INTEGER;
         outp: STRING[3];
         TextInfo: TextSettingsType;

   BEGIN

      ax1 := axr + Trunc(ir * ratio) + one_fourtith;
      length1 := lmaxx DIV 3 - 40;
      ax2 := ax1 + length1;
      length1 := lmaxy DIV 3;
      ay1 := ayr - length1;
      ay2 := ayr + length1;

      Line(ax1, ay1, ax1, ay2);
      Line(ax1, ayr, ax2, ayr);

      OutTextXY(ax1 - (lmaxx DIV 32), ay2, '-1');
      OutTextXY(ax1 - (lmaxx DIV 64), ayr, '0');
      OutTextXY(ax1 - (lmaxx DIV 64), ay1, '1');
      STR(Trunc(smax), outp);
      OutTextXY(ax2 + 8, ayr, outp);
      length1 := (ax2 + ax1) DIV 2 - 50;
      OutTextXY(length1, ay1 - (lmaxy DIV 48), 'Poincare Section');
      OutTextXY(length1, ay2 + 10, 'Position');
      GetTextSettings(TextInfo);
      SetTextStyle(TextInfo.Font, 1, TextInfo.CharSize);
      OutTextXY(ax1 - (lmaxx DIV 64), ayr - 120, 'Momentum');
      SetTextStyle(TextInfo.Font, 0, TextInfo.CharSize);

      length1 := (ay2 - ayr) DIV 3;
      FOR i := 1 TO 3 DO
         Line(ax1, ay1 +(i-1) *length1, ax1 +10, ay1 +(i-1) *length1);
      length1 := (ayr - ay1) DIV 3;
      FOR i := 1 TO 3 DO
         Line(ax1, ayr + (i-1) *length1, ax1 +10, ayr +(i-1) *length1);
      length1 := (ax2 - ax1) DIV 4;
      FOR i := 1 TO 3 DO
         Line(ax1 + i * length1, ayr - 5, ax1 + i * length1, ayr + 5);
      Line(ax1, ay2, ax1 + 10, ay2);
      Line(ax2, ayr - 5, ax2, ayr + 5);

   END;  { PROCEDURE GraphPhase }

   PROCEDURE GraphDist (axr, ayl, ir, lmaxx, lmaxy, one_fourtith: INTEGER;
   n: LONGINT; ratio, dmax: REAL; VAR ax1, ax2, ay11, ay12, ay21,
   ay22: INTEGER);

{ ********************************************************** }
{                           }
{ This procedure draws the axes for the phase space distance }
{ graph                                                      }
{                           }
{ The outputs ax1, ax2, ay11, ay12, ay21 and ay22 are the    }
{ screen coordinates for the axes                }
{                           }
{ ********************************************************** }

      VAR
         length1, i: LONGINT;
         outp: STRING[3];
         lmax: REAL;
         TextInfo: TextSettingsType;

   BEGIN

      ax1 := axr + Trunc(ir * ratio) + 3 * one_fourtith;
      length1 := lmaxx DIV 3 - 40;
      ax2 := ax1 + length1;
      ay11 := lmaxy DIV 6;
      ay12 := ayl - 20;
      ay21 := ayl + 20;
      ay22 := ay12 + ay21 - ay11;

      Line(ax1, ay11, ax1, ay12);
      Line(ax1, ay12, ax2, ay12);

      STR(Trunc(dmax), outp);
      OutTextXY(ax1 - (lmaxx DIV 32), ay11, outp);
      OutTextXY(ax1 - (lmaxx DIV 64), ay12, '0');
      STR(n, outp);
      OutTextXY(ax2 - (lmaxx DIV 64), ay12 + (lmaxy DIV 48), outp);
      OutTextXY(ax1 + (lmaxx DIV 64), ay11 - (lmaxy DIV 48),
                'PHASE SPACE DISTANCE');

      OutTextXY(ax1 + 10, ay12 + 18, 'No. of Bounces (n)');
      GetTextSettings(TextInfo);
      SetTextStyle(TextInfo.Font, 1, TextInfo.CharSize);
      OutTextXY(ax1 - (lmaxx DIV 32) - 5, ay11 - 15,
                'Sep. Dist. in P Space');
      SetTextStyle(TextInfo.Font, 0, TextInfo.CharSize);

      length1 := (ay12 - ay11) DIV 3;
      FOR i := 1 TO 3 DO
         Line(ax1, ay11 + (i-1) *length1, ax1 +10, ay11 +(i-1) *length1);
      length1 := (ax2 - ax1) DIV 3;
      FOR i := 1 TO 3 DO
         Line(ax1 +(i-1) *length1, ay12, ax1 +(i-1) *length1, ay12 -10);
      Line(ax2, ay12, ax2, ay12 - 10);

   END;  { PROCEDURE GraphDist }

BEGIN  { PROCEDURE Initialize_Stadium }

   DefineViewPort(1, 0.01, 0.99, 0.05, 0.95);
   OpenViewPort(1);

   Pi2 := Pi / 2.0E0;
   r2d := 180.0E0 / Pi;

   AL2 := AL / 2.0E0;
   dmax := SQRT(SQR(smax) + 4);

   StadiumOutline(AL, axend, axstart, axr, ayendl, aystartl, ayr, ir,
                  lmaxx, lmaxy, one_fourtith, ratio);

   IF (nb = 1) THEN
      GraphPhase(axr, ayr, ir, lmaxx, lmaxy, one_fourtith, ratio,
                 2.0 * smax, ax1, ax2, ay1, ay2)
   ELSE
   BEGIN
      GraphDist(axr, ayr, ir, lmaxx, lmaxy, one_fourtith, n, ratio,
                dmax, ax1, ax2, ay11, ay12, ay21, ay22);
      SETCOLOR(15);
      OutTextXY(ax1 - 20, ay12 + 40, 'initial diff. - 0.1');
      IF (iiph >= 2) THEN
      BEGIN
         SETCOLOR(13);
         OutTextXY(ax1 - 20, ay12 + 54, 'initial diff. - 0.01');
      END;
      IF (iiph >= 3) THEN
      BEGIN
         SETCOLOR(3);
         OutTextXY(ax1 - 20, ay12 + 68, 'initial diff. - 0.001');
      END;
      IF (iiph >= 4) THEN
      BEGIN
         SETCOLOR(10);
         OutTextXY(ax1 - 20, ay12 + 82, 'initial diff. - 0.0001');
      END;
      SETCOLOR(white);
      IF (iiph > 1) THEN
         FOR i := 1 TO (iiph - 1) DO
         BEGIN
            FOR i1 := 1 TO n DO
               PlotDistance(ax1, ax2, ay11, ay12, ay21, ay22, i, i1, n,
                            dist[i, i1], dmax);
         END;
   END;
   SETCOLOR(white);
   x := axstart;
   y := ayendl + (lmaxy DIV 48);
   STR(n, st);
   st := 'n = ' + st;
   OutTextXY(x, y, st);
   y := y + (lmaxy DIV 48);
   STR(AL : 2 : 3, ch);
   st := 'AL = ' + ch;
   OutTextXY(x, y, st);
   y := y + (lmaxy DIV 48);
   STR(s[1] : 1 : 3, st);
   st := 'Initial s = ' + st;
   OutTextXY(x, y, st);
   y := y + (lmaxy DIV 48);
   STR(Round(thetair), st);
   st := 'Initial thetai = ' + st;
   OutTextXY(x, y, st);

{ determine thetat and which section s is in }

   FOR i := 1 TO nb DO
   BEGIN

      IF (s[i] >= 0.0E0) AND (s[i] < Pi2 * AR) THEN
      BEGIN
         intersect[i] := 6;
         theta := s[i] / AR;
         yi[i] := AR * sin(theta);
         xi[i] := AL2 + SQRT(SQR(AR) - SQR(yi[i]));
         IF ABS(yi[i]) > delta THEN
            thetat := ArcTan((xi[i] - AL2) / yi[i])
         ELSE
            thetat := Pi2;
         IF thetat < 0.0E0 THEN
            thetat := Pi + thetat;
      END

      ELSE

         IF (s[i] >= Pi2 * AR) AND (s[i] <= AL + Pi2 * AR) THEN
         BEGIN
            intersect[i] := 1;
            xi[i] := Pi2 * AR + AL2 - s[i];
            yi[i] := AR;
            thetat := 0.0E0;
         END

         ELSE

            IF (s[i] > AL + Pi2 * AR) AND (s[i] <= Pi * AR + AL) THEN
            BEGIN
               intersect[i] := 2;
               theta := (s[i] - Pi2 * AR - AL) / AR;
               yi[i] := AR * cos(theta);
               xi[i] := -AL2 - SQRT(SQR(AR) - SQR(yi[i]));
               IF ABS(yi[i]) > delta THEN
                  thetat := ArcTan((xi[i] + AL2) / yi[i])
               ELSE
                  thetat := Pi2;
               IF thetat < 0.0E0 THEN
                  thetat := Pi + thetat;
            END

            ELSE

               IF (s[i] > Pi*AR +AL) AND (s[i] <= 3.0*Pi2*AR +AL) THEN
               BEGIN
                  intersect[i] := 3;
                  theta := (s[i] - Pi * AR - AL) / AR;
                  yi[i] := -AR * sin(theta);
                  xi[i] := -AL2 - SQRT(SQR(AR) - SQR(yi[i]));
                  IF ABS(yi[i]) > delta THEN
                     thetat := ArcTan(-(xi[i] + AL2) / yi[i])
                  ELSE
                     thetat := Pi2;
                  IF thetat < 0.0E0 THEN
                     thetat := Pi + thetat;
               END

               ELSE

                  IF (s[i] >= (1.5E0 * Pi * AR + AL)) AND
                     (s[i] <= (1.5E0 * Pi * AR + 2.0E0 * AL)) THEN
                  BEGIN
                     intersect[i] := 4;
                     xi[i] := s[i] - 1.5E0 * Pi * AR - 3.0E0 * AL2;
                     yi[i] := -AR;
                     thetat := 0.0E0;
                  END

                  ELSE

                  BEGIN
                     intersect[i] := 5;
                     theta := (s[i] - 1.5 * Pi * AR - 2.0E0 * AL) / AR;
                     xi[i] := AL2 + AR * sin(theta);
                     yi[i] := -SQRT(SQR(AR) - SQR(xi[i] - AL2));
                     IF ABS(yi[i]) > delta THEN
                        thetat := ArcTan(-(xi[i] - AL2) / yi[i])
                     ELSE
                        thetat := Pi2;
                     IF thetat < 0.0E0 THEN
                        thetat := Pi + thetat;
                  END;

      alpha := thetat - thetai[i];
      IF alpha < 0.0E0 THEN
         alpha := Pi + alpha;
      p[i] := cos(alpha);

   END;  { FOR i := 1 TO nb }

END;  { PROCEDURE Initialize_Stadium }

PROCEDURE PlotPoint (AL, x, y: REAL; axend, axstart, ayendl,
                     aystartl: INTEGER; VAR xd, yd: INTEGER);

 { ************************************************************* }
 {                         }
 { PROCEDURE PlotPoint determines the screen coordinates (xd,yd) }
 { of coordinates (x,y)                }
 {                         }
 { ************************************************************* }

BEGIN

   xd := Trunc(x *(axend-axstart) / (AL+2.0E0) +0.5E0 *(axend+axstart));

   yd := Trunc(0.5E0 *y *(-ayendl+aystartl) + 0.5E0 *(ayendl+aystartl));

END; { PROCEDURE PlotPoint }

PROCEDURE Intersection (delta, AR, AL, xi, yi, thetai: REAL;
intersect: INTEGER; VAR xt1, yt1, thetar, s, p: EXTENDED;
VAR intersect_new: INTEGER; VAR alpha: EXTENDED);

  { ******************************************************** }
  {                            }
  { This subroutine determines where the ball intersects the }
  { stadium outline.                    }
  {                            }
  {  INPUTS:                         }
  {  AR   The radius of the semicircles             }
  {  AL   The length of the straight portion of the     }
  {        stadium relative to R              }
  {  (xi,yi)    The initial coordinates             }
  {  thetai  The angle that the incident path makes with   }
  {        the x axis                   }
  {  intersect Flag indicating in which section the ball     }
  {        starts                 }
  {                            }
  {  OUTPUTS:                     }
  {  (xt,yt)    The impact coordinates              }
  {  thetar  The angle that the reflected path makes with  }
  {        the x axis                   }
  {  s    The arc length of (xt,yt)           }
  {  p    The tangential momentum = cos(alpha) where    }
  {        alpha is the angle between the reflected     }
  {        ball's trajectory and the forward tangent of }
  {        the stadium outline at the point of impact   }
  {  intersect_new                      }
  {       Flag indicating in which section the ball hit }
  {                            }
  { ******************************************************** }

   VAR
      AL2, Pi2, r2d, theta, yprime: REAL;
      a, b, b2, c, cost, q, q1, sint, TanThetai, x1, y1: EXTENDED;
      inverse: REAL;
      xt, yt, thetat, pi: EXTENDED;

BEGIN

   AL2 := AL / 2.0E0;
   Pi := 3.1415926535897932384626;
   Pi2 := Pi / 2.0E0;
   r2d := 180.0E0 / Pi;
   intersect_new := 0;

  { Determine in which section the ball hits }

  { Tests for vertical or hortizontal motion }

   IF (ABS(sin(thetai)) <= 0.0000001) THEN
   BEGIN
      xt := -xi;
      yt := yi;
      CASE intersect OF
         1:
            intersect_new := 7;  { ERROR }
         2:
            intersect_new := 6;
         3:
            intersect_new := 5;
         4:
            intersect_new := 7;  { ERROR }
         5:
            intersect_new := 3;
         6:
            intersect_new := 2;
      END;   {CASE}

   END { IF (ABS(sin(thetai)) < 0.0000001) }

   ELSE

      IF (ABS(cos(thetai)) < 0.000001) THEN
      BEGIN
         xt := xi;
         yt := -yi;
         CASE intersect OF
            1:
               intersect_new := 4;
            2:
               intersect_new := 3;
            3:
               intersect_new := 2;
            4:
               intersect_new := 1;
            5:
               intersect_new := 6;
            6:
               intersect_new := 5;
         END;   {CASE}

      END; { IF (ABS(cos(thetai)) < 0.000001) ... }

   IF (intersect_new = 0) THEN
   BEGIN

      inverse := 0;
  { Determine in which section the ball hits }
      sint := sin(thetai);
      cost := cos(thetai);
      IF (ABS(cost) > 0.001) THEN
         TanThetai := sint / cost
      ELSE
      BEGIN
         inverse := 1;
         TanThetai := cost / sint;
      END;
   { Section 1 }
      IF (AL > 0) THEN
      BEGIN
         IF (inverse = 0) THEN
            x1 := (AR - yi) / TanThetai + xi
         ELSE
            x1 := (AR - yi) * TanThetai + xi;
         IF (ABS(x1 - xi) > delta) AND (x1 > -AL2 - delta) AND
            (x1 < AL2 + delta) THEN
         BEGIN
            xt := x1;
            yt := AR;
            intersect_new := 1;
         END;  { IF (x1 <> xi) AND (x1 > - ... }
      END; { IF (AL > 0) }
   { Section 4 }
      IF (AL > 0) AND (intersect_new = 0) THEN
      BEGIN
         IF (inverse = 0) THEN
            x1 := (-AR - yi) / TanThetai + xi
         ELSE
            x1 := (-AR - yi) * TanThetai + xi;
         IF (ABS(x1 - xi) > delta) AND (x1 > -AL2 - delta) AND
            (x1 < AL2 + delta) THEN
         BEGIN
            xt := x1;
            yt := -AR;
            intersect_new := 4;
         END; { IF (x1 <> xi) ... }
      END; { IF (intersect_new = 0) }
   { Sections 2 & 3 }
      IF (intersect_new = 0) AND (inverse = 0) THEN
      BEGIN
         a := TanThetai * TanThetai + 1.0E0;
         b := -2.0E0 * (TanThetai *TanThetai *xi - TanThetai *yi -AL2);
         b2 := b * b;
         c := TanThetai *TanThetai *xi *xi - 2.0E0 *TanThetai *xi *yi +
              yi *yi - SQR(AR) + SQR(AL2);
         q := (b2 - 4.0E0 * a * c);
         q1 := q;
         IF (q >= 0.0E0) THEN
         BEGIN
            q := SQRT(q);
            x1 := (-b - q) / (2.0E0 * a);
            y1 := TanThetai * (x1 - xi) + yi;
            IF ((ABS(x1 - xi) > delta) OR (ABS(y1 - yi) > delta)) AND
               (x1 <= -AL2 + delta) AND (x1 >= -(AR + AL2 + delta)) THEN
            BEGIN
               xt := x1;
               yt := y1;
               IF (y1 > 0) THEN
                  intersect_new := 2
               ELSE
                  intersect_new := 3;
            END; { IF (ABX(x1 ... }
            x1 := (-b + q) / (2.0E0 * a);
            y1 := TanThetai * (x1 - xi) + yi;
            IF ((ABS(x1 - xi) > delta) OR (ABS(y1 - yi) > delta)) AND
               (x1 <= -AL2 + delta) AND (x1 >= -(AR + AL2 + delta)) THEN
            BEGIN
               xt := x1;
               yt := y1;
               IF (y1 > 0) THEN
                  intersect_new := 2
               ELSE
                  intersect_new := 3;
            END;  { IF (ABS(x1 - ... }
         END; { IF q > 0 }
      END; { IF (intersect_new = 0) AND (inverse = 0) THEN }
      IF (intersect_new = 0) AND (inverse = 1) THEN
      BEGIN
         a := TanThetai * TanThetai + 1.0E0;
         b := -2.0E0 *(TanThetai *TanThetai *yi - TanThetai *(xi + AL2));
         b2 := b * b;
         c := SQR(TanThetai * yi) - 2.0E0 * TanThetai * (xi + AL2) * yi +
              (xi + AL2) * (xi + AL2) - AR * AR;
         q := (b2 - 4.0E0 * a * c);
         q1 := q;
         IF (q >= 0.0E0) THEN
         BEGIN
            q := SQRT(q);
            y1 := (-b - q) / (2.0E0 * a);
            x1 := TanThetai * (y1 - yi) + xi;
            IF ((ABS(x1 - xi) > delta) OR (ABS(y1 - yi) > delta)) AND
                (x1 <= -AL2 + delta) AND (x1 >= -(AR + AL2 + delta)) THEN
            BEGIN
               xt := x1;
               yt := y1;
               IF (y1 > 0) THEN
                  intersect_new := 2
               ELSE
                  intersect_new := 3;
            END; { IF (ABX(x1 ... }
            IF (intersect_new = 0) THEN
            BEGIN
               y1 := (-b + q) / (2.0E0 * a);
               x1 := TanThetai * (y1 - yi) + xi;
               IF ((ABS(x1 - xi) > delta) OR (ABS(y1 - yi) > delta)) AND
                  (x1 <= -AL2 + delta) AND (x1 >= -(AR + AL2 + delta)) THEN
               BEGIN
                  xt := x1;
                  yt := y1;
                  IF (y1 > 0) THEN
                     intersect_new := 2
                  ELSE
                     intersect_new := 3;
               END; { IF (ABX(x1 ... }
            END;  {IF (intersect_new = 0) THEN }
         END; { IF q > 0 }
      END; { IF (intersect_new = 0) AND (inverse = 1) THEN }
     { Sectios 5 & 6}
      IF (intersect_new = 0) AND (inverse = 0) THEN
      BEGIN
         a := TanThetai * TanThetai + 1.0E0;
         b := -2.0E0 * (TanThetai * TanThetai * xi - TanThetai *yi + AL2);
         b2 := b * b;
         c := TanThetai * TanThetai * xi * xi - 2.0E0 *TanThetai *xi *yi
               + yi * yi - SQR(AR) + SQR(AL2);
         q := (b2 - 4.0E0 * a * c);
         q1 := q;
         IF (q >= 0.0E0) THEN
         BEGIN
            q := SQRT(q);
            x1 := (-b - q) / (2.0E0 * a);
            y1 := TanThetai * (x1 - xi) + yi;
            IF ((ABS(x1 - xi) > delta) OR (ABS(y1 - yi) > delta)) AND
            (x1 >= AL2 - delta) AND (x1 <= (AR + AL2 + delta)) THEN
            BEGIN
               xt := x1;
               yt := y1;
               IF (y1 > 0) THEN
                  intersect_new := 6
               ELSE
                  intersect_new := 5;
            END; { IF (ABS(x1-xi) ... }
            IF (intersect_new = 0) THEN
            BEGIN
               x1 := (-b + q) / (2.0E0 * a);
               y1 := TanThetai * (x1 - xi) + yi;
               IF ((ABS(x1 - xi) > delta) OR (ABS(y1 - yi) > delta)) AND
                  (x1 >= AL2 - delta) AND (x1 <= (AR + AL2 + delta)) THEN
               BEGIN
                  xt := x1;
                  yt := y1;
                  IF (y1 > 0) THEN
                     intersect_new := 6
                  ELSE
                     intersect_new := 5;
               END; { IF (ABS(x1-xi) ... }
            END; { IF (intersect_new = 0) THEN }
         END;  { IF q > 0 }
      END;  { IF (intersect_new = 0) AND (inverse = 0) THEN }
      IF (intersect_new = 0) AND (inverse = 1) THEN
      BEGIN
         a := TanThetai * TanThetai + 1.0E0;
         b := -2.0E0 *(TanThetai *TanThetai *yi - TanThetai *(xi - AL2));
         b2 := b * b;
         c := SQR(TanThetai * yi) - 2.0E0 * TanThetai * (xi + AL2) * yi +
              (xi - AL2) * (xi - AL2) - AR * AR;
         q := (b2 - 4.0E0 * a * c);
         q1 := q;
         IF (q >= 0.0E0) THEN
         BEGIN
            q := SQRT(q);
            y1 := (-b - q) / (2.0E0 * a);
            x1 := TanThetai * (y1 - yi) + xi;
            IF ((ABS(x1 - xi) > delta) OR (ABS(y1 - yi) > delta)) AND
               (x1 >= AL2 - delta) AND (x1 <= (AR + AL2 + delta)) THEN
            BEGIN
               xt := x1;
               yt := y1;
               IF (y1 > 0) THEN
                  intersect_new := 2
               ELSE
                  intersect_new := 3;
            END; { IF (ABX(x1 ... }
            IF (intersect_new = 0) THEN
            BEGIN
               y1 := (-b + q) / (2.0E0 * a);
               x1 := TanThetai * (y1 - yi) + xi;
               IF ((ABS(x1 - xi) > delta) OR (ABS(y1 - yi) > delta)) AND
                  (x1 >= AL2 - delta) AND (x1 <= (AR + AL2 + delta)) THEN
               BEGIN
                  xt := x1;
                  yt := y1;
                  IF (y1 > 0) THEN
                     intersect_new := 2
                  ELSE
                     intersect_new := 3;
               END; { IF (ABX(x1 ... }
            END;  {IF (intersect_new = 0) THEN }
         END; { IF q > 0 }
      END; { IF (intersect_new = 0) AND (inverse = 1) THEN }
      IF (intersect_new = 0) THEN
      BEGIN
       { error }
         xt := 0.0E0;
         yt := 0.0E0;
         intersect_new := 7;
      END; { IF (interesect_new = 0 ) THEN }
   END;  { IF (intersect_new = 0) THEN }

   { Compute s and thetat }
   CASE intersect_new OF
      1:
      BEGIN
         s := Pi2 * AR + AL2 - xt;
         thetat := 0.0E0;
      END;
      2:
      BEGIN
         IF (-(xt + AL2) > 0.001) THEN
            theta := ArcTan(-yt / (xt + AL2))
         ELSE
            theta := Pi2 - ArcTan(-(xt + AL2) / yt);
         thetat := pi2 - theta;
         s := (Pi2 + thetat) * AR + AL;
      END;
      3:
      BEGIN
         IF (-(xt + AL2) > 0.001) THEN
            theta := ArcTan(yt / (xt + AL2))
         ELSE
            theta := Pi2 - ArcTan((xt + AL2) / yt);
         thetat := Pi2 + theta;
         s := (Pi + theta) * AR + AL;
      END;
      4:
      BEGIN
         s := 1.5E0 * (AR * Pi + AL) + xt;
         thetat := 0.0E0;
      END;
      5:
      BEGIN
         IF ((xt - AL2) > 0.001) THEN
            theta := ArcTan(-yt / (xt - AL2))
         ELSE
            theta := Pi2 - ArcTan(-(xt - AL2) / yt);
         thetat := Pi2 - theta;
         s := (1.5 * Pi + thetat) * AR + 2.0 * AL;
      END;
      6:
      BEGIN
         IF ((xt - AL2) > 0.001) THEN
            theta := ArcTan(yt / (xt - AL2))
         ELSE
            theta := Pi2 - ArcTan((xt - AL2) / yt);
         thetat := theta + Pi2;
         s := (theta) * AR;
      END;
      7:
      BEGIN
         writeln(Output, ' ERROR ');
         thetat := 0.E0
         ;
         s := 0.E0
         ;
      END
   END;   {CASE}

  { Compute alpha (the angle of reflection), p (the momentum) and   }
  { thetar (the angle that the reflected path makes with the x axis)}

   IF (thetat < 0.0E0) THEN
      thetat := Pi + thetat;
   alpha := thetat - thetai;
   IF (alpha < 0.0E0) THEN
      alpha := Pi + alpha;
   p := cos(alpha);
   thetar := 2.0E0 * thetat - thetai;
   IF (ABS(thetar) <= delta) THEN
      thetar := 0.0E0;
   IF ( thetar < 0.E0
   ) THEN thetar := Pi + thetar;
   IF (thetar >= Pi - delta) AND (thetar < 2.0E0 * Pi + delta) THEN
      thetar := thetar - Pi;
   IF (thetar >= 2.0E0 * Pi - delta) THEN
      thetar := thetar - 2.0E0 * Pi;
   xt1 := xt;
   yt1 := yt;
END;  { PROCEDURE Intersection }

PROCEDURE PlotDot (ax1, ax2, ay1, ay2, ayr, rev: INTEGER; p, s, smax: REAL);

   { *********************************************** }
   {                        }
   { Procedure PlotDot plots the points (s,p) on     }
   { the phase diagram - if nb = 1            }
   {                        }
   { *********************************************** }

   CONST
      shape:
      FillPatternType = ( $FF , $FF , $FF , $FF , $FF , $FF , $FF , $FF );

   VAR
      i: WORD;
      as, asmax, ap, x, y: LONGINT;

BEGIN
   as := Trunc ( 100.E0 * s );
   asmax := Trunc ( smax * 100.E0 );
   x := ax1 + ((ax2 - ax1) * as DIV asmax);
   ap := Trunc(100.0E0 * p);
   y := ayr + ((ap * (ay1 - ayr)) DIV 100);
   IF (rev = 1) THEN
      SetFillPattern(shape, white)
   ELSE
      SetFillPattern(shape, red);
   FillEllipse(x, y, 2, 2);
END;  { PROCEDURE PlotDot }

PROCEDURE CountDown (i, n, ir: LONGINT);

BEGIN
   SetColor(white);
   OutTextXY(50, 70, 'Countdown:');
   line(130, 60, 160, 60);
   line(130, 80, 160, 80);
   line(130, 60, 130, 80);
   line(160, 60, 160, 80);
   SetFillPattern(shape, darkgray);
   FloodFill(31, 61, white);
   SetColor(darkgray);
   IF (ir = 1) THEN
      OutTextXY(135, 70, NumStr(i - 1, 3, 0))
   ELSE IF (ir = 2) THEN
      OutTextXY(135, 70, NumStr(n + i - 1, 3, 0))
   ELSE
      OutTextXY(135, 70, NumStr(n, 3, 0));
   SetColor(white);
   IF (ir = 1) THEN
      OutTextXY(135, 70, NumStr(i, 3, 0))
   ELSE IF (ir = 2) THEN
      OutTextXY(135, 70, NumStr(n + i, 3, 0));
END;  { PROCEDURE CountDown }

BEGIN  { PROCEDURE Stadium }
   CUPSinit;
   ABOUTPROGRAM;
   CUPSdone;
   CUPSinit;
   AL := 2.0;
   sr := 1.0;
   thetair := 35.0;
   n := 20;
   nb := 1;
   r2d := 180.0E0 / Pi;
   AR := 1.0E0;

   REPEAT
      delta := 1.0;
      dsize := 0.0010;
      backcolor := GetBkColor;
      ClearMuppetPort;
      IF (first = true) THEN
      BEGIN
         first := false;
         ReadData;
         ASSIGN(Output, Stadium_Output);
         REWRITE(Output);
      END
      ELSE
         ReadData1;
      SetUpHotKeys(HotKeys);
      SetUpMyMenu;
      MouseBorders(0, GetMaxY - 15, GetMaxX - 5, GetMaxY - 10);
      IF (quitf = false) THEN
      BEGIN
         HotKeys.Display;
         MyMenuOut.display;
         smax := 2 * (Pi + AL);
         writeln(Output, ' Initial Conditions ');
         writeln(Output, ' ------------------ ');
         writeln(Output, ' AL (stadium length): ', AL : 5 : 2);
         writeln(Output, ' n (number of bounces): ', n : 3);
         writeln(Output, ' nb (number of balls): ', nb : 1);
         writeln(Output, ' 2*smax (stadium''s circumference): ', 2*smax:10:7);
         writeln(Output, ' s (initial position): ', sr : 5 : 2);
         writeln(Output, ' initial angle wrt the x axis: ', thetair : 6 : 2);
         writeln(Output);

         IF (nb = 1) THEN
            iph := 1
         ELSE
            iph := 4;
         iiph := 0;
         WHILE ((iiph < iph) AND (quitf = false) AND (restart = false)) DO
         BEGIN
            Inc(iiph);
            delta := delta * 0.1;
            IF (nb = 2) THEN
            BEGIN
               writeln(Output, ' initial nearest neighbor separation ',
                       delta : 10 : 7);
               writeln(Output, ' i     s[1]      s[2]        p[1]       p[2]         d');
            END;
            thetai[1] := thetair / r2d;
            thetai[2] := thetai[1] + delta / r2d;
            s[1] := sr;
            s[2] := s[1] + delta;
            IF (nb = 1) THEN
            BEGIN
               writeln(Output, ' Output ');
               writeln(Output, ' ------ ');
               writeln(Output, '    s    (   xt,       yt    )    alpha       p');
            END;
            Initialize_Stadium(AL, dsize, AR, smax, thetair, s, thetai, dist,
             iiph, n, nb, dmax, p, xi, yi, axend, axstart, ax1, ax2, ayendl,
             aystartl, ayr, ay1, ay2, ay11, ay12, ay21, ay22, lmaxx, lmaxy,
             intersect);
            IF (nb = 1) THEN
               writeln(Output, ' ', s[1] : 5 : 2, '   (', xi[1] : 9 : 5, ',',
                       yi[1] : 9 : 5, ')  ', '         ', p[1] : 9 : 6);

            PlotPoint(AL, xi[1], yi[1], axend, axstart, ayendl, aystartl,
                      xd1[1], yd1[1]);

            IF (nb = 2) THEN
               PlotPoint(AL, xi[2], yi[2], axend, axstart, ayendl, aystartl,
                         xd1[2], yd1[2]);
            i := 0;
            WHILE ((i < n) AND (quitf = false) AND (restart = false)) DO
            BEGIN
               Inc(i);
               SetColor(white);
               Countdown(i, n, 1);
               ib := 0;
               WHILE ((ib < nb) AND (quitf = false) AND (restart = false)) DO
               BEGIN
                  Inc(ib);
                  Intersection(dsize, AR, AL, xi[ib], yi[ib], thetai[ib],
                   intersect[ib], xt[ib], yt[ib], thetar[ib], s[ib], p[ib],
                   intersect_new[ib], alpha[ib]);
                  IF (nb = 1) THEN
                     writeln(Output, ' ', s[1] : 5 : 2, '   (', xt[1] :9:5,
                      ',', yt[1] : 9 : 5, ') ', alpha[1] : 9 : 4, ' ',
                      p[1] : 9 : 6);
                  PlotPoint(AL, xt[ib], yt[ib], axend, axstart, ayendl,
                            aystartl, xd2[ib], yd2[ib]);

                  IF (ib = 1) THEN
                     SetColor(white)
                  ELSE
                     SetColor(lightred);
                  Line(xd1[ib], yd1[ib], xd2[ib], yd2[ib]);
                  SetColor(white);
                  xi[ib] := xt[ib];
                  yi[ib] := yt[ib];
                  xd1[ib] := xd2[ib];
                  yd1[ib] := yd2[ib];
                  thetal := thetai[1];
                  thetai[ib] := thetar[ib];
                  intersect[ib] := intersect_new[ib];
               END;   { WHILE ((ib < nb) AND ... }

               IF (nb = 1) THEN
               BEGIN
                  rev := 1;
                  PlotDot(ax1, ax2, ay1, ay2, ayr, rev, p[1], s[1], 2.0*smax);
               END  { IF nb = 1 }
               ELSE
               BEGIN
                  sd := s[2] - s[1];
                  IF (ABS(sd) > smax) THEN
                     sd := 2.0 * smax - ABS(sd);
                  d := SQRT(SQR(sd) + SQR(p[2] - p[1]));
                  writeln(Output, i : 3, ' ', s[1] : 10 : 7, ' ', s[2] : 10 : 7,
                          ' ', p[1] : 10 : 7, ' ', p[2]:10:7, ' ', d:10:7);
                  dist[iiph, i] := d;
                  PlotDistance(ax1, ax2, ay11, ay12, ay21, ay22, iiph, i, n,
                               d, dmax);
               END;   { ELSE - nb = 2 }

               IF STEP THEN
               BEGIN
                  PressKeyMessage;
                  SetUpMyMenu;
               END
               ELSE
                  Delay(deltat);

               CheckForEvents;
               IF HotKeys.Pressed(key) THEN
                  HandleHotKeys(key, nb, deltat, STEP);
               MouseBorders(0, GetMaxY - 15, GetMaxX - 5, GetMaxY - 10);
            END;  { WHILE ((i < n) AND ... }

            IF ((nb = 1) AND (quitf = false) AND (restart = false)) THEN
            BEGIN
        { Reverse Trajectory }
               i := 1;
               Countdown(i, n, 3);
               thetai[1] := thetal;
               writeln(Output, 'Reverse Trajectory');
               WHILE ((i < n + 1) AND (quitf = false) AND (restart = false)) DO
               BEGIN
                  Inc(i);
                  Countdown(i - 1, n, 2);
                  SetColor(lightred);
                  Intersection(dsize, AR, AL, xi[1], yi[1], thetai[1],
                   intersect[1], xt[1], yt[1], thetar[1], s[1], p[1],
                   intersect_new[1], alpha[1]);
                  writeln(Output, ' ', s[1] : 5 : 2, '   (', xt[1] : 9 : 5,
                   ',', yt[1] : 9 :5, ') ', alpha[1]:9:4, ' ', p[1]:9:6);
                  PlotPoint(AL, xt[1], yt[1], axend, axstart, ayendl,
                            aystartl, xd2[1], yd2[1]);
                  Line(xd1[1], yd1[1], xd2[1], yd2[1]);
                  SetColor(lightred);

                  IF STEP THEN
                  BEGIN
                     PressKeyMessage;
                     SetUpMyMenu;
                  END
                  ELSE
                     Delay(deltat);

                  CheckForEvents;
                  IF (HotKeys.Pressed(key)) THEN
                     HandleHotKeys(key, 1, deltat, STEP);
                  MouseBorders(0, GetMaxY - 15, GetMaxX - 5, GetMaxY - 10);
                  xi[1] := xt[1];
                  yi[1] := yt[1];

                  xd1[1] := xd2[1];
                  yd1[1] := yd2[1];

                  thetal := thetai[1];
                  thetai[1] := thetar[1];
                  intersect[1] := intersect_new[1];

                  rev := 2;
                  PlotDot(ax1, ax2, ay1, ay2, ayr, rev, p[1], s[1], 2.0*smax);
                  SetColor(lightred);

               END;  { WHILE (i < n + 1) .... }
               IF (ABS(s[1] - 2.0 * smax) < 1.0E-7) THEN
                  s[1] := 0.0;

               SetColor(white);
               x := axstart;
               y := ayendl + 6 * (lmaxy DIV 48);
               STR(s[1] : 1 : 3, st);
               st := 'Final s = ' + st;
               OutTextXY(x, y, st);

            END;    { IF (nb = 1) }

            SetColor(white);
            writeln(Output);
         END;  { While ((iph < iiph) ... }
         MouseBorders(0, 0, GetMaxX - 5, GetMaxY);
         IF ((quitf = false) AND (restart = false)) THEN
         REPEAT
            CheckForEvents;
            IF HotKeys.Pressed(key) THEN
              HandleHotKeys(key, nb, deltat, STEP);
             if MyMenuOut.Activated then
              HandleMenu(restart, quitf);
         UNTIL (quitf or restart);
         restart := false;
      END;  { IF (quitf = false) }
   UNTIL (quitf = true);
   CUPSDone;
   Close(Output);
END. { Stadium }