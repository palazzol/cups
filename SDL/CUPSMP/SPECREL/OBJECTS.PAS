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

{ Start of file OBJECTS.PAS }

{ ----------------------------------------------------------------------
   Program Title:    Objects for wave mechanics units and special
                     relativity units.
   Author:           Douglas E. Brandt
   Version:          0.11
   Program Summary:  
   Library Files:    Crt, Graph, MUPPET, CUPSGUI, CUPSFUNC, CUPSPROC
   Utilities:
   Algorithms:
------------------------------------------------------------------------}

UNIT Objects;

INTERFACE

USES Crt, Graph, CUPS, CUPSmupp,CUPSgui,CUPSfunc,CUPSproc;

CONST MyBackColor = 0;

TYPE
   Point = object
      x,y: real;
   end;


   MessageRec = Record
      Quit: Boolean;
      Done: Boolean;
      Running: Boolean;
      Paused: Boolean;
      Next: Integer;
      Change: Integer;
   END;



   CDvector = OBJECT
         Re,Im: DVector;
         Size: Integer;
         MemBlock: Word;
         PROCEDURE Init(NumberElements: Integer);
         PROCEDURE Free;
         PROCEDURE ReSize(NumberElements: Integer);
         FUNCTION Length: Integer;
         PROCEDURE Put(Index: Integer; Z: Complex);
         PROCEDURE Get(Index: Integer; VAR Z: Complex);
         FUNCTION RePart(Index: Integer): Real;
         FUNCTION ImPart(Index: Integer): Real;
         PROCEDURE Equate(Z: Complex; VectB: CDVector);
         PROCEDURE MultReal(s: Real);
         PROCEDURE MultScalar(Scalar: Complex);
         PROCEDURE AddScalar(Scalar: Complex);
         PROCEDURE AddVector(s: Complex; VectB: CDVector);
         FUNCTION CAbs(i: Integer; Vect: CDVector): Real;
         PROCEDURE AbsSqr(VAR Vect: DVector);
         PROCEDURE AddZ(Index: Integer; Z: Complex);
   END;

   Base_display = object
      Title: String;
      Window_num: Integer;
      Window_corner: array[1..6,1..2] of Point;
{      Menu: TMenu;
      HotKeys: THotKeys;}
      Init: Boolean;
      procedure Open;
      procedure Close;
   end;

IMPLEMENTATION

PROCEDURE Base_Display.Open;

{---------------------------------------------------------------------------
   Initialization and drawing of hot keys. menus, borders, and title for
   each section.
---------------------------------------------------------------------------}

VAR
   i: Integer;

BEGIN
   DefineViewPort(9,0.00,1.00,0.05,0.95);
   SelectViewPort(9);
   HideCursor;
   ClearViewPort;
   ShowCursor;
   DefineViewPort(10, 0.00, 1.0, 0.9, 0.95);
   HideCursor;
   SetColor(15);
   GraphBackColor := Brown;
   OpenViewPort(10);
   GraphBackColor := MyBackColor;
   FOR i := 1 to Window_num DO
   BEGIN
      DefineViewPort(i, Window_corner[i,1].x, Window_corner[i,2].x,
                        Window_corner[i,1].y, Window_corner[i,2].y);
      OpenViewPort(i);

   END;
   SelectViewPort(10);
   Print(40 - Length(Title) div 2,1,Title);
   ShowCursor;
END;



PROCEDURE Base_Display.Close;

{--------------------------------------------------------------------------
Clean up any necessary items before exiting
--------------------------------------------------------------------------}

BEGIN
END;


PROCEDURE CDVector.Init(NumberElements: Integer);

   BEGIN
      Re.Init(NumberElements);
      Im.Init(NumberElements);
      Size := NumberElements;
   END;


PROCEDURE CDVector.Free;

   BEGIN
      Re.Free;
      Im.Free;
   END;


PROCEDURE CDVector.ReSize(NumberElements: Integer);

   BEGIN
     Re.Resize(NumberElements);
     Im.Resize(NumberElements);
     Size := NumberElements;
  END;


FUNCTION CDVector.Length: Integer;

   BEGIN
      Length := Size;
   END;


PROCEDURE CDVector.Put(Index: Integer; Z: Complex);

   BEGIN
      Re.Put(Index, Z.Re);
      Im.Put(Index, Z.Im);
   END;


PROCEDURE CDVector.Get(Index: Integer; VAR Z: Complex);

   BEGIN
      Re.Get(Index, Z.Re);
      Im.Get(Index, Z.Im);
   END;


FUNCTION CDVector.RePart(Index: Integer): Real;

   BEGIN
      RePart := Re.Value(Index);
   END;


FUNCTION CDVector.ImPart(Index: Integer): Real;

   BEGIN
      ImPart := Im.Value(Index);
   END;


PROCEDURE CDVector.Equate(Z: Complex; VectB: CDVector);

   VAR
      q,r: DVector;

   BEGIN
      q.init(Size);
      r.init(Size);
      q.SumOfVectors(Z.Re,VectB.Re,-Z.Im,VectB.Im);
      r.SumOfVectors(Z.Im,VectB.Re,Z.Re,VectB.Im);
      Re.Equate(1,q);
      Im.Equate(1,r);
      q.Free;
      r.Free;
   END;


PROCEDURE CDVector.MultReal(s:Real);

   BEGIN
      Re.MultScalar(s);
      Im.MultScalar(s);
   END;


PROCEDURE CDVector.MultScalar(Scalar: Complex);

   VAR
      q,r: DVector;

   BEGIN
      q.init(Size);
      r.init(Size);
      q.SumOfVectors(Scalar.Re,Re,-Scalar.Im,Im);
      r.SumOfVectors(Scalar.Im,Re,Scalar.Re,Im);
      Re.Equate(1,q);
      Im.Equate(1,r);
      q.Free;
      r.Free;
   END;

PROCEDURE CDVector.AddScalar(Scalar: Complex);

   BEGIN
      Re.AddScalar(Scalar.Re);
      Im.AddScalar(Scalar.Im);
   END;


PROCEDURE CDVector.AddVector(s: Complex; VectB: CDVector);

   BEGIN
      Re.AddVector(s.Re, VectB.Re);
      Re.AddVector(-s.Im, VectB.Im);
      Im.AddVector(s.Re, VectB.Im);
      Im.AddVector(s.Im, VectB.Re);
   END;

FUNCTION CDVector.CAbs(i: Integer; Vect: CDVector): Real;

   BEGIN
      CAbs := Sqr(Vect.Re.value(i)) + Sqr(Vect.Im.value(i));
   END;


PROCEDURE CDVector.AbsSqr(VAR Vect: DVector);

   VAR
      i: Integer;

   BEGIN
      FOR i := 1 TO Size DO Vect.put(i, CABS(i, Self));
   END;

PROCEDURE CDVector.AddZ(Index: Integer; Z: Complex);

   BEGIN
      Re.Put(Index, Re.Value(Index) + Z.Re);
      Im.Put(Index, Im.Value(Index) + Z.Im);
   END;

END.  { End of file OBJECTS.PAS }
