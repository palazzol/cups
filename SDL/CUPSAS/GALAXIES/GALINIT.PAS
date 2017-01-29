Unit GalInit;
{$M 64000,0,327680}
interface
implementation
uses overlay;
BEGIN
   OvrInit('Galaxies.Ovr');
   if OvrResult <> ovrOk then
   begin
     Writeln('File "Galaxies.Ovr" not found.');
     Halt(1);
   end;
   OvrInitEMS;
END.
