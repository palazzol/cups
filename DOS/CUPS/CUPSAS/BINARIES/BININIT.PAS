Unit BinInit;
{$M 64000,0,327680}
interface
implementation
uses overlay;
BEGIN
   OvrInit('Binaries.Ovr');
   if OvrResult <> ovrOk then
   begin
     Writeln('File "Binaries.Ovr" not found.');
     Halt(1);
   end;
   OvrInitEMS;
END.
