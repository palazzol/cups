Unit NBInit;
{$M 64000,0,327680}
interface
implementation
uses overlay;
BEGIN
   OvrInit('NBodies.Ovr');
   if OvrResult <> ovrOk then
   begin
     Writeln('File "NBodies.Ovr" not found.');
     Halt(1);
   end;
   OvrInitEMS;
END.
