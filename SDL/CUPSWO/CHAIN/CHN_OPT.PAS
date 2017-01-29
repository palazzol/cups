{The follosing files are needed for chain.
  Chain.pas     //The main program.
  Chn_gbl.pas    //Global variables
  Chn_base.pas   // Atoms, buttons, and other display objects
  Chn_Plot.pas   // A generic plot objects that has scale, knows how to go full screen etc.
  Chn_Anly.pas   // The analysis routines that take data from the chain and plot it.
  Chn_Graf.pas   // The drawing rountines for the chain.
  Chn_Lst.pas    // A linked list object.}







{$DEFINE DEBUG}
{$UNDEF DEBUG}
{$IFDEF DEBUG} {$R+,S+,L+,D+} {$ELSE} {$R-,S-,L-,D-} {$ENDIF}
{$IFDEF CPU87} {$N+,E-} {$ELSE} {$N-} {$ENDIF}
{$G+}