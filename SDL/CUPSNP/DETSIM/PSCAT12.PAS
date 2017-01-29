           (*************************************************)
           (*************************************************)
           (**                                             **)
           (** Consortium of Upper-level Physics Software  **)
           (**             ( CUPS Project )                **)
           (**     (c) 1994 by John Wiley & Sons           **)
           (** Compiled with Utilities Ver. 1.7 (95/06/27) **)
           (**                                             **)
           (*************************************************)
           (*************************************************)

{$D+,L+}
              {****************************************}
              {****************************************}
              {   Unit PSCAT   Relativistic Kinematics }
              {                 2 body scattering      }
              {                                        }
              {                         J. Rothberg    }
              {                                        }
              {                     9 June 1994        }
              {****************************************}
              {****************************************}


UNIT pscat12;

interface

USES Graph, Crt, cups, cupsMUPP, cupsfunc, cupsproc, cupsgui, cupsgrph,
                    plot2, relutil;

VAR
    numpart : Integer;          { number of particles in data base}
    maxout  : Integer;          {number of output data items}

TYPE
  partcon = record
   name:   String[8];
   nick:   String[4];          {short name}
   class:  String[8];
   num:    Integer;
   mass:   Real;
   lifet:  Real;
   width:  Real;
   color:  Integer;
  END;

  parco = array[1..18] of partcon;
  outarr = array[1..8] of Real;           {output data array}

 {----------------- Table of Reactions --------------------------}
VAR
   nreact: Integer;        {number of reactions}

TYPE
   finalc = array[1..3] of Integer;

TYPE
   reactc = record
     nf    : Integer;    { number of final states}
     in1   : Integer;    { initial state particle 1 }
     in2   : Integer;                   {particle 2 }
     fin1  : finalc;     { final state   particle 1 }
     fin2  : finalc;                    {particle 2 }
END;

   reactab = array[1..30] of reactc;
VAR
   reac : reactab;

 {------------------------------------------}


VAR
   LabVec  :  Real;          { max momentum for Lab Momentum vector, MeV}
   Eunit   :  String[4];     { units for mass, energy, momentum}
   HideMass:Boolean;
   OutFileEnable: Integer;       {Enable=1; Disable=0}
   outfile : Text;               {output file}
   outdata : outarr;
   blnk: String;
   ii: word;

VAR
   raddeg : Real;
   cpi    : Real;
   blank: String[6] ;
   LabVecdef : Real;         {default vector size for display}
   OutFilename: String[10] ;   {output file name}
   new: boolean ;
   col1  : Integer;  { CM and Lab numbers locations}
   col1b : Integer;
   col2  : Integer;

VAR
   photon   : Integer;
   gamma    : Integer;
   electron : Integer;
   positron : Integer;
   neutrino : Integer;
   muon     : Integer;
   pion     : Integer;
   pizero   : Integer;
   kaon     : Integer;
   kzero    : Integer;
   proton   : Integer;
   neutron  : Integer;
   rho      : Integer;
   lambda   : Integer;
   null     : Integer;
   eta      : Integer;
   tau      : Integer;
   W        : Integer;
   Z0       : Integer;

 TYPE
   partab = array[1..18] of String[10];

VAR
   parnam: partab;

VAR
   pcons: parco;

TYPE
     scatsys = object
       s      : Real;          {CM energy squared}
       roots  : Real;          {root s   CM energy}
       t      : Real;          {four-momentum transfer squared}
       q      : Real;          {3 momentum transfer}
       qsqr   : Real;          {3 momentum transfer squared}
       pp,pE        : Real;    {momentum, energy in Lab}
       beta,gamma   : Real;    {CM system as seen from Lab}
       avail_energy : Real;    {root s - final state masses}
       invarmass    : Real;    {invariant mass, final state}

       pmass        : Real;
       pmassq       : Real;
       pid1,pid2,pid3,pid4,pid5 : Integer;   {particle id numbers}

       PROCEDURE firstinit;
       PROCEDURE rdreac;

    END;

TYPE
    particle = record
      pnum        : Integer;         {particle number}
      pcolord     : Integer;         {display color}
      pname       : String[8];
      pnick       : String[4];       {short name}
      pmassq      : Real;            {mass squared}
      pmass :   Real;       {current value of mass in MeV}
      pcmass:   Real;       {central value of mass in MeV}
      plifet:   Real;
      pwidth:   Real;       {width in MeV}

                            { CM system variables}
      pCM,ECM     : Real;          { momentum, energy}
      pthetCM     : Real;          {CM angle in degrees}
      costhCM     : Real;
      sinthCM     : Real;
      pbetacm     : Real;          {beta}
      phi         : Real;
                            { Lab system variables}
      pLab,ELab   : Real;          { momentum, energy}
      KELab       : Real;
      costhLab    : Real;
      sinthLab    : Real;
      tanthLab    : Real;
      pthetlab    : Real;         {Lab theta in degrees}
      angdiff     : Real;         {Lab opening angle}
      pT          : Real;         {transverse momentum}
      px,py,pz    : Real;         {components of momentum}
      pbeta,pgamma: Real;
      plmax       : Real;         {max momentum in lab}

    END;

  pardecPtr =  ^pardec;

  pardec = object (scatsys)

  parin1, parin2 : particle;       {initial state particles}
  par1,   par2   : particle;       {final state particles}


      constructor Init;
      PROCEDURE istate(ipnum1, ipnum2: Integer);
      PROCEDURE fstate(ipnum1, ipnum2: Integer);
      PROCEDURE getmom(ip: Real);
      PROCEDURE getang(ithetcm: Real);
      PROCEDURE calc_s;
      PROCEDURE calc_t;
      PROCEDURE CMvari;
      PROCEDURE CMvarf;
      FUNCTION  qvalue: Real;
      PROCEDURE rescale;
      PROCEDURE dmass;        {random mass if particle has width}
      PROCEDURE LabMax;
      PROCEDURE decay;
      PROCEDURE CMtoLab;
      PROCEDURE list;
      PROCEDURE listgCM;
      PROCEDURE listgLab;
      PROCEDURE showCM;        {CM vectors}
      PROCEDURE showLab;       {Lab vectors}
      PROCEDURE fillntuple;    {fill kinematic variables for event}
      PROCEDURE filloutdat;    {fill output data ntuple}
      PROCEDURE WriteData;     {Write output file}
      FUNCTION  labtan(ihit: Integer): Real;          {return tan theta}
      FUNCTION  labenergy(ihit: Integer): Real;       {return lab energy}
      FUNCTION  labphi(ihit: Integer): Real;          {return phi}
  END;



implementation


FUNCTION  breitwig(center,fullwid: Real): Real;   {random Breit_Wigner}
VAR
     rr: Real;
BEGIN
     rr := 0.8*Random + 0.1;
     breitwig:= center + 0.5*fullwid*Tan(cpi*(rr-0.5));
END;

PROCEDURE rexit;
BEGIN
   CUPSdone;
END;

PROCEDURE scatsys.rdreac;
VAR
   i,ip,ir,id : INTEGER;
   FP: TEXT;
   nf : INTEGER;
   ini1,ini2,fnl1,fnl2,n1,n2 : STRING[10];

BEGIN
   ASSIGN(FP,'reaction.dat');
   {$I-} Reset(FP); {$I+}
   if IOResult<>0 then begin
     Announce('Input file REACTION.DAT not found in current directory');
     CUPSdone; halt(1);
   end;
   ip:= 0;
{   writeln( ' read data');}
   READLN(FP); {comment line}
   WHILE NOT EOF(FP) DO
   BEGIN
    READLN(FP,ini1,ini2);
    READLN(FP,nf);           {number of final states}
    IF nf>4 THEN
    BEGIN
        announce('too many finals states: file reaction.dat');
        CUPSdone; halt(1);
    END;
    ip:=ip+1;
    n1:=ini1;  n2:=ini2;
    reac[ip].nf := nf;
    reac[ip].in1:=0; reac[ip].in2:=0;
    FOR i := 1 TO numpart DO IF parnam[i]=n1 THEN   reac[ip].in1:=i;
    FOR i := 1 TO numpart DO IF parnam[i]=n2 THEN   reac[ip].in2:=i;
    IF (reac[ip].in1=0) or (reac[ip].in2=0) THEN
    BEGIN
       message (CONCAT(
             'initial particle ',n1,' ',n2,' not found: file part.dat'));
       {rexit;} pause;
    END;
 {    writeln(n1,n2);
      Writeln(ip:3,parnam[reac[ip].in1]:12,parnam[reac[ip].in2]:12);
    }
    FOR id := 1 TO nf DO
    BEGIN
       READLN(FP,fnl1,fnl2);
       n1:=fnl1;
       n2:=fnl2;

       reac[ip].fin1[id]:=0; reac[ip].fin2[id]:=0;
    FOR i := 1 TO numpart DO IF parnam[i]=n1 THEN   reac[ip].fin1[id]:=i;
    FOR i := 1 TO numpart DO IF parnam[i]=n2 THEN   reac[ip].fin2[id]:=i;
       IF (reac[ip].fin1[id]=0) or (reac[ip].fin2[id]=0) THEN
       BEGIN
         message (CONCAT(
         'final particle ',n1,' ',n2,' not found: file part.dat'));
       {  rexit; } pause;
       END;

   {     Writeln(id:6,parnam[reac[ip].fin1[id]]:12,
         parnam[reac[ip].fin2[id]]:12); }
    END;  {final states}
    READLN(FP);
  END;    { read file}
  nreact := ip;
END;


                                { Read particle data file }
  PROCEDURE scatsys.Firstinit;

VAR
  F: Text;
  rname : String[12];
  rclass: String[12];
  rnick : String[4];
  rnum  : Integer;
  rmass : Real;
  rlife : Real;
  rwidth: Real;
  rcolor: String[12];

type
  coltab = array[0..15] of String[12];

VAR
  colval: coltab;

VAR
  ic, icolor : Integer;

BEGIN
  {initialize constants}
    numpart := 18;          { number of particles in data base}
    maxout  :=  8;          {number of output data items}

    colval[0]  := 'black       ';
    colval[1]  := 'blue        ';
    colval[2]  := 'green       ';
    colval[3]  := 'cyan        ';
    colval[4]  := 'red         ';
    colval[5]  := 'magenta     ';
    colval[6]  := 'brown       ';
    colval[7]  := 'lightgray   ';
    colval[8]  := 'darkgray    ';
    colval[9] :=  'lightblue   ';
    colval[10] := 'lightgreen  ';
    colval[11] := 'lightcyan   ';
    colval[12] := 'lightred    ';
    colval[13] := 'lightmagenta';
    colval[14] := 'yellow      ';
    colval[15] := 'whitte       ';

   raddeg    := pi/180;
   cpi       := pi;
   blank     := '      ';
   LabVecdef := 100;         {default vector size for display}
   OutFilename := 'RELDET.OUT';   {output file name}
   new  := true;
   col1  := 52;  { CM and Lab numbers locations}
   col1b :=112;
   col2  :=124;

   photon   :=  1;
   gamma    :=  1;
   electron :=  2;
   positron :=  3;
   neutrino :=  4;
   muon     :=  5;
   pion     :=  6;
   pizero   :=  7;
   kaon     :=  8;
   kzero    :=  9;
   proton   :=  10;
   neutron  :=  11;
   rho      :=  12;
   lambda   :=  13;
   null     :=  14;
   eta      :=  15;
   tau      :=  16;
   W        :=  17;
   Z0       :=  18;

   parnam[1] :=  'gamma     ';
   parnam[2] :=  'electron  ';
   parnam[3] :=  'positron  ';
   parnam[4] :=  'neutrino  ';
   parnam[5] :=  'muon      ';
   parnam[6] :=  'pion      ';
   parnam[7] :=  'pizero    ';
   parnam[8] :=  'kaon      ';
   parnam[9] :=  'kzero     ';
   parnam[10] := 'proton    ';
   parnam[11] := 'neutron   ';
   parnam[12] := 'rho       ';
   parnam[13] := 'lambda    ';
   parnam[14] := '          ';
   parnam[15] := 'eta       ';
   parnam[16] := 'tau       ';
   parnam[17] := 'W         ';
   parnam[18] := 'Z0        ';

{ end of initialize constants}

 LabVec:= LabVecdef;                { scale for vector display}
 HideMass := False;                 { display decaying mass}

 Assign(F,'particle.dat');              { particle data file}
 {$I-} Reset(F); {$I+}
 if IOResult<>0 then begin
   Announce('Input file PATRICLE.DAT not found in current directory');
   CUPSdone; halt(1);
 end;
 Readln(F,rname); Readln(F,rname); Readln(F,rname);  {comments}
 while not Eof(F) do
 BEGIN
   Readln(F, rname, rnick, rclass, rcolor, rnum, rmass, rlife, rwidth);
  { Writeln(rname:12, rclass:12, rcolor:14,rnum:4, rmass:12:4,rwidth:12:4);}
   for ic:=0 to 15 DO IF colval[ic]=rcolor THEN icolor:=ic;
  { Writeln(rcolor:14, icolor:4, colval[icolor]:14);}
   IF (rnum <= numpart) THEN
   BEGIN
     With pcons[rnum] do
     BEGIN
      num  :=  rnum;
      name := copy(rname,1,pos(' ',rname)-1);
      nick := rnick;
      mass := rmass;
      lifet:= rlife;
      width:= rwidth;
      color:=icolor;
{     Writeln(num:4, mass:12:4, width:12:4, name:14, colval[color]:14);}
     END;
   END;
 END;   {read}

  rdreac;

  blnk:='';
  for ii:=1 to 10 do blnk:=CONCAT(blnk,char(219));

END;  {firstinit}



     constructor pardec.init;
BEGIN
END;

  PROCEDURE pardec.istate(ipnum1,ipnum2: Integer);   {initial state }
BEGIN
     pid1:= ipnum1;
     pid2:= ipnum2;
     parin1.pnum := pcons[ipnum1].num;
     parin2.pnum := pcons[ipnum2].num;
     parin1.pname:= pcons[ipnum1].name;
     parin2.pname:= pcons[ipnum2].name;
     parin1.pnick:= pcons[ipnum1].nick;
     parin2.pnick:= pcons[ipnum2].nick;
     parin1.pmass:= pcons[ipnum1].mass;
     parin1.pcmass:= pcons[ipnum1].mass;
     parin2.pmass:= pcons[ipnum2].mass;
     parin1.pcolord:=pcons[ipnum1].color;
     parin2.pcolord:=pcons[ipnum2].color;
     parin1.pmassq:= parin1.pmass*parin1.pmass;
     parin2.pmassq:= parin2.pmass*parin2.pmass;

     parin1.pwidth:=pcons[ipnum1].width;

END;

  PROCEDURE pardec.fstate(ipnum1,ipnum2: Integer);   {final state }
BEGIN
     pid3:= ipnum1;
     pid4:= ipnum2;
     par1.pnum := pcons[ipnum1].num;
     par2.pnum := pcons[ipnum2].num;
     par1.pname:= pcons[ipnum1].name;
     par2.pname:= pcons[ipnum2].name;
     par1.pnick:= pcons[ipnum1].nick;
     par2.pnick:= pcons[ipnum2].nick;

     par1.pmass:= pcons[ipnum1].mass;
     par2.pmass:= pcons[ipnum2].mass;
     par1.pcolord:=pcons[ipnum1].color;
     par2.pcolord:=pcons[ipnum2].color;
     par1.pmassq:= par1.pmass*par1.pmass;
     par2.pmassq:= par2.pmass*par2.pmass;

END;

  PROCEDURE pardec.getmom(ip: Real);
BEGIN
     parin1.plab:=ip;
END;
  PROCEDURE pardec.getang(ithetcm: Real);
BEGIN
     par1.pthetcm:=ithetcm;

END;

PROCEDURE EraseXY(x,y:integer; num,BkColor:word);
{ It erase num characters ,starting from location x,y.}
VAR
   col,row,color,h,w : integer;
   fill : FillSettingsType;
   text : TextSettingsType;
BEGIN
   color := GetColor;
   SetColor(BkColor);
   GetFillSettings( fill);
   GetTextSettings( text);
   SetFillStyle(SolidFill,BkColor);
   if text.Direction=HorizDir then
   begin
      h := TextHeight('W');
      w := num*TextWidth ('W');
   end
   else
   begin
      w := TextHeight('W');
      h := num*TextWidth ('W');
   end;
   Col := Round(x - w*text.horiz/2);
   Row := Round(y - h*(2 - text.vert)/2 );
   if (text.Direction=VertDir) and (text.horiz=LeftText) then Col := Col-w;
   {HideMouse; }
   Bar(Col, Row, Col+w, Row+h);
   { ShowMouse; }
   SetColor(Color);
   WITH fill DO SetFillStyle( Pattern, Color);
END;


  PROCEDURE OutPut(x,y:integer; Field:String);

BEGIN
      EraseXY(x,y,9,Graphbackcolor);
      setcolor(whitte);
      OutTextXY(x,y, Field);
END;

  PROCEDURE pardec.rescale;
            { find maximum lab momentum and scale vector display}
BEGIN
     calc_s;
     CMvari;
     CMvarf;
     labmax;
     IF (par1.plmax > par2.plmax)THEN
              LabVec:=par1.plmax ELSE LabVec:=par2.plmax;
END;


  PROCEDURE pardec.dmass;      {choose decaying mass from Breit-Wigner}
VAR
   qw  : Real;
BEGIN
     IF (parin1.pwidth > 0.0) THEN
     BEGIN
       qw := 0.0;
       WHILE (qw <= 50.0) DO
         BEGIN
          parin1.pmass:=breitwig(parin1.pcmass,parin1.pwidth);
          qw:= parin1.pmass - par1.pmass - par2.pmass;
         END;
     END;

     parin1.pmassq := parin1.pmass*parin1.pmass;
  {   q:= pmass-par1.pmass-par2.pmass;   }

END;


  PROCEDURE pardec.decay;           {CM decay angles}
VAR
     ithetcm: Real;
     dum    : Integer;
BEGIN
     dum:= 1;
     ithetcm:= par1.pthetcm;
     par2.pthetcm:= 180-ithetcm;
     par1.costhCM:= cos(raddeg*ithetcm);
     par1.sinthCM:= sin(raddeg*ithetcm);
     par2.costhCM:= -par1.costhCM;
     par2.sinthCM:= par1.sinthCM;

{     par1.phi:= 2*cpi*Urand(dum);}
     par1.phi:=2*cpi*random;
     par2.phi:= par1.phi + cpi;

END;


PROCEDURE pardec.calc_s;          {calculate CM variables}

VAR
   pEin, ip : Real;
BEGIN
    ip:= parin1.plab;
    pEin := Sqrt(ip*ip + parin1.pmassq);

    parin1.Elab:= pEin;              {beam particle}
    parin1.KElab:= pEin-parin1.pmass;

    parin2.Elab:= parin2.pmass;      {target particle}
    parin2.plab:= 0;

    s := parin1.pmassq + parin2.pmassq +
                          2 * parin2.pmass * pEin;
    roots := Sqrt(s);
    beta  := ip/(pEin+parin2.pmass);

    IF (beta>macheps) THEN  gamma := ip/(beta*roots)
                      ELSE  gamma := 1;

    avail_energy:= roots-par1.pmass-par2.pmass;

    pE     := roots;
    pp     := beta*roots;
    pmassq := s;
    pmass  := roots;

END;

FUNCTION  pardec.qvalue: Real;
BEGIN
     qvalue:=avail_energy;
END;

PROCEDURE pardec.calc_t;
BEGIN
t:= parin1.pmassq + par1.pmassq - 2*parin1.ecm*par1.ecm
                                + 2*parin1.pcm*par1.pcm*par1.costhCM;

if (t>macheps) THEN q:=0 ELSE q:=Sqrt(-t);

END;


PROCEDURE pardec.CMvari;   {CM variables for two initial particles}
VAR
   argg1,argg2 : Real;
BEGIN

parin1.ecm:= (pmassq + parin1.pmassq-parin2.pmassq)/(2*pmass);
parin2.ecm:= (pmassq + parin2.pmassq-parin1.pmassq)/(2*pmass);

argg1:=  pmassq-sqr(parin1.pmass+parin2.pmass);
argg2:=  pmassq-sqr(parin1.pmass-parin2.pmass);
         IF(argg1 < macheps) THEN argg1:=0;
         IF(argg2 < macheps) THEN argg2:=0;
parin1.pcm:= Sqrt(argg1)* Sqrt(argg2)/(2*pmass);
parin2.pcm:= parin1.pcm;
END;

PROCEDURE pardec.CMvarf;   {CM variables for two final particles}
VAR
   argg1,argg2  : Real;
BEGIN

par1.ecm:= (pmassq + par1.pmassq-par2.pmassq)/(2*pmass);
par2.ecm:= (pmassq + par2.pmassq-par1.pmassq)/(2*pmass);


argg1:=  pmassq-sqr(par1.pmass+par2.pmass);
argg2:=  pmassq-sqr(par1.pmass-par2.pmass);
         IF(argg1 < macheps) THEN argg1:=0;
         IF(argg2 < macheps) THEN argg2:=0;
par1.pcm:= Sqrt(argg1)* Sqrt(argg2)/(2*pmass);

par2.pcm:= par1.pcm;

par1.pbetacm:= par1.pcm/par1.ecm;
par2.pbetacm:= par2.pcm/par2.ecm;

END;

PROCEDURE pardec.labmax;    {maximum momenta in Lab system}
BEGIN

par1.plmax:= gamma*(beta*par1.ecm+par1.pcm);
par2.plmax:= gamma*(beta*par2.ecm+par2.pcm);

END;

PROCEDURE pardec.CMtoLab;   {transform final state particles to Lab}
VAR
   invmsq, argg, argx, argy : Real;
BEGIN
   par1.elab:= gamma*(par1.ecm + beta* par1.pcm* par1.costhcm);
   par2.elab:= gamma*(par2.ecm + beta* par2.pcm* par2.costhcm);


   par1.KELab:= par1.ELab - par1.pmass;
   par2.KELab:= par2.ELab - par2.pmass;

   argg:= par1.Elab*par1.Elab - par1.pmassq ;
   IF(argg < macheps) THEN argg := 0;
   par1.plab:= Sqrt(argg);
   argg:= par2.Elab*par2.Elab - par2.pmassq ;
   IF(argg < macheps) THEN argg := 0;
   par2.plab:= Sqrt(argg);


   par1.pt:= par1.pcm*sin(raddeg* par1.pthetCM);   {transverse momentum}
   par2.pt:= -par1.pt;


   argx:= beta+par1.pbetacm*par1.costhcm;
   argy:= par1.pbetacm*par1.sinthcm;
   IF (abs(argx) < macheps) and (abs(argy) < macheps) THEN par1.pthetlab:=0
   ELSE  par1.pthetlab:= arctan2(argx*gamma,argy);

   argx:= beta+par2.pbetacm*par2.costhcm;
   argy:= par2.pbetacm*par2.sinthcm;
   IF (abs(argx) < macheps) and (abs(argy) < macheps) THEN par2.pthetlab:=0

   ELSE par2.pthetlab:= arctan2(argx*gamma,argy);

   par1.costhlab:= cos(par1.pthetlab);
   par2.costhlab:= cos(par2.pthetlab);
   par1.sinthlab:= sin(par1.pthetlab);
   par2.sinthlab:= sin(par2.pthetlab);

   par1.pthetlab:= par1.pthetlab/raddeg;
   par2.pthetlab:= par2.pthetlab/raddeg;

   par1.px:= par1.plab*par1.sinthlab*cos(par1.phi);
   par1.py:= par1.plab*par1.sinthlab*sin(par1.phi);
   par1.pz:= par1.plab*par1.costhlab;
   par2.px:= par2.plab*par2.sinthlab*cos(par2.phi);
   par2.py:= par2.plab*par2.sinthlab*sin(par2.phi);
   par2.pz:= par2.plab*par2.costhlab;


   par1.angdiff:= par2.pthetlab + par1.pthetlab;
   par2.angdiff:= par1.angdiff;

   {invariant mass}
   invmsq := par1.pmassq+par2.pmassq + 2*par1.Elab*par2.Elab
                  -2*par1.plab*par2.plab*cos(par1.angdiff*raddeg);
   invarmass:= sqrt(invmsq);

   par1.pbeta:= par1.plab/par1.Elab;
   par2.pbeta:= par2.plab/par2.Elab;

END;

FUNCTION  pardec.labtan(ihit: Integer): Real;
BEGIN
   labtan:=0;
   IF (ihit = 1) THEN labtan:=tan(par1.pthetlab*raddeg);
   IF (ihit = 2) THEN labtan:=tan(par2.pthetlab*raddeg);
END;

FUNCTION  pardec.labphi(ihit: Integer): Real;
BEGIN
   labphi:=0;
   IF (ihit = 1) THEN labphi:=par1.phi;
   IF (ihit = 2) THEN labphi:=par2.phi;
END;

FUNCTION  pardec.labenergy(ihit: Integer): Real;
BEGIN
   labenergy:=0;
   IF (ihit = 1) THEN labenergy:=par1.KElab;
   IF (ihit = 2) THEN labenergy:=par2.KElab;
END;

PROCEDURE pardec.fillntuple;
BEGIN
  vcons[theta,lab1].val:= par1.pthetlab;
  vcons[theta,lab2].val:= par2.pthetlab;

  vcons[costheta,lab1].val:= par1.costhlab;
  vcons[costheta,lab2].val:= par2.costhlab;

  vcons[energy,lab1].val:= par1.elab;
  vcons[energy,lab2].val:= par2.elab;

  vcons[momentum,lab1].val:= par1.plab;
  vcons[momentum,lab2].val:= par2.plab;

  vcons[kinetic,lab1].val:= par1.kelab;
  vcons[kinetic,lab2].val:= par2.kelab;

  vcons[angdiff,lab1].val:= par1.angdiff;
  vcons[angdiff,lab2].val:= par2.angdiff;

  vcons[theta,CM1].val:= par1.pthetCM;
  vcons[theta,CM2].val:= par2.pthetCM;

  vcons[costheta,CM1].val:= par1.costhCM;
  vcons[costheta,CM2].val:= par2.costhCM;

  vcons[ptrans,CM1].val:= par1.pt;
  vcons[ptrans,CM2].val:= par2.pt;

  vcons[invmass,CM1].val:= pmass;
  vcons[invmass,CM2].val:= pmass;
  vcons[invmass,lab1].val:= invarmass;
  vcons[invmass,lab2].val:= invarmass;

END;

PROCEDURE pardec.filloutdat;
                                {fill output data array}
BEGIN
  outdata[1] := pmass;
  outdata[2] := par1.pthetCM;
  outdata[3] := par1.plab;
  outdata[4] := par2.plab;
  outdata[5] := par1.pthetlab;
  outdata[6] := par2.pthetlab;
END;

PROCEDURE pardec.WriteData;
VAR
     it : Integer;
BEGIN
     FOR it:=1 to maxout DO Write(outfile,outdata[it]:8:2);
     Writeln(outfile);
END;

  PROCEDURE pardec.list;
BEGIN

     Writeln(par1.pname:12,par1.pmass:10:3,
             par2.pname:12,par2.pmass:10:3,roots:10:2);

   {  Writeln('CM angle ', par1.pthetcm:10:1, ' Lab mom',pp:10:3 ); }
END;

PROCEDURE polvect(x1,y1: Integer; leng,ang: Real);
         {draw vector given starting point, length, angle(degrees)}
VAR
  x2,y2:  Integer;
  angrad: Real;
BEGIN
    angrad:= ang*raddeg;
    x2:= x1 + round(leng*cos(angrad));
    y2:= y1 - round(leng*sin(angrad));
    vector(x1,y1,x2,y2);
END;


PROCEDURE pardec.showCM;
VAR
   vectleng  : Real;
   circcent  : Integer;
   circrad   : Integer;
   boxmax    : Real;
   x,y,radius  : Integer;
   x1,y1,x2,y2 : Integer;
   ang,leng:          Real;
   lengscale   : Real;
BEGIN
   vectleng  :=  50;
   circcent  := 100;
   circrad   :=   3;
   boxmax    := 100;

   IF (LabVec <= 0) THEN LabVec:= LabVecdef;
   lengscale:= boxmax/LabVec;
  { SetFillStyle (0,color);}
   SetColor(LightRed);
   print(1,1,'CM');
   SetColor(par1.pcolord);
   OutTextxy(20,8,CONCAT(numstr(par1.pthetcm,5,0),' deg'));

   x:= circcent;
   y:= circcent;
   radius:= circrad;
   SetColor(parin2.pcolord);
   Circle(X, Y, Radius);
   leng:= par1.pcm*lengscale;
   SetColor(par1.pcolord);
   polvect(x,y,leng,par1.pthetcm);
   SetColor(par2.pcolord);
   leng:= par2.pcm*lengscale;
   polvect(x,y,leng,360-par2.pthetcm);

   SetColor(whitte);
END;

PROCEDURE pardec.showLab;
VAR
   circcent  : Integer;
   circrad   : Integer;
   boxmax    : Real;
   yar       : Integer;

VAR
   x,y,radius  : Integer;
   x1,y1,x2,y2 : Integer;
   ang,leng    : Real;
   lengscale   : Real;
BEGIN
   circcent  := 100;
   circrad   :=   3;
   boxmax    := 100;
   yar       :=  20;

   IF (LabVec <= 0) THEN LabVec:= LabVecdef;
   lengscale:= boxmax/LabVec;
   SETCOLOR(LightGreen);
   print(1,1,'LAB');
   x:= circcent;
   y:= circcent;
   radius:= circrad;

   { target particle}
   SetColor(parin2.pcolord);
   Circle(X, Y, Radius);
   { beam particle}
   SETColor(parin1.pcolord);
   OutTextxy(30,8,CONCAT(numstr(parin1.pLab,5,0),' MeV'));
   leng:=parin1.pLab*lengscale;
   IF (leng > 0) THEN polvect(x,yar,leng,0)
                 ELSE circle(x,yar,radius);

   { decay products}
   leng:= par1.plab*lengscale;
   SetColor(par1.pcolord);
   polvect(x,y,leng,par1.pthetlab);
   leng:= par2.plab*lengscale;
   SetColor(par2.pcolord);
   polvect(x,y,leng,360-par2.pthetlab);
   SetColor(whitte);

END;

PROCEDURE pardec.listgCM;        { Text Output CM frame}
VAR
     h,hs,line: integer;
     pmasst, pcmasst: String[6];
     plus : String[2];
BEGIN
     pmasst  := numStr(roots,6,0);
     pcmasst := numStr(roots,6,0);
     IF (HideMass) THEN
     BEGIN
        pmasst := blank; pcmasst := blank ;
     END;

     h:= TextHeight('0')+4;  hs:= 0;
     SetColor(LightRed);
     line:= 2+hs;
     IF(new) THEN OutTextxy(3,line,'CM system');
     hs:= hs + h;

     SetColor(Yellow);
     line:= 2+hs;

     plus:='+ ';
     IF(parin2.pnick = '    ') THEN plus:='  ';
     IF (new) THEN
     BEGIN
       OutTextxy(3,2+hs,CONCAT(' ', parin1.pnick, plus , parin2.pnick,'--> '
       ,par1.pnick,'+ ',par2.pnick));
     END;

     hs:=hs+3;
 {    OutTextxy(3,2+h+hs,CONCAT(
        decstr(parin1.pcmass,5) ,' ',  decstr(parin2.pmass,5),'->'
        ,decstr(par1.pmass,5)   ,' ',  decstr(par2.pmass,5)));
   }
     SetColor(Whitte);
     hs:=hs+2;
     line:= 2+2*h+hs;
     IF (new) THEN OutTextxy(3,line,  'CM angle  ');
     OutPut(col1,line,numStr(round(par1.pthetcm),4,0));
  {   OutPut(col2,line,CONCAT('t ',numStr(t,6,0)));   }

     line:= 2+3*h+hs;
     IF (new) THEN OutTextxy(3,line,  'sqrt s');
     OutPut(col1,line, pmasst);
     OutPut(col2,line, CONCAT(' q ' , numStr(avail_energy,6,0)));

     hs:=hs+3;
     
     IF (new) THEN
     BEGIN
       SetColor(par2.pcolord);
       OutTextxy(3,2+4*h+hs, CONCAT('        ','         ',par2.pname));
       SetColor(par1.pcolord);
       OutTextxy(3,2+4*h+hs,CONCAT( '        ', par1.pname ));
     END;

     SetColor(whitte);
     
     line:= 2+5*h+hs;
     IF (new) THEN OutTextxy(3,line, 'Energy');    
     OutPut(col1,line,numStr(par1.ecm,9,2));
     OutPut(col2,line,numStr(par2.ecm,9,2));
     
     line:= 2+6*h+hs;
     IF (new) THEN OutTextxy(3,line, 'P     ');    
     OutPut(col1,line,numStr(par1.pcm,9,2));
     OutPut(col2,line,numStr(par2.pcm,9,2));
     
     line:= 2+7*h+hs;
     IF (new) THEN OutTextxy(3,line, 'Pt    ');
     OutPut(col1,line,numStr(par1.pt,9,2));
     OutPut(col2,line,numStr(par2.pt,9,2));

{     OutTextxy(3,2+8*h+hs,CONCAT( 'CosTh '    ,numStr(par1.costhcm,8,3)
                                       ,numStr(par2.costhcm,8,3)));
  }
     line:= 2+8*h+hs;
     IF (new) THEN OutTextxy(3,line, 'theta ');
     OutPut(col1,line,numStr(par1.pthetcm,8,1));
     OutPut(col2,line,numStr(par2.pthetcm,8,1));
     
     line:= 2+9*h+hs;
     IF (new) THEN OutTextxy(3,line,'beta  ');    
     OutPut(col1,line,numStr(par1.pbetacm,8,3));
     OutPut(col2,line,numStr(par2.pbetacm,8,3));
     
     IF (new) THEN
     BEGIN
      OutTextxy(3,2+12*h+hs,'mass, energy, momentum');
      OutTextxy(3,2+13*h+hs,CONCAT('   in',Eunit));
     END;

END;

PROCEDURE pardec.listgLab;        { Text output LAB frame}
VAR
     h,hs,line: integer;
     invmast: String[8];

BEGIN
     invmast  := numStr(invarmass,8,0);
     IF (HideMass) THEN
     BEGIN
        invmast := blank;
     END;


     H:= TextHeight('0')+4;  hs:=0;

      SetColor(LightGreen);
      line:= 2+hs;
      IF(new) THEN OutTextxy(3,line,'LAB system');
      hs:= hs + h;
      SetColor(whitte);
      line:= 2+hs;
      IF(new) THEN OutTextxy(3,line, 'beam KE    ');
      OutPut(col1b,line,numStr(parin1.KELab,6,1) + Eunit);

      line:= 2+h+hs;
      IF(new) THEN OutTextxy(3,line, 'beam momentum');
      OutPut(col1b,line,numStr(parin1.pLab,6,1) + Eunit);

      line:=2+2*h+hs;
      IF(new) THEN OutTextxy(3,line, 'Beta of CM ');
      OutPut(col2,line,numStr(beta,6,3));
      hs:=hs+2;

     IF (new) THEN
     BEGIN
      SetColor(parin1.pcolord);
      OutTextxy(3,2+3*h+hs,CONCAT( '     ',  parin1.pnick));
      SetColor(parin2.pcolord);
      OutTextxy(3,2+3*h+hs,CONCAT( '                     ',parin2.pnick));

      SetColor(whitte);
      OutTextxy(3,2+3*h+hs, 'beam          target ');
      SetColor(par2.pcolord);
      OutTextxy(3,2+4*h+hs,CONCAT( '        ','         ',par2.pname));
      SetColor(par1.pcolord);
      OutTextxy(3,2+4*h+hs,CONCAT( '        ', par1.pname ));
     END;

     SetColor(whitte);
     line:=2+5*h+hs;
     IF (new) THEN OutTextxy(3,line, 'Energy');
     OutPut(col1,line,numStr(par1.elab,9,2));
     OutPut(col2,line,numStr(par2.elab,9,2));

     line:=2+6*h+hs;
     IF (new) THEN OutTextxy(3,line, 'P     ');
     OutPut(col1,line,numStr(par1.plab,9,2));
     OutPut(col2,line,numStr(par2.plab,9,2));

     line:=2+7*h+hs;
     IF (new) THEN OutTextxy(3,line, 'KE    ');
     OutPut(col1,line,numStr(par1.KELab,9,2));
     OutPut(col2,line,numStr(par2.KELab,9,2));

{    OutTextxy(3,2+8*h+hs,CONCAT( 'CosTh '    ,numStr(par1.costhlab,8,3)
                                       ,numStr(par2.costhlab,8,3)));
  }

     line:=2+8*h+hs;
     IF (new)THEN   OutTextXY(3,line, 'theta ');
     OutPut(col1,line,numStr(par1.pthetlab,8,1));
     OutPut(col2,line,numStr(par2.pthetlab,8,1));

     line:=2+9*h+hs;
     IF (new) THEN OutTextxy(3,line,'beta  ');
     OutPut(col1,line,numStr(par1.pbeta,8,3));
     OutPut(col2,line,numStr(par2.pbeta,8,3));

     line:= 2+11*h+hs;
     IF (new) THEN OutTextxy(3,line,'theta diff');
     OutPut(col2,line, numStr(par1.angdiff,8,1));

     line:= 2+12*h+hs;
     IF (new) THEN OutTextxy(3,line,'invariant mass');
     OutPut(col2,line,invmast);

END;


END.
