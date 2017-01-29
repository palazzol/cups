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

Unit NucGraph;

Interface
   Uses
       Graph, Cups, CupsMupp, Cupsfunc, CupsGui, CupsGrph, CupsProc, NucInter;
   Const
      numAngles = 90;
      numChan = 50;

      protonColor = lightgreen;
      muonColor = lightblue;
      electronColor = lightmagenta;
      photonColor = lightred;

      comptonColor = lightgreen;
      photoColor = lightblue;
      pairColor = lightred;
      totalColor = whitte;

      esqr = 1.44e-13;{MeVcm, electron charge squared}
      re = 0.28183e-12;{classical electron radius in cm}
      mec2 = 0.511; {MeV}
      mpc2 = 936.28;{MeV}
      mmc2 = 105.6595;{MeV}
      Degtorad = 0.0174533;        {pi/180}
      RadtoDeg = 57.29578;
      Na = 6.023e+23;
 {  Type }

   var
      transmit, reflect, absorb, out: integer;


   Procedure EraseXY (x, y: integer;
                           numx, numy, BkColor: word);
   procedure SingleLabel (var thisSource: sourcetype; var text: Str25);
   procedure RefreshPlotPath (var Rad: RadType);
   procedure PrintExpInfo (var Rad: RadType;
               thick, scalefactor: real;
               unitStr: Str25);
   procedure InitializePlotPath (thick: real; var Rad: RadType;
                                 var thismaterial: materialType;
                                 thisSource: SourceType;
                                 viewsnum: integer;
                                 var scalefactor: real; var unitStr: Str25);
   procedure PlotPath (var lastPt, thisPt: MyPointType;
               viewsnum: integer;
               var thisSource: sourceType;
               scalefactor: real);
   procedure RedrawPath (viewsnum, lastcollision: integer;
               var thisSource: sourceType;
               var PointVector: PointVectorType;
               scalefactor: real);
   procedure InitializeRangePlot (Rad: RadType;
                thismaterial: materialType;
                atten: real;
                Var deltaT: DVector);
   procedure PlotRangepoints (Rad: RadType;
               atten: real;
               TransDistr, deltaT: DVector);
   procedure PlotRange (Rad: RadType;
               atten: real;
               TransDistr, deltaT: DVector);
   procedure InitializePrintData(thisSource: sourceType);
   procedure PrintData (num, MC: integer;
              var thisSource: sourcetype;
              var thisMaterial: MaterialType);
   procedure InitializeChargeRangePlot (Rad: RadType;
                             thismaterial: MaterialType;
                             RangeDist: DVector);
   procedure PlotChargeRangePoints (k: integer;
                RangeFreq, ReflectFreq, RangeDist: dVector);
   procedure PlotChargeRange (RangeFreq, ReflectFreq, RangeDist: dVector);
   procedure InitPlotAngle (Rad:RadType);
   procedure PlotAngle (Rad: RadType;
                var AngleDistr: dVector;
                collisionAv, viewsnum:integer);
   procedure InitializePlotSigma (var Rad: RadType;
                                  var thisMaterial: MaterialType;
                                  var max: real;
                                  logtype: boolean;
                                  sigtheorytotal: dvector);
   procedure PlotSigmaTheory (var logEarray, SigmaTheoryPhoto, SigmaTheoryCompton,{}
                           SigmaTheoryPair, SigmaTheoryTotal: DVector;
                           var thisMaterial: MaterialType;
                           logtype: boolean; num: integer;
                           max: real);
   procedure InitializePlotdEdxElectron (var Rad: RadType;
               var thisMaterial: MaterialType);
   procedure PlotdEdxElectronTheory (var logE, logdEdx: DVector; ibeg, numbins, num: integer;
                           var thisMaterial: MaterialType);
   procedure InitializePlotdEdxHeavy (var Rad: RadType;
                           var thisMaterial: MaterialType;
                           var thisSource: SourceType);
   procedure PlotdEdxHeavyTheory (var logE, logdEdx: DVector;
                     ibeg, numbins, num: integer;
                     var thisMaterial: MaterialType;
                     var thisSource: sourcetype);
   procedure PrintNaIinfo (var NaI: NaIType);
   procedure InitializePlotSpect (var NaI: NaIType);
   procedure PlotResponseSpect (var NaI: NaIType;
                           var Spectrum: Dmatrix;
                           broaden: boolean;
                           peakchan, ComptonChan: integer;
                           ComptonEnergy: real);

Implementation
   Procedure EraseXY;{ (x, y: integer;
                           numx, numy, BkColor: word);}
 { It erase num characters, starting from location x,y.}
      Var
         col, row, color, h, w: integer;
         fill: FillSettingsType;
         text: TextSettingsType;
   Begin
      color := GetColor;
      SetColor(BkColor);
      GetFillSettings(fill);
      GetTextSettings(text);
      SetFillStyle(SolidFill, BkColor);
      h := numy * TextHeight('0');
      w := numx * TextWidth('W');
      Col := Round(x - w * text.horiz / 2);
      Row := Round(y - h * (2 - text.vert) / 2);
      Bar(Col, Row, Col + w, Row + h);
      SetColor(Color);
      With fill Do
         SetFillStyle(Pattern, Color);
   End;{EraseXY}

   procedure SingleLabel;{ (var thisSource: sourcetype; var text: Str25);}
      var
         i: integer;
   begin
      case thisSource of
         proton:
            begin
               SetColor(protonColor);
               text := ' proton';
            end;
         muon:
            begin
               SetColor(muonColor);
               text := 'muon';
            end;
         electron:
            begin
               SetColor(electronColor);
               text := 'electron';
            end;
         photon:
            begin
               SetColor(photonColor);
               text := 'photon';
            end;
      end;{case thisSource}
   end;{SetSingleLabel}

   procedure RefreshPlotPath;{ (var Rad: RadType);}
      var
         i,viewsnum: integer;
         ticx, ticy, xmin, xmax, ymin, ymax,scalefactor: real;
         text: Str80;

   begin
      with Rad do
         begin
            if (thickness-0.001)<=1e-8 then
                  scalefactor := 10000.0
            else
                  scalefactor := 1.0;
            xmin := 0.0;
            xmax := 1.2 * scalefactor * thickness;
            ymin := -2  * scalefactor * thickness;
            ymax := 2   * scalefactor * thickness;
            viewsnum:= -3;
            for i:= 1 to 2 do begin
               viewsnum:= viewsnum + 4;
               OpenViewPort(viewsnum);
               SelectScale(1);
               SetColor(whitte);
               SetFillStyle(solidFill, lightgray);
               HideMouse;
               bar( mapx(scalefactor * thickness) , mapy(ymin)+1, mapx(xmax), mapy(ymax)-1);
               rectangle( mapx(scalefactor * thickness) , mapy(ymin)+1, mapx(xmax), mapy(ymax)-1);
               ShowMouse;
               Axis(0, 0, TickSpace(xmax - xmin), TickSpace(ymax - ymin));
            end;{for i}
         end;{with Rad}
   end;{RefreshPlotPath}

   procedure PrintExpInfo;{ (var Rad: RadType;
               thick, scalefactor: real;
               unitStr: Str25);}
     var
       text: str25;
       scry1, scry2, scry3: integer;
   begin
     OpenViewPort(7);
     DefineScale(7, 1, 10, 1, 10);
     SelectScale(7);
     scry1 := MapY(9.3);
     scry2 := MapY(7.3);
     scry3 := MapY(5.3);
     SetColor(lightgreen);
     HideMouse;
     OutTextXY(MapX(1.2), scry1, 'Source');
     OutTextXY(MapX(2.4), scry1, 'Energy');
     OutTextXY(MapX(2.4), scry2, '(MeV)');
     OutTextXY(MapX(3.5), scry1, 'Material');
     OutTextXY(MapX(5.1), scry1, 'Width');
     OutTextXY(MapX(5.1), scry2, unitStr);
     if Rad.range then begin
        OutTextXY(MapX(6.2), scry1, 'Range');
        OutTextXY(MapX(6.0), scry2, 'cycle(11)');
      end;
     OutTextXY(MapX(8.0), scry1, 'Particles');
     OutTextXY(MapX(7.7), scry2, 'Total');
     OutTextXY(MapX(9.0), scry2, 'To Go');
     SetColor(whitte);
     with Rad do begin
       OutTextXY(MapX(1.2), scry3, sourceStr);
       OutTextXY(MapX(2.3), scry3, energyStr);
       OutTextXY(MapX(3.5), scry3, materialStr);
       OutTextXY(MapX(5.0), scry3, NumStr(scalefactor * thick, 6, 4));
       if rad.range then
          OutTextXY(MapX(6.3), scry3, NumStr(1, 3, 0));
       OutTextXY(MapX(7.5), scry3, MCtotalStr);
       MCtogoStr := MCtotalStr;
       OutTextXY(MapX(8.8), scry3, MCtogoStr);
       ShowMouse;
      end;{with Rad}
   end;{PrintExpInfo}

   procedure InitializePlotPath;{ (thick: real; var Rad: RadType;
                                 var thismaterial: materialType;
                                 thisSource: SourceType;
                                 viewsnum: integer;
                                 var scalefactor: real; var unitStr: Str25);}
      var
         i: integer;
         ticx, ticy, xmin, xmax, ymin, ymax: real;
         text: Str80;

   begin
      if viewsnum = 1 then begin
         SelectViewPort(0);
         EraseXY(0, 80, 5, 20, blakk);
         EraseXY(15, 243, 40, 2, blakk);
       end;
      with Rad do
         begin
            if (thick - 0.001)<=1e-8 then
               begin
                  scalefactor := 10000.0;
                  unitStr := concat(' (',chr(230),'m)');
               end
            else
               begin
                  scalefactor := 1.0;
                  unitStr := ' (cm)';
               end;
            xmin := -0.1 * scalefactor * thick;
            xmax := 1.2 * scalefactor * thick;
            ymin := -2.5 * scalefactor * thick;
            ymax := 2.5 * scalefactor * thick;
            DefineScale(1, xmin, xmax, ymin, ymax);{path}
            OpenViewPort(viewsnum);
            SelectScale(1);
            SetColor(whitte);
            SetFillStyle(solidFill, lightgray);
            HideMouse;
            bar(mapx(xmin)+1 , mapy(ymin)-1, mapx(0.0), mapy(ymax)+1);
            bar(mapx(scalefactor * thick)+1 , mapy(ymin)-1, mapx(xmax)-1, mapy(ymax)+1);
            rectangle(mapx(0.0), mapy(-2.5*scalefactor*thick), mapx(scalefactor * thick), mapy(2.5*scalefactor*thick));
            ShowMouse;
            Axis(0, 0, TickSpace(xmax - xmin), TickSpace(ymax - ymin));
            case thissource of
             proton: text:= 'proton';
             muon: text:= 'muon';
             electron: text:= 'electron';
             photon: text:= 'photon';
            end;
            with thismaterial do
               PutLabel(bottom, concat('Path of ', text,'s  in ', singleMaterialname, unitStr));
{            setcolor(blakk);
            plotline(xmin, 0.0, 0.0, 0.0); }
         end;{with Rad}
   end;{InitializePlotPath}

   procedure PlotPath;{ (var lastPt, thisPt: MyPointType;
               viewsnum: integer;
               var thisSource: sourceType;
               scalefactor: real);}
      const
        pathColor = blakk;
      var
        i: integer;
        text: Str25;
   begin
      SelectViewPort(viewsnum);
      SelectScale(1);
      SetColor(pathColor);
      PlotLine(lastPt.z * scalefactor, lastPt.x * scalefactor,
                       thisPt.z * scalefactor, thisPt.x * scalefactor);
   end;{PlotPath}

   procedure RedrawPath;{ (viewsnum, lastcollision: integer;
               var thisSource: sourceType;
               var PointVector: PointVectorType;
               scalefactor: real);}
      var
         i: integer;
         Pt1, Pt2: MyPointType;
   begin
      SelectViewPort(viewsnum);
      SelectScale(1);
      case thisSource of
         proton: 
            SetColor(protonColor);
         muon: 
            SetColor(muonColor);
         electron:
            SetColor(electronColor);
         photon:
            SetColor(photonColor);
      end;{case thisSource}
 {     plotline(-0.1 * scalefactor* thick, 0.0, 0.0, 0.0);}
      for i := 1 to lastcollision - 1 do
         begin
            Pt1 := PointVector[i];
            Pt2 := PointVector[i + 1];
            PlotLine(Pt1.z * scalefactor, Pt1.x * scalefactor,
                     Pt2.z * scalefactor, Pt2.x * scalefactor);
         end;
   end;{RedrawPath}

   procedure InitializeRangePlot;{ (Rad: RadType;
                thismaterial: materialType;
                atten: real;
                Var deltaT: DVector);}
    var
     i: integer;
     ticx: real;
     Str, text: Str80;
     expdecay: dVector;

   begin
      expdecay.init(11);
      deltaT.put(1, 0.1 / atten);
      deltaT.put(2, 0.2 / atten);
      deltaT.put(3, 0.4 / atten);
      deltaT.put(4, 0.6 / atten);
      deltaT.put(5, 0.8 / atten);
      deltaT.put(6, 1.0 / atten);
      deltaT.put(7, 1.3 / atten);
      deltaT.put(8, 2.0 / atten);
      deltaT.put(9, 2.3 / atten);
      deltaT.put(10, 3.0 / atten);
      deltaT.put(11, 4.6 / atten);
      with Rad do begin
         Str := ' , cm';
         ticx := deltaT.value(11) / 5.0;
         DefineScale(2, 0.0, deltaT.value(11) * 1.1, 0.0, 1.1);
         OpenViewPort(5);
         SelectScale(2);
         Axis(0, 0, tickspace(deltaT.value(11)), 0.1);
         with thismaterial do
            text := concat('Attenuation in ', singleMaterialname, Str);
         setcolor(whitte);
         PutLabel(bottom, text);
         PutLabel(left, 'Rel. Transmission per cm');
     end;{with Rad}
     for i:= 1 to 11 do
        expDecay.put(i, exp(-atten * deltaT.value(i)));
      PlotDVectors(deltaT, expDecay, 1, 11);
      expDecay.free;
   end;{InitializeRangePlot}

   procedure PlotRangePoints;{ (Rad: RadType;
               atten: real;
               TransDistr, deltaT: DVector);}
     const
       mc = '*';
     var
       i: integer;
       Distr: dVector;
   begin
      Distr.init(11);
      SelectViewPort(5);
      for i:= 1 to 11 do
         Distr.put(i, TransDistr.value(i) / Rad.MonteCarlo);
      SelectScale(2);
      SetColor(lightmagenta);
      Plotpoints(deltaT, Distr, mc);
      Distr.free;
   end;{PlotRangePoints}

   procedure PlotRange;{ (Rad: RadType;
               atten: real;
               TransDistr, deltaT: DVector);}
     var
       i: integer;
   begin
      SelectViewPort(5);
      for i:= 1 to 11 do
         TransDistr.put(i, TransDistr.value(i) / Rad.MonteCarlo);
      SelectScale(2);
      SetColor(lightmagenta);
      PlotDVectors(deltaT, TransDistr, 1, 11);
   end;{PlotRange}

   procedure InitializePrintData;{(thisSource: sourceType);}
   begin
      OpenViewPort(3);
      SelectScale(7);
      SetColor(whitte);
      OutTextXY(MapX(1.2), MapY(9.3), 'Source');
      OutTextXY(MapX(1.2), MapY(8.3), 'Material');
      OutTextXY(MapX(1.2), MapY(6.3), 'Backscattered');
      OutTextXY(MapX(1.2), MapY(5.3), 'Absorbed');
      OutTextXY(MapX(1.2), MapY(4.3), 'Transmitted');
      if thisSource = photon then begin
         OutTextXY(MapX(1.2), MapY(3.3), 'Scat. Out');
         OutTextXY(MapX(1.2), MapY(2.3), concat(chr(230),', cm^-1'));
       end;
   end;{InitializePrintData}

   procedure PrintData;{ (num, MC: integer;
              var thisSource: sourcetype;
              var thisMaterial: MaterialType);}
      var
         offset: integer;
         sourcename: Str25;
   begin
      if (num = 1) and (MC = 1) then
         InitializePrintData(thisSource);
      if num = 1 then
         begin
            SelectViewPort(3);
            offset := MapX(4.3);
         end
      else
         begin
            SelectViewPort(3);
            offset := MapX(8.0);
         end;
      case thisSource of
         proton: begin
               SetColor(protoncolor);
               sourcename := 'proton';
            end;
         muon: begin
               SetColor(muoncolor);
               sourcename := 'muon';
            end;
         electron: begin
               SetColor(electroncolor);
               sourcename := 'electron';
            end;
         photon: begin
               SetColor(photoncolor);
               sourcename := 'photon';
            end;
      end;{case thisSource}
      SetColor(yellow);
      OutTextXY(offset, MapY(9.3), sourcename);
      OutTextXY(offset, MapY(8.3), thisMaterial.singlematerialname);
      EraseXY(offset, MapY(6.3), 5, 1, darkgray);
      OutTextXY(offset, MapY(6.3), NumStr(reflect, 5, 0));
      EraseXY(offset, MapY(5.3), 5, 1, darkgray);
      OutTextXY(offset, MapY(5.3), NumStr(absorb, 5, 0));
      EraseXY(offset, MapY(4.3), 5, 1, darkgray);
      OutTextXY(offset, MapY(4.3), NumStr(transmit, 5, 0));
      if thisSource = photon then begin
         EraseXY(offset, MapY(3.3), 5, 1, darkgray);
         OutTextXY(offset, MapY(3.3), NumStr(out, 5, 0));
       end;
   end;{PrintData}

   procedure InitializeChargeRangePlot;{ (Rad: RadType;
                             thismaterial: MaterialType;
                             RangeDist: DVector);}
      var
       i: integer;
       ticx, ticy, xmin, xmax, ymin, ymax: real;
       unitStr, text: str80;

   begin
      with Rad do begin
         unitStr := '(cm)';
         xmin := 0.0;
         xmax := RangeDist.value(10) * 1.2;
         ymin := 0.0;
         ymax := 1.1;
         DefineScale(5, xmin, xmax, ymin, ymax);
         OpenViewPort(5);
         SelectScale(5);
         Axis(0, 0, tickspace(xmax), 0.1);
         with thismaterial do
            text := concat('Range in ', singleMaterialname, unitStr);
         setcolor(whitte);
         putLabel(bottom, text);
         putLabel(left, 'Rel. freq. per cm');
         setcolor(lightmagenta);
         print(16, 1, '-*-Transmitted');
         setcolor(lightblue);
         print(16, 2, '-*-Backscattered');
       end;{with Rad}
   end;{InitializeChargeRangePlot}

   procedure PlotChargeRangePoints;{ (k: integer;
                RangeFreq, ReflectFreq, RangeDist: dVector);}
     const
       mc = '*';
     var
       i: integer;

   begin
      SelectViewPort(5);
      SelectScale(5);
      SetColor(lightmagenta);
      Plotpoints(RangeDist, RangeFreq, mc);
      SetColor(lightblue);
      Plotpoints(RangeDist, ReflectFreq, mc);
    end;{PlotChargeRangePoints}

   procedure PlotChargeRange;{ (RangeFreq, ReflectFreq, RangeDist: dVector);}
   begin
      SelectViewPort(5);
      SelectScale(5);
      SetColor(lightmagenta);
      PlotDVectors(RangeDist, RangeFreq, 1, 11);
      SetColor(lightblue);
      PlotDVectors(RangeDist, ReflectFreq, 1, 11);
   end;{PlotChargeRange}

   procedure InitPlotAngle;{ (Rad:RadType);}
      var
        i: integer;
        angle, resolution, deltaAngle, Emc2, costheta, term1, term2, norm: real;
        theta, cross: dVector;
   begin
      theta.init(180);
      cross.init(180);
{         SelectViewPort(0);
         EraseXY(318, 82, 5, 21, blakk); }
      OpenViewPort(2);
      SetColor(whitte);
      Emc2:= Rad.Energy / mec2;
      resolution := round(180 / NumAngles);
      deltaAngle := resolution / 2;
      for i := 1 to NumAngles do begin
         Angle := round(i * resolution) - deltaAngle;
         theta.Put(i, Angle);
          {Calculate the prob. for scattering into theta from Klein-Nishina formula}
         costheta:= cos(degtorad * Angle);
         term1:= 1 / sqr(1 + Emc2 * (1 - costheta));
         term2:= sqr(Emc2 * (1 - costheta)) / (1 + Emc2 * (1 - costheta));
         cross.put(i, 0.5 * sqr(re) * (term1 * (1 + sqr(costheta) + term2)));
       end;
      norm:= cross.value(1);
      for i := 1 to NumAngles do
         cross.put(i, cross.value(i) / norm);
      AutoScaling(10, theta, cross);
      SelectScale(10);
      setcolor(whitte);
      PutLabel(left, 'Rel. Freq.');
      PutLabel(bottom, 'angle, degrees');
      PlotDVectors(theta, cross, 1, NumAngles);
      cross.free;
      theta.free;
   end;{InitPlotAngle}

   procedure PlotAngle;{ (Rad: RadType;
                var AngleDistr: dVector;
                collisionAv, viewsnum:integer);}
      const
         mc = '*';
      var
         i, resolution: integer;
         Emc2,deltaAngle, Angle, costheta, term1, term2, norm1, norm2: real;
         theta, Distr, cross: dvector;
   begin
      theta.init(180);
      Distr.init(180);
      cross.init(180);
      if viewsnum = 1 then begin
         SelectViewPort(0);
         EraseXY(318, 82, 5, 21, blakk);
         OpenViewPort(2);
       end
      else
         SelectViewPort(2);
      SetColor(whitte);
      Emc2:= Rad.Energy / mec2;
      resolution := round(180 / NumAngles);
      deltaAngle := resolution / 2;
      for i := 1 to NumAngles do begin
         Angle := round(i * resolution) - deltaAngle;
         theta.Put(i, Angle);
         Distr.Put(i, AngleDistr.value(i){ / sin(degtorad * Angle)});
          {Calculate the prob. for scattering into theta from Klein-Nishina formula}
         costheta:= cos(degtorad * Angle);
         term1:= 1 / sqr(1 + Emc2 * (1 - costheta));
         term2:= sqr(Emc2 * (1 - costheta)) / (1 + Emc2 * (1 - costheta));
         cross.put(i, 0.5 * sqr(re) * (term1 * (1 + sqr(costheta) + term2)));
       end;
      norm1:= 0.0;
      for i:= 1 to 5 do
         norm1:= norm1 + Distr.value(i);
      norm1:= norm1 / 5;
      if norm1 = 0.0 then
         norm1:= 1.0;
      norm2:= cross.value(1);
      for i := 1 to NumAngles do begin
         Distr.Put(i, Distr.value(i) / norm1);
         cross.put(i, cross.value(i) / norm2);
       end;
      if viewsnum = 1 then
         AutoScaling(10, theta, Distr);
      SelectScale(10);
      setcolor(whitte);
      PutLabel(left, 'Rel. Freq.');
      PutLabel(bottom, 'angle, degrees');
      If viewsnum = 1 then
         SetColor(lightmagenta)
      else
         SetColor(lightblue);
      PlotPoints(theta, Distr, mc);
      PlotDVectors(theta, Distr, 1, NumAngles);
      If viewsnum = 1 then begin
         setColor(whitte);
         PlotDVectors(theta, cross, 1, NumAngles);
       end;
 {     setcolor(yellow);
      print(5,4,concat('Ave. # of coll. = ',numstr(collisionAv,4,0)));}
      cross.free;
      theta.free;
      Distr.free;
   end;{PlotAngle}

   procedure InitializePlotSigma;{ (var Rad: RadType;
                                  var thisMaterial: MaterialType;
                                  var max: real;
                                  logtype: boolean;
                                  sigtheorytotal: dvector); }
      var
         i: integer;
         sizeTicY, scaleYmax, zeroy: real;
         text1: Str25;
         text2: Str80;
   begin
      with thisMaterial do begin
         if logtype then begin
            max:= sigtheorytotal.value(1);
            for i:= 2 to 59 do
               if sigtheorytotal.value(i) > max then
                  max:= sigtheorytotal.value(i);
            max := max * 1.1;
            DefineScale(3, -1, 3.1, -2, max);
            sizeticy := 1;
            zeroy := 0;
            text1 := 'Sigma, b/atom (log)';
            text2 := concat('Energy, MeV (log) ', singleMaterialname);
          end
         else begin
            scaleYmax := sigmax[1] * 1.1;
            DefineScale(3, -1.1, 3.1, 0, scaleYmax);{sigma}
            sizeTicY := TickSpace(scaleYmax - 0.0);{scaleYmax / 10}
            zeroy := 0;
            text1 := 'Sigma, b/atom';
            text2 := concat('Energy, MeV (log) ', singleMaterialname);
          end;
         OpenViewPort(5);
         SelectScale(3);
         SetColor(whitte);
         Axis(-1, -2, 1, 1);
         PutLabel(left, text1);
         PutLabel(bottom, text2);
      end;{with thisMaterial}
   end;{InitializePlotSigma}

   procedure PlotSigmaTheory;{ (var logEarray, SigmaTheoryPhoto, SigmaTheoryCompton,
                           SigmaTheoryPair, SigmaTheoryTotal: DVector;
                           var thisMaterial: MaterialType;
                           logtype: boolean; num: integer;
                           max: real);}
      var
         text: Str25;
         scry: integer;
   begin
      SelectViewPort(5);
      SelectScale(3);
      scry := MapY(max);
   {  if not logtype then begin }
         SetColor(PhotoColor);
         PlotDVectors(logEarray, SigmaTheoryPhoto, 1, 59);
         OutTextXY(MapX(0.5), scry + 5, '__ Photoelectric');
         SetColor(ComptonColor);
         PlotDVectors(logEarray, SigmaTheoryCompton, 1, 59);
         OutTextXY(MapX(0.5), scry + 15, '__ Compton');
         SetColor(PairColor);
         PlotDVectors(logEarray, SigmaTheoryPair, 1, 59);
         OutTextXY(MapX(0.5), scry + 25, '__ Pair Production');
         text := ' ';
         SetColor(totalColor);
         OutTextXY(MapX(0.5), scry + 35, '__ Total');
{      end{if}
{     else begin
         text := thisMaterial.singleMaterialname;
         setColor(num);
       end;
      Print(12, num, text);}
      PlotDVectors(logEarray, SigmaTheoryTotal, 1, 59);
   end;{PlotSigmaTheory}

   procedure InitializePlotdEdxElectron;{ (var Rad: RadType;
               var thisMaterial: MaterialType);}
      var
         i: integer;
         sizeTicY, scaleYmax: real;
         text: Str25;
   begin
      with thisMaterial do
         begin
            DefineScale(4, -4, 5.1, -1, 5);{dEdx}
            OpenViewPort(5);
            SelectScale(4);
            SetColor(whitte);
            Print(3, 1, '__Total');
            SetColor(lightred);
            Print(3, 2, '__Collision');
            SetColor(lightblue);
            Print(3, 3, '__Radiation');
            Axis(-4, -1, 1, 1);
            SetColor(whitte);
            PutLabel(left, 'dE/ds MeV cm^2/g (log)');
            PutLabel(bottom, 'E, MeV (log)');
         end;{with thisMaterial}
   end;{InitializePlotdEdxElectron}

   procedure PlotdEdxElectronTheory;{ (var logE, logdEdx: DVector; ibeg, numbins, num: integer;
                           var thisMaterial: MaterialType);}
      var
         text: Str80;
   begin
      SelectViewPort(5);
      SelectScale(4);
      SetColor(num);
      PlotDVectors(logE, logdEdx, ibeg, numbins);
   end;{PlotdEdxElectronTheory}

   procedure InitializePlotdEdxHeavy;{ (var Rad: RadType;
                           var thisMaterial: MaterialType;
                           var thisSource: SourceType);}
      var
         i: integer;
         sizeTicY, scaleYmax: real;
         text: Str80;
   begin
      with thisMaterial do begin
         DefineScale(6, -4, 5.1, -1, 5);{dEdx}
         OpenViewPort(5);
         SelectScale(6);
         SetColor(whitte);
         Axis(-4, -1, 1, 1);
{           if thisSource = proton then
               PutLabel(inside, concat('protons in ', singleMaterialname))
            else if thisSource = muon then
               PutLabel(inside, concat('muon in ', singleMaterialname));}
         PutLabel(left, 'dE/ds');
         PutLabel(bottom, 'E  log');
       end;{with thisMaterial}
   end;{InitializePlotdEdxHeavy}

   procedure PlotdEdxHeavyTheory;{ (var logE, logdEdx: DVector;
                     ibeg, numbins, num: integer;
                     var thisMaterial: MaterialType;
                     var thisSource: sourcetype);}
      var
         text: Str80;
   begin
      if num = 1 then
      SelectViewPort(5);
      SelectScale(6);
      if thisSource = proton then begin
         text := 'protons';
         setcolor(protoncolor);
       end
      else if thisSource = muon then begin
         text := 'muons';
         setcolor(muoncolor);
       end;
      text := concat(text, ' in ', thisMaterial.singlematerialname);
      Print(5, num + 1, text);
      SetColor(whitte);
      Print(3, 1, '__Theory');
      {SetColor(num);}
      PlotDVectors(logE, logdEdx, ibeg, numbins);
   end;{PlotdEdxHeavyTheory}

   procedure PrintNaIinfo;{ (var NaI: NaIType);}
   var
      scry1, scry2, scry3: integer;
   begin
      SetColor(lightgreen);
      OpenViewPort(8);
      DefineScale(7, 1, 10, 1, 10);
      SelectScale(7);
      scry1 := MapY(9.2);
      scry2 := MapY(6.5);
      scry3 := MapY(3.5);
      HideMouse;
      OutTextXY(MapX(1.2), scry1, 'Energy');
      OutTextXY(MapX(1.25), scry2, '(MeV)');
      OutTextXY(MapX(2.3), scry1, 'Source to Det.');
      OutTextXY(MapX(2.7), scry2, '(cm)');
      OutTextXY(MapX(4.9), scry1, 'Photons');
      OutTextXY(MapX(4.5), scry2, 'Total');
      OutTextXY(MapX(5.6), scry2, 'To Go');
      OutTextXY(MapX(6.9), scry1, '   Peak-Total ');
      OutTextXY(MapX(6.9), scry2, 'SD Peak-Total ');
      OutTextXY(MapX(6.9), scry3, '   Efficiency ');
      SetColor(whitte);
      with NaI do
         begin
            OutTextXY(MapX(1.25), scry3, energyStr);
            OutTextXY(MapX(2.7), scry3, DetSourceDistStr);
            OutTextXY(MapX(4.4), scry3, MCtotalStr);
            MCtogoStr := MCtotalStr;
            OutTextXY(MapX(5.5), scry3, MCtogoStr);
         end;{with NaI}
      ShowMouse;
   end;{PrintNaIinfo}

   procedure InitializePlotSpect;{ (var NaI: NaIType); }
      var
         i: integer;
         TicX, TicY, scXmin, scXmax, scYmin, scYmax: real;
   begin
      with NaI do begin
         scXmax := Esource * 1.15;
         scYmax := 0.1;
         scXmin := 0.0;
         scYmin := -8.1;
         DefineScale(8, scXmin, scXmax, scYmin, scYmax);
         Ticy := 1;
         SetColor(whitte);
         OpenViewPort(11);
         SelectScale(8);
         Axis(0.0, 0.0, TickSpace(scXmax - scXmin), TicY);
         PutLabel(left, 'Counts per MeV (log)');
         PutLabel(bottom, 'Unbroaden E (MeV)');
         scYmin := -3.1;
         DefineScale(9, scXmin, scXmax, scYmin, scYmax);
         OpenViewPort(12);
         SelectScale(9);
         Axis(0.0, 0.0, TickSpace(scXmax - scXmin), TicY);
         PutLabel(left, 'Counts per MeV (log)');
         PutLabel(bottom, 'Broaden E  (MeV)');
         end;{with NaI}
   end;{InitializePlotSpect}

   procedure PlotResponseSpect;{ (var NaI: NaIType;
                           var Spectrum: Dmatrix;
                           broaden: boolean;
                           peakchan, ComptonChan: integer;
                           ComptonEnergy: real); }
      const
         MC = '*';
      var
         i, viewsnum: integer;
         temp: real;
         pulseheight, logcounts: dVector;
   begin
{new(pulseheight);}
{new(logcounts);}
      pulseheight.init(numChan);
      logcounts.init(numChan);
      if broaden then
         viewsnum := 12
      else
         viewsnum := 11;
      for i := 1 to numChan do begin
         pulseheight.Put(i, spectrum.Value(1, i));
         if spectrum.value(2, i) > 1e-8 then
            logcounts.put(i, log10(spectrum.value(2, i)))
         else begin
            temp := 1e-8;
            logcounts.put(i, log10(temp));
          end;{else}
       end;{for i}
      OpenViewPort(viewsnum);
      if viewsnum = 11 then begin
{         EraseXY(8, 65, 4, 45, blakk);
         EraseXY(8, 427, 40, 1, blakk);
         setcolor(whitte);
         PutLabel(bottom, 'Unbroaden  E (MeV)');}
         SelectScale(8);
         with graphScales[8] do
          Axis(0.0, 0.0, TickSpace(xmax - xmin), 1);
       end
      else begin
{         EraseXY(315, 65, 4, 45, blakk);
         EraseXY(315, 427, 40, 1, blakk);
         setcolor(whitte);
         PutLabel(bottom, 'Broaden  E (MeV)');}
         SelectScale(9);
         with graphScales[9] do
          Axis(0.0, 0.0, TickSpace(xmax - xmin), 1);
       end;
      SetColor(lightred);
      Plotpoints(pulseheight, logcounts, MC);
      PlotDVectors(pulseheight, logcounts, 1, NumChan);
(*      SetColor(lightblue);
      PlotLine(pulseheight.value(peakChan), -10, pulseheight.value(peakChan), 0);
      PlotLine(ComptonEnergy, -10, ComptonEnergy, 0);
      if NaI.Esource >= 1.5 then begin
         PlotLine(NaI.Esource-0.511, -10, NaI.Esource-0.511, 0);
         PlotLine(NaI.Esource-1.02, -10, NaI.Esource-1.02, 0);
        {PlotLine(pulseheight.value(ComptonChan), -10, pulseheight.value(ComptonChan), 0);}
       end;                               *)
      pulseheight.free;
      logcounts.free;
{dispose(pulseheight);}
{dispose(logcounts);}
   end;{PlotResponseSpect}



End.{unit RadGraph}
