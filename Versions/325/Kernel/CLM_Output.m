(* ============================================== *)
(* :Summary: Various output functions for CLM. *)
(* ============================================== *)
(* :Author: Konstantin K. Konstantinov *)
(* :Email: konstantin.k.konstantinov@gmail.com *)
(* :License type: GPL v3 or any later version, see http://www.gnu.org/licenses/ *)
(* :Copyright: K^3, 2013 - 2017 *)
(* :Version: 3.25 .001, Date : 2017/02/26 *)
(* :Mathematica Version: 10.0 *)
(* ============================================== *)
(* This program is free software: you can redistribute it and/or modify it under the terms *)
(* of the GNU General Public License as published by the Free Software Foundation, *)
(* either version 3 of the License, or any later version. This program is distributed in the hope that  *)
(* it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY *)
(* or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details. *)
(* You should have received a copy of the GNU General Public License along with this program. *)
(* If not, see <http://www.gnu.org/licenses/>. *)
(* ============================================== *)
(* ============================================== *)
Options[CLMOutput] = {PrintSubstanceMatrix -> True, PrintAllChainsTbl -> True, PrintGetReactionInfo -> True, PrintCoeffArray -> True, PrintEqMatrix -> True};
(* ============================================== *)
legendNames = {"x", "y", "z", "t", "v", "w", "a", "b", "c"};
(* ============================================== *)
BDIMAGESIZE = 500;
xName = "x";
yName = "y";
zName = "z";

BDFONTSIZE = 16;
BDFONTFAMILY = "Courier";
BDFONTWEIGHT = "Bold";

BDPLTTEXTOPTS = {FontFamily -> BDFONTFAMILY, FontSize -> BDFONTSIZE, FontWeight -> BDFONTWEIGHT};
lightingVal = Automatic;
(* ============================================== *)
(* Function to plot vectors. *)
IndexedVariableFunc[idxVal_?NumericQ, vect_?VectorQ] := Module[{idx, retVal, len},
  len = Length[vect];
  idx = Round[idxVal];
  retVal = If[idx < 1 || idx > len, Indeterminate, vect[[idx]]];
  Return[retVal];
];
(* ============================================== *)
(* Function to set the legends of 2D plots. *)
SetLegends[names_?VectorQ] := Module[{len, ii, legLen},
  len = Length[names];
  legLen = Length[legendNames];
  For[ii = 1, ii <= Min[len, legLen], ii++,
    (
      legendNames[[ii]] = ToString[names[[ii]]];
    )
  ];

(* Print["SetLegends::New names: ", legendNames]; *)
];
(* ============================================== *)
(*
discrPlotOpts:={PlotRange\[Rule]All,PlotMarkers\[Rule]{Automatic,Medium},Frame\[Rule] True,GridLines\[Rule]Automatic,ImageSize \[Rule] BDIMAGESIZE,LabelStyle \[Rule] BDPLTTEXTOPTS,AxesLabel\[Rule]{xName,yName},ExtentSize\[Rule]Full,ExtentElementFunction \[Rule] "FadingRectangle",PlotLegends\[Rule]Placed[SwatchLegend[{Style["xxx",FontFamily\[Rule]"Courier",FontSize\[Rule]20,FontWeight\[Rule]"Bold"]}],Above]};
*)

discrPlotOpts := {PlotRange -> All, PlotMarkers -> {Automatic, Medium}, Frame -> True, GridLines -> Automatic, ImageSize -> BDIMAGESIZE, LabelStyle -> BDPLTTEXTOPTS, AxesLabel -> {xName, yName}, ExtentSize -> Full, ExtentElementFunction -> "FadingRectangle", AxesOrigin -> {0, 0}, PlotLegends -> Placed[SwatchLegend[{Style[legendNames[[1]], FontFamily -> BDFONTFAMILY, FontSize -> BDFONTSIZE, FontWeight -> BDFONTWEIGHT], Style[legendNames[[2]], FontFamily -> BDFONTFAMILY, FontSize -> BDFONTSIZE, FontWeight -> BDFONTWEIGHT], Style[legendNames[[3]], FontFamily -> BDFONTFAMILY, FontSize -> BDFONTSIZE, FontWeight -> BDFONTWEIGHT], Style[legendNames[[4]], FontFamily -> BDFONTFAMILY, FontSize -> BDFONTSIZE, FontWeight -> BDFONTWEIGHT],
  Style[legendNames[[5]], FontFamily -> BDFONTFAMILY, FontSize -> BDFONTSIZE, FontWeight -> BDFONTWEIGHT], Style[legendNames[[6]], FontFamily -> BDFONTFAMILY, FontSize -> BDFONTSIZE, FontWeight -> BDFONTWEIGHT]}], Above]};

discrPlotOpts2 := {PlotRange -> All, PlotMarkers -> {Automatic, Medium}, Frame -> True, GridLines -> Automatic, ImageSize -> BDIMAGESIZE, LabelStyle -> BDPLTTEXTOPTS, AxesLabel -> {xName, yName}, ExtentSize -> 0.5, ExtentElementFunction -> "FadingRectangle", AxesOrigin -> {0, 0}, PlotLegends -> Placed[SwatchLegend[{Style[legendNames[[1]], FontFamily -> BDFONTFAMILY, FontSize -> BDFONTSIZE, FontWeight -> BDFONTWEIGHT], Style[legendNames[[2]], FontFamily -> BDFONTFAMILY, FontSize -> BDFONTSIZE, FontWeight -> BDFONTWEIGHT], Style[legendNames[[3]], FontFamily -> BDFONTFAMILY, FontSize -> BDFONTSIZE, FontWeight -> BDFONTWEIGHT], Style[legendNames[[4]], FontFamily -> BDFONTFAMILY, FontSize -> BDFONTSIZE, FontWeight -> BDFONTWEIGHT],
  Style[legendNames[[5]], FontFamily -> BDFONTFAMILY, FontSize -> BDFONTSIZE, FontWeight -> BDFONTWEIGHT], Style[legendNames[[6]], FontFamily -> BDFONTFAMILY, FontSize -> BDFONTSIZE, FontWeight -> BDFONTWEIGHT]}], Above]};

discrPlotOneColorOpts := {PlotRange -> All, PlotMarkers -> {Automatic, Medium}, Frame -> True, GridLines -> Automatic, ImageSize -> BDIMAGESIZE, LabelStyle -> BDPLTTEXTOPTS, AxesLabel -> {xName, yName}, ExtentSize -> Full, ExtentElementFunction -> "FadingRectangle", FillingStyle -> {Red, Red}, (* ColorFunction \[Rule] 0, *)PlotLegends -> Placed[SwatchLegend[{Style[legendNames[[1]], FontFamily -> BDFONTFAMILY, FontSize -> BDFONTSIZE, FontWeight -> BDFONTWEIGHT], Style[legendNames[[2]], FontFamily -> BDFONTFAMILY, FontSize -> BDFONTSIZE, FontWeight -> BDFONTWEIGHT], Style[legendNames[[3]], FontFamily -> BDFONTFAMILY, FontSize -> BDFONTSIZE, FontWeight -> BDFONTWEIGHT], Style[legendNames[[4]], FontFamily -> BDFONTFAMILY, FontSize -> BDFONTSIZE, FontWeight -> BDFONTWEIGHT],
  Style[legendNames[[5]], FontFamily -> BDFONTFAMILY, FontSize -> BDFONTSIZE, FontWeight -> BDFONTWEIGHT], Style[legendNames[[6]], FontFamily -> BDFONTFAMILY, FontSize -> BDFONTSIZE, FontWeight -> BDFONTWEIGHT]}], Above]};

discrPlotTwoColorOpts := {PlotRange -> All, PlotMarkers -> {Automatic, Medium}, Frame -> True, GridLines -> Automatic, ImageSize -> BDIMAGESIZE, LabelStyle -> BDPLTTEXTOPTS, AxesLabel -> {xName, yName}, ExtentSize -> Full, ExtentElementFunction -> "FadingRectangle", FillingStyle -> {Red, Blue}, (* ColorFunction \[Rule] 0, *)PlotLegends -> Placed[SwatchLegend[{Style[legendNames[[1]], FontFamily -> BDFONTFAMILY, FontSize -> BDFONTSIZE, FontWeight -> BDFONTWEIGHT], Style[legendNames[[2]], FontFamily -> BDFONTFAMILY, FontSize -> BDFONTSIZE, FontWeight -> BDFONTWEIGHT], Style[legendNames[[3]], FontFamily -> BDFONTFAMILY, FontSize -> BDFONTSIZE, FontWeight -> BDFONTWEIGHT], Style[legendNames[[4]], FontFamily -> BDFONTFAMILY, FontSize -> BDFONTSIZE, FontWeight -> BDFONTWEIGHT],
  Style[legendNames[[5]], FontFamily -> BDFONTFAMILY, FontSize -> BDFONTSIZE, FontWeight -> BDFONTWEIGHT], Style[legendNames[[6]], FontFamily -> BDFONTFAMILY, FontSize -> BDFONTSIZE, FontWeight -> BDFONTWEIGHT]}], Above]};

discrPlotTwoColorOpts2 := {PlotRange -> All, PlotMarkers -> {Automatic, Medium}, Frame -> True, GridLines -> Automatic, ImageSize -> BDIMAGESIZE, LabelStyle -> BDPLTTEXTOPTS, AxesLabel -> {xName, yName}, ExtentSize -> Full, ExtentElementFunction -> "FadingRectangle", FillingStyle -> {LightRed, LightBlue}, (* ColorFunction \[Rule] 0, *)PlotLegends -> Placed[SwatchLegend[{Style[legendNames[[1]], FontFamily -> BDFONTFAMILY, FontSize -> BDFONTSIZE, FontWeight -> BDFONTWEIGHT], Style[legendNames[[2]], FontFamily -> BDFONTFAMILY, FontSize -> BDFONTSIZE, FontWeight -> BDFONTWEIGHT], Style[legendNames[[3]], FontFamily -> BDFONTFAMILY, FontSize -> BDFONTSIZE, FontWeight -> BDFONTWEIGHT], Style[legendNames[[4]], FontFamily -> BDFONTFAMILY, FontSize -> BDFONTSIZE, FontWeight -> BDFONTWEIGHT],
  Style[legendNames[[5]], FontFamily -> BDFONTFAMILY, FontSize -> BDFONTSIZE, FontWeight -> BDFONTWEIGHT], Style[legendNames[[6]], FontFamily -> BDFONTFAMILY, FontSize -> BDFONTSIZE, FontWeight -> BDFONTWEIGHT]}], Above]};


discrPlotOpts2 := {PlotRange -> All, PlotMarkers -> {Automatic, Medium}, Frame -> True, GridLines -> Automatic, ImageSize -> BDIMAGESIZE, LabelStyle -> BDPLTTEXTOPTS, AxesLabel -> {xName, yName}, ExtentSize -> Full (*,ExtentElementFunction \[Rule] "FadingRectangle" *), PlotLegends -> Placed[SwatchLegend[{Style[legendNames[[1]], FontFamily -> BDFONTFAMILY, FontSize -> BDFONTSIZE, FontWeight -> BDFONTWEIGHT], Style[legendNames[[2]], FontFamily -> BDFONTFAMILY, FontSize -> BDFONTSIZE, FontWeight -> BDFONTWEIGHT], Style[legendNames[[3]], FontFamily -> BDFONTFAMILY, FontSize -> BDFONTSIZE, FontWeight -> BDFONTWEIGHT], Style[legendNames[[4]], FontFamily -> BDFONTFAMILY, FontSize -> BDFONTSIZE, FontWeight -> BDFONTWEIGHT],
  Style[legendNames[[5]], FontFamily -> BDFONTFAMILY, FontSize -> BDFONTSIZE, FontWeight -> BDFONTWEIGHT], Style[legendNames[[6]], FontFamily -> BDFONTFAMILY, FontSize -> BDFONTSIZE, FontWeight -> BDFONTWEIGHT]}], Above]};

discrPlotOpts3 := {PlotRange -> All, PlotMarkers -> {Automatic, Small}, Frame -> True, GridLines -> Automatic, ImageSize -> BDIMAGESIZE, LabelStyle -> BDPLTTEXTOPTS, AxesLabel -> {xName, yName}, PlotLegends -> Placed[SwatchLegend[{Style[legendNames[[1]], FontFamily -> BDFONTFAMILY, FontSize -> BDFONTSIZE, FontWeight -> BDFONTWEIGHT], Style[legendNames[[2]], FontFamily -> BDFONTFAMILY, FontSize -> BDFONTSIZE, FontWeight -> BDFONTWEIGHT], Style[legendNames[[3]], FontFamily -> BDFONTFAMILY, FontSize -> BDFONTSIZE, FontWeight -> BDFONTWEIGHT], Style[legendNames[[4]], FontFamily -> BDFONTFAMILY, FontSize -> BDFONTSIZE, FontWeight -> BDFONTWEIGHT],
  Style[legendNames[[5]], FontFamily -> BDFONTFAMILY, FontSize -> BDFONTSIZE, FontWeight -> BDFONTWEIGHT], Style[legendNames[[6]], FontFamily -> BDFONTFAMILY, FontSize -> BDFONTSIZE, FontWeight -> BDFONTWEIGHT]}], Above]};

(* PlotLegends\[Rule]Placed[{Style["xxx",FontFamily\[Rule]"Courier",FontSize\[Rule]20,FontWeight\[Rule]"Bold"]},Above] *)

(* SwatchLegend *)
discrPlot3DOpts := {PlotRange -> All, ImageSize -> BDIMAGESIZE, ExtentSize -> Full, LabelStyle -> BDPLTTEXTOPTS, AxesLabel -> {xName, yName, ""}, PlotLabel -> zName, Lighting -> lightingVal};
(* ============================================== *)
OutputFunc[idxFunc_?NumericQ, solIdx_?NumericQ, xVal_?NumericQ, yVal_?NumericQ, interpolationTables_] := Module[{retVal},

  If[idxFunc == 1, retVal = \[Eta]OutputFunc[solIdx, xVal, yVal, interpolationTables];];
  If[idxFunc == 2, retVal = \[Eta]BOutputFunc[solIdx, xVal, yVal, interpolationTables];];

  Return[retVal];
];
(* ============================================== *)
\[Eta]OutputFunc[solIdx_?NumericQ, xVal_?NumericQ, yVal_?NumericQ, interpolationTables_] := Module[{retVal},
  retVal = interpolationTables[[1]][[solIdx]][xVal, yVal];

  (* Print["OutputFunc::retVal = ", retVal]; *)
  Return[retVal];
];
(* ============================================== *)
\[Eta]BOutputFunc[solIdx_?NumericQ, xVal_?NumericQ, yVal_?NumericQ, interpolationTables_] := Module[{retVal},
  retVal = interpolationTables[[2]][[solIdx]][xVal, yVal];

  (* Print["OutputFunc::retVal = ", retVal]; *)
  Return[retVal];
];
(* ============================================== *)
PlotFunction[idxFunc_?NumericQ, interpolationTables_, paramsArray : {__}, pltPoints_?NumericQ, yMult_?NumericQ, rawOptions___] := Module[{nMaxRecursion, BDIMAGESIZE, BDPLTTEXTOPTS, plotOpts, plotOpts3, xName, yName, zName, xStart, xEnd, yStart, yEnd, xRange, yRange, XYrange, opts, UseCoeffTransformVal, plotOpts2DAll, idxFunc\[Eta], idxFunc\[Eta]B, nMaxRecursion2D, plotOpts3DAll, swapXYval, useBWLightingInPlot3Dval, lightingVal, rotateXYval, viewPointOpt},

  Print["TODO Move idxFunc\[Eta] and idxFunc\[Eta]B into constants."];
  idxFunc\[Eta] = 1;
  idxFunc\[Eta]B = 2;

  Print[strSeparatorCRLF, strSeparatorCRLF];

  opts = ProcessOptions[rawOptions];
  UseCoeffTransformVal = UseCoeffTransform /. opts /. Options[CLMS];
  swapXYval = SwapXY /. opts /. Options[CLMS];
  rotateXYval = RotateXY /. opts /. Options[CLMS];
  useBWLightingInPlot3Dval = UseBWLightingInPlot3D /. opts /. Options[CLMS];

  lightingVal = Automatic;
  If[useBWLightingInPlot3Dval == True, lightingVal = "Neutral"];

  If[BooleanQ[swapXYval] == False, swapXYval = False];
  If[BooleanQ[rotateXYval] == False, rotateXYval = False];

  viewPointOpt = {};
  If[rotateXYval == True, viewPointOpt = {ViewPoint -> {-2.4, -1.3, 2}}];

  XYrange = GetXYrange[paramsArray, UseCoeffTransformVal];
  xRange = XYrange[[1]];
  yRange = XYrange[[2]];

  xStart = xRange[[1]];
  xEnd = xRange[[2]];
  xName = xRange[[3]];

  yStart = yRange[[1]];
  yEnd = yRange[[2]];
  yName = yMult * yRange[[3]];

  nMaxRecursion = 0;
  nMaxRecursion2D = 0;
  BDIMAGESIZE = 500;
  BDPLTTEXTOPTS = {FontFamily -> "Courier", FontSize -> 14, FontWeight -> "Bold"};

  (* zName=NameFuncArray[[idxFunc]]; *)
  If[idxFunc == idxFunc\[Eta], zName = \[Eta]];
  If[idxFunc == idxFunc\[Eta]B, zName = Subscript[\[Eta], B]];

  plotOpts := Join[{PlotRange -> {-1, 1}, PlotPoints -> pltPoints, MaxRecursion -> nMaxRecursion, ImageSize -> BDIMAGESIZE , LabelStyle -> BDPLTTEXTOPTS, AxesLabel -> {xName, yName, ""}, PlotLabel -> zName, Lighting -> lightingVal}, viewPointOpt];

  plotOpts3DAll := Join[{PlotRange -> All, PlotPoints -> pltPoints, MaxRecursion -> nMaxRecursion, ImageSize -> BDIMAGESIZE , LabelStyle -> BDPLTTEXTOPTS, AxesLabel -> {xName, yName, ""}, PlotLabel -> zName, Lighting -> lightingVal}, viewPointOpt];

  plotOpts3 := Join[{PlotRange -> All, PlotPoints -> pltPoints, MaxRecursion -> nMaxRecursion, ImageSize -> BDIMAGESIZE , LabelStyle -> BDPLTTEXTOPTS, AxesLabel -> {xName, yName, ""}, PlotLabel -> zName, PlotStyle -> {Directive[Opacity[1]], Directive[Yellow, Specularity[White, 20], Opacity[0.5]], Directive[Opacity[1]]}, Lighting -> lightingVal}, viewPointOpt];

  plotOpts2DAll := {PlotRange -> All, PlotPoints -> pltPoints, MaxRecursion -> nMaxRecursion2D, ImageSize -> BDIMAGESIZE , LabelStyle -> BDPLTTEXTOPTS, AxesLabel -> {xName} , Frame -> True, GridLines -> Automatic, PlotStyle -> {{Thick, Black}, {Thick, Dashed, Black}, {Thick, Dotted, Black}}, PlotLegends -> Placed[zName, Below], Axes -> False , Epilog -> Text[xName, Scaled[{0.90, 0.1}], BaseStyle -> BDPLTTEXTOPTS] };

  If[swapXYval == False,
    (
      zName = \[Eta];
      Print[zName];
      Print[Plot3D[{OutputFunc[idxFunc\[Eta], 1, xVar, yVar / yMult, interpolationTables]}, {xVar, xStart, xEnd}, {yVar, yStart * yMult, yEnd * yMult}, Evaluate[plotOpts3DAll]]];

      zName = Subscript[\[Eta], B];
      Print[zName];
      Print[Plot3D[{OutputFunc[idxFunc\[Eta]B, 1, xVar, yVar / yMult, interpolationTables]}, {xVar, xStart, xEnd}, {yVar, yStart * yMult, yEnd * yMult}, Evaluate[plotOpts3DAll]]];
    ),
    (
      Print["Swapping X and Y."];
      zName = \[Eta];
      Print[zName];
      Print[Plot3D[{OutputFunc[idxFunc\[Eta], 1, xVar, yVar / yMult, interpolationTables]}, {yVar, yStart * yMult, yEnd * yMult}, {xVar, xStart, xEnd}, Evaluate[plotOpts3DAll]]];

      zName = Subscript[\[Eta], B];
      Print[zName];
      Print[Plot3D[{OutputFunc[idxFunc\[Eta]B, 1, xVar, yVar / yMult, interpolationTables]}, {yVar, yStart * yMult, yEnd * yMult}, {xVar, xStart, xEnd}, Evaluate[plotOpts3DAll]]];
    )
  ];

  (*
Print[zName, "[[1]]: "];
Print[Plot3D[{OutputFunc[idxFunc,1,xVar,yVar/yMult,interpolationTables]},{xVar,xStart,xEnd},{yVar,yStart*yMult,yEnd*yMult},Evaluate[plotOpts]]];


(*
xName=xRange[[3]];
Print[zName, "[[1, 2]]: "];
Print[Plot3D[{OutputFunc[idxFunc,1,xVar,yVar/yMult,interpolationTables],OutputFunc[idxFunc,2,xVar,yVar/yMult,interpolationTables]},{xVar,xStart,xEnd},{yVar,yStart*yMult,yEnd*yMult},Evaluate[plotOpts3]]];
*)
*)

  (* Print["Values on the edges."]; *)

  (*
xName=yMult*yRange[[3]];
Print[zName, " for ", xRange[[3]], " = ",Chop[N[xEnd]]];
Print[Plot[{OutputFunc[idxFunc,1,xEnd,yVar/yMult,interpolationTables]},{yVar,yStart*yMult,yEnd*yMult},Evaluate[plotOpts2DAll]]];

xName=xRange[[3]];
Print[zName, " for ", yRange[[3]], " = ",Chop[N[yStart]]];
Print[Plot[{OutputFunc[idxFunc,1,xVar,yStart/yMult,interpolationTables]},{xVar,xStart,xEnd},Evaluate[plotOpts2DAll]]];

Print[zName, " for ", yRange[[3]], " = ",Chop[N[yEnd]]];
Print[Plot[{OutputFunc[idxFunc,1,xVar,yEnd/yMult,interpolationTables]},{xVar,xStart,xEnd},Evaluate[plotOpts2DAll]]];
*)

  If[idxFunc == idxFunc\[Eta],
    Print["Values on the edges for two chiral polarizations together."];
    zName = {\[Eta], Subscript[\[Eta], B]};

    xName = yMult * yRange[[3]];
    Print[zName, " for ", xRange[[3]], " = ", Chop[N[xStart]]];
    Print[Plot[{OutputFunc[idxFunc\[Eta], 1, xStart, yVar / yMult, interpolationTables], OutputFunc[idxFunc\[Eta]B, 1, xStart, yVar / yMult, interpolationTables]}, {yVar, yStart * yMult, yEnd * yMult}, Evaluate[plotOpts2DAll]]];

    Print[zName, " for ", xRange[[3]], " = ", Chop[N[xEnd]]];
    Print[Plot[{OutputFunc[idxFunc\[Eta], 1, xEnd, yVar / yMult, interpolationTables], OutputFunc[idxFunc\[Eta]B, 1, xEnd, yVar / yMult, interpolationTables]}, {yVar, yStart * yMult, yEnd * yMult}, Evaluate[plotOpts2DAll]]];

    xName = xRange[[3]];
    Print[zName, " for ", yRange[[3]], " = ", Chop[N[yStart]]];
    Print[Plot[{OutputFunc[idxFunc\[Eta], 1, xVar, yStart / yMult, interpolationTables], OutputFunc[idxFunc\[Eta]B, 1, xVar, yStart / yMult, interpolationTables]}, {xVar, xStart, xEnd}, Evaluate[plotOpts2DAll]]];

    Print[zName, " for ", yRange[[3]], " = ", Chop[N[yEnd]]];
    Print[Plot[{OutputFunc[idxFunc\[Eta], 1, xVar, yEnd / yMult, interpolationTables], OutputFunc[idxFunc\[Eta]B, 1, xVar, yEnd / yMult, interpolationTables]}, {xVar, xStart, xEnd}, Evaluate[plotOpts2DAll]]];
  ];
];
(* ============================================== *)
IsZeroVal[val_] := Module[{retVal},
  retVal = If[Length[Flatten[{val}]] == 1, If[Chop[N[Flatten[{val}][[1]]]] == 0, True, False, False], False, False];
  Return[retVal];
];
(* ============================================== *)
PrepareCoeffOutputTable[coeffNameArrayVar : {__}, paramsArray : {__}, DoNotOutputZeroCoefficientsVar_?BooleanQ] := Module[{retVal, len, idx},
  len = Length[coeffNameArrayVar];
  retVal = {};

  For[idx = 1, idx <= len, idx++,
    (
      If[IsZeroVal[paramsArray[[idx]]] == False,
        (
          retVal = Join[retVal, {coeffNameArrayVar[[idx]] -> If[Length[paramsArray[[idx]]] == 1, InputForm[N[Flatten[{paramsArray[[idx]]}][[1]]]], InputForm[N[paramsArray[[idx]]]]]}]
        )
      ];
    )
  ];

  Return[retVal];
];
(* ============================================== *)
OutputParams[paramsArray : {__}, rawOptions___] := Module[{s, len, idx, opts, UseCoeffTransformVal, outputTbl, DoNotOutputZeroCoefficientsVal, paramsArrayHlp},
  len = Length[paramsArray];
  s = "";

  opts = ProcessOptions[rawOptions];
  UseCoeffTransformVal = UseCoeffTransform /. opts /. Options[CLMS];
  DoNotOutputZeroCoefficientsVal = DoNotOutputZeroCoefficients /. opts /. Options[CLMS];

  paramsArrayHlp = ApplyAllRacemization[paramsArray, paramsArray[[coeffIdxRacemization]], rawOptions];
  paramsArrayHlp = ApplyAllGamma[paramsArrayHlp, {paramsArrayHlp[[coeffIdxGamma]], paramsArrayHlp[[coeffIdxGammaPlus]], paramsArrayHlp[[coeffIdxGammaMinus]]}, rawOptions];

  If[UseCoeffTransformVal == True,
    outputTbl = PrepareCoeffOutputTable[coeffNewArrayDisplayName, paramsArrayHlp, DoNotOutputZeroCoefficientsVal]
    ,
    outputTbl = PrepareCoeffOutputTable[coeffArrayDisplayName, paramsArrayHlp, DoNotOutputZeroCoefficientsVal]
  ];

  (* Print[strSeparator]; *)
  (* Print["paramsArray =  ", paramsArray]; *)
  (* Print["Values: ", s]; *)
  Print["Values: ", outputTbl];
];
(* ============================================== *)
CalcPlotAll[paramsArray : {__}, tMaxVal_?NumericQ, rawOptions___] := Module[{pltPointsX, pltPointsY, xStart, xEnd, yStart, yEnd, xRange, yRange, XYrange, CalcGridArray, xRangeVal, yRangeVal, interpolationTbl, opts, UseCoeffTransformVal, pltPointsVal, yMultVal},

  opts = ProcessOptions[rawOptions];
  UseCoeffTransformVal = UseCoeffTransform /. opts /. Options[CLMS];
  pltPointsVal = NoOfPlotPoints /. opts /. Options[CLMS];
  yMultVal = yMultDefault /. opts /. Options[CLMS];

  pltPointsX = pltPointsVal;
  pltPointsY = pltPointsVal;

  XYrange = GetXYrange[paramsArray, UseCoeffTransformVal];
  xRange = XYrange[[1]];
  yRange = XYrange[[2]];

  xStart = xRange[[1]];
  xEnd = xRange[[2]];

  yStart = yRange[[1]];
  yEnd = yRange[[2]];

  OutputParams[paramsArray, rawOptions];

  Print["CalcPlotAll::CalcGridArray"];
  CalcGridArray = GetCalcGridArray[paramsArray, tMaxVal, pltPointsX, pltPointsY, rawOptions];
  (* Print["CalcPlotAll::CalcGridArray = ", CalcGridArray]; *)

  Print["CalcPlotAll::interpolationTbl"];
  xRangeVal = {xStart, xEnd, pltPointsX};
  yRangeVal = {yStart, yEnd, pltPointsY};
  interpolationTbl = GetTables[CalcGridArray, xRangeVal, yRangeVal];
  (* Print["CalcPlotAll::interpolationTbl = ", interpolationTbl]; *)

  Print["Plot Functions"];
  PlotFunction[1, interpolationTbl, paramsArray, pltPointsVal, yMultVal, rawOptions];
(* PlotFunction[2,interpolationTbl,paramsArray,pltPointsVal,yMultVal,rawOptions]; *)
];
(* ============================================== *)
(* ToTable converts indexer function into regular list. *)
ToTable[indexerFunc_, maxCount_?IntegerQ] := Table[{ii, indexerFunc[ii]}, {ii, 1, maxCount}];

ToTable[indexerFunc1_, indexerFunc2_, maxCount_?IntegerQ] := Table[{ii, indexerFunc1[ii], indexerFunc2[ii]}, {ii, 1, maxCount}];

ToTable[indexerFunc1_, indexerFunc2_, indexerFunc3_, maxCount_?IntegerQ] := Table[{ii, indexerFunc1[ii], indexerFunc2[ii], indexerFunc3[ii]}, {ii, 1, maxCount}];

ToTable[indexerFunc1_, indexerFunc2_, indexerFunc3_, indexerFunc4_, maxCount_?IntegerQ] := Table[{ii, indexerFunc1[ii], indexerFunc2[ii], indexerFunc3[ii], indexerFunc4[ii]}, {ii, 1, maxCount}];
(* ============================================== *)
(* PrintIndexer prints indexer function in a table form. *)
PrintIndexer[indexerFunc_, maxCount_?IntegerQ] := Print[ToString[indexerFunc], " = ", ToTable[indexerFunc, maxCount] // MatrixForm];

PrintIndexer[indexerFunc1_, indexerFunc2_, maxCount_?IntegerQ] := Print[ToString[indexerFunc1], ", ", ToString[indexerFunc2], " = ", ToTable[indexerFunc1, indexerFunc2, maxCount] // MatrixForm];

PrintIndexer[indexerFunc1_, indexerFunc2_, indexerFunc3_, maxCount_?IntegerQ] := Print[ToString[indexerFunc1], ", ", ToString[indexerFunc2], ", ", ToString[indexerFunc3], " = ", ToTable[indexerFunc1, indexerFunc2, indexerFunc3, maxCount] // MatrixForm];

PrintIndexer[indexerFunc1_, indexerFunc2_, indexerFunc3_, indexerFunc4_, maxCount_?IntegerQ] := Print[ToString[indexerFunc1], ", ", ToString[indexerFunc2], ", ", ToString[indexerFunc3], ", ", ToString[indexerFunc4], " = ", ToTable[indexerFunc1, indexerFunc2, indexerFunc3, indexerFunc4, maxCount] // MatrixForm];
(* ============================================== *)
(* PrintAllInfo print information about substances, reactions, etc...*)
PrintAllInfo[printInfo_?BooleanQ, prepareEqns_?BooleanQ, plotDistributions_?BooleanQ, rawOptions___] := Module[
  {opts, usePrintSubstanceMatrix, usePrintAllChainsTbl, usePrintGetReactionInfo, usePrintCoeffArray, usePrintEqMatrix},
  opts = ProcessOptions[rawOptions];
  usePrintSubstanceMatrix = PrintSubstanceMatrix /. opts /. Options[CLMOutput];
  usePrintAllChainsTbl = PrintAllChainsTbl /. opts /. Options[CLMOutput];
  usePrintGetReactionInfo = PrintGetReactionInfo /. opts /. Options[CLMOutput];
  usePrintCoeffArray = PrintCoeffArray /. opts /. Options[CLMOutput];
  usePrintEqMatrix = PrintEqMatrix /. opts /. Options[CLMOutput];

  If[printInfo,
    (
      If[usePrintSubstanceMatrix, PrintIndexer[SubstanceMatrix, SubstanceDisplayMatrix, SubstanceEnantiomericContentMatrix, SubstanceTypeMatrix, NoSubstCnt]];
      If[usePrintAllChainsTbl, Print["AllChainsTbl = ", AllChainsTbl // MatrixForm]];
      If[usePrintGetReactionInfo, PrintIndexer[GetReactionInfo, NoCnt]];
      If[usePrintCoeffArray, PrintIndexer[coeffArrayStringName, coeffArrayName, NoCoeffCnt]];

      If[prepareEqns,
        (
          PrintTimeUsed[];
          PrepareEquations[rawOptions];
          PrintTimeUsed[];
        )
      ];

      If[usePrintEqMatrix, PrintIndexer[EqMatrix, NoSubstCnt]];
    )
  ];

  Print["Length[SubstanceMatrix] = ", NoSubstCnt];
  Print["Length[AllChainsTbl] = ", Length[Flatten[AllChainsTbl]]];
  Print["Length[ReactionMatrix] = ", NoCnt];
  Print["Length[coeffArrayName] = ", NoCoeffCnt];
  PrintTimeUsed[];

  If[plotDistributions,
    (
      Print["Distributions."];
      PlotAllDistributions[];

      (* Print["Logs of distributions."]; *)
      (* PlotAllLogDistributions[]; *)

      PrintTimeUsed[];
    )
  ];
];
(* ============================================== *)
