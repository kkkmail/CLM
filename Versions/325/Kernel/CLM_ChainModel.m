(* ============================================== *)
(* :Summary: CLM chain model logic. *)
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
(* Various initial values for chain models. *)
ChainModelMin = 1;
ChainModelY = 1;
ChainModelL = 2;
ChainModelGoodL = 3;
ChainModelGoodAllLD = 4;
ChainModelRandMixLD = 5;
ChainModelPropMixLD = 6;
ChainModelMax = 6;
(* ============================================== *)
(* Output types for RunChainModel *)
OutputTypeNone = 0;
OutputTypeAll = 1;
OutputTypeCPandRho = 2;
(* ============================================== *)
(* Parameters to control what OutputMonitor actually outputs to screen *)
OutputMonitor$ChiralPolarization = True;
OutputMonitor$Substances = True;
OutputMonitor$Distributions = True;
(* ============================================== *)
InterpolationMonitorArray = {};
(* ============================================== *)
ChainModelInitFuncList := {InitYChainModel, InitLChainModel, InitGoodLChainModel, InitGoodAllLDChainModel, InitRandMixLDChainModel, InitPropMixLDChainModel};
(* ============================================== *)
odsmStartTime = AbsoluteTime[];
odsmNowTime = odsmStartTime;
odsmPrevTime = odsmStartTime;

OutputDynamicStepMonitorTime[dummyIndex_?IntegerQ] := Module[{retVal},
  retVal = "";

  If[BooleanQ[QuitRequested],
    (
      If[!QuitRequested,
        (
          odsmNowTime = AbsoluteTime[];
          retVal = Evaluate[DateString[] <> ", time used: " <> ToString[Round[(odsmNowTime - odsmPrevTime)]] <> ", total time used: " <> ToString[Round[(odsmNowTime - odsmStartTime)]] <> ", StepMonitorArrayCounter = " <> ToString[StepMonitorArrayCounter]];

          If[PrevStepMonitorArrayCounter != StepMonitorArrayCounter,
            (
              odsmPrevTime = odsmNowTime;
              PrevStepMonitorArrayCounter = StepMonitorArrayCounter;
            )
          ];
        )
      ];
    )
  ];

  Return[retVal];
];
(* ============================================== *)
OutputDynamicStepMonitorData[dummyIndex_?IntegerQ] := Module[{retVal},
  retVal = "";

  If[BooleanQ[QuitRequested] && IntegerQ[StepMonitorArrayCounter],
    (
      If[!QuitRequested,
        (
          retVal = Evaluate[MatrixForm[Join[DataHeader[], StepMonitorArray]]];
        )
      ];
    )
  ];

  Return[retVal];
];
(* ============================================== *)
OutputDynamicStepMonitor[dummyIndex_?IntegerQ] := Module[{pltOptsPP, retVal},
  retVal = "";

  If[BooleanQ[QuitRequested] && IntegerQ[StepMonitorArrayCounter],
    (
      If[!QuitRequested,
        (
        (* pltOptsPP:={PlotRange \[Rule] All,ImageSize \[Rule] BDIMAGESIZE,PlotStyle\[Rule] Thick, Frame \[Rule] True,GridLines \[Rule] Automatic,AspectRatio \[Rule] 1/GoldenRatio}; *)

          pltOptsPP := {PlotRange -> All, ImageSize -> BDIMAGESIZE, PlotStyle -> Thick, Frame -> True, GridLines -> Automatic, AspectRatio -> 1 / GoldenRatio, PlotLegends :> Placed[SwatchLegend[{Style[legendNames[[1]], FontFamily -> BDFONTFAMILY, FontSize -> BDFONTSIZE, FontWeight -> BDFONTWEIGHT], Style[legendNames[[2]], FontFamily -> BDFONTFAMILY, FontSize -> BDFONTSIZE, FontWeight -> BDFONTWEIGHT], Style[legendNames[[3]], FontFamily -> BDFONTFAMILY, FontSize -> BDFONTSIZE, FontWeight -> BDFONTWEIGHT], Style[legendNames[[4]], FontFamily -> BDFONTFAMILY, FontSize -> BDFONTSIZE, FontWeight -> BDFONTWEIGHT]}], Above]};

          retVal = Evaluate[OutputMonitorNu["StepMonitorArray", StepMonitorArray, StepMonitorArrayCounter, pltOptsPP]];
        )
      ];
    )
  ];

  Return[retVal];
];
(* ============================================== *)
Print["CLM_ChainModel::TODO::Fix stepMonitorFunctionVal in QuitMonitorFunction."];
QuitMonitorFunction[] := Module[{pltOptsPP},
  If[BooleanQ[QuitRequested] && IntegerQ[StepMonitorArrayCounter],
    (
    (* pltOptsPP:={PlotRange \[Rule] All,ImageSize \[Rule] BDIMAGESIZE,PlotStyle\[Rule] Thick, Frame \[Rule] True,GridLines \[Rule] Automatic,AspectRatio \[Rule] 1/GoldenRatio}; *)

      pltOptsPP := {PlotRange -> All, ImageSize -> BDIMAGESIZE, PlotStyle -> Thick, Frame -> True, GridLines -> Automatic, AspectRatio -> 1 / GoldenRatio, PlotLegends :> Placed[SwatchLegend[{Style[legendNames[[1]], FontFamily -> BDFONTFAMILY, FontSize -> BDFONTSIZE, FontWeight -> BDFONTWEIGHT], Style[legendNames[[2]], FontFamily -> BDFONTFAMILY, FontSize -> BDFONTSIZE, FontWeight -> BDFONTWEIGHT], Style[legendNames[[3]], FontFamily -> BDFONTFAMILY, FontSize -> BDFONTSIZE, FontWeight -> BDFONTWEIGHT], Style[legendNames[[4]], FontFamily -> BDFONTFAMILY, FontSize -> BDFONTSIZE, FontWeight -> BDFONTWEIGHT]}], Above]};

      If[ToString[stepMonitorFunctionVal] != ToString[None], OutputMonitor["StepMonitorArray", StepMonitorArray, StepMonitorArrayCounter, pltOptsPP]];
      PrintTimeUsed[];
      Print["SeedRandomValue = ", SeedRandomValue];
    )
  ];
];
(* ============================================== *)
(* CalculateData is used during call backs from StepMonitor and EvaluationMonitor *)
CalculateDataCounter = 0;
CalculateData[monitorIndex_, t_, varVals_?VectorQ, rawOptions___] := Module[{retVal, len, ii, rLVal, rRVal, rYVal, nuVal, rYtotVal, roLtbl, roDtbl, level, opts, roTotalNormVal, printMonitorDataVal, monitorPrintFrequencyVal, monitorPrintAllFirstVal, monitorPlotFunctionVal, nuPval, nuCval, roPLtbl, roPDtbl, roCLtbl, roCDtbl, roPLval, roPDval, roCLval, roCDval, rXVal, roLTot, roRTot, nuTot},
  opts = ProcessOptions[rawOptions];
  roTotalNormVal = RoTotalNorm /. opts /. Options[CLMS];
  printMonitorDataVal = PrintMonitorData /. opts /. Options[CLMS];
  monitorPrintFrequencyVal = MonitorPrintFrequency /. opts /. Options[CLMS];
  monitorPrintAllFirstVal = MonitorPrintAllFirst /. opts /. Options[CLMS];
  monitorPlotFunctionVal = MonitorPlotFunction /. opts /. Options[CLMS];

  len = Length[varVals];
  (* Print["monitorIndex = ", monitorIndex,", t = ",t, ", varVals = ", varVals]; *)
  rYVal = varVals[[idxY]] / roTotalNormVal;

  rLVal = TotalRoLChain[varVals] / roTotalNormVal;
  rRVal = TotalRoDChain[varVals] / roTotalNormVal;

  roLTot = rLVal;
  roRTot = rRVal;

  nuVal = nuValue[rLVal, rRVal];

  roLtbl = Table[TotalRoLChainLevel[varVals, level], {level, 1, MaxChainLength}] / roTotalNormVal;
  roDtbl = Table[TotalRoDChainLevel[varVals, level], {level, 1, MaxChainLength}] / roTotalNormVal;

  If[InitializeBasicCrystValue || InitializeChainCrystValue,
    (
      roPLval = TotalRoLPair[varVals] / roTotalNormVal;
      roPDval = TotalRoDPair[varVals] / roTotalNormVal;
      nuPval = nuValue[roPLval, roPDval];

      roPLtbl = Table[TotalRoLPairLevel[varVals, level], {level, 1, MaxChainLength}] / roTotalNormVal;
      roPDtbl = Table[TotalRoDPairLevel[varVals, level], {level, 1, MaxChainLength}] / roTotalNormVal;

      roCLval = TotalRoLCryst[varVals] / roTotalNormVal;
      roCDval = TotalRoDCryst[varVals] / roTotalNormVal;

      roLTot = rLVal + roPLval + roCLval;
      roRTot = rRVal + roPDval + roCDval;

      (*
Print["CalculateData::t = , ", t, ", rLVal = ", rLVal, ", roPLval = ", roPLval, ", roCLval = ", roCLval, ", roLTot = ", roLTot,FromCharacterCode[10],"CalculateData::t = , ", t, ", rRVal = ", rRVal, ", roPDval = ", roPDval, ", roCDval = ", roCDval, ", roRTot = ", roRTot,FromCharacterCode[10]];
*)

      nuCval = nuValue[roCLval, roCDval];

      nuTot = nuValue[roLTot, roRTot];

      roCLtbl = Table[TotalRoLCrystLevel[varVals, level], {level, 1, MaxChainLength}] / roTotalNormVal;
      roCDtbl = Table[TotalRoDCrystLevel[varVals, level], {level, 1, MaxChainLength}] / roTotalNormVal;

      If[!UseAllSubstForCrystValue,
        (
          retVal = Join[{monitorIndex, t, nuVal, nuPval, nuCval, nuTot, rYVal}, roLtbl, roDtbl, roPLtbl, roPDtbl, roCLtbl, roCDtbl];
        ),
        (
          rXVal = varVals[[idxX]];
          retVal = Join[{monitorIndex, t, nuVal, nuPval, nuCval, nuTot, rYVal, rXVal}, roLtbl, roDtbl, roPLtbl, roPDtbl, roCLtbl, roCDtbl];
        )
      ];
    ),
    (
      If[!UseAllSubstForCrystValue,
        (
          retVal = Join[{monitorIndex, t, nuVal, rYVal}, roLtbl, roDtbl];
        ),
        (
          rXVal = varVals[[idxX]];
          retVal = Join[{monitorIndex, t, nuVal, rYVal, rXVal}, roLtbl, roDtbl];
        )
      ];
    )
  ];

  If[printMonitorDataVal,
    (
      If[(Mod[CalculateDataCounter, monitorPrintFrequencyVal] == 0) || (CalculateDataCounter <= monitorPrintAllFirstVal),
        (
          Print["CalculateData:: ", Join[DataHeader[], {retVal}] // MatrixForm];

          (* MonitorPlotFunction can only print parametric plot (when implemented!!!) *)
          (*
If[(ToString[monitorPlotFunctionVal]\[NotEqual]ToString[None]) && (Mod[CalculateDataCounter,monitorPrintFrequencyVal]\[Equal]0),
(
OutputMonitorStep[monitorContent,CalculateDataCounter,pltOptsPP];
)
];
*)
          PrintTimeUsed[];
        )
      ];
    )
  ];

  CalculateDataCounter++;
  Return[retVal];
];
(* ============================================== *)
DataHeader[] := Module[{retVal, level, roLtbl, roDtbl, roPLtbl, roPDtbl, roCLtbl, roCDtbl},
  roLtbl = Table["Tot L (" <> ToString[level] <> ")", {level, 1, MaxChainLength}];
  roDtbl = Table["Tot D (" <> ToString[level] <> ")", {level, 1, MaxChainLength}];

  If[InitializeBasicCrystValue || InitializeChainCrystValue,
    (
      roPLtbl = Table["Tot pair L (" <> ToString[level] <> ")", {level, 1, MaxChainLength}];
      roPDtbl = Table["Tot pair D (" <> ToString[level] <> ")", {level, 1, MaxChainLength}];

      roCLtbl = Table["Tot cryst L (" <> ToString[level] <> ")", {level, 1, MaxChainLength}];
      roCDtbl = Table["Tot cryst D (" <> ToString[level] <> ")", {level, 1, MaxChainLength}];

      If[!UseAllSubstForCrystValue,
        (
          retVal = {Join[{"Idx", "t", "nu", "nuP", "nuC", "nuT", "rY"}, roLtbl, roDtbl, roPLtbl, roPDtbl, roCLtbl, roCDtbl]};
        ),
        (
          retVal = {Join[{"Idx", "t", "nu", "nuP", "nuC", "nuT", "rY", "rX"}, roLtbl, roDtbl, roPLtbl, roPDtbl, roCLtbl, roCDtbl]};
        )
      ];
    ),
    (
      If[!UseAllSubstForCrystValue,
        (
          retVal = {Join[{"Idx", "t", "nu", "rY"}, roLtbl, roDtbl]};
        ),
        (
          retVal = {Join[{"Idx", "t", "nu", "rY", "rX"}, roLtbl, roDtbl]};
        )
      ];
    )
  ];

  Return[retVal];
];
(* ============================================== *)
(* StorageArrayT returns time value (t). *)
StorageArrayT[arr_, val_?NumericQ] := arr[[Round[val] + 1, 2]];
(* StorageArrayNu returns nu value at time t. *)
StorageArrayNu[arr_, val_?NumericQ] := arr[[Round[val] + 1, 3]];
StorageArrayNuP[arr_, val_?NumericQ] := If[InitializeBasicCrystValue || InitializeChainCrystValue, arr[[Round[val] + 1, 4]], Indeterminate];
StorageArrayNuC[arr_, val_?NumericQ] := If[InitializeBasicCrystValue || InitializeChainCrystValue, arr[[Round[val] + 1, 5]], Indeterminate];
StorageArrayNuT[arr_, val_?NumericQ] := If[InitializeBasicCrystValue || InitializeChainCrystValue, arr[[Round[val] + 1, 6]], Indeterminate];

(* StorageArrayY returns Y substance value at time t. *)
StorageArrayY[arr_, val_?NumericQ] := If[InitializeBasicCrystValue || InitializeChainCrystValue, arr[[Round[val] + 1, 7]], arr[[Round[val] + 1, 4]]];

(* StorageArrayX returns X substance value at time t (if applicable). *)
StorageArrayX[arr_, val_?NumericQ] := If[UseAllSubstForCrystValue, If[InitializeBasicCrystValue || InitializeChainCrystValue, arr[[Round[val] + 1, 8]], arr[[Round[val] + 1, 5]]], Indeterminate];

(* UasfC is an extra shift if UseAllSubstForCrystValue \[Equal] 1 *)
UasfC[] := If[UseAllSubstForCrystValue, 1, 0, 0];

(* StorageArrayL returns total concentration of all L molecules on <level> at time t. *)
StorageArrayL[arr_, val_?NumericQ, level_?IntegerQ] := If[InitializeBasicCrystValue || InitializeChainCrystValue, arr[[Round[val] + 1, 7 + level + UasfC[]]], arr[[Round[val] + 1, 4 + level + UasfC[]]]];
(* StorageArrayL returns total concentration of all D molecules on <level> at time t. *)
StorageArrayD[arr_, val_?NumericQ, level_?IntegerQ] := If[InitializeBasicCrystValue || InitializeChainCrystValue, arr[[Round[val] + 1, 7 + level + MaxChainLength + UasfC[]]], arr[[Round[val] + 1, 4 + level + MaxChainLength + UasfC[]]]];

StorageArrayPairL[arr_, val_?NumericQ, level_?IntegerQ] := If[InitializeBasicCrystValue || InitializeChainCrystValue, arr[[Round[val] + 1, 7 + level + 2 * MaxChainLength + UasfC[]]], Indeterminate];
StorageArrayPairD[arr_, val_?NumericQ, level_?IntegerQ] := If[InitializeBasicCrystValue || InitializeChainCrystValue, arr[[Round[val] + 1, 7 + level + 3 * MaxChainLength + UasfC[]]], Indeterminate];

StorageArrayCrystL[arr_, val_?NumericQ, level_?IntegerQ] := If[InitializeBasicCrystValue || InitializeChainCrystValue, arr[[Round[val] + 1, 7 + level + 4 * MaxChainLength + UasfC[]]], Indeterminate];
StorageArrayCrystD[arr_, val_?NumericQ, level_?IntegerQ] := If[InitializeBasicCrystValue || InitializeChainCrystValue, arr[[Round[val] + 1, 7 + level + 5 * MaxChainLength + UasfC[]]], Indeterminate];
(* ============================================== *)
(* CreateSubstUsageList creates a list of 1 and 0 based on on the list of substance IDs. *)
CreateSubstUsageList[substIDLst_?VectorQ] := Module[{substUsageLst, ii},
  substUsageLst = Table[0, {NoSubstCnt}];

  Do[(
    If[substIDLst[[ii]] < 1 || substIDLst[[ii]] > NoSubstCnt,
      (
        Print["CreateSubstUsageList::Invalid content of substIDLst: ", substIDLst];
        Quit[];
      )
    ];
    substUsageLst[[substIDLst[[ii]]]] = 1;
  ), {ii, 1, Length[substIDLst]}
  ];

  Return[substUsageLst];
] /; VectorQ[substIDLst, IntegerQ];
(* ============================================== *)
(* InitGeneralChainModel creates random initial values based on the list of weights of all subtances. *)
InitGeneralChainModel[roTotInitVal_?NumericQ, substUsageLst_?VectorQ, rawOptions___] := Module[{initVal, opts, ii, rndVals, rndTotal},
  opts = ProcessOptions[rawOptions];

  If[Length[substUsageLst] != NoSubstCnt,
    (
      Print["InitGeneralChainModel::Invalid length of substUsageLst: ", Length[substUsageLst]];
      Quit[];
    )
  ];

  rndVals = Table[RandomReal[{0, 1}] * substUsageLst[[ii]], {ii, 1, NoSubstCnt}];
  rndTotal = Sum[rndVals[[ii]] * GetChainLength[ii], {ii, 1, NoSubstCnt}];

  If[rndTotal <= 0,
    (
      Print["InitGeneralChainModel::Invalid rndTotal: ", rndTotal];
      Quit[];
    )
  ];

  initVal = roTotInitVal * rndVals / rndTotal;
  OutputInitValues[initVal, rawOptions];
  Return[initVal];
] /; VectorQ[substUsageLst, NumericQ];
(* ============================================== *)
OutputInitValues[initVal_, rawOptions___] := Module[{ii, opts, printInitValuesVal, tbl},
  opts = ProcessOptions[rawOptions];
  printInitValuesVal = PrintInitValues /. opts /. Options[CLMChains];
  If[printInitValuesVal,
    (
      tbl = Table[{SubstanceDisplayMatrix[ii], initVal[[ii]]}, {ii, 1, NoSubstCnt}];
      Print["Initial values: ", tbl // MatrixForm];
    )
  ];
];
(* ============================================== *)
(* InitYChainModel creates only Y initial values. *)
InitYChainModel[roTotInitVal_?NumericQ, rawOptions___] := Module[{initVal, substUsageLst},
  Print["InitYChainModel::Creating Y only initial values."];
  substUsageLst = CreateSubstUsageList[{idxY}];
  initVal = InitGeneralChainModel[roTotInitVal, substUsageLst, rawOptions];
  Return[initVal];
];
(* ============================================== *)
(* InitLChainModel creates random initial values for all L only simple subtances (no pairs and no crystals). *)
InitLChainModel[roTotInitVal_?NumericQ, rawOptions___] := Module[{initVal, substUsageLst, ii},
  Print["InitYChainModel::Creating L only initial values."];
  substUsageLst = Table[If[GetChainNoOfD[ii] == 0 && GetChainNoOfL[ii] > 0 && ii <= NoSimpleSubstCnt, 1, 0, 0], {ii, 1, NoSubstCnt}];
  initVal = InitGeneralChainModel[roTotInitVal, substUsageLst, rawOptions];
  Return[initVal];
];
(* ============================================== *)
(* InitGoodLChainModel creates random initial values for all L only good catalyst subtances. *)
InitGoodLChainModel[roTotInitVal_?NumericQ, rawOptions___] := Module[{initVal, substUsageLst, substIDlst, ii},
  Print["InitYChainModel::Creating good L only initial values."];
  substIDlst = Join[Table[GoodLSynthCat[ii], {ii, 1, GoodLSynthCatCnt}], Table[GoodLLigCat[ii], {ii, 1, GoodLLigCatCnt}]];
  substUsageLst = CreateSubstUsageList[substIDlst];
  initVal = InitGeneralChainModel[roTotInitVal, substUsageLst, rawOptions];
  Return[initVal];
];
(* ============================================== *)
(* InitGoodAllLDChainModel creates random initial values for all L based good catalyst subtances. *)
InitGoodAllLDChainModel[roTotInitVal_?NumericQ, rawOptions___] := Module[{initVal, substUsageLst, substIDlst, ii},
  Print["InitGoodAllLDChainModel::Creating all good L catalysts initial values."];
  substIDlst = Join[Table[GoodLSynthCat[ii], {ii, 1, GoodLSynthCatCnt}], Table[GoodLLigCat[ii], {ii, 1, GoodLLigCatCnt}]];
  substUsageLst = CreateSubstUsageList[substIDlst];
  initVal = InitGeneralChainModel[roTotInitVal, substUsageLst, rawOptions];
  Return[initVal];
];
(* ============================================== *)
(* InitRandMixLDChainModel creates a random mix of simple L and D substances *)
InitRandMixLDChainModel[roTotInitVal_?NumericQ, rawOptions___] := Module[{initVal, substUsageLst, substIDlst, ii},
  Print["InitMixLDChainModel::Creating a random mix of simple L and D substances."];
  substUsageLst = Table[If[((GetChainNoOfD[ii] + GetChainNoOfL[ii]) > 0) && ii <= NoSimpleSubstCnt, 1, 0, 0], {ii, 1, NoSubstCnt}];
  (* Print["InitMixLDChainModel::substUsageLst = ", substUsageLst // MatrixForm]; *)
  initVal = InitGeneralChainModel[roTotInitVal, substUsageLst, rawOptions];
  (* Print["InitMixLDChainModel::initVal created."]; *)
  Return[initVal];
];
(* ============================================== *)
(* InitPropMixLDChainModel creates a random mix of simple L and proportional to them D substances *)
InitPropMixLDChainModel[roTotInitVal_?NumericQ, rawOptions___] := Module[{initVal, substUsageLst, substIDlst, ii, mult, opts, testTbl},
  Print["InitPropMixLDChainModel::Creating a random mix of simple L and proportional to them D substances."];
  opts = ProcessOptions[rawOptions];
  mult = InitDMultiplier /. opts /. Options[CLMChains];

  (* substUsageLst = Table[If[GetChainNoOfD[ii] == 0 && GetChainNoOfL[ii] > 0 && ii <= NoSimpleSubstCnt, 1, 0, 0], {ii, 1, NoSubstCnt}]; *)
  substUsageLst = Table[If[((GetChainNoOfD[ii] + GetChainNoOfL[ii]) > 0) && ii <= NoSimpleSubstCnt, 1, 0, 0], {ii, 1, NoSubstCnt}];

  (* testTbl = Table[Join[{ii,GetChainNoOfD[ii], GetChainNoOfL[ii],(ii <= NoSimpleSubstCnt)}, SubstanceEnantiomericContentMatrix[ii]],{ii, 1, NoSubstCnt}]; *)
  (* Print["InitPropMixLDChainModel::substUsageLst = ", substUsageLst // MatrixForm]; *)
  initVal = InitGeneralChainModel[roTotInitVal, substUsageLst, rawOptions];

  Do[
    (
      If[substUsageLst[[ii]] == 1 && EnantiomerSubstanceID[ii] > ii,
        (
          initVal[[EnantiomerSubstanceID[ii]]] = mult * initVal[[ii]];
        )
      ];
    ), {ii, 1, NoSubstCnt}
  ];
  Return[initVal];
];
(* ============================================== *)
(* RunChainModel - runs the chain model with the specified parameters and plots whatever it can. *)
(* The function retuns NDSolve solution in case any other things are needed. *)
RunChainModel[chainModelNo_?IntegerQ, tMaxVal_?NumericQ, roTotInitVal_?NumericQ, rawOptions___] := Module[{opts, stepMonitorFunctionVal, evaluationMonitorFunctionVal, doNotUseInterpolationFunctionVal, xNameLbl, yNameLbl, plotOptsT, pltOptsPP, coeffNewValAll, initVal, rndVals, rndTotal, initValTbl, sol, divisor, interpFunc, f, tPlot, tPlotStart, ii, initRandomVal, initYsubstVal, YSubstWeightVal, initLEnantVal, initDEnantVal, tStartMult, level, monitorTypeVal, roTotalNormVal, jj, varVals, tt, roAllFuncDescriptionVal, plotSubstancesVal, substancePlotListVal, substLst, roVecRule, len, runModelOutputTypeVal, runChainModelPlotRangeVal, pltOptsEta},

  If[chainModelNo < ChainModelMin || chainModelNo > ChainModelMax,
    (
      Print["RunChainModel::Invalid chainModelNo = ", chainModelNo];
      Quit[];
    )
  ];

  CalculateDataCounter = 0;
  opts = ProcessOptions[rawOptions];

  stepMonitorFunctionVal = StepMonitorFunction /. opts /. Options[CLMS];
  evaluationMonitorFunctionVal = EvaluationMonitorFunction /. opts /. Options[CLMS];
  doNotUseInterpolationFunctionVal = DoNotUseInterpolationFunction /. opts /. Options[CLMS];
  tStartMult = TStartMultiplier /. opts /. Options[CLMS];
  monitorTypeVal = MonitorType /. opts /. Options[CLMS];
  If[!NumericQ[monitorTypeVal], monitorTypeVal = MonitorTypeNone];
  roTotalNormVal = RoTotalNorm /. opts /. Options[CLMS];
  roAllFuncDescriptionVal = \[Rho]AllFuncDescription /. opts /. Options[CLMS];

  (* Print["RunChainModel::roTotalNormVal = ", roTotalNormVal]; *)
  (* Print["RunChainModel::rawOptions = ", rawOptions]; *)

  plotSubstancesVal = PlotSubstances /. opts /. Options[CLMChains];
  substancePlotListVal = SubstancePlotList /. opts /. Options[CLMChains];
  runModelOutputTypeVal = RunModelOutputType /. opts /. Options[CLMChains];
  runChainModelPlotRangeVal = RunChainModelPlotRange /. opts /. Options[CLMChains];

  If[roAllFuncDescriptionVal != "", Print["RunChainModel::Description: ", roAllFuncDescriptionVal]];

  xNameLbl = "t";
  yNameLbl = " ";

  plotOptsT := {PlotRange -> runChainModelPlotRangeVal, ImageSize -> BDIMAGESIZE, LabelStyle -> BDPLTTEXTOPTS, AxesLabel -> {xNameLbl} , Frame -> True, GridLines -> Automatic, PlotStyle -> Thick, PlotLegends :> Placed[SwatchLegend[{Style[legendNames[[1]], FontFamily -> BDFONTFAMILY, FontSize -> BDFONTSIZE, FontWeight -> BDFONTWEIGHT], Style[legendNames[[2]], FontFamily -> BDFONTFAMILY, FontSize -> BDFONTSIZE, FontWeight -> BDFONTWEIGHT], Style[legendNames[[3]], FontFamily -> BDFONTFAMILY, FontSize -> BDFONTSIZE, FontWeight -> BDFONTWEIGHT], Style[legendNames[[4]], FontFamily -> BDFONTFAMILY, FontSize -> BDFONTSIZE, FontWeight -> BDFONTWEIGHT],
    Style[legendNames[[5]], FontFamily -> BDFONTFAMILY, FontSize -> BDFONTSIZE, FontWeight -> BDFONTWEIGHT], Style[legendNames[[6]], FontFamily -> BDFONTFAMILY, FontSize -> BDFONTSIZE, FontWeight -> BDFONTWEIGHT]}], Above], Axes -> False , Epilog -> Text[xNameLbl, Scaled[{0.95, 0.1}], BaseStyle -> BDPLTTEXTOPTS] };

  pltOptsPP := {PlotRange -> runChainModelPlotRangeVal, ImageSize -> BDIMAGESIZE, PlotStyle -> Thick, Frame -> True, GridLines -> Automatic, AspectRatio -> 1 / GoldenRatio, PlotLegends :> Placed[SwatchLegend[{Style[legendNames[[1]], FontFamily -> BDFONTFAMILY, FontSize -> BDFONTSIZE, FontWeight -> BDFONTWEIGHT], Style[legendNames[[2]], FontFamily -> BDFONTFAMILY, FontSize -> BDFONTSIZE, FontWeight -> BDFONTWEIGHT], Style[legendNames[[3]], FontFamily -> BDFONTFAMILY, FontSize -> BDFONTSIZE, FontWeight -> BDFONTWEIGHT], Style[legendNames[[4]], FontFamily -> BDFONTFAMILY, FontSize -> BDFONTSIZE, FontWeight -> BDFONTWEIGHT],
    Style[legendNames[[5]], FontFamily -> BDFONTFAMILY, FontSize -> BDFONTSIZE, FontWeight -> BDFONTWEIGHT], Style[legendNames[[6]], FontFamily -> BDFONTFAMILY, FontSize -> BDFONTSIZE, FontWeight -> BDFONTWEIGHT]}], Above], Epilog -> Text[xNameLbl, Scaled[{0.95, 0.1}], BaseStyle -> BDPLTTEXTOPTS]};


  pltOptsEta := {PlotRange -> All, ImageSize -> BDIMAGESIZE, PlotStyle -> Thick, Frame -> True, GridLines -> Automatic, AspectRatio -> 1 / GoldenRatio, PlotLegends :> Placed[SwatchLegend[{Style[legendNames[[1]], FontFamily -> BDFONTFAMILY, FontSize -> BDFONTSIZE, FontWeight -> BDFONTWEIGHT], Style[legendNames[[2]], FontFamily -> BDFONTFAMILY, FontSize -> BDFONTSIZE, FontWeight -> BDFONTWEIGHT], Style[legendNames[[3]], FontFamily -> BDFONTFAMILY, FontSize -> BDFONTSIZE, FontWeight -> BDFONTWEIGHT], Style[legendNames[[4]], FontFamily -> BDFONTFAMILY, FontSize -> BDFONTSIZE, FontWeight -> BDFONTWEIGHT],
    Style[legendNames[[5]], FontFamily -> BDFONTFAMILY, FontSize -> BDFONTSIZE, FontWeight -> BDFONTWEIGHT], Style[legendNames[[6]], FontFamily -> BDFONTFAMILY, FontSize -> BDFONTSIZE, FontWeight -> BDFONTWEIGHT]}], Above], Epilog -> Text[xNameLbl, Scaled[{0.95, 0.1}], BaseStyle -> BDPLTTEXTOPTS]};

  PrepareEquations[opts];
  coeffNewValAll = CreateZeroCoefficientValues[];

  initVal = ChainModelInitFuncList[[chainModelNo]][roTotInitVal, rawOptions];

  (* Print["RunChainModel::Solving..."]; *)
  sol = \[Rho]AllFunc[coeffNewValAll, initVal, tMaxVal, True, rawOptions];
  PrintTimeUsed[];

  If[runModelOutputTypeVal == OutputTypeAll || runModelOutputTypeVal == OutputTypeCPandRho,
    (
      interpFunc = \[Rho]AllGetInterpolationFunctions[sol];
      f = SubstanceMatrix[1] /. interpFunc[[1]];
      tPlot = f["Domain"][[1, 2]];
      tPlotStart = Min[Max[tStartMult, 0], 1] * tPlot;

      If[runModelOutputTypeVal == OutputTypeAll,
        (
          Print["RunChainModel::Plotting all..."];
          Print["tPlot = ", tPlot];

          If[!doNotUseInterpolationFunctionVal,
            (
              Print["RunChainModel::TODO::Hardcoded calls to Plot..."];

              If[InitializePairFormationValue,
                (
                  Print["Total pairs LL, DD, LD."];
                  substLst = Table[SubstanceMatrix[ii][tt], {ii, 1, NoSubstCnt}];
                  SetLegends[{"LL", "DD", "LD"}];
                  Print[Plot[{GetTotalRoPairLL[Evaluate[(substLst /. interpFunc[[1]]) / roTotalNormVal]], GetTotalRoPairDD[Evaluate[(substLst /. interpFunc[[1]]) / roTotalNormVal]], GetTotalRoPairLD[Evaluate[(substLst /. interpFunc[[1]]) / roTotalNormVal]]}, {tt, tPlotStart, tPlot}, Evaluate[plotOptsT]]];
                )
              ];
            )
          ];
        )
      ];

      If[monitorTypeVal == MonitorTypeData,
        (
          If[ToString[stepMonitorFunctionVal] != ToString[None], OutputMonitor["StepMonitorArray", StepMonitorArray, StepMonitorArrayCounter, pltOptsPP, pltOptsEta]];
          If[ToString[evaluationMonitorFunctionVal] != ToString[None], OutputMonitor["EvaluationMonitorArray", EvaluationMonitorArray, EvaluationArrayCounter, pltOptsPP, pltOptsEta]];
        ),
        (
          If[!doNotUseInterpolationFunctionVal,
            (
              CalculateDataCounter = 0;

              InterpolationMonitorArray = ParallelTable[CalculateData[jj, (jj / NStorageValue) * tPlot, \[Rho]AllValueVectorFunc[sol, NoSubstCnt, (jj / NStorageValue) * tPlot], rawOptions], {jj, 0, NStorageValue}];

              OutputMonitor["InterpolationMonitorArray", InterpolationMonitorArray, NStorageValue, pltOptsPP, pltOptsEta]
            ),
            (
              Print["RunChainModel::No data to plot..."];
            )
          ];
        )
      ];

      If[!doNotUseInterpolationFunctionVal && plotSubstancesVal,
        (
          Print["Plotting substances."];
          roVecRule = Table[SubstanceMatrix[ii] -> SubstanceMatrix[ii][tt], {ii, 1, NoSubstCnt}];

          Do[
            (
              substLst = substancePlotListVal[[ii]] /. roVecRule;
              Print[substancePlotListVal[[ii]]];

              If[ToString[Head[substLst]] == "List",
                (
                  len = Length[substLst];
                  SetLegends[substancePlotListVal[[ii]]];
                  Print[Plot[Evaluate[Table[(substLst[[ii]] /. interpFunc[[1]]) / roTotalNormVal, {ii, 1, len}]], {tt, tPlotStart, tPlot}, Evaluate[plotOptsT]]];
                ),
                (
                  Print[Plot[Evaluate[substLst /. interpFunc[[1]]] / roTotalNormVal, {tt, tPlotStart, tPlot}, Evaluate[plotOptsT]]];
                )
              ];
            ), {ii, Length[substancePlotListVal]}
          ];
        )
      ];
    )
  (*
,
(
Print["RunChainModel::NOT plotting anything..."];
)
*)
  ];

  Return[sol];
];

(* ============================================== *)
(* OutputMonitorStep - quickly plots content of a given monitor at a given step. *)
OutputMonitorStep[monitorName_?StringQ, monitorContent_, monitorLength_?IntegerQ, pltOptsPP_] := Module[{tMaxPlot, lstL, lstD, lstLmult, lstDmult},
  If[monitorLength >= 2,
    (
      Print["Nu from ", monitorName, "."];
      SetLegends[{"\[Eta]"}];Print[ParametricPlot[{StorageArrayT[monitorContent, ii], StorageArrayNu[monitorContent, ii]}, {ii, 1, monitorLength}, Evaluate[pltOptsPP]]];
    )
    ,
    (
      Print[monitorName, " has incorrect length: ", monitorLength];
    )
  ];
];
(* ============================================== *)
OutputMonitorNu[monitorName_?StringQ, monitorContent_, monitorLength_?IntegerQ, pltOptsPP_] := Module[{tMaxPlot, lstL, lstD, lstLmult, lstDmult, retVal},
  If[InitializeBasicCrystValue || InitializeChainCrystValue,
    (
      If[!UseAllSubstForCrystValue,
        (
          retVal = If[monitorLength >= 2, ParametricPlot[{{StorageArrayT[monitorContent, ii], StorageArrayNu[monitorContent, ii]}, {StorageArrayT[monitorContent, ii], StorageArrayNuP[monitorContent, ii]}, {StorageArrayT[monitorContent, ii], StorageArrayNuC[monitorContent, ii]}, {StorageArrayT[monitorContent, ii], StorageArrayNuT[monitorContent, ii]}}, {ii, 1, monitorLength}, Evaluate[pltOptsPP]], ""];

        ),
        (
          retVal = If[monitorLength >= 2,
            (
              GraphicsRow[{ParametricPlot[{{StorageArrayT[monitorContent, ii], StorageArrayNu[monitorContent, ii]}, {StorageArrayT[monitorContent, ii], StorageArrayNuP[monitorContent, ii]}, {StorageArrayT[monitorContent, ii], StorageArrayNuC[monitorContent, ii]}, {StorageArrayT[monitorContent, ii], StorageArrayNuT[monitorContent, ii]}}, {ii, 1, monitorLength}, Evaluate[pltOptsPP]], ParametricPlot[{StorageArrayT[monitorContent, ii], StorageArrayX[monitorContent, ii]}, {ii, 1, monitorLength}, Evaluate[pltOptsPP]]}]
            ),
            (
              ""
            )
          ];
        )
      ];
    ),
    (
      retVal = If[monitorLength >= 2,
        (
          ParametricPlot[{StorageArrayT[monitorContent, ii], StorageArrayNu[monitorContent, ii]}, {ii, 1, monitorLength}, Evaluate[pltOptsPP]]
        )
        ,
        (
          ""
        )
      ];
    )
  ];

  Return[retVal];
];
(* ============================================== *)
(* OutputMonitor - plots content of a given monitor. *)
OutputMonitor[monitorName_?StringQ, monitorContent_, monitorLength_?IntegerQ, pltOptsPP_] := OutputMonitor[monitorName, monitorContent, monitorLength, pltOptsPP, pltOptsPP];
OutputMonitor[monitorName_?StringQ, monitorContent_, monitorLength_?IntegerQ, pltOptsPP_, pltOptsEta_] := Module[{tMaxPlot, lstL, lstD, lstLmult, lstDmult, mult},
  Print["Y, L, D from ", monitorName, "."];
  SetLegends[{"Y", "L", "D"}];
  Print[ParametricPlot[{{StorageArrayT[monitorContent, ii], StorageArrayY[monitorContent, ii]}, {StorageArrayT[monitorContent, ii], StorageArrayL[monitorContent, ii, 1]}, {StorageArrayT[monitorContent, ii], StorageArrayD[monitorContent, ii, 1]}}, {ii, 1, monitorLength}, Evaluate[pltOptsPP]]];

  If[monitorLength >= 2,
    (
      If[OutputMonitor$ChiralPolarization,
        (
          If[InitializeBasicCrystValue || InitializeChainCrystValue,
            (
              Print["Nu, NuP, NuC, NuT from ", monitorName, "."];
              SetLegends[{"Nu", "NuP", "NuC", "NuT"}];
              Print[ParametricPlot[{{StorageArrayT[monitorContent, ii], StorageArrayNu[monitorContent, ii]}, {StorageArrayT[monitorContent, ii], StorageArrayNuP[monitorContent, ii]}, {StorageArrayT[monitorContent, ii], StorageArrayNuC[monitorContent, ii]}, {StorageArrayT[monitorContent, ii], StorageArrayNuT[monitorContent, ii]}}, {ii, 1, monitorLength}, Evaluate[pltOptsEta]]];

              If[UseAllSubstForCrystValue,
                (
                  mult = 1;
                  If[AllSubstForCrystTypeValue == AllSubstForCrystVolumeAllocation,
                    (
                      Print["(rX * alpha) (not scaled) from ", monitorName, ", alpha = ", N[AllSubstForCrystAlphaValue]];
                      mult = AllSubstForCrystAlphaValue;
                    ),
                    (
                      Print["rX (not scaled) from ", monitorName, "."];
                    )
                  ];

                  SetLegends[{"X"}];
                  Print[ParametricPlot[{StorageArrayT[monitorContent, ii], mult * StorageArrayX[monitorContent, ii]}, {ii, 1, monitorLength}, Evaluate[pltOptsPP]]];
                )
              ];
            ),
            (
              Print["Nu from ", monitorName, "."];
              SetLegends[{"Nu"}];
              Print[ParametricPlot[{StorageArrayT[monitorContent, ii], StorageArrayNu[monitorContent, ii]}, {ii, 1, monitorLength}, Evaluate[pltOptsEta]]];
            )
          ];
        )
      ];

      If[OutputMonitor$Substances,
        (
          Do[
            (
              Print["L, D, and (L+D) for level ", level, " from ", monitorName, "."];
              SetLegends[{"L", "D", "L+D"}];
              Print[ParametricPlot[{{StorageArrayT[monitorContent, ii], StorageArrayL[monitorContent, ii, level]}, {StorageArrayT[monitorContent, ii], StorageArrayD[monitorContent, ii, level]}, {StorageArrayT[monitorContent, ii], StorageArrayL[monitorContent, ii, level] + StorageArrayD[monitorContent, ii, level]}}, {ii, 1, monitorLength}, Evaluate[pltOptsPP]]];
            ), {level, 1, MaxChainLength}
          ];

          If[InitializeBasicCrystValue || InitializeChainCrystValue,
            (
              Do[
                (
                  Print["Pairs L, D, and (L+D) for level ", level, " from ", monitorName, "."];
                  SetLegends[{"L", "D", "L+D"}];
                  Print[ParametricPlot[{{StorageArrayT[monitorContent, ii], StorageArrayPairL[monitorContent, ii, level]}, {StorageArrayT[monitorContent, ii], StorageArrayPairD[monitorContent, ii, level]}, {StorageArrayT[monitorContent, ii], StorageArrayPairL[monitorContent, ii, level] + StorageArrayPairD[monitorContent, ii, level]}}, {ii, 1, monitorLength}, Evaluate[pltOptsPP]]];
                ), {level, 1, MaxChainLength}
              ];

              Do[
                (
                  Print["Solids L, D, and (L+D) for level ", level, " from ", monitorName, "."];
                  SetLegends[{"L", "D", "L+D"}];
                  Print[ParametricPlot[{{StorageArrayT[monitorContent, ii], StorageArrayCrystL[monitorContent, ii, level]}, {StorageArrayT[monitorContent, ii], StorageArrayCrystD[monitorContent, ii, level]}, {StorageArrayT[monitorContent, ii], StorageArrayCrystL[monitorContent, ii, level] + StorageArrayCrystD[monitorContent, ii, level]}}, {ii, 1, monitorLength}, Evaluate[pltOptsPP]]];
                ), {level, 1, MaxChainLength}
              ];
            )
          ];
        )
      ];
    )
    ,
    (
      Print[monitorName, " has incorrect length: ", monitorLength];
    )
  ];

  If[OutputMonitor$Distributions,
    (
    (* Print[monitorName," = ", Join[DataHeader[],monitorContent] // MatrixForm]; *)

      tMaxPlot = StorageArrayT[monitorContent, monitorLength];
      Print["Total subst in L molecules: ", Sum[StorageArrayL[monitorContent, monitorLength, level], {level, 1, MaxChainLength}]];
      Print["Total subst in D molecules: ", Sum[StorageArrayD[monitorContent, monitorLength, level], {level, 1, MaxChainLength}]];

      Print["Distribution of L and D molecules by chain level for t = ", tMaxPlot];
      SetLegends[{"L", "D"}];
      Print[DiscretePlot[{StorageArrayL[monitorContent, monitorLength, level], StorageArrayD[monitorContent, monitorLength, level]}, {level, 1, MaxChainLength}, Evaluate[discrPlotOpts2]]];

      Print["Distribution of L and D molecules (divided by the chain length) by chain level for t = ", tMaxPlot];
      SetLegends[{"L", "D"}];
      Print[DiscretePlot[{StorageArrayL[monitorContent, monitorLength, level] / level, StorageArrayD[monitorContent, monitorLength, level] / level}, {level, 1, MaxChainLength}, Evaluate[discrPlotOpts2]]];

      (*
Print["Total subst in L molecules: ",Sum[StorageArrayL[monitorContent,monitorLength,level],{level,1,MaxChainLength}]];

Print["Distribution of L molecules by chain level for t = ", tMaxPlot];
SetLegends[{"L"}];
Print[DiscretePlot[StorageArrayL[monitorContent,monitorLength,level],{level,1,MaxChainLength},Evaluate[discrPlotOpts]]];

Print["Distribution of L molecules (divided by the chain length) by chain level for t = ", tMaxPlot];
SetLegends[{"L"}];
Print[DiscretePlot[StorageArrayL[monitorContent,monitorLength,level]/level,{level,1,MaxChainLength},Evaluate[discrPlotOpts]]];

Print["Total subst in D molecules: ",Sum[StorageArrayD[monitorContent,monitorLength,level],{level,1,MaxChainLength}]];

Print["Distribution of D molecules by chain level for t = ", tMaxPlot];
SetLegends[{"D"}];
Print[DiscretePlot[StorageArrayD[monitorContent,monitorLength,level],{level,1,MaxChainLength},Evaluate[discrPlotOpts]]];

Print["Distribution of D molecules (divided by the chain length) by chain level for t = ", tMaxPlot];
SetLegends[{"D"}];
Print[DiscretePlot[StorageArrayD[monitorContent,monitorLength,level]/level,{level,1,MaxChainLength},Evaluate[discrPlotOpts]]];
*)


      lstL = Table[{level, StorageArrayL[monitorContent, monitorLength, level] / level}, {level, 1, MaxChainLength}];
      lstLmult = Table[{ii, ((lstL[[ii, 2]]) / (lstL[[ii + 1, 2]]))^-1}, {ii, 1, MaxChainLength - 1}];

      lstD = Table[{level, StorageArrayD[monitorContent, monitorLength, level] / level}, {level, 1, MaxChainLength}];
      lstDmult = Table[{ii, ((lstD[[ii, 2]]) / (lstD[[ii + 1, 2]]))^-1}, {ii, 1, MaxChainLength - 1}];

      Print["lstL = ", lstL // MatrixForm];
      Print["lstLmult = ", lstLmult // MatrixForm];

      Print["lstD = ", lstD // MatrixForm];
      Print["lstDmult = ", lstDmult // MatrixForm];
    )
  ];
];
(* ============================================== *)
If[!SilentRunValue, Print["TODO::CLM_ChainModel::Move nuValueListAminoAcid, AminoAcidCount, ... elsewhere..."]];
(* nuValueListAminoAcid returns a list of chiral polarizations of all amino acids *)
(* based on ODE solution and the value of time. *)
(* t is the time at which we are interested in finding the chiral polarization of a given amino acid. *)
(* sol is the solution of ODE from NDSolve *)
nuValueListAminoAcid[t_?NumericQ, sol_] := Module[{retVal, interpFunc, f, tPlot, substLlst, substDlst, tt, tVal},
  interpFunc = \[Rho]AllGetInterpolationFunctions[sol];
  f = SubstanceMatrix[1] /. interpFunc[[1]];
  tPlot = f["Domain"][[1, 2]];

  tVal = Min[t, tPlot];

  (*
substLlst=Table[SubstanceMatrix[GetSubstanceID[DigitArrayL[[ii]]]][tt],{ii,1,MaxEnantNo}];
substDlst=Table[SubstanceMatrix[GetSubstanceID[DigitArrayD[[ii]]]][tt],{ii,1,MaxEnantNo}];
Print["nuValueListAminoAcid::substLlst = ", substLlst// MatrixForm, ", substDlst = ", substDlst // MatrixForm];
*)

  substLlst = Table[SubstanceMatrix[GetSubstanceID[DigitArrayL[[ii]]]][tVal] /. interpFunc[[1]], {ii, 1, MaxEnantNo}];
  substDlst = Table[SubstanceMatrix[GetSubstanceID[DigitArrayD[[ii]]]][tVal] /. interpFunc[[1]], {ii, 1, MaxEnantNo}];

  (* Print["nuValueListAminoAcid::substLlst = ", substLlst// MatrixForm, ", substDlst = ", substDlst // MatrixForm]; *)

  retVal = Table[nuValue[substLlst[[ii]], substDlst[[ii]]], {ii, 1, MaxEnantNo}];
  Return[retVal];
];
(* ============================================== *)
(* AminoAcidCount returns the number of a given amino acid in a given substance. *)
AminoAcidCount[substID_?IntegerQ, aminoacidID_?IntegerQ] := Module[{substName, aaCount, aaName},
  substName = GetSubstanceName[substID];
  aaCount = StringCount[substName, DigitArray[[aminoacidID]]];
  Return[aaCount];
];
(* ============================================== *)
(* TotaAminoAcid returns total amount of a given amino acid *)
TotaAminoAcid[roVec_?VectorQ, aminoacidID_?IntegerQ] := Module[{retVal, len, ii},
  len = Length[roVec];
  If[len != NoSubstCnt,
    (
      Print["TotaAminoAcid::Invalid length of roVec: ", len];
      Return[Indeterminate];
    )
  ];

  retVal = Sum[Max[roVec[[ii]], 0] * AminoAcidCount[ii, aminoacidID], {ii, 1, NoSubstCnt}];

  (* Print["TotaAminoAcid::retVal = ", retVal]; *)

  Return[retVal];
];
(* ============================================== *)
TotalAminoAcidList[t_?NumericQ, sol_] := Module[{interpFunc, f, tPlot, tVal, retVal, roVec},
  interpFunc = \[Rho]AllGetInterpolationFunctions[sol];
  f = SubstanceMatrix[1] /. interpFunc[[1]];
  tPlot = f["Domain"][[1, 2]];
  tVal = Min[t, tPlot];
  roVec = Table[SubstanceMatrix[ii][tVal] /. interpFunc[[1]], {ii, 1, NoSubstCnt}];
  retVal = Table[TotaAminoAcid[roVec, ii], {ii, 1, 2 * MaxEnantNo}];

  (* Print["TotalAminoAcidList::retVal = ", retVal]; *)

  Return[retVal];
];
(* ============================================== *)
