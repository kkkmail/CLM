(* ::Package:: *)

(* ============================================== *)
(* :Summary: Common CLM logic. *)
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
CLMVersionMain = "3.25.001";
CLMReleaseDateMain = "2017/02/26";
CLMCopyrightStr = "Copyright: K^3, 2013 - 2017.";
CLMEmailStr = "konstantin.k.konstantinov@gmail.com"
CLMLicenseStr = "License type: GPL v3 or any later version, see http://www.gnu.org/licenses/";
CLMDisclaimerStr = "This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or any later version. This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with this program. If not, see http://www.gnu.org/licenses/ .";
(* ============================================== *)
strSeparator = "==============================================";
strSeparatorSmall = "____________________________________________";
strCRLF = FromCharacterCode[10];
strSeparatorCRLF = strSeparator <> strCRLF;
strSeparatorCRLF2 = strCRLF <> strSeparator <> strCRLF;
nnnStr = If[nnn == 1, "", ToString[nnn] <> " "];
nnn2Str = ToString[2 * nnn] <> " ";

(* ============================================== *)
If[!SilentRunValue, Print["TODO::Check that CLM_Gen_1N and CLM_Gen_2N were properly updated to support AddReaction..."]];
(* ============================================== *)

(* 
Options[CLMS],

ClmWorkingPrecision - precision of the calculation, which is used only if high precision is used,

TODO - describe and implement,
LRInitialAmount - default initial amount of L or R enantiomers,
ForceZeroLRInitialAmount \[Equal] True - forces that LRInitialAmount \[Rule] 0,
UseCoeffTransform \[Equal] True - forces use of transformed coefficients (sum and difference of enantiomers),
UseZSubstanceForRo0 \[Equal] True forces to use \[Rho]0 as a starting value of Z substance instead of A,

TODO - useHighPrecision is not implemented,

TODO - UsePrerequisites - whether or not to use prerequisite requirements for various models,

UseVariableRacemizationForAll - if True then m racemization rate is used for all racemizations (up to generation racemization multipliers: MultStoNS, MultBsstoBlr, MultBlrtoBss, MultCsstoClr, MultClrtoCss, ...). 

*)

(* ============================================== *)
(* Models for crystallization of many substances together *)
(* For AllSubstForCrystVolumeAllocation we calculate the sum: (r = Sum[ro[k]/roMax[k], {k,1,NoOfSubst}]) *)
(* Then change the concentration of each substance by rule ro[k] \[Rule] (r * roMax[k]) *)

AllSubstForCrystTypeMin = 1;
AllSubstForCrystSumOfConcentrations = 1;
AllSubstForCrystVolumeAllocation = 2;
AllSubstForCrystTypeMax = 2;

AllSubstForCrystFuncPointer = {TotalSubstSize, TotalCrystThreshold};
(* ============================================== *)
(* Types of non negative functions to use... *)
NonNegativeTypeNone = 0;
NonNegativeTypeNumeric = 1;
NonNegativeTypeAnalytic = 2;
(* ============================================== *)
(* Types of monitor to use. Default \[Equal] None (gets converted into MonitorTypeNone) *)
MonitorTypeNone = 0;
MonitorTypeTime = 1;
MonitorTypeData = 2;
(* ============================================== *)
Options[CLMS] =
    {
      ClmWorkingPrecision -> 35, UseHighPrecision -> True, LRInitialAmount -> 10^-6, ForceZeroLRInitialAmount -> False,
      UseCoeffTransform -> True, UseZSubstanceForRo0 -> False, DoNotOutputZeroCoefficients -> True, NoOfPlotPoints -> 51,
      yMultDefault -> 1, PlotFast -> False, DoNotOutputWeakModels -> True, WeakNuValue -> 10^-3, SilentWeakModels -> True,
      UsePrerequisites -> False, UseVariableRacemizationForAll -> True, OutputAllModelNames -> False, UseZeroAEquation -> False,
      MultStoNS -> 1, MultBsstoBlr -> 1, MultBlrtoBss -> 1, MultCsstoClr -> 1, MultClrtoCss -> 1, UseGamma -> False,
      UseGammaPlus -> False, UseGammaMinus -> False, Print\[Rho]AllFuncInfo -> False, Print\[Rho]AllFuncInfoTrigger -> True,
      \[Rho]AllFuncDescription -> "", NDSolveMaxSteps -> Infinity, NDSolveStepSize -> None (*10^-4*),
      NDSolveMethod -> {"FixedStep", Method -> {"ExplicitRungeKutta", "DifferenceOrder" -> 5, "StiffnessTest" -> False}},
      NDSolveAccuracyGoal -> None, NDSolvePrecisionGoal -> None, NDSolveWorkingPrecision -> None,
      NDSolveUseDerivativeForAggregateSubst -> False, NDSolveUseFuncForAggregateSubst -> True,
      NDSolveUseNumericFuncForAggregateSubst -> True, ModelDescriptorValue -> Undefined, RotateXY -> False,
      SwapXY -> False (* does not work in full yet *), UseBWLightingInPlot3D -> False, DoNotOutput3D -> False, ApplyCoeffRule -> True,
      PrintPrepareEquationsInfo -> False, DoNotUseInterpolationFunction -> False, MonitorType -> None, UseShowProgress -> True,
      StepMonitorFunction -> None, EvaluationMonitorFunction -> None, UseOneMonitor -> True, PrintMonitorData -> False,
      MonitorPrintFrequency -> 1, MonitorPrintAllFirst -> 20, NStorage -> 1000, NStorageMultiplier -> 10, RoTotalNorm -> 1,
      MonitorPlotFunction -> None, DynamicStepMonitor -> None, QuitMonitor -> None, UseAllSubstForCryst -> True,
      AllSubstForCrystType -> AllSubstForCrystVolumeAllocation, AllSubstForCrystAlpha -> (1 / 2), UseWhenEvent -> False,
      UseAnalyticNonNegatieX -> True, NonNegativeType -> NonNegativeTypeAnalytic
    };
(* ============================================== *)
NStorageValue = 0;
StepMonitorArray = {};
EvaluationMonitorArray = {};
StepMonitorArrayCounter = 0;
EvaluationArrayCounter = 0;
UseAllSubstForCrystValue = Indeterminate;
AllSubstForCrystTypeValue = Indeterminate;
(* ============================================== *)
(* AllSubstForCrystAlphaValue is a "similarity" factor or how substances affect solubility of each other *)
(* In general it si expected that 0 \[LessEqual] alpha \[LessEqual] 1 *)
(* If all substances were the same then alpha \[Equal] 1. So more similar are substance, the closer to 1 must be alpha *)
AllSubstForCrystAlphaValue = Indeterminate;
(* ============================================== *)
IdxRoWCryst = Indeterminate;
UseAnalyticNonNegatieXValue = False;
NonNegativeTypeValue = Indeterminate;
UstEEpsilonValue = 10^-3;
(* ============================================== *)
(* GetVarName returns the name of the variable (for convenience) *)
GetVarName[var_] := Module[{s},
  s = ToString[Unevaluated[var]];
  Return[s];
];
SetAttributes[GetVarName, HoldAll];
(* ============================================== *)
(* Initialize performs initialization of various parameters. *)
InitializeCommonParameters[rawOptions___] := Module[{opts},
  ResetAll[];

  opts = ProcessOptions[rawOptions];

  UseAllSubstForCrystValue = UseAllSubstForCryst /. opts /. Options[CLMS];
  AllSubstForCrystTypeValue = AllSubstForCrystType /. opts /. Options[CLMS];
  AllSubstForCrystAlphaValue = AllSubstForCrystAlpha /. opts /. Options[CLMS];
  If[!SilentRunValue, Print["Initialize::AllSubstForCrystAlphaValue = ", AllSubstForCrystAlphaValue]];

  UseAnalyticNonNegatieXValue = UseAnalyticNonNegatieX /. opts /. Options[CLMS];

  InitializeNonNegativeFunctions[rawOptions];
  InitializeCrystDiss[rawOptions];
];
(* ============================================== *)
ResetAll[] := Module[{},
  Clear[\[Tau], GetSubstanceID, GetSubstanceName, GetSubstance, SubstanceMatrix, SubstanceDisplayMatrix, SubstanceContentMatrix, SubstanceEnantiomericContentMatrix, SubstanceTypeMatrix, roMaxMatrix, SumSubstanceMatrix, SubstSizeMatrix, GetReactionInfo, GetReacSumInfo, coeffArrayName, coeffArrayStringName, coeffArrayDisplayName, coeffRuleArray, coeffNewArrayName, coeffNewArrayDisplayName, EqMatrix];

  MaxGenerationLevel = 0;
  NoGenerationCnt = 0;
  NoAtomCnt = 0;
  NoSubstCnt = 0;
  NoSumSubstCnt = 0;
  NoCnt = 0;
  NoReacSumCnt = 0;
  NoFuncCnt = 0;
  NoCoeffCnt = 0;
];
(* ============================================== *)
strSeparator = "=============================================" <> FromCharacterCode[10];
strSeparatorSmall = "---------------------------------------------";
strCRLF = FromCharacterCode[10];
(* ============================================== *)
tStart = AbsoluteTime[];
tMid = tStart;

PrintTimeUsed[showTime_?BooleanQ] := Module[{tEnd, now, diff, diffTot},
  tEnd = AbsoluteTime[];
  now = DateString[];

  If[showTime,
    (
      diff = tEnd - tMid;

      If[diff < 100,
        (
          diff = N[Round[diff, 10^-3]];
        ),
        (
          diff = Round[diff];
        )
      ];

      diffTot = Round[tEnd - tStart];
      Print[now, ", time used: ", diff, ", total time used: ", diffTot, ".", FromCharacterCode[10] <> strSeparatorSmall]
    ),
    (
      Print["Time used reset."]
    )
  ];

  tMid = tEnd;
];

PrintTimeUsed[] := PrintTimeUsed[True];
(* ============================================== *)
(* Maximum generation level. *)
MaxGenerationLevel = 0;
NoGenerationCnt = 0;
(* ============================================== *)
(* RandomBool[] returns randomly False or True hopefully with 50/50 probability *)
RandomBool[] := If[RandomInteger[] == 0, False, True, Indeterminate];
(* RandomBool[p] returns randomly False or True hopefully with p/(1-p) probability *)
RandomBool[pFailure_?NumericQ] := If[RandomReal[] <= pFailure, False, True, Indeterminate];
(* ============================================== *)
Clear[\[Tau]];
(* ============================================== *)
(* Sorts the second list using the normal ordering of the first one. *)
SortByFirst[lstOrder : {__}, lstToBeSorted : {__}] := Module[{retVal, nn, lst, lstSorted},
  nn = Length[lstOrder];

  If[Length[lstToBeSorted] != nn,
    (
      Print["SortByFirt::Lists have different length!"];
      Return[Indeterminate];
    )
  ];

  lst = Table[{lstOrder[[ii]], lstToBeSorted[[ii]]}, {ii, 1, nn}];
  lstSorted = SortBy[lst, First];
  retVal = Table[lstSorted[[ii, 2]], {ii, 1, nn}];
  Return[retVal];
];
(* ============================================== *)
SetMaxGenerationLevel[generationLevel_?IntegerQ] := Module[{},
  If[generationLevel > MaxGenerationLevel, MaxGenerationLevel = generationLevel];
];
(* ============================================== *)
(*IsGenerationDefined must be overrideded in all generations, which implement the necessary functionality (TBD) *)
IsGenerationDefined[generationLevel_?IntegerQ] := False;
(* ============================================== *)
(* This function must be called to initialize the generation (part of the model) up to MaxGenerationLevel. If return value is False, then the initialization has failed and we should not proceed further... *)
InitializeGeneration[generationLevel_?IntegerQ] := False;
(* ============================================== *)
(* Call this function to intialize all generations. *)
InitializeAllGenerations[] := Module[{ii},
  For[ii = 1, ii <= MaxGenerationLevel, ii++,
    (
      If[!SilentRunValue, Print["InitializeAllGenerations::GenerationIndex: ", ii]];
      If[IsGenerationDefined[ii] == True,
        (
          Print["InitializeAllGenerations::Generation: ", ii, " is defined."];
          If[InitializeGeneration[ii] == True,
            (
            (* Nothing to do so far ... *)
              Print["Generation:", ii, " initialized."];
            )
            ,
            (
              Print["InitializeAllGenerations::Cannot initialize generation: ", ii];
            )
            ,
            (
              Print["!!! InitializeAllGenerations::FUBAR !!!"];
              Abort[];
              Return[False];
            )
          ];
        )
      ];
    )
  ];

  CreateNewCoeffName[];
  PrepareEquations[];

  If[!SilentRunValue,
    (
      Print["Number of substances NoSubstCnt = ", NoSubstCnt];
      Print["Number of reactions NoCnt = ", NoCnt];
      Print["Number of coefficients NoCoeffCnt = ", NoCoeffCnt];
      (* Print["ReactionMatrix = ", ReactionMatrix // MatrixForm]; *)
      (* Print["coeffArrayName = ", coeffArrayName // MatrixForm,", coeffNewArrayName = ", coeffNewArrayName // MatrixForm]; *)
      Print["Initialization completed."];
    )
  ];

  Return[True];
];
(* ============================================== *)
If[$VersionNumber <= 9.,
  (
    If[!SilentRunValue, Print["Initializing BooleanQ."]];
    BooleanQ[x_] := If[Element[x, Booleans], True, False, False];
  ),
  (
    If[!SilentRunValue, Print["Version number is ", $VersionNumber, " NOT initializing BooleanQ."]];
  )
];
(* ============================================== *)
NoAtomCnt = 0;
(* ============================================== *)
AddAtom[atomName_, atomDisplayName_] := Module[{},
  NoAtomCnt++;
  AtomMatrix[NoAtomCnt] = atomName;
  AtomDisplayMatrix[NoAtomCnt] = atomDisplayName;
  Return[NoAtomCnt];
];
(* ============================================== *)
GetIdxAtomFromAtomRec[atomRec : {__}] := atomRec[[1]];
GetNoOfAtomsFromAtomRec[atomRec : {__}] := atomRec[[2]];
(* ============================================== *)
SubstanceTypeMin = 0;
SubstanceTypeUndefined = 0;
SubstanceTypeEnantiomer = 1;
SubstanceTypePair = 2;
SubstanceTypeSolid = 3;
SubstanceTypeSum = 4;
SubstanceTypeMax = 4;
(* ============================================== *)
NoSubstCnt = 0;
NoSumSubstCnt = 0;
(* ============================================== *)
AddSubstance[substName_, substDisplayName_, substContent : {__}] := AddSubstance[substName, substDisplayName, substContent, SubstanceTypeUndefined];
(* ============================================== *)
AddEnantiomerSubstance[substName_, substDisplayName_, substContent : {__}] := AddSubstance[substName, substDisplayName, substContent, SubstanceTypeEnantiomer];
(* ============================================== *)
AddPairSubstance[substName_, substDisplayName_, substContent : {__}] := AddSubstance[substName, substDisplayName, substContent, SubstanceTypePair];
(* ============================================== *)
AddSolidSubstance[substName_, substDisplayName_, substContent : {__}] := AddSubstance[substName, substDisplayName, substContent, SubstanceTypeSolid];
(* ============================================== *)
AddSubstance[substName_, substDisplayName_, substContent : {__}, substType_?IntegerQ] := Module[{chainInfo, numb, base, substNameStr, substID},
  substNameStr = StringDrop[ToString[substName], 1];

  substID = GetSubstanceID[substNameStr];

  (* Check if the substance has been already created... *)
  If[!IntegerQ[substID],
    (
      NoSubstCnt++;
      substID = NoSubstCnt;

      (* Print["AddSubstance::substName = ", substName, ", substID = ", substID, ", NoSubstCnt = ", NoSubstCnt]; *)

      GetSubstanceID[substNameStr] = substID;
      GetSubstanceID[substName] = substID;
      GetSubstanceName[substID] = substNameStr;
      GetSubstance[substID] = substName;

      SubstanceMatrix[substID] = substName;
      SubstanceDisplayMatrix[substID] = substDisplayName;
      SubstanceContentMatrix[substID] = substContent;
      chainInfo = GetChainInfo[ToString[substName]];
      SubstanceEnantiomericContentMatrix[substID] = chainInfo;
      SubstanceTypeMatrix[substID] = substType;
      roMaxMatrix[substID] = Infinity;

      If[substType == SubstanceTypeSum,
        (
          Print["CLM_Common::TODO::AddSubstance::Check (substType==SubstanceTypeSum) branch for repetitive substances."];

          NoSumSubstCnt++;
          SumSubstanceMatrix[NoSumSubstCnt] = substID;
        )
      ];
      SubstSizeMatrix[substID] = GetSubstSize[substID];
    )
  ];

  Return[substID];
];
(* ============================================== *)
(* ToSubstanceDisplay returns a list of SubstanceDisplayMatrix for a given List of IDs. *)
ToSubstanceDisplay[lst_?VectorQ] := Module[{retVal, len, ii},
  len = Length[lst];
  retVal = Table[SubstanceDisplayMatrix[lst[[ii]]], {ii, 1, len}];
  Return[retVal];
];
(* ============================================== *)
(* TotalSubstSize returns total "size" of all substances based on the concentrations and "sizes" of each substance *)
TotalSubstSize[] := Module[{retVal, ii},
  If[UseAnalyticNonNegatieXValue,
    (
      retVal = Sum[NnnX[SubstanceMatrix[ii]] * SubstSizeMatrix[ii], {ii, 1, NoSubstCnt}];
    ),
    (
      retVal = Sum[SubstanceMatrix[ii] * SubstSizeMatrix[ii], {ii, 1, NoSubstCnt}];
    )
  ];
  Return[retVal];
];

TotalSubstSize[ro_?VectorQ] := Module[{retVal, ii},
  If[UseAnalyticNonNegatieXValue,
    (
      retVal = Sum[NnnX[ro[[ii]]] * SubstSizeMatrix[ii], {ii, 1, NoSubstCnt}];
    ),
    (
      retVal = Sum[ro[[ii]] * SubstSizeMatrix[ii], {ii, 1, NoSubstCnt}];)
  ];
  Return[retVal];
];
(* ============================================== *)
(* GetSubstSize calculates substance "size" for crystallization / dissolution based on various setting and parameters. *)
(* We start from the simplest version - size = sum of L and D molecules in the substance for a pair in solution and zero for any other substance type *)
GetSubstSize[substID_?IntegerQ] := Module[{size},
  size = 0;
  (* If[(SubstanceTypeMatrix[substID] \[Equal] SubstanceTypeEnantiomer) || (SubstanceTypeMatrix[substID] \[Equal] SubstanceTypePair),size=GetChainNoOfLandD[substID]]; *)
  If[(SubstanceTypeMatrix[substID] == SubstanceTypePair), size = GetChainNoOfLandD[substID]];
  Return[size];
];
(* ============================================== *)
(* TotalCrystThreshold calculates the sum r = Sum[ro[k]/roMax[k], {k,1,NoOfSubst}] *)
TotalCrystThreshold[] := Module[{retVal, kk},
  If[UseAnalyticNonNegatieXValue,
    (
      retVal = Sum[NnnX[SubstanceMatrix[ii]] / roMaxMatrix[ii], {ii, 1, NoSubstCnt}];
    ),
    (
      retVal = Sum[SubstanceMatrix[ii] / roMaxMatrix[ii], {ii, 1, NoSubstCnt}];
    )
  ];

  Return[retVal];
];
(* ============================================== *)
GetNoOfAtomsInSubstance[idxSubst_?IntegerQ, idxAtom_?IntegerQ] := Module[{retVal, ii, len, atomRec, idxAtomContent, noOfAtoms},
  retVal = 0;
  If[idxSubst < 1 || idxSubst > NoSubstCnt, (Print["GetNoOfAtomsInSubstance::Incorrect idxSubst."]; Return[retVal])];
  If[idxAtom < 1 || idxAtom > NoAtomCnt, (Print["GetNoOfAtomsInSubstance::Incorrect idxAtom."]; Return[retVal])];

  len = Length[SubstanceContentMatrix[idxSubst]];

  For[ii = 1, ii <= len, ii++,
    (
      atomRec = SubstanceContentMatrix[idxSubst][[ii]];
      idxAtomContent = GetIdxAtomFromAtomRec[atomRec];
      noOfAtoms = GetNoOfAtomsFromAtomRec[atomRec];

      If[idxAtomContent == idxAtom, retVal = noOfAtoms];
    )
  ];

  Return[retVal];
];
(* ============================================== *)
GetTotalNoOfAtoms[idxAtom_?IntegerQ, substVal : {__}] := Module[{retVal, ii, idxSubst},
  retVal = 0;
  If[Length[substVal] != NoSubstCnt, (Print["GetTotalNoOfAtoms::Incorrect substVal."]; Return[retVal])];
  If[idxAtom < 1 || idxAtom > NoAtomCnt, (Print["GetTotalNoOfAtoms::Incorrect idxAtom."]; Return[retVal])];

  For[idxSubst = 1, idxSubst <= NoSubstCnt, idxSubst++,
    (
      retVal += substVal[[idxSubst]] * GetNoOfAtomsInSubstance[idxSubst, idxAtom];
    )
  ];

  Return[retVal];
];
(* ============================================== *)
NoCnt = 0;
(* ============================================== *)
(* Signature of ReactionInfo object is as follows: *)
(* {{reactionFunc,reactionDescription},{{source1,number1},...},{reactionRate,pow1,...},{{result1,number1},...}} *)
SetAttributes[AddReaction, HoldAll];
AddReaction[reactionInfo : {{_, ___}, {{_, _}, ___}, {_, ___}, {{_, _}, ___}}] := Module[{react, inpt, outpt, params, roMaxVal, substID},
  NoCnt++;
  GetReactionInfo[NoCnt] := reactionInfo;
  react = GetReaction[reactionInfo];

  (* We need to update roMaxMatrix for CrystallizeDissolveReaction *)
  If[ToString[react] == "CrystallizeDissolveReaction",
    (
      inpt = GetInput[reactionInfo];
      outpt = GetOutput[reactionInfo];
      params = GetParams[reactionInfo];
      roMaxVal = GetSolubleSubstRoMax[params];
      substID = GetSolubleSubstID[inpt];
      roMaxMatrix[substID] = roMaxVal;
    )
  ];

  Return[NoCnt];
];
(* ============================================== *)
(* GetSolubleSubstRoMax returns roMax for a given reaction *)
GetSolubleSubstRoMax[params_] := params[[1]];
GetSolubleSubstID[inpt_] := inpt[[1, 1]];
(* ============================================== *)
NoReacSumCnt = 0;
(* ============================================== *)
GetReacSumInfoSubstanceID[reacInfo_?VectorQ] := reacInfo[[1]];
GetReacSumInfoFunction[reacInfo_?VectorQ] := reacInfo[[2]];
(* ============================================== *)
(* AddSumReaction allows to have aggregate "fictitious" substances *)
(* To do so a substance must be declared as "sum" (substance type \[Equal] SubstanceTypeSum) *)
(* Then parameterless aggregate function must be provided via AddSumReaction *)
(* The function aggregateFunc is then bound to the substance substID *)
AddSumReaction[substID_?IntegerQ, aggregateFunc_] := Module[{},
  NoReacSumCnt++;
  GetReacSumInfo[NoReacSumCnt] = {substID, aggregateFunc};
  Return[NoReacSumCnt];
];
(* ============================================== *)
NoFuncCnt = 0;
(* ============================================== *)
NoCoeffCnt = 0;
(* ============================================== *)
AddCoeffName[coeffName_, coeffDisplayName_] := Module[{coefArrayLen, coeffArrayTmp, coeffArrayTmp1, ii},
  NoCoeffCnt++;
  coeffArrayName[NoCoeffCnt] = coeffName;
  coeffArrayStringName[NoCoeffCnt] = ToString[coeffName];
  coeffArrayDisplayName[NoCoeffCnt] = coeffDisplayName;
  Return[NoCoeffCnt];
];
(* ============================================== *)
If[!SilentRunValue, Print["TODO::CLM_Common::AddCoeffRule must be fixed for indexer logic... "]];
AddCoeffRule[NewCoeffIdx : {__}, NewCoeffName : {__}, NewCoeffDisplayName : {__}, coeffRule : {___}] := Module[{coefArrayLen, coeffArrayTmp, coeffIdx, ii},
  coefArrayLen = Length[coeffRuleArray];
  coeffIdx = coefArrayLen + 1;
  coeffArrayTmp = Table[Null, {ii, 1, coeffIdx}];

  For[ii = 1, ii <= coefArrayLen, ii++,
    coeffArrayTmp[ii] = coeffRuleArray[ii];
  ];

  coeffRuleArray = coeffArrayTmp;
  coeffRuleArray[coeffIdx] = {NewCoeffIdx, NewCoeffName, NewCoeffDisplayName, coeffRule};
];
(* ============================================== *)
GetCoeffRuleIdx[idx_] := coeffRuleArray[idx][[1]];
GetCoeffRuleName[idx_] := coeffRuleArray[idx][[2]];
GetCoeffRuleDisplayName[idx_] := coeffRuleArray[idx][[3]];
GetCoeffRule[idx_] := coeffRuleArray[idx][[4]];
(* ============================================== *)
If[!SilentRunValue, Print["TODO::CLM_Common::CreateNewCoeffName must be fixed for indexer logic... "]];
CreateNewCoeffName[] := Module[{coefArrayLen, ii, jj, coeffRuleLen, NewCoeffIdx, NewCoeffName, coeffRule, coeffIdxLen, NewCoeffDisplayName},
  coefArrayLen = Length[coeffArrayName];
  coeffRuleLen = Length[coeffRuleArray];
  coeffNewArrayName = Table[coeffArrayName[[ii]], {ii, 1, coefArrayLen}];
  coeffNewArrayDisplayName = Table[coeffArrayDisplayName[[ii]], {ii, 1, coefArrayLen}];

  For[ii = 1, ii <= coeffRuleLen, ii++,
    NewCoeffIdx = GetCoeffRuleIdx[ii];
    NewCoeffName = GetCoeffRuleName[ii];
    NewCoeffDisplayName = GetCoeffRuleDisplayName[ii];
    coeffIdxLen = Length[NewCoeffIdx];

    For[jj = 1, jj <= coeffIdxLen, jj++,
      coeffNewArrayName[[NewCoeffIdx[[jj]]]] = NewCoeffName[[jj]];
      coeffNewArrayDisplayName[[NewCoeffIdx[[jj]]]] = NewCoeffDisplayName[[jj]];
    ];
  ];
];
(* ============================================== *)
If[!SilentRunValue, Print[strSeparatorCRLF, "StepFunc"]];
sfEps = 10^0;
TanhFast[x_?NumericQ] := Tanh[x * (1 + x^2 / 4)];
StepFunc[x_, mu_, epss_] := (TanhFast[((x - mu) / epss)] + 1) / 2 ;
(* Plot[StepFunc[x,0.05,sfEps],{x,-0.1,0.1},PlotStyle\[Rule]Thick, PlotRange \[Rule] All, PlotPoints \[Rule] 100] *)
(* ============================================== *)
If[!SilentRunValue, Print[strSeparatorCRLF, "StepPeriodicFunc"]];
BasePeriod = 1;
BaseStiffness = 5;
StepPeriodicFunc[x_?NumericQ] := StepPeriodicFunc[x, BasePeriod];
StepPeriodicFunc[x_?NumericQ, period_?NumericQ] := StepPeriodicFunc[x, period, BaseStiffness];
StepPeriodicFunc[x_?NumericQ, period_?NumericQ, stiff_?NumericQ] := ((Tanh[stiff * Sin[2 * Pi * x / period]] + 1) / 2);
SetAttributes[StepPeriodicFunc, NumericFunction];

InvStepPeriodicFunc[x_?NumericQ] := StepPeriodicFunc[(x + BasePeriod / 2), BasePeriod];
InvStepPeriodicFunc[x_?NumericQ, period_?NumericQ] := StepPeriodicFunc[(x + period / 2), period, BaseStiffness];
InvStepPeriodicFunc[x_?NumericQ, period_?NumericQ, stiff_?NumericQ] := StepPeriodicFunc[(x + period / 2), period, stiff];
SetAttributes[InvStepPeriodicFunc, NumericFunction];
(* ============================================== *)
cdSplineFunc[x_] := (2 * x^2 - x^4);
cdAnalyticFuncBase[x_, eps_] := ((x + Sqrt[x^2]) / 2) * (x / (Sqrt[x^2] + eps));
cdAnalyticSign[x_, eps_] := ((x + Sqrt[x^2]) / (2 * (Sqrt[x^2] + eps)));
(* ============================================== *)
InitializeNonNegativeFunctions[rawOptions___] := Module[{opts},
  opts = ProcessOptions[rawOptions];

  NonNegativeTypeValue = NonNegativeType /. opts /. Options[CLMS];

  Clear[NnnX];
  Clear[AnnX];
  Clear[UnitStepBase];
  Clear[UstE];

  UstEEpsilonValue = 10^-3;

  If[NonNegativeTypeValue == NonNegativeTypeNumeric,
    (
      If[!SilentRunValue, Print["InitializeNonNegativeFunctions::Using numerical NnnX and UstE functions."]];
      (* ============================================== *)
      (* Numeric non negative X function *)
      (* NnnX[x_?NumericQ]:= ((x+Sqrt[x^2])/2); *)
      (* NnnX[x_]:= If[x>0,x,0]; *)
      (* NnnX[x_]:=Piecewise[{{x,x>0}},0]; *)
      NnnX[x_?NumericQ] := Piecewise[{{x, x > 0}}, 0];
      (* ============================================== *)
      (* Analytic non negative X numeric function *)
      AnnX[x_] := ((x + Sqrt[x^2]) / 2);
      (* ============================================== *)
      (* Numeric smooth UnitStep *)
      UnitStepBase[x_?NumericQ] := Piecewise[{{0, x < 0}, {(2 * x^2 - x^4), 0 <= x < 1}, {1, x >= 1}}];
      UstE[x_?NumericQ, norm_?NumericQ] := UstE[x, norm, UstEEpsilonValue];
      UstE[x_?NumericQ, norm_?NumericQ, eps_?NumericQ] := UnitStepBase[x / (norm * eps)];

    (* UstE[x_,norm_]:=If[x>0,1,0]; *)
    (* UstE[x_,norm_]:=Piecewise[{{1,x>0}},0]; *)
    (* ============================================== *)
    ),
    (
      If[NonNegativeTypeValue == NonNegativeTypeAnalytic,
        (
          If[!SilentRunValue, Print["InitializeNonNegativeFunctions::Using analytical NnnX and UstE functions."]];
          NnnX[x_] := Max[x, 0];
          AnnX[x_] := Max[x, 0];
          UnitStepBase[x_] := Piecewise[{{0, x < 0}, {(2 * x^2 - x^4), 0 <= x < 1}, {1, x >= 1}}];
          UstE[x_, norm_] := UstE[x, norm, UstEEpsilonValue];
          UstE[x_, norm_, eps_] := UnitStepBase[x / (norm * eps)];
        ),
        (
          If[NonNegativeTypeValue == NonNegativeTypeNone,
            (
              If[!SilentRunValue, Print["InitializeNonNegativeFunctions::NOT using NnnX and UstE functions."]];
              NnnX[x_] := x;
              AnnX[x_] := x;
              UstE[x_, norm_] := x;
              UstE[x_, norm_, eps_] := x;
            ),
            (
              Print["InitializeNonNegativeFunctions::Incorrect NonNegativeTypeValue = ", NonNegativeTypeValue, ". Quitting..."];
              Quit[];
            )
          ];
        )
      ];
    )
  ];
];
(* ============================================== *)
CDFuncSelectorDescr = {"(x*(1+Sign[x])/2)", "((x+Sqrt[x^2])/2)*(x/(Sqrt[x^2]+eps))", "((x+Sqrt[x^2])/2)"};
DissolveSelectorDescr = {"(kDissVal*cdAnalyticFunc[\[Rho]ValC]*cdAnalyticFunc[\[Rho]MaxVal-\[Rho]Val])", "(kDissVal*cdAnalyticFunc[\[Rho]ValC]*cdAnalyticFunc[\[Rho]MaxVal-\[Rho]Val]/(\[Rho]MaxVal+cdEps))", "(kDissVal*cdAnalyticSign[\[Rho]ValC,cdEps]*cdAnalyticFunc[\[Rho]MaxVal-\[Rho]Val])"};
(* ============================================== *)
InitializeCrystDiss[rawOptions___] := Module[{opts},
  If[!SilentRunValue, Print[strSeparatorCRLF, "Crystallize & Dissolve."]];
  opts = ProcessOptions[rawOptions];

  cdEps = 10^-3;
  CDmultVal = 10;
  (* ============================================== *)
  If[!IntegerQ[CDFuncSelector], CDFuncSelector = 3];
  CDFuncSelector = Max[Min[CDFuncSelector, 3], 1];

  (* CDFuncSelector \[Equal] 1 - uses (x*(1+Sign[x])/2) *)
  (* CDFuncSelector \[Equal] 2 - uses ((x+Sqrt[x^2])/2)*(x/(Sqrt[x^2]+eps)) *)
  (* CDFuncSelector \[Equal] 3 (and others) - uses NnnX *)

  If[!SilentRunValue, Print["CDFuncSelector = ", CDFuncSelector, ", description: ", CDFuncSelectorDescr[[CDFuncSelector]]]];

  If[CDFuncSelector == 1,
    (
      cdAnalyticFunc[x_] := (x * (1 + Sign[x]) / 2)
    ),
    (
      If[CDFuncSelector == 2,
        (
          cdAnalyticFunc[x_] := cdAnalyticFuncBase[x, cdEps]
        ),
        (
          cdAnalyticFunc[x_] := NnnX[x]
        )
      ];
    )
  ];
  If[!SilentRunValue, Print["cdAnalyticFunc[x]: ", Definition[cdAnalyticFunc]]];
  (* ============================================== *)
  crystallizeA[\[Rho]Val_, \[Rho]ValC_, \[Rho]MaxVal_, kCrystVal_] := kCrystVal * cdAnalyticFunc[\[Rho]Val - \[Rho]MaxVal];

  If[!IntegerQ[DissolveSelector], DissolveSelector = 2];
  DissolveSelector = Max[Min[DissolveSelector, 3], 1];
  (* DissolveSelector \[Equal] 1 - uses (kDissVal*cdAnalyticFunc[\[Rho]ValC]*cdAnalyticFunc[\[Rho]MaxVal-\[Rho]Val]) *)
  (* DissolveSelector \[Equal] 2 - uses (kDissVal*cdAnalyticFunc[\[Rho]ValC]*cdAnalyticFunc[\[Rho]MaxVal-\[Rho]Val]/(\[Rho]MaxVal+cdEps)) *)
  (* DissolveSelector \[Equal] 3 - uses (kDissVal*cdAnalyticSign[\[Rho]ValC,cdEps]*cdAnalyticFunc[\[Rho]MaxVal-\[Rho]Val]) *)

  If[!SilentRunValue, Print["DissolveSelector = ", DissolveSelector, ", description: ", DissolveSelectorDescr[[DissolveSelector]]]];

  If[DissolveSelector == 1,
    (
      dissolveA[\[Rho]Val_, \[Rho]ValC_, \[Rho]MaxVal_, kDissVal_] := (kDissVal * cdAnalyticFunc[\[Rho]ValC] * cdAnalyticFunc[\[Rho]MaxVal - \[Rho]Val])
    ),
    (
      If[DissolveSelector == 2,
        (
          dissolveA[\[Rho]Val_, \[Rho]ValC_, \[Rho]MaxVal_, kDissVal_] := (kDissVal * cdAnalyticFunc[\[Rho]ValC] * cdAnalyticFunc[\[Rho]MaxVal - \[Rho]Val] / (\[Rho]MaxVal + cdEps))
        ),
        (
          dissolveA[\[Rho]Val_, \[Rho]ValC_, \[Rho]MaxVal_, kDissVal_] := (kDissVal * cdAnalyticSign[\[Rho]ValC, cdEps] * cdAnalyticFunc[\[Rho]MaxVal - \[Rho]Val])
        )
      ];
    )
  ];

  If[!SilentRunValue, Print["dissolveA: ", Definition[dissolveA]]];
  (* ============================================== *)
  CrystallizeDissolveA[\[Rho]Val_, \[Rho]ValC_, \[Rho]MaxVal_, kCrystVal_, kDissVal_] := (crystallizeA[\[Rho]Val, \[Rho]ValC, \[Rho]MaxVal, kCrystVal] - dissolveA[\[Rho]Val, \[Rho]ValC, \[Rho]MaxVal, kDissVal]);
  (* ============================================== *)
  crystallize[\[Rho]Val_?NumericQ, \[Rho]ValC_?NumericQ, \[Rho]MaxVal_?NumericQ, kCrystVal_?NumericQ] := Module[{retVal},
    retVal = kCrystVal * (Sign[\[Rho]Val - \[Rho]MaxVal] + 1) * (\[Rho]Val - \[Rho]MaxVal) * If[\[Rho]Val > 0, 1, 0, 0] / 2;
    Return[retVal];
  ];
  (* ============================================== *)
  dissolve[\[Rho]Val_?NumericQ, \[Rho]ValC_?NumericQ, \[Rho]MaxVal_?NumericQ, kDissVal_?NumericQ] := Module[{retVal},
    retVal = kDissVal * (Sign[\[Rho]MaxVal - \[Rho]Val] + 1) * (\[Rho]MaxVal - \[Rho]Val) * If[\[Rho]ValC > 0, 1, 0, 0] * If[ \[Rho]MaxVal > 0, If[(\[Rho]ValC >= cdEps * \[Rho]MaxVal), 1, cdSplineFunc[(\[Rho]ValC / (cdEps * \[Rho]MaxVal))], 0], 0, 0] / 2;
    Return[retVal];
  ];
  (* ============================================== *)
  CrystallizeDissolve[\[Rho]Val_?NumericQ, \[Rho]ValC_?NumericQ, \[Rho]MaxVal_?NumericQ, kCrystVal_?NumericQ, kDissVal_?NumericQ] := Module[{retVal, divisor},
    divisor = If[\[Rho]MaxVal > 0, (1 / \[Rho]MaxVal), 1, 1];
    retVal = divisor * (crystallize[\[Rho]Val, \[Rho]ValC, \[Rho]MaxVal, kCrystVal] - dissolve[\[Rho]Val, \[Rho]ValC, \[Rho]MaxVal, kDissVal]);
    Return[retVal];
  ];
];
(* ============================================== *)
CrystallizeDissolveReaction[input : {{_, _}, {_, _}}, params : {_, _, _}] := Module[{retVal, \[Rho]Val, \[Rho]MaxVal, kCrystVal, kDissVal, inptIdx, inptIdxC, \[Rho]ValC, \[Rho]ValW, weight, \[Rho]ValT, rVal},
  inptIdx = InputReagentIndex[input[[1]]];
  inptIdxC = InputReagentIndex[input[[2]]];
  \[Rho]Val = SubstanceMatrix[inptIdx];
  \[Rho]ValC = SubstanceMatrix[inptIdxC];
  \[Rho]MaxVal = params[[1]];
  kCrystVal = params[[2]];
  kDissVal = params[[3]];

  retVal = Indeterminate;

  If[UseAllSubstForCrystValue,
    (
      \[Rho]ValW = SubstanceMatrix[IdxRoWCryst];

      If[AllSubstForCrystTypeValue == AllSubstForCrystSumOfConcentrations,
        (
          weight = SubstSizeMatrix[inptIdx];

          If[weight > 0,
            (
              \[Rho]ValT = \[Rho]ValW / weight;
              retVal = (UstE[\[Rho]Val, \[Rho]MaxVal] * crystallizeA[\[Rho]ValT, \[Rho]ValC, \[Rho]MaxVal, kCrystVal] - dissolveA[\[Rho]ValT, \[Rho]ValC, \[Rho]MaxVal, kDissVal]);
            ),
            (
              Print["CrystallizeDissolveReaction::Invalid weight = ", weight, " for substance ID = ", inptIdx, ". Quitting..."];
              Quit[];
            )
          ];
        )
        ,
        (
          If[AllSubstForCrystTypeValue == AllSubstForCrystVolumeAllocation,
            (
            (* Ok \[Rho]ValW is not really a concentration but rather a multiplier for roMax for a given substance *)
              rVal = \[Rho]Val / \[Rho]MaxVal;
              \[Rho]ValT = \[Rho]MaxVal * (rVal + AllSubstForCrystAlphaValue * (\[Rho]ValW - rVal));
              retVal = (UstE[\[Rho]Val, \[Rho]MaxVal] * crystallizeA[\[Rho]ValT, \[Rho]ValC, \[Rho]MaxVal, kCrystVal] - dissolveA[\[Rho]ValT, \[Rho]ValC, \[Rho]MaxVal, kDissVal]);
            ),
            (
              Print["CrystallizeDissolveReaction::Invalid AllSubstForCrystTypeValue = ", AllSubstForCrystTypeValue, ". Quitting..."];
              Quit[];
            )
          ];
        )
      ];
    ),
    (
      retVal = CrystallizeDissolveA[\[Rho]Val, \[Rho]ValC, \[Rho]MaxVal, kCrystVal, kDissVal];
    )
  ];


  Return[retVal];
];
(* ============================================== *)
(* Reaction function signature is: ReactionFunc[input:{{_,_},___},params:{___}]*)
CreateZeroCoefficientParams[] := Module[{retVal, ii},
  retVal = Table[{0}, {ii, 1, NoCoeffCnt}];
  Return[retVal];
];
(* ============================================== *)
CreateZeroCoefficientValues[] := Module[{retVal, ii},
  retVal = Table[0, {ii, 1, NoCoeffCnt}];
  Return[retVal];
];
(* ============================================== *)
CreateZeroInitValues[] := Module[{retVal, ii},
  retVal = Table[0, {ii, 1, NoSubstCnt}];
  Return[retVal];
];
(* ============================================== *)
GetReaction[reaction : {{_, ___}, {{_, _}, ___}, {_, ___}, {{_, _}, ___}}] := reaction[[1]][[1]];
GetReactionDescription[reaction : {{_, ___}, {{_, _}, ___}, {_, ___}, {{_, _}, ___}}] := reaction[[1]][[2]];
GetInput[reaction : {{_, ___}, {{_, _}, ___}, {_, ___}, {{_, _}, ___}}] := reaction[[2]];
GetParams[reaction : {{_, ___}, {{_, _}, ___}, {_, ___}, {{_, _}, ___}}] := reaction[[3]];
GetOutput[reaction : {{_, ___}, {{_, _}, ___}, {_, ___}, {{_, _}, ___}}] := reaction[[4]];
(* ============================================== *)
InputLength[input : {{_, _}, ___}] := Length[input];
InputReagent[input : {{_, _}, ___}, reagentNo_] := input[[reagentNo]];
InputReagentIndex[inptreag : {_, _}] := inptreag[[1]];
InputReagentQuantity[inptreag : {_, _}] := inptreag[[2]];
(* ============================================== *)
OutputLength[output : {{_, _}, ___}] := Length[output];
OutputReagent[output : {{_, _}, ___}, reagentNo_] := output[[reagentNo]];
OutputReagentIndex[outputreag : {_, _}] := outputreag[[1]];
OutputReagentQuantity[outputreag : {_, _}] := outputreag[[2]];
(* ============================================== *)
CatSynthReaction[input : {{_, _}, ___}, params : {_, ___}] := StandardReaction[input, params];
InvCatSynthReaction[input : {{_, _}, ___}, params : {_, ___}] := StandardReaction[input, params];
LigationReaction[input : {{_, _}, ___}, params : {_, ___}] := StandardReaction[input, params];
InvLigationReaction[input : {{_, _}, ___}, params : {_, ___}] := StandardReaction[input, params];
CatLigReaction[input : {{_, _}, ___}, params : {_, ___}] := StandardReaction[input, params];
InvCatLigReaction[input : {{_, _}, ___}, params : {_, ___}] := StandardReaction[input, params];
DiastFormReaction[input : {{_, _}, ___}, params : {_, ___}] := StandardReaction[input, params];
InvDiastFormReaction[input : {{_, _}, ___}, params : {_, ___}] := StandardReaction[input, params];
CrystDecayReaction[input : {{_, _}, ___}, params : {_, ___}] := StandardReaction[input, params];
EpimReaction[input : {{_, _}, ___}, params : {_, ___}] := StandardReaction[input, params];
ActivationReaction[input : {{_, _}, ___}, params : {_, ___}] := StandardReaction[input, params];
DeactivationReaction[input : {{_, _}, ___}, params : {_, ___}] := StandardReaction[input, params];
DirectCrystReaction[input : {{_, _}, ___}, params : {_, ___}] := StandardReaction[input, params];
(* ============================================== *)
(* StandardReaction params are reaction rate followed by powers of coefficients. *)
StandardReaction[input : {{_, _}, ___}, params : {_, ___}] := Module[{retVal, inptLen, inputReagentCnt, inptReag, inptIdx, inptPow, paramLen, reaction, rate},
(* Print["StandardReaction::starting"]; *)
  inptLen = InputLength[input];
  paramLen = Length[params];
  rate = params[[1]];
  reaction = rate;

  (*
Print["StandardReaction::inptLen = ", inptLen, ", paramLen = ", paramLen, ", rate = ", rate];
*)

  If[inptLen == paramLen - 1,
    (
      For[inputReagentCnt = 1, inputReagentCnt <= inptLen, inputReagentCnt++,
        inptReag = InputReagent[input, inputReagentCnt];
        (* Print["StandardReaction::inptReag = ", inptReag]; *)
        inptIdx = InputReagentIndex[inptReag];
        inptPow = params[[inputReagentCnt + 1]];

        If[UseAnalyticNonNegatieXValue,
          (
            reaction *= NnnX[SubstanceMatrix[inptIdx]]^inptPow;
          ),
          (
            reaction *= SubstanceMatrix[inptIdx]^inptPow;
          )
        ];
      ];
      retVal = reaction;
    ),
    retVal = Indeterminate,
    retVal = Indeterminate
  ];

  (* Print["StandardReaction::retVal = ", retVal]; *)
  Return[retVal];
];
(* ============================================== *)
PrepareEquations[rawOptions___] := Module[{reactionCnt, react, inpt, outpt, params, reaction, inptLen, inputReagentCnt, inptReag, inptIdx, inptQuant, outptLen, outputReagentCnt, outptReag, outptIdx, outptQuant, reactionRec, opts, printPrepareEquationsInfoVal, ii, jj, EqMatrixPlusCounter, EqMatrixMinusCounter, EqMatrixPlus, EqMatrixMinus, substID, func},
  If[!SilentRunValue, Print[strSeparatorCRLF, strSeparatorCRLF, "Preparing Equations..."]];

  opts = ProcessOptions[rawOptions];
  printPrepareEquationsInfoVal = PrintPrepareEquationsInfo /. opts /. Options[CLMS];

  Do[
    (
      EqMatrix[ii] = 0;
      EqMatrixPlusCounter[ii] = 0;
      EqMatrixMinusCounter[ii] = 0;
      If[printPrepareEquationsInfoVal, Print["EqMatrixPlusCounter[", ii, "] = ", EqMatrixPlusCounter[ii]]];
    ), {ii, 1, NoSubstCnt}
  ];

  For[reactionCnt = 1, reactionCnt <= NoCnt, reactionCnt++,
    reactionRec = GetReactionInfo[reactionCnt];
    (* Print[strSeparatorCRLF, "reactionCnt = ", reactionCnt, ", reactionRec = ", reactionRec]; *)
    react = GetReaction[reactionRec];
    inpt = GetInput[reactionRec];
    outpt = GetOutput[reactionRec];
    params = GetParams[reactionRec];
    reaction = Apply[react, {inpt, params}];

    If[printPrepareEquationsInfoVal,
      (
        Print[strSeparatorSmall];
        Print["reactionRec = ", reactionRec, ", react = ", react, ", inpt = ", inpt, ", outpt = ", outpt, ", params = ", params];
        Print["reaction = ", reaction];
      )
    ];

    inptLen = InputLength[inpt];

    For[inputReagentCnt = 1, inputReagentCnt <= inptLen, inputReagentCnt++,
      inptReag = InputReagent[inpt, inputReagentCnt];
      inptIdx = InputReagentIndex[inptReag];
      inptQuant = InputReagentQuantity[inptReag];

      If[printPrepareEquationsInfoVal, (Print["inptReag = ", inptReag, ", inptIdx = ", inptIdx, ", inptQuant = ", inptQuant];)];

      EqMatrixMinusCounter[inptIdx]++;
      EqMatrixMinus[inptIdx, EqMatrixMinusCounter[inptIdx]] = reaction * inptQuant;
    (* EqMatrix[inptIdx]=(EqMatrix[inptIdx]-reaction*inptQuant );*)
    ];

    outptLen = OutputLength[outpt];

    For[outputReagentCnt = 1, outputReagentCnt <= outptLen, outputReagentCnt++,
      outptReag = OutputReagent[outpt, outputReagentCnt];
      outptIdx = OutputReagentIndex[outptReag];
      outptQuant = OutputReagentQuantity[outptReag];

      If[printPrepareEquationsInfoVal, Print["outputReagentCnt = ", outputReagentCnt, ", outptReag = ", outptReag, ", outptIdx = ", outptIdx, ", outptQuant = ", outptQuant]];

      If[printPrepareEquationsInfoVal, Print["EqMatrixPlusCounter[", outptIdx, "] = ", EqMatrixPlusCounter[outptIdx]]];

      EqMatrixPlusCounter[outptIdx]++;

      If[printPrepareEquationsInfoVal, Print["EqMatrixPlusCounter[outptIdx] = ", EqMatrixPlusCounter[outptIdx]]];

      EqMatrixPlus[outptIdx, EqMatrixPlusCounter[outptIdx]] = reaction * outptQuant;

      If[printPrepareEquationsInfoVal, Print["EqMatrixPlus[outptIdx,EqMatrixPlusCounter[outptIdx]] = ", EqMatrixPlus[outptIdx, EqMatrixPlusCounter[outptIdx]]]];

      If[printPrepareEquationsInfoVal, Print["Processing for outputReagentCnt = ", outputReagentCnt, " is completed."]];
    ];
  ];

  Do[
    (
      If[printPrepareEquationsInfoVal, (Print["Applying Plus to EqMatrix[", ii, "]"];)];
      EqMatrix[ii] = Apply[Plus, Table[EqMatrixPlus[ii, jj], {jj, 1, EqMatrixPlusCounter[ii]}]] - Apply[Plus, Table[EqMatrixMinus[ii, jj], {jj, 1, EqMatrixMinusCounter[ii]}]];

    ), {ii, 1, NoSubstCnt}
  ];

  (* Adding aggregate functions. *)
  (* func must take no parameters. *)
  Do[
    (
      If[printPrepareEquationsInfoVal, (Print["Processing aggregate function for ii = ", ii];)];
      reactionRec = GetReacSumInfo[ii];
      substID = GetReacSumInfoSubstanceID[reactionRec];
      func = GetReacSumInfoFunction[reactionRec];
      EqMatrix[substID] += func[];
    ), {ii, 1, NoReacSumCnt}
  ];

  Clear[EqMatrixPlus, EqMatrixMinus, EqMatrixPlusCounter, EqMatrixMinusCounter];

  If[printPrepareEquationsInfoVal, (Print["EqMatrix = ", Table[EqMatrix[ii], {ii, 1, NoSubstCnt}] // MatrixForm];)];

  PrintTimeUsed[];
];
(* ============================================== *)
If[!SilentRunValue, Print["CLM_Common::TODO: ProcessOptions does not support lists in options."]];
(* ProcessOptions ensures that options are in a list. It wraps List over raw options if necessary. *)
ProcessOptions[rawOpts___] := Module[{opts, rawOptsLst},
(* Print["=============="]; *)
  rawOptsLst = Flatten[{rawOpts}];
  opts = rawOptsLst;

  (*
(*
Print["rawOpts = ",rawOpts];
Print["rawOptsLst = ",rawOptsLst];
Print["Length[rawOptsLst] = ", Length[rawOptsLst]];
*)

If[Length[rawOptsLst]\[Equal]1,
(
(*
Print["Length[rawOpts] = ", Length[rawOpts]];
Print["rawOptsLst[[1]] = ", rawOptsLst[[1]]];
Print["Head[rawOptsLst[[1]]] = ", Head[rawOptsLst[[1]]]];
Print["Head[{}] = ", Head[{}]];
*)

If[ToString[Head[rawOptsLst[[1]]]]\[Equal] ToString[Head[{}]],opts =rawOptsLst[[1]],opts =rawOptsLst];
),
opts = rawOptsLst
];

(* Print["opts = ",opts]; *)
*)
  Return[opts];
];
(* ============================================== *)
(* Returns model descriptor for a specified level *)
GetModelLevelDescriptor[generationLevel_?IntegerQ, modelDescriptor : {__}] := Module[{retVal},
  retVal = If[Length[modelDescriptor] < MaxGenerationLevel, Indeterminate, modelDescriptor[[generationLevel]], Indeterminate];

  (* Print["GetModelLevelDescriptor::retVal = ", retVal]; *)
  Return[retVal];
];
(* ============================================== *)
(* nuValue returns chiral polarization for two enantiomers. *)
nuValue[roL_?NumericQ, roD_?NumericQ] := Module[{nu, roLVal, roDval},
  roLVal = Max[roL, 0];
  roDval = Max[roD, 0];
  nu = If[(roLVal + roDval) > 0, (roLVal - roDval) / (roLVal + roDval), 0, 0];
  Return[nu];
];
(* ============================================== *)
DeleteAllOutput[] := FrontEndTokenExecute["DeleteGeneratedCells"];
(* ============================================== *)
NotNumericQ[symb_] := (!NumericQ[symb]);
(* ============================================== *)
OutputCopyright[] := Module[{},
  Print[strSeparator];
  Print["CLM version: ", CLMVersionMain];
  Print["Release date: ", CLMReleaseDateMain];
  Print[CLMCopyrightStr];
  Print["Email: ", CLMEmailStr];
  Print[CLMLicenseStr];
  Print[strSeparator];
  Print[CLMDisclaimerStr];
  Print[strSeparator];
];
(* ============================================== *)

