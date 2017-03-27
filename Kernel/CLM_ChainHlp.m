(* ============================================== *)
(* :Summary: CLM chain additional logic. *)
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
StringMax[s1_?StringQ, s2_?StringQ] := Sort[{s1, s2}][[1]];
(* ============================================== *)
(* Reverses order of the chain *)
ReverseOrder[name_?StringQ] := StringReverse[name];
(* ============================================== *)
(* Returns string code of a chain *)
GetChainName[chainInfo_?VectorQ] := chainInfo[[1]];
(* GetChainName[substID_?IntegerQ]:=GetChainName[SubstanceEnantiomericContentMatrix[substID]]; *)

If[!SilentRunValue, Print["TODO::GetChainLength is only valid for regular substances. "]];
GetChainLength[chainInfo_?VectorQ] := StringLength[GetChainName[chainInfo]];
GetChainLength[substID_?IntegerQ] := StringLength[ToDeactivated[GetSubstanceName[substID]]];

GetChainNoOfL[chainInfo_?VectorQ] := Module[{retVal},
  retVal = chainInfo[[2]];
  (* Print["GetChainNoOfL::retVal = ", retVal]; *)
  Return[retVal];
];

GetChainNoOfD[chainInfo_?VectorQ] := Module[{retVal},
  retVal = chainInfo[[3]];
  (* Print["GetChainNoOfD::retVal = ", retVal]; *)
  Return[retVal];
];
GetChainNoOfL[substID_?IntegerQ] := GetChainNoOfL[GetChainInfo[substID]];
GetChainNoOfD[substID_?IntegerQ] := GetChainNoOfD[GetChainInfo[substID]];
GetChainNoOfLandD[substID_?IntegerQ] := (GetChainNoOfL[GetChainInfo[substID]] + GetChainNoOfD[GetChainInfo[substID]]);
(* ============================================== *)
(*
EnantiomerSubstanceID[substID_?IntegerQ]:=Module[{name,enant,enantSubstID},
name=GetChainName[substID];
enant=EnantiomerChain[name];
enantSubstID=GetSubstanceID[enant];
Return[enantSubstID];
];
*)

EnantiomerSubstanceID[substID_?IntegerQ] := Module[{name, enant, enantSubstID},
  name = GetSubstanceName[substID];
  enant = EnantiomerChain[name];
  enantSubstID = GetSubstanceID[enant];
  Return[enantSubstID];
];
(* ============================================== *)
(* Returns enantiomer of a chain in a canonic order (if needed) *)
EnantiomerChain[substName_?StringQ] := Module[{retVal, base, nameRev},
  base = StringLength[substName];
  retVal = StringReplace[substName, EnantiomerRuleTbl];
  Return[retVal];
];
(* ============================================== *)
(* Merges two chains and return properly ordered final chain *)
(* Chains are always merged as Chain1 + Chain2 *)
(* useStartX \[Equal] True reverses the order of chain X before merging *)
MergeChains[substName1inp_?StringQ, useStart1_?BooleanQ, substName2inp_?StringQ, useStart2_?BooleanQ] := Module[{substName1Val, substName2Val, retVal, subst, substRev, substName1, substName2},
  substName1 = ToDeactivated[substName1inp];
  substName2 = ToDeactivated[substName2inp];

  substName1Val = If[!useStart1, substName1, ReverseOrder[substName1]];
  substName2Val = If[!useStart2, substName2, ReverseOrder[substName2]];
  subst = StringJoin[substName1Val, substName2Val];
  substRev = ReverseOrder[subst];
  retVal = StringMax[subst, substRev];
  Return[retVal];
];
(* ============================================== *)
(* Merges two chains as given and without applying canonical ordering. *)
MergeChains[substName1inp_?StringQ, substName2inp_?StringQ] := Module[{subst, substName1, substName2},
  substName1 = ToDeactivated[substName1inp];
  substName2 = ToDeactivated[substName2inp];
  subst = StringJoin[substName1, substName2];
  Return[subst];
];
(* ============================================== *)
(* We use A,B,C, .. a,b,c, ... as the "digits". So we need them created first in order not to create them all the time *)
CreateDigits[] := CreateDigits[False];

CreateDigits[useIdentical_?BooleanQ] := Module[{},
(*
DigitArrayL=Table[FromCharacterCode[ToCharacterCode["A"]+ii],{ii,0,MaxEnantNo-1}];
DigitArrayD=If[!useIdentical,Table[FromCharacterCode[ToCharacterCode["a"]+ii],{ii,0,MaxEnantNo-1}],DigitArrayL];
*)

  DigitArrayL = Table[FromCharacterCode[UpperCaseLetterCodes[[ii]]], {ii, 1, MaxEnantNo}];
  DigitArrayD = If[!useIdentical, Table[ToLowerCase[FromCharacterCode[UpperCaseLetterCodes[[ii]]]], {ii, 1, MaxEnantNo}], DigitArrayL];
  DigitArray = Join[DigitArrayL, DigitArrayD];

  If[!SilentRunValue, Print["CreateDigits::Digits = ", Table[{ii, DigitArrayL[[ii]], DigitArrayD[[ii]]}, {ii, 1, Length[DigitArrayL]}] // MatrixForm]];
];
(* ============================================== *)
CreateEnantRule[] := Module[{},
  EnantiomerRuleTbl = Join[Table[DigitArrayL[[ii]] -> DigitArrayD[[ii]], {ii, 1, MaxEnantNo}], Table[DigitArrayD[[ii]] -> DigitArrayL[[ii]], {ii, 1, MaxEnantNo}]];
];
(* ============================================== *)
(* Function, which creates the name of the chain *)
CreateChainName[numb_?IntegerQ, length_?IntegerQ] := Module[{retVal, lst, strLst, ii},
  lst = IntegerDigits[numb, 2 * MaxEnantNo, length];
  strLst = Table[DigitArray[[lst[[ii]] + 1]], {ii, 1, length}];
  retVal = StringJoin[strLst];
  Return[retVal];
];
(* ============================================== *)
(* Function to convert substance ID into chain info *)
GetChainInfo[substID_?IntegerQ] := Module[{retVal},
  retVal = SubstanceEnantiomericContentMatrix[substID];
  (* Print["GetChainInfo::substID = ", substID, ", retVal = ", retVal]; *)
  Return[retVal];
];
(* ============================================== *)
(* Function to convert substance name into chain info *)
GetChainInfo[substFullName_?StringQ] := Module[{substName, noOfL, noOfD, retVal},
  substName = StringDrop[substFullName, 1];
  noOfL = NoOfL[substName];
  noOfD = NoOfD[substName];

  retVal = {substName, noOfL, noOfD};
  Return[retVal];
];
(* ============================================== *)
(* Returns number of L in a molecule *)
NoOfL[substName_?StringQ] := Module[{retVal},
  retVal = StringCount[substName, DigitArrayL];
  Return[retVal];
];
(* ============================================== *)
NoOfD[substName_?StringQ] := Module[{retVal},
  retVal = StringCount[substName, DigitArrayD];
  Return[retVal];
];
(* ============================================== *)
(* Returns total concentration of all chains of length N *)
RoChainLevel[roVec_?VectorQ, level_?IntegerQ] := Module[{retVal, len, ii, levelLen},
  len = Length[roVec];

  If[len != NoSubstCnt,
    (
      Print["RoChainLevel::Invalid length of roVec: ", len];
      Return[Indeterminate];
    )
  ];

  levelLen = Length[AllChainsTbl[[level]]];
  retVal = Sum[roVec[[GetSubstanceID[AllChainsTbl[[level, ii]]]]], {ii, 1, levelLen}];
  Return[retVal];
];
(* ============================================== *)
(* Returns total concentration of all chains of all lengths *)
RoChainLevelTbl[roVec_?VectorQ] := Module[{retVal, len, ii, level},
  len = Length[roVec];

  If[len != NoSubstCnt,
    (
      Print["RoChainLevelTbl::Invalid length of roVec: ", len];
      Return[Indeterminate];
    )
  ];

  retVal = Table[Sum[roVec[[GetSubstanceID[AllChainsTbl[[level, ii]]]]], {ii, 1, Length[AllChainsTbl[[level]]]}], {level, 1, MaxChainLength}];
  Return[retVal];
] /; VectorQ[roVec, NumericQ];
(* ============================================== *)
(* Returns total concentration of Y equivalent molecules across all substances *)
TotalRoY[roVec_?VectorQ] := Module[{retVal, len, ii},
  len = Length[roVec];
  If[len != NoSubstCnt,
    (
      Print["TotalRoY::Invalid length of roVec: ", len];
      Return[Indeterminate];
    )
  ];

  retVal = Sum[Max[roVec[[ii]], 0] * GetChainLength[SubstanceEnantiomericContentMatrix[ii]], {ii, 2, NoSubstCnt}];
  Return[retVal];
];
(* ============================================== *)
(* Returns total concentration of L molecules across all substances *)
TotalRoL[roVec_?VectorQ] := Module[{retVal, len, ii},
  len = Length[roVec];
  If[len != NoSubstCnt,
    (
      Print["TotalRoL::Invalid length of roVec: ", len];
      Return[Indeterminate];
    )
  ];

  retVal = Sum[Max[roVec[[ii]], 0] * GetChainNoOfL[SubstanceEnantiomericContentMatrix[ii]], {ii, 1, NoSubstCnt}];
  Return[retVal];
];
(* ============================================== *)
(* Returns total concentration of D molecules across all substances *)
TotalRoD[roVec_?VectorQ] := Module[{retVal, len, ii},
  len = Length[roVec];

  If[len != NoSubstCnt,
    (
      Print["TotalRoL::Invalid length of roVec: ", len];
      Return[Indeterminate];
    )
  ];

  retVal = Sum[Max[roVec[[ii]], 0] * GetChainNoOfD[SubstanceEnantiomericContentMatrix[ii]], {ii, 1, NoSubstCnt}];
  Return[retVal];
];
(* ============================================== *)
(* Returns total concentration of L molecules across all chains *)
TotalRoLChain[roVec_?VectorQ] := Module[{retVal, len, ii},
  len = Length[roVec];
  If[len != NoSubstCnt,
    (
      Print["TotalRoL::Invalid length of roVec: ", len];
      Return[Indeterminate];
    )
  ];

  retVal = Sum[Max[roVec[[ii]], 0] * GetChainNoOfL[SubstanceEnantiomericContentMatrix[ii]] * If[SubstanceTypeMatrix[ii] == SubstanceTypeEnantiomer, 1, 0, 0], {ii, 1, NoSubstCnt}];
  Return[retVal];
];
(* ============================================== *)
(* Returns total concentration of D molecules across all chains *)
TotalRoDChain[roVec_?VectorQ] := Module[{retVal, len, ii},
  len = Length[roVec];

  If[len != NoSubstCnt,
    (
      Print["TotalRoL::Invalid length of roVec: ", len];
      Return[Indeterminate];
    )
  ];

  retVal = Sum[Max[roVec[[ii]], 0] * GetChainNoOfD[SubstanceEnantiomericContentMatrix[ii]] * If[SubstanceTypeMatrix[ii] == SubstanceTypeEnantiomer, 1, 0, 0], {ii, 1, NoSubstCnt}];
  Return[retVal];
];
(* ============================================== *)
(* Returns total concentration of L molecules across all pairs *)
TotalRoLPair[roVec_?VectorQ] := Module[{retVal, len, ii},
  len = Length[roVec];
  If[len != NoSubstCnt,
    (
      Print["TotalRoL::Invalid length of roVec: ", len];
      Return[Indeterminate];
    )
  ];

  retVal = Sum[Max[roVec[[ii]], 0] * GetChainNoOfL[SubstanceEnantiomericContentMatrix[ii]] * If[SubstanceTypeMatrix[ii] == SubstanceTypePair, 1, 0, 0], {ii, 1, NoSubstCnt}];
  Return[retVal];
];
(* ============================================== *)
(* Returns total concentration of D molecules across all pairs *)
TotalRoDPair[roVec_?VectorQ] := Module[{retVal, len, ii},
  len = Length[roVec];

  If[len != NoSubstCnt,
    (
      Print["TotalRoL::Invalid length of roVec: ", len];
      Return[Indeterminate];
    )
  ];

  retVal = Sum[Max[roVec[[ii]], 0] * GetChainNoOfD[SubstanceEnantiomericContentMatrix[ii]] * If[SubstanceTypeMatrix[ii] == SubstanceTypePair, 1, 0, 0], {ii, 1, NoSubstCnt}];
  Return[retVal];
];
(* ============================================== *)
(* Returns total concentration of L molecules across all solids *)
TotalRoLCryst[roVec_?VectorQ] := Module[{retVal, len, ii},
  len = Length[roVec];
  If[len != NoSubstCnt,
    (
      Print["TotalRoL::Invalid length of roVec: ", len];
      Return[Indeterminate];
    )
  ];

  retVal = Sum[Max[roVec[[ii]], 0] * GetChainNoOfL[SubstanceEnantiomericContentMatrix[ii]] * If[SubstanceTypeMatrix[ii] == SubstanceTypeSolid, 1, 0, 0], {ii, 1, NoSubstCnt}];
  Return[retVal];
];
(* ============================================== *)
(* Returns total concentration of D molecules across all solids *)
TotalRoDCryst[roVec_?VectorQ] := Module[{retVal, len, ii},
  len = Length[roVec];

  If[len != NoSubstCnt,
    (
      Print["TotalRoL::Invalid length of roVec: ", len];
      Return[Indeterminate];
    )
  ];

  retVal = Sum[Max[roVec[[ii]], 0] * GetChainNoOfD[SubstanceEnantiomericContentMatrix[ii]] * If[SubstanceTypeMatrix[ii] == SubstanceTypeSolid, 1, 0, 0], {ii, 1, NoSubstCnt}];
  Return[retVal];
];
(* ============================================== *)
(* Returns total concentration of L for all chains of length level *)
TotalRoLChainLevel[roVec_?VectorQ, level_?IntegerQ] := Module[{retVal, len, ii, levelLen},
  len = Length[roVec];

  If[len != NoSubstCnt,
    (
      Print["TotalRoLChainLevel::Invalid length of roVec: ", len];
      Return[Indeterminate];
    )
  ];
  levelLen = Length[AllChainsTbl[[level]]];
  retVal = Sum[Max[roVec[[GetSubstanceID[AllChainsTbl[[level, ii]]]]], 0] * GetChainNoOfL[SubstanceEnantiomericContentMatrix[GetSubstanceID[AllChainsTbl[[level, ii]]]]], {ii, 1, levelLen}];
  Return[retVal];
];
(* ============================================== *)
(* Returns total concentration of D for all chains of length level *)
TotalRoDChainLevel[roVec_?VectorQ, level_?IntegerQ] := Module[{retVal, len, ii, levelLen},
  len = Length[roVec];

  If[len != NoSubstCnt,
    (
      Print["TotalRoDChainLevel::Invalid length of roVec: ", len];
      Return[Indeterminate];
    )
  ];
  levelLen = Length[AllChainsTbl[[level]]];
  retVal = Sum[Max[roVec[[GetSubstanceID[AllChainsTbl[[level, ii]]]]], 0] * GetChainNoOfD[SubstanceEnantiomericContentMatrix[GetSubstanceID[AllChainsTbl[[level, ii]]]]], {ii, 1, levelLen}];
  Return[retVal];
];
(* ============================================== *)
(* Returns total concentration of L for all pairs of length 2*level *)
TotalRoLPairLevel[roVec_?VectorQ, level_?IntegerQ] := Module[{retVal, len, ii, levelLen},
  len = Length[roVec];

  If[len != NoSubstCnt,
    (
      Print["TotalRoLPairLevel::Invalid length of roVec: ", len];
      Return[Indeterminate];
    )
  ];
  levelLen = Length[AllChainsTbl[[level]]];
  retVal = Sum[Max[roVec[[ii]], 0] * GetChainNoOfL[SubstanceEnantiomericContentMatrix[ii]] * If[(SubstanceTypeMatrix[ii] == SubstanceTypePair) && (GetChainNoOfLandD[ii] == 2 * level), 1, 0, 0], {ii, 1, NoSubstCnt}];
  Return[retVal];
];
(* ============================================== *)
(* Returns total concentration of D for all pairs of length 2*level *)
TotalRoDPairLevel[roVec_?VectorQ, level_?IntegerQ] := Module[{retVal, len, ii, levelLen},
  len = Length[roVec];

  If[len != NoSubstCnt,
    (
      Print["TotalRoLPairLevel::Invalid length of roVec: ", len];
      Return[Indeterminate];
    )
  ];
  levelLen = Length[AllChainsTbl[[level]]];
  retVal = Sum[Max[roVec[[ii]], 0] * GetChainNoOfD[SubstanceEnantiomericContentMatrix[ii]] * If[(SubstanceTypeMatrix[ii] == SubstanceTypePair) && (GetChainNoOfLandD[ii] == 2 * level), 1, 0, 0], {ii, 1, NoSubstCnt}];
  Return[retVal];
];
(* ============================================== *)
(* Returns total concentration of L for all solids of length 2*level *)
TotalRoLCrystLevel[roVec_?VectorQ, level_?IntegerQ] := Module[{retVal, len, ii, levelLen},
  len = Length[roVec];

  If[len != NoSubstCnt,
    (
      Print["TotalRoLPairLevel::Invalid length of roVec: ", len];
      Return[Indeterminate];
    )
  ];
  levelLen = Length[AllChainsTbl[[level]]];
  retVal = Sum[Max[roVec[[ii]], 0] * GetChainNoOfL[SubstanceEnantiomericContentMatrix[ii]] * If[(SubstanceTypeMatrix[ii] == SubstanceTypeSolid) && (GetChainNoOfLandD[ii] == 2 * level), 1, 0, 0], {ii, 1, NoSubstCnt}];
  Return[retVal];
];
(* ============================================== *)
(* Returns total concentration of D for all solids of length 2*level *)
TotalRoDCrystLevel[roVec_?VectorQ, level_?IntegerQ] := Module[{retVal, len, ii, levelLen},
  len = Length[roVec];

  If[len != NoSubstCnt,
    (
      Print["TotalRoLPairLevel::Invalid length of roVec: ", len];
      Return[Indeterminate];
    )
  ];
  levelLen = Length[AllChainsTbl[[level]]];
  retVal = Sum[Max[roVec[[ii]], 0] * GetChainNoOfD[SubstanceEnantiomericContentMatrix[ii]] * If[(SubstanceTypeMatrix[ii] == SubstanceTypeSolid) && (GetChainNoOfLandD[ii] == 2 * level), 1, 0, 0], {ii, 1, NoSubstCnt}];
  Return[retVal];
];
(* ============================================== *)
\[Rho]TotalLValueFunc1[solution : {__}, tVal_?NumericQ] := Module[{retVal, interpFunc, t, roVec, ii},
  interpFunc = \[Rho]AllGetInterpolationFunctions[solution];
  roVec = Table[((SubstanceMatrix[ii][t] /. interpFunc[[1]]) /. {t -> tVal}), {ii, 1, NoSubstCnt}];
  Print["\[Rho]TotalLValueFunc1::roVec = ", Table[{SubstanceDisplayMatrix[ii], roVec[[ii]]}, {ii, 1, NoSubstCnt}] // MatrixForm];
  retVal = TotalRoL[roVec];
  Return[retVal];
];
(* ============================================== *)
\[Rho]TotalDValueFunc1[solution : {__}, tVal_?NumericQ] := Module[{retVal, interpFunc, t, roVec, ii},
  interpFunc = \[Rho]AllGetInterpolationFunctions[solution];
  roVec = Table[((SubstanceMatrix[ii][t] /. interpFunc[[1]]) /. {t -> tVal}), {ii, 1, NoSubstCnt}];
  retVal = TotalRoD[roVec];
  Print["\[Rho]TotalDValueFunc1::roVec = ", Table[{SubstanceDisplayMatrix[ii], roVec[[ii]]}, {ii, 1, NoSubstCnt}] // MatrixForm];
  Return[retVal];
];
(* ============================================== *)

(* GetGammaPlus[contentReactions_] := (GetParams[contentReactions[[1]]][[1]] - GetParams[contentReactions[[3]]][[1]]) / (GetParams[contentReactions[[1]]][[1]] + GetParams[contentReactions[[3]]][[1]]); *)
(* GetGammaMinus[contentReactions_] := (GetParams[contentReactions[[4]]][[1]] - GetParams[contentReactions[[2]]][[1]]) / (GetParams[contentReactions[[4]]][[1]] + GetParams[contentReactions[[2]]][[1]]); *)

GetGammaPlus[contentReactions_] := Module[{retVal},
  Print["GetGammaPlus::contentReactions = ", contentReactions // MatrixForm];
  retVal = (GetParams[contentReactions[[1]]][[1]] - GetParams[contentReactions[[3]]][[1]]) / (GetParams[contentReactions[[1]]][[1]] + GetParams[contentReactions[[3]]][[1]]);
  Print["GetGammaPlus::retVal = ", retVal];
  Return[retVal];
  ];

GetGammaMinus[contentReactions_] := Module[{retVal},
  Print["GetGammaMinus::contentReactions = ", contentReactions // MatrixForm];
  retVal = (GetParams[contentReactions[[4]]][[1]] - GetParams[contentReactions[[2]]][[1]]) / (GetParams[contentReactions[[4]]][[1]] + GetParams[contentReactions[[2]]][[1]]);
  Print["GetGammaMinus::retVal = ", retVal];
  Return[retVal];
];

GetGamma[contentReactions_] := (GetGammaPlus[contentReactions] + GetGammaMinus[contentReactions]) / 2;
GetOutputSubstance[contentReactions_] := GetOutput[contentReactions[[1]]][[1, 1]];
GetCatalystSubstance[contentReactions_] := GetOutput[contentReactions[[1]]][[2, 1]];
(* ============================================== *)
GetGammaPlusRL[reactionList_?VectorQ] := GetGammaPlus[Map[GetReactionInfo, reactionList]];
GetGammaMinusRL[reactionList_?VectorQ] := GetGammaMinus[Map[GetReactionInfo, reactionList]];
GetGammaRL[reactionList_?VectorQ] := GetGamma[Map[GetReactionInfo, reactionList]];
GetOutputSubstanceRL[reactionList_?VectorQ] := GetOutputSubstance[Map[GetReactionInfo, reactionList]];
GetCatalystSubstanceRL[reactionList_?VectorQ] := GetCatalystSubstance[Map[GetReactionInfo, reactionList]];
(* ============================================== *)
(* ProcessLCatInfo processes information about pure L catalysts *)
(* TODO : ProcessLCatInfo - does not work properly as of version 3.25 *)
Print["TODO : ProcessLCatInfo - does not work properly as of version 3.25 !!!"];

(* If printInfo \[Equal] True, then it also prints the informaiton. *)
GoodLSynthCatCnt = 0;
GoodLLigCatCnt = 0;

ProcessLCatInfo[printInfo_?BooleanQ, rawOptions___] := Module[{ii, len, chainLenCnt, tbl, rl, cr, subst, cat, gammaThresholdVal, gamma, synthCnt, ligCnt, opts},
  opts = ProcessOptions[rawOptions];
  gammaThresholdVal = (GammaThreshold /. opts) /. Options[CLMChains];
  GoodLSynthCatCnt = 0;
  GoodLLigCatCnt = 0;

  If[!SilentRunValue, Print["ProcessLCatInfo::GammaThreshold = ", gammaThresholdVal]];
  If[!SilentRunValue, Print["ProcessLCatInfo::Catalytic synthesis..."]];

  Do[
    (
      len = LCatSynthCatalystCnt[chainLenCnt];
      If[!SilentRunValue, Print["ProcessLCatInfo::len = ", len]];

      tbl = {};
      synthCnt = 0;

      Do[
        (
          rl = LCatSynthCatalystContent[chainLenCnt, ii];
          cr = Map[GetReactionInfo, rl];
          subst = GetOutputSubstance[cr];
          If[!SilentRunValue, Print["ProcessLCatInfo::subst = ", subst]];

          cat = GetCatalystSubstance[cr];
          If[!SilentRunValue, Print["ProcessLCatInfo::cat = ", cat]];
          gamma = GetGamma[cr];
          If[!SilentRunValue, Print["ProcessLCatInfo::gamma = ", gamma]];

          If[gamma >= gammaThresholdVal,
            (
              (* tbl = Join[tbl, {{cr // MatrixForm, GetGammaPlus[cr], GetGammaMinus[cr], GetGamma[cr], SubstanceDisplayMatrix[subst], SubstanceDisplayMatrix[cat], cat}}]; *)
              tbl = Join[tbl, {{cr, GetGammaPlus[cr], GetGammaMinus[cr], GetGamma[cr], SubstanceDisplayMatrix[subst], SubstanceDisplayMatrix[cat], cat}}];
              synthCnt++;
              GoodLSynthCatCnt++;
              GoodLSynthCat[GoodLSynthCatCnt] = cat;
            )
          ];
        ), {ii, 1, len}
      ];

      If[printInfo,
        (
          Print["chainLenCnt = ", chainLenCnt, ", synthCnt = ", synthCnt, ", LCatSynthCatalystContent = ", tbl // MatrixForm];
        )
      ];

    ), {chainLenCnt, 1, MaxChainLength}
  ];

  If[!SilentRunValue,
    (
      PrintTimeUsed[];
      Print["ProcessLCatInfo::Catalytic ligation..."];
    )
  ];

  Do[
    (
      len = LCatLigCatalystCnt[chainLenCnt];
      tbl = {};
      ligCnt = 0;

      Do[
        (
          rl = LCatLigCatalystContent[chainLenCnt, ii];
          cr = Map[GetReactionInfo, rl];
          subst = GetOutputSubstance[cr];
          cat = GetCatalystSubstance[cr];
          gamma = GetGamma[cr];
          If[gamma >= gammaThresholdVal,
            (
              tbl = Join[tbl, {{cr // MatrixForm, GetGammaPlus[cr], GetGammaMinus[cr], GetGamma[cr], SubstanceDisplayMatrix[subst], SubstanceDisplayMatrix[cat], cat}}];
              ligCnt++;
              GoodLLigCatCnt++;
              GoodLLigCat[GoodLLigCatCnt] = cat;
            )
          ];
        ), {ii, 1, len}
      ];

      If[printInfo,
        (
          Print["chainLenCnt = ", chainLenCnt, ", ligCnt = ", ligCnt, ", LCatLigCatalystContent = ", tbl // MatrixForm];
        )
      ];

    ), {chainLenCnt, 1, MaxChainLength}
  ];

  PrintTimeUsed[];
];
(* ============================================== *)
