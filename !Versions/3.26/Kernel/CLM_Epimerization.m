(* ============================================== *)
(* :Summary: CLM epimerization (APED and possibly other) logic. *)
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
Options[CLMEpimerization] = {};
(* ============================================== *)
InitializeEpimerizationValue = Indeterminate;
AssignEpimCoefficientsValue = Indeterminate;
(* ============================================== *)
(* ============================================== *)
(* The following coefficients govern epimerization model. *)
(* ============================================== *)
(* ============================================== *)
(* Coefficients for single amino acid based substances. *)
(* ============================================== *)
(* Distribution parameters of total epimerization rate. *)
EpimTotalRateDistribution = InverseGaussianDistribution;
EpimTotalRateParams = {1, 1};
EpimTotalRateControlParams = {}; // use default values
(* ============================================== *)
(* Distribution parameters of unweighted and not normalized epimerization coefficients. *)
EpimCoeffDistribution = InverseGaussianDistribution;
EpimCoeffParams = {1, 1};
EpimCoeffControlParams = {}; // use default values
(* ============================================== *)
(* EpimUnstableAAweight: varies between 0 and 1 and determines how stable is AA substance. *)
(* If value \[Equal] 0 then AA is completely stable (does not have any epimerization) and Aa is unstable. *)
(* If value \[Equal] 1 then Aa is completely stable (does not have any epimerization) and AA is unstable. *)
(* This weight is used during random generation of values of coefficients for AA like substances. *)
EpimUnstableAAweight = Indeterminate;
(* ============================================== *)
(* EpimSTermAAweight determines the allocation between N and S terminals for AA (and Aa) like substances : *)
(* If value \[Equal] 0 then S terminal is completely stable and N terminal is unstable. *)
(* If value \[Equal] 1 then N terminal is completely stable and S terminal is unstable. *)
(* This weight is used during random generation of values of coefficients for AA like substances. *)
EpimSTermAAweight = Indeterminate;
(* ============================================== *)
(* ============================================== *)
(* Coefficients for pairs of amino acids. *)
(* ============================================== *)
(* EpimTotalRateRandWeight: varies between 0 and 1 *)
(* The value determines linear allocation between deterministic and random value for total rate *)
(* If \[Equal] 0, then total rate of epimerization of a pair AB is completely determined by a function of two parameters: *)
(* EpimRatePairDet[A,B] where A and B are IDs of the relevant substances *)
(* If \[Equal] 1, then epimerization of a pair AB is completely random *)
EpimTotalRateRandWeight = Indeterminate;
(* ============================================== *)
(* EpimTotalRateDiffWeight: varies between -1 and 1 *)
(* The coefficient determines the behaviour (linear allocation) of function EpimRatePairDet *)
(* If \[Equal] 0, then the half sum is taken *)
(* If \[Equal] -1, then the first coefficient is used *)
(* If \[Equal] 1, then the second coefficient is used *)
EpimTotalRateDiffWeight = Indeterminate;
(* ============================================== *)
(* EpimCoeffRandWeight: varies between 0 and 1 *)
(* The value determines linear allocation between deterministic and random value for coefficients *)
(* If \[Equal] 0, then coefficients of epimerization of a pair AB are completely determined by a function of two parameters: *)
(* EpimCoeffPair[A,B] where A and B are IDs of the relevant substances *)
(* If \[Equal] 1, then epimerization of a pair AB is completely random *)
EpimCoeffRandWeight = Indeterminate;
(* ============================================== *)
(* EpimCoeffDiffWeight: varies between -1 and 1 *)
(* The coefficient determines the behaviour (linear allocation) of function EpimCoeffPairDet *)
(* If \[Equal] 0, then the half sum is taken *)
(* If \[Equal] -1, then the first coefficient is used *)
(* If \[Equal] 1, then the second coefficient is used *)
EpimCoeffDiffWeight = Indeterminate;
(* ============================================== *)
(* ============================================== *)
(* EpimTotalRateValue returns random value of total epimerizaton rate (sum of 4 coefficients). *)
EpimTotalRateValue[] := Module[{retVal},
  retVal = RandomCoefficientValue[EpimTotalRateDistribution, EpimTotalRateParams, EpimTotalRateControlParams];
  Return[retVal];
];
(* ============================================== *)
(* EpimCoeffValue returns random UNWEIGHTED value of relative epimerization coefficient. *)
(* See EpimCoeffRandom BELOW for further description. *)
EpimCoeffValue[] := Module[{retVal},
  retVal = RandomCoefficientValue[EpimCoeffDistribution, EpimCoeffParams, EpimCoeffControlParams];
  Return[retVal];
];
(* ============================================== *)
(* Functions to extract total rate and split among all 4 epimerization coefficients for given descriptor. *)
GetEpimTotalRate[epimDescr_?VectorQ] := epimDescr[[1]];
(* S-Terminal *)
GetkABtoAb[epimDescr_?VectorQ] := epimDescr[[2]];
GetkAbtoAB[epimDescr_?VectorQ] := epimDescr[[3]];
(* N-Terminal *)
GetkABtoaB[epimDescr_?VectorQ] := epimDescr[[4]];
GetkAbtoab[epimDescr_?VectorQ] := epimDescr[[5]];
(* All epimerization coefficients *)
GetEpimCoeffTable[epimDescr_?VectorQ] := Delete[epimDescr, 1];
(* ============================================== *)
(* GetEpimDescriptor returns epimerization descriptor for a given substance. *)
GetEpimDescriptor[substAid_?IntegerQ] := Module[{Aid, descr},
  Aid = Min[substAid, EnantiomerSubstanceID[substAid]];
  descr = EpimDescriptorFunc[Aid];

  If[!VectorQ[descr, NumericQ],
    (
    (* Generating new value. *)
      descr = Join[{EpimTotalRateValue[]}, EpimCoeffRandom[]];
      EpimDescriptorFunc[Aid] = descr;
    )
  ];

  Return[descr];
];
(* ============================================== *)
(* ============================================== *)
(* GetEpimDescriptor always returns a canonically sorted descriptor for Min[A, E[A]] and Min[B, E[B]] *)
(* The descriptor will contain: *)
(* Total rate (sum of all 4 epimerization rates). Call GetEpimTotalRate[descriptor] to extract. *)
(* Normalized coefficient for the reaction AB \[Rule] Ab. Call GetkABtoAb[descriptor] to extract. *)
(* Normalized coefficient for the reaction Ab \[Rule] AB. Call GetkAbtoAB[descriptor] to extract. *)
(* Normalized coefficient for the reaction AB \[Rule] aB. Call GetkABtoaB[descriptor] to extract. *)
(* Normalized coefficient for the reaction Ab \[Rule] ab. Call GetkAbtoab[descriptor] to extract. *)
(* If there is no descriptor for a given pair then it is calculated and stored. *)
(* ============================================== *)
If[!SilentRunValue, Print["TODO::CLM_Epimerization::GetEpimDescriptor::Fix check for canonical sort ordering."]];
(* ============================================== *)
GetEpimDescriptor[substAid_?IntegerQ, substBid_?IntegerQ] := Module[{Aid, Bid, descrA, descrB, rate, coeffA, coeffB, descr, coeff, rateA, rateB},
  Aid = Min[substAid, EnantiomerSubstanceID[substAid]];
  Bid = Min[substBid, EnantiomerSubstanceID[substBid]];

  (* Double check that canonical sort order of A vs. a and B vs. b coincides with enantiomeric content of A and B. *)
  (* If not then we can't proceed further. *)
  (*
If[SameChiralPolarizationQ[GetRightAminoAcid[Aid],GetLeftAminoAcid[Bid]],
(
Print["GetEpimDescriptor::Invalid canonical sort order for A and B, substAid = ", substAid, ", substBid = ", substBid, ". Quitting..."];
Quit[];
)
];
*)

  descrA = GetEpimDescriptor[Aid];

  If[Aid == Bid,
    (
      Return[descrA];
    )
  ];

  (* Attempting to get a numerical value *)
  descr = EpimDescriptorFunc[Aid, Bid];

  If[!VectorQ[descr, NumericQ],
    (
      descrB = GetEpimDescriptor[Bid];

      rateA = GetEpimTotalRate[descrA];
      rateB = GetEpimTotalRate[descrB];
      rate = EpimTotalRatePair[rateA, rateB];

      coeffA = GetEpimCoeffTable[descrA];
      coeffB = GetEpimCoeffTable[descrB];
      coeff = EpimCoeffPair[coeffA, coeffB];

      descr = Join[{rate}, coeff];
      EpimDescriptorFunc[Aid, Bid] = descr;
    )
  ];

  Return[descr];
];
(* ============================================== *)
(* ============================================== *)
(* EpimTotalRatePair returns total epimerization rate for a pair AB based on the rates of AA and BB *)
(* ============================================== *)
EpimTotalRatePair[epimRateA_, epimRateB_] := Module[{epimRate},
  epimRate = EpimTotalRatePairDet[epimRateA, epimRateB] * (1 - EpimTotalRateRandWeight) + EpimTotalRateValue[] * EpimTotalRateRandWeight;
  Return[epimRate];
];
(* ============================================== *)
(* ============================================== *)
(* EpimTotalRatePairDet returns deterministic part of the total epimerization rate of a pair of *)
(* two diferent amino acids A and B based on the total epimerization rates of A only and B only pairs. *)
(* ============================================== *)
EpimTotalRatePairDet[epimRateA_, epimRateB_] := Module[{epimRate},
  epimRate = ((epimRateA + epimRateB) / 2) + EpimTotalRateDiffWeight * ((epimRateB - epimRateA) / 2);
  Return[epimRate];
];
(* ============================================== *)
(* ============================================== *)
(* EpimCoeffPair returns weighted and normalized epimerization coefficients for a pair AB *)
(* based on the coefficients of AA and BB *)
(* ============================================== *)
EpimCoeffPair[coeffA_?VectorQ, coeffB_?VectorQ] := Module[{epimCoeff},
  epimCoeff = EpimCoeffPairDet[coeffA, coeffB] * (1 - EpimCoeffRandWeight) + EpimCoeffRandom[] * EpimCoeffRandWeight;
  Return[epimCoeff];
];
(* ============================================== *)
(* ============================================== *)
(* EpimCoeffPairDet returns deterministic part of weighted and normalized epimerization coefficients of a pair of *)
(* two diferent amino acids A and B based on the coefficients of A only and B only pairs. *)
(* ============================================== *)
EpimCoeffPairDet[coeffA_?VectorQ, coeffB_?VectorQ] := Module[{epimCoeff},
  epimCoeff = ((coeffA + coeffB) / 2) + EpimCoeffDiffWeight * ((coeffB - coeffA) / 2);
  Return[epimCoeff];
];
(* ============================================== *)
(* ============================================== *)
(* EpimCoeffRandom returns properly weighted and normalized random epimerization coefficients. *)
(* The sum of all 4 coefficients must be equal to 1. *)
(* There are two weights: *)
(* EpimUnstableAAweight determines how stable is AA (or LL) substance *)
(* If value \[Equal] 0 then AA is completely stable (does not have any epimerization) and Aa is unstable. *)
(* If value \[Equal] 1 then Aa is completely stable (does not have any epimerization) and AA is unstable. *)
(* EpimSTermAAweight determines the allocation between N and S terminals: *)
(* If value \[Equal] 0 then S terminal is completely stable and N terminal is unstable. *)
(* If value \[Equal] 1 then N terminal is completely stable and S terminal is unstable. *)
(* ============================================== *)
EpimCoeffRandom[] := Module[{coeff, ii, kAAtoAa, kAAtoaA, kAatoAA, kaAtoAA, norm},
  kAAtoAa = EpimUnstableAAweight * EpimSTermAAweight * EpimCoeffValue[];
  kAatoAA = (1 - EpimUnstableAAweight) * EpimSTermAAweight * EpimCoeffValue[];

  kAAtoaA = EpimUnstableAAweight * (1 - EpimSTermAAweight) * EpimCoeffValue[];
  kaAtoAA = (1 - EpimUnstableAAweight) * (1 - EpimSTermAAweight) * EpimCoeffValue[];

  coeff = {kAAtoAa, kAatoAA, kAAtoaA, kaAtoAA};
  norm = Sum[coeff[[ii]], {ii, 1, 4}];

  If[norm > 0,
    (
      coeff = coeff / norm;
    ),
    (
      Print["EpimCoeffRandom::Invalid coefficients. coeff = ", coeff // MatrixForm, ". Quitting..."];
      coeff = {Indeterminate, Indeterminate, Indeterminate, Indeterminate };
      Quit[];
    )
  ];

  Return[coeff];
];
(* ============================================== *)
(* ============================================== *)
(* EpimRate return epimerization rate for AB-like substance. *)
(* Suppose that the input substances are A and b, then the function returns the following: *)
(* If useSterm \[Equal] False then it returns a rate for Ab \[Rule] ab *)
(* If useSterm \[Equal] True then it returns a rate for Ab \[Rule] AB *)
(* ============================================== *)
EpimRate[substAid_?IntegerQ, substBid_?IntegerQ, useSTerm_?BooleanQ] := Module[{descr, Aid, Bid, substEAid, epimRate},
  descr = GetEpimDescriptor[substAid, substBid];

  substEAid = EnantiomerSubstanceID[substAid];

  If[substAid <= substEAid,
    (
      Aid = substAid;
      Bid = substBid;
    ),
    (
      Aid = substEAid;
      Bid = EnantiomerSubstanceID[substBid];
    )
  ];

  If[SameChiralPolarizationQ[GetRightAminoAcid[Aid], GetLeftAminoAcid[Bid]],
    (
    (* We have something like A and B *)
      epimRate = GetEpimTotalRate[descr] * If[useSTerm, GetkABtoAb[descr], GetkABtoaB[descr]];
    ),
    (
    (* We have something like A and b *)
      epimRate = GetEpimTotalRate[descr] * If[useSTerm, GetkAbtoAB[descr], GetkAbtoab[descr]];
    )
  ];

  Return[epimRate];
];
(* ============================================== *)
(* ============================================== *)
(* EpimRateValue returns epimerization rate from substID to substEpimID *)
If[!SilentRunValue, Print["TODO:CLM_Epimerization::EpimRateValue needs to be rewritten."]];
(* ============================================== *)
EpimRateValue[substID_?IntegerQ, substEpimID_?IntegerQ] := Module[{rate, name, nameEpim, AsubstID, BsubstID, AsubstEpimID, BsubstEpimID},
  name = GetSubstanceName[substID];
  nameEpim = GetSubstanceName[substEpimID];
  (*
If[StringLength[name] \[NotEqual]2 || StringLength[nameEpim]\[NotEqual] 2, 
(
Print["EpimRateValue::Invalid length of chains for substances: ", name, ", ", nameEpim ,"."];
Return[Indeterminate];
)
];
*)
  If[name == nameEpim,
    (
      Print["EpimRateValue::Substances are identical: ", name, ", ", nameEpim , "."];
      Return[Indeterminate];
    )
  ];

  If[ToUpperCase[name] != ToUpperCase[nameEpim],
    (
      Print["EpimRateValue::Substances don't match for epimerization: ", name, ", ", nameEpim , "."];
      Return[Indeterminate];
    )
  ];

  AsubstID = GetSubstanceID[StringTake[name, {1}]];
  BsubstID = GetSubstanceID[StringTake[name, {2}]];

  AsubstEpimID = GetSubstanceID[StringTake[nameEpim, {1}]];
  BsubstEpimID = GetSubstanceID[StringTake[nameEpim, {2}]];

  rate = If[AsubstID == AsubstEpimID, EpimRate[AsubstID, BsubstID, True], EpimRate[AsubstID, BsubstID, False]];

  Return[rate];
];
(* ============================================== *)
(* ============================================== *)
(* CreateEpimReactionInfo create a record of 4-tuple of epimerization reactions. *)
(* We store reaction info only for minimum of both enantiomers and only one record for all 4 reactions. *)
CreateEpimReactionInfo[substAid_?IntegerQ, substBid_?IntegerQ] := Module[{reacInfo, Aid, Bid},
  Aid = Min[substAid, EnantiomerSubstanceID[substAid]];
  Bid = Min[substBid, EnantiomerSubstanceID[substBid]];
  reacInfo = {Aid, Bid};
  Return[reacInfo];
];
(* ============================================== *)
(* ============================================== *)
(* EpimReactionRecordCheck returns true if there is a record for the reaction. *)
(* We need to be able to check if a given reaction IS catalyzed by a given catalyst *)(* without invoking full evaluation "test". *)
EpimReactionRecordCheck[reacInfo_?VectorQ] := Module[{retVal},
  retVal = False;
  If[BooleanQ[Apply[EpimReacMatrix, reacInfo]], retVal = True];
  Return[retVal];
];
(* ============================================== *)
(* ============================================== *)
(* EpimReacMatrixAdd adds record of the reaction into EpimReacMatrix *)
EpimReacMatrixAdd[reacInfo_?VectorQ] := Module[{},
  ToExpression[ToString[Apply[EpimReacMatrix, reacInfo]] <> "=True"];
];
(* ============================================== *)
(* ============================================== *)
(* InitializeAllEpimerizationReactions initialized all epimerization reactions *)
InitializeAllEpimerizationReactions[rawOpts___] := Module[{opts, chainLenStart, chainLenEnd, ii, jj, len, firstSubstName, Aid, WOfirstSubstName, Bid, Aname, aID, aName, Bname, bID, bName, reacLstOrig, reacLstOrigE, reacLst, reacCount, reacIdx, substId, substEpimId, reacInfo},

  opts = ProcessOptions[rawOpts];

  If[InitializeEpimerizationValue,
    (
      If[!SilentRunValue, Print["InitializeAllEpimerizationReactions::Assigning epimerization reactions."]];
    ),
    (
      If[!SilentRunValue, Print["InitializeAllEpimerizationReactions::NOT assigning epimerization reactions."]];
      Return[];
    )
  ];

  If[!SilentRunValue, Print["TODO::InitializeAllEpimerizationReactions::Implement epimerization on S terminal when chainLenEnd > 2."]];

  chainLenStart = 2;
  chainLenEnd = MaxChainLength;

  Do[
    (
      If[!SilentRunValue, Print[strSeparatorCRLF]];
      len = Length[AllChainsTbl[[chainLenCnt]]];
      If[!SilentRunValue, Print["InitializeAllEpimerizationReactions::chainLenCnt = ", chainLenCnt, ", len = ", len, ", Number of reactions (NoCnt) = ", NoCnt]];

      Do[
        (
        (* A is the first amino acid in the chain. *)
          firstSubstName = StringTake[AllChainsTbl[[chainLenCnt, ii]], {1}];
          Aid = Min[GetSubstanceID[firstSubstName], EnantiomerSubstanceID[GetSubstanceID[firstSubstName]]];
          Aname = GetSubstanceName[Aid];
          aID = EnantiomerSubstanceID[Aid];
          aName = GetSubstanceName[aID];

          (* B is the rest of the chain without the first amino acid. *)
          WOfirstSubstName = StringTake[AllChainsTbl[[chainLenCnt, ii]], {2, chainLenCnt}];
          Bid = Min[GetSubstanceID[WOfirstSubstName], EnantiomerSubstanceID[GetSubstanceID[WOfirstSubstName]]];
          Bname = GetSubstanceName[Bid];
          bID = EnantiomerSubstanceID[Bid];
          bName = GetSubstanceName[bID];

          (*
lastSubstName=StringTake[AllChainsTbl[[chainLenCnt,ii]],{chainLenCnt}];
WOlastSubstName=StringTake[AllChainsTbl[[chainLenCnt,ii]],{1,chainLenCnt-1}];
*)

          (* Possible epimerization reactions for chains of length 2 are: *)
          (* AB \[Rule] aB *)
          (* AB \[Rule] Ab *)
          (* Ab \[Rule] ab *)
          (* Ab \[Rule] AB *)
          (* and E[EQ] *)

          (* Possible supported epimerization reactions for chains of length > 2 are: *)
          (* ABxxx \[Rule] aBxxx *)
          (* aBxxx \[Rule] ABxxx *)
          (* and E[EQ] *)

          reacInfo = CreateEpimReactionInfo[Aid, Bid];

          (* We assign reactions only if we have not done that yet for enantiomers. *)
          If[!EpimReactionRecordCheck[reacInfo],
            (
              EpimReacMatrixAdd[reacInfo];

              If[chainLenCnt == 2,
                (
                  reacLstOrig = {{MergeChains[Aname, Bname], MergeChains[aName, Bname]}, {MergeChains[Aname, Bname], MergeChains[Aname, bName]}, {MergeChains[Aname, bName], MergeChains[aName, bName]}, {MergeChains[Aname, bName], MergeChains[Aname, Bname]}};
                ),
                (
                  reacLstOrig = {{MergeChains[Aname, Bname], MergeChains[aName, Bname]}, {MergeChains[Aname, bName], MergeChains[aName, bName]}};
                )
              ];

              reacCount = Length[reacLstOrig];
              reacLstOrigE = Table[EnantiomerChain[reacLstOrig[[ii, jj]]], {ii, 1, reacCount}, {jj, 1, 2}];
              reacLst = Sort[DeleteDuplicates[Join[reacLstOrig, reacLstOrigE]]];

              (*
Print["InitializeAllEpimerizationReactions::reacLstOrig = ", reacLstOrig // MatrixForm,", reacLstOrigE = ", reacLstOrigE // MatrixForm, ", reacLst = ", reacLst // MatrixForm ];
*)

              reacCount = Length[reacLst];

              Do[
                (
                  substId = GetSubstanceID[reacLst[[reacIdx, 1]]];
                  substEpimId = GetSubstanceID[reacLst[[reacIdx, 2]]];

                  AssignEpimReaction[substId, substEpimId];
                ),
                {reacIdx, 1, reacCount}
              ];
            )
          ];
        ),
        {ii, 1, len}
      ];

    ),
    {chainLenCnt, chainLenStart, chainLenEnd}
  ];
  (* ============================================== *)

  If[!SilentRunValue, Print["InitializeAllEpimerizationReactions::Final number of reactions = ", NoCnt]];
];
(* ============================================== *)
(* ============================================== *)
AssignEpimReaction[substId_?IntegerQ, substEpimId_?IntegerQ] := Module[{name, reacName, substIdE, id, epimId, nameCoeff, reacNameCoeff, coeffIdxName, coeffName, reacStringName, reacIdxName, retVal, reacStringNameCoeff, substIdxName, substIdxEpimName},
  name = GetSubstanceName[substId];
  reacName = GetSubstanceName[substEpimId];
  substIdE = EnantiomerSubstanceID[substId];
  substIdxName = IndexPrefixValue <> name;
  substIdxEpimName = IndexPrefixValue <> reacName;

  id = Min[substId, substIdE];
  epimId = If[id < substId, EnantiomerSubstanceID[substEpimId], substEpimId];

  nameCoeff = GetSubstanceName[id];
  reacNameCoeff = GetSubstanceName[epimId];
  reacStringNameCoeff = name <> " -> " <> reacName;

  coeffIdxName = CoeffPrefixValue <> "Idx" <> nameCoeff <> ToLetter <> reacNameCoeff;
  coeffName = CoeffPrefixValue <> nameCoeff <> ToLetter <> reacNameCoeff;

  reacStringName = name <> " -> " <> reacName;
  reacIdxName = ReactionPrefixValue <> name <> ToLetter <> reacName;

  (* We assign coefficients only once for a pair of enantiomers *)
  If[substId <= substIdE,
    (
      ToExpression[coeffIdxName <> "=AddCoeffName[" <> coeffName <> ",Subscript[k,\"" <> reacStringNameCoeff <> "\"]]"];

      If[AssignEpimCoefficientsValue,
        (
        (* Print["Assigning value to coefficient, ",coeffName]; *)
          ToExpression[coeffName <> "=EpimRateValue[" <> ToString[substId] <> ", " <> ToString[substEpimId] <> "]"];
        )
      ];
    )
  ];

  ToExpression[reacIdxName <> "=AddReaction[{{EpimReaction,\"" <> reacStringName <> "\"},{{" <> substIdxName <> ",1}},{" <> coeffName <> ",1},{{" <> substIdxEpimName <> ",1}}}]"];

  retVal = ToExpression[reacIdxName];
  Return[retVal];
];
(* ============================================== *)
