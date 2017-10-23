(* ============================================== *)
(* :Summary: CLM direct sedimentation logic. *)
(* ============================================== *)
(* :Author: Konstantin K. Konstantinov *)
(* :Email: konstantin.k.konstantinov@gmail.com *)
(* :License type: GPL v3 or any later version, see http://www.gnu.org/licenses/ *)
(* :Copyright: K^3, 2013 - 2017 *)
(* :Version: 3.26.001, Date : 2017/10/21 *)
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
InitializeDirectCrystallizationValue = Indeterminate;
(* ============================================== *)
AssignDirectCrystCoefficientsValue = Indeterminate;
(* ============================================== *)
UseAllPairsForDirectCrystValue = Indeterminate;
(* ============================================== *)
kDirectCrystCoeffDistribution = ParetoDistribution;
kDirectCrystCoeffParams = {1, 1};
kDirectCrystCoeffControlParams = {}; // use default values
(* ============================================== *)
DirectCrystMinLen := 1;
DirectCrystSecondSubstMinLen := 1;
(* ============================================== *)
(* If True, then consider that some substances serve as "matrices" to which any pepeide can attach by one of the ends to form a sediment *)
UseMatrixDirectCryst := True;

(* If True, then left ennd of peptide will "attach" to matrix. If False, then right *)
UseLeftEndForDirectCryst :=True;
(* ============================================== *)
(* TODO : kDirectCrystCoefficientValue::base was supposed to be a length of one enantiomer in the pair
          Once the code was extended to support any pairs that assumtion can no longer be applied.
          Subsequently base = 0 is currently pushed down. *)

Print["TODO::CLM_SedimentationDirect::kDirectCrystCoefficientValue::base was supposed to be a length of one enantiomer in the pair Once the code was extended to support any pairs that assumtion can no longer be applied. Subsequently base = 0 is currently pushed down."];
kDirectCrystCoefficientValueSimple[substAid_?IntegerQ, substBid_?IntegerQ] := Module[{retVal, base},

  (* base = (GetChainLength[substAid] - 1) / 2; *)
  base = 0;

  retVal = RandomCoefficientValue[kDirectCrystCoeffDistribution, kDirectCrystCoeffParams, kDirectCrystCoeffControlParams, base];
  (* If[!SilentRunValue, Print["kDirectCrystCoefficientValueSimple:: substAid = ", substAid, ", GetChainLength[substAid] = ", GetChainLength[substAid], ", substBid = ", substBid, ", GetChainLength[substBid] = ", GetChainLength[substBid], ", base = ", base, ", retVal = ", retVal]]; *)
  Return[retVal];
];
(* ============================================== *)
(* substAid is considered as a "sedimentation matrix". See description for UseMatrixDirectCryst *)
GenerateAllkDirectCrystCoefficientMatrix[substAid_?IntegerQ, substBid_?IntegerQ] := Module[
  {retVal, aID, bID, EaID, EbID, descr, base, rndAvgCoeff, rndAvgCoeffE, bEndID, EbEndID, ii, simpleSubstLst, MatchingEnantiomerQ, matchingSubstLst, multLst, multLstE, decrE, printTbl},

  If[(EnantiomerSubstanceID[substAid] < substAid),
    (
      aID = EnantiomerSubstanceID[substAid];
      bID = EnantiomerSubstanceID[substBid];
    ),
    (
      aID = substAid;
      bID = substBid;
    )
  ];

  EaID = EnantiomerSubstanceID[aID];
  EbID = EnantiomerSubstanceID[bID];

  bEndID = GetEndAminoAcid[bID, UseLeftEndForDirectCryst];
  EbEndID = GetEndAminoAcid[EbID, UseLeftEndForDirectCryst];

  descr = AllDirectCrystDescriptorFunc[aID, bID];

  (* Print["GenerateAllkDirectCrystCoefficientMatrix::bEndID = ", bEndID, ", ", GetSubstanceName[bEndID]]; *)

  If[!NumericQ[descr],
    (
      (* Generating new values. *)

    (* Returns True is substances are matching bEndID *)
      MatchingEnantiomerQ[substID_?IntegerQ] := Module[{retVal, endAminAcid},
        endAminAcid = GetEndAminoAcid[substID, UseLeftEndForDirectCryst];
        retVal = If[NoOfL[bEndID] == NoOfL[endAminAcid] && NoOfD[bEndID] == NoOfD[endAminAcid], True, False, Indeterminate];
        (* Print["    MatchingEnantiomerQ::substID = ", substID, ", ", GetSubstanceName[substID], ", endAminAcid = ", endAminAcid, ", ", GetSubstanceName[endAminAcid], ", retVal = ", retVal]; *)
        (* Print["    MatchingEnantiomerQ::NoOfL[bEndID] = ", NoOfL[bEndID], ", NoOfL[endAminAcid] = ", NoOfL[endAminAcid], ", NoOfD[bEndID] = ", NoOfD[bEndID], ", NoOfD[endAminAcid] = ", NoOfD[endAminAcid]]; *)
        Return[retVal];
      ];

      (* base is not used yet *)
      base = 0;
      (* Average coeff for substances like aID + bID *)
      rndAvgCoeff = RandomCoefficientValue[kDirectCrystCoeffDistribution, kDirectCrystCoeffParams, kDirectCrystCoeffControlParams, base];

      (* Average coeff for substances like aID + EbID *)
      rndAvgCoeffE = RandomCoefficientValue[kDirectCrystCoeffDistribution, kDirectCrystCoeffParams, kDirectCrystCoeffControlParams, base];

      (* All simple substances (peptides) *)
      simpleSubstLst = Table[ii,{ii, 1, NoSimpleSubstCnt}];

      (* Peptides, which have matching end. *)
      matchingSubstLst = Select[simpleSubstLst, MatchingEnantiomerQ];

      (* TODO - Add distribution *)
      multLst = Table[RandomReal[],{ii, 1, Length[matchingSubstLst]}];
      multLstE = Table[RandomReal[],{ii, 1, Length[matchingSubstLst]}];

      (* multLst = Table[1.0,{ii, 1, Length[matchingSubstLst]}]; *)
      (* multLstE = Table[1.0,{ii, 1, Length[matchingSubstLst]}]; *)

      multLst = multLst / Mean[multLst];
      multLstE = multLst / Mean[multLst];

      For[ii = 1, ii <= Length[matchingSubstLst], ii++,
        (
          descr = rndAvgCoeff * multLst[[ii]];
          decrE = rndAvgCoeffE * multLstE[[ii]];
          AllDirectCrystDescriptorFunc[aID, matchingSubstLst[[ii]]] = descr;
          AllDirectCrystDescriptorFunc[EnantiomerSubstanceID[aID], EnantiomerSubstanceID[matchingSubstLst[[ii]]]] = descr;

          AllDirectCrystDescriptorFunc[aID, EnantiomerSubstanceID[matchingSubstLst[[ii]]]] = decrE;
          AllDirectCrystDescriptorFunc[EnantiomerSubstanceID[aID], matchingSubstLst[[ii]]] = decrE;
        )
      ];

      printTbl = Table[{ii, matchingSubstLst[[ii]], GetSubstanceName[matchingSubstLst[[ii]]], AllDirectCrystDescriptorFunc[aID, matchingSubstLst[[ii]]]},{ii,1, Length[matchingSubstLst]}];
      (* Print["GenerateAllkDirectCrystCoefficientMatrix::substA = ", GetSubstanceName[substAid], ", substB = ", GetSubstanceName[substBid], ", matchingSubstLst = ", printTbl // MatrixForm]; *)
    )
  ];

  Return[];
];

kDirectCrystCoefficientValueMatrix[substAid_?IntegerQ, substBid_?IntegerQ] := Module[{descr},
  (* If[!SilentRunValue, Print["kDirectCrystCoefficientValueMatrix:: substAid = ", substAid, ", GetChainLength[substAid] = ", GetChainLength[substAid], ", substBid = ", substBid, ", GetChainLength[substBid] = ", GetChainLength[substBid]]]; *)
  GenerateAllkDirectCrystCoefficientMatrix[substAid, substBid];
  descr = AllDirectCrystDescriptorFunc[substAid, substBid];
  Return[descr];
];
(* ============================================== *)
kDirectCrystCoefficientValue[substAid_?IntegerQ, substBid_?IntegerQ] :=
    If[UseMatrixDirectCryst,
      kDirectCrystCoefficientValueMatrix[substAid, substBid],
      kDirectCrystCoefficientValueSimple[substAid, substBid],
      kDirectCrystCoefficientValueSimple[substAid, substBid]
    ];
(* ============================================== *)
(* Simplified direct sedimentation is A + B \[Rule] (NA + NB) * Y *)
AssignDirectCrystReactions[substIdVal_?IntegerQ, subst1Id1Val_?IntegerQ, multiplier_?IntegerQ, allocateCoeff_?BooleanQ] := Module[
  {substIDlst, substEiDlst, substId, subst1Id, name, name1, base, base1, substLen, substDecayID, substDecayName, retVal, nameCoeff, name1Coeff, baseSubstId, coeffIdxName, coeffName, reacStringName, reacIdxName},
(*
  If[!SilentRunValue,
    Print["AssignDirectCrystReactions::Starting..."];
    Print["AssignDirectCrystReactions::substIdVal = ", substIdVal, ", subst1Id1Val = ", subst1Id1Val, ", multiplier = ", multiplier, ", allocateCoeff = ", allocateCoeff];
  ];
*)

  If[UseMatrixDirectCryst,
    (
      (* We DO NOT sort the substances in canonic ordering when UseMatrixDirectCryst = True *)
      (* The first substance is considered as a "matrix" to which many substances can attach by one of the ends *)
      substIDlst = {substIdVal, subst1Id1Val};
      substEiDlst = {EnantiomerSubstanceID[substIdVal], EnantiomerSubstanceID[subst1Id1Val]};
    ),
    (
    (* We sort the substances in canonic ordering (by IDs) *)
      substIDlst = Sort[{substIdVal, subst1Id1Val}];
      substEiDlst = Sort[{EnantiomerSubstanceID[substIdVal], EnantiomerSubstanceID[subst1Id1Val]}];
    )
  ];

  substId = substIDlst[[1]];
  subst1Id = substIDlst[[2]];

  name = GetSubstanceName[substId];
  name1 = GetSubstanceName[subst1Id];

  base = GetChainLength[substId];
  base1 = GetChainLength[subst1Id];
  substLen = base + base1;

  substDecayID = idxY;
  substDecayName = GetSubstanceName[substDecayID];

  retVal = {};

  If[substId != subst1Id,
    (
      If[substIDlst[[1]] <= substEiDlst[[1]],
        (
          nameCoeff = name;
          name1Coeff = name1;
        ),
        (
          nameCoeff = GetSubstanceName[ substEiDlst[[1]]];
          name1Coeff = GetSubstanceName[ substEiDlst[[2]]];
        )
      ];
    ),
    (
      baseSubstId = Min[EnantiomerSubstanceID[substId], substId];
      nameCoeff = GetSubstanceName[baseSubstId];
      name1Coeff = nameCoeff;
    )
  ];

  (* Direct crystallization  *)
  coeffIdxName = CoeffPrefixValue <> "Idx" <> nameCoeff <> PlusLetter <> name1Coeff <> ToLetter <> ToString[substLen] <> substDecayName;
  coeffName = CoeffPrefixValue <> nameCoeff <> PlusLetter <> name1Coeff <> ToLetter <> ToString[substLen] <> substDecayName;
  reacStringName = name <> " + " <> name1 <> " -> " <> ToString[substLen] <> substDecayName;
  reacIdxName = ReactionPrefixValue <> name <> PlusLetter <> name1 <> ToLetter <> ToString[substLen] <> substDecayName;

  (* If[!SilentRunValue, Print["AssignDirectCrystReactions::reacIdxName = ", reacIdxName, ", reacStringName = ", reacStringName, ", coeffName = ", coeffName,", substId = ", substId, ", subst1Id = ", subst1Id]]; *)

  If[allocateCoeff,
    (
      (* If[!SilentRunValue, Print["AssignDirectCrystReactions::Allocating coefficient..."]]; *)
      ToExpression[coeffIdxName <> "=AddCoeffName[" <> coeffName <> ",Subscript[k,\"" <> reacStringName <> "\"]]"];

      If[AssignDirectCrystCoefficientsValue,
        (
          (* If[!SilentRunValue, Print["AssignDirectCrystReactions::Calling kDirectCrystCoefficientValue..."]]; *)
          ToExpression[coeffName <> "=kDirectCrystCoefficientValue[" <> ToString[substId] <> ", " <> ToString[subst1Id] <> "]"];
        )
      ];
    )
  ];

  ToExpression[reacIdxName <> "=AddReaction[{{DirectCrystReaction,\"" <> reacStringName <> "\"},{{" <> ToString[substId] <> ",1},{" <> ToString[subst1Id] <> ",1}},{" <> ToString[multiplier] <> coeffName <> ",1,1},{{" <> ToString[substDecayID] <> "," <> ToString[substLen] <> "}}}]"];

  retVal = reacIdxName;
  Return[retVal];
];
(* ============================================== *)
(* Simplified direct sedimentation is A + B \[Rule] (NA + NB) * Y *)
AssignDirectCrystReactionsOld[substIdVal_?IntegerQ, subst1Id1Val_?IntegerQ, multiplier_?IntegerQ, allocateCoeff_?BooleanQ] := Module[{substIDlst, substEiDlst, substId, subst1Id, name, name1, base, base1, substLen, substDecayID, substDecayName, retVal, nameCoeff, name1Coeff, baseSubstId, coeffIdxName, coeffName, reacStringName, reacIdxName},
  (* Print["AssignDirectCrystReactions::Starting..."]; *)
  (* We sort the substances in canonic ordering (by IDs) *)

  substIDlst = Sort[{substIdVal, subst1Id1Val}];
  substEiDlst = Sort[{EnantiomerSubstanceID[substIdVal], EnantiomerSubstanceID[subst1Id1Val]}];

  substId = substIDlst[[1]];
  subst1Id = substIDlst[[2]];

  name = GetSubstanceName[substId];
  name1 = GetSubstanceName[subst1Id];

  base = GetChainLength[substId];
  base1 = GetChainLength[subst1Id];
  substLen = base + base1;

  substDecayID = idxY;
  substDecayName = GetSubstanceName[substDecayID];

  retVal = {};

  If[substId != subst1Id,
    (
      If[substIDlst[[1]] <= substEiDlst[[1]],
        (
          nameCoeff = name;
          name1Coeff = name1;
        ),
        (
          nameCoeff = GetSubstanceName[ substEiDlst[[1]]];
          name1Coeff = GetSubstanceName[ substEiDlst[[2]]];
        )
      ];
    ),
    (
      baseSubstId = Min[EnantiomerSubstanceID[substId], substId];
      nameCoeff = GetSubstanceName[baseSubstId];
      name1Coeff = nameCoeff;
    )
  ];

  (* Direct crystallization  *)
  coeffIdxName = CoeffPrefixValue <> "Idx" <> nameCoeff <> PlusLetter <> name1Coeff <> ToLetter <> ToString[substLen] <> substDecayName;
  coeffName = CoeffPrefixValue <> nameCoeff <> PlusLetter <> name1Coeff <> ToLetter <> ToString[substLen] <> substDecayName;
  reacStringName = name <> " + " <> name1 <> " -> " <> ToString[substLen] <> substDecayName;
  reacIdxName = ReactionPrefixValue <> name <> PlusLetter <> name1 <> ToLetter <> ToString[substLen] <> substDecayName;

  (* Print["AssignDirectCrystReactions::reacIdxName = ", reacIdxName, ", reacStringName = ", reacStringName, ", coeffName = ", coeffName,", substId = ", substId, ", subst1Id = ", subst1Id]; *)

  If[allocateCoeff,
    (
      ToExpression[coeffIdxName <> "=AddCoeffName[" <> coeffName <> ",Subscript[k,\"" <> reacStringName <> "\"]]"];

      If[AssignDirectCrystCoefficientsValue,
        (
          ToExpression[coeffName <> "=kDirectCrystCoefficientValue[" <> ToString[substId] <> ", " <> ToString[subst1Id] <> "]"];
        )
      ];
    )
  ];

  ToExpression[reacIdxName <> "=AddReaction[{{DirectCrystReaction,\"" <> reacStringName <> "\"},{{" <> ToString[substId] <> ",1},{" <> ToString[subst1Id] <> ",1}},{" <> ToString[multiplier] <> coeffName <> ",1,1},{{" <> ToString[substDecayID] <> "," <> ToString[substLen] <> "}}}]"];

  retVal = reacIdxName;
  Return[retVal];
];
(* ============================================== *)
InitializeDirectCrystReactions[rawOpts___] := Module[
  {opts, ii, idxCval, idxChainStart, idxChainEnd, idxChain, len, reacAA, substPairAA, substCrystAA, reacaa, substPairaa, substCrystaa, reacAa, substPairAa, substCrystAa, idxChainB, substBlst, lenB, jj, substBid, idxB, BlstLen, Aid, Bid, enantAid, enantBid, kk, idxChainBend},
  opts = ProcessOptions[rawOpts];

  InitializeDirectCrystallizationValue = InitializeDirectCrystallization /. opts /. Options[CLMChains];
  AssignDirectCrystCoefficientsValue = AssignDirectCrystCoefficients /. opts /. Options[CLMChains];
  UseAllPairsForDirectCrystValue = UseAllPairsForDirectCryst /. opts /. Options[CLMChains];

  If[InitializeDirectCrystallizationValue && (!(InitializeBasicCrystValue || InitializeChainCrystValue)),
    (
      If[!SilentRunValue, Print["InitializeDirectCrystReactions::Initializing direct crystallization reactions..."]];
      idxChainStart = DirectCrystMinLen;
      idxChainEnd = MaxChainLength;
    ),
    (
      If[!SilentRunValue, Print["InitializeDirectCrystReactions::Not initializing direct crystallization reactions..."]];
      Return[];
    )
  ];

  Do[
    (
      len = Length[AllChainsTbl[[idxChain]]] / 2;
      (* Print["InitializeDirectCrystReactions::Used subst for idxChain = ", idxChain, " are ", Table[{ii, AllChainsTbl[[idxChain, ii]]}, {ii, 1, len}] // MatrixForm]; *)
      If[!SilentRunValue, Print["InitializeDirectCrystReactions::len = ", len]];
      If[!IntegerQ[len], Abort[]];

      (* If the first substance is considered as a "matrix" then order matters and we have to consider all second substances *)
      idxChainBend = If[UseMatrixDirectCryst, idxChainEnd, idxChain];

      Do[
        (
          Aid = GetSubstanceID[AllChainsTbl[[idxChain, ii]]];
          enantAid = EnantiomerSubstanceID[Aid];

          If[UseAllPairsForDirectCrystValue,
            (
              substBlst = {};
              Do[
                (
                  If[UseMatrixDirectCryst,
                    (
                      (* Enantiomers will be picked up further in this function. *)
                      lenB = Length[AllChainsTbl[[idxChainB]]] / 2;
                    ),
                    (
                      If[idxChainB == idxChain,
                        (
                          (* lenB = If[UseMatrixDirectCryst, Length[AllChainsTbl[[idxChainB]]] / 2, ii]; *)
                          lenB = ii;
                        ),
                        (
                          (* lenB = If[UseMatrixDirectCryst, Length[AllChainsTbl[[idxChainB]]], Length[AllChainsTbl[[idxChainB]]] / 2]; *)
                          lenB = Length[AllChainsTbl[[idxChainB]]] / 2;
                          (* If[!SilentRunValue, Print["InitializeDirectCrystReactions::lenB = ", lenB]]; *)
                          If[!IntegerQ[lenB], Abort[]];
                        )
                      ];
                    )
                  ];

                  (* If[!SilentRunValue, Print["InitializeDirectCrystReactions::idxChain = ", idxChain, ", ii = ", ii, ", idxChainB = ", idxChainB, ", lenB = ", lenB]]; *)

                  Do[
                    (
                      substBid = GetSubstanceID[AllChainsTbl[[idxChainB, jj]]];
                      substBlst = Join[substBlst, {substBid}];
                    ), {jj, 1, lenB}
                  ];
                ), {idxChainB, DirectCrystSecondSubstMinLen, idxChainBend}
              ];
            ),
            (
              substBlst = {EnantiomerSubstanceID[Aid]};
            )
          ];

          BlstLen = Length[substBlst];
          (* Print["    Aid = ", Aid, ", ",  GetSubstanceName[Aid], ", substBlst =  ", Table[{ii, substBlst[[ii]], GetSubstanceName[substBlst[[ii]]]}, {ii, 1, BlstLen}] // MatrixForm]; *)

          (* If[!SilentRunValue, Print["InitializeDirectCrystReactions::A = ", SubstanceMatrix[Aid], ", substBlst = ", Table[{kk, substBlst[[kk]], SubstanceMatrix[substBlst[[kk]]]}, {kk, 1, BlstLen}] // MatrixForm]]; *)

          Do[
            (
              Bid = substBlst[[idxB]];
              enantBid = EnantiomerSubstanceID[Bid];

              (* Direct crystallization reactions like like A+B \[Rule] 2Y *)
              AssignDirectCrystReactions[Aid, Bid, 1, True];

              (* Direct crystallization reactions like like a+b \[Rule] 2Y *)
              AssignDirectCrystReactions[enantAid, enantBid, 1, False];

              (* Direct crystallization reactions like like A+b \[Rule] 2Y *)
              AssignDirectCrystReactions[Aid, enantBid, 1, True];

              (* Direct crystallization reactions like like a+B \[Rule] 2Y *)
              AssignDirectCrystReactions[enantAid, Bid, 1, False];

            ), {idxB, 1, BlstLen}
          ];
        ), {ii, len}
      ];
    ), {idxChain, idxChainStart, idxChainEnd}
  ];

  If[!SilentRunValue,
    (
      PrintTimeUsed[];
      Print["InitializeDirectCrystReactions::Completed."]
    )
  ];
];
(* ============================================== *)
