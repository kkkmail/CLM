(* ============================================== *)
(* :Summary: CLM sedimentation logic. *)
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
InitializePairFormationValue = Indeterminate;
UseSamePairFormCoeffValue = Indeterminate;
UseSymmetricPairsValue = Indeterminate;

InitializeCrystallizationValue = Indeterminate;
InitializeCrystalDecayValue = Indeterminate;
InitializeBasicCrystValue = Indeterminate;
InitializeChainCrystValue = Indeterminate;
InitializeDiastereomerOnlyPairsValue = Indeterminate;
InitializeTwoSubstCrystValue = Indeterminate;

TwoSubstCrystValue = {};
(* ============================================== *)
AssignBasicCrystCoefficientsValue = Indeterminate;
AssignChainCrystCoefficientsValue = Indeterminate;
(* ============================================== *)
DiastCoeffDistribution = InverseGaussianDistribution;
DiastCoeffParams = {1, 1};
DiastCoeffControlParams = {}; // use default values
(* ============================================== *)
InvDiastCoeffDistribution = InverseGaussianDistribution;
InvDiastCoeffParams = {1, 1};
InvDiastCoeffControlParams = {}; // use default values
(* ============================================== *)
rMaxCoeffDistribution = InverseGaussianDistribution;
rMaxCoeffParams = {1, 1};
rMaxCoeffControlParams = {}; // use default values

kCrystCoeffDistribution = InverseGaussianDistribution;
kCrystCoeffParams = {1, 1};
kCrystCoeffControlParams = {}; // use default values

kDissCoeffDistribution = InverseGaussianDistribution;
kDissCoeffParams = {1, 1};
kDissCoeffControlParams = {}; // use default values
(* ============================================== *)
CrystDecayCoeffDistribution = InverseGaussianDistribution;
CrystDecayCoeffParams = {1, 1};
CrystDecayCoeffControlParams = {}; // use default values
(* ============================================== *)
DiastCoefficientValue[substAid_?IntegerQ, substBid_?IntegerQ] := Module[{retVal},
  retVal = RandomCoefficientValue[DiastCoeffDistribution, DiastCoeffParams, DiastCoeffControlParams];
  Return[retVal];
];
(* ============================================== *)
InvDiastCoefficientValue[substAid_?IntegerQ, substBid_?IntegerQ] := Module[{retVal},
  retVal = RandomCoefficientValue[InvDiastCoeffDistribution, InvDiastCoeffParams, InvDiastCoeffControlParams];
  Return[retVal];
];
(* ============================================== *)
(* TODO::CLM_Chain::rMaxCoefficientValue, kCrystCoefficientValue, kDissCoefficientValue - calculation of base should be updated...*)
Print["TODO::CLM_Chain::rMaxCoefficientValue, kCrystCoefficientValue, kDissCoefficientValue - calculation of base should be updated..."];
rMaxCoefficientValue[substAid_?IntegerQ, substBid_?IntegerQ] := Module[{retVal, base},
  base = (GetChainLength[substAid] - 1) / 2;
  retVal = RandomCoefficientValue[rMaxCoeffDistribution, rMaxCoeffParams, rMaxCoeffControlParams, base];
  Return[retVal];
];
(* ============================================== *)
kCrystCoefficientValue[substAid_?IntegerQ, substBid_?IntegerQ] := Module[{retVal, base},
  base = (GetChainLength[substAid] - 1) / 2;
  retVal = RandomCoefficientValue[kCrystCoeffDistribution, kCrystCoeffParams, kCrystCoeffControlParams, base];
  Return[retVal];
];
(* ============================================== *)
kDissCoefficientValue[substAid_?IntegerQ, substBid_?IntegerQ] := Module[{retVal, base},
  base = (GetChainLength[substAid] - 1) / 2;
  retVal = RandomCoefficientValue[kDissCoeffDistribution, kDissCoeffParams, kDissCoeffControlParams, base];
  Return[retVal];
];
(* ============================================== *)
CrystDecayCoefficientValue[substAid_?IntegerQ] := Module[{retVal},
  retVal = RandomCoefficientValue[CrystDecayCoeffDistribution, CrystDecayCoeffParams, CrystDecayCoeffControlParams];
  Return[retVal];
];
(* ============================================== *)
Create$PairName[name1_, name2_] := (ToString[name1] <> DiastereomerSeparatorValue <> ToString[name2]);
Create$CrystName[name1_, name2_] := (DiastereomerSeparatorValue <> DiastereomerSeparatorValue <> DiastereomerSeparatorValue <> ToString[name1] <> DiastereomerSeparatorValue <> ToString[name2]);

Create$rMaxCoeffName[name_] := (CoeffPrefixValue <> "r" <> ToString[name] <> DiastereomerSeparatorValue <> "Max");
Create$kCrystCoeffName[name_] := (CoeffPrefixValue <> ToString[name] <> DiastereomerSeparatorValue <> "kCryst");
Create$kDissCoeffName[name_] := (CoeffPrefixValue <> ToString[name] <> DiastereomerSeparatorValue <> "kDiss");
Create$CrystDecayCoeffName[crystName_, substDecayName_, substLen_] := (CoeffPrefixValue <> ToString[crystName] <> ToLetter <> ToString[substLen] <> ToString[substDecayName]);

Create$rMaxName[name_] := ("r" <> DiastereomerSeparatorValue <> ToString[name] <> DiastereomerSeparatorValue <> "Max");
Create$kCrystName[name_] := ("kCryst" <> DiastereomerSeparatorValue <> ToString[name]);
Create$kDissName[name_] := ("kDiss" <> DiastereomerSeparatorValue <> ToString[name]);
(* ============================================== *)
GetSubstDecayID[] := idxY;
GetSubstDecayName[] := "Y";
(* ============================================== *)
GetPairSubstAndEnantIDs[substIdVal_?IntegerQ, subst1Id1Val_?IntegerQ, useSymmetricPairsVal_?BooleanQ] := Module[{substId, subst1Id, substIDlst, enantSubstIDlst, retVal},
  If[useSymmetricPairsVal,
    (
    (* We sort the substances in canonic ordering (by IDs) *)
      substIDlst = Sort[{substIdVal, subst1Id1Val}];
      enantSubstIDlst = Sort[{EnantiomerSubstanceID[substIdVal], EnantiomerSubstanceID[subst1Id1Val]}];
    ),
    (
    (* We DO NOT sort the substances in canonic ordering (by IDs) *)
      substIDlst = {substIdVal, subst1Id1Val};
      enantSubstIDlst = {EnantiomerSubstanceID[substIdVal], EnantiomerSubstanceID[subst1Id1Val]};
    )
  ];

  substId = substIDlst[[1]];
  subst1Id = substIDlst[[2]];

  retVal = Join[{substId, subst1Id}, enantSubstIDlst];

  Return[retVal];
];
(* ============================================== *)
GetPairSubstIDs[substIdVal_?IntegerQ, subst1Id1Val_?IntegerQ, useSymmetricPairsVal_?BooleanQ] := Module[{substId, subst1Id, enantSubstID, enantSubstID1},
  {substId, subst1Id, enantSubstID, enantSubstID1} = GetPairSubstAndEnantIDs[substIdVal, subst1Id1Val, useSymmetricPairsVal];
  Return[{substId, subst1Id}];
];
(* ============================================== *)
GetPairCoeffNames[substIdVal_?IntegerQ, subst1Id1Val_?IntegerQ, useSymmetricPairsVal_?BooleanQ] := Module[{substId, subst1Id, nameCoeff, name1Coeff, name, name1, baseSubstId, enantSubstID, enantSubstID1},
  {substId, subst1Id, enantSubstID, enantSubstID1} = GetPairSubstAndEnantIDs[substIdVal, subst1Id1Val, useSymmetricPairsVal];

  name = GetSubstanceName[substId];
  name1 = GetSubstanceName[subst1Id];

  If[substId != subst1Id,
    (
      If[substId <= enantSubstID,
        (
          nameCoeff = name;
          name1Coeff = name1;
        ),
        (
          nameCoeff = GetSubstanceName[enantSubstID];
          name1Coeff = GetSubstanceName[enantSubstID1];
        )
      ];
    ),
    (
      baseSubstId = Min[EnantiomerSubstanceID[substId], substId];
      nameCoeff = GetSubstanceName[baseSubstId];
      name1Coeff = nameCoeff;
    )
  ];

  Return[{nameCoeff, name1Coeff}];
];
(* ============================================== *)
AssignDiastereomericReactions[assignPairs_?BooleanQ, assignCryst_?BooleanQ, IsInverse_?BooleanQ, substIdVal_?IntegerQ, subst1Id1Val_?IntegerQ, multiplier_?IntegerQ, allocateCoeff_?BooleanQ] := Module[{substId, subst1Id, idxBval, substLen, reacName, substIdxName, coeffIdxName, coeffName, reacStringName, reacIdxName, substIDlst, name, name1, nameCoeff, name1Coeff, baseSubstId, reacCoeffName, retVal, idxCval, substCrystIdxName, reacCrystName, rIdxDiasttoCrystName, coeffIdxrMaxName, coeffIdxkCrystName, coeffIdxkDissName, rMaxName, kCrystName, kDissName, substDecayName, substDecayID, reacCrystCoeffName, rIdxCrystDecayName, coeffIdxCrystDecayName, coeffCrystDecayName, base, base1, enantSubstIDlst},
(* Print["AssignDiastereomericReactions::Starting..."]; *)

  {substId, subst1Id} = GetPairSubstIDs[substIdVal, subst1Id1Val, UseSymmetricPairsValue];

  name = GetSubstanceName[substId];
  name1 = GetSubstanceName[subst1Id];

  base = GetChainLength[substId];
  base1 = GetChainLength[subst1Id];
  substLen = base + base1;

  substDecayID = GetSubstDecayID[];
  substDecayName = GetSubstDecayName[];

  idxBval = "idxDiast" <> DiastereomerSeparatorValue;
  idxCval = "idxCryst" <> DiastereomerSeparatorValue;

  retVal = {};

  {nameCoeff, name1Coeff} = GetPairCoeffNames[substIdVal, subst1Id1Val, UseSymmetricPairsValue];

  reacName = Create$PairName[name, name1];
  substIdxName = idxBval <> reacName;
  reacCoeffName = Create$PairName[nameCoeff, name1Coeff];

  substCrystIdxName = idxCval <> reacName;
  reacCrystName = Create$CrystName[name, name1];
  reacCrystCoeffName = Create$CrystName[nameCoeff, name1Coeff];

  If[!IsInverse,
    (
    (* Direct *)
      If[assignPairs,
        (
        (* Pair formation *)
          coeffIdxName = CoeffPrefixValue <> "Idx" <> nameCoeff <> PlusLetter <> name1Coeff <> ToLetter <> reacCoeffName;
          coeffName = CoeffPrefixValue <> nameCoeff <> PlusLetter <> name1Coeff <> ToLetter <> reacCoeffName;
          reacStringName = name <> " + " <> name1 <> " -> " <> reacName;
          reacIdxName = ReactionPrefixValue <> name <> PlusLetter <> name1 <> ToLetter <> reacName;

          (* Print["substIdxName = ", substIdxName, ", reacName = ", reacName, ", coeffIdxName = ", coeffIdxName, ", coeffName = ", coeffName, ", reacStringName = ", reacStringName, ", reacIdxName = ", reacIdxName]; *)

          ToExpression[substIdxName <> "=AddPairSubstance[r" <> reacName <> ",Subscript[\[Rho],\"" <> reacName <> "\"],{{idxAtomZ," <> ToString[substLen] <> "}}]"];
        )
      ];

      If[assignCryst,
        (
        (* Crystallization & dissolution *)
          rIdxDiasttoCrystName = ReactionPrefixValue <> reacName <> ToLetter <> reacCrystName;

          coeffIdxrMaxName = Create$rMaxCoeffName[reacCoeffName];
          coeffIdxkCrystName = Create$kCrystCoeffName[reacCoeffName];
          coeffIdxkDissName = Create$kDissCoeffName[reacCoeffName];

          rMaxName = Create$rMaxName[reacCoeffName];
          kCrystName = Create$kCrystName[reacCoeffName];
          kDissName = Create$kDissName[reacCoeffName];


          (* Crystal full decay *)
          rIdxCrystDecayName = ReactionPrefixValue <> reacCrystName <> ToLetter <> substDecayName;
          coeffIdxCrystDecayName = CoeffPrefixValue <> "Idx" <> reacCrystCoeffName <> ToLetter <> ToString[substLen] <> substDecayName;
          coeffCrystDecayName = Create$CrystDecayCoeffName[reacCrystCoeffName, substDecayName, substLen];

          ToExpression[substCrystIdxName <> "=AddSolidSubstance[r" <> reacCrystName <> ",Subscript[\[Rho],\"" <> reacCrystName <> "\"],{{idxAtomZ," <> ToString[substLen] <> "}}]"];
        )
      ];

      retVal = {ToExpression[substIdxName], ToExpression[substCrystIdxName]};

      If[allocateCoeff,
        (
        (* Pair formation *)
          If[InitializePairFormationValue && assignPairs,
            (
              ToExpression[coeffIdxName <> "=AddCoeffName[" <> coeffName <> ",Subscript[k,\"" <> reacStringName <> "\"]]"];
              If[UseSamePairFormCoeffValue, ToExpression[coeffName <> "=" <> CapitalPiLetter <> PlusLetter]];
            )
          ];

          (* Crystallization & dissolution *)
          If[InitializeCrystallizationValue && assignCryst,
            (
              ToExpression[coeffIdxrMaxName <> "=AddCoeffName[" <> rMaxName <> ",Superscript[Subscript[\[Rho]," <> reacCoeffName <> "],max]]"];
              ToExpression[coeffIdxkCrystName <> "=AddCoeffName[" <> kCrystName <> ",Subscript[c," <> reacCoeffName <> "]]"];
              ToExpression[coeffIdxkDissName <> "=AddCoeffName[" <> kDissName <> ",Subscript[d," <> reacCoeffName <> "]]"];
            )];

          (* Crystal full decay *)
          If[InitializeCrystalDecayValue && assignCryst,
            (
              ToExpression[coeffIdxCrystDecayName <> "=AddCoeffName[" <> coeffCrystDecayName <> ",Subscript[k," <> reacCrystCoeffName <> " -> " <> ToString[substLen] <> " " <> substDecayName <> "]]"];
            )
          ];

          If[AssignBasicCrystCoefficientsValue,
            (
              If[InitializePairFormationValue && assignPairs,
                (
                (* Print["Assigning values to coefficients: ",coeffName, ", ",rMaxName, ", ",kCrystName, ", ",kDissName, ", ", coeffCrystDecayName]; *)
                  ToExpression[coeffName <> "=DiastCoefficientValue[" <> ToString[substId] <> ", " <> ToString[subst1Id] <> "]"];
                )
              ];
              If[InitializeCrystallizationValue && assignCryst,
                (
                  ToExpression[rMaxName <> "=rMaxCoefficientValue[" <> substIdxName <> ", " <> substCrystIdxName <> "]"];
                  ToExpression[kCrystName <> "=kCrystCoefficientValue[" <> substIdxName <> ", " <> substCrystIdxName <> "]"];
                  ToExpression[kDissName <> "=kDissCoefficientValue[" <> substIdxName <> ", " <> substCrystIdxName <> "]"];
                )
              ];

              If[InitializeCrystalDecayValue && assignCryst,
                (
                  ToExpression[coeffCrystDecayName <> "=CrystDecayCoefficientValue[" <> substCrystIdxName <> "]"];
                )];

            (* Print["Values of coefficients: ",coeffName, " = ",ToExpression[coeffName],", ",rMaxName, " = ",ToExpression[rMaxName], ", ",kCrystName, " = ",ToExpression[kCrystName], ", ",kDissName, " = ",ToExpression[kDissName], ", ", coeffCrystDecayName, " = ",ToExpression[coeffCrystDecayName]]; *)
            )
          ];
        )
      ];

      If[InitializePairFormationValue && assignPairs,
        (
          ToExpression[reacIdxName <> "=AddReaction[{{DiastFormReaction,\"" <> reacStringName <> "\"},{{" <> ToString[substId] <> ",1},{" <> ToString[subst1Id] <> ",1}},{" <> ToString[multiplier] <> coeffName <> ",1,1},{{" <> substIdxName <> ",1}}}]"];
        )
      ];

      If[InitializeCrystallizationValue && assignCryst,
        (
          ToExpression[rIdxDiasttoCrystName = "AddReaction[{{CrystallizeDissolveReaction,\"" <> reacName <> " -> " <> reacCrystName <> "\"}, {{" <> substIdxName <> ",1},{" <> substCrystIdxName <> ",0}},{" <> rMaxName <> "," <> kCrystName <> "," <> kDissName <> "},{{" <> substCrystIdxName <> ",1}}}]"];
        )
      ];

      If[InitializeCrystalDecayValue && assignCryst,
        (
          ToExpression[rIdxCrystDecayName <> "=AddReaction[{{CrystDecayReaction,\"" <> reacCrystName <> " -> " <> ToString[substLen] <> " " <> substDecayName <> "\"},{{" <> substCrystIdxName <> ",1}},{" <> coeffCrystDecayName <> ",1},{{" <> ToString[substDecayID] <> "," <> ToString[substLen] <> "}}}]"];
        )
      ];
    ),
    (
    (* Inverse - only for pair formation !!! *)
      If[InitializePairFormationValue && assignPairs,
        (
          coeffIdxName = CoeffPrefixValue <> "Idx" <> reacCoeffName <> ToLetter <> nameCoeff <> PlusLetter <> name1Coeff;
          coeffName = CoeffPrefixValue <> reacCoeffName <> ToLetter <> nameCoeff <> PlusLetter <> name1Coeff;
          reacStringName = reacName <> " -> " <> name <> " + " <> name1 ;
          reacIdxName = ReactionPrefixValue <> reacName <> ToLetter <> name <> PlusLetter <> name1;

          If[allocateCoeff,
            (
              ToExpression[coeffIdxName <> "=AddCoeffName[" <> coeffName <> ",Subscript[k,\"" <> reacStringName <> "\"]]"];
              If[UseSamePairFormCoeffValue, ToExpression[coeffName <> "=" <> CapitalPiLetter <> MinusLetter]];

              If[AssignBasicCrystCoefficientsValue,
                (
                (* Print["Assigning value to coefficient, ",coeffName]; *)
                  ToExpression[coeffName <> "=InvDiastCoefficientValue[" <> ToString[substId] <> ", " <> ToString[subst1Id] <> "]"];

                (* Print["Values of coefficients: ",coeffName, " = ",ToExpression[coeffName]]; *)
                )
              ];
            )
          ];

          ToExpression[reacIdxName <> "=AddReaction[{{InvDiastFormReaction,\"" <> reacStringName <> "\"},{{" <> substIdxName <> ",1}},{" <> coeffName <> ",1},{{" <> ToString[substId] <> ",1},{" <> ToString[subst1Id] <> ",1}}}]"];
        )
      ];
    )
  ];

  retVal = Join[{ToExpression[reacIdxName]}, retVal];
  Return[retVal];
];
(* ============================================== *)

(* ============================================== *)
InitializeCrystallizationReactions[rawOpts___] := Module[{opts, ii, enantLid, enantDid, idxCval, idxChainStart, idxChainEnd, idxChain, len, idxChain1, ii1, len1, reacAA, substPairAA, substCrystAA, reacaa, substPairaa, substCrystaa, reacAa, substPairAa, substCrystAa, Aid, Bid, enantAid, enantBid, reacaA, substPairaA, substCrystaA, idxChain1End},
  opts = ProcessOptions[rawOpts];
  InitializePairFormationValue = InitializePairFormation /. opts /. Options[CLMChains];
  InitializeCrystallizationValue = InitializeCrystallization /. opts /. Options[CLMChains];
  InitializeCrystalDecayValue = InitializeCrystalDecay /. opts /. Options[CLMChains];
  InitializeBasicCrystValue = InitializeBasicCryst /. opts /. Options[CLMChains];
  InitializeChainCrystValue = InitializeChainCryst /. opts /. Options[CLMChains];
  InitializeDiastereomerOnlyPairsValue = InitializeDiastereomerOnlyPairs /. opts /. Options[CLMChains];
  InitializeTwoSubstCrystValue = InitializeTwoSubstCryst /. opts /. Options[CLMChains];
  UseSamePairFormCoeffValue = UseSamePairFormCoeff /. opts /. Options[CLMChains];
  UseSymmetricPairsValue = UseSymmetricPairs /. opts /. Options[CLMChains];
  TwoSubstCrystValue = Flatten[{TwoSubstCryst /. opts /. Options[CLMChains]}];

  DiastereomerSeparatorValue = DiastereomerSeparator /. opts /. Options[CLMChains];
  AssignBasicCrystCoefficientsValue = AssignBasicCrystCoefficients /. opts /. Options[CLMChains];
  AssignChainCrystCoefficientsValue = AssignChainCrystCoefficients /. opts /. Options[CLMChains];

  If[InitializeBasicCrystValue && InitializeChainCrystValue,
    (
      If[!SilentRunValue, Print["InitializeCrystallizationReactions::Initializing basic and chain crystallization related reactions..."]];
      idxChainStart = 1;
      idxChainEnd = MaxChainLength;
    ),
    If[InitializeBasicCrystValue && !InitializeChainCrystValue,
      (
        If[!SilentRunValue, Print["InitializeCrystallizationReactions::Initializing basic crystallization related reactions..."]];
        idxChainStart = 1;
        idxChainEnd = 1;
      ),
      If[!InitializeBasicCrystValue && InitializeChainCrystValue,
        (
          If[!SilentRunValue, Print["InitializeCrystallizationReactions::Initializing chain crystallization related reactions..."]];
          idxChainStart = 2;
          idxChainEnd = MaxChainLength;
        ),
        (
          If[!SilentRunValue, Print["InitializeCrystallizationReactions::Not initializing crystallization related reactions..."]];
          Return[];
        )
      ]
    ]
  ];

  If[!SilentRunValue,
    (
      If[InitializePairFormationValue, Print["InitializeCrystallizationReactions::Initializing pair formation."], Print["InitializeCrystallizationReactions::NOT Initializing pair formation."]];

      If[InitializeCrystallizationValue, Print["InitializeCrystallizationReactions::Initializing crystallization."], Print["InitializeCrystallizationReactions::NOT Initializing crystallization."]];

      If[InitializeCrystalDecayValue, Print["InitializeCrystallizationReactions::Initializing crystal decay."], Print["InitializeCrystallizationReactions::NOT Initializing crystal decay."]];
    )
  ];

  If[InitializeTwoSubstCrystValue,
    (
      If[!SilentRunValue, Print["InitializeCrystallizationReactions::Initializing crystallization of specified pairs..."]];

      (* Assigning all possible pair formation but no crystallization *)
      Do[
        (
          len = Length[AllChainsTbl[[idxChain]]] / 2;
          idxChain1End = If[UseSymmetricPairsValue, idxChain, idxChainEnd];
          If[!SilentRunValue, Print["InitializeCrystallizationReactions::len = ", len]];
          If[!IntegerQ[len], Abort[]];

          Do[
            (
              Aid = GetSubstanceID[AllChainsTbl[[idxChain, ii]]];
              Do[
                (
                  len1 = If[UseSymmetricPairsValue, Min[ii, Length[AllChainsTbl[[idxChain1]]] / 2], Length[AllChainsTbl[[idxChain1]]] / 2];
                  If[!SilentRunValue, Print["InitializeCrystallizationReactions::len1 = ", len1]];
                  If[!IntegerQ[len1], Abort[]];

                  Do[
                    (
                      Bid = GetSubstanceID[AllChainsTbl[[idxChain1, ii1]]];

                      (* Print["InitializeCrystallizationReactions::Aid = ", Aid, ", Bid = ", Bid]; *)

                      AssignPairDiastereomericReactions[True, False, Aid, Bid];
                    ), {ii1, len1}
                  ];
                ), {idxChain1, idxChainStart, idxChain1End}
              ];
            ), {ii, len}
          ];
        ), {idxChain, idxChainStart, idxChainEnd}
      ];

      (* Assigning crystallization for specified pairs *)
      len = Length[TwoSubstCrystValue] / 2;
      If[!SilentRunValue, Print["InitializeCrystallizationReactions::len = ", len]];
      If[!IntegerQ[len], Abort[]];

      Do[
        (
          Aid = TwoSubstCrystValue[[2 * ii - 1]];
          Bid = TwoSubstCrystValue[[2 * ii]];
          AssignPairDiastereomericReactions[False, True, Aid, Bid];
        ), {ii, 1, len}
      ];
    ),
    (
      If[InitializeDiastereomerOnlyPairsValue,
        (
          If[!SilentRunValue, Print["InitializeCrystallizationReactions::Initializing crystallization of diastereomer pairs only..."]];

          Do[
            (
              len = Length[AllChainsTbl[[idxChain]]] / 2;
              If[!SilentRunValue, Print["InitializeCrystallizationReactions::len = ", len]];
              If[!IntegerQ[len], Abort[]];

              Do[
                (
                  enantLid = GetSubstanceID[AllChainsTbl[[idxChain, ii]]];
                  enantDid = EnantiomerSubstanceID[enantLid];

                  (* Print["InitializeCrystallizationReactions::enantLid = ", enantLid, ", enantDid = ", enantDid]; *)

                  AssignPairDiastereomericReactions[True, True, enantLid, enantLid];
                ), {ii, len}
              ];
            ), {idxChain, idxChainStart, idxChainEnd}
          ];
        ),
        (
          If[!SilentRunValue, Print["InitializeCrystallizationReactions::Initializing crystallization of all pairs..."]];

          Do[
            (
              len = Length[AllChainsTbl[[idxChain]]] / 2;
              If[!SilentRunValue, Print["InitializeCrystallizationReactions::len = ", len]];
              If[!IntegerQ[len], Abort[]];

              idxChain1End = If[UseSymmetricPairsValue, idxChain, idxChainEnd];
              Do[
                (
                  Aid = GetSubstanceID[AllChainsTbl[[idxChain, ii]]];
                  Do[
                    (
                      len1 = If[UseSymmetricPairsValue, Min[ii, Length[AllChainsTbl[[idxChain1]]] / 2], Length[AllChainsTbl[[idxChain1]]] / 2];
                      If[!SilentRunValue, Print["InitializeCrystallizationReactions::len1 = ", len1]];
                      If[!IntegerQ[len1], Abort[]];

                      Do[
                        (
                          Bid = GetSubstanceID[AllChainsTbl[[idxChain1, ii1]]];

                          (* Print["InitializeCrystallizationReactions::Aid = ", Aid, ", Bid = ", Bid]; *)

                          AssignPairDiastereomericReactions[True, True, Aid, Bid];
                        ), {ii1, len1}
                      ];
                    ), {idxChain1, idxChainStart, idxChain1End}
                  ];
                ), {ii, len}
              ];
            ), {idxChain, idxChainStart, idxChainEnd}
          ];
        )
      ];
    )
  ];
];
(* ============================================== *)
AssignPairDiastereomericReactions[assignPairs_?BooleanQ, assignCryst_?BooleanQ, substAid_?IntegerQ, substBid_?IntegerQ] := Module[{Aid, Bid, enantAid, enantBid, reacAB, substPairAB, substCrystAB, reacAb, substPairAb, substCrystAb, reacab, substPairab, substCrystab, reacaB, substPairaB, substCrystaB},
  Aid = Min[substAid, EnantiomerSubstanceID[substAid]];
  Bid = Min[substBid, EnantiomerSubstanceID[substBid]];

  enantAid = EnantiomerSubstanceID[Aid];
  enantBid = EnantiomerSubstanceID[Bid];

  (* Direct and reverse pair formation reactions like like A + B \[Rule] A$B *)
  {reacAB, substPairAB, substCrystAB} = AssignDiastereomericReactions[assignPairs, assignCryst, False, Aid, Bid, 1, True];
  AssignDiastereomericReactions[assignPairs, assignCryst, True, Aid, Bid, 1, True];

  (* Direct and reverse pair formation reactions like like A + b \[Rule] A$b *)
  {reacAb, substPairAb, substCrystAb} = AssignDiastereomericReactions[assignPairs, assignCryst, False, Aid, enantBid, 1, True];
  AssignDiastereomericReactions[assignPairs, assignCryst, True, Aid, enantBid, 1, True];

  (* Direct and reverse pair formation reactions like like a + b \[Rule] a$b *)
  {reacab, substPairab, substCrystab} = AssignDiastereomericReactions[assignPairs, assignCryst, False, enantAid, enantBid, 1, False];
  AssignDiastereomericReactions[assignPairs, assignCryst, True, enantAid, enantBid, 1, False];

  (* Direct and reverse pair formation reactions like like a + B \[Rule] a$B *)
  (* If A \[Equal] B, then aB *)
  {reacaB, substPairaB, substCrystaB} = AssignDiastereomericReactions[assignPairs, assignCryst, False, enantAid, Bid, 1, False];
  AssignDiastereomericReactions[assignPairs, assignCryst, True, enantAid, Bid, 1, False];

  (* Save IDs of pairs and crystals tied to the main substances for future use *)
  PairCrystInfo[Aid] = PairCrystInfo[Bid] = PairCrystInfo[enantAid] = PairCrystInfo[enantBid] = {substPairAB, substCrystAB, substPairab, substCrystab, substPairAb, substCrystAb, substPairaB, substCrystaB};
];
(* ============================================== *)
