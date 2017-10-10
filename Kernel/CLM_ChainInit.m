(* ============================================== *)
(* :Summary: CLM chain init logic. *)
(* ============================================== *)
(* :Author: Konstantin K. Konstantinov *)
(* :Email: konstantin.k.konstantinov@gmail.com *)
(* :License type: GPL v3 or any later version, see http://www.gnu.org/licenses/ *)
(* :Copyright: K^3, 2013 - 2017 *)
(* :Version: 3.26.001, Date : 2017/10/09 *)
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
InitializeCoefficientParameters[rawOpts___] := Module[{opts},
  opts = ProcessOptions[rawOpts];
  AssignSynthCoefficientsValue = AssignSynthCoefficients /. opts /. Options[CLMChains];
  AssignCatSynthCoefficientsValue = AssignCatSynthCoefficients /. opts /. Options[CLMChains];
  PrintGammaValue = PrintGamma /. opts /. Options[CLMChains];
  AssignWrongCatSynthReactionsValue = AssignWrongCatSynthReactions /. opts /. Options[CLMChains];
  AssignLigCoefficientsValue = AssignLigCoefficients /. opts /. Options[CLMChains];
  AssignCatLigCoefficientsValue = AssignCatLigCoefficients /. opts /. Options[CLMChains];
  AssignWrongCatLigReactionsValue = AssignWrongCatLigReactions /. opts /. Options[CLMChains];
  AssignEpimCoefficientsValue = AssignEpimCoefficients /. opts /. Options[CLMChains];
  AssignActivationCoefficientsValue = AssignActivationCoefficients /. opts /. Options[CLMChains];
];
(* ============================================== *)
InitializeChainBase[maxChainLen_?IntegerQ, maxEnantNumb_?IntegerQ, rawOpts___] := Module[{opts, useIdenticalEnantiomersVal},
  If[!SilentRunValue, Print["InitializeChainBase::Initializing..."]];

  If[maxEnantNumb > MaxEnantNoLimit || maxEnantNumb < 1,
    (
      Print["InitializeChains::maxEnantNumb value of ", maxEnantNumb, " is invalid. Maximum supported value MaxEnantNoLimit = ", MaxEnantNoLimit];
      Return[False];
    )
  ];

  MaxChainLength = maxChainLen;
  MaxEnantNo = maxEnantNumb;

  opts = ProcessOptions[rawOpts];

  InitializeSynthesisValue = InitializeSynthesis /. opts /. Options[CLMChains];
  InitializeCatSynthesisValue = InitializeCatSynthesis /. opts /. Options[CLMChains];
  UseCatSynthEnantGroupingValue = UseCatSynthEnantGrouping /. opts /. Options[CLMChains];
  InitializeLigationValue = InitializeLigation /. opts /. Options[CLMChains];
  InitializeCatLigationValue = InitializeCatLigation /. opts /. Options[CLMChains];
  UseCatLigEnantGroupingValue = UseCatLigEnantGrouping /. opts /. Options[CLMChains];
  InitializeEpimerizationValue = InitializeEpimerization /. opts /. Options[CLMChains];
  InitializeActivationValue = InitializeActivation /. opts /. Options[CLMChains];

  IndexPrefixValue = IndexPrefix /. opts /. Options[CLMChains];
  ReactionPrefixValue = ReactionPrefix /. opts /. Options[CLMChains];
  CoeffPrefixValue = CoeffPrefix /. opts /. Options[CLMChains];
  useIdenticalEnantiomersVal = UseIdenticalEnantiomers /. opts /. Options[CLMChains];

  CreateDigits[useIdenticalEnantiomersVal];
  CreateEnantRule[];
  InitializeCoefficientParameters[rawOpts];

  If[!SilentRunValue, Print[strSeparatorCRLF, "Atoms."]];
  idxAtomZ = AddAtom[aZ, Superscript[Subscript[\[Rho], Z], total]];

  (* Substances *)
  If[!SilentRunValue, Print[strSeparatorCRLF, "Basic substances."]];

  (* We don't count Z substance, so mathematically it does not have any atoms in it. *)
  idxZ = AddSubstance[rZ, Subscript[\[Rho], Z], {{idxAtomZ, 0}}];
  idxY = AddSubstance[rY, Subscript[\[Rho], Y], {{idxAtomZ, 1}}];

  If[UseAllSubstForCrystValue,
    (
    (* Adding aggregate substance for crystallization *)
      idxX = AddSubstance[rX, Subscript[\[Rho], X], {{idxAtomZ, 0}}, SubstanceTypeSum];
      IdxRoWCryst = idxX;
      AddSumReaction[idxX, AllSubstForCrystFuncPointer[[AllSubstForCrystTypeValue]]];
    )
  ];

  MaxNonChiralSubstNo = NoSubstCnt;

  If[!SilentRunValue, Print[strSeparatorCRLF, "Chains."]];
  AllChainsTbl = Table[Indeterminate, {jj, 1, maxChainLen}];
  AllChainsSubstMap = Table[Indeterminate, {jj, 1, maxChainLen}];
];
(* ============================================== *)
ResetChainAll[] := Module[{},
  Clear[CatLigMatrix, CatSynthMatrix, CatLigReacMatrix, CatSynthReacMatrix, LigReacMatrix, LLigReactionCnt, LCatSynthCatalystCnt, LCatLigReactionCnt, LCatLigCatalystCnt, CatLigReacMatrix, CatLigMatrix, CatLigGroupMatrix, CatLigAllMatrix, LCatLigReactionCnt, LCatLigReactionContent, EpimDescriptorFunc, EpimReacMatrix, LigDescriptorFunc, InvLigDescriptorFunc, LigReacMatrix, PairCrystInfo];

  NoCatLigAllCnt = 0;
];
(* ============================================== *)
(* Main function to initialize chains and all reactions. *)
If[!SilentRunValue, Print["CLM_ChainHlp::InitializeChains::TODO::Possibly update substIDlst to indexers..."]];
InitializeChains[maxChainLen_?IntegerQ, maxEnantNumb_?IntegerQ, rawOpts___] := Module[{chainLenCnt, nameLst, name, nameRev, substIDlst, jj, substIdxName},
  ResetChainAll[];

  InitializeChainBase[maxChainLen, maxEnantNumb, rawOpts];

  For[chainLenCnt = 1, chainLenCnt <= MaxChainLength, chainLenCnt++,
    (
      nameLst = {};
      substIDlst = {};
      If[!SilentRunValue, Print["Initializing names for chain length = ", chainLenCnt]];
      For[jj = 0, jj < (2 * MaxEnantNo)^chainLenCnt, jj++,
        (
          name = CreateChainName[jj, chainLenCnt];

          If[!MemberQ[nameLst, name],
            (
              substIdxName = IndexPrefixValue <> name;
              ToExpression[substIdxName <> "=AddEnantiomerSubstance[r" <> name <> ",Subscript[\[Rho],\"" <> name <> "\"],{{idxAtomZ," <> ToString[chainLenCnt] <> "}}]"];
              nameLst = Join[nameLst, {name}];
              substIDlst = Join[substIDlst, {ToExpression[substIdxName]}];

              If[InitializeActivationValue && chainLenCnt <= MaxActivationLength,
                (
                  substIdxName = IndexPrefixValue <> name <> ActivationLetter;
                  name = ToActivated[name];
                  ToExpression[substIdxName <> "=AddEnantiomerSubstance[r" <> name <> ",Subscript[\[Rho],\"" <> name <> "\"],{{idxAtomZ," <> ToString[chainLenCnt] <> "}}]"];
                )
              ];
            )
          ];
        )
      ];

      AllChainsTbl[[chainLenCnt]] = nameLst;
      AllChainsSubstMap[[chainLenCnt]] = substIDlst;
      NoSimpleSubstCnt = NoSubstCnt;
      If[!SilentRunValue, Print["InitializeChains::NoSubstCnt = ", NoSubstCnt]];
    )
  ];

  InitializeReactions[rawOpts];

  Return[True];
];
(* ============================================== *)
InitializeReactionTests[rawOpts___] := Module[{opts, ii},
  opts = ProcessOptions[rawOpts];

  If[NumericQ[SeedRandomValue],
    (
      If[!SilentRunValue, Print["InitializeReactionTests::Using random seed value = ", SeedRandomValue]];
      SeedRandom[SeedRandomValue];
    ),
    (
      If[!SilentRunValue, Print["InitializeReactionTests::Not using random seed value."]];
    )
  ];

  Do[
    (
      CatLigMatrix[ii] = Undefined;
      CatSynthMatrix[ii] = Undefined;
      CatLigReacMatrix[ii] = Undefined;
      CatSynthReacMatrix[ii] = Undefined;

      LigReacMatrix[ii] = Undefined;

      (* The indexers below are for simplifying further analysis. *)
      (* Number of "pure L" ligation, catalytic synthesis, and catalytic ligation reactions at each level. *)
      (* For ligation reaction the level is defined by the reaction outcome. *)
      (* For catalytic reactions the number of outcomes <xxx>Reaction *)
      (* and the number of catalysts <xxx>Catalyst are defined. *)
      LLigReactionCnt[ii] = 0;
      LCatSynthCatalystCnt[ii] = 0;
      LCatLigReactionCnt[ii] = 0;
      LCatLigCatalystCnt[ii] = 0;
    ),
    {ii, 1, NoSubstCnt}
  ];

  LCatSynthReactionCnt = 0;

  InitializeCatSynth[];

  IsCatLigAllInitialized = False;
  InitializeCatLig[];
];
(* ============================================== *)
InitializeReactions[rawOpts___] := Module[{},
  InitializeReactionTests[rawOpts];
  InitializeAllSynthReactions[rawOpts];
  InitializeAllActivationReactions[rawOpts];
  InitializeAllLigationReactions[rawOpts];
  InitializeCrystallizationReactions[rawOpts];
  InitializeDirectCrystReactions[rawOpts];
  InitializeAllEpimerizationReactions[rawOpts];

  If[!SilentRunValue,
    (
      Print["InitializeReactions::Final LCatSynthReactionCnt = ", LCatSynthReactionCnt];

      Print["InitializeReactions::Final LCatSynthCatalystCnt = ", Table[{ii, LCatSynthCatalystCnt[ii]}, {ii, 1, MaxChainLength}] // MatrixForm];

      Print["InitializeReactions::Final LLigReactionCnt = ", Table[{ii, LLigReactionCnt[ii]}, {ii, 1, MaxChainLength}] // MatrixForm];

      Print["InitializeReactions::Final LCatLigReactionCnt = ", Table[{ii, LCatLigReactionCnt[ii]}, {ii, 1, MaxChainLength}] // MatrixForm];

      Print["InitializeReactions::Final LCatLigCatalystCnt = ", Table[{ii, LCatLigCatalystCnt[ii]}, {ii, 1, MaxChainLength}] // MatrixForm];
    )
  ];

  Return[True];
];
(* ============================================== *)
