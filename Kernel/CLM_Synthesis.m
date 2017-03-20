(* ============================================== *)
(* :Summary: CLM synthesis and catalytic synthesis logic. *)
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
NoCatSynthAllCnt = Indeterminate;
(* ============================================== *)
InitializeSynthesisValue = Indeterminate;
InitializeCatSynthesisValue = Indeterminate;
UseCatSynthEnantGroupingValue = Indeterminate;
(* ============================================== *)
AssignSynthCoefficientsValue = Indeterminate;
AssignCatSynthCoefficientsValue = Indeterminate;
(* ============================================== *)
AssignWrongCatSynthReactionsValue = Indeterminate;
AssignWrongCatLigReactionsValue = Indeterminate;
(* ============================================== *)
SynthCoeffDistribution = InverseGaussianDistribution;
SynthCoeffParams = {1, 1};
SynthCoeffControlParams = {}; // use default values
(* ============================================== *)
InvSynthCoeffDistribution = InverseGaussianDistribution;
InvSynthCoeffParams = {1, 1};
InvSynthCoeffControlParams = {}; // use default values
(* ============================================== *)
CatSynthC = 1;
CatSynthR = 0;
CatSynthMinLen = 0;
(* ============================================== *)
CatSynthCoeffDistribution = ParetoDistribution;
CatSynthCoeffParams = {1, 1};
CatSynthCoeffControlParams = {}; // use default values
(* ============================================== *)
InvCatSynthCoeffDistribution = ParetoDistribution;
InvCatSynthCoeffParams = {1, 1};
InvCatSynthCoeffControlParams = {}; // use default values
(* ============================================== *)
SynthDescriptorGetCoeff[descr_?VectorQ]:=descr[[1]];
InvSynthDescriptorGetCoeff[descr_?VectorQ]:=descr[[1]];
(* ============================================== *)
(* GetSynthDescriptor returns synthesis descriptor for a given substance. *)
GetSynthDescriptor[substAid_?IntegerQ] := Module[{Aid, descr, base, rndVal},
  Aid = Min[substAid, EnantiomerSubstanceID[substAid]];
  descr = SynthDescriptorFunc[Aid];

  If[!VectorQ[descr, NumericQ],
    (
    (* Generating new value. *)
      base = GetChainLength[Aid];
      rndVal = RandomCoefficientValue[SynthCoeffDistribution, SynthCoeffParams, SynthCoeffControlParams, base];
      descr = {rndVal};
      SynthDescriptorFunc[Aid] = descr;
    )
  ];

  Return[descr];
];
(* ============================================== *)
(* GetInvSynthDescriptor returns inverse synthesis descriptor for a given substance. *)
GetInvSynthDescriptor[substAid_?IntegerQ] := Module[{Aid, descr, base, rndVal},
  Aid = Min[substAid, EnantiomerSubstanceID[substAid]];
  descr = InvSynthDescriptorFunc[Aid];

  If[!VectorQ[descr, NumericQ],
    (
    (* Generating new value. *)
      base = GetChainLength[Aid];
      rndVal = RandomCoefficientValue[InvSynthCoeffDistribution, InvSynthCoeffParams, InvSynthCoeffControlParams, base];
      descr = {rndVal};
      InvSynthDescriptorFunc[Aid] = descr;
    )
  ];

  Return[descr];
];
(* ============================================== *)
SynthCoefficientValue[substAid_?IntegerQ, substBid_?IntegerQ] := Module[{retVal, descr},
  descr = GetSynthDescriptor[substBid];
  retVal = SynthDescriptorGetCoeff[descr];
  Return[retVal];
];
(* ============================================== *)
InvSynthCoefficientValue[substAid_?IntegerQ, substBid_?IntegerQ] := Module[{retVal, base},
  descr = GetInvSynthDescriptor[substBid];
  retVal = InvSynthDescriptorGetCoeff[descr];
  Return[retVal];
];
(* ============================================== *)
(* TODO :: CLM_Synthesis :: 20170319 :: SEED CatSynth / InvCatSynth then Gamma+ / Gamma-, then calculate all coefficients. *)
Print["TODO :: CLM_Synthesis :: 20170319 :: SEED CatSynth / InvCatSynth then Gamma+ / Gamma-, then calculate all coefficients."];
(* CatSynthCoefficientValue creates a value of catalytic synthesis coefficient *)
CatSynthCoefficientValue[substAid_?IntegerQ, substBid_?IntegerQ, catalystSubstID_?IntegerQ, IsWrong_?BooleanQ] := Module[{retVal, base, groupDescr, reacDescr, reacDescrW, func, params},
  base = GetChainLength[catalystSubstID];

  (* retVal=RandomCoefficientValue[CatSynthCoeffDistribution,CatSynthCoeffParams,CatSynthCoeffControlParams,base]; *)

  groupDescr = If[UseCatSynthEnantGroupingValue, CatSynthGroupMatrix[catalystSubstID], Indeterminate];
  reacDescr = {substAid, substBid, catalystSubstID};
  reacDescrW = {EnantiomerSubstanceID[substAid], EnantiomerSubstanceID[substBid], catalystSubstID};
  func = RandomCoefficientValue;
  params = {CatSynthCoeffDistribution, CatSynthCoeffParams, CatSynthCoeffControlParams, base};
  retVal = PairedCoefficientValue[reacDescr, reacDescrW, IsWrong, "CatSynthGroupCoeff", groupDescr, func, params];
  Return[retVal];
];
(* ============================================== *)
InvCatSynthCoefficientValue[substAid_?IntegerQ, substBid_?IntegerQ, catalystSubstID_?IntegerQ, IsWrong_?BooleanQ] := Module[{retVal, base, groupDescr, reacDescr, reacDescrW, func, params},
  base = GetChainLength[catalystSubstID];

  (* retVal=RandomCoefficientValue[InvCatSynthCoeffDistribution,InvCatSynthCoeffParams,InvCatSynthCoeffControlParams,base]; *)

  groupDescr = If[UseCatSynthEnantGroupingValue, CatSynthGroupMatrix[catalystSubstID], Indeterminate];
  reacDescr = {substAid, substBid, catalystSubstID};
  reacDescrW = {EnantiomerSubstanceID[substAid], EnantiomerSubstanceID[substBid], catalystSubstID};
  func = RandomCoefficientValue;
  params = {InvCatSynthCoeffDistribution, InvCatSynthCoeffParams, InvCatSynthCoeffControlParams, base};
  retVal = PairedCoefficientValue[reacDescr, reacDescrW, IsWrong, "InvCatSynthGroupCoeff", groupDescr, func, params];
  Return[retVal];
];
(* ============================================== *)
InitializeCatSynth[] := Module[{},
  NoCatSynthAllCnt = 0;
];
(* ============================================== *)
CatSynthReactionTestRandom[reacInfo_?VectorQ] := RandomTest[CatSynthR];
(* ============================================== *)
CatSynthReactionTestBase = CatSynthReactionTestRandom;
(* ============================================== *)
(* CatSynthReactionRecordCheck returns true if there is a record for the reaction *)
CatSynthReactionRecordCheck[reacInfo_?VectorQ] := Module[{retVal, substID},
  retVal = False;
  substID = GetReactionCatalyst[reacInfo];
  If[MemberQ[CatSynthReacMatrix[substID], {reacInfo}], (retVal = True;)];

  (* Print["CatSynthReactionRecordCheck[",reacInfo,"] = ",retVal]; *)

  Return[retVal];
];
(* ============================================== *)
(* CatSynthReacMatrixAdd adds record of the reaction into the CatSynthReacMatrix *)
CatSynthReacMatrixAdd[reacInfo_?VectorQ] := Module[{substID},
  substID = GetReactionCatalyst[reacInfo];

  If[Length[CatSynthReacMatrix[substID]] == 0,
    (
      CatSynthReacMatrix[substID] = {{reacInfo}};
    ),
    (
      CatSynthReacMatrix[substID] = Join[CatSynthReacMatrix[substID], {{reacInfo}}];
    )
  ];
];
(* ============================================== *)
(* Returns true if catalytic reaction: a + c = b + c can exist *)
CatSynthReactionTest[reacInfo_?VectorQ] := Module[{retVal, substID, base},
  retVal = False;
  substID = GetReactionCatalyst[reacInfo];

  If[InitializeCatSynthesisValue,
    (
      base = GetChainLength[substID];

      If[base >= CatSynthMinLen,
        (
          If[CatSynthTest[substID],
            (
            (* catalystSubstID is a catalyst. Now check if the reaction can be catalyzed by this catalyst *)
              If[CatSynthReactionRecordCheck[reacInfo],
                (
                (* Reaction is already in the list *)
                  retVal = True;
                ),
                (
                  If[CatSynthReactionTestBase[reacInfo],
                    (
                    (* Adding reaction to the list *)
                      CatSynthReacMatrixAdd[reacInfo];
                      retVal = True;
                    )
                  ];
                )
              ];
            )
          ];
        )
      ];
    )
  ];

  Return[retVal];
];
(* ============================================== *)
CatSynthTestRandom[catalystSubstID_?IntegerQ] := RandomTest[CatSynthC];
(* ============================================== *)
(* Different tests may be utilized by assigning to CatSynthTestBase *)
CatSynthTestBase = CatSynthTestRandom;
(* ============================================== *)
(* CatSynthTest checks if a given substance is a catalyst *)
CatSynthTest[catalystSubstID_?IntegerQ] := Module[{enantID, substID},
  enantID = EnantiomerSubstanceID[catalystSubstID];
  substID = Min[catalystSubstID, enantID];

  If[CatSynthMatrix[substID],
    Return[True],
    Return[False],
    (
      CatSynthMatrix[substID] = CatSynthTestBase[substID];
      (* CatSynthGroupMatrix contains the information if a given catalyst must be "grouped" *)
      (* If yes (True) or no (False) then the catalyst is either "good" or "bad" catalyst for ALL *)
      (* enantiomers, for which it is a catalyst. *)
      (*If the value is set to Indeterminate then each reaction is checked individually. *)
      (* We set the same value for catalyst and its enantiomer as it is possible that both could be used *)
      CatSynthGroupMatrix[catalystSubstID] = If[UseCatSynthEnantGroupingValue, RandomBool[], Indeterminate];
      CatSynthGroupMatrix[enantID] = CatSynthGroupMatrix[catalystSubstID];
      Return[CatSynthMatrix[substID]];
    )
  ];
];
(* ============================================== *)
(* AssignSynthReaction returns ID of created synthesis reaction *)
AssignSynthReaction[IsEnantiomer_?BooleanQ, IsInverse_?BooleanQ, substAid_?IntegerQ, substBid_?IntegerQ] := Module[{substIdxNameA, substIdxNameB, nameA, nameB, substAidVal, substBidVal, coeffIdxName, coeffName, reacStringName, reacIdxName, retVal},

  If[InitializeSynthesisValue,
    (
      If[!IsEnantiomer,
        (
          substAidVal = substAid;
          substBidVal = substBid;
        ),
        (
        (* Enantiomers *)
          substAidVal = EnantiomerSubstanceID[substAid];
          substBidVal = EnantiomerSubstanceID[substBid];
        )
      ];

      nameA = GetSubstanceName[substAidVal];
      nameB = GetSubstanceName[substBidVal];
      substIdxNameA = IndexPrefixValue <> nameA;
      substIdxNameB = IndexPrefixValue <> nameB;

      If[!IsInverse,
        (
        (* Direct reaction *)
          reacStringName = nameA <> " -> " <> nameB;
          reacIdxName = ReactionPrefixValue <> nameA <> ToLetter <> nameB;
          coeffIdxName = CoeffPrefixValue <> "Idx" <> nameA <> ToLetter <> nameB;
          coeffName = CoeffPrefixValue <> nameA <> ToLetter <> nameB;

          (* Print["AssignSynthReaction::reacIdxName = ", reacIdxName, ", reacStringName = ", reacStringName, ", substIdxNameA = ", substIdxNameA, ", coeffName = ", coeffName, ", substIdxNameB = ", substIdxNameB]; *)

          ToExpression[reacIdxName <> "=AddReaction[{{StandardReaction,\"" <> reacStringName <> "\"},{{" <> substIdxNameA <> ",1}},{" <> coeffName <> ",1},{{" <> substIdxNameB <> ",1}}}]"];

          ToExpression[coeffIdxName <> "=AddCoeffName[" <> coeffName <> ",Subscript[k,\"" <> reacStringName <> "\"]]"];

          If[AssignSynthCoefficientsValue, (ToExpression[coeffName <> "=SynthCoefficientValue[" <> ToString[substAidVal] <> ", " <> ToString[substBidVal] <> "]"];)];
        ),
        (
        (* Inverse reaction *)
          reacStringName = nameB <> " -> " <> nameA;
          reacIdxName = ReactionPrefixValue <> nameB <> ToLetter <> nameA;
          coeffIdxName = CoeffPrefixValue <> "Idx" <> nameB <> ToLetter <> nameA;
          coeffName = CoeffPrefixValue <> nameB <> ToLetter <> nameA;

          ToExpression[reacIdxName <> "=AddReaction[{{StandardReaction,\"" <> reacStringName <> "\"},{{" <> substIdxNameB <> ",1}},{" <> coeffName <> ",1},{{" <> substIdxNameA <> ",1}}}]"];

          ToExpression[coeffIdxName <> "=AddCoeffName[" <> coeffName <> ",Subscript[k,\"" <> reacStringName <> "\"]]"];

          If[AssignSynthCoefficientsValue, (ToExpression[coeffName <> "=InvSynthCoefficientValue[" <> ToString[substAidVal] <> ", " <> ToString[substBidVal] <> "]"];)];
        )
      ];
    )
  ];

  retVal = ToExpression[reacIdxName];
  Return[retVal];
];
(* ============================================== *)
(* Functions to initialize all synthesis reactions *)
InitializeAllSynthReactions[rawOpts___] := Module[{opts, ii, substAid, substBid, substBName},
  If[!SilentRunValue, Print["InitializeAllSynthReactions::Starting."]];
  opts = ProcessOptions[rawOpts];

  (* Print["InitializeAllSynthReactions::GetSubstanceID = ", Definition[GetSubstanceID]]; *)

  If[InitializeSynthesisValue,
    (
      If[!SilentRunValue, Print[strSeparatorCRLF, "InitializeAllSynthReactions::Direct Synthesis."]];

      substAid = idxY;

      For[ii = 1, ii <= MaxEnantNo, ii++,
        (
          substBName = DigitArrayL[[ii]];
          substBid = GetSubstanceID[substBName];

          (* Print["InitializeAllSynthReactions::substBName = ", substBName, ", substBid = ", substBid]; *)
          (* Print["InitializeAllSynthReactions::substAid = ", substAid, ", substBName = ", substBName, ", substBid = ", substBid]; *)

          AssignSynthReaction[False, False, substAid, substBid];
          AssignSynthReaction[False, True, substAid, substBid];
          AssignSynthReaction[True, False, substAid, substBid];
          AssignSynthReaction[True, True, substAid, substBid];

          InitializeCatSynthReactions[substAid, substBid];
        )
      ];
    ),
    (
      If[!SilentRunValue, Print["InitializeAllSynthReactions::NOT initializing direct synthesis..."]];
    )
  ];

  If[InitializeCatSynthesisValue,
    (
      If[!SilentRunValue,
        (
          Print["InitializeAllSynthReactions::NoCatSynthAllCnt = ", NoCatSynthAllCnt];
          Print["InitializeAllSynthReactions::LCatSynthReactionCnt = ", LCatSynthReactionCnt];
          Print["InitializeAllSynthReactions::LCatSynthCatalystCnt = ", Table[{ii, LCatSynthCatalystCnt[ii]}, {ii, 1, MaxChainLength}] // MatrixForm];
        )
      ];
    )
  ];

  If[!SilentRunValue, Print["InitializeAllSynthReactions::Completed."]];
  Return[True];
];
(* ============================================== *)
AssignCatSynthReaction[IsEnantiomer_?BooleanQ, IsInverse_?BooleanQ, IsWrong_?BooleanQ, substAid_?IntegerQ, substBid_?IntegerQ, catSubstID_?IntegerQ, multRev_] := Module[{opts, name, name1, reacStringName, reacIdxName, coeffIdxName, coeffName, substAidE, substBidE, catSubstIDE, nameE, name1E, reacNameE, substIdxName, substIdxName1, substIdxNameD, reacName, retVal},

  name = GetSubstanceName[substAid];
  name1 = GetSubstanceName[catSubstID];
  reacName = GetSubstanceName[substBid];

  If[!IsEnantiomer,
    (
      substIdxName = IndexPrefixValue <> name;
      substIdxName1 = IndexPrefixValue <> name1;
      substIdxNameD = IndexPrefixValue <> reacName;
    ),
    (
    (* Enantiomers *)
      substAidE = EnantiomerSubstanceID[substAid];
      substBidE = EnantiomerSubstanceID[substBid];
      catSubstIDE = EnantiomerSubstanceID[catSubstID];

      nameE = GetSubstanceName[substAidE];
      name1E = GetSubstanceName[catSubstIDE];
      reacNameE = GetSubstanceName[substBidE];

      substIdxName = IndexPrefixValue <> nameE;
      substIdxName1 = IndexPrefixValue <> name1E;
      substIdxNameD = IndexPrefixValue <> reacNameE;
    )
  ];

  If[!IsInverse,
    (
    (* Direct reaction *)
      coeffIdxName = CoeffPrefixValue <> "Idx" <> name <> PlusLetter <> name1 <> ToLetter <> reacName <> PlusLetter <> name1 ;
      coeffName = CoeffPrefixValue <> name <> PlusLetter <> name1 <> ToLetter <> reacName <> PlusLetter <> name1 ;

      If[!IsEnantiomer,
        (
          reacStringName = name <> " + " <> name1 <> " -> " <> reacName <> " + " <> name1;
          reacIdxName = ReactionPrefixValue <> name <> PlusLetter <> name1 <> ToLetter <> reacName <> PlusLetter <> name1 ;

          (* We assign coefficients only once for a pair of enantiomers *)
          ToExpression[coeffIdxName <> "=AddCoeffName[" <> coeffName <> ",Subscript[k,\"" <> reacStringName <> "\"]]"];

          If[AssignCatSynthCoefficientsValue,
            (
            (* Print["Assigning value to coefficient, ",coeffName]; *)
              ToExpression[coeffName <> "=CatSynthCoefficientValue[" <> ToString[substAid] <> ", " <> ToString[substBid] <> ", " <> ToString[catSubstID] <> ", " <> ToString[IsWrong] <> "]"];
            )
          ];
        ),
        (
          reacStringName = nameE <> " + " <> name1E <> " -> " <> reacNameE <> " + " <> name1E;
          reacIdxName = ReactionPrefixValue <> nameE <> PlusLetter <> name1E <> ToLetter <> reacNameE <> PlusLetter <> name1E;
        )
      ];

      ToExpression[reacIdxName <> "=AddReaction[{{CatSynthReaction,\"" <> reacStringName <> "\"},{{" <> substIdxName <> ",1},{" <> substIdxName1 <> ",1}},{" <> coeffName <> ",1,1},{{" <> substIdxNameD <> ",1},{" <> substIdxName1 <> ",1}}}]"];
    ),
    (
    (* Inverse reaction *)
      coeffIdxName = CoeffPrefixValue <> "Idx" <> reacName <> PlusLetter <> name1 <> ToLetter <> name <> PlusLetter <> name1;
      coeffName = CoeffPrefixValue <> reacName <> PlusLetter <> name1 <> ToLetter <> name <> PlusLetter <> name1;

      If[!IsEnantiomer,
        (
          reacStringName = reacName <> " + " <> name1 <> " -> " <> name <> " + " <> name1 ;
          reacIdxName = ReactionPrefixValue <> reacName <> PlusLetter <> name1 <> ToLetter <> name <> PlusLetter <> name1;

          (* We assign coefficients only once for a pair of enantiomers *)
          ToExpression[coeffIdxName <> "=AddCoeffName[" <> coeffName <> ",Subscript[k,\"" <> reacStringName <> "\"]]"];

          If[AssignCatSynthCoefficientsValue,
            (
            (* Print["Assigning value to coefficient, ",coeffName]; *)
              ToExpression[coeffName <> "=InvCatSynthCoefficientValue[" <> ToString[substAid] <> ", " <> ToString[substBid] <> ", " <> ToString[catSubstID] <> ", " <> ToString[IsWrong] <> "]"];
            )
          ];
        ),
        (
          reacStringName = reacNameE <> " + " <> name1E <> " -> " <> nameE <> " + " <> name1E ;
          reacIdxName = ReactionPrefixValue <> reacNameE <> PlusLetter <> name1E <> ToLetter <> nameE <> PlusLetter <> name1E;
        )
      ];

      ToExpression[reacIdxName <> "=AddReaction[{{InvCatSynthReaction,\"" <> reacStringName <> "\"},{{" <> substIdxNameD <> ",1},{" <> substIdxName1 <> ",1}},{" <> ToString[multRev] <> coeffName <> ",1,1},{{" <> substIdxName <> ",1},{" <> substIdxName1 <> ",1}}}]"];
    )
  ];

  retVal = ToExpression[reacIdxName];
  Return[retVal];
];
(* ============================================== *)

If[!SilentRunValue, Print["InitializeCatSynthReactions::TODO Check multRev=If[chainLenCnt==1 && ii!= 1,2,1,1] as it seems too fishy. "]];
InitializeCatSynthReactions[substAid_, substBid_] := Module[{chainLenCnt, len, ii, multRev, catSubstID, reacInfo, substBidE, catSubstName, substAidE, reacList, gammaPlusVal, gammaMinusVal, kp1, kp2, km1, km2, reacInfoE, gammaPlusTbl, gammaMinusTbl, gammaTbl},
  GammaCount = 0;

  If[InitializeCatSynthesisValue,
    (
      reacList = If[AssignWrongCatSynthReactionsValue, Table[0, {8}], Table[0, {4}]];

      For[chainLenCnt = 1, chainLenCnt <= MaxChainLength, chainLenCnt++,
        (
          len = Length[AllChainsTbl[[chainLenCnt]]];

          For[ii = 1, ii <= len, ii++,
            (
              catSubstName = AllChainsTbl[[chainLenCnt, ii]];
              catSubstID = GetSubstanceID[catSubstName];

              (* Print["    InitializeCatSynthReactions::ii = ", ii, ", substAid = ", substAid, ", substBid = ", substBid, ", catSubstID = ", catSubstID, ", CatSynthReactionRecordCheck[",substAid,", ",substBid,", ",catSubstID,"] = ", CatSynthReactionRecordCheck[substAid,substBid,catSubstID]]; *)

              reacInfo = CreateReactionInfo[substAid, substBid, catSubstID];

              (* Print["InitializeCatSynthReactions::substAid = ", substAid, ", substBid = ", substBid, ", catSubstID = ", catSubstID, ", reacInfo = ", reacInfo]; *)

              If[!CatSynthReactionRecordCheck[reacInfo],
                (
                  If[CatSynthReactionTest[reacInfo],
                    (
                      NoCatSynthAllCnt++;
                      multRev = If[chainLenCnt == 1 && ii != 1, 2, 1, 1];

                      (* Direct reaction *)
                      reacList[[1]] = AssignCatSynthReaction[False, False, False, substAid, substBid, catSubstID, multRev];
                      (* Inverse reaction *)
                      reacList[[2]] = AssignCatSynthReaction[False, True, False, substAid, substBid, catSubstID, multRev];
                      (* Enantiomers - Direct reaction *)
                      reacList[[3]] = AssignCatSynthReaction[True, False, False, substAid, substBid, catSubstID, multRev];
                      (* Enantiomers - Inverse reaction *)
                      reacList[[4]] = AssignCatSynthReaction[True, True, False, substAid, substBid, catSubstID, multRev];

                      If[AssignWrongCatSynthReactionsValue,
                        (
                        (* Assigning "wrong" catalytic synthesis reactions (e)A + C \[Rule] eB + C *)
                        (* A is not a chiral subatance thus eA \[Equal] A *)
                          substAidE = EnantiomerSubstanceID[substAid];
                          substBidE = EnantiomerSubstanceID[substBid];

                          (* Adding record of reaction *)
                          reacInfoE = CreateReactionInfo[substAidE, substBidE, catSubstID];
                          CatSynthReacMatrixAdd[reacInfoE];

                          (*
Print["InitializeCatSynthReactions::substAidE = ", substAidE, ", substBidE = ", substBidE, ", catSubstID = ", catSubstID, ", reacInfoE = ", reacInfoE];
*)

                          multRev = If[chainLenCnt == 1 && ii != 2, 2, 1, 1];
                          reacList[[5]] = AssignCatSynthReaction[False, False, True, substAidE, substBidE, catSubstID, multRev];
                          reacList[[6]] = AssignCatSynthReaction[False, True, True, substAidE, substBidE, catSubstID, multRev];
                          reacList[[7]] = AssignCatSynthReaction[True, False, True, substAidE, substBidE, catSubstID, multRev];
                          reacList[[8]] = AssignCatSynthReaction[True, True, True, substAidE, substBidE, catSubstID, multRev];

                          If[AssignCatSynthCoefficientsValue,
                            (
                            (* Assigning gamma values for convenience. *)
                            (* Print["InitializeCatSynthReactions::Assigning gamma values for convenience."]; *)

                              GammaCount++;
                              gammaPlusVal = 0;
                              gammaMinusVal = 0;

                              kp1 = GetParams[GetReactionInfo[reacList[[1]]]][[1]];
                              kp2 = GetParams[GetReactionInfo[reacList[[5]]]][[1]];
                              km1 = GetParams[GetReactionInfo[reacList[[2]]]][[1]];
                              km2 = GetParams[GetReactionInfo[reacList[[6]]]][[1]];

                              GammaPlus[GammaCount] = {catSubstName, reacList[[1]], reacList[[5]], (kp1 - kp2) / (kp1 + kp2), kp1, kp2};
                              GammaMinus[GammaCount] = {catSubstName, reacList[[2]], reacList[[6]], (km2 - km1) / (km2 + km1), km1, km2};

                            (*
Print["InitializeCatSynthReactions::reacList = ", reacList // MatrixForm];
Print["InitializeCatSynthReactions::kp1 = ", kp1, ", kp2 = ", kp2, ", km1 = ", km1, ", km2 = ", km2];
Print["InitializeCatSynthReactions::GammaPlus[",GammaCount,"] = ",GammaPlus[GammaCount]];
Print["InitializeCatSynthReactions::GammaMinus[",GammaCount,"] = ", GammaMinus[GammaCount]];
Print["InitializeCatSynthReactions::Gamma[",GammaCount,"] = ", (GammaPlus[GammaCount]+GammaMinus[GammaCount])];
Print[strSeparatorCRLF];
*)
                            )
                          ];
                        )
                      ];

                      If[((GetChainNoOfD[substAid] + GetChainNoOfD[substBid] + GetChainNoOfD[catSubstID]) == 0) || ((GetChainNoOfL[substAid] + GetChainNoOfL[substBid] + GetChainNoOfL[catSubstID]) == 0),
                        (
                        (* Pure L catalytic synthesis reaction. *)
                          LCatSynthReactionCnt++;
                          LCatSynthCatalystCnt[chainLenCnt]++;

                          LCatSynthReactionContent[LCatSynthReactionCnt] = reacList;
                          LCatSynthCatalystContent[chainLenCnt, LCatSynthCatalystCnt[chainLenCnt]] = reacList;
                        )
                      ];
                    )
                  ];
                )
              ];
            )
          ];
        )
      ];
      If[PrintGammaValue,
        (
          gammaPlusTbl = Table[Join[{ii}, GammaPlus[ii]], {ii, 1, GammaCount}];
          gammaMinusTbl = Table[Join[{ii}, GammaMinus[ii]], {ii, 1, GammaCount}];
          gammaTbl = Table[{ii, (GammaGetGammaVal[GammaPlus[ii]] + GammaGetGammaVal[GammaMinus[ii]])}, {ii, 1, GammaCount}];

          Print["\[Gamma]+ = ", gammaPlusTbl // MatrixForm, ", \[Gamma]- = ", gammaMinusTbl // MatrixForm, ", \[Gamma] = ", gammaTbl // MatrixForm];
        )
      ];
    )
  ];

  Return[True];
];
(* ============================================== *)
InitSynthCoeffValues[maxEnantNumb_?IntegerQ, rawOptions___] := Module[{opts},
  opts = ProcessOptions[rawOptions];

  If[InitializeSynthesisValue,
    (
      If[!SilentRunValue, Print["InitSynthCoeffValues::Assigning synthesis coefficients."]];

      If[maxEnantNumb > 6,
        (
          Print["InitSynthCoeffValues::Value of maxEnantNumb = ", maxEnantNumb, " exceeds 6. Only the first six will be assigned predetermined values."];
        )
      ];

      coeffYtoA = coeffYtoAValue;
      coeffYtoB = coeffYtoBValue;
      coeffYtoa = coeffYtoAValue;
      coeffYtob = coeffYtoBValue;

      coeffAtoY = coeffStoYValue * (1 - dCoeffStoYValue);
      coeffBtoY = coeffStoYValue * (1 - dCoeffStoYValue);
      coeffatoY = coeffStoYValue * (1 + dCoeffStoYValue);
      coeffbtoY = coeffStoYValue * (1 + dCoeffStoYValue);

      coeffYtoC = coeffYtoCValue;
      coeffYtoD = coeffYtoDValue;
      coeffYtoc = coeffYtoCValue;
      coeffYtod = coeffYtoDValue;

      coeffCtoY = coeffStoYValue * (1 - dCoeffStoYValue);
      coeffDtoY = coeffStoYValue * (1 - dCoeffStoYValue);
      coeffctoY = coeffStoYValue * (1 + dCoeffStoYValue);
      coeffdtoY = coeffStoYValue * (1 + dCoeffStoYValue);

      coeffYtoE = coeffYtoEValue;
      coeffYtoF = coeffYtoFValue;
      coeffYtoe = coeffYtoEValue;
      coeffYtof = coeffYtoFValue;

      coeffEtoY = coeffStoYValue * (1 - dCoeffStoYValue);
      coeffFtoY = coeffStoYValue * (1 - dCoeffStoYValue);
      coeffetoY = coeffStoYValue * (1 + dCoeffStoYValue);
      coeffftoY = coeffStoYValue * (1 + dCoeffStoYValue);
    ),
    (
      If[!SilentRunValue, Print["InitSynthCoeffValues::NOT assigning synthesis coefficients."]];
    )
  ];
];
(* ============================================== *)
(* ============================================== *)