(* ============================================== *)
(* :Summary: CLM catalytic ligation logic. *)
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
InitializeCatLigationValue = Indeterminate;
AssignCatLigCoefficientsValue = Indeterminate;
(* ============================================== *)
NoCatLigAllCnt = Indeterminate;
(* ============================================== *)
UseCatLigEnantGroupingValue = Indeterminate;
(* ============================================== *)
IsCatLigAllInitialized = False;
(* ============================================== *)
CatLigC = 0;
CatLigR = 0;
CatLigMinLen = 0;
(* ============================================== *)
CatLigCoeffDistribution = ParetoDistribution;
CatLigCoeffParams = {1, 1};
(* ============================================== *)
InvCatLigCoeffDistribution = ParetoDistribution;
InvCatLigCoeffParams = {1, 1};
(* ============================================== *)
CreateCatLigReactionInfo[substAid_?IntegerQ, substBid_?IntegerQ, substABid_?IntegerQ, catalystSubstID_?IntegerQ] := Module[{reacInfo, catEnantSubstID, Aid, Bid, ABid, substID},
  catEnantSubstID = EnantiomerSubstanceID[catalystSubstID];

  (* We store reaction info only for minimum of two enantiomers for a catalyst *)
  (* The reaction is A+B+C \[Rule] AB+C and we DO NOT sort A and B *)
  If[catalystSubstID <= catEnantSubstID,
    (
      Aid = substAid;
      Bid = substBid;
      ABid = substABid;
      substID = catalystSubstID;
    ),
    (
      Aid = EnantiomerSubstanceID[substAid];
      Bid = EnantiomerSubstanceID[substBid];
      ABid = EnantiomerSubstanceID[substABid];
      substID = catEnantSubstID;
    )
  ];

  reacInfo = {Aid, Bid, ABid, substID};

  Return[reacInfo];
];
(* ============================================== *)
CatLigCoefficientValue[substAid_?IntegerQ, substBid_?IntegerQ, substABid_?IntegerQ, catalystSubstID_?IntegerQ, IsWrong_?BooleanQ] := Module[{retVal, base, groupDescr, reacDescr, reacDescrW, func, params},
  base = GetChainLength[catalystSubstID];

  (* retVal=RandomCoefficientValue[CatLigCoeffDistribution,CatLigCoeffParams,base]; *)

  groupDescr = If[UseCatLigEnantGroupingValue, CatLigGroupMatrix[catalystSubstID], Indeterminate];
  reacDescr = {substAid, substBid, substABid, catalystSubstID};
  reacDescrW = {EnantiomerSubstanceID[substAid], EnantiomerSubstanceID[substBid], EnantiomerSubstanceID[substABid], catalystSubstID};
  func = RandomCoefficientValue;
  params = {CatLigCoeffDistribution, CatLigCoeffParams, base};
  retVal = PairedCoefficientValue[reacDescr, reacDescrW, IsWrong, "CatLigGroupCoeff", groupDescr, func, params];
  Return[retVal];
];
(* ============================================== *)
InvCatLigCoefficientValue[substAid_?IntegerQ, substBid_?IntegerQ, substABid_?IntegerQ, catalystSubstID_?IntegerQ, IsWrong_?BooleanQ] := Module[{retVal, base, groupDescr, reacDescr, reacDescrW, func, params},
  base = GetChainLength[catalystSubstID];

  (* retVal=RandomCoefficientValue[InvCatLigCoeffDistribution,InvCatLigCoeffParams,base]; *)

  groupDescr = If[UseCatLigEnantGroupingValue, CatLigGroupMatrix[catalystSubstID], Indeterminate];
  reacDescr = {substAid, substBid, substABid, catalystSubstID};
  reacDescrW = {EnantiomerSubstanceID[substAid], EnantiomerSubstanceID[substBid], EnantiomerSubstanceID[substABid], catalystSubstID};
  func = RandomCoefficientValue;
  params = {InvCatLigCoeffDistribution, InvCatLigCoeffParams, base};
  retVal = PairedCoefficientValue[reacDescr, reacDescrW, IsWrong, "InvCatLigGroupCoeff", groupDescr, func, params];
  Return[retVal];
];
(* ============================================== *)
(* CatLigReactionRecordCheck returns true if there is a record for the reaction. *)
(* We need to be able to check if a given reaction IS catalyzed by a given catalyst *)(* without invoking full evaluation "test". *)
CatLigReactionRecordCheck[reacInfo_?VectorQ] := Module[{retVal, substID, Aid, Bid, ABid},
  retVal = False;
  substID = GetReactionCatalyst[reacInfo];
  If[BooleanQ[Apply[CatLigReacMatrix, reacInfo]], retVal = True];

  (*
Print["CatLigReactionRecordCheck::substID = ", substID, ", CatLigReacMatrix[",substID,"] = ", CatLigReacMatrix[substID], ", reacInfo = ", reacInfo, ", retVal = ", retVal];
*)

  Return[retVal];
];
(* ============================================== *)
(* CatLigReacMatrixAdd adds record of the reaction into CatLigReacMatrix *)
CatLigReacMatrixAdd[reacInfo_?VectorQ] := Module[{},
  ToExpression[ToString[Apply[CatLigReacMatrix, reacInfo]] <> "=True"];
];
(* ============================================== *)
(* Returns true if catalytic reaction: a + b + c = ab + c can exist *)
(* Uses substance IDs to simplify lookup of the data *)
CatLigReactionTest[reacInfo_?VectorQ] := Module[{retVal, substID, base},
  retVal = False;
  substID = GetReactionCatalyst[reacInfo];

  If[InitializeCatLigationValue,
    (
      base = GetChainLength[substID];

      If[base >= CatLigMinLen,
        (
          If[CatLigTest[substID],
            (

              If[CatLigReactionRecordCheck[reacInfo],
                (
                (* Reaction is already in the list *)
                  retVal = True;
                ),
                (
                  If[CatLigReactionTestBase[reacInfo],
                    (
                      CatLigReacMatrixAdd[reacInfo];
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
CatLigReactionTestRandom[reacInfo_?VectorQ] := RandomTest[CatLigR];
(* ============================================== *)
CatLigReactionTestBase = CatLigReactionTestRandom;
(* ============================================== *)
CatLigTestRandom[catalystSubstID_?IntegerQ] := RandomTest[CatLigC];
(* ============================================== *)
CatLigTestBase = CatLigTestRandom;
(* ============================================== *)
(* Returns True if a given substance is a catalyst *)
(* Uses substance ID to simplify lookup of the data *)
CatLigTest[catalystSubstID_?IntegerQ] := Module[{enantID, substID, retVal},
  enantID = EnantiomerSubstanceID[catalystSubstID];
  substID = Min[catalystSubstID, enantID];

  If[CatLigMatrix[substID],
    retVal = True,
    retVal = False,
    (
      CatLigMatrix[substID] = CatLigTestBase[substID];
      retVal = CatLigMatrix[substID];

      (* CatLigGroupMatrix contains the information if a given catalyst must be "grouped" *)
      (* If yes (True) or no (False) then the catalyst is either "good" or "bad" catalyst for ALL *)
      (* enantiomers, for which it is a catalyst. *)
      (*If the value is set to Indeterminate then each reaction is checked individually. *)
      (* We set the same value for catalyst and its enantiomer as it is possible that both could be used *)
      CatLigGroupMatrix[catalystSubstID] = If[UseCatLigEnantGroupingValue, RandomBool[], Indeterminate];
      CatLigGroupMatrix[enantID] = CatLigGroupMatrix[catalystSubstID];
    )
  ];

  (*
Print["CatLigTest::retVal = ", retVal, ", substID = ", substID, ", subst = ",SubstanceMatrix[substID], ", catalystSubstID = ", catalystSubstID, ", cat = ",SubstanceMatrix[catalystSubstID], ", enantID = ", enantID, ", enant = ",SubstanceMatrix[enantID]];
*)

  Return[retVal];
];
(* ============================================== *)
AssignCatLigReaction[IsEnantiomer_?BooleanQ, IsInverse_?BooleanQ, IsWrong_?BooleanQ, substAid_?IntegerQ, substBid_?IntegerQ, substABid_?IntegerQ, catSubstID_?IntegerQ, multC_, multCRev_, reacCountValue_] := Module[{substAidE, substBidE, substABidE, catSubstIDE, nameE, name1E, nameCE, reacNameE, reacStringName, reacIdxName, coeffIdxName, coeffName, nameC, substIdxNameC, name, name1, reacName, substIdxName, substIdxName1, substIdxNameD, retVal},

  name = GetSubstanceName[substAid];
  name1 = GetSubstanceName[substBid];
  nameC = GetSubstanceName[catSubstID];
  reacName = GetSubstanceName[substABid];

  If[!IsEnantiomer,
    (
      substIdxName = IndexPrefixValue <> name;
      substIdxName1 = IndexPrefixValue <> name1;
      substIdxNameD = IndexPrefixValue <> reacName;
      substIdxNameC = IndexPrefixValue <> nameC;
    ),
    (
    (* Enantiomers *)
      substAidE = EnantiomerSubstanceID[substAid];
      substBidE = EnantiomerSubstanceID[substBid];
      substABidE = EnantiomerSubstanceID[substABid];
      catSubstIDE = EnantiomerSubstanceID[catSubstID];

      nameE = GetSubstanceName[substAidE];
      name1E = GetSubstanceName[substBidE];
      nameCE = GetSubstanceName[catSubstIDE];
      reacNameE = GetSubstanceName[substABidE];

      substIdxName = IndexPrefixValue <> nameE;
      substIdxName1 = IndexPrefixValue <> name1E;
      substIdxNameD = IndexPrefixValue <> reacNameE;
      substIdxNameC = IndexPrefixValue <> nameCE;
    )
  ];

  If[!IsInverse,
    (
    (* Direct reaction *)
      coeffIdxName = CoeffPrefixValue <> "Idx" <> name <> PlusLetter <> name1 <> PlusLetter <> nameC <> ToLetter <> reacName <> PlusLetter <> nameC;
      coeffName = CoeffPrefixValue <> name <> PlusLetter <> name1 <> PlusLetter <> nameC <> ToLetter <> reacName <> PlusLetter <> nameC;

      If[!IsEnantiomer,
        (
          reacStringName = name <> " + " <> name1 <> " + " <> nameC <> " -> " <> reacName <> " + " <> nameC;
          reacIdxName = ReactionPrefixValue <> name <> PlusLetter <> name1 <> PlusLetter <> nameC <> ToLetter <> reacName <> PlusLetter <> nameC;

          ToExpression[coeffIdxName <> "=AddCoeffName[" <> coeffName <> ",Subscript[k,\"" <> reacStringName <> "\"]]"];

          (* We assign coefficients only once for a pair of enantiomers *)
          If[AssignCatLigCoefficientsValue,
            (
            (* Print["Assigning value to coefficient, ",coeffName]; *)
              ToExpression[coeffName <> "=CatLigCoefficientValue[" <> ToString[substAid] <> ", " <> ToString[substBid] <> ", " <> ToString[substABid] <> ", " <> ToString[catSubstID] <> ", " <> ToString[IsWrong] <> "]"];
            )
          ];
        ),
        (
          reacStringName = nameE <> " + " <> name1E <> " + " <> nameCE <> " -> " <> reacNameE <> " + " <> nameCE;
          reacIdxName = ReactionPrefixValue <> nameE <> PlusLetter <> name1E <> PlusLetter <> nameCE <> ToLetter <> reacNameE <> PlusLetter <> nameCE;
        )
      ];

      ToExpression[reacIdxName <> "=AddReaction[{{CatLigReaction,\"" <> reacStringName <> "\"},{{" <> substIdxName <> ",1},{" <> substIdxName1 <> ",1},{" <> substIdxNameC <> ",1}},{" <> ToString[multC * reacCountValue] <> coeffName <> ",1,1,1},{{" <> substIdxNameD <> ",1},{" <> substIdxNameC <> ",1}}}]"];
    ),
    (
    (* Inverse reaction *)
      coeffIdxName = CoeffPrefixValue <> "Idx" <> reacName <> PlusLetter <> nameC <> ToLetter <> name <> PlusLetter <> name1 <> PlusLetter <> nameC;
      coeffName = CoeffPrefixValue <> reacName <> PlusLetter <> nameC <> ToLetter <> name <> PlusLetter <> name1 <> PlusLetter <> nameC;

      If[!IsEnantiomer,
        (
          reacStringName = reacName <> " + " <> nameC <> " -> " <> name <> " + " <> name1 <> " + " <> nameC;
          reacIdxName = ReactionPrefixValue <> reacName <> PlusLetter <> nameC <> ToLetter <> name <> PlusLetter <> name1 <> PlusLetter <> nameC;

          ToExpression[coeffIdxName <> "=AddCoeffName[" <> coeffName <> ",Subscript[k,\"" <> reacStringName <> "\"]]"];

          (* We assign coefficients only once for a pair of enantiomers *)
          If[AssignCatLigCoefficientsValue,
            (
            (* Print["Assigning value to coefficient, ",coeffName]; *)
              ToExpression[coeffName <> "=InvCatLigCoefficientValue[" <> ToString[substAid] <> ", " <> ToString[substBid] <> ", " <> ToString[substABid] <> ", " <> ToString[catSubstID] <> ", " <> ToString[IsWrong] <> "]"];
            )
          ];
        ),
        (
          reacStringName = reacNameE <> " + " <> nameCE <> " -> " <> nameE <> " + " <> name1E <> " + " <> nameCE;
          reacIdxName = ReactionPrefixValue <> reacNameE <> PlusLetter <> nameCE <> ToLetter <> nameE <> PlusLetter <> name1E <> PlusLetter <> nameCE;
        )
      ];

      ToExpression[reacIdxName <> "=AddReaction[{{InvCatLigReaction,\"" <> reacStringName <> "\"},{{" <> substIdxNameD <> ",1},{" <> substIdxNameC <> ",1}},{" <> ToString[multCRev] <> coeffName <> ",1,1},{{" <> substIdxName <> ",1},{" <> substIdxName1 <> ",1},{" <> substIdxNameC <> ",1}}}]"];
    )
  ];

  retVal = ToExpression[reacIdxName];
  Return[retVal];
];
(* ============================================== *)
(* InitializeCatLigReactions initializes all catalytic ligation reactions *)
InitializeCatLigReactions[substAid_?IntegerQ, substBid_?IntegerQ, substABid_?IntegerQ, chainLenD_?IntegerQ] := InitializeCatLigReactions[substAid, substBid, substABid, chainLenD, 1, 1]

InitializeCatLigReactions[substAid_?IntegerQ, substBid_?IntegerQ, substABid_?IntegerQ, chainLenD_?IntegerQ, mult_, reacCountValue_] := Module[{chainLenCntC, lenC, iiC, catSubstID, reacInfo, multC, multCRev, substAidE, substBidE, substABidE, catSubstName, idxCat, multRev, reacList, reacInfoE},
  multRev = 1;

  (*
Print["InitializeCatLigReactions::InitializeCatLigationValue = ", InitializeCatLigationValue, ", NoCatLigAllCnt = ", NoCatLigAllCnt];
*)

  If[InitializeCatLigationValue,
    (
      Print["TODO::CLM_CatLigation::InitializeCatLigReactions::Check binomial coefficients for catalytic ligation."];

      reacList = If[AssignWrongCatLigReactionsValue, Table[0, {8}], Table[0, {4}]];

      For[idxCat = 1, idxCat <= NoCatLigAllCnt, idxCat++,
        (
          catSubstID = CatLigAllMatrix[idxCat];
          chainLenCntC = GetChainLength[catSubstID];
          reacInfo = CreateCatLigReactionInfo[substAid, substBid, substABid, catSubstID];

          (*
Print[strSeparatorCRLF];
Print["InitializeCatLigReactions::reacInfo = ", reacInfo];
*)

          If[!CatLigReactionRecordCheck[reacInfo],
            (
              If[CatLigReactionTest[reacInfo],
                (
                (*
Print["InitializeCatLigReactions::Assigning reactions."];
*)

                  multC = mult;
                  multCRev = If[chainLenCntC == chainLenD && substABid != catSubstID, 2 * multRev, multRev];

                  (* Direct reaction *)
                  reacList[[1]] = AssignCatLigReaction[False, False, False, substAid, substBid, substABid, catSubstID, multC, multCRev, reacCountValue];
                  (* Inverse reaction *)
                  reacList[[2]] = AssignCatLigReaction[False, True, False, ToDeactivated[substAid], ToDeactivated[substBid], substABid, catSubstID, multC, multCRev, reacCountValue];
                  (* Enantiomers - Direct reaction *)
                  reacList[[3]] = AssignCatLigReaction[True, False, False, substAid, substBid, substABid, catSubstID, multC, multCRev, reacCountValue];
                  (* Enantiomers - Inverse reaction *)
                  reacList[[4]] = AssignCatLigReaction[True, True, False, ToDeactivated[substAid], ToDeactivated[substBid], substABid, catSubstID, multC, multCRev, reacCountValue];

                  If[AssignWrongCatLigReactionsValue,
                    (
                    (* Assigning "wrong" catalytic reactions *)
                      substAidE = EnantiomerSubstanceID[substAid];
                      substBidE = EnantiomerSubstanceID[substBid];
                      substABidE = EnantiomerSubstanceID[substABid];

                      (* Adding record of reaction *)
                      reacInfoE = CreateCatLigReactionInfo[substAidE, substBidE, substABidE, catSubstID];
                      CatLigReacMatrixAdd[reacInfoE];

                      (*
Print["InitializeCatLigReactions::substAidE = ", substAidE, ", substBidE = ", substBidE, ", substABidE = ", substABidE, ", catSubstID = ", catSubstID, ", reacInfoE = ", reacInfoE];
*)

                      multCRev = If[chainLenCntC == chainLenD && substABidE != catSubstID, 2 * multRev, multRev];

                      reacList[[5]] = AssignCatLigReaction[False, False, False, substAidE, substBidE, substABidE, catSubstID, multC, multCRev, reacCountValue];
                      reacList[[6]] = AssignCatLigReaction[False, True, False, ToDeactivated[substAidE], ToDeactivated[substBidE], substABidE, catSubstID, multC, multCRev, reacCountValue];
                      reacList[[7]] = AssignCatLigReaction[True, False, False, substAidE, substBidE, substABidE, catSubstID, multC, multCRev, reacCountValue];
                      reacList[[8]] = AssignCatLigReaction[True, True, False, ToDeactivated[substAidE], ToDeactivated[substBidE], substABidE, catSubstID, multC, multCRev, reacCountValue];
                    )
                  ];

                  If[((GetChainNoOfD[substAid] + GetChainNoOfD[substBid] + GetChainNoOfD[catSubstID]) == 0) || ((GetChainNoOfL[substAid] + GetChainNoOfL[substBid] + GetChainNoOfL[catSubstID]) == 0),
                    (
                    (* Pure L catalytic ligation reaction. *)
                      LCatLigReactionCnt[chainLenD]++;
                      LCatLigCatalystCnt[chainLenCntC]++;

                      LCatLigReactionContent[chainLenD, LCatLigReactionCnt[chainLenD]] = reacList;
                      LCatLigCatalystContent[chainLenCntC, LCatLigCatalystCnt[chainLenCntC]] = reacList;
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
  Return[True];
];
(* ============================================== *)
AddCatLigAll[catSubstID_?IntegerQ] := Module[{chainInfo, numb, base},
  NoCatLigAllCnt++;
  CatLigAllMatrix[NoCatLigAllCnt] = catSubstID;
  Return[NoCatLigAllCnt];
];
(* ============================================== *)
InitializeCatLig[] := Module[{chainLenCntC, lenC, iiC, catSubstName, catSubstID, idxCat},
  If[!IsCatLigAllInitialized,
    (
      NoCatLigAllCnt = 0;

      For[chainLenCntC = 1, chainLenCntC <= MaxChainLength, chainLenCntC++,
        (
          lenC = Length[AllChainsTbl[[chainLenCntC]]];

          For[iiC = 1, iiC <= lenC, iiC++,
            (
              catSubstName = AllChainsTbl[[chainLenCntC, iiC]];
              catSubstID = GetSubstanceID[catSubstName];

              (*
enantID=EnantiomerSubstanceID[catalystSubstID];
substID=Min[catalystSubstID,enantID];
*)

              If[CatLigTest[catSubstID],
                (
                  AddCatLigAll[catSubstID];
                )
              ];
            )
          ];
        )
      ];
      If[!SilentRunValue, Print["InitializeCatLig::Total number of catalysts NoCatLigAllCnt = ", NoCatLigAllCnt]];
      IsCatLigAllInitialized = True;
    )
  ];
];
(* ============================================== *)

