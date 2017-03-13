(* ============================================== *)
(* :Summary: CLM activation / deactivation logic. *)
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
InitializeActivationValue = Indeterminate;
(* ============================================== *)
AssignActivationCoefficientsValue = Indeterminate;
(* ============================================== *)
ActivationCoeffDistribution = InverseGaussianDistribution;
ActivationCoeffParams = {1, 1};
ActivationCoeffControlParams = {}; // use default values
(* ============================================== *)
DeactivationCoeffDistribution = InverseGaussianDistribution;
DeactivationCoeffParams = {1, 1};
DeactivationCoeffControlParams = {}; // use default values
(* ============================================== *)
(* ============================================== *)
(* ============================================== *)
ActivationCoefficientValue[substAid_?IntegerQ] := Module[{retVal, base},
  base = GetChainLength[substAid];
  retVal = RandomCoefficientValue[ActivationCoeffDistribution, ActivationCoeffParams, ActivationCoeffControlParams, base];
  (* Print["ActivationCoefficientValue::substAid = ", substAid, ", base = ", base, ", retVal = ", retVal, ", ActivationCoeffDistribution = ", ActivationCoeffDistribution, ", ActivationCoeffParams = ", ActivationCoeffParams]; *)
  Return[retVal];
];
(* ============================================== *)
DeactivationCoefficientValue[substAid_?IntegerQ] := Module[{retVal, base},
  base = GetChainLength[substAid];
  retVal = RandomCoefficientValue[DeactivationCoeffDistribution, DeactivationCoeffParams, DeactivationCoeffControlParams, base];
  Return[retVal];
];
(* ============================================== *)
(* AssignActivationReaction returns ID of created activation or deactivation reaction *)
AssignActivationReaction[IsEnantiomer_?BooleanQ, IsInverse_?BooleanQ, substAid_?IntegerQ, substBid_?IntegerQ] := Module[{substIdxNameA, substIdxNameB, nameA, nameB, substAidVal, substBidVal, coeffIdxName, coeffName, reacStringName, reacIdxName, retVal},

  If[InitializeActivationValue,
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
          coeffIdxName = CoeffPrefixValue <> "Idx" <> GetSubstanceName[substAid] <> ToLetter <> GetSubstanceName[substBid];
          coeffName = CoeffPrefixValue <> GetSubstanceName[substAid] <> ToLetter <> GetSubstanceName[substBid];

          ToExpression[reacIdxName <> "=AddReaction[{{ActivationReaction,\"" <> reacStringName <> "\"},{{" <> substIdxNameA <> ",1}},{" <> coeffName <> ",1},{{" <> substIdxNameB <> ",1}}}]"];

          If[!IsEnantiomer,
            (
              ToExpression[coeffIdxName <> "=AddCoeffName[" <> coeffName <> ",Subscript[k,\"" <> reacStringName <> "\"]]"];

              If[AssignActivationCoefficientsValue, (ToExpression[coeffName <> "=ActivationCoefficientValue[" <> ToString[substAidVal] <> "]"];)];
            )
          ];
        ),
        (
        (* Inverse reaction *)
          reacStringName = nameB <> " -> " <> nameA;
          reacIdxName = ReactionPrefixValue <> nameB <> ToLetter <> nameA;
          coeffIdxName = CoeffPrefixValue <> "Idx" <> GetSubstanceName[substBid] <> ToLetter <> GetSubstanceName[substAid];
          coeffName = CoeffPrefixValue <> GetSubstanceName[substBid] <> ToLetter <> GetSubstanceName[substAid];

          ToExpression[reacIdxName <> "=AddReaction[{{DeactivationReaction,\"" <> reacStringName <> "\"},{{" <> substIdxNameB <> ",1}},{" <> coeffName <> ",1},{{" <> substIdxNameA <> ",1}}}]"];

          If[!IsEnantiomer,
            (
              ToExpression[coeffIdxName <> "=AddCoeffName[" <> coeffName <> ",Subscript[k,\"" <> reacStringName <> "\"]]"];

              If[AssignActivationCoefficientsValue, (ToExpression[coeffName <> "=DeactivationCoefficientValue[" <> ToString[substAidVal] <> "]"];)];
            )
          ];
        )
      ];
    )
  ];

  retVal = ToExpression[reacIdxName];
  Return[retVal];
];
(* ============================================== *)
(* Functions to initialize all activation / deactivation reactions *)
InitializeAllActivationReactions[rawOpts___] := Module[{opts, ii, substAid, substBid, substBName, substAName},
  If[!SilentRunValue, Print["InitializeAllSynthReactions::Starting."]];
  opts = ProcessOptions[rawOpts];

  If[InitializeActivationValue,
    (
      If[!SilentRunValue, Print[strSeparatorCRLF, "InitializeAllActivationReactions::Initializing activation / deactivation reactions."]];

      For[ii = 1, ii <= MaxEnantNo, ii++,
        (
          substAName = DigitArrayL[[ii]];
          substAid = GetSubstanceID[substAName];
          substBid = ToActivated[substAid];

          AssignActivationReaction[False, False, substAid, substBid];
          AssignActivationReaction[False, True, substAid, substBid];
          AssignActivationReaction[True, False, substAid, substBid];
          AssignActivationReaction[True, True, substAid, substBid];
        )
      ];
    ),
    (
      If[!SilentRunValue, Print[strSeparatorCRLF, "InitializeAllActivationReactions::NOT Initializing activation / deactivation reactions."]];
    )
  ];

  If[!SilentRunValue, Print["InitializeAllActivationReactions::Completed."]];
  Return[True];
];
(* ============================================== *)
(* ============================================== *)
