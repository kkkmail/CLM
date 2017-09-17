(* ============================================== *)
(* :Summary: CLM generator common module. *)
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
(* EnantSelNoReaction - No reaction (bogh Gamma and gamma \[Equal] 0) *)
(* EnantSelNone - No enantioselectivity (gamma = 0, but Gamma can take the values in the range [0, \[CapitalOmega]] ) *)
(* EnantSelPlus - gamma has range [0, \[CapitalDelta]\[Gamma]] *)
(* EnantSelMinus - gamma has range [-\[CapitalDelta]\[Gamma], 0]  *)
(* EnantSelAll - gamma has range [-\[CapitalDelta]\[Gamma], \[CapitalDelta]\[Gamma]] *)
(* EnantSelPlusOne - gamma = 1*)
(* EnantSelMinusOne - gamma = -1 *)
(* ============================================== *)
EnantSelNoReaction = -1;
EnantSelNone = 0;
EnantSelPlus = 1;
EnantSelMinus = 2;
EnantSelAll = 3;
EnantSelPlusOne = 4;
EnantSelMinusOne = 5;
(* ============================================== *)
EnantSelToString[eSel_?IntegerQ] := Module[{s},
  s = "x";

  If[eSel == EnantSelNoReaction, s = "z"];
  If[eSel == EnantSelNone, s = "n"];
  If[eSel == EnantSelPlus, s = "p"];
  If[eSel == EnantSelMinus, s = "m"];
  If[eSel == EnantSelAll, s = "a"];
  If[eSel == EnantSelPlusOne, s = "m1"];
  If[eSel == EnantSelMinusOne, s = "p1"];

  Return[s];
];
(* ============================================== *)
EnantSelMin = -1;
EnantSelMax = 5;
(* ============================================== *)
(* ============================================== *)
VarTypeEpsilon = 1;
VarTypeEpsilonRange = 2;
VarTypeNormalRange = 3;
(* ============================================== *)
VarTypeMin = 1;
VarTypeMax = 3;
(* ============================================== *)
SynthVarType = VarTypeEpsilonRange;
InvSynthVarType = VarTypeEpsilonRange;
(* ============================================== *)
(* Set to True to impose fixed \[CapitalSigma] value instead of range *)
UseSigmaFixed = False;
SigmaFixedValue = \[Mu] * \[CapitalOmega];
(* ============================================== *)
UseMinusOne$sigma = False;
(* ============================================== *)
UseRho0Fixed = False;
Rho0FixedValue = \[Rho]00;
(* ============================================== *)
AlmostInfinity = 10^10;
(* ============================================== *)
RunAllCoeffsShowNSolveProgress = True;
RunAllCoeffsPrecision = 100;
RunAllCoeffsNSolveWorkingPrecision = 50;
RunAllCoeffsUseNSolve = True;
(* ============================================== *)
GenerateEqCheck = False;
UseParallelTable = True;
UseParallelTableForEvolution = False;
(* ============================================== *)
BifurcationRunNDSolve = False;
BifurcationPrintStepInfo = True;
BifurcationRunFindMaximum = False;
PrintFrequencyValue = 1000;
NMaximizeMaxIterations = 200;
(* NMaximizeMethod="\"NelderMead\""; *)
NMaximizeMethod = "";
(* ============================================== *)
(* ============================================== *)
(* Parameters to control catalytic synthesis *)
CatSynthEnantSel = Indeterminate;
InvCatSynthEnantSel = Indeterminate;
(* ============================================== *)
(* ============================================== *)
(* Parameters to control activation (if activation is used) *)
UseFixedInvActivationEps = False;
(* ============================================== *)
(* ============================================== *)
(* Parameters to control epimerization *)
UseSameEpimerizationForAll = Indeterminate;
EpimEnantSel = Indeterminate;
(* ============================================== *)
(* ============================================== *)
(* Parameters to control ligation *)
UseSameLigationForAll = Indeterminate;

(* UseSTermLigation is applicable only to models WITHOUT activation. *)
(* If True, then reactions A + P \[Rule] AP and P + A \[Rule] PA are allowed. P is a chain. *)
(* Default value is set to False to ensure compatibility with eariler version. *)
UseSTermLigation = False;

(* If True, then reactions AP \[Rule] A + P and PA \[Rule] P + A are allowed. P is a chain *)
(* Default value is set to False to ensure compatibility with eariler version. *)
UseSTermInvLigation = False;
LigationEnantSel = Indeterminate;
InvLigationEnantSel = Indeterminate;
(* ============================================== *)
(* ============================================== *)
(* Pair formation *)
InitPairFormValGen = False;
UseSamePairFormCoeffValGen = False;
UseSymmetricPairsValGen = False;
(* ============================================== *)
(* Parameters of regular crystallization *)
InitCrystValGen = False;
InitCrystDecValGen = False;
InitBasicCrystValGen = False;
InitChainCrystValGen = False;
(* ============================================== *)
(* Direct crystallization *)
InitDirectCrystValGen = False;
(* ============================================== *)
(* Crystallization of two substances *)
InitTwoSubstCrystValGen = False;
TwoSubstCrystValGen = {};
(* ============================================== *)
(* Epimerization *)
InitEpimerizationValGen = False;
(* ============================================== *)
RoTotInitValGen = 100;
AlphaValGen = (9 / 10);
UseRoTotalNormValGen = False;
(* ============================================== *)
UseLogScale = True;
(* ============================================== *)
\[CapitalDelta]\[Gamma]Value = 1.0 * 10^-2;
(* ============================================== *)
\[Delta]\[CapitalGamma]Value = 1.0 * 10^-4;
\[Delta]aValue = 1.0 * 10^-4;
\[Delta]Value = 1.0 * 10^-10;
\[CapitalOmega]Value = 1.0 * 10^2;
\[Mu]Value = 1.0 * 10^3;
\[Rho]0Value = 1.0 * 10^2;
\[Delta]\[Rho]0Value = 1.0 * 10^-4;
(* ============================================== *)
\[Delta]\[Rho]Value = 1.0 * 10^-4;
(* ============================================== *)
tEvPlotMultiplier = 5;
tEvSmallValuePlotMultiplier = 10;
tEvRunMultiplier = 50;
EvPositiveEpsilon = 1.0 * 10^-10;
EvSmallValue = 1.0 * 10^-4;
tMaxNegativeEVval = 1.;
roTotalTolerance = 1.0 * 10^-1;
(* ============================================== *)
GetBifurcationMaxEigenValue[bifurcInfo : {___}] := bifurcInfo[[1]];
GetBifurcationAllCoeffs[bifurcInfo : {___}] := bifurcInfo[[2]];
GetBifurcationEigenVector[bifurcInfo : {___}] := bifurcInfo[[3]];
GetBifurcationThetaSol[bifurcInfo : {___}] := bifurcInfo[[4]];
GetBifurcationAllRo[bifurcInfo : {___}] := bifurcInfo[[5]];
GetBifurcationAllRoSymm[bifurcInfo : {___}] := bifurcInfo[[6]];
(* ============================================== *)
CreateBifurcationInfo[maxEVal_, allCoeffs_, eVec_, thetalSol_, allRo_, allRoSymm_] := Module[{retVal},
  retVal = {maxEVal, allCoeffs, eVec, thetalSol, allRo, allRoSymm};
  Return[retVal];
];
(* ============================================== *)
GammaPlusRule[enantSelType_?IntegerQ, coeff1_, coeff2_, GammaName_, gammaName_] := GammaRule[enantSelType, True, coeff1, coeff2, GammaName, gammaName];
(* ============================================== *)
GammaMinusRule[enantSelType_?IntegerQ, coeff1_, coeff2_, GammaName_, gammaName_] := GammaRule[enantSelType, False, coeff1, coeff2, GammaName, gammaName];
(* ============================================== *)
GammaRule[enantSelType_?IntegerQ, isPlus_?BooleanQ, coeff1_, coeff2_, GammaName_, gammaName_] := Module[{G, GR, g, rule, gammaMax, gammaMin, subscriptSign, addgamma, addGamma, gammaRange},
  If[enantSelType < EnantSelMin || enantSelType > EnantSelMax,
    (
      Print["GammaRule::Invalid enantSelType = ", enantSelType];
      Quit[];
    )
  ];

  If[ToString[InputForm[coeff1]] == ToString[InputForm[coeff2]],
    (
      Print["GammaRule::Names must be different: coeff1 = ", coeff1, ", coeff2 = ", coeff2];
      Quit[];
    )
  ];

  gammaMin = 0;
  gammaMax = 0;
  gammaRange = \[CapitalDelta]\[Gamma];

  addGamma = True;
  addgamma = True;

  subscriptSign = If[isPlus, "\:02d6", "\:02d7"];

  If[UseDefaultGammas,
    (
      G = ToExpression[ToString[GammaDefault] <> subscriptSign];
      g = ToExpression[ToString[gammaDefault] <> subscriptSign];
    ),
    (
      G = ToExpression[ToString[GammaName] <> subscriptSign];
      g = ToExpression[ToString[gammaName] <> subscriptSign];
    )
  ];

  GR = If[UseLogScale, 10^G, G];

  (* Corrections due to enantSelType *)
  (* Could use Switch but the readability seems to suffer *)
  If[enantSelType == EnantSelNoReaction,
    (
      G = 0;
      GR = 0;
      g = 0;
      addGamma = False;
      addgamma = False;
    )
  ];

  If[enantSelType == EnantSelNone,
    (
      g = 0;
      addgamma = False;
    )
  ];

  If[enantSelType == EnantSelPlus,
    (
      gammaMax = gammaRange;
    )
  ];

  If[enantSelType == EnantSelMinus,
    (
      gammaMin = -gammaRange;
    )
  ];

  If[enantSelType == EnantSelAll,
    (
      gammaMin = -gammaRange;
      gammaMax = gammaRange;
    )
  ];

  If[enantSelType == EnantSelPlusOne,
    (
      g = 1;
      addgamma = False;
    )
  ];

  If[enantSelType == EnantSelMinusOne,
    (
      g = -1;
      addgamma = False;
    )
  ];

  If[UseLogScale,
    (
      If[addGamma, AddVariable[G, Log10[GammaMinValue], Log10[GammaMaxValue]]];
    ),
    (
      If[addGamma, AddVariable[G, GammaMinValue, GammaMaxValue]];
    )
  ];

  If[addgamma, AddVariable[g, gammaMin, gammaMax]];

  rule = If[isPlus, {coeff1 -> (1 + g) * GR, coeff2 -> (1 - g) * GR}, {coeff1 -> (1 - g) * GR, coeff2 -> (1 + g) * GR}];

  Return[rule];
];
(* ============================================== *)
GammaDefault = Indeterminate;
gammaDefault = Indeterminate;
UseDefaultGammas = False;
(* ============================================== *)
ResetGammaValues[] := ResetGammaValues[\[CapitalOmega]];
(* ============================================== *)
ResetGammaValues[GammaMaxVal_] := ResetGammaValues[\[Delta]\[CapitalGamma], GammaMaxVal];
(* ============================================== *)
ResetGammaValues[GammaMinVal_, GammaMaxVal_] := Module[{},
  GammaMinValue = GammaMinVal;
  GammaMaxValue = GammaMaxVal;

  GammaDefault = Indeterminate;
  gammaDefault = Indeterminate;
  UseDefaultGammas = False;
];
(* ============================================== *)
SetDefaultGammas[GammaName_, gammaname_] := Module[{},
  GammaDefault = GammaName;
  gammaDefault = gammaname;
  UseDefaultGammas = True;
];
(* ============================================== *)
AddVariable[var_, lowerLimit_, upperLimit_] := Module[{},
  If[!MemberQ[allVarLst, var, Infinity],
    (
      allVarLst = Join[allVarLst, {var}];

      If[lowerLimit == upperLimit,
        (
          allVarEqLst = Join[allVarEqLst, {var == lowerLimit}];
        ),
        (
          allVarEqLst = Join[allVarEqLst, {lowerLimit <= var <= upperLimit}];
        ),
        (
          allVarEqLst = Join[allVarEqLst, {lowerLimit <= var <= upperLimit}];
        )
      ];
    )
  ];
];
(* ============================================== *)
AddAllCoeffRules[catSynthIdx_?IntegerQ, dirSedIdx_?IntegerQ] := Module[{},
  AddSynthRules[];
  AddActivationRules[];
  AddCatSynthRules[catSynthIdx];
  AddEpimRules[];
  AddLigRules[];
  AddDirSedRules[dirSedIdx];
];
(* ============================================== *)
AddSynthRules[] := Module[{cpMinValue, cpMaxValue, cmMinValue, cmMaxValue, r0Min, r0Max},
(* Synthesis *)
(* Print["AddSynthRules::Starting... "]; *)
  ResetGammaValues[];

  (* ============================================== *)

  cpMinValue = epsValue;
  cpMaxValue = epsValue;

  If[SynthVarType == VarTypeEpsilonRange, cpMaxValue = \[CapitalOmega]];

  If[SynthVarType == VarTypeNormalRange,
    (
      cpMinValue = \[Delta]\[CapitalGamma];
      cpMaxValue = \[CapitalOmega];
    )
  ];

  (* ============================================== *)

  cmMinValue = epsValue;
  cmMaxValue = epsValue;

  If[InvSynthVarType == VarTypeEpsilonRange, cmMaxValue = \[CapitalOmega]];

  If[InvSynthVarType == VarTypeNormalRange,
    (
      cmMinValue = \[Delta]\[CapitalGamma];
      cmMaxValue = \[CapitalOmega];
    )
  ];

  (* ============================================== *)

  r0Min = \[Delta]\[Rho]0;
  r0Max = \[Rho]00;

  If[UseRho0Fixed,
    (
      r0Min = Rho0FixedValue;
      r0Max = Rho0FixedValue;
    )
  ];

  If[UseLogScale,
    (
    AddCoeffRule[{r00 -> 10^r0, coeff\:0df4A\:2794Y -> 10^c\:02d7, coeff\:0df4a\:2794Y -> 10^c\:02d7, coeff\:0df4Y\:2794A -> 10^c\:02d6, coeff\:0df4Y\:2794a -> 10^c\:02d6}];
AddVariable[r0, Log10[r0Min], Log10[r0Max]];
AddVariable[c\:02d6, Log10[cpMinValue], Log10[cpMaxValue]];
AddVariable[c\:02d7, Log10[cmMinValue], Log10[cmMaxValue]];
),
(
AddCoeffRule[{r00 -> r0, coeff\:0df4A\:2794Y -> c\:02d7, coeff\:0df4a\:2794Y -> c\:02d7, coeff\:0df4Y\:2794A -> c\:02d6, coeff\:0df4Y\:2794a -> c\:02d6}];
AddVariable[r0, r0Min, r0Max];
AddVariable[c\:02d6, cpMinValue, cpMaxValue];
AddVariable[c\:02d7, cmMinValue, cmMaxValue];
)
];

];
(* ============================================== *)
AddCatSynthRules[catSynthIdx_?IntegerQ] := Module[{cnt, gammaMin, gammaMax, catSynthSel, invCanSynthSel},
(* Catalytic Synthesis *)
(* Print["AddCatSynthRules::Starting... catSynthIdx = ", catSynthIdx]; *)

  ResetGammaValues[];
  cnt = 1;

  (* ============================================== *)
  If[cnt++ == catSynthIdx, (catSynthSel = CatSynthEnantSel;invCanSynthSel = InvCatSynthEnantSel;), (catSynthSel = (invCanSynthSel = EnantSelNoReaction);)];
  AddCoeffRule[GammaPlusRule[catSynthSel, coeff\:0df4Y\:02d6A\:2794A\:02d6A, coeff\:0df4Y\:02d6A\:2794a\:02d6A, \[CapitalGamma]A, \[Gamma]A]];
AddCoeffRule[GammaMinusRule[invCanSynthSel, coeff\:0df4A\:02d6A\:2794Y\:02d6A, coeff\:0df4a\:02d6A\:2794Y\:02d6A, \[CapitalGamma]A, \[Gamma]A]];
(* ============================================== *)
If[cnt++ == catSynthIdx, (catSynthSel = CatSynthEnantSel;invCanSynthSel = InvCatSynthEnantSel;), (catSynthSel = (invCanSynthSel = EnantSelNoReaction);)];
AddCoeffRule[GammaPlusRule[catSynthSel, coeff\:0df4Y\:02d6AA\:2794A\:02d6AA, coeff\:0df4Y\:02d6AA\:2794a\:02d6AA, \[CapitalGamma]AA, \[Gamma]AA]];
AddCoeffRule[GammaMinusRule[invCanSynthSel, coeff\:0df4A\:02d6AA\:2794Y\:02d6AA, coeff\:0df4a\:02d6AA\:2794Y\:02d6AA, \[CapitalGamma]AA, \[Gamma]AA]];

If[cnt++ == catSynthIdx, (catSynthSel = CatSynthEnantSel;invCanSynthSel = InvCatSynthEnantSel;), (catSynthSel = (invCanSynthSel = EnantSelNoReaction);)];
AddCoeffRule[GammaPlusRule[catSynthSel, coeff\:0df4Y\:02d6Aa\:2794A\:02d6Aa, coeff\:0df4Y\:02d6Aa\:2794a\:02d6Aa, \[CapitalGamma]Aa, \[Gamma]Aa]];
AddCoeffRule[GammaMinusRule[invCanSynthSel, coeff\:0df4A\:02d6Aa\:2794Y\:02d6Aa, coeff\:0df4a\:02d6Aa\:2794Y\:02d6Aa, \[CapitalGamma]Aa, \[Gamma]Aa]];
(* ============================================== *)
If[cnt++ == catSynthIdx, (catSynthSel = CatSynthEnantSel;invCanSynthSel = InvCatSynthEnantSel;), (catSynthSel = (invCanSynthSel = EnantSelNoReaction);)];
AddCoeffRule[GammaPlusRule[catSynthSel, coeff\:0df4Y\:02d6AAA\:2794A\:02d6AAA, coeff\:0df4Y\:02d6AAA\:2794a\:02d6AAA, \[CapitalGamma]AAA, \[Gamma]AAA]];
AddCoeffRule[GammaMinusRule[invCanSynthSel, coeff\:0df4A\:02d6AAA\:2794Y\:02d6AAA, coeff\:0df4a\:02d6AAA\:2794Y\:02d6AAA, \[CapitalGamma]AAA, \[Gamma]AAA]];

If[cnt++ == catSynthIdx, (catSynthSel = CatSynthEnantSel;invCanSynthSel = InvCatSynthEnantSel;), (catSynthSel = (invCanSynthSel = EnantSelNoReaction);)];
AddCoeffRule[GammaPlusRule[catSynthSel, coeff\:0df4Y\:02d6AAa\:2794A\:02d6AAa, coeff\:0df4Y\:02d6AAa\:2794a\:02d6AAa, \[CapitalGamma]AAa, \[Gamma]AAa]];
AddCoeffRule[GammaMinusRule[invCanSynthSel, coeff\:0df4A\:02d6AAa\:2794Y\:02d6AAa, coeff\:0df4a\:02d6AAa\:2794Y\:02d6AAa, \[CapitalGamma]AAa, \[Gamma]AAa]];

If[cnt++ == catSynthIdx, (catSynthSel = CatSynthEnantSel;invCanSynthSel = InvCatSynthEnantSel;), (catSynthSel = (invCanSynthSel = EnantSelNoReaction);)];
AddCoeffRule[GammaPlusRule[catSynthSel, coeff\:0df4Y\:02d6AaA\:2794A\:02d6AaA, coeff\:0df4Y\:02d6AaA\:2794a\:02d6AaA, \[CapitalGamma]AaA, \[Gamma]AaA]];
AddCoeffRule[GammaMinusRule[invCanSynthSel, coeff\:0df4A\:02d6AaA\:2794Y\:02d6AaA, coeff\:0df4a\:02d6AaA\:2794Y\:02d6AaA, \[CapitalGamma]AaA, \[Gamma]AaA]];

If[cnt++ == catSynthIdx, (catSynthSel = CatSynthEnantSel;invCanSynthSel = InvCatSynthEnantSel;), (catSynthSel = (invCanSynthSel = EnantSelNoReaction);)];
AddCoeffRule[GammaPlusRule[catSynthSel, coeff\:0df4Y\:02d6Aaa\:2794A\:02d6Aaa, coeff\:0df4Y\:02d6Aaa\:2794a\:02d6Aaa, \[CapitalGamma]Aaa, \[Gamma]Aaa]];
AddCoeffRule[GammaMinusRule[invCanSynthSel, coeff\:0df4A\:02d6Aaa\:2794Y\:02d6Aaa, coeff\:0df4a\:02d6Aaa\:2794Y\:02d6Aaa, \[CapitalGamma]Aaa, \[Gamma]Aaa]];

(* Print["AddCatSynthRules::Total number of pairs of rules is ", (cnt-1)]; *)

PrepareCoeffGammaLst[];
];
(* ============================================== *)
AddEpimRules[] := Module[{epimSel},
(* Epimerization *)
(* Print["AddEpimRules::Starting... "]; *)
  ResetGammaValues[];
  epimSel = EpimEnantSel;
  If[UseSameEpimerizationForAll, SetDefaultGammas[\[CapitalEpsilon], \[Epsilon]]];

  (* No S terminal epimerization so far... *)
  AddCoeffRule[{coeff\:0df4Aa\:2794AA -> 0, coeff\:0df4AA\:2794Aa -> 0}];
(* ============================================== *)
AddCoeffRule[GammaPlusRule[epimSel, coeff\:0df4Aa\:2794aa, coeff\:0df4AA\:2794aA, \[CapitalEpsilon]A, \[Epsilon]A]];
(* ============================================== *)
AddCoeffRule[GammaPlusRule[epimSel, coeff\:0df4Aaa\:2794aaa, coeff\:0df4AAA\:2794aAA, \[CapitalEpsilon]AA, \[Epsilon]AA]];
AddCoeffRule[GammaPlusRule[epimSel, coeff\:0df4AaA\:2794aaA, coeff\:0df4AAa\:2794aAa, \[CapitalEpsilon]Aa, \[Epsilon]Aa]];
];
(* ============================================== *)
AddLigRules[] := Module[{ligSel, invLigSel},
(* Ligation *)
(* Print["AddLigRules::Starting... "]; *)

  ResetGammaValues[];
  If[UseSameLigationForAll, SetDefaultGammas[\[CapitalLambda], \[Lambda]]];

  ligSel = LigationEnantSel;
  invLigSel = InvLigationEnantSel;

  If[InitializeActivationValue,
    (
  (* ============================================== *)
  (* Ligation *)
  (* ============================================== *)
    AddCoeffRule[GammaPlusRule[ligSel, coeff\:0df4A\:066d\:02d6A\:2794AA, coeff\:0df4a\:066d\:02d6A\:2794aA, \[CapitalLambda]A, \[Lambda]A]];
(* ============================================== *)
AddCoeffRule[GammaPlusRule[ligSel, coeff\:0df4A\:066d\:02d6AA\:2794AAA, coeff\:0df4a\:066d\:02d6AA\:2794aAA, \[CapitalLambda]AA, \[Lambda]AA]];
AddCoeffRule[GammaPlusRule[ligSel, coeff\:0df4A\:066d\:02d6Aa\:2794AAa, coeff\:0df4a\:066d\:02d6Aa\:2794aAa, \[CapitalLambda]Aa, \[Lambda]Aa]];
(* ============================================== *)
(* ============================================== *)
(* Inverse ligation *)
(* ============================================== *)
AddCoeffRule[GammaMinusRule[invLigSel, coeff\:0df4AA\:2794A\:02d6A, coeff\:0df4aA\:2794a\:02d6A, \[CapitalLambda]A, \[Lambda]A]];
(* ============================================== *)
AddCoeffRule[GammaMinusRule[invLigSel, coeff\:0df4AAA\:2794A\:02d6AA, coeff\:0df4aAA\:2794a\:02d6AA, \[CapitalLambda]AA, \[Lambda]AA]];
AddCoeffRule[GammaMinusRule[invLigSel, coeff\:0df4AAa\:2794A\:02d6Aa, coeff\:0df4aAa\:2794a\:02d6Aa, \[CapitalLambda]Aa, \[Lambda]Aa]];
(* ============================================== *)
),
(
(* Print["TODO::CLM_BifurcationGenerator::AddLigRules::Check inverse ligation for models without activation."]; *)
(* ============================================== *)
(* Ligation *)
(* ============================================== *)
AddCoeffRule[GammaPlusRule[ligSel, coeff\:0df4A\:02d6A\:2794AA, coeff\:0df4a\:02d6A\:2794aA, \[CapitalLambda]A, \[Lambda]A]];
(* ============================================== *)
AddCoeffRule[GammaPlusRule[ligSel, coeff\:0df4A\:02d6AA\:2794AAA, coeff\:0df4a\:02d6AA\:2794aAA, \[CapitalLambda]xAA, \[Lambda]xAA]];
AddCoeffRule[GammaPlusRule[ligSel, coeff\:0df4A\:02d6Aa\:2794AAa, coeff\:0df4a\:02d6Aa\:2794aAa, \[CapitalLambda]xAa, \[Lambda]xAa]];
(* ============================================== *)
AddCoeffRule[GammaPlusRule[ligSel, coeff\:0df4AA\:02d6A\:2794AAA, coeff\:0df4AA\:02d6a\:2794AAa, \[CapitalLambda]AAx, \[Lambda]AAx]];
AddCoeffRule[GammaPlusRule[ligSel, coeff\:0df4Aa\:02d6A\:2794AaA, coeff\:0df4Aa\:02d6a\:2794Aaa, \[CapitalLambda]Aax, \[Lambda]Aax]];
(* ============================================== *)
(* ============================================== *)
(* Inverse ligation *)
(* ============================================== *)
AddCoeffRule[GammaMinusRule[invLigSel, coeff\:0df4AA\:2794A\:02d6A, coeff\:0df4aA\:2794a\:02d6A, \[CapitalLambda]A, \[Lambda]A]];
(* ============================================== *)
AddCoeffRule[GammaMinusRule[invLigSel, coeff\:0df4AAA\:2794A\:02d6AA, coeff\:0df4aAA\:2794a\:02d6AA, \[CapitalLambda]xAA, \[Lambda]xAA]];
AddCoeffRule[GammaMinusRule[invLigSel, coeff\:0df4AAa\:2794A\:02d6Aa, coeff\:0df4aAa\:2794a\:02d6Aa, \[CapitalLambda]xAa, \[Lambda]xAa]];
(* ============================================== *)
AddCoeffRule[GammaMinusRule[invLigSel, coeff\:0df4AAA\:2794AA\:02d6A, coeff\:0df4AAa\:2794AA\:02d6a, \[CapitalLambda]AAx, \[Lambda]AAx]];
AddCoeffRule[GammaMinusRule[invLigSel, coeff\:0df4AaA\:2794Aa\:02d6A, coeff\:0df4Aaa\:2794Aa\:02d6a, \[CapitalLambda]Aax, \[Lambda]Aax]];
(* ============================================== *)
(*
If[UseSTermLigation,
(
)
];
If[UseSTermInvLigation,
(
AddCoeffRule[GammaMinusRule[invLigSel,coeff\:0df4AaA\:2794Aa\:02d6A,coeff\:0df4Aaa\:2794Aa\:02d6a,\[CapitalLambda]Aa1,\[Lambda]Aa1]];
)
];
(* No S-Terminal ligation and inverse ligation so far. *)
AddCoeffRule[{coeff\:0df4AA\:02d6a\:2794AAa\[Rule]0, coeff\:0df4AAa\:2794AA\:02d6a\[Rule]0}];
AddCoeffRule[{coeff\:0df4Aa\:02d6A\:2794AaA\[Rule]0}];
AddCoeffRule[{coeff\:0df4Aa\:02d6a\:2794Aaa\[Rule]0}];
*)
(* ============================================== *)
Print["TODO::CLM_BifurcationGenerator::AddLigRules::UseSTermLigation and UseSTermInvLigation are not used yet. Check if this is needed."];
)
];
];
(* ============================================== *)
AddDirSedRule[cnt_?IntegerQ, dirSedIdx_?IntegerQ, coeff1_, coeff2_, SigmaName_, sigmaName_] := Module[{dirSedSel},
(*
Print["AddDirSedRule::Starting... cnt = ", cnt, ", dirSedIdx = ", dirSedIdx, ", coeff1 = ", coeff1, ", coeff2 = ", coeff2, ", SigmaName = ", SigmaName, ", sigmaName = ", sigmaName];
*)

  dirSedSel = If[cnt == dirSedIdx, If[UseMinusOne$sigma, EnantSelMinusOne, EnantSelPlusOne, EnantSelPlusOne], EnantSelNoReaction, EnantSelNoReaction];
  AddCoeffRule[GammaMinusRule[dirSedSel, coeff1, coeff2, SigmaName, sigmaName]];
];
(* ============================================== *)
AddDirSedRules[dirSedIdx_?IntegerQ] := Module[{cnt},
(* Frank - direct sedimentation *)
(* Print["AddDirSedRules::Starting... dirSedIdx = ", dirSedIdx]; *)
  cnt = 1;
  If[UseSigmaFixed, ResetGammaValues[SigmaFixedValue, SigmaFixedValue], ResetGammaValues[\[Mu] * \[CapitalOmega]]];
  (* ============================================== *)
  AddDirSedRule[cnt++, dirSedIdx, coeff\:0df4A\:02d6A\:27942Y, coeff\:0df4A\:02d6a\:27942Y, \[CapitalSigma]A, \[Sigma]A];
(* ============================================== *)
AddDirSedRule[cnt++, dirSedIdx, coeff\:0df4AA\:02d6AA\:27944Y, coeff\:0df4AA\:02d6aa\:27944Y, \[CapitalSigma]AA, \[Sigma]AA];
AddDirSedRule[cnt++, dirSedIdx, coeff\:0df4Aa\:02d6Aa\:27944Y, coeff\:0df4Aa\:02d6aA\:27944Y, \[CapitalSigma]Aa, \[Sigma]Aa];
(* ============================================== *)
AddDirSedRule[cnt++, dirSedIdx, coeff\:0df4AAA\:02d6AAA\:27946Y, coeff\:0df4AAA\:02d6aaa\:27946Y, \[CapitalSigma]AAA, \[Sigma]AAA];
AddDirSedRule[cnt++, dirSedIdx, coeff\:0df4AAa\:02d6AAa\:27946Y, coeff\:0df4AAa\:02d6aaA\:27946Y, \[CapitalSigma]AAa, \[Sigma]AAa];
AddDirSedRule[cnt++, dirSedIdx, coeff\:0df4AaA\:02d6AaA\:27946Y, coeff\:0df4AaA\:02d6aAa\:27946Y, \[CapitalSigma]AaA, \[Sigma]AaA];
AddDirSedRule[cnt++, dirSedIdx, coeff\:0df4Aaa\:02d6Aaa\:27946Y, coeff\:0df4Aaa\:02d6aAA\:27946Y, \[CapitalSigma]Aaa, \[Sigma]Aaa];
(* ============================================== *)
(* So far, these are all the same no matter what!!! *)
AddDirSedRule[cnt++, dirSedIdx, coeff\:0df4A\:02d6AA\:27943Y, coeff\:0df4A\:02d6aa\:27943Y, \[CapitalSigma], \[Sigma]];
AddDirSedRule[cnt++, dirSedIdx, coeff\:0df4A\:02d6Aa\:27943Y, coeff\:0df4A\:02d6aA\:27943Y, \[CapitalSigma], \[Sigma]];
AddDirSedRule[cnt++, dirSedIdx, coeff\:0df4AA\:02d6Aa\:27944Y, coeff\:0df4AA\:02d6aA\:27944Y, \[CapitalSigma], \[Sigma]];
AddDirSedRule[cnt++, dirSedIdx, coeff\:0df4A\:02d6AAA\:27944Y, coeff\:0df4A\:02d6aaa\:27944Y, \[CapitalSigma], \[Sigma]];
AddDirSedRule[cnt++, dirSedIdx, coeff\:0df4AA\:02d6AAA\:27945Y, coeff\:0df4AA\:02d6aaa\:27945Y, \[CapitalSigma], \[Sigma]];
AddDirSedRule[cnt++, dirSedIdx, coeff\:0df4Aa\:02d6AAA\:27945Y, coeff\:0df4Aa\:02d6aaa\:27945Y, \[CapitalSigma], \[Sigma]];
AddDirSedRule[cnt++, dirSedIdx, coeff\:0df4A\:02d6AAa\:27944Y, coeff\:0df4A\:02d6aaA\:27944Y, \[CapitalSigma], \[Sigma]];
AddDirSedRule[cnt++, dirSedIdx, coeff\:0df4AA\:02d6AAa\:27945Y, coeff\:0df4AA\:02d6aaA\:27945Y, \[CapitalSigma], \[Sigma]];
AddDirSedRule[cnt++, dirSedIdx, coeff\:0df4Aa\:02d6AAa\:27945Y, coeff\:0df4Aa\:02d6aaA\:27945Y, \[CapitalSigma], \[Sigma]];
AddDirSedRule[cnt++, dirSedIdx, coeff\:0df4AAA\:02d6AAa\:27946Y, coeff\:0df4AAA\:02d6aaA\:27946Y, \[CapitalSigma], \[Sigma]];
AddDirSedRule[cnt++, dirSedIdx, coeff\:0df4A\:02d6AaA\:27944Y, coeff\:0df4A\:02d6aAa\:27944Y, \[CapitalSigma], \[Sigma]];
AddDirSedRule[cnt++, dirSedIdx, coeff\:0df4AA\:02d6AaA\:27945Y, coeff\:0df4AA\:02d6aAa\:27945Y, \[CapitalSigma], \[Sigma]];
AddDirSedRule[cnt++, dirSedIdx, coeff\:0df4Aa\:02d6AaA\:27945Y, coeff\:0df4Aa\:02d6aAa\:27945Y, \[CapitalSigma], \[Sigma]];
AddDirSedRule[cnt++, dirSedIdx, coeff\:0df4AAA\:02d6AaA\:27946Y, coeff\:0df4AAA\:02d6aAa\:27946Y, \[CapitalSigma], \[Sigma]];
AddDirSedRule[cnt++, dirSedIdx, coeff\:0df4AAa\:02d6AaA\:27946Y, coeff\:0df4AAa\:02d6aAa\:27946Y, \[CapitalSigma], \[Sigma]];
AddDirSedRule[cnt++, dirSedIdx, coeff\:0df4A\:02d6Aaa\:27944Y, coeff\:0df4A\:02d6aAA\:27944Y, \[CapitalSigma], \[Sigma]];
AddDirSedRule[cnt++, dirSedIdx, coeff\:0df4AA\:02d6Aaa\:27945Y, coeff\:0df4AA\:02d6aAA\:27945Y, \[CapitalSigma], \[Sigma]];
AddDirSedRule[cnt++, dirSedIdx, coeff\:0df4Aa\:02d6Aaa\:27945Y, coeff\:0df4Aa\:02d6aAA\:27945Y, \[CapitalSigma], \[Sigma]];
AddDirSedRule[cnt++, dirSedIdx, coeff\:0df4AAA\:02d6Aaa\:27946Y, coeff\:0df4AAA\:02d6aAA\:27946Y, \[CapitalSigma], \[Sigma]];
AddDirSedRule[cnt++, dirSedIdx, coeff\:0df4AAa\:02d6Aaa\:27946Y, coeff\:0df4AAa\:02d6aAA\:27946Y, \[CapitalSigma], \[Sigma]];
AddDirSedRule[cnt++, dirSedIdx, coeff\:0df4AaA\:02d6Aaa\:27946Y, coeff\:0df4AaA\:02d6aAA\:27946Y, \[CapitalSigma], \[Sigma]];

(* Print["AddDirSedRules::Total number of rules is ", (cnt-1)]; *)
];
(* ============================================== *)
(* Add all rules for sedimentation of pairs as specified by sedIdx *)
AddPairSedRules[sedIdx_?IntegerQ] := Module[{},
  coeff\:0df4AA\:02d6Aaa\:2794AA$Aaa = 10;
coeff\:0df4AA$Aaa\:2794AA\:02d6Aaa = 10;
r$AA$Aaa$Max = 10^-3;
kCryst$AA$Aaa = 10^3;
kDiss$AA$Aaa = 10^-2;
coeff\:0df4$$$AA$Aaa\:27945Y = 10^-3;
];
(* ============================================== *)
AddActivationRules[] := Module[{apMinValue, apMaxValue, amMinValue, amMaxValue},
(* Activation *)
(* Print["AddActivationRules::Starting..."]; *)
  ResetGammaValues[];

  amMinValue = apMinValue = \[Delta]a;
  amMaxValue = apMaxValue = \[CapitalOmega];

  If[UseFixedInvActivationEps,
    (
      amMinValue = amMaxValue = epsValue;
    )
  ];

  If[InitializeActivationValue,
    (
    If[UseLogScale,
      (
      AddCoeffRule[{coeff\:0df4A\:2794A\:066d -> 10^a\:02d6, coeff\:0df4A\:066d\:2794A -> 10^a\:02d7}];
AddVariable[a\:02d6, Log10[apMinValue], Log10[apMaxValue]];
AddVariable[a\:02d7, Log10[amMinValue], Log10[amMaxValue]];
),
(
AddCoeffRule[{coeff\:0df4A\:2794A\:066d -> a\:02d6, coeff\:0df4A\:066d\:2794A -> a\:02d7}];
AddVariable[a\:02d6, apMinValue, apMaxValue];
AddVariable[a\:02d7, amMinValue, amMaxValue];
)
];
)
];
];
(* ============================================== *)


PrepareCoeffGammaLst[] := Module[{},
  coeffGammaPlusLst = Table[Indeterminate, {ii, 1, 7}];
  coeffGammaMinusLst = Table[Indeterminate, {ii, 1, 7}];

  coeffGammaPlusLst[[1]] = {coeff\:0df4Y\:02d6A\:2794A\:02d6A, coeff\:0df4Y\:02d6A\:2794a\:02d6A};
coeffGammaMinusLst[[1]] = {coeff\:0df4A\:02d6A\:2794Y\:02d6A, coeff\:0df4a\:02d6A\:2794Y\:02d6A};

coeffGammaPlusLst[[2]] = {coeff\:0df4Y\:02d6AA\:2794A\:02d6AA, coeff\:0df4Y\:02d6AA\:2794a\:02d6AA};
coeffGammaMinusLst[[2]] = {coeff\:0df4A\:02d6AA\:2794Y\:02d6AA, coeff\:0df4a\:02d6AA\:2794Y\:02d6AA};

coeffGammaPlusLst[[3]] = {coeff\:0df4Y\:02d6Aa\:2794A\:02d6Aa, coeff\:0df4Y\:02d6Aa\:2794a\:02d6Aa};
coeffGammaMinusLst[[3]] = {coeff\:0df4A\:02d6Aa\:2794Y\:02d6Aa, coeff\:0df4a\:02d6Aa\:2794Y\:02d6Aa};

coeffGammaPlusLst[[4]] = {coeff\:0df4Y\:02d6AAA\:2794A\:02d6AAA, coeff\:0df4Y\:02d6AAA\:2794a\:02d6AAA};
coeffGammaMinusLst[[4]] = {coeff\:0df4A\:02d6AAA\:2794Y\:02d6AAA, coeff\:0df4a\:02d6AAA\:2794Y\:02d6AAA};

coeffGammaPlusLst[[5]] = {coeff\:0df4Y\:02d6AAa\:2794A\:02d6AAa, coeff\:0df4Y\:02d6AAa\:2794a\:02d6AAa};
coeffGammaMinusLst[[5]] = {coeff\:0df4A\:02d6AAa\:2794Y\:02d6AAa, coeff\:0df4a\:02d6AAa\:2794Y\:02d6AAa};

coeffGammaPlusLst[[6]] = {coeff\:0df4Y\:02d6AaA\:2794A\:02d6AaA, coeff\:0df4Y\:02d6AaA\:2794a\:02d6AaA};
coeffGammaMinusLst[[6]] = {coeff\:0df4A\:02d6AaA\:2794Y\:02d6AaA, coeff\:0df4a\:02d6AaA\:2794Y\:02d6AaA};

coeffGammaPlusLst[[7]] = {coeff\:0df4Y\:02d6Aaa\:2794A\:02d6Aaa, coeff\:0df4Y\:02d6Aaa\:2794a\:02d6Aaa};
coeffGammaMinusLst[[7]] = {coeff\:0df4A\:02d6Aaa\:2794Y\:02d6Aaa, coeff\:0df4a\:02d6Aaa\:2794Y\:02d6Aaa};
];
(* ============================================== *)
CoeffGammaPlus[catSynthIdx_?IntegerQ, rule_?VectorQ] := Module[{gamma, nom, denom},
  nom = (coeffGammaPlusLst[[catSynthIdx, 1]] - coeffGammaPlusLst[[catSynthIdx, 2]]) /. rule;
  denom = (coeffGammaPlusLst[[catSynthIdx, 1]] + coeffGammaPlusLst[[catSynthIdx, 2]]) /. rule;
  gamma = If[denom > 0, nom / denom, 1, Indeterminate];

  (*
Print["CoeffGammaPlus::coeffGammaPlusLst[[",catSynthIdx,"]] = ",coeffGammaPlusLst[[catSynthIdx]], ", rule = ", rule,", value of coeffGammaPlusLst[[",catSynthIdx,"]] = ",(coeffGammaPlusLst[[catSynthIdx]]/. rule)] ;
Print["CoeffGammaPlus::nom = ", nom,", denom = ", denom, ", gamma = ", gamma];
*)

  Return[gamma];
];
(* ============================================== *)
CoeffGammaMinus[catSynthIdx_?IntegerQ, rule_?VectorQ] := Module[{gamma, nom, denom},
  nom = (coeffGammaMinusLst[[catSynthIdx, 2]] - coeffGammaMinusLst[[catSynthIdx, 1]]) /. rule;
  denom = (coeffGammaMinusLst[[catSynthIdx, 1]] + coeffGammaMinusLst[[catSynthIdx, 2]]) /. rule;
  gamma = If[denom > 0, nom / denom, 1, Indeterminate];

  (*
Print["CoeffGammaMinus::coeffGammaMinusLst[[",catSynthIdx,"]] = ",coeffGammaMinusLst[[catSynthIdx]], ", rule = ", rule,", value of coeffGammaMinusLst[[",catSynthIdx,"]] = ",(coeffGammaMinusLst[[catSynthIdx]]/. rule)] ;
Print["CoeffGammaMinus::nom = ", nom,", denom = ", denom, ", gamma = ", gamma];
*)
  Return[gamma];
];
(* ============================================== *)
AddCoeffRule[cRules_?VectorQ] := Module[{},
  coeffRules = Join[coeffRules, cRules];
(* Print["AddCoeffRule::coeffRules = ", coeffRules // MatrixForm]; *)
];
(* ============================================== *)
GetUniqueName[name_, catCnt_, dirSedCnt_] := (ToString[name] <> "$" <> ToString[catCnt] <> "$" <> ToString[dirSedCnt]);
(* ============================================== *)
(* ============================================== *)
(* Initializes the model based on the values of various prameters and returns options for use in other places *)
GeneratorInitializeModel[maxChainLen_?IntegerQ, useActivation_?BooleanQ, useNNT_?BooleanQ] := Module[{maxEnantNumb, rndVal, alphaVal, roTotInitVal, YSubstWeightVal, unknownOptions, debugOptions, generalOptions, synthesisOptions, activationOptions, ligationOptions, catLigationOptions, pairFormationOptions, crystallizationOptions, epimerizationOptions, initialValuesOptions, NDSolveOptions, NDSolveMonitorOptions, outputOptions, runChainModelOptions, plotSubstanceOptions, description, options, retVal, initDirectCrystVal, initPairFormVal, initCrystVal, initCrystDecVal, initBasicCrystVal, initChainCrystVal, initTwoSubstCrystVal, twoSubstCrystVal, useSamePairFormCoeffVal, useSymmetricPairsVal, initEpimerizationVal, useRoTotalNorm},
(* ============================================== *)
  maxEnantNumb = 1;
  initDirectCrystVal = InitDirectCrystValGen;

  initPairFormVal = InitPairFormValGen;
  useSamePairFormCoeffVal = UseSamePairFormCoeffValGen;
  useSymmetricPairsVal = UseSymmetricPairsValGen;

  initCrystVal = InitCrystValGen;
  initCrystDecVal = InitCrystDecValGen;
  initBasicCrystVal = InitBasicCrystValGen;
  initChainCrystVal = InitChainCrystValGen;
  initTwoSubstCrystVal = InitTwoSubstCrystValGen;
  twoSubstCrystVal = TwoSubstCrystValGen;

  initEpimerizationVal = InitEpimerizationValGen;
  (* ============================================== *)
  CDFuncSelector = 3;
  DissolveSelector = 1;
  (* ============================================== *)
  rndVal = RandomInteger[{0, 10^12}];
  SeedRandomValue = rndVal;
  (* ============================================== *)
  If[!SilentRunValue, Print["SeedRandomValue = ", SeedRandomValue]];
  (* ============================================== *)
  roTotInitVal = RoTotInitValGen;
  alphaVal = AlphaValGen;
  useRoTotalNorm = UseRoTotalNormValGen;
  (* ============================================== *)
  DirectCrystMinLen = 1;
  DirectCrystSecondSubstMinLen = 1;
  (* ============================================== *)
  CatSynthR = 1;
  CatSynthMinLen = 1;
  (* ============================================== *)
  CatLigC = 0.05;
  CatLigR = 0.05;
  CatLigMinLen = maxChainLen;
  (* ============================================== *)
  YSubstWeightVal = 1;
  (* ============================================== *)
  unknownOptions := {PrintGamma -> False, RoTotalNorm -> 1, PrintInitValues -> False};
  If[useRoTotalNorm,
    (
      unknownOptions := {PrintGamma -> False, RoTotalNorm -> 2 * roTotInitVal, PrintInitValues -> False};
    )
  ];
  (* ============================================== *)
  debugOptions := {PrintPrepareEquationsInfo -> False, Print\[Rho]AllFuncInfo -> False};
  (* ============================================== *)
  (* NonNegativeTypeNone, NonNegativeTypeNumeric, NonNegativeTypeAnalytic *)
  If[useNNT,
    (
      generalOptions := {UseHighPrecision -> True, NonNegativeType -> NonNegativeTypeNumeric, UseIdenticalEnantiomers -> False};
    ),
    (
      generalOptions := {UseHighPrecision -> True, NonNegativeType -> NonNegativeTypeNone, UseIdenticalEnantiomers -> False};
    )
  ];
  (* ============================================== *)
  synthesisOptions := {InitializeSynthesis -> True, InitializeCatSynthesis -> True, UseCatSynthEnantGrouping -> True, AssignSynthCoefficients -> False, AssignCatSynthCoefficients -> False, AssignWrongCatSynthReactions -> True};
  (* ============================================== *)
  activationOptions := {InitializeActivation -> useActivation, AssignActivationCoefficients -> False};
  (* ============================================== *)
  ligationOptions := {InitializeLigation -> True, UseOnlySimpleLigation -> True, AssignLigCoefficients -> False};
  (* ============================================== *)
  catLigationOptions := {InitializeCatLigation -> False, UseCatLigEnantGrouping -> False, AssignCatLigCoefficients -> True, AssignWrongCatLigReactions -> True};
  (* ============================================== *)
  pairFormationOptions := {InitializePairFormation -> initPairFormVal, UseSamePairFormCoeff -> useSamePairFormCoeffVal, UseSymmetricPairs -> useSymmetricPairsVal};
  (* ============================================== *)
  crystallizationOptions := {InitializeCrystallization -> initCrystVal, InitializeCrystalDecay -> initCrystDecVal, InitializeBasicCryst -> initBasicCrystVal, InitializeChainCryst -> initChainCrystVal, InitializeTwoSubstCryst -> initTwoSubstCrystVal, TwoSubstCryst -> twoSubstCrystVal, AssignBasicCrystCoefficients -> False, UseAllSubstForCryst -> False, AllSubstForCrystAlpha -> alphaVal, InitializeDirectCrystallization -> initDirectCrystVal, UseAllPairsForDirectCryst -> True, AssignDirectCrystCoefficients -> False};
  (* ============================================== *)
  epimerizationOptions := {InitializeEpimerization -> initEpimerizationVal, AssignEpimCoefficients -> False};
  (* ============================================== *)
  initialValuesOptions := {InitRandom -> initRandomVal, InitYsubst -> initYsubstVal, YSubstWeight -> YSubstWeightVal, InitLEnant -> initLEnantVal, InitDEnant -> initDEnantVal, InitDMultiplier -> 0.9999};
  (* ============================================== *)
  NDSolveOptions := {ApplyCoeffRule -> False, NDSolveMethod -> {"EquationSimplification" -> "Residual"}, NDSolveUseDerivativeForAggregateSubst -> False, NDSolveUseFuncForAggregateSubst -> True, NDSolveUseNumericFuncForAggregateSubst -> False, DoNotUseInterpolationFunction -> False};
  (* ============================================== *)
  NDSolveMonitorOptions := {NStorage -> 1000, PrintMonitorData -> False, MonitorPrintFrequency -> AlmostInfinity, MonitorType -> None, StepMonitorFunction :> None, DynamicStepMonitor -> None, QuitMonitor -> None, UseShowProgress -> False};
  (* ============================================== *)
  outputOptions := {\[Rho]AllFuncDescription -> description, TStartMultiplier -> 0};
  (* ============================================== *)
  runChainModelOptions := {RunModelOutputType -> OutputTypeNone};
  (* ============================================== *)
  plotSubstanceOptions := {PlotSubstances -> False, SubstancePlotList -> {}};
  (* ============================================== *)
  description = "maxEnantNumb = " <> ToString[maxEnantNumb] <> ", maxChainLen = " <> ToString[maxChainLen] <> ", roTotInitVal = " <> ToString[roTotInitVal] <> ", alphaVal = " <> ToString[N[alphaVal]];
  (* ============================================== *)
  options = Join[generalOptions, synthesisOptions, activationOptions, ligationOptions, catLigationOptions, pairFormationOptions, crystallizationOptions, epimerizationOptions, initialValuesOptions, debugOptions, NDSolveOptions, NDSolveMonitorOptions, outputOptions, runChainModelOptions, plotSubstanceOptions, unknownOptions];
  (* ============================================== *)
  InitializeCommonParameters[options];
  (* ============================================== *)
  InitializeChains[maxChainLen, maxEnantNumb, options];
  (* ============================================== *)
  Return[options];
];
(* ============================================== *)
