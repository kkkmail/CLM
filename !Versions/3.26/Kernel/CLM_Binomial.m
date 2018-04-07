(* ============================================== *)
(* :Summary: CLM Binomial logic. *)
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
Options[CLMBinomial] = {precision -> 500, kAtoS -> 10^-6, kStoA -> 10^-3, dkStoA -> 10^-17, kStoT -> 10^-3, kTtoS -> 10^-6, NChain -> 1, kAPtoSPbaseWeight -> 0, kAPtoSPslowWeight -> 0, kAPtoSPfastWeight -> 0, kAPtoSPfastShift -> 0, kAPtoSPchainLenDivisor -> 50, kAPtoSPchainLenPower -> 1, kAPtoSPchainLenMultiplier -> 8, kAPtoSPstepShift -> 0, kAPtoSPstepMinValue -> 0, kAPtoSPstepInverse -> False, kAPtoSPfuncTanhBase -> 1, kAPtoSPfuncTanhMid -> 0, kAPtoSPfuncTanhSlope -> 1, roTdefaultValue -> 1, zeroEps -> 10^-10, frEps -> 3 * 10^-2, UseDeltaDistribution -> False};
(* ============================================== *)
kStoArule = {kLtoA -> kStoA * (1 - dkStoA), kDtoA -> kStoA * (1 + dkStoA)};
(* ============================================== *)
If[!SilentRunValue, Print["CLM_Binomial::TODO::Move remaining constants into options."]];
precisionVal = 500;

PrintDetails = False;
PrintDebugInfo = False;
PrintEachStepInfo = True;
(* ============================================== *)
If[!SilentRunValue,
  (
    Print["CLM_Binomial::TODO: Most of evolution (BinomialRun and calls from it) was not fully updated to support options..."];
    Print["CLM_Binomial::TODO: Clean up the mess with GammaFunc, kFunc, kmFunc"];
  )
];
(* ============================================== *)
(* idx must be between 0 and nn *)
GammaFunc[kLst_?VectorQ, idx_?IntegerQ] := Module[{gamma, nn},
  nn = Length[kLst] - 1;
  gamma = (kLst[[idx + 1]] - kLst[[nn + 1 - idx]]) / (kLst[[idx + 1]] + kLst[[nn + 1 - idx]]);

  Return[gamma];
];
(* ============================================== *)
(* idx must be between 0 and nn *)
kFunc[kLst_?VectorQ, idx_?IntegerQ] := Module[{k, nn},
  nn = Length[kLst] - 1;
  k = (kLst[[idx + 1]] + kLst[[nn + 1 - idx]]) / 2;

  Return[k];
];
(* ============================================== *)
(* idx must be between 0 and nn *)
kmFunc[kLst_?VectorQ, idx_?IntegerQ] := Module[{k, nn},
  nn = Length[kLst] - 1;
  k = (kLst[[idx + 1]] - kLst[[nn + 1 - idx]]) / 2;

  Return[k];
];
(* ============================================== *)
GammaFunc[func_, idx_?IntegerQ, nn_?IntegerQ] := Module[{gamma},
  gamma = (func[idx, nn] - func[nn - idx, nn]) / (func[idx, nn] + func[nn - idx, nn]);
  (* Print["GammaFunc::idx = ", idx, ", nn = ", nn, ", gamma = ",N[gamma], ", func[",idx,"] = ", N[func[idx,nn]], ", func[",(nn-idx),"] = ", N[func[nn-idx,nn]]]; *)
  Return[gamma];
];
(* ============================================== *)
kFunc[func_, idx_?IntegerQ, nn_?IntegerQ] := Module[{k},
  k = (func[idx, nn] + func[nn - idx, nn]) / 2;
  (* Print["kFunc::idx = ", idx, ", nn = ", nn, ", k = ",N[k], ", func[",idx,"] = ", N[func[idx,nn]], ", func[",(nn-idx),"] = ", N[func[nn-idx,nn]]]; *)
  Return[k];
];
(* ============================================== *)
kmFunc[func_, idx_?IntegerQ, nn_?IntegerQ] := Module[{k},
  k = (func[idx, nn] - func[nn - idx, nn]) / 2;
  (* Print["kmFunc::idx = ", idx, ", nn = ", nn, ", k = ",N[k], ", func[",idx,"] = ", N[func[idx,nn]], ", func[",(nn-idx),"] = ", N[func[nn-idx,nn]]]; *)
  Return[k];
];
(* ============================================== *)
kFunctionBaseBare[nu_, base_, mid_, slope_] := Module[{retVal, nuVal, baseVal},
  nuVal = nu;
  baseVal = base;
  retVal = baseVal + (1 - baseVal) * ((Tanh[nuVal * slope - mid] - Tanh[-slope - mid]) / (Tanh[slope - mid] - Tanh[-slope - mid]));
  Return[retVal];
];
(* ============================================== *)
kFunctionBase[nu_?NumericQ, base_?NumericQ, mid_?NumericQ, slope_?NumericQ] := Module[{retVal, nuVal, baseVal},
  nuVal = Max[Min[nu, 1], -1];
  baseVal = Max[Min[base, 1], 0];
  retVal = kFunctionBaseBare[nuVal, baseVal, mid, slope];
  Return[retVal];
];
(* ============================================== *)
DkFunctionBaseBare[nu_, base_, mid_, slope_] := Module[{retVal, nuu},
  retVal = D[kFunctionBaseBare[nuu, base, mid, slope], nuu] /. {nuu -> nu};
  Return[retVal];
];
(* ============================================== *)
DkFunctionBase[nu_?NumericQ, base_?NumericQ, mid_?NumericQ, slope_?NumericQ] := DkFunctionBaseBare[nu, base, mid, slope];
(* ============================================== *)
If[!SilentRunValue, Print["CLM_Binomial::Direct autocatalysis"]];

kAPtoSPfuncFirstRun = False;
kAPtoSPfuncIdxMin = 1;
kAPtoSPfuncGenericIdx = 1;
kAPtoSPfuncStepIdx = 2;
kAPtoSPfuncTanhIdx = 3;
kAPtoSPfuncIdxMax = 3;
kAPtoSPfuncLst := {kAPtoSPfuncGeneric, kAPtoSPfuncStep, kAPtoSPfuncTanh};
(* ============================================== *)
kAPtoSPfunc[kAPtoSPfuncIdx_?IntegerQ, nL_?IntegerQ, ChainLen_?IntegerQ, rawOptions___] := Module[{retVal},
  If[kAPtoSPfuncIdx < kAPtoSPfuncIdxMin || kAPtoSPfuncIdx > kAPtoSPfuncIdxMax,
    (
      Print["kAPtoSPfunc::kAPtoSPfuncIdx is out of range. kAPtoSPfuncIdx = ", kAPtoSPfuncIdx];
      Return[Indeterminate];
    )
  ];

  retVal = kAPtoSPfuncLst[[kAPtoSPfuncIdx]][nL, ChainLen, rawOptions];

  Return[retVal];
];
(* ============================================== *)
kAPtoSPfuncGeneric[nL_?IntegerQ, ChainLen_?IntegerQ, rawOptions___] := Module[{retVal, opts, kAPtoSPbaseWeightVal, kAPtoSPslowWeightVal, kAPtoSPfastWeightVal, kAPtoSPfastShiftVal, kAPtoSPchainLenDivisorVal, kAPtoSPchainLenPowerVal, kAPtoSPchainLenMultiplierVal, precisionVal},
  opts = ProcessOptions[rawOptions];

  kAPtoSPbaseWeightVal = kAPtoSPbaseWeight /. opts /. Options[CLMBinomial];
  kAPtoSPslowWeightVal = kAPtoSPslowWeight /. opts /. Options[CLMBinomial];
  kAPtoSPfastWeightVal = kAPtoSPfastWeight /. opts /. Options[CLMBinomial];
  kAPtoSPfastShiftVal = kAPtoSPfastShift /. opts /. Options[CLMBinomial];
  kAPtoSPchainLenDivisorVal = kAPtoSPchainLenDivisor /. opts /. Options[CLMBinomial];
  kAPtoSPchainLenPowerVal = kAPtoSPchainLenPower /. opts /. Options[CLMBinomial];
  kAPtoSPchainLenMultiplierVal = kAPtoSPchainLenMultiplier /. opts /. Options[CLMBinomial];

  precisionVal = precision /. opts /. Options[CLMBinomial];

  If[! kAPtoSPfuncFirstRun,
    (
      kAPtoSPfuncFirstRun = True;

      Print["kAPtoSPfuncGeneric::kAPtoSPbaseWeight = ", kAPtoSPbaseWeightVal, ", kAPtoSPslowWeight = ", kAPtoSPslowWeightVal, ", kAPtoSPfastWeight = ", kAPtoSPfastWeightVal, ", kAPtoSPfastShift = ", kAPtoSPfastShiftVal, ", kAPtoSPchainLenDivisor = ", kAPtoSPchainLenDivisorVal, ", kAPtoSPchainLenPower = ", kAPtoSPchainLenPowerVal, ", kAPtoSPchainLenMultiplier = ", kAPtoSPchainLenMultiplierVal];
    )
  ];

  retVal = N[(kAPtoSPbaseWeightVal + kAPtoSPslowWeightVal * If[(nL >= 0 && nL <= ChainLen), ((nL + ChainLen) / (ChainLen + ChainLen))^(kAPtoSPchainLenMultiplierVal * (ChainLen / kAPtoSPchainLenDivisorVal)^(kAPtoSPchainLenPowerVal)), Indeterminate] + kAPtoSPfastWeightVal * If[(nL >= 0 && nL <= ChainLen), If[nL >= (kAPtoSPfastShiftVal * ChainLen), ((nL - (kAPtoSPfastShiftVal * ChainLen)) / (ChainLen - (kAPtoSPfastShiftVal * ChainLen)))^(kAPtoSPchainLenMultiplierVal * (ChainLen / kAPtoSPchainLenDivisorVal)^(kAPtoSPchainLenPowerVal)), 0], Indeterminate]) / (kAPtoSPbaseWeightVal + kAPtoSPslowWeightVal + kAPtoSPfastWeightVal), precisionVal];

  Return[retVal];
];
(* ============================================== *)
kAPtoSPfuncStep[nL_?IntegerQ, ChainLen_?IntegerQ, rawOptions___] := Module[{retVal, opts, precisionVal, kAPtoSPstepShiftVal, kAPtoSPstepMinValueVal, kAPtoSPstepInverseVal},
  opts = ProcessOptions[rawOptions];

  kAPtoSPstepShiftVal = Max[Min[(kAPtoSPstepShift /. opts /. Options[CLMBinomial]), ChainLen], 0];
  kAPtoSPstepMinValueVal = Max[Min[(kAPtoSPstepMinValue /. opts /. Options[CLMBinomial]), 1], 0];
  kAPtoSPstepInverseVal = kAPtoSPstepInverse /. opts /. Options[CLMBinomial];

  precisionVal = precision /. opts /. Options[CLMBinomial];

  If[! kAPtoSPfuncFirstRun,
    (
      kAPtoSPfuncFirstRun = True;
      Print["kAPtoSPfuncStep::kAPtoSPstepShiftVal = ", kAPtoSPstepShiftVal, ", kAPtoSPstepMinValueVal = ", kAPtoSPstepMinValueVal];
    )
  ];

  If[kAPtoSPstepInverseVal,
    (
      retVal = N[If[(ChainLen - nL) >= kAPtoSPstepShiftVal, 1, kAPtoSPstepMinValueVal], precisionVal];
    ),
    (
      retVal = N[If[nL >= kAPtoSPstepShiftVal, 1, kAPtoSPstepMinValueVal], precisionVal];
    )
  ];

  Return[retVal];
];
(* ============================================== *)
kAPtoSPfuncTanh[nL_?IntegerQ, ChainLen_?IntegerQ, rawOptions___] := Module[{retVal, opts, precisionVal, base, mid, slope, nu},
  opts = ProcessOptions[rawOptions];

  nu = (2 * (nL - ChainLen / 2)) / ChainLen;
  base = (kAPtoSPfuncTanhBase /. opts /. Options[CLMBinomial]);
  mid = (kAPtoSPfuncTanhMid /. opts /. Options[CLMBinomial]);
  slope = (kAPtoSPfuncTanhSlope /. opts /. Options[CLMBinomial]);

  precisionVal = precision /. opts /. Options[CLMBinomial];

  If[! kAPtoSPfuncFirstRun,
    (
      kAPtoSPfuncFirstRun = True;
      Print["kAPtoSPfuncTanh::base = ", base, ", mid = ", mid, ", slope = ", slope];
    )
  ];

  retVal = kFunctionBase[nu, base, mid, slope];

  Return[retVal];
];
(* ============================================== *)
If[!SilentRunValue, Print["CLM_Binomial::Inverse autocatalysis."]];
kSPtoAPfuncIdxMin = 1;
kSPtoAPfuncUnitIdx = 1;
kSPtoAPfuncIdxMax = 1;
kSPtoAPfuncLst := {};
(* ============================================== *)
kSPtoAPfunc[kSPtoAPfuncIdx_?IntegerQ, nL_?IntegerQ, ChainLen_?IntegerQ, rawOptions___] := 1;
(* ============================================== *)
(* Function to return derivative of PDF[BinomialDistribution] by p *)
DBinomialDistribution[nn_, pp_, kk_] := Module[{n, p, k, rule, \[CapitalDelta]p, expr1, expr0, diff, dbin},
  rule = {n -> nn, p -> pp, k -> kk};
  expr1 = Normal[Series[PDF[BinomialDistribution[n, p + \[CapitalDelta]p], k], {\[CapitalDelta]p, 0, 1}]];
  expr0 = Normal[Series[PDF[BinomialDistribution[n, p + \[CapitalDelta]p], k], {\[CapitalDelta]p, 0, 0}]];
  diff = FullSimplify[(expr1[[1]][[1, 1]] - expr0[[1]][[1, 1]]) / \[CapitalDelta]p];
  dbin = Piecewise[{{diff, expr1[[1]][[1, 2]]}}, 0] /. rule;
  Return[dbin];
];
(* ============================================== *)
If[!SilentRunValue,
  (
    Print["CLM_Binomial::Bifurcation parameters."];
    Print["..."];
  )
];

CalculateBifurcation[nn_?IntegerQ, ppVal_?NumericQ, roT_?NumericQ, rawOptions___] := Module[{kNumerator, kDenominator, retVal, pp, opts, kAtoSvalue},
(* Print["ppVal = ", N[ppVal], ", roT = ", N[roT]]; *)

  opts = ProcessOptions[rawOptions];
  kAtoSvalue = (kAtoS /. opts /. Options[CLMBinomial]);

  pp = If[ppVal > (1 / 2), ppVal, (1 - ppVal)];
  kNumerator = Sum[kAPtoSPfunc[kk, nn] * DBinomialDistribution[nn, pp, kk], {kk, 0, nn}] * roT;
  kDenominator = 2 * Sum[kAPtoSPfunc[kk, nn] * PDF[BinomialDistribution[nn, pp], kk], {kk, 0, nn}] * roT;
  retVal = (kNumerator) / (2 * kAtoSvalue + kDenominator);
  Print["pp = ", N[pp], ", kNumerator = ", N[kNumerator], ", kDenominator = ", N[kDenominator], ", kAtoS = ", N[kAtoSvalue], ", kNumerator/(2*kAtoS+kDenominator) = ", N[retVal]];
  Return[retVal];
];
(* ============================================== *)
pLfunc[roL_?NumericQ, roD_?NumericQ] := SetPrecision[((1 + nuFunc[roL, roD]) / 2), precisionVal];
pLfuncSymb[roL_, roD_] := ((1 + nuFuncSymb[roL, roD]) / 2);
(* ============================================== *)
nuFunc[roL_?NumericQ, roD_?NumericQ] := Module[{nuVal},
  nuVal = 0;
  If[roL > 0 && roD > 0, SetPrecision[nuVal = (roL - roD) / (roL + roD), precisionVal]];
  Return[nuVal];
];
(* ============================================== *)
nuFuncSymb[roL_, roD_] := Module[{nuVal},
  nuVal = nuVal = (roL - roD) / (roL + roD);
  Return[nuVal];
];
(* ============================================== *)
GetRoA[vars : {_, _, _, _, _}] := vars[[1]];
GetRoL[vars : {_, _, _, _, _}] := vars[[2]];
GetRoD[vars : {_, _, _, _, _}] := vars[[3]];
GetRoChain[vars : {_, _, _, _, _}] := vars[[4]];
GetCoefficients[vars : {_, _, _, _, _}] := vars[[5]];
(* ============================================== *)
GetRoAtotal[vars : {_, _, _, _, _}] := Module[{roA, roL, roD, roChainLst, coeffs, nn, roT, ii, roAtotal},
  roA = GetRoA[vars];
  roL = GetRoL[vars];
  roD = GetRoD[vars];
  roChainLst = GetRoChain[vars];
  coeffs = GetCoefficients[vars];

  nn = Length[roChainLst] - 1;

  roT = Sum[roChainLst[[ii]], {ii, 1, nn + 1}];

  roAtotal = roA + roL + roD + nn * roT;
  Return[roAtotal];
];
(* ============================================== *)
PrintRoAllInfo[msg_, vars : {_, _, _, _, _}] := PrintRoAllInfo[msg, vars, False];
PrintRoAllInfo[msg_, vars : {_, _, _, _, _}, forcePrint_?BooleanQ] := Module[{roA, roL, roD, roChainLst, coeffs, nn, roT, \[Eta]Val},
  If[(PrintDebugInfo || forcePrint), (
    roA = GetRoA[vars];
    roL = GetRoL[vars];
    roD = GetRoD[vars];
    roChainLst = GetRoChain[vars];
    coeffs = GetCoefficients[vars];

    nn = Length[roChainLst] - 1;
    roT = Sum[roChainLst[[ii]], {ii, 1, nn + 1}];
    \[Eta]Val = If[(roL + roD) > 0, N[(roL - roD) / (roL + roD)], 0];

    Print[ToString[msg], "::roA (total) = ", N[GetRoAtotal[vars]], ", roA = ", N[roA], ", roL = ", N[roL], ", roD = ", N[roD], ", \[Eta] = ", \[Eta]Val, ", roT*NNN = ", N[nn * roT], ", (roA+roL+roD+NNN*roT) = ", N[(roA + roL + roD + nn * roT)], ", NNN = ", nn];
  )];
];
(* ============================================== *)
(* Functions that makes random chains of length nn of the initial distribution of Probability(L) = pL *)
(* chainLst[[1]] - all D, chainLst[[nn+1]] - all L *)
ChainFormationStep[vars : {_, _, _, _, _}, rawOptions___] := Module[{roA, roL, roD, roChainLst, coeffs, roChainNewLst, nn, k, pL, roLtot, roDtot, roLchain, roDchain, roLDtotal, sol, roT, roTheta, eqLst, roTval, roThetaVal, roLnew, roDnew, retVal, opts, kStoTvalue, kTtoSvalue, NChainvalue},

  opts = ProcessOptions[rawOptions];
  kStoTvalue = (kStoT /. opts /. Options[CLMBinomial]);
  kTtoSvalue = (kTtoS /. opts /. Options[CLMBinomial]);
  NChainvalue = (NChain /. opts /. Options[CLMBinomial]);

  PrintRoAllInfo["    ChainFormationStep::Starting", vars, PrintEachStepInfo];

  roA = GetRoA[vars];
  roL = GetRoL[vars];
  roD = GetRoD[vars];
  roChainLst = GetRoChain[vars];
  coeffs = GetCoefficients[vars];

  nn = Length[roChainLst] - 1;

  (* Number of L and D in all chains *)
  roLchain = Sum[k * roChainLst[[k + 1]], {k, 0, nn}];
  roDchain = Sum[(nn - k) * roChainLst[[k + 1]], {k, 0, nn}];

  roLtot = roL + roLchain;
  roDtot = roD + roDchain;

  roLDtotal = roLtot + roDtot;

  eqLst = SetPrecision[{kStoTvalue * roTheta^NChainvalue - kTtoSvalue * roT == 0, roTheta + nn * roT == roLDtotal}, precisionVal];
  sol = NSolve[eqLst, {roT, roTheta}][[NChainvalue]];

  roTval = roT /. sol;
  roThetaVal = roTheta /. sol;

  If[PrintDetails || PrintEachStepInfo, Print["    ChainFormationStep::roLchain = ", N[roLchain], ", roDchain = ", N[roDchain], ", roLtot = ", N[roLtot], ", roDtot = ", N[roDtot]]];

  pL = pLfunc[roLtot, roDtot];
  roChainNewLst = N[Table[roTval * PDF[BinomialDistribution[nn, pL], k], {k, 0, nn}], precisionVal];

  roLnew = pL * roThetaVal;
  roDnew = (1 - pL) * roThetaVal;

  If[PrintDetails || PrintEachStepInfo, Print["    ChainFormationStep::roChainNewLst = ", N[roChainNewLst]]];

  retVal = SetPrecision[{roA, roLnew, roDnew, roChainNewLst, coeffs}, precisionVal];

  PrintRoAllInfo["    ChainFormationStep::Ending", retVal, PrintEachStepInfo];

  Return[retVal];
];
(* ============================================== *)
PDFDeltaDistribution[nn_, pp_, kk_] := If[kk == Floor[pp * nn], 1 + Floor[pp * nn] - pp * nn, If[kk == Floor[pp * nn] + 1, 1 - (1 + Floor[pp * nn] - pp * nn), 0]];
PDFDeltaDistribution1[nn_, pp_, kk_] := If[kk == Round[pp * nn], 1, 0];
(* ============================================== *)
CalculateAutocatalysisAmplification[ChainLen_?IntegerQ, ppVal_?NumericQ, IdxkAPtoSPfunc_?IntegerQ, rawOptions___] := Module[{pp, roChainLst, roA0, vars, coeffs, printEachStepInfoHlp, printDetailsHlp, roL, roD, ppOut, retVal, nn, opts, roT, zeroEpsVal, useDeltaDistributionVal},
  nn = ChainLen;

  opts = ProcessOptions[rawOptions];

  roT = roTdefaultValue /. opts /. Options[CLMBinomial];
  zeroEpsVal = zeroEps /. opts /. Options[CLMBinomial];
  useDeltaDistributionVal = UseDeltaDistribution /. opts /. Options[CLMBinomial];
  (*
Print["CalculateAutocatalysisAmplification::opts = ", opts];
Print["CalculateAutocatalysisAmplification::zeroEpsVal = ", zeroEpsVal];
*)
  (* Print["ppVal = ", N[ppVal], ", roT = ", N[roT]]; *)
  (* pp=If[ppVal > (1/2),ppVal,(1-ppVal)]; *)
  pp = Max[Min[ppVal, 1], 0];

  printEachStepInfoHlp = PrintEachStepInfo;
  printDetailsHlp = PrintDetails;

  kAPtoSPlstVal = Table[kAPtoSPfunc[IdxkAPtoSPfunc, ii, nn, rawOptions], {ii, 0, nn}];
  kSPtoAPlstVal = Table[kSPtoAPfunc[IdxkAPtoSPfunc, ii, nn, rawOptions], {ii, 0, nn}];

  PrintEachStepInfo = False;
  PrintDetails = False;

  roA0 = 1;
  coeffs = {};
  roChainLst = If[!useDeltaDistributionVal, N[Table[roT * PDF[BinomialDistribution[nn, pp], k], {k, 0, nn}], precisionVal], N[Table[roT * PDFDeltaDistribution1[nn, pp, k], {k, 0, nn}], precisionVal]];

  vars = {roA0, 0, 0, roChainLst, coeffs};

  vars = AutocatalysisStep[vars, rawOptions];
  roL = GetRoL[vars];
  roD = GetRoD[vars];

  ppOut = pLfunc[roL, roD];

  retVal = If[(pp - (1 / 2)) != 0, (((ppOut - (1 / 2)) / (pp - (1 / 2))) - 1), (1 / 2) * (CalculateAutocatalysisAmplification[nn, (1 / 2) + zeroEpsVal, IdxkAPtoSPfunc, rawOptions] + CalculateAutocatalysisAmplification[nn, (1 / 2) - zeroEpsVal, IdxkAPtoSPfunc, rawOptions])];

  PrintEachStepInfo = printEachStepInfoHlp;
  PrintDetails = printDetailsHlp;

  Return[retVal];
];
(* ============================================== *)
(* Function that returns concentrations of L and D after autocatalysis, direct synthesis and direct decay *)
AutocatalysisStep[vars : {_, _, _, _, _}, rawOptions___] := Module[{roA, roL, roD, roChainLst, coeffs, nn, ii, roLnumerator, roLdenominator, roDnumerator, roDdenominator, cL, cD, roAnew, roLnew, roDnew, retVal, opts, kAtoSvalue, kLtoAvalue, kDtoAvalue},

  PrintRoAllInfo["    AutocatalysisStep::Starting", vars, PrintEachStepInfo];

  opts = ProcessOptions[rawOptions];
  kAtoSvalue = (kAtoS /. opts /. Options[CLMBinomial]);
  kLtoAvalue = ((kLtoA /. kStoArule) /. opts /. Options[CLMBinomial]);
  kDtoAvalue = ((kDtoA /. kStoArule) /. opts /. Options[CLMBinomial]);

  roA = GetRoA[vars];
  roL = GetRoL[vars];
  roD = GetRoD[vars];
  roChainLst = GetRoChain[vars];
  coeffs = GetCoefficients[vars];

  nn = Length[roChainLst] - 1;

  (* Print["AutocatalysisStep::kAtoSvalue = ", N[kAtoSvalue], ", Sum[roChainLst[[ii]],{ii,1,nn+1}] = ", N[Sum[roChainLst[[ii]],{ii,1,nn+1}]]]; *)

  If[PrintDetails || PrintEachStepInfo,
    (
      SetLegends[{"roChainLst"}];
      Print[DiscretePlot[roChainLst[[k]], {k, 1, nn + 1}, Evaluate[discrPlotOpts]]];

      SetLegends[{"kAPtoSPlstVal"}];
      Print[DiscretePlot[{kAPtoSPlstVal[[k]], kAPtoSPlstVal[[nn + 2 - k]]}, {k, 1, nn + 1}, Evaluate[discrPlotOpts]]];

      SetLegends[{"roLnumerator"}];
      Print[DiscretePlot[kAPtoSPlstVal[[k]] * roChainLst[[k]], {k, 1, nn + 1}, Evaluate[discrPlotOpts]]];

      SetLegends[{"roDnumerator"}];
      Print[DiscretePlot[kAPtoSPlstVal[[nn + 2 - k]] * roChainLst[[k]], {k, 1, nn + 1}, Evaluate[discrPlotOpts]]];
    )
  ];

  roLnumerator = SetPrecision[kAtoSvalue + Sum[kAPtoSPlstVal[[ii]] * roChainLst[[ii]], {ii, 1, nn + 1}], precisionVal];
  roLdenominator = SetPrecision[kLtoAvalue + Sum[kSPtoAPlstVal[[ii]] * roChainLst[[ii]], {ii, 1, nn + 1}], precisionVal];
  cL = SetPrecision[roLnumerator / roLdenominator, precisionVal];

  roDnumerator = SetPrecision[kAtoSvalue + Sum[kAPtoSPlstVal[[nn + 2 - ii]] * roChainLst[[ii]], {ii, 1, nn + 1}], precisionVal];
  roDdenominator = SetPrecision[kDtoAvalue + Sum[kSPtoAPlstVal[[nn + 2 - ii]] * roChainLst[[ii]], {ii, 1, nn + 1}], precisionVal];
  cD = SetPrecision[roDnumerator / roDdenominator, precisionVal];

  roAnew = N[(roA + roL + roD) / (1 + cL + cD), precisionVal];

  roLnew = N[cL * roAnew, precisionVal];
  roDnew = N[cD * roAnew, precisionVal];

  If[PrintDetails || PrintEachStepInfo,
    (
      Print["    AutocatalysisStep::roLnumerator = ", N[roLnumerator], ", roLdenominator = ", N[roLdenominator], ", cL = ", N[cL], ", roDnumerator = ", N[roDnumerator], ", roDdenominator = ", N[roDdenominator], ", cD = ", N[cD], ", cL/cD = ", N[cL / cD]];
      Print["    AutocatalysisStep::roLnew = ", N[roLnew], ", roDnew = ", N[roDnew], ", roAnew = ", N[roAnew], ", (roAnew+roLnew+roDnew) = ", N[(roAnew + roLnew + roDnew)]];
    )
  ];

  retVal = SetPrecision[{roAnew, roLnew, roDnew, roChainLst, coeffs}, precisionVal];
  PrintRoAllInfo["    AutocatalysisStep::Ending", retVal, PrintEachStepInfo];

  Return[retVal];
];
(* ============================================== *)
DestroyLDstep[vars : {_, _, _, _, _}, rawOptions___] := Module[{roA, roL, roD, roChainLst, coeffs, roAnew, roLnew, roDnew, retVal, DestroyLDPctVal},
  PrintRoAllInfo["    DestroyLDstep::Starting", vars, PrintEachStepInfo];

  roA = GetRoA[vars];
  roL = GetRoL[vars];
  roD = GetRoD[vars];
  roChainLst = GetRoChain[vars];
  coeffs = GetCoefficients[vars];

  roAnew = roA;
  roLnew = roL;
  roDnew = roD;

  If[DestroyLD,
    (
      DestroyLDPctVal = Max[Min[Abs[DestroyLDPct], 1], 0];
      Print["    DestroyLDstep::Destroying LD..., DestroyLDPctVal = ", N[DestroyLDPctVal]];
      roAnew = roA + DestroyLDPctVal * (roL + roD);
      roLnew = (1 - DestroyLDPctVal) * roL;
      roDnew = (1 - DestroyLDPctVal) * roD;
    )
  ];

  retVal = SetPrecision[{roAnew, roLnew, roDnew, roChainLst, coeffs}, precisionVal];
  PrintRoAllInfo["    DestroyLDstep::Ending", retVal, PrintEachStepInfo];

  Return[retVal];
];
(* ============================================== *)
DestroyMstep[vars : {_, _, _, _, _}, rawOptions___] := Module[{roA, roL, roD, roChainLst, coeffs, roAret, roChainRetLst, nn, roT, ii, retVal, DestroyMPctVal},
  PrintRoAllInfo["    DestroyMstep::Starting", vars, PrintEachStepInfo];

  roA = GetRoA[vars];
  roL = GetRoL[vars];
  roD = GetRoD[vars];
  roChainLst = GetRoChain[vars];
  coeffs = GetCoefficients[vars];

  roAret = roA;
  roChainRetLst = roChainLst;

  If[DestroyM,
    (
      DestroyMPctVal = Max[Min[Abs[DestroyMPct], 1], 0];
      Print["    DestroyMstep::Destroying M..., DestroyMPctVal = ", N[DestroyMPctVal]];
      nn = Length[roChainLst] - 1;
      roT = DestroyMPctVal * Sum[roChainLst[[ii]], {ii, 1, nn + 1}];
      roAret = roA + nn * roT;
      roChainRetLst = Table[(1 - DestroyMPctVal) * roChainLst[[ii]], {ii, 1, nn + 1}];
    )
  ];

  retVal = SetPrecision[{roAret, roL, roD, roChainRetLst, coeffs}, precisionVal];
  PrintRoAllInfo["    DestroyMstep::Ending", retVal, PrintEachStepInfo];

  Return[retVal];
];
(* ============================================== *)
ProcessStep[idx_?IntegerQ, vars : {_, _, _, _, _}, rawOptions___] := Module[{varsNewAC, roLac, roDac, roChainACLst},
  varsNewAC = vars;
  roLac = GetRoL[varsNewAC];
  roDac = GetRoD[varsNewAC];
  roChainACLst = GetRoChain[varsNewAC];
  nuValLst[[idx + 1]] = N[nuFunc[roLac, roDac]];
  roLDistributionAll[[idx]] = Table[PDF[BinomialDistribution[NNN, pLfunc[roLac, roDac]], k], {k, 0, NNN}];

  If[Mod[idx, idxMult] == 0,
    (
      cnt++;
      roLDistribution[[cnt]] = Table[PDF[BinomialDistribution[NNN, pLfunc[roLac, roDac]], k], {k, 0, NNN}];
    )
  ];
  Return[];
];
(* ============================================== *)
PrintStep[idx_?IntegerQ, varsNew_, varsNewAC_, varsNewDM_, varsNewDLD_, varsNewCF_] := Module[{roA, roL, roD, roChainLst, coeffs, pLnew, nn, retVal, k, ii, roT, roChainACLst, roTac, roLac, roDac},
  If[Mod[idx, PrintCount] == 0,
    (
      roA = GetRoA[varsNew];
      roL = GetRoL[varsNew];
      roD = GetRoD[varsNew];
      roChainLst = GetRoChain[varsNew];
      coeffs = GetCoefficients[varsNew];

      roLac = GetRoL[varsNewAC];
      roDac = GetRoD[varsNewAC];
      roChainACLst = GetRoChain[varsNewAC];

      nn = Length[roChainLst] - 1;
      roT = Sum[roChainLst[[ii]], {ii, 1, nn + 1}];
      roTac = Sum[roChainACLst[[ii]], {ii, 1, nn + 1}];

      Print["idx = ", idx];
      (*
PrintRoAllInfo["MakeAllSteps::AutocatalysisStep",varsNewAC,True];
PrintRoAllInfo["MakeAllSteps::DestroyMstep",varsNewDM,True];
PrintRoAllInfo["MakeAllSteps::DestroyLDstep",varsNewDLD,True];
PrintRoAllInfo["MakeAllSteps::ChainFormationStep",varsNewCF,True];
PrintRoAllInfo["MakeAllSteps::AllSteps",varsNew,True];
*)
      Print["\[Eta] = ", N[nuFunc[roLac, roDac]], ", roLac = ", N[roLac], ", roDac = ", N[roDac]];

      Print["roChainACLst[[k]], PDF[BinomialDistribution[nn,pLfunc[roLac,roDac]],k]"];
      SetLegends[{"\[Rho]"}];
      Print[DiscretePlot[roChainACLst[[k + 1]], {k, 0, nn}, Evaluate[discrPlotOpts]]];
      SetLegends[{"p"}];
      Print[DiscretePlot[PDF[BinomialDistribution[nn, pLfunc[roLac, roDac]], k], {k, 0, nn}, Evaluate[discrPlotOpts]]];
      Print["nuValLst"];
      SetLegends[{"\[Eta]"}];
      Print[DiscretePlot[nuValLst[[ii]], {ii, 1, idx}, Evaluate[discrPlotOpts]]];
      SetLegends[{"Log[\[Eta]]"}];
      Print[DiscretePlot[-Log[Abs[nuValLst[[ii]]]], {ii, 1, idx}, Evaluate[discrPlotOpts]]];
      SetLegends[{"D[Log[\[Eta]]]"}];
      Print[DiscretePlot[-Log[Abs[nuValLst[[ii]]]] + Log[Abs[nuValLst[[ii + 1]]]], {ii, 1, idx - 1}, Evaluate[discrPlotOpts]]];

      CalculateBifurcation[pLfunc[roLac, roDac], roTac];
      PrintTimeUsed[];
      Print[sep];
    )
  ];

  Return[];
];
(* ============================================== *)
MakeAllSteps[idx_?IntegerQ, vars : {_, _, _, _, _}, rawOptions___] := Module[{varsNewAC, varsNew, varsNewDM, varsNewDLD, varsNewCF},
  varsNew = vars;
  Print[sep];
  Print[sep];
  PrintRoAllInfo["MakeAllSteps::Starting", varsNew, PrintEachStepInfo];

  (*
varsNew=AutocatalysisStep[varsNew];
varsNewAC=varsNew;
ProcessStep[idx,varsNew];


varsNew=DestroyMstep[varsNew];
varsNewDM=varsNew;

varsNew=ChainFormationStep[varsNew];
varsNewCF=varsNew;

varsNew=DestroyLDstep[varsNew];
varsNewDLD=varsNew;
*)

  varsNew = ChainFormationStep[varsNew];
  PrintRoAllInfo["MakeAllSteps::ChainFormationStep", varsNew, PrintEachStepInfo];
  varsNewCF = varsNew;

  varsNew = DestroyLDstep[varsNew];
  PrintRoAllInfo["MakeAllSteps::DestroyLDstep", varsNew, PrintEachStepInfo];
  varsNewDLD = varsNew;

  varsNew = AutocatalysisStep[varsNew];
  PrintRoAllInfo["MakeAllSteps::AutocatalysisStep", varsNew, PrintEachStepInfo];
  varsNewAC = varsNew;
  ProcessStep[idx, varsNew];

  varsNew = DestroyMstep[varsNew];
  PrintRoAllInfo["MakeAllSteps::DestroyMstep", varsNew, PrintEachStepInfo];
  varsNewDM = varsNew;


  (*
varsNew=ChainFormationStep[varsNew];
varsNewCF=varsNew;

varsNew=DestroyMstep[varsNew];
varsNewDM=varsNew;

varsNew=DestroyLDstep[varsNew];
varsNewDLD=varsNew;
*)

  (*
varsNew=DestroyMstep[varsNew];
varsNewDM=varsNew;

varsNew=DestroyLDstep[varsNew];
varsNewDLD=varsNew;

varsNew=ChainFormationStep[varsNew];
varsNewCF=varsNew;
*)

  (*
varsNew=ChainFormationStep[varsNew];
varsNewCF=varsNew;

varsNew=DestroyMstep[varsNew];
varsNewDM=varsNew;

varsNew=DestroyLDstep[varsNew];
varsNewDLD=varsNew;
*)

  PrintStep[idx, varsNew, varsNewAC, varsNewDM, varsNewDLD, varsNewCF];
  PrintRoAllInfo["MakeAllSteps::Ending", varsNew];

  Return[varsNew];
];

(* ============================================== *)
If[!SilentRunValue, Print["CLM_Binomial::TODO: Finish BinomialRun."]];
BinomialRun[rawOptions___] := Module[{},
  If[idxMax > 0,
    (
      pLInit = (1 + nuInit) / 2;
      roL = pLInit * roTot;
      roD = (1 - pLInit) * roTot;


      (*
roA=roA0;
roL=0;
roD=0;
*)

      roApct = (61 / 100);
      \[Eta]0 = 8 / 10;
      roA = roApct * roA0;
      roL = (1 / 2) * (1 - roApct) * roA0 * (1 + \[Eta]0);
      roD = (1 / 2) * (1 - roApct) * roA0 * (1 - \[Eta]0);


      roChainLst = Table[0, {ii, 1, NNN + 1}];
      coeffs = {};
      vars = {roA, roL, roD, roChainLst, coeffs};

      nuValLst = Table[Indeterminate, {ii, 0, idxMax}];
      roLDistribution = Table[Indeterminate, {idxCount, 0, idxMaxCount + 2}, {idxNNN, 0, NNN}];
      roLDistributionAll = Table[Indeterminate, {idxCount, 0, idxMax + 1}, {idxNNN, 0, NNN}];

      (* ============================================== *)

      cnt = 1;
      Print["idx = ", 0];
      Print["\[Eta] = ", N[nuFunc[roL, roD]], ", roL = ", N[roL], ", roD = ", N[roD] (*, ", roChainLst = ", roChainLst *)];
      SetLegends[{"p"}];
      Print[DiscretePlot[PDF[BinomialDistribution[NNN, pLfunc[roL, roD]], k], {k, 0, NNN}, Evaluate[discrPlotOpts]]];
      nuValLst[[1]] = N[nuFunc[roL, roD]];
      Print[sep];

      roLDistribution[[cnt]] = Table[PDF[BinomialDistribution[NNN, pLfunc[roL, roD]], k], {k, 0, NNN}];

      (* ============================================== *)

      For[idx = 1, idx <= idxMax, idx++,
        (
          vars = MakeAllSteps[idx, vars];

          roA = GetRoA[vars];
          roL = GetRoL[vars];
          roD = GetRoD[vars];
          roChainLst = GetRoChain[vars];
          coeffs = GetCoefficients[vars];
        )
      ];

      (* ============================================== *)
      Print["nuValLst"];
      SetLegends[{"\[Eta]"}];
      Print[DiscretePlot[nuValLst[[ii]], {ii, 1, idxMax}, Evaluate[discrPlotOpts]]];

      (* ============================================== *)
      (*
Print["roLDistribution"];
Print[N[roLDistribution] // MatrixForm];
Print[strSeparator];
*)
      (* ============================================== *)

      Print["roLDistributionAll"];
      roLDistributionAllFunc[ii_?IntegerQ, jj_?IntegerQ] := roLDistribution[[ii, jj]];
      SetLegends[Table["p(" <> ToString[idx] <> ")", {idx, idxLst}]];
      Print[DiscretePlot[Evaluate@Table[roLDistributionAllFunc[idx + 1, kk + 1 + NNN / 2], {idx, idxLst}], {kk, -NNN / 2, NNN / 2}, Evaluate[discrPlotOpts]]];
      Print[strSeparator];

      (* ============================================== *)

      roLDistributionFunc[ii_?IntegerQ, jj_?IntegerQ] := roLDistribution[[ii, jj]];

      Print[DiscretePlot3D[roLDistributionFunc[idxCnt, idxNNN + 1 + NNN / 2], {idxNNN, -NNN / 2, NNN / 2}, {idxCnt, 1, idxMaxCount}, Evaluate[discrPlot3DOpts]]];
      Print[strSeparator];
    )
  ];
];
(* ============================================== *)
(* Function to find positve zero root *)
ZeroRoot[IsZeroStable_?BooleanQ, IdxkAPtoSPfunc_?IntegerQ, ChainLen_?IntegerQ, rawOptions___] := Module[{retVal, sol, nu, startVal, funcVal, func, nuStart, nuEnd, nuStep, Nnu, valTbl, cmpVal, useFindRoot, nuu, opts, frEpsVal},

(*
Print["ZeroRoot:: IdxkAPtoSPfunc = ",IdxkAPtoSPfunc, ", ChainLen = ", ChainLen, ", frEpsVal = ", frEpsVal];
Print["ZeroRoot:: rawOptions = ",rawOptions];

Print[Plot[(CalculateAutocatalysisAmplification[ChainLen,(1+nu)/2,roTval,IdxkAPtoSPfunc,rawOptions]-1),{nu,-1,1},Evaluate[plotOpts]]];
Print[Plot[Log[Abs[(CalculateAutocatalysisAmplification[ChainLen,(1+nu)/2,roTval,IdxkAPtoSPfunc,rawOptions]-1)]],{nu,-1,1},Evaluate[plotOpts]]];
PrintTimeUsed[];

Print["ZeroRoot:: Definition[func] = ", Definition[func]];

*)

  opts = ProcessOptions[rawOptions];
  frEpsVal = frEps /. opts /. Options[CLMBinomial];

  func[nuu_?NumericQ] := (CalculateAutocatalysisAmplification[ChainLen, (1 + nuu) / 2, IdxkAPtoSPfunc, rawOptions] - 1);

  nuStart = 0;
  nuEnd = (1 - frEpsVal);

  Nnu = 10;
  nuStep = (nuEnd - nuStart) / Nnu;

  valTbl = Table[{nu, func[nu]}, {nu, nuStart, nuEnd, nuStep}];
  (* Print["ZeroRoot::valTbl = ",N[valTbl] // MatrixForm]; *)

  useFindRoot = False;
  retVal = Indeterminate;

  If[valTbl[[1, 2]] * valTbl[[Nnu + 1, 2]] < 0,
    (
      If[IsZeroStable || ((! IsZeroStable) && valTbl[[1, 2]] < 0),
        (
          sol = FindRoot[func[nu], {nu, nuStart, nuEnd}, Method -> "Brent"];
          retVal = nu /. sol;
        (* Print["sol = ", sol]; *)
        )
      ];
    ),
    (
      If[valTbl[[1, 2]] >= 0 && valTbl[[Nnu + 1, 2]] >= 0 ,
        (
          retVal = If[IsZeroStable, 1, Indeterminate];
        ),
        (
          retVal = Indeterminate;
        )
      ];
    )
  ];

  (* Print["retVal = ", retVal]; *)

  (*
funcVal=(CalculateAutocatalysisAmplification[ChainLen,(1+retVal)/2,roTval,IdxkAPtoSPfunc,rawOptions]-1);

Print["sol = ", sol];
Print["funcVal = ", funcVal];
PrintTimeUsed[];
*)

  Return[retVal];
];
(* ============================================== *)




