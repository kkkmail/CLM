(* :Summary: CLM chain plot logic. *)
(* :Author: Konstantin K. Konstantinov *)
(* :Email: konstantin.k.konstantinov@gmail.com *)
(* :License type: GPL *)
(* :Copyright: K^3, 2013 - 2017 *)
(* :Version: 3.25 .001, Date : 2017/02/26 *)
(* :Mathematica Version: 10.0 *)
(* ============================================== *)
PlotDistribution[plotName_, distributionVec_?VectorQ, paramMatr_?MatrixQ] := PlotDistribution[plotName, distributionVec, paramMatr, DistributionPlotRange[distributionVec, paramMatr]];
(* ============================================== *)
PlotLogDistribution[plotName_, distributionVec_?VectorQ, paramMatr_?MatrixQ] := PlotLogDistribution[plotName, distributionVec, paramMatr, DistributionPlotRange[distributionVec, paramMatr]];
(* ============================================== *)
PlotDistribution[plotName_, distribution_, params_?VectorQ] := PlotDistribution[plotName, distribution, params, DistributionPlotRange[distribution, params]];
(* ============================================== *)
PlotLogDistribution[plotName_, distribution_, params_?VectorQ] := PlotLogDistribution[plotName, distribution, params, DistributionPlotRange[distribution, params]];
(* ============================================== *)
PlotDistribution[plotName_, distributionVec_?VectorQ, paramMatr_?MatrixQ, xMax_] := Module[{plotOpts, x, ii},
  plotOpts = {PlotRange -> All, Frame -> True, GridLines -> Automatic, Filling -> Axis};
  Print["Distribution for ", ToString[plotName], ": distributionVec = ", distributionVec // MatrixForm, ", paramMatr = ", N[paramMatr] // MatrixForm, ", final paramVals = ", N[Table[PrepareDistributionParameters[distributionVec, paramMatr, ii], {ii, 1, MaxChainLength}]] // MatrixForm, ", shiftVal = ", N[Table[PrepareDistributionShift[distributionVec, paramMatr, ii], {ii, 1, MaxChainLength}]] // MatrixForm];
  Print[Plot[Evaluate[Table[PDF[Apply[distributionVec[[ii]], PrepareDistributionParameters[distributionVec, paramMatr, ii]], x + PrepareDistributionShift[distributionVec, paramMatr, ii]], {ii, 1, MaxChainLength}]], {x, 0, xMax}, Evaluate[plotOpts]]];
  Print["..."];
];
(* ============================================== *)
PlotLogDistribution[plotName_, distributionVec_?VectorQ, paramMatr_?MatrixQ, xMax_] := Module[{plotOpts, x, ii},
  plotOpts = {Frame -> True, GridLines -> Automatic, Filling -> Axis};
  Print["Distribution for Log ", ToString[plotName], ": distributionVec = ", distributionVec // MatrixForm, ", paramMatr = ", N[paramMatr] // MatrixForm, ", final paramVals = ", N[Table[PrepareDistributionParameters[distributionVec, paramMatr, ii], {ii, 1, MaxChainLength}]] // MatrixForm, ", shiftVal = ", N[Table[PrepareDistributionShift[distributionVec, paramMatr, ii], {ii, 1, MaxChainLength}]] // MatrixForm];
  Print[Plot[Evaluate[Table[Log[PDF[Apply[distributionVec[[ii]], PrepareDistributionParameters[distributionVec, paramMatr, ii]], x + PrepareDistributionShift[distributionVec, paramMatr, ii]]], {ii, 1, MaxChainLength}]], {x, 0, xMax}, Evaluate[plotOpts]]];
  Print["..."];
];
(* ============================================== *)
PlotDistribution[plotName_, distribution_, params_?VectorQ, xMax_] := Module[{plotOpts, x, shiftVal},
  plotOpts = {PlotRange -> All, Frame -> True, GridLines -> Automatic, Filling -> Axis};
  shiftVal = PrepareDistributionShift[distribution, params];
  Print["Distribution for ", ToString[plotName], ": distribution = ", ToString[distribution], ", params = ", N[params], ", final paramVals = ", N[PrepareDistributionParameters[distribution, params]], ", shiftVal = ", N[shiftVal]];
  Print[Plot[PDF[Apply[distribution, PrepareDistributionParameters[distribution, params]], x + shiftVal], {x, 0, xMax}, Evaluate[plotOpts]]];
  Print["..."];
];
(* ============================================== *)
PlotLogDistribution[plotName_, distribution_, params_?VectorQ, xMax_] := Module[{plotOpts, x, shiftVal},
  plotOpts = {Frame -> True, GridLines -> Automatic, Filling -> Axis};
  shiftVal = PrepareDistributionShift[distribution, params];
  Print["Distribution for Log ", ToString[plotName], ": distribution = ", ToString[distribution], ", params = ", N[params], ", final paramVals = ", N[PrepareDistributionParameters[distribution, params]], ", shiftVal = ", N[shiftVal]];
  Print[Plot[Log[PDF[Apply[distribution, PrepareDistributionParameters[distribution, params]], x + shiftVal]], {x, 0, xMax}, Evaluate[plotOpts]]];
  Print["..."];
];
(* ============================================== *)
PlotAllDistributions[] := Module[{},
  If[InitializeSynthesisValue,
    (
      Print["Synthesis."];
      PlotDistribution["SynthCoeff", SynthCoeffDistribution, SynthCoeffParams];
      PlotDistribution["InvSynthCoeff", InvSynthCoeffDistribution, InvSynthCoeffParams];
      Print[strSeparator];
    )
  ];

  If[InitializeCatLigationValue,
    (
      Print["Catalytic synthesis."];
      PlotDistribution["CatSynthCoeff", CatSynthCoeffDistribution, CatSynthCoeffParams];
      PlotDistribution["InvCatSynthCoeff", InvCatSynthCoeffDistribution, InvCatSynthCoeffParams];
      Print[strSeparator];
    )
  ];

  If[InitializeActivationValue,
    (
      Print["Activation."];
      PlotDistribution["ActivationCoeff", ActivationCoeffDistribution, ActivationCoeffParams];
      PlotDistribution["DeactivationCoeff", DeactivationCoeffDistribution, DeactivationCoeffParams];
      Print[strSeparator];
    )
  ];

  If[InitializeLigationValue,
    (
      Print["Ligation."];
      PlotDistribution["LigTotalRate", LigTotalRateDistribution, LigTotalRateParams];
      PlotDistribution["LigCoeff", LigCoeffDistribution, LigCoeffParams];

      PlotDistribution["InvLigTotalRate", InvLigTotalRateDistribution, InvLigTotalRateParams];
      PlotDistribution["InvLigCoeff", InvLigCoeffDistribution, InvLigCoeffParams];
      Print[strSeparator];
    )
  ];

  If[InitializeCatLigationValue,
    (
      Print["Catalytic ligation."];
      PlotDistribution["CatLigCoeff", CatLigCoeffDistribution, CatLigCoeffParams];
      PlotDistribution["InvCatLigCoeff", InvCatLigCoeffDistribution, InvCatLigCoeffParams];
      Print[strSeparator];
    )
  ];

  If[InitializeEpimerizationValue,
    (
      Print["Epimerization."];
      PlotDistribution["EpimTotalRate", EpimTotalRateDistribution, EpimTotalRateParams];
      PlotDistribution["EpimCoeff", EpimCoeffDistribution, EpimCoeffParams];
      Print[strSeparator];
    )
  ];

  If[InitializeCrystallizationValue,
    (
      Print["Crystallization and related."];
      PlotDistribution["DiastCoeff", DiastCoeffDistribution, DiastCoeffParams];
      PlotDistribution["InvDiastCoeff", InvDiastCoeffDistribution, InvDiastCoeffParams];

      PlotDistribution["rMaxCoeff", rMaxCoeffDistribution, rMaxCoeffParams];
      PlotDistribution["kCrystCoeff", kCrystCoeffDistribution, kCrystCoeffParams];
      PlotDistribution["kDissCoeff", kDissCoeffDistribution, kDissCoeffParams];

      PlotDistribution["CrystDecayCoeff", CrystDecayCoeffDistribution, CrystDecayCoeffParams];
      Print[strSeparator];
    )
  ];
];
(* ============================================== *)
PlotAllLogDistributions[] := Module[{},
  PlotLogDistribution["CatSynthCoeff", CatSynthCoeffDistribution, CatSynthCoeffParams];
  PlotLogDistribution["InvCatSynthCoeff", InvCatSynthCoeffDistribution, InvCatSynthCoeffParams];

  PlotLogDistribution["LigCoeff", LigCoeffDistribution, LigCoeffParams];
  PlotLogDistribution["InvLigCoeff", InvLigCoeffDistribution, InvLigCoeffParams];

  PlotLogDistribution["CatLigCoeff", CatLigCoeffDistribution, CatLigCoeffParams];
  PlotLogDistribution["InvCatLigCoeff", InvCatLigCoeffDistribution, InvCatLigCoeffParams];
];
(* ============================================== *)