(* ============================================== *)
(* :Summary: CLM evolution generator module. *)
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
EvolutionGeneratorVersion = "3.26.001";
(* ============================================== *)
Options[CLMEvolutionGenerator] :=
    {
      CalculateSubstDisplMatrix -> True
    };
(* ============================================== *)
EvRunTypeNone = 0;
EvRunTypeAll = 1;
EvRunTypeList = 2;
EvRunTypeRange = 3;
(* ============================================== *)
IsInitializedEvol = False;
PrintInfoEval = False;
RunChainModelPlotRangeValEval = Automatic;
CalculateSubstDisplMatrixValue = CalculateSubstDisplMatrix /. Options[CLMEvolutionGenerator];
(* ============================================== *)
SubstLstEvolNameLevelFunc[maxChainLen_?IntegerQ] := Module[{retVal, ii, len, prev},
  If[maxChainLen <= 1,
    (
      retVal = {"A"};
    ),
    (
      prev = SubstLstEvolNameLevelFunc[maxChainLen - 1];
      len = Length[prev];
      retVal = Flatten[Table[{prev[[ii]] <> "A", prev[[ii]] <> "a"}, {ii, 1, len}]];
    )
  ];

  Return[retVal];
];
(* ============================================== *)
SubstLstEvolNameFunc[maxChainLen_?IntegerQ] := Module[{nameLst, ii},
  nameLst = Flatten[Table[SubstLstEvolNameLevelFunc[ii], {ii, 1, maxChainLen}]];
  (* Print["nameLst = ", nameLst // MatrixForm]; *)
  Return[nameLst];
];
(* ============================================== *)
SubstLstEvolFunc[maxChainLen_?IntegerQ] := Module[{nameLst, len, ii, retVal},
  nameLst = Flatten[Table[SubstLstEvolNameLevelFunc[ii], {ii, 1, maxChainLen}]];
  (* Print["nameLst = ", nameLst // MatrixForm]; *)
  len = Length[nameLst];
  retVal = Table[ToExpression["idx" <> nameLst[[ii]]], {ii, 1, len}];
  Return[retVal];
];
(* ============================================== *)
EvolutionInitialize[maxChainLen_?IntegerQ, useActivation_?BooleanQ, useNNT_?BooleanQ, useDirectCryst_?BooleanQ] := EvolutionInitialize[maxChainLen, useActivation, useNNT, useDirectCryst, True];

EvolutionInitialize[maxChainLen_?IntegerQ, useActivation_?BooleanQ, useNNT_?BooleanQ, useDirectCryst_?BooleanQ, runNDSolveVal_?BooleanQ, rawOpts___] := Module[{len, ii, lenPair, s, opts},
  Print["EvolutionGeneratorVersion = ", EvolutionGeneratorVersion];
  opts = ProcessOptions[rawOpts];
  CalculateSubstDisplMatrixValue = CalculateSubstDisplMatrix /. opts /. Options[CLMEvolutionGenerator];

  Print[strSeparator];

  RunNDSolve = runNDSolveVal;
  MaxChainLenValEvol = maxChainLen;
  UseActivationValEvol = useActivation;
  UseNNTValEvol = useNNT;
  UseDirectCrystEvol = useDirectCryst;
  PlotSubstancesValEval = False;
  EvolutionRunCallLst = {};
  EvolutionCount = 0;

  plotDistributionsVal = False;
  MaxDetailedOutputChainLen = 1;

  OutputMonitor$ChiralPolarization = True;
  OutputMonitor$Substances = False;
  OutputMonitor$Distributions = False;
  OutputType$Eval = OutputTypeCPandRho;

  (* SubstLstEvol:={idxA, idxAA, idxAa, idxAAA, idxAAa, idxAaA, idxAaa}; *)
  SubstLstEvol = SubstLstEvolFunc[MaxChainLenValEvol];
  len = Length[SubstLstEvol];
  EnantSubstLstEvol = Table[EnantiomerSubstanceID[SubstLstEvol[[ii]]], {ii, 1, len}];
  (* Print["EvolutionInitialize::SubstLstEvol = ", Table[{ii,SubstLstEvol[[ii]]},{ii,1,len}] // MatrixForm]; *)

  SubstPairsLstEvol = Flatten[Table[{SubstLstEvol[[jj]], SubstLstEvol[[ii]]}, {ii, 1, len}, {jj, 1, ii}], 1];
  EnantSubstPairsLstEvol = Flatten[Table[{SubstLstEvol[[jj]], EnantSubstLstEvol[[ii]]}, {ii, 1, len}, {jj, 1, ii}], 1];
  SubstPairsLstCntEvol = Length[SubstPairsLstEvol];
  (* Print["EvolutionInitialize::SubstPairsLstEvol = ", SubstPairsLstEvol // MatrixForm]; *)

  chopDelta = 3 * 10^-2;

  c\:02d6 = 10^-2;
  c\:02d7 = 10^-3;

  a\:02d6 = 1;
  a\:02d7 = 10^-3;

  \[CapitalLambda]\:02d6 = 1;
  \[CapitalLambda]\:02d7 = 1;

  \[CapitalGamma]0\:02d6 = 1 * 10^1;
  \[CapitalGamma]0\:02d7 = 1 * 10^-1;
  \[Gamma]0\:02d6 = 0.01;
  \[Gamma]0\:02d7 = 0.01;

  \[CapitalPi]\:02d6 = 10;
  \[CapitalPi]\:02d7 = 10;
  \[Pi]\:02d6 = 0;
  \[Pi]\:02d7 = 0;

  \[CapitalSigma]\:02d6 = 10^3;
  \[CapitalSigma]\:02d7 = 10^-2;
  \[CapitalDelta]\:02d7 = 10^-3;
  \[Rho]\:02d7 = 10^-3;
  \[Rho]\:02d6 = 10^2;

  \[CapitalSigma]\:02d7\:02d7 = 10^3;

  GeneratorInitializeModel[maxChainLen, useActivation, useNNT];
  MaxCatCntEvol = If[CatSynthEnantSel == EnantSelNoReaction && InvCatSynthEnantSel == EnantSelNoReaction, 1, len];

  If[maxChainLen <= 5 && CalculateSubstDisplMatrixValue,
    (
      Print["EvolutionInitialize::Initializing display matrices..."];
      substDisplMatrix = If[MaxCatCntEvol == 1, (Table[{jj, TensorProduct[GetSubstanceName[SubstPairsLstEvol[[jj, 1]]] , GetSubstanceName[SubstPairsLstEvol[[jj, 2]]]]}, {jj, 1, SubstPairsLstCntEvol}]), (Join[Table[{jj}, {jj, 1, SubstPairsLstCntEvol}], Table[GetSubstanceName[SubstLstEvol[[ii]]] ** TensorProduct[GetSubstanceName[SubstPairsLstEvol[[jj, 1]]] , GetSubstanceName[SubstPairsLstEvol[[jj, 2]]]], {jj, 1, SubstPairsLstCntEvol}, {ii, 1, len}], 2])];

      substDisplMatrixPP = substDisplMatrix;

      substDisplMatrixPM = If[MaxCatCntEvol == 1, (Table[{jj, TensorProduct[GetSubstanceName[EnantSubstPairsLstEvol[[jj, 1]]] , GetSubstanceName[EnantSubstPairsLstEvol[[jj, 2]]]]}, {jj, 1, SubstPairsLstCntEvol}]), (Join[Table[{jj}, {jj, 1, SubstPairsLstCntEvol}], Table[GetSubstanceName[SubstLstEvol[[ii]]] ** TensorProduct[GetSubstanceName[EnantSubstPairsLstEvol[[jj, 1]]] , GetSubstanceName[EnantSubstPairsLstEvol[[jj, 2]]]], {jj, 1, SubstPairsLstCntEvol}, {ii, 1, len}], 2])];

      substDisplMatrixMP = If[MaxCatCntEvol == 1, (Table[{jj, TensorProduct[GetSubstanceName[SubstPairsLstEvol[[jj, 1]]] , GetSubstanceName[SubstPairsLstEvol[[jj, 2]]]]}, {jj, 1, SubstPairsLstCntEvol}]), (Join[Table[{jj}, {jj, 1, SubstPairsLstCntEvol}], Table[GetSubstanceName[EnantSubstLstEvol[[ii]]] ** TensorProduct[GetSubstanceName[SubstPairsLstEvol[[jj, 1]]] , GetSubstanceName[SubstPairsLstEvol[[jj, 2]]]], {jj, 1, SubstPairsLstCntEvol}, {ii, 1, len}], 2])];

      substDisplMatrixMM = If[MaxCatCntEvol == 1, (Table[{jj, TensorProduct[GetSubstanceName[EnantSubstPairsLstEvol[[jj, 1]]] , GetSubstanceName[EnantSubstPairsLstEvol[[jj, 2]]]]}, {jj, 1, SubstPairsLstCntEvol}]), (Join[Table[{jj}, {jj, 1, SubstPairsLstCntEvol}], Table[GetSubstanceName[EnantSubstLstEvol[[ii]]] ** TensorProduct[GetSubstanceName[EnantSubstPairsLstEvol[[jj, 1]]] , GetSubstanceName[EnantSubstPairsLstEvol[[jj, 2]]]], {jj, 1, SubstPairsLstCntEvol}, {ii, 1, len}], 2])];
      Print["EvolutionInitialize:: ... done."];
      PrintSubstMatrixEvol[];
    ),
    (
      Print["EvolutionInitialize::NOT initializing display matrices."];
    )
  ];

  IsInitializedEvol = True;
];
(* ============================================== *)
PrintSubstMatrixEvol[] := Module[{s, jj},
  Print[strSeparator];

  Print[Table[GetSubstanceName[SubstLstEvol[[jj]]], {jj, 1, Length[SubstLstEvol]}] // MatrixForm];
  Print[Table[GetSubstanceName[EnantSubstLstEvol[[jj]]], {jj, 1, Length[SubstLstEvol]}] // MatrixForm];

  If[MaxCatCntEvol == 1,
    (
      s = "(Subst1 \[TensorProduct] Subst2)";
      Print["nP: ", s, " = ", substDisplMatrixPP // MatrixForm];
      Print["nM: ", s, " = ", substDisplMatrixPM // MatrixForm];
    ),
    (
      s = "Catalysts ** (Subst1 \[TensorProduct] Subst2)";
      Print["PP: ", s, " = ", substDisplMatrixPP // MatrixForm];
      Print["PM: ", s, " = ", substDisplMatrixPM // MatrixForm];
      Print["MP: ", s, " = ", substDisplMatrixMP // MatrixForm];
      Print["MM: ", s, " = ", substDisplMatrixMM // MatrixForm];
    )
  ];

  Print[strSeparator];
];
(* ============================================== *)
AssignEvolutionCoefficients[catIdx_?IntegerQ] := Module[{substNameLst, len, ii, expr, substLst, pep, genIdx, jj, catLst, substY, subst$A, subst$a},
  coeff\:0df4Y\:2794A = c\:02d6;
  coeff\:0df4A\:2794Y = c\:02d7;
  coeff\:0df4Y\:2794a = coeff\:0df4Y\:2794A;
  coeff\:0df4a\:2794Y = coeff\:0df4A\:2794Y;
  (* ==============================================*)
  coeff\:0df4A\:2794A\:066d = a\:02d6;
  coeff\:0df4A\:066d\:2794A = a\:02d7;
  (* ==============================================*)
  substLst = {"A", "a"};

  Do[
  (
    substNameLst = SubstLstEvolNameLevelFunc[genIdx];
    len = Length[substNameLst];

    Do[
      (
        pep = substNameLst[[ii]];

        Do[
          (
            If[UseActivationValEvol,
              (
                expr = CoeffPrefixValue <> substLst[[jj]] <> ActivationLetter <> PlusLetter <> pep <> ToLetter <> substLst[[jj]] <> pep <> " = " <> CapitalLambdaLetter <> PlusLetter;
                (* Print["ii = ", ii, ", expr: ", expr]; *)
                ToExpression[expr];
              ),
              (
                expr = CoeffPrefixValue <> substLst[[jj]] <> PlusLetter <> pep <> ToLetter <> substLst[[jj]] <> pep <> " = " <> CapitalLambdaLetter <> PlusLetter;
                (* Print["ii = ", ii, ", expr: ", expr]; *)
                ToExpression[expr];

                If[ substLst[[jj]] != pep,
                  (
                    expr = CoeffPrefixValue <> pep <> PlusLetter <> substLst[[jj]] <> ToLetter <> pep <> substLst[[jj]] <> " = " <> CapitalLambdaLetter <> PlusLetter;
                    (* Print["ii = ", ii, ", expr: ", expr]; *)
                    ToExpression[expr];

                    expr = CoeffPrefixValue <> pep <> substLst[[jj]] <> ToLetter <> pep <> PlusLetter <> substLst[[jj]] <> " = " <> CapitalLambdaLetter <> MinusLetter;
                    (* Print["ii = ", ii, ", expr: ", expr]; *)
                    ToExpression[expr];
                  )
                ];
              )
            ];

            expr = CoeffPrefixValue <> substLst[[jj]] <> pep <> ToLetter <> substLst[[jj]] <> PlusLetter <> pep <> " = " <> CapitalLambdaLetter <> MinusLetter;
            (* Print["ii = ", ii, ", expr: ", expr]; *)
            ToExpression[expr];
          ), {jj, 1, 2}
        ];
      ), {ii, 1, len}
    ];
  ), {genIdx, 1, (MaxChainLenValEvol - 1)}
];
  (* ==============================================*)
  \[CapitalGamma]\:02d6 = \[CapitalGamma]0\:02d6;
  \[CapitalGamma]\:02d7 = \[CapitalGamma]0\:02d7;
  \[Gamma]\:02d6 = \[Gamma]0\:02d6;
  \[Gamma]\:02d7 = \[Gamma]0\:02d7;
  (* ==============================================*)
  Switch[CatSynthEnantSel,
    EnantSelNoReaction,
    (
      \[CapitalGamma]\:02d6 = 0;
      \[Gamma]\:02d6 = 0;
    ),
    EnantSelNone,
    (
      \[Gamma]\:02d6 = 0;
    ),
    EnantSelPlus,
    (
      \[Gamma]\:02d6 = \[Gamma]0\:02d6;
    ),
    EnantSelMinus,
    (
      \[Gamma]\:02d6 = -\[Gamma]0\:02d6;
    )
  ];
  (* ==============================================*)
  Switch[InvCatSynthEnantSel,
    EnantSelNoReaction,
    (
      \[CapitalGamma]\:02d7 = 0;
      \[Gamma]\:02d7 = 0;
    ),
    EnantSelNone,
    (
      \[Gamma]\:02d7 = 0;
    ),
    EnantSelPlus,
    (
      \[Gamma]\:02d7 = \[Gamma]0\:02d7;
    ),
    EnantSelMinus,
    (
      \[Gamma]\:02d7 = -\[Gamma]0\:02d7;
    )
  ];
  (* ==============================================*)
  catLst = SubstLstEvolNameFunc[MaxChainLenValEvol];
  len = Length[catLst];
  (* Print["catLst = ", catLst]; *)

  catIdxTbl = Table[0, {ii, 1, len}];
  catIdxTbl[[catIdx]] = 1;
  (* Print["catIdxTbl = ", catIdxTbl // MatrixForm]; *)

  substY = "Y";
  {subst$A, subst$a} = substLst;

  Do[
    (
      expr = CoeffPrefixValue <> substY <> PlusLetter <> catLst[[ii]] <> ToLetter <> subst$A <> PlusLetter <> catLst[[ii]] <> " = " <> ToString[catIdxTbl[[ii]]] <> "*" <> CapitalGammaLetter <> PlusLetter <> "*(1+" <> GammaLetter <> PlusLetter <> ")";
      (* Print["ii = ", ii, ", expr: ", expr]; *)
      ToExpression[expr];

      expr = CoeffPrefixValue <> substY <> PlusLetter <> catLst[[ii]] <> ToLetter <> subst$a <> PlusLetter <> catLst[[ii]] <> " = " <> ToString[catIdxTbl[[ii]]] <> "*" <> CapitalGammaLetter <> PlusLetter <> "*(1-" <> GammaLetter <> PlusLetter <> ")";
      (* Print["ii = ", ii, ", expr: ", expr]; *)
      ToExpression[expr];


      expr = CoeffPrefixValue <> subst$A <> PlusLetter <> catLst[[ii]] <> ToLetter <> substY <> PlusLetter <> catLst[[ii]] <> " = " <> ToString[catIdxTbl[[ii]]] <> "*" <> CapitalGammaLetter <> MinusLetter <> "*(1-" <> GammaLetter <> MinusLetter <> ")";
      (* Print["ii = ", ii, ", expr: ", expr]; *)
      ToExpression[expr];

      expr = CoeffPrefixValue <> subst$a <> PlusLetter <> catLst[[ii]] <> ToLetter <> substY <> PlusLetter <> catLst[[ii]] <> " = " <> ToString[catIdxTbl[[ii]]] <> "*" <> CapitalGammaLetter <> MinusLetter <> "*(1+" <> GammaLetter <> MinusLetter <> ")";
      (* Print["ii = ", ii, ", expr: ", expr]; *)
      ToExpression[expr];

    ), {ii, 1, len}
  ];
];
(* ==============================================*)
EvolutionRunRange[tMaxVal_?NumericQ, substPairStart_?IntegerQ, substPairEnd_?IntegerQ] := Module[{plusTbl, minusTbl, retVal, ii, jj},
  Print["EvolutionRunRange::Calculating plusTbl..."];
  plusTbl = Join[Table[{jj}, {jj, substPairStart, substPairEnd}], Table[EvolutionRun[ii, jj, False, tMaxVal], {jj, substPairStart, substPairEnd}, {ii, 1, MaxCatCntEvol}], 2];
  (* ============================================== *)
  Print["EvolutionRunRange::Calculating minusTbl..."];
  minusTbl = Join[Table[{jj}, {jj, substPairStart, substPairEnd}], Table[EvolutionRun[ii, jj, True, tMaxVal], {jj, substPairStart, substPairEnd}, {ii, 1, MaxCatCntEvol}], 2];
  (* ============================================== *)
  Print[strSeparator];
  Print["Catalysts ** (Subst1 \[TensorProduct] Subst2) = ", substDisplMatrix // MatrixForm];
  Print[strSeparator];
  Print["plusTbl = ", Chop[plusTbl, chopDelta] // MatrixForm];
  Print[strSeparator];
  Print["minusTbl = ", Chop[minusTbl, chopDelta] // MatrixForm];
  Print[strSeparator];
  PrintTimeUsed[];
  retVal = {plusTbl, minusTbl};
  Return[retVal];
];
(* ==============================================*)
EvolutionRunAll[tMaxVal_?NumericQ] := Module[{plusTbl, minusTbl, retVal, ii, jj},
  Print["EvolutionRunAll::Calculating plusTbl..."];
  plusTbl = Join[Table[{jj}, {jj, 1, SubstPairsLstCntEvol}], Table[EvolutionRun[ii, jj, False, tMaxVal], {jj, 1, SubstPairsLstCntEvol}, {ii, 1, MaxCatCntEvol}], 2];
  (* ============================================== *)
  Print["EvolutionRunAll::Calculating minusTbl..."];
  minusTbl = Join[Table[{jj}, {jj, 1, SubstPairsLstCntEvol}], Table[EvolutionRun[ii, jj, True, tMaxVal], {jj, 1, SubstPairsLstCntEvol}, {ii, 1, MaxCatCntEvol}], 2];
  (* ============================================== *)
  Print[strSeparator];
  Print["Catalysts ** (Subst1 \[TensorProduct] Subst2) = ", substDisplMatrix // MatrixForm];
  Print[strSeparator];
  Print["plusTbl = ", Chop[plusTbl, chopDelta] // MatrixForm];
  Print[strSeparator];
  Print["minusTbl = ", Chop[minusTbl, chopDelta] // MatrixForm];
  Print[strSeparator];
  PrintTimeUsed[];
  retVal = {plusTbl, minusTbl};
  Return[retVal];
];
(* ==============================================*)
AssignEvolDirSedCoefficients[sedPairIdx_?IntegerQ] := Module[{len, ii, substIdVal, subst1Id1Val, substId, subst1Id, substIDlst, substEiDlst, name, name1, base, base1, substLen, substDecayID, substDecayName, nameCoeff, name1Coeff, baseSubstId, coeffName, name1CoeffEnant, coeffNameEnant},
  len = SubstPairsLstCntEvol;

  If[!UseDirectCrystEvol, Return[]];

  Do[
    (
      substIdVal = SubstPairsLstEvol[[ii, 1]];
      subst1Id1Val = SubstPairsLstEvol[[ii, 2]];

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

      name1CoeffEnant = GetSubstanceName[EnantiomerSubstanceID[GetSubstanceID[name1Coeff]]];

      coeffName = CoeffPrefixValue <> nameCoeff <> PlusLetter <> name1Coeff <> ToLetter <> ToString[substLen] <> substDecayName;
      coeffNameEnant = CoeffPrefixValue <> nameCoeff <> PlusLetter <> name1CoeffEnant <> ToLetter <> ToString[substLen] <> substDecayName;

      (* Print["AssignEvolDirSedCoefficients::ii = ", ii, ", nameCoeff = ", nameCoeff, ", name1Coeff = ", name1Coeff, ", coeffName = ", coeffName,", coeffNameEnant = ", coeffNameEnant]; *)

      If[ii == sedPairIdx,
        (
        (* Print["   ... assigning NON ZERO."]; *)

          If[!UseMinusOne$sigma,
            (
              ToExpression[coeffName <> "=" <> CapitalSigmaLetter <> MinusLetter <> MinusLetter];
              ToExpression[coeffNameEnant <> "=0"];
            ),
            (
              ToExpression[coeffName <> "=0"];
              ToExpression[coeffNameEnant <> "=" <> CapitalSigmaLetter <> MinusLetter <> MinusLetter];
            )
          ];
        ),
        (
        (* Print["   ... assigning ZERO."]; *)

          ToExpression[coeffName <> "=0"];
          ToExpression[coeffNameEnant <> "=0"];
        )
      ];
    ), {ii, 1, len}
  ];
];
(* ==============================================*)
(* lst is a 3 x N matrix in the form {{catIdx1, sedIdx1, useMinusOne$sigma1},{catIdx2, sedIdx2, useMinusOne$sigma2}, ...} *)
EvolutionRunList[lst_?MatrixQ, tMaxVal_?NumericQ] := Module[{len, ii, catIdx, sedIdx, useMinusOne$sigma},
  len = Length[lst];

  If[Length[lst[[1]]] != 3,
    (
      Print["EvolutionRunList::Invalid list. Correct form is: {{catIdx1, sedIdx1, useMinusOne$sigma1},{catIdx2, sedIdx2, useMinusOne$sigma2}, ...}."];
      Return[];
    )
  ];

  Do[
    (
      {catIdx, sedIdx, useMinusOne$sigma} = lst[[ii]];
      EvolutionRun[catIdx, sedIdx, useMinusOne$sigma, tMaxVal];
    ), {ii, 1, len}
  ];

  Print["EvolutionRunCallLst = ", EvolutionRunCallLst // MatrixForm];
  PrintTimeUsed[];

  Return[EvolutionRunCallLst];
];
(* ==============================================*)
EvolutionRun[catIdx_?IntegerQ, sedPairIdx_?IntegerQ, useMinusOne$sigmaVal_?BooleanQ, tMaxVal_?NumericQ] :=
    Module[
      {
        options, useNNT, name1\:02d6, name2\:02d6, crystName\:02d6\:02d6, name1\:02d7, name2\:02d7, crystName\:02d6\:02d7,
        rMax\:02d6\:02d6, kCryst\:02d6\:02d6, kDiss\:02d6\:02d6, rMax\:02d6\:02d7, kCryst\:02d6\:02d7, kDiss\:02d6\:02d7,
        substId, subst1Id, substLen, base, base1, substDecayName, crystDecay\:02d6\:02d6, crystDecay\:02d6\:02d7,
        initialValuesOptions, outputOptions, runChainModelOptions, plotSubstanceOptions, description, sol, nuLst, nuLstSorted,
        aaTotalLst, aaTotal, aaBothEnantNormLst, aaBothEnantNormLstSorted, nuTotalLst, nuTotalLstSorted, aaBothEnantNormTotalLstSorted,
        nameCoeff, name1Coeff, nameCoeffEnant, name1CoeffEnant, reacCoeffName, reacCoeffNameEnant, retVal, NDSolveMonitorOptions,
        subst1Id1DisplVal, reacCoeff$A$EA$Name, reacCoeff$EA$A$Name, cryst$A$A$Name, cryst$EA$EA$Name, cryst$A$EA$Name,
        cryst$EA$A$Name, maxChainLen, useActivation, nameCoeff$E$A, nameCoeff$E$B, reacCoeff$A$A$Name, reacCoeff$EA$EA$Name,
        substIdVal, subst1Id1Val, len, strCryst, rLst
      },

      (* ============================================== *)
      useNNT = UseNNTValEvol;
      maxChainLen = MaxChainLenValEvol;
      useActivation = UseActivationValEvol;
      EvolutionCount++;
      (* ============================================== *)
      len = SubstPairsLstCntEvol;

      If[sedPairIdx > len,
        (
          Print["EvolutionRun::sedPairIdx = ", sedPairIdx, " exceeds maximum allowed value of ", len];
          Return[Indeterminate];
        )
      ];

substIdVal = SubstPairsLstEvol[[sedPairIdx, 1]];
subst1Id1Val = SubstPairsLstEvol[[sedPairIdx, 2]];
(* ============================================== *)
Print[strSeparator];
subst1Id1DisplVal = If[!useMinusOne$sigmaVal, subst1Id1Val, EnantiomerSubstanceID[subst1Id1Val]];

UseMinusOne$sigma = useMinusOne$sigmaVal;

If[UseDirectCrystEvol,
  (
    strCryst = "Using direct crystallization.";
    InitDirectCrystValGen = True;

    InitPairFormValGen = False;
    UseSamePairFormCoeffValGen = False;
    UseSymmetricPairsValGen = False;

    InitCrystValGen = False;
    InitCrystDecValGen = False;
    InitBasicCrystValGen = False;
    InitChainCrystValGen = False;
    InitTwoSubstCrystValGen = False;
    TwoSubstCrystValGen = {};
  ),
  (
    strCryst = "Using pair formation and standard crystallization.";
    InitDirectCrystValGen = False;

    InitPairFormValGen = True;
    UseSamePairFormCoeffValGen = True;
    UseSymmetricPairsValGen = False;

    InitCrystValGen = True;
    InitCrystDecValGen = True;
    InitBasicCrystValGen = True;
    InitChainCrystValGen = True;
    InitTwoSubstCrystValGen = True;
  )
];

(* Print["EvolutionRun::", strCryst]; *)
If[CatSynthEnantSel == EnantSelNoReaction && InvCatSynthEnantSel == EnantSelNoReaction,
  (
    Print["EvolutionRun::EvolutionCount = ", EvolutionCount, " ,Catalyst = None, Pair (", sedPairIdx, ") = (", GetSubstanceName[substIdVal], ", ", GetSubstanceName[subst1Id1DisplVal], "). ", strCryst];
  ),
  (
    Print["EvolutionRun::EvolutionCount = ", EvolutionCount, " ,Catalyst (", catIdx, ") = ", GetSubstanceName[SubstLstEvol[[catIdx]]], ", Pair (", sedPairIdx, ") = (", GetSubstanceName[substIdVal], ", ", GetSubstanceName[subst1Id1DisplVal], "). ", strCryst];
  )
];


InitEpimerizationValGen = False;
(* ============================================== *)
{substId, subst1Id} = GetPairSubstIDs[substIdVal, subst1Id1Val, UseSymmetricPairsValGen];
{nameCoeff, name1Coeff} = GetPairCoeffNames[substIdVal, subst1Id1Val, UseSymmetricPairsValGen];
{nameCoeffEnant, name1CoeffEnant} = GetPairCoeffNames[substIdVal, EnantiomerSubstanceID[subst1Id], UseSymmetricPairsValGen];

nameCoeff$E$A = GetSubstanceName[EnantiomerSubstanceID[GetSubstanceID[nameCoeff]]];
nameCoeff$E$B = GetSubstanceName[EnantiomerSubstanceID[GetSubstanceID[name1Coeff]]];

reacCoeffName = Create$PairName[nameCoeff, name1Coeff];
reacCoeffNameEnant = Create$PairName[nameCoeffEnant, name1CoeffEnant];

reacCoeff$A$A$Name = Create$PairName[nameCoeff, name1Coeff];
reacCoeff$EA$EA$Name = Create$PairName[nameCoeff$E$A, nameCoeff$E$B];
reacCoeff$A$EA$Name = Create$PairName[nameCoeff, nameCoeff$E$B];
reacCoeff$EA$A$Name = Create$PairName[nameCoeff$E$A, name1Coeff];

cryst$A$A$Name = Create$CrystName[nameCoeff, name1Coeff];
cryst$EA$EA$Name = Create$CrystName[nameCoeff$E$A, nameCoeff$E$B];
cryst$A$EA$Name = Create$CrystName[nameCoeff, nameCoeff$E$B];
cryst$EA$A$Name = Create$CrystName[nameCoeff$E$A, name1Coeff];

base = GetChainLength[substId];
base1 = GetChainLength[subst1Id];
substLen = base + base1;
substDecayName = GetSubstDecayName[];

name1\:02d6 = GetSubstanceName[substId];
name2\:02d6 = GetSubstanceName[subst1Id];
name2\:02d7 = GetSubstanceName[EnantiomerSubstanceID[subst1Id]];

TwoSubstCrystValGen = {{ToExpression["idx" <> name1\:02d6], ToExpression["idx" <> name2\:02d6]}};
(* Print["TwoSubstCrystValGen = ", TwoSubstCrystValGen]; *)
(* ============================================== *)
options = GeneratorInitializeModel[maxChainLen, useActivation, useNNT];
(* ============================================== *)
(* ChainModelY, ChainModelL, ChainModelGoodL, ChainModelGoodAllLD, ChainModelRandMixLD, ChainModelPropMixLD *)
ChainModelID = ChainModelPropMixLD;
(*ChainModelID=ChainModelY; *)
(* ============================================== *)
YSubstWeightVal = 1;
SigmaMultiplier = 0;
(* ============================================== *)
initialValuesOptions := {InitRandom -> initRandomVal, InitYsubst -> initYsubstVal, YSubstWeight -> YSubstWeightVal, InitLEnant -> initLEnantVal, InitDEnant -> initDEnantVal, InitDMultiplier -> (99 / 101)};
(* ============================================== *)
outputOptions := {\[Rho]AllFuncDescription -> description, TStartMultiplier -> 0};
(* ============================================== *)
(* OutputTypeNone, OutputTypeAll, OutputTypeCPandRho *)
runChainModelOptions := {RunModelOutputType -> OutputType$Eval, RunChainModelPlotRange -> RunChainModelPlotRangeValEval};
(* ============================================== *)
NDSolveMonitorOptions := {NStorage -> 1000, PrintMonitorData -> False, MonitorPrintFrequency -> 10000, MonitorType -> MonitorTypeTime, StepMonitorFunction :> CalculateData, DynamicStepMonitor -> {OutputDynamicStepMonitorTime, OutputDynamicStepMonitorData, OutputDynamicStepMonitor}, QuitMonitor -> QuitMonitorFunction, UseShowProgress -> True};
(* ============================================== *)

rLst = If[UseActivationValEvol, {rY, rA, ra, rA\:066d, ra\:066d}, {rY, rA, ra}];

If[UseDirectCrystEvol,
  (
    plotSubstanceOptions = {PlotSubstances -> False, SubstancePlotList -> {}};

    Switch[MaxChainLenValEvol,
      1,
      (
        plotSubstanceOptions := {PlotSubstances -> PlotSubstancesValEval, SubstancePlotList -> {rLst, {rA, ra}}};
      ),
      2,
      (
        plotSubstanceOptions := {PlotSubstances -> PlotSubstancesValEval, SubstancePlotList -> {rLst, {rAA, raa, rAa, raA}}};
      ),
      3,
      (
        plotSubstanceOptions := {PlotSubstances -> PlotSubstancesValEval, SubstancePlotList -> {rLst, {rAA, raa, rAa, raA}, {rAAA, raaa, rAAa, raaA}, {rAaA, raAa, rAaa, raAA}}};
      ),
      4,
      (
        plotSubstanceOptions := {PlotSubstances -> PlotSubstancesValEval, SubstancePlotList -> {rLst, {rAA, raa, rAa, raA}, {rAAA, raaa, rAAa, raaA}, {rAaA, raAa, rAaa, raAA},
          {rAAAA, raaaa, rAAAa, raaaA}, {rAAaA, raaAa, rAAaa, raaAA}, {raAAA, rAaaa, raAAa, rAaaA}, {raAaA, rAaAa, raAaa, rAaAA}
        }};
      ),
      5,
      (
        plotSubstanceOptions := {PlotSubstances -> PlotSubstancesValEval, SubstancePlotList -> {rLst, {rAA, raa, rAa, raA}, {rAAA, raaa, rAAa, raaA}, {rAaA, raAa, rAaa, raAA},
          {rAAAA, raaaa, rAAAa, raaaA}, {rAAaA, raaAa, rAAaa, raaAA}, {raAAA, rAaaa, raAAa, rAaaA}, {raAaA, rAaAa, raAaa, rAaAA},
          {rAAAAA, raaaaa, rAAAAa, raaaaA}, {rAAAaA, raaaAa, rAAAaa, raaaAA}, {rAaAAA, raAaaa, rAaAAa, raAaaA}, {rAaAaA, raAaAa, AraAaa, raAaAA}
        }};
      )
    ];
  ),
  (
    plotSubstanceOptions := {PlotSubstances -> PlotSubstancesValEval, SubstancePlotList -> {rLst, {rAA, raa, rAa, raA}, {rAAA, raaa, rAAa, raaA}, {rAaA, raAa, rAaa, raAA}, {ToExpression["r" <> reacCoeff$A$A$Name], ToExpression["r" <> reacCoeff$EA$EA$Name], ToExpression["r" <> reacCoeff$A$EA$Name], ToExpression["r" <> reacCoeff$EA$A$Name]}, {ToExpression["r" <> cryst$A$A$Name], ToExpression["r" <> cryst$EA$EA$Name], ToExpression["r" <> cryst$A$EA$Name], ToExpression["r" <> cryst$EA$A$Name]}}};
  )
];
(* ============================================== *)
(*
description="maxEnantNumb = " <> ToString[maxEnantNumb] <>", maxChainLen = " <> ToString[maxChainLen] <>", roTotInitVal = " <> ToString[roTotInitVal] <>", alphaVal = " <> ToString[N[alphaVal]];
*)
description = "";
(* ============================================== *)
options = Join[initialValuesOptions, outputOptions, runChainModelOptions, plotSubstanceOptions, NDSolveMonitorOptions, options];
(* Print["options = ", options]; *)
(* ============================================== *)
(* ============================================== *)
crystName\:02d6\:02d6 = Create$CrystName[name1\:02d6, name2\:02d6];
crystName\:02d6\:02d7 = Create$CrystName[name1\:02d6, name2\:02d7];

rMax\:02d6\:02d6 = Create$rMaxName[reacCoeffName];
kCryst\:02d6\:02d6 = Create$kCrystName[reacCoeffName];
kDiss\:02d6\:02d6 = Create$kDissName[reacCoeffName];
crystDecay\:02d6\:02d6 = Create$CrystDecayCoeffName[crystName\:02d6\:02d6, substDecayName, substLen];

rMax\:02d6\:02d7 = Create$rMaxName[reacCoeffNameEnant];
kCryst\:02d6\:02d7 = Create$kCrystName[reacCoeffNameEnant];
kDiss\:02d6\:02d7 = Create$kDissName[reacCoeffNameEnant];
crystDecay\:02d6\:02d7 = Create$CrystDecayCoeffName[crystName\:02d6\:02d7, substDecayName, substLen];

(*
Print["name1\:02d6 = ", name1\:02d6, ", name2\:02d6 = ", name2\:02d6, ", name2\:02d7 = ", name2\:02d7, ", crystName\:02d6\:02d6 = ", crystName\:02d6\:02d6, ", crystName\:02d6\:02d7 = ", crystName\:02d6\:02d7];
Print["rMax\:02d6\:02d6 = ", rMax\:02d6\:02d6, ", kCryst\:02d6\:02d6 = ", kCryst\:02d6\:02d6, ", kDiss\:02d6\:02d6 = ", kDiss\:02d6\:02d6, ", crystDecay\:02d6\:02d6 = ", crystDecay\:02d6\:02d6];
Print["rMax\:02d6\:02d7 = ", rMax\:02d6\:02d7, ", kCryst\:02d6\:02d7 = ", kCryst\:02d6\:02d7, ", kDiss\:02d6\:02d7 = ", kDiss\:02d6\:02d7, ", crystDecay\:02d6\:02d7 = ", crystDecay\:02d6\:02d7];
Print["TwoSubstCrystValGen = ", TwoSubstCrystValGen];
*)
AssignEvolutionCoefficients[catIdx];
AssignEvolDirSedCoefficients[sedPairIdx];

If[!useMinusOne$sigmaVal,
  (
  ToExpression[rMax\:02d6\:02d6 <> "=\[Rho]\:02d7"];
ToExpression[rMax\:02d6\:02d7 <> "=\[Rho]\:02d6"];
),
(
ToExpression[rMax\:02d6\:02d6 <> "=\[Rho]\:02d6"];
ToExpression[rMax\:02d6\:02d7 <> "=\[Rho]\:02d7"];
)
];

ToExpression[kCryst\:02d6\:02d6 <> "=\[CapitalSigma]\:02d6"];
ToExpression[kDiss\:02d6\:02d6 <> "=\[CapitalSigma]\:02d7"];
ToExpression[crystDecay\:02d6\:02d6 <> "=\[CapitalDelta]\:02d7"];

ToExpression[kCryst\:02d6\:02d7 <> "=\[CapitalSigma]\:02d6"];
ToExpression[kDiss\:02d6\:02d7 <> "=\[CapitalSigma]\:02d7"];
ToExpression[crystDecay\:02d6\:02d7 <> "=\[CapitalDelta]\:02d7"];

(*
Print["Assigned..."];
Print["rMax\:02d6\:02d6 = ", ToExpression[rMax\:02d6\:02d6], ", kCryst\:02d6\:02d6 = ",ToExpression[kCryst\:02d6\:02d6], ", kDiss\:02d6\:02d6 = ", ToExpression[kDiss\:02d6\:02d6], ", crystDecay\:02d6\:02d6 = ", ToExpression[crystDecay\:02d6\:02d6]];
Print["rMax\:02d6\:02d7 = ", ToExpression[rMax\:02d6\:02d7], ", kCryst\:02d6\:02d7 = ", ToExpression[kCryst\:02d6\:02d7], ", kDiss\:02d6\:02d7 = ", ToExpression[kDiss\:02d6\:02d7], ", crystDecay\:02d6\:02d7 = ", ToExpression[crystDecay\:02d6\:02d7]];
Print[strSeparator];
*)

(* ============================================== *)
If[PrintInfoEval,
  (
    PrintAllInfo[If[MaxChainLength <= MaxDetailedOutputChainLen, True, False], True, plotDistributionsVal, options];
  )
];
(* ============================================== *)

If[RunNDSolve,
  (
  (* Print["EvolutionRun::options = ", options]; *)

    sol = RunChainModel[ChainModelID, tMaxVal, RoTotInitValGen, options];
    nuLst = nuValueListAminoAcid[tMaxVal, sol];
    nuLstSorted = Sort[nuLst];

    (* Print["nuLst = ", nuLst // MatrixForm]; *)

    aaTotalLst = TotalAminoAcidList[tMaxVal, sol];

    (*
Print["Concentrations of amino acids = ", Table[{ii,DigitArrayL[[ii]],aaTotalLst[[ii]],DigitArrayD[[ii]],aaTotalLst[[ii+MaxEnantNo]]},{ii,1,MaxEnantNo}] // MatrixForm];
*)

    aaTotal = Sum[aaTotalLst[[ii]], {ii, 1, 2 * MaxEnantNo}];

    (* Print["aaTotal = ", aaTotal]; *)

    aaBothEnantNormLst = Table[MaxEnantNo * (aaTotalLst[[ii]] + aaTotalLst[[ii + MaxEnantNo]]) / aaTotal, {ii, 1, MaxEnantNo}];
    aaBothEnantNormLstSorted = SortByFirst[nuLst, aaBothEnantNormLst];
    (*
Print[strSeparator];
Print[strSeparator];

Print["\[Eta] at amino acid level."];
SetLegends[{"\[Eta]"}];
Print[DiscretePlot[nuLstSorted[[ii]],{ii,1,MaxEnantNo},Evaluate[discrPlotOpts2]]];

Print[strSeparator];


SetLegends[{"\[Eta]","\[Rho]"}];
Print[DiscretePlot[{nuLstSorted[[ii]],aaBothEnantNormLstSorted[[ii]]},{ii,1,MaxEnantNo},Evaluate[discrPlotOpts2]]];

Print[strSeparator];
Print[strSeparator];
*)
    (* ============================================== *)
    (*
Print["\[Eta] amont all substances..."];
nuTotalLst=Table[nuValue[aaTotalLst[[ii]],aaTotalLst[[ii+MaxEnantNo]]],{ii,1,MaxEnantNo}];
nuTotalLstSorted=Sort[nuTotalLst];

SetLegends[{"\[Eta]"}];
Print[DiscretePlot[nuTotalLstSorted[[ii]],{ii,1,MaxEnantNo},Evaluate[discrPlotOpts2]]];

aaBothEnantNormTotalLstSorted=SortByFirst[nuTotalLst,aaBothEnantNormLst];
SetLegends[{"\[Eta]","\[Rho]"}];
Print[DiscretePlot[{nuTotalLstSorted[[ii]],aaBothEnantNormTotalLstSorted[[ii]]},{ii,1,MaxEnantNo},Evaluate[discrPlotOpts2]]];

Print[strSeparator];
Print[strSeparator];
*)
    retVal = nuLst[[1]];
  ),
  (
    retVal = Indeterminate;
  )
];

If[CatSynthEnantSel == EnantSelNoReaction && InvCatSynthEnantSel == EnantSelNoReaction,
  (
    EvolutionRunCallLst = Join[EvolutionRunCallLst, {{TensorProduct[ GetSubstanceName[substIdVal], GetSubstanceName[subst1Id1DisplVal]], retVal}}];
  ),
  (
    EvolutionRunCallLst = Join[EvolutionRunCallLst, {{GetSubstanceName[SubstLstEvol[[catIdx]]] ** TensorProduct[ GetSubstanceName[substIdVal], GetSubstanceName[subst1Id1DisplVal]], retVal}}];
  )
];

Print["retVal = ", retVal];
Return[retVal];
];
(* ============================================== *)
