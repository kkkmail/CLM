(* ============================================== *)
(* :Summary: CLM Gradient Descent Solver. *)
(* ============================================== *)
(* :Author: Konstantin K. Konstantinov *)
(* :Email: konstantin.k.konstantinov@gmail.com *)
(* :License type: GPL v3 or any later version, see http://www.gnu.org/licenses/ *)
(* :Copyright: K^3, 2013 - 2018 *)
(* :Version: 3.27.001, Date : 2018/04/07 *)
(* :Mathematica Version: 10.0 - 11.0 *)
(* ============================================== *)
GetAllEquationsGDS[allGDS_]:=allGDS[[1]];
GetStabilityMatrixGDS[allGDS_]:=allGDS[[2]];

(* RunGradientDescentSolver[coeffValues_, initValues_, rawOptions___]:= *)
PrepareGradientDescentSolver[mult_?NumericQ, randomSeed_?IntegerQ, rawOptions___]:=
    Module[{opts, substMatrix, eqTbl, minFunc, initCond, nonNegCond, allEq, sol,
      substConservCond, substConservVal, stabMatr, evTbl, ev, precision, findMinOptions, accuracy},

      SeedRandomValue = randomSeed;
      InitializeChains[maxChainLen, maxEnantNumb, options];
      PrepareEquations[options];

      opts = ProcessOptions[rawOptions];

      precision = 50;
      accuracy = 20;

      initValues = ChainModelInitFuncList[[ChainModelID]][roTotInitVal, rawOptions];
      Print["PrepareGradientDescentSolver::initValues = ", initValues];

      substMatrix = Table[SubstanceMatrix[ii], {ii, 2, NoSubstCnt}];
      eqTbl = Table[SetPrecision[EqMatrix[ii], precision], {ii, 2, NoSubstCnt}];
      minFunc = mult * Sum[eqTbl[[ii]]^2,{ii, 1, NoSubstCnt - 1}];
      initCond = Table[{SubstanceMatrix[ii], SetPrecision[initValues[[ii]], precision]}, {ii, 2, NoSubstCnt}];
      nonNegCond = Table[SubstanceMatrix[ii] >= 0, {ii, 2, NoSubstCnt}];
      substConservCond = Sum[GetNoOfAtomsInSubstance[ii, 1] * SubstanceMatrix[ii], {ii, 2, NoSubstCnt}];
      substConservVal = Sum[GetNoOfAtomsInSubstance[ii, 1] * SetPrecision[initValues[[ii]], precision], {ii, 2, NoSubstCnt}];
      stabMatr = Table[D[eqTbl[[ii]], substMatrix[[jj]]], {ii, 1, NoSubstCnt - 1}, {jj, 1, NoSubstCnt - 1}];

      (* findMinOptions = {WorkingPrecision -> precision, MaxIterations -> 100000, AccuracyGoal -> accuracy, PrecisionGoal -> accuracy}; *)
      findMinOptions = {WorkingPrecision -> precision, MaxIterations -> 5000, AccuracyGoal -> accuracy, PrecisionGoal -> accuracy};

      allEq = Join[{Join[{minFunc, substConservCond == substConservVal}, nonNegCond], initCond}, findMinOptions];

      (*
      Print["PrepareGradientDescentSolver::substMatrix = ", substMatrix];
      Print["PrepareGradientDescentSolver::eqTbl = ", eqTbl // MatrixForm];
      Print["PrepareGradientDescentSolver::initCond = ", initCond];
      Print["PrepareGradientDescentSolver::minFunc = ", minFunc];
      Print["PrepareGradientDescentSolver::nonNegCond = ", nonNegCond];
      Print["PrepareGradientDescentSolver::allEq = ", allEq];
      Print["PrepareGradientDescentSolver::substConservCond = ", substConservCond];
      Print["PrepareGradientDescentSolver::substConservVal = ", substConservVal];
      Print["PrepareGradientDescentSolver::stabMatr = ", stabMatr // MatrixForm];
      *)

      Return[{allEq, stabMatr}];


      (*
      sol = Apply[FindMinimum, allEq];
      Print["RunGraduateDescentSolver::sol[[1]] = ", N[sol[[1]]/mult]];
      Print["RunGraduateDescentSolver::sol[[2]] = ", N[sol[[2]]] // MatrixForm];

      evTbl = stabMatr /. sol[[2]];
      (* )Print["RunGraduateDescentSolver::evTbl = ", evTbl // MatrixForm]; *)

      ev = Eigenvalues[evTbl];
      Print["RunGraduateDescentSolver::ev = ", Chop[N[ev]] // MatrixForm];

      roVec = Join[{0}, substMatrix /. sol[[2]]];
      rLVal = TotalRoL[roVec];
      rRVal = TotalRoD[roVec];
      retVal = nuValue[rLVal,rRVal];

      Return[sol];
      *)
    ];

(* Runs Gradient Descend Solver using the equations prepared by PrepareGradientDescentSolver *)
RunGradientDescentSolver[mult_?NumericQ, allGDS_]:= Module[
  {sol, allEq, stabMatr, nu, substMatrix},

  allEq = GetAllEquationsGDS[allGDS];
  stabMatr = GetStabilityMatrixGDS[allGDS];
  substMatrix = Table[SubstanceMatrix[ii], {ii, 2, NoSubstCnt}];

  sol = Apply[FindMinimum, allEq];
  Print["RunGraduateDescentSolver::sol[[1]] = ", N[sol[[1]]/mult]];
  Print["RunGraduateDescentSolver::sol[[2]] = ", N[sol[[2]]] // MatrixForm];

  evTbl = stabMatr /. sol[[2]];
  (* )Print["RunGraduateDescentSolver::evTbl = ", evTbl // MatrixForm]; *)

  ev = Eigenvalues[evTbl];
  Print["RunGraduateDescentSolver::ev = ", Chop[N[ev]] // MatrixForm];

  roVec = Join[{0}, substMatrix /. sol[[2]]];
  rLVal = TotalRoL[roVec];
  rRVal = TotalRoD[roVec];
  nu = nuValue[rLVal,rRVal];

  Return[Join[{nu}, sol]];
];

(* Returns Nu, minimm value and {r<XYZ> -> number} rules. *)
GdsGetNu[allSol_]:=allSol[[1]];
GdsGetMinVal[allSol_]:=allSol[[2]];
GdsGetRules[allSol_]:=allSol[[3]];
(* GdsGetRandomSeed[allSol_]:=allSol[[3]]; *)

