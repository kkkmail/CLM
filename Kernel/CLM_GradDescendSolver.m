(* ============================================== *)
(* :Summary: CLM Graduate Descent Solver. *)
(* ============================================== *)
(* :Author: Konstantin K. Konstantinov *)
(* :Email: konstantin.k.konstantinov@gmail.com *)
(* :License type: GPL v3 or any later version, see http://www.gnu.org/licenses/ *)
(* :Copyright: K^3, 2013 - 2018 *)
(* :Version: 3.27.001, Date : 2018/04/07 *)
(* :Mathematica Version: 10.0 - 11.0 *)
(* ============================================== *)

RunGraduateDescentSolver[coeffValues_?VectorQ,initValues_?VectorQ,rawOptions___]:=
    Module[{opts, coeffRuleTbl, applyCoeffRuleVal, substMatrix, eqTbl, minFunc, initCond, nonNegCond, allEq, sol,
      substConservCond, substConservVal, stabMatr, evTbl, ev, precision, findMinOptions, accuracy, mult},

      If[!SilentRunValue,
        (
          Print["RunGraduateDescentSolver::Starting..."];
          PrintTimeUsed[];
        )
      ];

      If[coeffLen!= NoCoeffCnt,(Print["RunGraduateDescentSolver::Invalid coeffValues."]; Return[Indeterminate];)];
      If[initLen!= NoSubstCnt,(Print["RunGraduateDescentSolver::Invalid initValues."]; Return[Indeterminate];)];

      opts = ProcessOptions[rawOptions];

      applyCoeffRuleVal=ApplyCoeffRule /. opts /. Options[CLMS];

      precision = 200;
      accuracy = 100;
      mult = 10^9;

      coeffRuleTbl = If[applyCoeffRuleVal,(Table[coeffArrayName[ii] -> coeffValues[[ii]],{ii,1,NoCoeffCnt}] /. tauRule),{}];
      substMatrix = Table[SubstanceMatrix[ii], {ii, 2, NoSubstCnt}];
      eqTbl = Table[SetPrecision[EqMatrix[ii], precision], {ii, 2, NoSubstCnt}];
      minFunc = mult * Sum[eqTbl[[ii]]^2,{ii, 1, NoSubstCnt - 1}];
      initCond = Table[{SubstanceMatrix[ii], SetPrecision[initValues[[ii]], precision]}, {ii, 2, NoSubstCnt}];
      nonNegCond = Table[SubstanceMatrix[ii] >= 0, {ii, 2, NoSubstCnt}];
      substConservCond = Sum[GetNoOfAtomsInSubstance[ii, 1] * SubstanceMatrix[ii], {ii, 2, NoSubstCnt}];
      substConservVal = Sum[GetNoOfAtomsInSubstance[ii, 1] * SetPrecision[initValues[[ii]], precision], {ii, 2, NoSubstCnt}];
      stabMatr = Table[D[eqTbl[[ii]], substMatrix[[jj]]], {ii, 1, NoSubstCnt - 1}, {jj, 1, NoSubstCnt - 1}];

      findMinOptions = {WorkingPrecision -> precision, MaxIterations -> 100000, AccuracyGoal -> accuracy, PrecisionGoal -> accuracy};

      allEq = Join[{Join[{minFunc, substConservCond == substConservVal}, nonNegCond], initCond}, findMinOptions];

      (*
      Print["RunGraduateDescentSolver::coeffRuleTbl = ", coeffRuleTbl];
      Print["RunGraduateDescentSolver::substMatrix = ", substMatrix];
      Print["RunGraduateDescentSolver::eqTbl = ", eqTbl // MatrixForm];
      Print["RunGraduateDescentSolver::initCond = ", initCond];
      Print["RunGraduateDescentSolver::minFunc = ", minFunc];
      Print["RunGraduateDescentSolver::nonNegCond = ", nonNegCond];
      Print["RunGraduateDescentSolver::allEq = ", allEq];
      Print["RunGraduateDescentSolver::substConservCond = ", substConservCond];
      Print["RunGraduateDescentSolver::substConservVal = ", substConservVal];
      Print["RunGraduateDescentSolver::stabMatr = ", stabMatr // MatrixForm];
      *)

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
    ];

