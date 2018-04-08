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
    Module[{opts, coeffRuleTbl, applyCoeffRuleVal, substMatrix},
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

      coeffRuleTbl=If[applyCoeffRuleVal,(Table[coeffArrayName[ii] -> coeffValues[[ii]],{ii,1,NoCoeffCnt}] /. tauRule),{}];
      substMatrix=Table[SubstanceMatrix[ii],{ii,1,NoSubstCnt}];

      Print["coeffRuleTbl = ", coeffRuleTbl];
      Print["substMatrix = ", substMatrix];

      Return[];
    ];

