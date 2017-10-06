(* ============================================== *)
(* :Summary:CLM initialization logic.*)
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
(* Set to true to disable most of the output. Debug output will still work. *)
SilentRunValue = False;
(* ============================================== *)
LoadModules[pathListVal_] := LoadModules[pathListVal, False];

LoadModules[pathListVal_, silentRunVal_?BooleanQ] := Module[{},
  SilentRunValue = silentRunVal;

  Get["CLM_Common.m", Path -> pathListVal];
  Get["CLM_Output.m", Path -> pathListVal];
  Get["CLM_Binomial.m", Path -> pathListVal];
  Get["CLM_BinomialOutput.m", Path -> pathListVal];
  Get["CLM_Calc.m", Path -> pathListVal];
  Get["CLM_Chain.m", Path -> pathListVal];
  Get["CLM_ChainHlp.m", Path -> pathListVal];

  Get["CLM_Synthesis.m", Path -> pathListVal];
  Get["CLM_Sedimentation.m", Path -> pathListVal];
  Get["CLM_SedimentationDirect.m", Path -> pathListVal];
  Get["CLM_Activation.m", Path -> pathListVal];
  Get["CLM_Ligation.m", Path -> pathListVal];
  Get["CLM_CatLigation.m", Path -> pathListVal];
  Get["CLM_Epimerization.m", Path -> pathListVal];
  Get["CLM_ChainPlot.m", Path -> pathListVal];
  Get["CLM_ChainModel.m", Path -> pathListVal];
  Get["CLM_ChainInit.m", Path -> pathListVal];

  Get["CLM_GeneratorCommon.m", Path -> pathListVal];
  Get["CLM_BifurcationGenerator.m", Path -> pathListVal];
  Get["CLM_EvolutionGenerator.m", Path -> pathListVal];

  Get["CLM_CommonPostFix.m", Path -> pathListVal];

  OutputCopyright[];
];
(* ============================================== *)
