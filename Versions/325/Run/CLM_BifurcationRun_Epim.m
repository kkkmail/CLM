(* ============================================== *)
(* :Summary: Bifurcation test. *)
(* ============================================== *)
(* :Author: Konstantin K. Konstantinov *)
(* :Email: konstantin.k.konstantinov@gmail.com *)
(* :License type: GPL v3 or any later version, see http://www.gnu.org/licenses/ *)
(* :Copyright: K^3, 2013 - 2015 *)
(* :Version: Revision: 3.16.001, Date: 2015/05/21 *)
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
ClearAll["Global`*"];
(* ============================================== *)
maxChainLen=2;
useActivation=True;
tMaxVal=1.*10^4;
(* ============================================== *)
catSynthIdx=1;
dirSedIdx=1;
(* ============================================== *)
noOfPoints=30;
r00Min=1.0*10^-3;
r00Max=1.0*10^1;
(* ============================================== *)
PathList={"W:\\Math\\CLM\\"};
Get["CLM_Init.m",Path-> PathList];
LoadModules[PathList, False];
(* ============================================== *)
PrintFrequencyValue=10;
NMaximizeMaxIterations=100;
InitDirectCrystValue=False;
ΔγValue=0.99;
(* ============================================== *)
UseSigmaFixed=False;
UseRho0Fixed=False;
UseLogScale=False;
(* μValue=1.0*10^1; *)
(* ============================================== *)
(*
NMaximizeMethod="\"DifferentialEvolution\"";
*)
(*
randomSeed=1;
NMaximizeMethod="{\"RandomSearch\", \"SearchPoints\" -> 100, \"RandomSeed\" -> " <> ToString[randomSeed]<>"}";
*)
(* NMaximizeMethod="\"NelderMead\""; *)
(* ============================================== *)
(* ============================================== *)
(* VarTypeEpsilon, VarTypeEpsilonRange, VarTypeNormalRange *)
(* ============================================== *)
SynthVarType=VarTypeEpsilon;
InvSynthVarType=VarTypeEpsilon;
(* ============================================== *)
(* ============================================== *)
(* EnantSelNoReaction, EnantSelNone, EnantSelPlus, EnantSelMinus, EnantSelAll, EnantSelPlusOne, EnantSelMinusOne *)
(* ============================================== *)
(* ============================================== *)
(* Catalytic synthesis *)
CatSynthEnantSel=EnantSelNoReaction;
InvCatSynthEnantSel=EnantSelNoReaction;
(* ============================================== *)
(* ============================================== *)
(* Ligation *)
UseSameLigationForAll=True;
(*
UseSTermLigation=True; (* applicable only to models WITHOUT activation *)
UseSTermInvLigation=True;
*)
LigationEnantSel=EnantSelPlus;
InvLigationEnantSel=EnantSelPlus;
(* ============================================== *)
(* ============================================== *)
(* Epimerization *)
UseSameEpimerizationForAll=True;
EpimEnantSel=EnantSelAll;
(* ============================================== *)
(* ============================================== *)
(* Direct sedimentation *)
UseMinusOne$sigma=False;
(* ============================================== *)
(* ============================================== *)
BifurcationRunNDSolve=False;
RunAllCoeffsUseNSolve=True;
RunAllCoeffsShowNSolveProgress=True;
(* ============================================== *)
(* allCoeffTblRule=GenerateAllBifurcationTests[maxChainLen,useActivation,{1,7},{1,28}]; *)

(*
allCoeffTblRule=GenerateAllBifurcationTests[maxChainLen,useActivation,{1,3},{1,1}];
(* allCoeffTblRule=GenerateAllBifurcationTests[maxChainLen,useActivation,{1,3},{1,2}]; *)
output=BifurcationRun[maxChainLen,useActivation,tMaxVal,allCoeffTblRule];
*)
(*
allCoeffTblRule=GenerateAllBifurcationTests[maxChainLen,useActivation,{catSynthIdx,catSynthIdx},{dirSedIdx,dirSedIdx}];
output=BifurcationRun[maxChainLen,useActivation,tMaxVal,allCoeffTblRule];
*)
(* ============================================== *)
(*
BifurcationPlotCoeff[maxChainLen,useActivation,catSynthIdx,dirSedIdx,r00Min,r00Max,noOfPoints];
*)
(* ============================================== *)
BifurcationRun[maxChainLen,useActivation];
retValName="retVal";
sTbl=Table[GenerateBifurcationTest[catCnt,dirSedCnt,GetUniqueName[retValName,catCnt,dirSedCnt],True,True],{catCnt,catSynthIdx,catSynthIdx},{dirSedCnt,dirSedIdx,dirSedIdx}];

Print["sTbl[[1,1]] = ", sTbl[[1,1]]];

PrintTimeUsed[];
(* ============================================== *)
