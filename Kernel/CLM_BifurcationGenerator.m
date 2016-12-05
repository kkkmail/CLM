(* ============================================== *)
(* :Summary: CLM bifurcation generator module. *)
(* ============================================== *)
(* :Author: Konstantin K. Konstantinov *)
(* :Email: konstantin.k.konstantinov@gmail.com *)
(* :License type: GPL v3 or any later version, see http://www.gnu.org/licenses/ *)
(* :Copyright: K^3, 2013 - 2015 *)
(* :Version: Revision: 3.19.001, Date: 2015/11/27 *)
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
BifurcationGeneratorVersion="3.19.001";
(* ============================================== *)
(* ============================================== *)
GenerateBifurcationTest[catSynthIdx_?IntegerQ,dirSedIdx_?IntegerQ,retValName_?StringQ,printInfo_?BooleanQ,generatePrint_?BooleanQ]:=Module[{allCoeffs0,allCoeffs,eqLst,rules,deltaLst,deltaZeroRules,thetaLst,r0Lst,dThetaLst,dDeltaLst,len,name,substID,enantID,enantName,rule,zeroDeltaSquareRule,ii,jj,substCnt,chainLenCnt,varLen,deltaCoeffs, thetaEqLst, allVarLen, crlf, funcVarDeclLst, varDeclLst, setPrecisionLst, pmFuncVarDeclLst, pmValDeclLst, pmCoeffsDeclLst, nMaxParamDeclLst,funcDeclStr,pmFuncDeclStr,nMaxCallStr,allCoeffsRule,allStr,nMaxFuncDeclStr,DoPrintName, MaxReCoeffValCountName, monitorCountName, maxEvalName, maxCoeffsName, MaxReCoeffValName, RunBifurcationTestName, PrintMonitorName,strMethod,maxEvecName,maxSolName,allSubstLst,killSymbRule,maxSubstName,eqCheckStr,ro0Val,initValAssignStr,initValDeclStr,varName,initValName, fmCondStr,fmInitValStr,varEqName,solName},

Clear[\[CapitalDelta]\[Gamma],\[Delta]\[CapitalGamma],\[Delta]a,\[Delta],\[CapitalOmega],\[Mu],\[Epsilon],\[Rho]0,\[Delta]\[Rho]0,\[Delta]\[Rho]];
epsValue=\[Delta];

killSymbRule:={someNotNeededSymbol_?NotNumericQ:>0};
allSubstLst=Table[SubstanceMatrix[ii],{ii,1,NoSubstCnt}];

If[printInfo,Print["allSubstLst = ",allSubstLst // MatrixForm]];

allCoeffs0=Join[{r00},Table[coeffArrayName[ii],{ii,1,NoCoeffCnt}]];
(* Print["allCoeffs0 = ", allCoeffs0 // MatrixForm]; *)
allCoeffs=allCoeffs0;

allVarLst={};
allVarEqLst={};
eqLst=Delete[Table[EqMatrix[ii],{ii,1,NoSubstCnt}],{{1},{3}}];

If[printInfo,Print["eqLst = ",eqLst // MatrixForm]];

rules={};
deltaLst={};
deltaZeroRules={};
thetaLst={};
r0Lst={};
dThetaLst={};
dDeltaLst={};
coeffRules={};

Do[
(
len=Length[AllChainsTbl[[chainLenCnt]]]/2;

Do[
(
name=AllChainsTbl[[chainLenCnt,substCnt]];
substID=GetSubstanceID[name];
enantID=EnantiomerSubstanceID[substID];
enantName=GetSubstanceName[enantID];
rule={ToExpression["r"<>name]-> ToExpression[ "(\[Theta]" <> name <> "+\[CapitalDelta]"<> name <> ")/2"],ToExpression["r"<>enantName]-> ToExpression[ "(\[Theta]" <> name <> "-\[CapitalDelta]"<> name <> ")/2"]};
rules=Join[rules,rule];
thetaLst=Join[thetaLst,{ToExpression[ "\[Theta]"<> name]}];
deltaLst=Join[deltaLst,{ToExpression[ "\[CapitalDelta]"<> name]}];
deltaZeroRules=Join[deltaZeroRules,{ToExpression[ "\[CapitalDelta]"<> name]-> 0}];
r0Lst=Join[r0Lst,{ToExpression["\[Theta]"<>name]*GetChainLength[substID]}];
dThetaLst=Join[dThetaLst,{EqMatrix[substID]+EqMatrix[enantID]}];
dDeltaLst=Join[dDeltaLst,{EqMatrix[substID]-EqMatrix[enantID]}];

(* Print["name = ", name, ", enantName = ", enantName]; *)

If[chainLenCnt==1 && InitializeActivationValue,
(
substID=ToActivated[substID];
enantID=EnantiomerSubstanceID[substID];
name=GetSubstanceName[substID];
enantName=GetSubstanceName[enantID];
rule={ToExpression["r"<>name]-> ToExpression[ "(\[Theta]" <> name <> "+\[CapitalDelta]"<> name  <> ")/2"],ToExpression["r"<>enantName]-> ToExpression[ "(\[Theta]" <> name <> "-\[CapitalDelta]"<> name<> ")/2"]};
rules=Join[rules,rule];
thetaLst=Join[thetaLst,{ToExpression[ "\[Theta]"<> name]}];deltaLst=Join[deltaLst,{ToExpression[ "\[CapitalDelta]"<> name]}];
deltaZeroRules=Join[deltaZeroRules,{ToExpression[ "\[CapitalDelta]"<> name]-> 0}];
r0Lst=Join[r0Lst,{ToExpression["\[Theta]"<>name]}];
dThetaLst=Join[dThetaLst,{EqMatrix[substID]+EqMatrix[enantID]}];
dDeltaLst=Join[dDeltaLst,{EqMatrix[substID]-EqMatrix[enantID]}];
)
];
),{substCnt,1,len}
];
),{chainLenCnt,1,Length[AllChainsTbl]}
];
(* ============================================== *)
len=Length[deltaLst];
zeroDeltaSquareRule={};
(* ============================================== *)
Do[
(
Do[
(
zeroDeltaSquareRule=Join[zeroDeltaSquareRule,{deltaLst[[ii]]*deltaLst[[jj]]-> 0}];
),{jj,1,ii}
];
),{ii,1,len}
];
(* ============================================== *)
If[printInfo,
(
Print["rules = ", rules // MatrixForm];
Print["thetaLst = ", thetaLst // MatrixForm];
Print["deltaLst = ", deltaLst // MatrixForm];
Print["deltaZeroRules = ", deltaZeroRules // MatrixForm];
Print["zeroDeltaSquareRule = ", zeroDeltaSquareRule // MatrixForm];
Print["r0Lst = ", r0Lst // MatrixForm];
Print["allVarLst = ", allVarLst // MatrixForm, ", allVarEqLst = ", allVarEqLst // MatrixForm];
)
];
(* ============================================== *)
AddAllCoeffRules[catSynthIdx,dirSedIdx];
(* ============================================== *)
(* Print["allCoeffs = ", allCoeffs // MatrixForm]; *)
allCoeffs = allCoeffs /. coeffRules;
If[printInfo,Print["Updated allCoeffs = ", allCoeffs // MatrixForm]];
(* ============================================== *)
ro0Val=If[UseLogScale,10^r0,r0];

dThetaLst=Join[{Apply[Plus,r0Lst]+rY-ro0Val},Simplify[Expand[(Expand[dThetaLst /. rules] /. deltaZeroRules) /. coeffRules]]];
dDeltaLst=Simplify[Expand[(Expand[dDeltaLst /. rules] /. zeroDeltaSquareRule) /. coeffRules]];

If[printInfo,
(
Print["dThetaLst = ", dThetaLst // MatrixForm];
Print["dDeltaLst = ", dDeltaLst // MatrixForm];
)
];

eqLst=eqLst /. rules;
eqLst[[1]]=Apply[Plus,r0Lst]+rY-ro0Val;

(* ============================================== *)
eqLst=Expand[Expand[eqLst] /. zeroDeltaSquareRule] /. zeroDeltaSquareRule;
If[printInfo,Print["eqLst = ",eqLst // MatrixForm]];

varLen=Length[deltaLst];

deltaCoeffs=Table[Coefficient[dDeltaLst[[ii]],deltaLst[[jj]]],{ii,1,varLen},{jj,1,varLen}];

If[printInfo,
(
Print["deltaCoeffs = ",Simplify[deltaCoeffs] // MatrixForm];
Print["allVarLst = ", allVarLst // MatrixForm, ", allVarEqLst = ", allVarEqLst // MatrixForm];
)
];

(* ============================================== *)
thetaEqLst=Join[Table[dThetaLst[[ii]]==0,{ii,1,Length[dThetaLst]}],{rY>= 0},Table[thetaLst[[ii]]>=0,{ii,1,Length[thetaLst]}]];

If[printInfo,
(
Print["thetaEqLst = ", thetaEqLst // MatrixForm];
Print["thetaEqLst = ", ToString[InputForm[thetaEqLst]]];
Print["deltaCoeffs = ", ToString[InputForm[deltaCoeffs]]];
)
];

(* ============================================== *)
allVarLen=Length[allVarLst];
crlf=ToString[FromCharacterCode[13]];

funcVarDeclLst="";
varDeclLst="";
setPrecisionLst="";

pmFuncVarDeclLst="";
pmValDeclLst="";
pmCoeffsDeclLst="";
nMaxParamDeclLst="";

initValDeclStr="";
initValAssignStr="";
fmCondStr="";
fmInitValStr="";

Do[
(
If[MemberQ[dThetaLst,allVarLst[[ii]],Infinity] || MemberQ[deltaCoeffs,allVarLst[[ii]],Infinity],
(
varName=ToString[InputForm[allVarLst[[ii]]]];
varEqName=ToString[InputForm[allVarEqLst[[ii]]]];

If[StringLength[funcVarDeclLst]>0,funcVarDeclLst=funcVarDeclLst<>", "];
funcVarDeclLst=funcVarDeclLst<>varName<>"Var_?NumericQ";
varDeclLst=varDeclLst<>varName <>", ";
setPrecisionLst=setPrecisionLst<>varName<>"=SetPrecision[" <>varName<>"Var, precisionVal];"<>crlf;

If[StringLength[pmFuncVarDeclLst]>0,pmFuncVarDeclLst=pmFuncVarDeclLst<>", "];
pmFuncVarDeclLst=pmFuncVarDeclLst<>varName<>"_?NumericQ";
If[StringLength[pmValDeclLst]>0,pmValDeclLst=pmValDeclLst<>", "];
pmValDeclLst=pmValDeclLst<>varName;
If[StringLength[pmCoeffsDeclLst]>0,pmCoeffsDeclLst=pmCoeffsDeclLst<>", "];
pmCoeffsDeclLst=pmCoeffsDeclLst<>"{\""<>varName<>"\" -> N["<>varName<>"]}";
nMaxParamDeclLst= nMaxParamDeclLst<>", "<>varEqName;

If[BifurcationRunFindMaximum,
(
initValName=varName<>"$0";
initValDeclStr=initValDeclStr<>", " <> initValName;
initValAssignStr=initValAssignStr<>initValName <> " = " <> varName <> " /.sol[[2]];"<>crlf;

If[StringLength[fmCondStr]>0,fmCondStr=fmCondStr<>" && "];
fmCondStr= fmCondStr <>varEqName;

If[StringLength[fmInitValStr]>0,fmInitValStr=fmInitValStr<>", "];
fmInitValStr= fmInitValStr <>"{"<>varName <> ", " <>  initValName <> "}";
)
];
)
];
),{ii,1,allVarLen}
];

 varDeclLst=varDeclLst<> "eqTbl,sol,len,ii,coeffTable,coeffVal,retVal,precisionVal,eValTbl,eValLen,jj,eSys,eVecTbl,maxPrec";
(* ============================================== *)
funcDeclStr="";

If[!SilentRunValue,
(
funcDeclStr=funcDeclStr <>"Print[\"Generated by CLM_BifurcationTest, version " <>ToString[BifurcationGeneratorVersion] <>" on "<>DateString[]<>".\"];" <> crlf <> crlf;
),
(
funcDeclStr=funcDeclStr <>crlf<>"(* BifurcationGeneratorVersion = \""<>ToString[BifurcationGeneratorVersion] <>"\" *)"<> crlf;
)
];

funcDeclStr=funcDeclStr <>"NotNumericQ[symb_]:=(!NumericQ[symb]);"<> crlf;

funcDeclStr=funcDeclStr <>"\[Delta] = " <>ToString[InputForm[\[Delta]Value]]<>";" <> crlf;
funcDeclStr=funcDeclStr <>"\[CapitalDelta]\[Gamma] = " <>ToString[InputForm[\[CapitalDelta]\[Gamma]Value]]<>";" <> crlf;
funcDeclStr=funcDeclStr <>"\[Delta]\[CapitalGamma] = " <>ToString[InputForm[\[Delta]\[CapitalGamma]Value]]<>";" <> crlf;
funcDeclStr=funcDeclStr <>"\[Delta]a = " <>ToString[InputForm[\[Delta]aValue]]<>";" <> crlf;
funcDeclStr=funcDeclStr <>"\[CapitalOmega] = " <>ToString[InputForm[\[CapitalOmega]Value]]<>";" <> crlf;
funcDeclStr=funcDeclStr <>"\[Mu] = " <>ToString[InputForm[\[Mu]Value]]<>";"<> crlf;
funcDeclStr=funcDeclStr <>"\[Rho]0 = " <>ToString[InputForm[\[Rho]0Value]]<>";" <> crlf;
funcDeclStr=funcDeclStr <>"\[Delta]\[Rho]0 = " <>ToString[InputForm[\[Delta]\[Rho]0Value]]<>";" <> crlf;
funcDeclStr=funcDeclStr <>"\[Delta]\[Rho] = " <>ToString[InputForm[\[Delta]\[Rho]Value]]<>";" <> crlf<> crlf;
funcDeclStr=funcDeclStr <>"PrintFrequency = " <>ToString[InputForm[PrintFrequencyValue]]<>";"<> crlf <> crlf;

DoPrintName=GetUniqueName["DoPrint",catSynthIdx,dirSedIdx];
MaxReCoeffValCountName=GetUniqueName["MaxReCoeffValCount",catSynthIdx,dirSedIdx];
monitorCountName=GetUniqueName["monitorCount",catSynthIdx,dirSedIdx];
maxEvalName=GetUniqueName["maxEval",catSynthIdx,dirSedIdx];
maxEvecName=GetUniqueName["maxEvec",catSynthIdx,dirSedIdx];
maxSolName=GetUniqueName["maxSol",catSynthIdx,dirSedIdx];
maxCoeffsName=GetUniqueName["maxCoeffs",catSynthIdx,dirSedIdx];
MaxReCoeffValName=GetUniqueName["MaxReCoeffVal",catSynthIdx,dirSedIdx];
RunBifurcationTestName=GetUniqueName["RunBifurcationTest",catSynthIdx,dirSedIdx];
PrintMonitorName=GetUniqueName["PrintMonitor",catSynthIdx,dirSedIdx];

funcDeclStr=funcDeclStr <>DoPrintName <>" = False;" <> crlf;
funcDeclStr=funcDeclStr <> MaxReCoeffValCountName <> " = 0;" <> crlf;
funcDeclStr=funcDeclStr <> monitorCountName <>" = -1;" <> crlf;
funcDeclStr=funcDeclStr <> maxEvalName <> " = -10^100;" <> crlf;
funcDeclStr=funcDeclStr <> maxCoeffsName <> " = Indeterminate;" <> crlf<> crlf;

funcDeclStr=funcDeclStr <> MaxReCoeffValName <> "[" <>funcVarDeclLst <> "]:=Module[{" <> varDeclLst <> "}," <> crlf;
funcDeclStr=funcDeclStr <>"precisionVal = " <>ToString[InputForm[RunAllCoeffsPrecision]]<>";" <> crlf;

funcDeclStr=funcDeclStr <>"maxPrec = $MaxPrecision;" <> crlf;
funcDeclStr=funcDeclStr <>"$MaxPrecision = Infinity;" <> crlf;

funcDeclStr=funcDeclStr <> MaxReCoeffValCountName <>"++;" <> crlf <> crlf;
funcDeclStr=funcDeclStr <>setPrecisionLst <> crlf <> crlf;
funcDeclStr=funcDeclStr <>"eqTbl=" <>ToString[InputForm[thetaEqLst]]<>";"<> crlf <> crlf;
funcDeclStr=funcDeclStr <>"coeffTable = " <>ToString[InputForm[deltaCoeffs]]<>";"<> crlf <> crlf;
funcDeclStr=funcDeclStr <>"sol = NSolve[eqTbl,{rY,"<>Apply[StringJoin,Table[ToString[InputForm[thetaLst[[ii]]]] <> If[ii!=Length[thetaLst],", ",""],{ii,1,Length[thetaLst]}]]<>"}, Reals, WorkingPrecision->" <>ToString[InputForm[RunAllCoeffsNSolveWorkingPrecision]]<>"];"<> crlf <> crlf;
funcDeclStr=funcDeclStr <>"len=Length[sol];"<> crlf <> crlf;

If[!SilentRunValue,
(
funcDeclStr=funcDeclStr <>"If[" <> DoPrintName <> ",Print[\"len = \",len,\", sol = \", N[sol] // MatrixForm]];"<> crlf ;
)
];

funcDeclStr=funcDeclStr <>"retVal=-10^100;"<> crlf <> crlf;
funcDeclStr=funcDeclStr <>"Do["<> crlf ;
funcDeclStr=funcDeclStr <>"("<> crlf;
funcDeclStr=funcDeclStr <>"coeffVal=coeffTable /. sol[[ii]];"<> crlf;
funcDeclStr=funcDeclStr <>"eSys=Eigensystem[coeffVal];"<> crlf;
funcDeclStr=funcDeclStr <>"eValTbl=eSys[[1]];"<> crlf;
funcDeclStr=funcDeclStr <>"eVecTbl=eSys[[2]];"<> crlf;
funcDeclStr=funcDeclStr <>"eValLen=Length[eValTbl];"<> crlf;

funcDeclStr=funcDeclStr <>"Do["<> crlf;
funcDeclStr=funcDeclStr <>"("<> crlf;
funcDeclStr=funcDeclStr <>"If[Re[eValTbl][[jj]]>retVal,"<> crlf;
funcDeclStr=funcDeclStr <>"("<> crlf;
funcDeclStr=funcDeclStr <>"retVal=Re[eValTbl][[jj]];"<> crlf<> crlf;
funcDeclStr=funcDeclStr <>maxEvalName <> "=Re[eValTbl][[jj]];"<> crlf;
funcDeclStr=funcDeclStr <>maxEvecName <> "=eVecTbl[[jj]];"<> crlf;
funcDeclStr=funcDeclStr <>maxSolName <>"=sol[[ii]];"<> crlf;
funcDeclStr=funcDeclStr <>")"<> crlf;
funcDeclStr=funcDeclStr <>"];"<> crlf;
funcDeclStr=funcDeclStr <>"),{jj,1,eValLen}"<> crlf;
funcDeclStr=funcDeclStr <>"];"<> crlf<> crlf;

If[!SilentRunValue,
(
funcDeclStr=funcDeclStr <>"If[" <> DoPrintName <> ",Print[\"eValTbl = \", N[eValTbl] // MatrixForm, \", retVal = \", N[retVal]]];"<> crlf;
)
];

funcDeclStr=funcDeclStr <>"),{ii,1,len}"<> crlf;
funcDeclStr=funcDeclStr <>"];"<> crlf <> crlf;

funcDeclStr=funcDeclStr <>"$MaxPrecision = maxPrec;" <> crlf;
funcDeclStr=funcDeclStr <>"Return[retVal];"<> crlf ;
funcDeclStr=funcDeclStr <>"];"<> crlf <> crlf;
(* ============================================== *)
If[printInfo,Print["funcDeclStr = ", funcDeclStr]];
(* ToExpression[funcDeclStr]; *)
(* ============================================== *)
pmFuncDeclStr=PrintMonitorName <> "[" <>pmFuncVarDeclLst<>"]:=Module[{val,coeffs},"<> crlf;
pmFuncDeclStr=pmFuncDeclStr <> monitorCountName <> "++;"<> crlf;
pmFuncDeclStr=pmFuncDeclStr <>"If[Mod[" <> monitorCountName <> ",PrintFrequency] == 0," <> DoPrintName <> " = True];"<> crlf;
pmFuncDeclStr=pmFuncDeclStr <>"val = " <> MaxReCoeffValName <> "["<>pmValDeclLst<>"];"<> crlf;
pmFuncDeclStr=pmFuncDeclStr <>"coeffs = {"<>pmCoeffsDeclLst<>"};"<> crlf;
pmFuncDeclStr=pmFuncDeclStr <>"If[val >= " <> maxEvalName <> ","<> crlf;
pmFuncDeclStr=pmFuncDeclStr <>"("<> crlf;
pmFuncDeclStr=pmFuncDeclStr <> maxEvalName <> " = val;"<> crlf;
pmFuncDeclStr=pmFuncDeclStr <> maxCoeffsName <> " = coeffs;"<> crlf;
pmFuncDeclStr=pmFuncDeclStr <>")"<> crlf;
pmFuncDeclStr=pmFuncDeclStr <>"];"<> crlf;

If[!SilentRunValue,
(
pmFuncDeclStr=pmFuncDeclStr <>"If[Mod[" <> monitorCountName <> ",PrintFrequency]==0,"<> crlf;
pmFuncDeclStr=pmFuncDeclStr <>"("<> crlf;
pmFuncDeclStr=pmFuncDeclStr <>"Print[\"monitor = \", " <> monitorCountName <> ",\", count = \", " <> MaxReCoeffValCountName <> ", \", max value = \",N[" <> maxEvalName <> "],\", coeffs = \"," <> maxCoeffsName <> " // MatrixForm, \", value = \",N[val],\", current coeffs = \",coeffs // MatrixForm];"<> crlf;
pmFuncDeclStr=pmFuncDeclStr <>")"<> crlf;
pmFuncDeclStr=pmFuncDeclStr <>"];"<> crlf;
)
];

pmFuncDeclStr=pmFuncDeclStr <>DoPrintName <>" = False;"<> crlf;
pmFuncDeclStr=pmFuncDeclStr <>"];"<> crlf;
(* ============================================== *)
If[printInfo,Print["pmFuncDeclStr = ", pmFuncDeclStr]];
(* ToExpression[pmFuncDeclStr]; *)

(* strMethod=If[ToString[NMaximizeMethod]\[NotEqual]"",", Method \[Rule] {\"" <> ToString[NMaximizeMethod] <> "\"}","",""]; *)
strMethod=If[ToString[NMaximizeMethod]!="",", Method -> " <> ToString[NMaximizeMethod],"",""];

nMaxFuncDeclStr= RunBifurcationTestName <> "[]:=" <>RunBifurcationTestName<> "[\[Rho]0];" <> crlf <> crlf;
nMaxFuncDeclStr= nMaxFuncDeclStr<>RunBifurcationTestName <> "[\[Rho]00_?NumericQ]:=Module[{sol, sol2, fmLst, retVal, allCoeffsRule, ii, deltaLst, deltaRule, deltaZeroRule, r0Val, rInitVal, rInitSymmVal, killSymbRule, maxVal" <> initValDeclStr <>"},"<> crlf <> crlf;
nMaxFuncDeclStr=nMaxFuncDeclStr <>"sol = NMaximize[{" <> MaxReCoeffValName <> "[" <>pmValDeclLst <>"]"<>nMaxParamDeclLst<>"}, {" <> pmValDeclLst <>"}, StepMonitor:> " <> PrintMonitorName <> "["<>pmValDeclLst <>"], MaxIterations -> " <> ToString[NMaximizeMaxIterations] <>strMethod<> "];" <> crlf <> crlf;

If[!SilentRunValue,
(
nMaxFuncDeclStr=nMaxFuncDeclStr <>"Print[\"sol = \", sol];"<> crlf <> crlf ;
)
];

allCoeffsRule=Table[allCoeffs0[[ii]] -> allCoeffs[[ii]],{ii,1,Length[allCoeffs]}];
If[printInfo,Print["allCoeffsRule = ",allCoeffsRule // MatrixForm]];

solName="sol";
If[BifurcationRunFindMaximum,
(
nMaxFuncDeclStr=nMaxFuncDeclStr <>initValAssignStr<> crlf;

(* 
fmLst={{MaxReCoeffVal$1$24[r0,c\:02d6,c\:02d7,a\:02d6,a\:02d7,\[CapitalLambda]\:02d6,\[CapitalLambda]\:02d7,\[CapitalSigma]\:02d7],r0\[Equal]Log[\[Rho]00]/Log[10] && Log[\[Delta]]/Log[10]\[LessEqual]c\:02d6\[LessEqual]Log[\[CapitalOmega]cp]/Log[10] && c\:02d7\[Equal] Log[\[Delta]]/Log[10] && Log[\[Delta]a]/Log[10]\[LessEqual]a\:02d6\[LessEqual]Log[\[CapitalOmega]]/Log[10] && Log[\[Delta]a]/Log[10]\[LessEqual]a\:02d7\[LessEqual]Log[\[CapitalOmega]]/Log[10] && Log[\[Delta]\[CapitalGamma]]/Log[10]\[LessEqual]\[CapitalLambda]\:02d6\[LessEqual]Log[\[CapitalOmega]]/Log[10] && Log[\[Delta]\[CapitalGamma]]/Log[10]\[LessEqual]\[CapitalLambda]\:02d7\[LessEqual]Log[\[CapitalOmega]]/Log[10] && \[CapitalSigma]\:02d7\[Equal]Log[\[Mu]*\[CapitalOmega]]/Log[10]},{{r0,r00},{c\:02d6,c\:02d60},{c\:02d7,c\:02d70},{a\:02d6,a\:02d60},{a\:02d7,a\:02d70},{\[CapitalLambda]\:02d6,\[CapitalLambda]\:02d60},{\[CapitalLambda]\:02d7,\[CapitalLambda]\:02d70},{\[CapitalSigma]\:02d7,\[CapitalSigma]\:02d70}},MaxIterations \[Rule] 200};
*)

nMaxFuncDeclStr=nMaxFuncDeclStr <>"fmLst = {{"<>MaxReCoeffValName <> "[" <>pmValDeclLst <>"], "<> fmCondStr <> "}, {"<> fmInitValStr <>"}, MaxIterations -> "<> ToString[NMaximizeMaxIterations]<>"};"<> crlf<> crlf;
nMaxFuncDeclStr=nMaxFuncDeclStr <>"sol2 = Apply[FindMaximum, fmLst];"<> crlf<> crlf;
solName="sol2";
)
];

nMaxFuncDeclStr=nMaxFuncDeclStr <> "allCoeffsRule = " <> ToString[InputForm[allCoeffsRule]] <> " /. " <> solName <>"[[2]];" <> crlf<> crlf;

If[generatePrint,
(
nMaxFuncDeclStr=nMaxFuncDeclStr<>"Print[\" Values of coefficients: allCoeffsRule = \", allCoeffsRule // MatrixForm];" <> crlf;
)
];

nMaxFuncDeclStr=nMaxFuncDeclStr <>"deltaLst=" <>ToString[InputForm[deltaLst]] <>";"<> crlf;
nMaxFuncDeclStr=nMaxFuncDeclStr <>"r0Val=r0 /. " <> solName <>"[[2]];"<> crlf;
nMaxFuncDeclStr=nMaxFuncDeclStr <>"maxVal=("<> MaxReCoeffValName <> "[" <>pmValDeclLst <>"]" <> " /. " <> solName <>"[[2]]);" <> crlf;

If[GenerateEqCheck,
(
eqCheckStr="((" <> ToString[InputForm[dThetaLst]] <>" /. " <> solName <>"[[2]]) /. " <> maxSolName <> ")";
nMaxFuncDeclStr=nMaxFuncDeclStr <> "Print[\"Check::eq = \"," <> eqCheckStr <> " // MatrixForm];" <> crlf;
)
];

nMaxFuncDeclStr=nMaxFuncDeclStr <>"killSymbRule:=" <> ToString[InputForm[killSymbRule]] <> ";"<> crlf;
nMaxFuncDeclStr=nMaxFuncDeclStr <> "deltaRule=Table[deltaLst[[ii]] -> \[Delta]\[Rho]*r0Val*" <> maxEvecName <> "[[ii]], {ii, 1, " <> ToString[Length[deltaLst]] <> "}];"<> crlf <> crlf;
nMaxFuncDeclStr=nMaxFuncDeclStr <> "deltaZeroRule=Table[deltaLst[[ii]] -> 0, {ii, 1, " <> ToString[Length[deltaLst]] <> "}];"<> crlf <> crlf;
nMaxFuncDeclStr=nMaxFuncDeclStr <> "rInitVal=(((" <>  ToString[InputForm[allSubstLst]] <> " /. " <>  ToString[InputForm[rules]] <>") /. deltaRule) /. " <> maxSolName <> ");" <> crlf <> crlf;
nMaxFuncDeclStr=nMaxFuncDeclStr <>"rInitVal=Table[rInitVal[[ii]] /. killSymbRule, {ii, 1, " <> ToString[NoSubstCnt] <> "}];"<> crlf <> crlf;
nMaxFuncDeclStr=nMaxFuncDeclStr <> "rInitSymmVal=(((" <>  ToString[InputForm[allSubstLst]] <> " /. " <>  ToString[InputForm[rules]] <>") /. deltaZeroRule) /. " <> maxSolName <> ");" <> crlf <> crlf;
nMaxFuncDeclStr=nMaxFuncDeclStr <>"rInitSymmVal=Table[rInitSymmVal[[ii]] /. killSymbRule, {ii, 1, " <> ToString[NoSubstCnt] <> "}];"<> crlf <> crlf;
nMaxFuncDeclStr=nMaxFuncDeclStr <>"retVal = {" <> solName <>"[[1]], allCoeffsRule, " <> maxEvecName <> ", "<> maxSolName<> ", rInitVal, rInitSymmVal};"<> crlf;
nMaxFuncDeclStr=nMaxFuncDeclStr <>"Return[retVal];"<> crlf;
nMaxFuncDeclStr=nMaxFuncDeclStr <>"];"<> crlf;

If[printInfo,Print["nMaxFuncDeclStr = ", nMaxFuncDeclStr]];

nMaxCallStr=If[retValName != "", retValName <> " = ", ""] <>RunBifurcationTestName <>"[];";

allStr="";
allStr=allStr<>funcDeclStr<> crlf<> crlf;
allStr=allStr<> pmFuncDeclStr <> crlf<> crlf;
allStr=allStr<> nMaxFuncDeclStr <> crlf<> crlf;
allStr=allStr<> nMaxCallStr <> crlf<> crlf;

(*
allStr=allStr<> "allCoeffsRule = " <> ToString[InputForm[allCoeffsRule]]<> crlf<> crlf;
allStr=allStr<>"Print[\" Values of coefficients allCoeffsRule = \"];" <> crlf;
allStr=allStr<> "allCoeffsRule /. sol[[2]]"<> crlf;
*)

If[printInfo,
(
Print[strSeparator];
Print[strSeparator];
Print["allStr = ", crlf,allStr];
Print[strSeparator];
)
];

Return[allStr];
];
(* ============================================== *)
RunAllCoeffsErrList={};
RunAllCoeffsAddError[err_]:=Module[{},
Print["RunAllCoeffsAddError::err = ", err];
RunAllCoeffsErrList=Join[RunAllCoeffsErrList,Flatten[{ToString[InputForm[err]]}]]
];
(* ============================================== *)
RunAllCoeffsOutputErrors[]:=Module[{ii,len},
len=Length[RunAllCoeffsErrList];

If[len==0,
(
Print["There were no errors during RunAllCoeffs execution."];
),
(
Print["Errors during RunAllCoeffs execution: ", Table[{ii,RunAllCoeffsErrList[[ii]]},{ii,1,len}] // MatrixForm];
)
];
];
(* ============================================== *)
(* RunRoAllFunc runs all evolutions and returns solution in the form of the {{rules}} *)
RunRoAllFunc[tMaxVal_?NumericQ,bifurcInfo_,ii_?IntegerQ,jj_?IntegerQ,rawOptions___]:=Module[{maxEigenValue,allCoeffRule,retVal,allRo,kk,coeffValues,tRunVal,sol,f,interpFunc,tDomain,errDescription,allRoSymm},

(* Print["RunRoAllFunc::Starting..."]; *)

maxEigenValue=GetBifurcationMaxEigenValue[bifurcInfo];
allCoeffRule= GetBifurcationAllCoeffs[bifurcInfo];
allRo= GetBifurcationAllRo[bifurcInfo];
allRo=Table[Max[allRo[[kk]],0],{kk,1,NoSubstCnt}];
allRoSymm=GetBifurcationAllRoSymm[bifurcInfo];
coeffValues=Table[coeffArrayName[kk],{kk,1,NoCoeffCnt}] /. allCoeffRule;

If[maxEigenValue >EvPositiveEpsilon,
(
tRunVal=tEvRunMultiplier*Log[1/\[Delta]\[Rho]Value]/maxEigenValue;
tRunVal=Ceiling[tRunVal,10^(Round[Log[10,tRunVal]]-3)];

If[tRunVal>tMaxVal,
(
errDescription="Insufficient tMaxVal for "<>"ii = " <> ToString[ii] <> ", jj = " <> ToString[jj] <> ". tRunVal = " <> ToString[InputForm[tRunVal]] <> " exceeds tMaxVal = " <> ToString[InputForm[tMaxVal]];
RunAllCoeffsAddError[errDescription];
tRunVal=tMaxVal;
)
];

If[RunAllCoeffsShowNSolveProgress,Print["Starting \[Rho]AllFunc on " , DateString[],", ii = ", ii, ", jj = ", jj]];
sol=\[Rho]AllFunc[coeffValues,allRo,tRunVal,True,ApplyCoeffRule-> True,rawOptions ];
interpFunc=\[Rho]AllGetInterpolationFunctions[sol];
f=SubstanceMatrix[1] /.interpFunc[[1]];
tDomain=f["Domain"][[1,2]];

retVal={Table[SubstanceMatrix[kk] ->(SubstanceMatrix[kk][tDomain] /.interpFunc[[1]]),{kk,1,NoSubstCnt}]};
),
(
If[RunAllCoeffsShowNSolveProgress,Print["Single solution for " , DateString[],", ii = ", ii, ", jj = ", jj]];
retVal={Table[SubstanceMatrix[kk] ->allRoSymm[[kk]],{kk,1,NoSubstCnt}]};
)
];

(* Print["RunRoAllFunc::retVal = ", N[retVal] // MatrixForm]; *)
Return[retVal];
];
(* ============================================== *)
(* RunAllCoeffsEvolution performs evolution for all solutions with EV > 0 *)
RunAllCoeffsEvolution[tMaxVal_?NumericQ,allCoeffTblRule_, rawOptions___]:=Module[{solEvolTbl,allCoeffTblLen1,allCoeffTblLen2,ii,jj},
(* Print["RunAllCoeffsEvolution::Starting..."]; *)

allCoeffTblLen1=Length[allCoeffTblRule];
allCoeffTblLen2=Length[allCoeffTblRule[[1]]];

If[UseParallelTableForEvolution,
(
Print["RunAllCoeffsEvolution::Calling DistributeDefinitions..."];
DistributeDefinitions[RunRoAllFunc];
Print["RunAllCoeffsEvolution::Calling ParallelTable..."];

solEvolTbl=ParallelTable[RunRoAllFunc[tMaxVal,allCoeffTblRule[[ii,jj]],ii,jj,rawOptions],{ii,1,allCoeffTblLen1},{jj,1,allCoeffTblLen2}];
),
(
(* Print["RunAllCoeffsEvolution::Calling Table..."]; *)
solEvolTbl=Table[RunRoAllFunc[tMaxVal,allCoeffTblRule[[ii,jj]],ii,jj,rawOptions],{ii,1,allCoeffTblLen1},{jj,1,allCoeffTblLen2}];
)
];

Return[solEvolTbl];
];
(* ============================================== *)
(* RunAllCoeffsNSolve performs parallel evaluation of all solutions using NSolve *)
RunAllCoeffsNSolve[allCoeffTblRule_, rawOptions___]:=Module[{eqLst,eqGTElst,substShortLst,ii,jj,kk,bifurcInfo,allCoeffRule,coeffValues,roTotInitVal,eqr0,eqLstNumeric,eqAll,toUniqueRuleTbl,fromUiqueRuleTbl,eqAllTbl,substLstTbl,solNSolveTbl,allCoeffTblLen1,allCoeffTblLen2,substLst,rXZzeroRule,allRoSymmTbl,maxEigenValue,maxEigenValueTbl,substShortLstTbl,solLen,eqMatrixTbl,eqMatrixNumeric,eqMatrixAllTbl,allRoSymmRuleTbl},

allCoeffTblLen1=Length[allCoeffTblRule];
allCoeffTblLen2=Length[allCoeffTblRule[[1]]];

rXZzeroRule={rX->0,rZ->0};
eqMatrixTbl=Table[EqMatrix[ii],{ii,1,NoSubstCnt}];
eqLst=Table[eqMatrixTbl[[ii]]==0,{ii,1,NoSubstCnt}];
eqGTElst=Table[SubstanceMatrix[ii]>=0,{ii,1,NoSubstCnt}];
eqGTElst=Delete[eqGTElst,{{idxZ},{idxX}}];

substLst=Table[SubstanceMatrix[ii],{ii,1,NoSubstCnt}];
substShortLst=Delete[substLst,{{idxZ},{idxX}}];

Do[
(
Do[
(
bifurcInfo=allCoeffTblRule[[ii,jj]];
maxEigenValue=GetBifurcationMaxEigenValue[bifurcInfo];
allRoSymmTbl[ii,jj]=GetBifurcationAllRoSymm[bifurcInfo];

maxEigenValueTbl[ii,jj]=maxEigenValue;
allRoSymmRuleTbl[ii,jj]=Table[substLst[[kk]]->allRoSymmTbl[ii,jj][[kk]],{kk,1,NoSubstCnt}];

toUniqueRuleTbl[ii,jj]=Table[SubstanceMatrix[kk]->ToExpression[ToString[SubstanceMatrix[kk]]<>"$"<>ToString[ii]<>"$"<>ToString[jj]],{kk,1,NoSubstCnt}];
fromUiqueRuleTbl[ii,jj]=Table[ToExpression[ToString[SubstanceMatrix[kk]]<>"$"<>ToString[ii]<>"$"<>ToString[jj]] -> SubstanceMatrix[kk],{kk,1,NoSubstCnt}];

allCoeffRule=SetPrecision[GetBifurcationAllCoeffs[bifurcInfo],precisionVal];
eqMatrixNumeric=eqMatrixTbl /. allCoeffRule;
eqMatrixAllTbl[ii,jj]=eqMatrixNumeric;

Print["RunAllCoeffsNSolve::ii = ", ii, ", jj = ", jj,", allCoeffRule = ", N[allCoeffRule] // MatrixForm,", allRoSymmRuleTbl[",ii,", ",jj,"] = ", N[allRoSymmRuleTbl[ii,jj]] // MatrixForm, ", eqMatrixAllTbl[",ii,", ",jj,"] = ", N[eqMatrixAllTbl[ii,jj]] // MatrixForm];

If[maxEigenValue>0,
(
(* Print["ii = ", ii, ", jj = ", jj, ", allCoeffRule = ", allCoeffRule]; *)

coeffValues=Table[coeffArrayName[kk],{kk,1,NoCoeffCnt}] /. allCoeffRule;
coeffValues=SetPrecision[coeffValues,RunAllCoeffsPrecision];

roTotInitVal=r00 /.  allCoeffRule;
roTotInitVal=SetPrecision[roTotInitVal,RunAllCoeffsPrecision];

eqr0=((Sum[GetChainLength[ii]*SubstanceMatrix[ii],{ii,1,NoSubstCnt}] /.rXZzeroRule)==roTotInitVal);
eqLstNumeric=eqLst /. allCoeffRule;
eqLstNumeric[[idxY]]=eqr0;
eqLstNumeric=Delete[eqLstNumeric,{{idxZ},{idxX}}];
eqAll=Join[eqLstNumeric,eqGTElst];
eqAllTbl[ii,jj]=eqAll /. toUniqueRuleTbl[ii,jj];

(* Print["RunAllCoeffsNSolve::ii = ", ii, ", jj = ", jj, ", eqAllTbl[",ii,", ",jj,"] = ", eqAllTbl[ii,jj]]; *)

substShortLstTbl[ii,jj]=substShortLst /. toUniqueRuleTbl[ii,jj];
),
(
substLstTbl[ii,jj]=substLst /. toUniqueRuleTbl[ii,jj];
If[!SilentRunValue,Print["RunAllCoeffsNSolve::ii = ", ii, ", jj = ", jj,", allRoSymmTbl[",ii,", ",jj,"] = ", N[allRoSymmRuleTbl[ii,jj]] // MatrixForm]];
)
];
),{jj,1,allCoeffTblLen2}
];
),{ii,1,allCoeffTblLen1}
];

If[BifurcationPrintStepInfo,
(
PrintTimeUsed[];
Print["RunAllCoeffsNSolve::About to call NSolve."];
)
];

(*
DistributeDefinitions[maxEigenValueTbl,SubstanceMatrix,allRoSymmTbl,NoSubstCnt,RunAllCoeffsShowNSolveProgress,RunAllCoeffsNSolveWorkingPrecision,allCoeffTblLen1,allCoeffTblLen2,substShortLstTbl,substLstTbl];
*)

If[UseParallelTable,
(
solNSolveTbl=ParallelTable[
If[maxEigenValueTbl[ii,jj]>0,(If[RunAllCoeffsShowNSolveProgress,Print["Starting NSolve on " , DateString[],", ii = ", ii, ", jj = ", jj]];NSolve[eqAllTbl[ii,jj],substShortLstTbl[ii,jj],Reals,WorkingPrecision->RunAllCoeffsNSolveWorkingPrecision]),(If[RunAllCoeffsShowNSolveProgress,Print["Single solution for " , DateString[],", ii = ", ii, ", jj = ", jj]];{Table[substLstTbl[ii,jj][[kk]] ->  allRoSymmTbl[ii,jj][[kk]],{kk,1,NoSubstCnt}]})],{ii,1,allCoeffTblLen1},{jj,1,allCoeffTblLen2}];
),
(
solNSolveTbl=Table[
If[maxEigenValueTbl[ii,jj]>0,(If[RunAllCoeffsShowNSolveProgress,Print["Starting NSolve on " , DateString[],", ii = ", ii, ", jj = ", jj]];NSolve[eqAllTbl[ii,jj],substShortLstTbl[ii,jj],Reals,WorkingPrecision->RunAllCoeffsNSolveWorkingPrecision]),(If[RunAllCoeffsShowNSolveProgress,Print["Single solution for " , DateString[],", ii = ", ii, ", jj = ", jj]];{Table[substLstTbl[ii,jj][[kk]] ->  allRoSymmTbl[ii,jj][[kk]],{kk,1,NoSubstCnt}]})],{ii,1,allCoeffTblLen1},{jj,1,allCoeffTblLen2}];
)
];



If[!SilentRunValue,Print["RunAllCoeffsNSolve::solNSolveTbl = ", N[solNSolveTbl] // MatrixForm]];

solNSolveTbl=Table[solNSolveTbl[[ii,jj]] /. fromUiqueRuleTbl[ii,jj],{ii,1,allCoeffTblLen1},{jj,1,allCoeffTblLen2}];

If[!SilentRunValue,Print["RunAllCoeffsNSolve::Final solNSolveTbl = ", N[solNSolveTbl] // MatrixForm]];

(* Check *)
Do[
(
Do[
(
solLen=Length[solNSolveTbl[[ii,jj]]];
Print["RunAllCoeffsNSolve::Check for ii = ", ii, ", jj = ", jj, ", diff =  ", Table[N[eqMatrixAllTbl[ii,jj] /.solNSolveTbl[[ii,jj,kk]]],{kk,1,solLen}] // MatrixForm, ", symm diff = ", N[Evaluate[(eqMatrixAllTbl[ii,jj] /. allRoSymmRuleTbl[ii,jj])]] // MatrixForm];
),{jj,1,allCoeffTblLen2}
];
),{ii,1,allCoeffTblLen1}
];

If[BifurcationPrintStepInfo,
(
PrintTimeUsed[];
Print["RunAllCoeffsNSolve::Call to NSolve is completed."];
Print[strSeparator];
Print[strSeparator];
)
];

Return[solNSolveTbl];
];

(* ============================================== *)

RunAllCoeffs[allCoeffTblRule_, tMaxVal_,rawOptions___]:=Module[{allCoeffTblLen1,allCoeffTblLen2,evAllTbl,NuAllTbl,RoAllTbl,gammaPlusTbl,gammaMinusTbl,maxEigenValue,allCoeffRule,coeffValues,roTotInitVal,gammaPlusValue,gammaMinusValue,sol,roLtotal,roDtotal,nuTotal,roTotalPct,retVal,bifurcInfo,eVec,allRo,substanceMatrix,descriptor,tRunVal,errDescription,ii,jj,kk,eqGTElst,solNSolve,substLst,lenNSolve,nuLst,nuMax,rYmax,roSol,symmetryBreakingFlag,requestNDSolveRun,interpFunc,f,tDomain,solNSolveTbl,eqAllTbl,substLstTbl,toUniqueRuleTbl, fromUiqueRuleTbl,substShortLst,rXZzeroRule,allRoSymm,opts},

opts=ProcessOptions[rawOptions];

rXZzeroRule={rX->0,rZ->0};
allCoeffTblLen1=Length[allCoeffTblRule];
allCoeffTblLen2=Length[allCoeffTblRule[[1]]];

If[!SilentRunValue,Print["allCoeffTblLen1 = ", allCoeffTblLen1, ", allCoeffTblLen2 = ", allCoeffTblLen2]];

evAllTbl=Table[Indeterminate,{ii,1,allCoeffTblLen1},{jj,1,allCoeffTblLen2}];
NuAllTbl=Table[Indeterminate,{ii,1,allCoeffTblLen1},{jj,1,allCoeffTblLen2}];
RoAllTbl=Table[Indeterminate,{ii,1,allCoeffTblLen1},{jj,1,allCoeffTblLen2}];
gammaPlusTbl=Table[Indeterminate,{ii,1,allCoeffTblLen1},{jj,1,allCoeffTblLen2}];
gammaMinusTbl=Table[Indeterminate,{ii,1,allCoeffTblLen1},{jj,1,allCoeffTblLen2}];
substLst=Table[SubstanceMatrix[ii],{ii,1,NoSubstCnt}];

PrepareEquations[rawOptions];
PrepareCoeffGammaLst[];

(* Print["RunAllCoeffs::Calculating solNSolveTbl..."]; *)
solNSolveTbl=If[RunAllCoeffsUseNSolve,RunAllCoeffsNSolve[allCoeffTblRule,rawOptions],RunAllCoeffsEvolution[tMaxVal,allCoeffTblRule, rawOptions]];
(* Print["RunAllCoeffs::solNSolveTbl calculation completed."]; *)

Do[
(
Do[
(
symmetryBreakingFlag=False;
requestNDSolveRun=False;

bifurcInfo=allCoeffTblRule[[ii,jj]];
maxEigenValue=GetBifurcationMaxEigenValue[bifurcInfo];
allCoeffRule= GetBifurcationAllCoeffs[bifurcInfo];
eVec= GetBifurcationEigenVector[bifurcInfo];
allRo= GetBifurcationAllRo[bifurcInfo];
allRo=Table[Max[allRo[[kk]],0],{kk,1,NoSubstCnt}];
allRoSymm=GetBifurcationAllRoSymm[bifurcInfo];

coeffValues=Table[coeffArrayName[kk],{kk,1,NoCoeffCnt}] /. allCoeffRule;
roTotInitVal=r00 /.  allCoeffRule;
gammaPlusValue=CoeffGammaPlus[ii,allCoeffRule];
gammaMinusValue=CoeffGammaMinus[ii,allCoeffRule];

solNSolve=solNSolveTbl[[ii,jj]];
lenNSolve=Length[solNSolve];
nuMax=0;
rYmax=allRo[[idxY]];
roTotalPct=(roTotInitVal-rYmax)/roTotInitVal;

Print["RunAllCoeffs::ii = ", ii, ", jj = ", jj, ", maxEigenValue = ", N[maxEigenValue],", roTotInitVal = ", N[roTotInitVal], ", allRoSymm = ", Table[{substLst[[kk]]->N[allRoSymm[[kk]]]},{kk,1,NoSubstCnt}] // MatrixForm];

If[BifurcationPrintStepInfo,
(
PrintTimeUsed[];
Print["RunAllCoeffs::Running for ii = ", ii, ", jj = ", jj, ", maxEigenValue = ", N[maxEigenValue],", roTotInitVal = ", N[roTotInitVal], ", solNSolve = ", Transpose[N[solNSolve]] // MatrixForm, ", allRo = ",Table[{substLst[[kk]]->N[allRo[[kk]]]},{kk,1,NoSubstCnt}]// MatrixForm];
)
];

If[maxEigenValue >EvPositiveEpsilon,
(
symmetryBreakingFlag=True;

tRunVal=If[maxEigenValue<EvSmallValue,tEvSmallValuePlotMultiplier,tEvPlotMultiplier]*Log[1/\[Delta]\[Rho]Value]/maxEigenValue;

tRunVal=Ceiling[tRunVal,10^(Round[Log[10,tRunVal]]-3)];
),
(
tRunVal=tMaxNegativeEVval;
)
];

If[(symmetryBreakingFlag && (lenNSolve <3) && (RunAllCoeffsUseNSolve)),
(
requestNDSolveRun=True;
tRunVal=tEvRunMultiplier*Log[1/\[Delta]\[Rho]Value]/maxEigenValue;
tRunVal=Ceiling[tRunVal,10^(Round[Log[10,tRunVal]]-3)];

errDescription="Not enough solutions for " <> "ii = " <> ToString[ii] <> ", jj = " <> ToString[jj] <> ", lenNSolve = " <> ToString[lenNSolve] <> ", solNSolve = " <> ToString[InputForm[N[solNSolve]]];
RunAllCoeffsAddError[errDescription];
),
(
Do[
(
roSol=(substLst /. solNSolve[[kk]]) /. rXZzeroRule;
nuTotal=Abs[nuValue[TotalRoL[roSol],TotalRoD[roSol]]];

If[NumericQ[nuTotal],
(
If[nuTotal>nuMax,
(
nuMax=nuTotal;
rYmax=(rY /. solNSolve[[kk]]);
roTotalPct=(roTotInitVal-rYmax)/roTotInitVal;
If[!NumericQ[roTotalPct],roTotalPct=Indeterminate];
)
];
),
(
requestNDSolveRun=True;
errDescription="Non numeric results for " <>"ii = " <> ToString[ii] <> ", jj = " <> ToString[jj] <> ", lenNSolve = " <> ToString[lenNSolve] <> ", solNSolve[[" <> ToString[kk] <> "]] = " <> ToString[InputForm[N[solNSolve[[kk]]]]]<>", nuTotal = " <> ToString[InputForm[N[nuTotal]]];
RunAllCoeffsAddError[errDescription];
)
];
),{kk,1,lenNSolve}
];
)
];

If[symmetryBreakingFlag && (BifurcationRunNDSolve || requestNDSolveRun),
(
If[tRunVal>tMaxVal,
(
errDescription="Insufficient tMaxVal for "<>"ii = " <> ToString[ii] <> ", jj = " <> ToString[jj] <> ". tRunVal = " <> ToString[InputForm[tRunVal]] <> " exceeds tMaxVal = " <> ToString[InputForm[tMaxVal]];
RunAllCoeffsAddError[errDescription];
tRunVal=tMaxVal;
)
];

If[BifurcationPrintStepInfo,Print["RunAllCoeffs::Calling \[Rho]AllFunc."]];
sol=\[Rho]AllFunc[coeffValues,allRo,tRunVal,True,ApplyCoeffRule-> True,rawOptions ];
)
];

If[requestNDSolveRun,
(
interpFunc=\[Rho]AllGetInterpolationFunctions[sol];
f=SubstanceMatrix[1] /.interpFunc[[1]];
tDomain=f["Domain"][[1,2]];

roLtotal=\[Rho]TotalLValueFunc[sol,tDomain];
roDtotal=\[Rho]TotalDValueFunc[sol,tDomain];
nuMax=Abs[nuValue[roLtotal,roDtotal]];
roTotalPct=(roLtotal+roDtotal)/roTotInitVal;

If[roTotalPct>1+roTotalTolerance,
(
errDescription="roTotalPct exceeds (1 + tolerance) for "<>"ii = " <> ToString[ii] <> ", jj = " <> ToString[jj] <> ". roTotalPct = " <> ToString[InputForm[N[roTotalPct]]] <> ", roTotInitVal = " <> ToString[InputForm[N[roTotInitVal]]] <> ", roLtotal = " <> ToString[InputForm[N[roLtotal]]] <> ", roDtotal = " <> ToString[InputForm[N[roDtotal]]] <> ", nuMax = " <> ToString[InputForm[N[nuMax]]];
RunAllCoeffsAddError[errDescription];

roTotalPct=Indeterminate;
nuMax=Indeterminate;
)
];

If[(!NumericQ[roTotalPct]) || (!NumericQ[nuMax]),
(
errDescription="Non numeric values for "<>"ii = " <> ToString[ii] <> ", jj = " <> ToString[jj] <> ". roTotalPct = " <> ToString[InputForm[N[roTotalPct]]] <> ", nuMax = " <> ToString[InputForm[N[nuMax]]];
RunAllCoeffsAddError[errDescription];

roTotalPct=Indeterminate;
nuMax=Indeterminate;
)
];
)
];

evAllTbl[[ii,jj]]=Chop[N[maxEigenValue]];
NuAllTbl[[ii,jj]]=N[nuMax];
RoAllTbl[[ii,jj]]=N[roTotalPct];
gammaPlusTbl[[ii,jj]]=N[gammaPlusValue];
gammaMinusTbl[[ii,jj]]=N[gammaMinusValue];

If[symmetryBreakingFlag,
(
Print["RunAllCoeffs::ii = ", ii, ", jj = ", jj, ", maxEigenValue = ", N[maxEigenValue], ", nuMax = ", N[nuMax], ", roTotalPct = ",N[roTotalPct], ", coeffValues = ", N[Table[{ii,coeffArrayName[ii],coeffValues[[ii]]},{ii,1,NoCoeffCnt}]] // MatrixForm, ", eVec = ", N[eVec] // MatrixForm, ", allRo = ",Table[{substLst[[kk]]->N[allRo[[kk]]]},{kk,1,NoSubstCnt}] // MatrixForm];

If[BifurcationRunNDSolve || requestNDSolveRun,
(
substanceMatrix=Table[SubstanceMatrix[ii],{ii,1,NoSubstCnt}];
BifurcationPlotEvolution[MaxChainLength,InitializeActivationValue,tMaxVal,maxEigenValue,roTotInitVal,substanceMatrix,sol];
)
];

Print[strSeparator];
)
];
),{jj,1,allCoeffTblLen2}
];
),{ii,1,allCoeffTblLen1}
];

Print[strSeparator];
Print[strSeparator];
(* ============================================== *)

retVal={evAllTbl,NuAllTbl,RoAllTbl,gammaPlusTbl,gammaMinusTbl};

BifurcationOutputResults[retVal];
RunAllCoeffsOutputErrors[];

Return[retVal];
];

(* ============================================== *)
BifurcationPlotEvolution[maxChainLen_?IntegerQ,useActivation_?BooleanQ,tMaxVal_?NumericQ,maxEigenValue_?NumericQ,roTotInitVal_?NumericQ,substanceMatrix_,sol_]:=Module[{interpFunc,tPlotStart,tPlot,plotOptsT,xNameLbl,yNameLbl,f,tDomain,tEv,tt,substLst,substancePlotListVal,len,roVecRule,ii},
xNameLbl="t";
yNameLbl=" ";

plotOptsT:={PlotRange ->All,ImageSize -> BDIMAGESIZE,LabelStyle -> BDPLTTEXTOPTS,AxesLabel->{xNameLbl} ,Frame -> True, GridLines -> Automatic,PlotStyle -> Thick,PlotLegends:>Placed[SwatchLegend[{Style[legendNames[[1]],FontFamily->BDFONTFAMILY,FontSize->BDFONTSIZE,FontWeight->BDFONTWEIGHT],Style[legendNames[[2]],FontFamily->BDFONTFAMILY,FontSize->BDFONTSIZE,FontWeight->BDFONTWEIGHT],Style[legendNames[[3]],FontFamily->BDFONTFAMILY,FontSize->BDFONTSIZE,FontWeight->BDFONTWEIGHT],Style[legendNames[[4]],FontFamily->BDFONTFAMILY,FontSize->BDFONTSIZE,FontWeight->BDFONTWEIGHT]}],Above], Axes -> False ,Epilog -> Text[xNameLbl,Scaled[{0.95,0.1}], BaseStyle-> BDPLTTEXTOPTS] };

substancePlotListVal={rY,{rA,ra}};

If[useActivation,
(
substancePlotListVal=Join[substancePlotListVal,{{rA\:066d,ra\:066d}}];
)
];

If[maxChainLen>=2,
(
substancePlotListVal=Join[substancePlotListVal,{{rAA,raa},{rAa,raA}}];
)
];

If[maxChainLen>=3,
(
substancePlotListVal=Join[substancePlotListVal,{{rAAA,raaa},{rAAa,raaA},{rAaA,raAa},{rAaa,raAA}}];
)
];

Print["Plotting substances."];
roVecRule=Table[substanceMatrix[[ii]]-> substanceMatrix[[ii]][tt],{ii,1,Length[substanceMatrix]}];
interpFunc=\[Rho]AllGetInterpolationFunctions[sol];
f=SubstanceMatrix[1] /.interpFunc[[1]];
tDomain=f["Domain"][[1,2]];

If[maxEigenValue >EvPositiveEpsilon,
(
tEv=tEvPlotMultiplier*Log[1/\[Delta]\[Rho]Value]/maxEigenValue;
If[maxEigenValue<EvSmallValue,tEv=(tEvSmallValuePlotMultiplier*Log[1/\[Delta]\[Rho]Value]/maxEigenValue)];
),
(
tEv=tDomain;
)
];

tPlot=Min[tDomain,tEv,tMaxVal];
tPlotStart=0;

Do[
(
substLst=substancePlotListVal[[ii]] /.roVecRule;
Print[substancePlotListVal[[ii]]];

If[ToString[Head[substLst]]=="List",
(
len=Length[substLst];
SetLegends[substancePlotListVal[[ii]]];
Print[Plot[Evaluate[Table[(substLst[[ii]] /.interpFunc[[1]])/roTotInitVal,{ii,1,len}]],{tt,tPlotStart,tPlot},Evaluate[plotOptsT]]];
),
(
SetLegends[{substancePlotListVal[[ii]]}];
Print[Plot[Evaluate[substLst /.interpFunc[[1]]]/roTotInitVal,{tt,tPlotStart,tPlot},Evaluate[plotOptsT]]];
)
];
),{ii,Length[substancePlotListVal]}
];

SetLegends[{"\[Eta]"}];
Print[Plot[Abs[nuValue[\[Rho]TotalLValueFunc[sol,tt],\[Rho]TotalDValueFunc[sol,tt]]],{tt,tPlotStart,tPlot},Evaluate[plotOptsT]]];

];
(* ============================================== *)
BifurcationOutputResults[output_:{___}]:=Module[{evAllTbl,NuAllTbl,RoAllTbl,gammaPlusTbl,gammaMinusTbl,NuAllSortedLinear,RoAllSortedLinear,gammaPlusSortedLinear,gammaMinusSortedLinear,linearLen},
evAllTbl=output[[1]];
NuAllTbl=output[[2]];
RoAllTbl=output[[3]];
gammaPlusTbl=output[[4]];
gammaMinusTbl=output[[5]];

Print["Results"];
Print["evAllTbl = ", Chop[Transpose[N[evAllTbl]],10^-6] // MatrixForm, ", NuAllTbl = ",Chop[Transpose[N[NuAllTbl]],10^-6] // MatrixForm, ", RoAllTbl = ",Transpose[N[RoAllTbl]] // MatrixForm, ", gammaPlusTbl = ",Transpose[N[gammaPlusTbl]] // MatrixForm,", gammaMinusTbl = ",Transpose[N[gammaMinusTbl]] // MatrixForm];

Print[strSeparator];

Print["NuAllTbl"];
NuAllSortedLinear = Sort[Flatten[NuAllTbl]];
RoAllSortedLinear = SortByFirst[Flatten[NuAllTbl],Flatten[RoAllTbl]];
gammaPlusSortedLinear = SortByFirst[Flatten[NuAllTbl],Flatten[gammaPlusTbl]];
gammaMinusSortedLinear = SortByFirst[Flatten[NuAllTbl],Flatten[gammaMinusTbl]];

linearLen=Length[NuAllSortedLinear];

SetLegends[{"\[Eta]"}];
Print[DiscretePlot[NuAllSortedLinear[[linearLen+1-ii]],{ii,1,linearLen}, Evaluate[discrPlotOpts3]]];

SetLegends[{"\[Rho]"}];
Print[DiscretePlot[RoAllSortedLinear[[linearLen+1-ii]],{ii,1,linearLen}, Evaluate[discrPlotOpts3]]];

SetLegends[{"\[Gamma]+","\[Gamma]-"}];
Print[DiscretePlot[{gammaPlusSortedLinear[[linearLen+1-ii]],gammaMinusSortedLinear[[linearLen+1-ii]]},{ii,1,linearLen}, Evaluate[discrPlotOpts3]]];

SetLegends[{"\[Eta]","\[Rho]"}];
Print[DiscretePlot[{NuAllSortedLinear[[linearLen+1-ii]],RoAllSortedLinear[[linearLen+1-ii]]},{ii,1,linearLen}, Evaluate[discrPlotOpts3]]];

(*
SetLegends[{"\[Eta]","\[Rho]","\[Gamma]"}];
Print[DiscretePlot[{NuAllSortedLinear[[linearLen+1-ii]],RoAllSortedLinear[[linearLen+1-ii]],gammaSortedLinear[[linearLen+1-ii]]},{ii,1,linearLen}, Evaluate[discrPlotOpts3]]];
*)

PrintTimeUsed[];
];
(* ============================================== *)
(* BifurcationRun initializes all the equations and runs all variants if requested *)
BifurcationRun[maxChainLen_?IntegerQ,useActivation_?BooleanQ]:=BifurcationRun[maxChainLen,useActivation,0,False,{{}}];
BifurcationRun[maxChainLen_?IntegerQ,useActivation_?BooleanQ,tMaxVal_?NumericQ,allCoeffTblRule_]:=BifurcationRun[maxChainLen,useActivation,tMaxVal,True,allCoeffTblRule];
BifurcationRun[maxChainLen_?IntegerQ,useActivation_?BooleanQ,tMaxVal_?NumericQ,calculateAll_?BooleanQ,allCoeffTblRule_]:=BifurcationRun[maxChainLen,useActivation,tMaxVal,calculateAll,allCoeffTblRule,False];
(* ============================================== *)
BifurcationRun[maxChainLen_?IntegerQ,useActivation_?BooleanQ,tMaxVal_?NumericQ,calculateAll_?BooleanQ,allCoeffTblRule_,useNNT_?BooleanQ]:=Module[{retVal,options},
(* ============================================== *)
options=GeneratorInitializeModel[maxChainLen,useActivation,useNNT];
(* ============================================== *)
If[calculateAll,
(
Print["All substances: ", Table[{ii, SubstanceMatrix[ii]},{ii,1,NoSubstCnt}] // MatrixForm];
retVal=RunAllCoeffs[allCoeffTblRule, tMaxVal,options];
),
(
PrepareEquations[options];
retVal=Indeterminate;
)
];
(* ============================================== *)
Return[retVal];
];
(* ============================================== *)
GenerateAllBifurcationTests[maxChainLen_?IntegerQ,useActivation_?BooleanQ,{catMin_?IntegerQ,catMax_?IntegerQ},{dirSedMin_?IntegerQ,dirSedMax_?IntegerQ}]:=Module[{retValName,sTbl,tbl,printInfo,generatePrint,catCnt,dirSedCnt},
Print["GenerateAllBifurcationTests::Running version ", BifurcationGeneratorVersion];

BifurcationRun[maxChainLen,useActivation];
retValName="retVal";

printInfo=False;
generatePrint=False;

sTbl=Table[GenerateBifurcationTest[catCnt,dirSedCnt,GetUniqueName[retValName,catCnt,dirSedCnt],printInfo,generatePrint],{catCnt,catMin,catMax},{dirSedCnt,dirSedMin,dirSedMax}];

If[!SilentRunValue,
(
Print["sTbl[[1,1]] = ", sTbl[[1,1]]];
PrintTimeUsed[];
)
];

(* ============================================== *)

If[UseParallelTable,
(
tbl=ParallelTable[(Print["Starting NMinimize on " , DateString[],", catCnt = ", catCnt, ", dirSedCnt = ", dirSedCnt];ToExpression[sTbl[[catCnt-catMin+1,dirSedCnt-dirSedMin+1]]];Print["         NMinimize completed on " , DateString[],", catCnt = ", catCnt, ", dirSedCnt = ", dirSedCnt];ToExpression[GetUniqueName[retValName,catCnt,dirSedCnt]]),{catCnt,catMin,catMax},{dirSedCnt,dirSedMin,dirSedMax}];
),
(
tbl=Table[(Print["Starting NMinimize on " , DateString[],", catCnt = ", catCnt, ", dirSedCnt = ", dirSedCnt];ToExpression[sTbl[[catCnt-catMin+1,dirSedCnt-dirSedMin+1]]];Print["         NMinimize completed on " , DateString[],", catCnt = ", catCnt, ", dirSedCnt = ", dirSedCnt];ToExpression[GetUniqueName[retValName,catCnt,dirSedCnt]]),{catCnt,catMin,catMax},{dirSedCnt,dirSedMin,dirSedMax}];
)
];

(* ============================================== *)

If[!SilentRunValue,
(
Print["tbl = ", N[tbl]];
PrintTimeUsed[];
)
];

Print["GenerateAllBifurcationTests::Completed."];
PrintTimeUsed[];
Return[tbl];
];
(* ============================================== *)
BifurcationPostRun[maxChainLen_?IntegerQ,useActivation_?BooleanQ,tMaxVal_?NumericQ,coeffRules_?VectorQ, rhoRules_?VectorQ]:=Module[{sol,coeffValues,allRho,kk,substanceMatrix,maxEigenValue,roTotInitVal},
BifurcationRun[maxChainLen,useActivation,tMaxVal,False,{{}},True];
coeffValues=Table[coeffArrayName[kk],{kk,1,NoCoeffCnt}] /. coeffRules;
allRho=Table[Max[SubstanceMatrix[kk] /. rhoRules,0],{kk,1,NoSubstCnt}];
maxEigenValue=1;
roTotInitVal=Sum[allRho[[kk]],{kk,1,NoSubstCnt}];
substanceMatrix=Table[SubstanceMatrix[ii],{ii,1,NoSubstCnt}];

sol=\[Rho]AllFunc[coeffValues,allRho,tMaxVal,True,ApplyCoeffRule-> True,NDSolveMethod-> None];
BifurcationPlotEvolution[MaxChainLength,InitializeActivationValue,tMaxVal,maxEigenValue,roTotInitVal,substanceMatrix,sol];
PrintTimeUsed[];

Return[sol];
];
(* ============================================== *)
BifurcationPlotCoeff[maxChainLen_?IntegerQ,useActivation_?BooleanQ,catSynthIdx_?IntegerQ,dirSedIdx_?IntegerQ,r00Min_?NumericQ,r00Max_?NumericQ,noOfPoints_?IntegerQ]:=Module[{allCoeffTblRule,retValName,printInfo,generatePrint,sExpr,r00Lst,retValLst,RunBifurcationTestName,evLst,cpLst,cmLst,apLst,amLst,LpLst,LmLst,ii,paramPlotOpts,xFunc,yFunc,discrPlotOpts},

Print["BifurcationPlotCoeff::Running version ", BifurcationGeneratorVersion];

discrPlotOpts={PlotRange->All,PlotMarkers->{Automatic,Medium},Frame->True,GridLines->Automatic,ImageSize->500,LabelStyle->{FontFamily->"Courier",FontSize->16,FontWeight->"Bold"},AxesLabel->{"x","y"},ExtentSize->Full,ExtentElementFunction->"FadingRectangle",AxesOrigin->{0,0},PlotLegends->Placed[SwatchLegend[{"e","y","z","t"}],Above]};

paramPlotOpts[var_]:={Frame -> True,GridLines->Automatic, PlotPoints->(noOfPoints+1), AspectRatio->1/GoldenRatio,ImageSize->500,LabelStyle->{FontFamily->"Courier",FontSize->16,FontWeight->"Bold"}, Epilog->{Text[lg[Subscript[r,0]],Scaled[{0.93,0.05}],BaseStyle->BDPLTTEXTOPTS],Text[lg[var],Scaled[{0.07,0.90}],BaseStyle->BDPLTTEXTOPTS]}};

BifurcationRun[maxChainLen,useActivation];
retValName="retVal";
printInfo=False;
generatePrint=False;

RunBifurcationTestName=GetUniqueName["RunBifurcationTest",catSynthIdx,dirSedIdx];
sExpr=GenerateBifurcationTest[catSynthIdx,dirSedIdx,GetUniqueName[retValName,catSynthIdx,dirSedIdx],printInfo,generatePrint];
ToExpression[sExpr];

r00Lst=Table[N[Exp[(Log[r00Min]+(Log[r00Max]-Log[r00Min])*(ii/noOfPoints))]],{ii,0,noOfPoints}];
Print["r00Lst = ", r00Lst];

retValLst=ParallelTable[(Print[DateString[],", ii = ", ii, ", r00Lst[[",ii,"]] = ", r00Lst[[ii]]];Quiet[ToExpression[RunBifurcationTestName <> "[" <> ToString[InputForm[r00Lst[[ii]]]] <> "]"]]),{ii,1,noOfPoints+1}];

evLst=Table[GetBifurcationMaxEigenValue[retValLst[[ii]]],{ii,1,noOfPoints+1}];
cpLst=Table[(coeff\:0df4Y\:2794A /. GetBifurcationAllCoeffs[retValLst[[ii]]]),{ii,1,noOfPoints+1}];
cmLst=Table[(coeff\:0df4A\:2794Y /. GetBifurcationAllCoeffs[retValLst[[ii]]]),{ii,1,noOfPoints+1}];
apLst=Table[(coeff\:0df4A\:2794A\:066d /. GetBifurcationAllCoeffs[retValLst[[ii]]]),{ii,1,noOfPoints+1}];
amLst=Table[(coeff\:0df4A\:066d\:2794A /. GetBifurcationAllCoeffs[retValLst[[ii]]]),{ii,1,noOfPoints+1}];
LpLst=Table[(coeff\:0df4A\:066d\:02d6A\:2794AA /. GetBifurcationAllCoeffs[retValLst[[ii]]]),{ii,1,noOfPoints+1}];
LmLst=Table[(coeff\:0df4AA\:2794A\:02d6A /. GetBifurcationAllCoeffs[retValLst[[ii]]]),{ii,1,noOfPoints+1}];

xFunc[ii_?NumericQ]:=Log10[r00Lst[[Round[ii]]]];
yFunc[ii_?NumericQ,source_?VectorQ]:=(Log10[source[[Round[ii]]]]);

Print["evLst = ", N[evLst]];
Print["cpLst = ", N[cpLst]];
Print["cmLst = ", N[cmLst]];
Print["apLst = ", N[apLst]];
Print["amLst = ", N[amLst]];
Print["LpLst = ", N[LpLst]];
Print["LmLst = ", N[LmLst]];

PrintTimeUsed[];
(* ============================================== *)
Print["Eigenvalues"];
Print[DiscretePlot[evLst[[ii]],{ii,1,noOfPoints+1}, Evaluate[discrPlotOpts]]];

Print["Log10[Eigenvalues]"];
Print[DiscretePlot[Log10[evLst[[ii]]],{ii,1,noOfPoints+1}, Evaluate[discrPlotOpts]]];

Print["Log10[Eigenvalues] + 5"];
Print[DiscretePlot[Log10[evLst[[ii]]]+5,{ii,1,noOfPoints+1}, Evaluate[discrPlotOpts]]];

Print[strSeparator];
(* ============================================== *)
Print["Eigenvalues"];
Print[ParametricPlot[{xFunc[ii],yFunc[ii,evLst]},{ii,1,noOfPoints}, Evaluate[paramPlotOpts[e]]]];
Print[strSeparator];
(* ============================================== *)
Print["c\:02d6"];
Print[ParametricPlot[{xFunc[ii],yFunc[ii,cpLst]},{ii,1,noOfPoints}, Evaluate[paramPlotOpts[c\:02d6]]]];
Print[strSeparator];
(* ============================================== *)
Print["c\:02d7"];
Print[ParametricPlot[{xFunc[ii],yFunc[ii,cmLst]},{ii,1,noOfPoints}, Evaluate[paramPlotOpts[c\:02d7]]]];
Print[strSeparator];
(* ============================================== *)
Print["a\:02d6"];
Print[ParametricPlot[{xFunc[ii],yFunc[ii,apLst]},{ii,1,noOfPoints}, Evaluate[paramPlotOpts[a\:02d6]]]];
Print[strSeparator];
(* ============================================== *)
Print["a\:02d7"];
Print[ParametricPlot[{xFunc[ii],yFunc[ii,amLst]},{ii,1,noOfPoints}, Evaluate[paramPlotOpts[a\:02d7]]]];
Print[strSeparator];
(* ============================================== *)
Print["\[CapitalLambda]\:02d6"];
Print[ParametricPlot[{xFunc[ii],yFunc[ii,LpLst]},{ii,1,noOfPoints}, Evaluate[paramPlotOpts[\[CapitalLambda]\:02d6]]]];
Print[strSeparator];
(* ============================================== *)
Print["\[CapitalLambda]\:02d7"];
Print[ParametricPlot[{xFunc[ii],yFunc[ii,LmLst]},{ii,1,noOfPoints}, Evaluate[paramPlotOpts[\[CapitalLambda]\:02d7]]]];
Print[strSeparator];
(* ============================================== *)
BifurcationGeneratorPrintInfo[];
(* ============================================== *)
PrintTimeUsed[];
(* ============================================== *)
];
(* ============================================== *)
BifurcationGeneratorPrintInfo[]:=Module[{},
Print["TODO::BifurcationGeneratorPrintInfo::Print all information about parameters..."];
Print["Bifurcation Generator parameters:"];
(* GetVarName[xxx]; *)
PrintTimeUsed[];
];
(* ============================================== *)


