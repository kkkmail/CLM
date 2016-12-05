(* ============================================== *)
(* :Summary: Various calculations for CLM. *)
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
(* Needs["DifferentialEquations`NDSolveProblems`"]; *)
(* Needs["DifferentialEquations`NDSolveUtilities`"]; *)
(* ============================================== *)
ModelList={};
NewCoeffModelType=1;
(* ============================================== *)
If[!SilentRunValue,Print["TODO::CLM_Calc::GetValues, ApplyAllRacemization, ApplyAllGamma, etc..., GetXYrange - fix to support indexers..."]];

(* Function GetValues takes array of parameters (paramsArray) in the form of either exact values or a range of values and {x point index, number of x points}, {y point index, number of y points} and return the appropriate values corresponding to x and y indices. First two found ranges are used for the two dimensional grid. Racemization and one of the Gamma parameters are applied ONLY if they are a range. *)
GetValues[paramsArray:{__},xIdxRange:{_,_},yIdxRange:{_,_},rawOptions___]:=Module[{xIdx,xStart,xEnd,yIdx,yStart,yEnd,pltPointsX,pltPointsY,cnt,param,values,paramLen,xAssigned,yAssigned,xVal,yVal,mValue,useCoeffTransformVal,opts,gammaValue,gammaPlusValue,gammaMinusValue},

(*
Print["GetValues::Starting."];
Print["GetValues::paramsArray = ", N[paramsArray]];
*)

opts=ProcessOptions[rawOptions];
useCoeffTransformVal=UseCoeffTransform /. opts /. Options[CLMS];

paramLen=Length[paramsArray];
If[paramLen!= coeffIdxMax,(Print["GetValues::Invalid paramsArray."]; Return[Indeterminate];)];

mValue=Flatten[{paramsArray[[coeffIdxRacemization]]}][[1]];
gammaValue=Flatten[{paramsArray[[coeffIdxGamma]]}][[1]];
gammaPlusValue=Flatten[{paramsArray[[coeffIdxGammaPlus]]}][[1]];
gammaMinusValue=Flatten[{paramsArray[[coeffIdxGammaMinus]]}][[1]];

values=Table[0,{cnt,1,paramLen}];

xAssigned=False;
yAssigned=False;

xIdx=xIdxRange[[1]];
pltPointsX=xIdxRange[[2]];

yIdx=yIdxRange[[1]];
pltPointsY=yIdxRange[[2]];

For[cnt=1, cnt <=coeffIdxMax,cnt++,
param=Flatten[{paramsArray[[cnt]]}];
(* Print["GetValues::cnt = ", cnt, ", param = ", N[param]]; *)
values[[cnt]]=param[[1]];

If[Length[param]==2,
If[xAssigned==False,
(
xStart=param[[1]];
xEnd=param[[2]];
xVal=xStart+(xIdx-1)*(xEnd-xStart)/(pltPointsX-1);
values[[cnt]]=xVal;
xAssigned=True;

If[cnt==coeffIdxRacemization,mValue=xVal];
If[cnt==coeffIdxGamma,gammaValue=xVal];
If[cnt==coeffIdxGammaPlus,gammaPlusValue=xVal];
If[cnt==coeffIdxGammaMinus,gammaMinusValue=xVal];
),
(
If[yAssigned==False,
(
yStart=param[[1]];
yEnd=param[[2]];
yVal=yStart+(yIdx-1)*(yEnd-yStart)/(pltPointsY-1);
values[[cnt]]=yVal;
yAssigned=True;

If[cnt==coeffIdxRacemization,mValue=yVal];
If[cnt==coeffIdxGamma,gammaValue=yVal];
If[cnt==coeffIdxGammaPlus,gammaPlusValue=yVal];
If[cnt==coeffIdxGammaMinus,gammaMinusValue=yVal];
)
];
)
];
];
];

If[xAssigned==False || yAssigned==False,Print["GetValues::Unable to assign X or Y variable."]];

values=ApplyAllRacemization[values,mValue,rawOptions];
values=ApplyAllGamma[values,{gammaValue,gammaPlusValue,gammaMinusValue},rawOptions];

If[useCoeffTransformVal==True,values=TransformCoeffValues[values]];

(* Print["GetValues::Return Value = ", N[values]]; *)

Return[values];
];
(* ============================================== *)
ApplyAllRacemization[values:{__},mValue_,rawOptions___]:=Module[{retVal,idx,ii},
retVal=values;

(* Print["ApplyRacemization::Called."]; *)

For[ii=1, ii <= MaxGenerationLevel,ii++,
(
retVal=ApplyRacemization[retVal,mValue,ii,rawOptions];
)
];

Return[retVal];
];
(* ============================================== *)
(* Applies Gamma values if any of them is used. *)
ApplyAllGamma[values:{__},gammaAllValues:{_,_,_},rawOptions___]:=Module[{retVal,gammaValue,gammaPlusValue,gammaMinusValue,opts,useGammaVal,useGammaPlusVal,useGammaMinusVal},

(* Print["ApplyAllGamma::Called."]; *)

retVal=values;
gammaValue=gammaAllValues[[1]];
gammaPlusValue=gammaAllValues[[2]];
gammaMinusValue=gammaAllValues[[3]];

useGammaVal=False;
useGammaPlusVal=False;
useGammaMinusVal=False;

opts=ProcessOptions[rawOptions];

(* If Gamma is used, then GammaPlus and GammaMinus are ignored. If GammaPlus is used then GammaMinus is ignored. *)
useGammaVal=UseGamma /. opts /. Options[CLMS];

If[useGammaVal == True,
(
retVal=ApplyGamma[values,gammaValue,rawOptions];
),
(
useGammaPlusVal=UseGammaPlus /. opts /. Options[CLMS];
If[useGammaPlusVal == True,
(
retVal=ApplyGammaPlus[values,gammaPlusValue,rawOptions];
),
(
useGammaMinusVal=UseGammaMinus /. opts /. Options[CLMS];
If[useGammaPlusVal == True,retVal=ApplyGammaMinus[values,gammaMinusValue,rawOptions];];
)
];
)
];

Return[retVal];
];
(* ============================================== *)
(* Applies gamma value. Gamma+ and Gamma- are halfs of Gamma. *)
ApplyGamma[values:{__},gammaValue_,rawOptions___]:=Module[{retVal,gammaPlusValue,gammaMinusValue},

(* Print["ApplyGamma::Called."]; *)

gammaPlusValue=gammaValue /2;
gammaMinusValue=gammaValue /2;

retVal=ApplyGammaPlus[values,gammaPlusValue,rawOptions];
retVal=ApplyGammaMinus[retVal,gammaMinusValue,rawOptions];
Return[retVal];
];
(* ============================================== *)
ApplyGammaPlus[values:{__},gammaPlusValue_,rawOptions___]:=Module[{retVal,coeffAAStoXSval,coeffAAStoSSval,coeffAAStoNSSval},

(* Print["ApplyGammaPlus::Called."]; *)

coeffAAStoXSval=values[[coeffIdxAAStoXS]];

coeffAAStoSSval=GetCoeffAAStoSS[gammaPlusValue,coeffAAStoXSval];
coeffAAStoNSSval=GetCoeffAAStoNSS[gammaPlusValue,coeffAAStoXSval];

retVal=values;
retVal[[coeffIdxAAStoSS]]=coeffAAStoSSval;
retVal[[coeffIdxAAStoNSS]]=coeffAAStoNSSval;
Return[retVal];
];
(* ============================================== *)
ApplyGammaMinus[values:{__},gammaMinusValue_,rawOptions___]:=Module[{retVal,coeffSNSto2ANSval,coeffSSto2ASval,coeffXSto2ASval},

(* Print["ApplyGammaMinus::Called."]; *)

coeffXSto2ASval=values[[coeffIdxXSto2AS]];

coeffSNSto2ANSval=GetCoeffSNSto2ANS[gammaMinusValue,coeffXSto2ASval];
coeffSSto2ASval=GetCoeffSSto2AS[gammaMinusValue,coeffXSto2ASval];

retVal=values;
retVal[[coeffIdxSNSto2ANS]]=coeffSNSto2ANSval;
retVal[[coeffIdxSSto2AS]]=coeffSSto2ASval;
Return[retVal];
];
(* ============================================== *)
(* GetXYrange returns the range of X and Y along with the names. *)
GetXYrange[paramsArray_?VectorQ,UseCoeffTransform_?BooleanQ]:=Module[{paramLen,xRange,yRange,xAssigned,yAssigned,cnt,param},
paramLen=Length[paramsArray];
If[paramLen!= NoCoeffCnt,(Print["GetXYrange::Invalid paramsArray."]; Return[Indeterminate];)];

xAssigned=False;
yAssigned=False;

For[cnt=1, cnt <=paramLen,cnt++,
param=Flatten[{paramsArray[[cnt]]}];

If[Length[param]==2,
If[xAssigned==False,
(
xRange={param[[1]],param[[2]],GetCoeffName[cnt,UseCoeffTransform]};
xAssigned=True;
),
(
If[yAssigned==False,
(
yRange={param[[1]],param[[2]],GetCoeffName[cnt,UseCoeffTransform]};
yAssigned=True;
)
];
)
];
];
];

If[xAssigned==False || yAssigned==False,Print["GetXYrange::Unable to assign X or Y range."]];

Return[{xRange,yRange}];
];
(* ============================================== *)
CreateInitValuesL[paramsArray:{__},pltPointsX_?NumericQ,pltPointsY_?NumericQ,xIdx_?NumericQ,yIdx_?NumericQ,rawOptions___]:=Module[{initValues,coeffValues,ro0Val,opts,UseCoeffTransformVal,ForceZeroLRInitialAmountVal,LRInitialAmountVal,UseZSubstanceForRo0Val},
opts=ProcessOptions[rawOptions];
UseCoeffTransformVal=UseCoeffTransform /. opts /. Options[CLMS];
ForceZeroLRInitialAmountVal =ForceZeroLRInitialAmount/. opts /. Options[CLMS];
UseZSubstanceForRo0Val = UseZSubstanceForRo0 /.opts /. Options[CLMS];
(* Print["CreateInitValuesL::UseZSubstanceForRo0Val = ", UseZSubstanceForRo0Val]; *)

LRInitialAmountVal=If[ForceZeroLRInitialAmountVal!= True,
(LRInitialAmount/. opts /. Options[CLMS]),
0,(Print["CreateInitValuesL::ForceZeroLRInitialAmount is undefined. Using LRInitialAmount = 0."]; 0)
];

initValues=CreateZeroInitValues[];
coeffValues=GetValues[paramsArray,{xIdx,pltPointsX},{yIdx,pltPointsY},rawOptions];
ro0Val=coeffValues[[coeffIdx\[Rho]0]];

If[UseZSubstanceForRo0Val==True,
(initValues[[idxZ]]=ro0Val*(1-nnn*LRInitialAmountVal)),
(initValues[[idxA]]=ro0Val*(1-nnn*LRInitialAmountVal))
];

initValues[[idxL]]=ro0Val*LRInitialAmountVal;

(* Print["CreateInitValuesL::initValues = ", initValues]; *)

Return[initValues];
];
(* ============================================== *)
CreateInitValuesR[paramsArray:{__},pltPointsX_?NumericQ,pltPointsY_?NumericQ,xIdx_?NumericQ,yIdx_?NumericQ,rawOptions___]:=Module[{initValues,coeffValues,ro0Val,opts,UseCoeffTransformVal,ForceZeroLRInitialAmountVal,LRInitialAmountVal,UseZSubstanceForRo0Val},

opts=ProcessOptions[rawOptions];
UseCoeffTransformVal=UseCoeffTransform /. opts /. Options[CLMS];
ForceZeroLRInitialAmountVal =ForceZeroLRInitialAmount/. opts /. Options[CLMS];
UseZSubstanceForRo0Val = UseZSubstanceForRo0 /.opts /. Options[CLMS];

LRInitialAmountVal=If[ForceZeroLRInitialAmountVal!= True,
(LRInitialAmount/. opts /. Options[CLMS]),
0,(Print["CreateInitValuesR::ForceZeroLRInitialAmount is undefined. Using LRInitialAmount = 0."]; 0)
];

initValues=CreateZeroInitValues[];
coeffValues=GetValues[paramsArray,{xIdx,pltPointsX},{yIdx,pltPointsY},rawOptions];
ro0Val=coeffValues[[coeffIdx\[Rho]0]];

If[UseZSubstanceForRo0Val==True,
(initValues[[idxZ]]=ro0Val*(1-nnn*LRInitialAmountVal)),
(initValues[[idxA]]=ro0Val*(1-nnn*LRInitialAmountVal))
];

initValues[[idxR]]=ro0Val*LRInitialAmountVal;

(* Print["CreateInitValuesR::initValues = ", initValues]; *)

Return[initValues];
];
(* ============================================== *)
(* GetCalcGridArray returns an array of solutions. *)
GetCalcGridArray[paramsArray:{__},tMaxVal_?NumericQ,pltPointsX_?NumericQ,pltPointsY_?NumericQ,rawOptions___]:=Module[{CalcGridArrL,CalcGridArrR,ii,jj, retVal,opts,UseCoeffTransformVal,LRInitialAmountVal,ForceZeroLRInitialAmountVal,UseZSubstanceForRo0Val},
(* Print["GetCalcGridArray::Started"]; *)

opts=ProcessOptions[rawOptions];

UseCoeffTransformVal=UseCoeffTransform /. opts /. Options[CLMS];
ForceZeroLRInitialAmountVal =ForceZeroLRInitialAmount/. opts /. Options[CLMS];

LRInitialAmountVal=If[ForceZeroLRInitialAmountVal!= True,
(LRInitialAmount/. opts /. Options[CLMS]),
0,(Print["GetCalcGridArray::ForceZeroLRInitialAmount is undefined. Using LRInitialAmount = 0."]; 0)
];

(* Print["GetCalcGridArray::LRInitialAmountVal = ", LRInitialAmountVal]; *)

UseZSubstanceForRo0Val = UseZSubstanceForRo0 /.opts /. Options[CLMS];

DistributeDefinitions[\[Rho]AllFunc,GetValues,paramsArray,pltPointsX,pltPointsY,UseCoeffTransformVal,CreateInitValuesL,CreateInitValuesR,tMaxVal,cdEps,CDmultVal,cdFunc0,cdFunc,cdSplineFunc,crystallize,dissolve,CrystallizeDissolve,CrystallizeDissolve1,CreateZeroInitValues,BooleanQ,coeffIdx\[Rho]0,idxA,idxR,idxL,LRInitialAmountVal,ApplyRacemization,ApplyAllRacemization,ApplyAllGamma,ApplyGamma, ApplyGammaPlus, ApplyGammaMinus];

CalcGridArrL=ParallelTable[\[Rho]AllFunc[GetValues[paramsArray,{ii,pltPointsX},{jj,pltPointsY},rawOptions],CreateInitValuesL[paramsArray,pltPointsX,pltPointsY,ii,jj,rawOptions],tMaxVal,False,rawOptions],{ii,1,pltPointsX},{jj,1,pltPointsY}];

CalcGridArrR=If[ForceZeroLRInitialAmountVal!=True,
ParallelTable[\[Rho]AllFunc[GetValues[paramsArray,{ii,pltPointsX},{jj,pltPointsY},rawOptions],CreateInitValuesR[paramsArray,pltPointsX,pltPointsY,ii,jj,rawOptions],tMaxVal,False,rawOptions],{ii,1,pltPointsX},{jj,1,pltPointsY}],
Table[\[Rho]AllIndeterminateFunc[],{ii,1,pltPointsX},{jj,1,pltPointsY}],
Table[\[Rho]AllIndeterminateFunc[],{ii,1,pltPointsX},{jj,1,pltPointsY}]
];

WaitAll[CalcGridArrL];
WaitAll[CalcGridArrR];

retVal={CalcGridArrL,CalcGridArrR};

(* Print["GetCalcGridArray::retVal = ", retVal]; *)

Return[retVal];
];
(* ============================================== *)
\[Eta]ValueFunc[generationLevel_?IntegerQ,solution:{__},tVal_?NumericQ]:=Module[{retVal,t,ii,roT,interpFunc},
interpFunc=\[Rho]AllGetInterpolationFunctions[solution];
roT=Table[(SubstanceMatrix[ii][t] /.interpFunc[[1]]) /. {t -> tVal},{ii,1,NoSubstCnt}];
retVal=\[Eta]Func[generationLevel,roT];
Return[retVal];
];
(* ============================================== *)
IsNuWeakValueTestPassed[sol_,tMaxVal_?NumericQ,rawOptions___]:=Module[{retVal,ii,sumVal,opts,doNotOutputWeakModelsVal,weakNuVal},
opts=ProcessOptions[rawOptions];
doNotOutputWeakModelsVal=DoNotOutputWeakModels /. opts /. Options[CLMS];
weakNuVal=WeakNuValue /. opts /. Options[CLMS];
(*
Print["IsNuWeakValueTestPassed::Starting."];
Print["IsNuWeakValueTestPassed::rawOptions = ", rawOptions];
Print["IsNuWeakValueTestPassed::doNotOutputWeakModelsVal = ", doNotOutputWeakModelsVal];
*)
retVal=True;

If[doNotOutputWeakModelsVal==True,
(
(* Print["IsNuWeakValueTestPassed::Checking model weakness."]; *)
sumVal=0;
For[ii=1,ii <= MaxGenerationLevel,ii++,
(
If[IsGenerationDefined[ii]==True,(sumVal=sumVal+\[Eta]ValueFunc[ii,sol,tMaxVal]^2)];
)
];
sumVal=Sqrt[sumVal];
retVal=If[sumVal <= weakNuVal,False,True,True];
)
];

(*
Print["IsNuWeakValueTestPassed::weakNuVal = ", weakNuVal];
Print["IsNuWeakValueTestPassed::sumVal = ", sumVal];
Print["IsNuWeakValueTestPassed::retVal = ", retVal];
*)
Return[retVal];
];
(* ============================================== *)
(* \[Rho]AllFunc calculates all \[Rho]'s[t] for given parameters AND coefficients and returns InterpolationFunction *)
\[Rho]AllGetValues[solution:{__}]:=solution[[1]];
\[Rho]AllGetFuncValues[solution:{__}]:=solution[[2]];
\[Rho]AllGetInterpolationFunctions[solution:{__}]:=solution[[3]];
(* ============================================== *)
\[Rho]AllValueFunc[solution:{__},\[Rho]IdxVal_?NumericQ,tVal_?NumericQ]:=Module[{retVal,interpFunc,t},
interpFunc=\[Rho]AllGetInterpolationFunctions[solution];
retVal=(SubstanceMatrix[\[Rho]IdxVal][t] /.interpFunc[[1]]) /. {t -> tVal};
Return[retVal];
];
(* ============================================== *)
\[Rho]AllValueVectorFunc[solution:{__},\[Rho]MaxIdxVal_?NumericQ,tVal_?NumericQ]:=Module[{retVal,interpFunc,t,ii},
interpFunc=\[Rho]AllGetInterpolationFunctions[solution];
retVal=Table[(SubstanceMatrix[ii][t] /.interpFunc[[1]]) /. {t -> tVal},{ii,1,\[Rho]MaxIdxVal}];
Return[retVal];
];
(* ============================================== *)
\[Rho]TotalLValueFunc[solution:{__},tVal_?NumericQ]:=Module[{retVal,interpFunc,t,roVec,ii},
interpFunc=\[Rho]AllGetInterpolationFunctions[solution];
roVec=Table[((SubstanceMatrix[ii][t] /.interpFunc[[1]]) /. {t -> tVal}),{ii,1,NoSubstCnt}];
retVal=TotalRoL[roVec];
Return[retVal];
];
(* ============================================== *)
\[Rho]TotalDValueFunc[solution:{__},tVal_?NumericQ]:=Module[{retVal,interpFunc,t,roVec,ii},
interpFunc=\[Rho]AllGetInterpolationFunctions[solution];
roVec=Table[((SubstanceMatrix[ii][t] /.interpFunc[[1]]) /. {t -> tVal}),{ii,1,NoSubstCnt}];
retVal=TotalRoD[roVec];
Return[retVal];
];
(* ============================================== *)
\[Rho]ChainLevelValueFunc[solution:{__},tVal_?NumericQ,level_?IntegerQ]:=Module[{retVal,interpFunc,t,roVec,ii},
interpFunc=\[Rho]AllGetInterpolationFunctions[solution];
roVec=Table[((SubstanceMatrix[ii][t] /.interpFunc[[1]]) /. {t -> tVal}),{ii,1,NoSubstCnt}];
retVal=RoChainLevel[roVec,level];
Return[retVal];
];
(* ============================================== *)
\[Rho]ChainLevelTblValueFunc[solution:{__},tVal_?NumericQ]:=Module[{retVal,interpFunc,t,roVec,ii},
interpFunc=\[Rho]AllGetInterpolationFunctions[solution];
roVec=Table[((SubstanceMatrix[ii][t] /.interpFunc[[1]]) /. {t -> tVal}),{ii,1,NoSubstCnt}];
retVal=RoChainLevelTbl[roVec];
Return[retVal];
];
(* ============================================== *)
(* \[Eta]Func is a chiral polarization or L and R *)
\[Eta]TotalFunc[solution:{__},tVal_?NumericQ]:=Module[{retVal,rLVal,rRVal},
rLVal=\[Rho]TotalLValueFunc[solution,tVal];
rRVal=\[Rho]TotalDValueFunc[solution,tVal];

retVal=nuValue[rLVal,rRVal];

Return[retVal];
];
(* ============================================== *)

\[Rho]AllIndeterminateFunc[]:=Module[{retVal,rotMaxVal,funcVal,ii},
rotMaxVal=Table[Indeterminate,{ii,1,NoSubstCnt}];
funcVal={Indeterminate,Indeterminate};
retVal={rotMaxVal,funcVal,Indeterminate};
Return[retVal];
];

(* ============================================== *)
QuitRequested=False;
PrevStepMonitorArrayCounter=0;
(* ============================================== *)
QuitFunction[]:=Module[{},
QuitRequested=True;
Quit[];
];
(* ============================================== *)
\[Rho]AllFunc[coeffValues_?VectorQ,initValues_?VectorQ,tMaxVal_]:=\[Rho]AllFunc[coeffValues,initValues,tMaxVal,False];

\[Rho]AllFunc[coeffValues_?VectorQ,initValues_?VectorQ,tMaxVal_,OutputSolution_?BooleanQ,rawOptions___]:=Module[{retVal, ii,coeffLen,initLen,coeffRuleTbl,eqInitTbl,rRuleTbl,t,smVarTbl,DsmVarTbl,eqTbl,eqAllTbl,sol,nMaxSteps,nAccuracyGoal,rotMaxVal,funcVal,useHighPrecisionVal,opts,useZeroAEquationVal,print\[Rho]AllFuncInfoVal,clmWorkingPrecisionVal,methodVal,startingStepSizeVal,ndSolveLst,accuracyGoalVal,precisionGoalVal,workingPrecisionVal,tauRule,applyCoeffRuleVal,doNotUseInterpolationFunctionVal,stepMonitorFunctionVal,monitor,monitorIndex,timeIndex,varVals,jj,evaluationMonitorFunctionVal,monitorEvalIndex,monitorEval,timeIndexEval,varValsEval,substMatrix,stepMonitorZeroVal,evalMonitorZeroVal,quitMinotorVal,timeFastIndex,nsMuilt,useOneMonitorVal,deleteIdLst,useWhenEventVal,eqWE,monitorTypeVal,monitorPrintFrequencyVal,useMonitor,useShowProgressVal,showProgress,roLtot,roDtot,nuTotVal,rLVal, rRVal, nuVal, roPLval, roPDval, nuPval, roCLval, roCDval, nuCval,roAllFuncDescriptionVal,mem,memMax,useDerivativeForAggregateSubstVal,useFuncForAggregateSubstVal,argStr,funcValStr,funcNameStr,funcDeclStr,funcCallStr,argCallStr, argTbl,argLen,argCallTbl,defRule,varName,newRuleTbl,useNumericFuncForAggregateSubstVal,funcTstr,funcTstrName,rLValTbl,rRValTbl,nuValTbl,roPLvalTbl,roPDvalTbl,nuPvalTbl,roCLvalTbl,roCDvalTbl,nuCvalTbl},
coeffLen=Length[coeffValues];
initLen=Length[initValues];

If[!SilentRunValue,
(
Print["\[Rho]AllFunc::Starting..."];
Print["TODO::\[Rho]AllFunc::Something is fucked up with monitors. They seem to slow down NDSolve in about 60-80 times!!!"];
PrintTimeUsed[];
)
];

QuitRequested=False;
PrevStepMonitorArrayCounter=0;

If[coeffLen!= NoCoeffCnt,(Print["\[Rho]AllFunc::Invalid coeffValues."]; Return[Indeterminate];)];
If[initLen!= NoSubstCnt,(Print["\[Rho]AllFunc::Invalid initValues."]; Return[Indeterminate];)];

opts=ProcessOptions[rawOptions];
useZeroAEquationVal=UseZeroAEquation /. opts /. Options[CLMS];
print\[Rho]AllFuncInfoVal=Print\[Rho]AllFuncInfo/. opts /. Options[CLMS];
useHighPrecisionVal=UseHighPrecision /. opts /. Options[CLMS];
clmWorkingPrecisionVal=ClmWorkingPrecision /. opts /. Options[CLMS];

nMaxSteps=NDSolveMaxSteps /. opts /. Options[CLMS];
methodVal=NDSolveMethod/. opts /. Options[CLMS];
startingStepSizeVal=NDSolveStepSize/. opts /. Options[CLMS];
accuracyGoalVal=NDSolveAccuracyGoal /. opts /. Options[CLMS];
precisionGoalVal=NDSolvePrecisionGoal /. opts /. Options[CLMS];
workingPrecisionVal=NDSolveWorkingPrecision /. opts /. Options[CLMS];
applyCoeffRuleVal=ApplyCoeffRule /. opts /. Options[CLMS];
doNotUseInterpolationFunctionVal=DoNotUseInterpolationFunction /. opts /. Options[CLMS];
stepMonitorFunctionVal=StepMonitorFunction /. opts /. Options[CLMS];
evaluationMonitorFunctionVal=EvaluationMonitorFunction /. opts /. Options[CLMS];
NStorageValue=NStorage /. opts /. Options[CLMS];
NStorageMultiplierValue=NStorageMultiplier /. opts /. Options[CLMS];
nsMuilt=NStorageMultiplierValue;
dynamicStepMonitorVal=DynamicStepMonitor/. opts /. Options[CLMS];
quitMinotorVal=QuitMonitor/. opts /. Options[CLMS];
useOneMonitorVal=UseOneMonitor/. opts /. Options[CLMS];
useWhenEventVal=UseWhenEvent/. opts /. Options[CLMS];
monitorTypeVal=MonitorType/. opts /. Options[CLMS];
monitorPrintFrequencyVal=MonitorPrintFrequency/. opts /. Options[CLMS];
useShowProgressVal=UseShowProgress/. opts /. Options[CLMS];
roAllFuncDescriptionVal=\[Rho]AllFuncDescription/. opts /. Options[CLMS];
useDerivativeForAggregateSubstVal=NDSolveUseDerivativeForAggregateSubst/. opts /. Options[CLMS];
useFuncForAggregateSubstVal=NDSolveUseFuncForAggregateSubst/. opts /. Options[CLMS];
useNumericFuncForAggregateSubstVal=NDSolveUseNumericFuncForAggregateSubst /. opts /. Options[CLMS];

useMonitor=False;

If[!SilentRunValue,If[roAllFuncDescriptionVal!="",Print["\[Rho]AllFunc::Description: ", roAllFuncDescriptionVal]]];

tauRule={\[Tau] -> t};

timeStartValue=AbsoluteTime[];
timeNowValue=timeStartValue;
timePrevValue=timeStartValue;

coeffRuleTbl=If[applyCoeffRuleVal,(Table[coeffArrayName[ii] -> coeffValues[[ii]],{ii,1,NoCoeffCnt}] /. tauRule),{}];
eqInitTbl=Table[SubstanceMatrix[ii][0]== initValues[[ii]],{ii,1,NoSubstCnt}];
rRuleTbl=Table[SubstanceMatrix[ii]-> SubstanceMatrix[ii][t],{ii,1,NoSubstCnt}];
substMatrix=Table[SubstanceMatrix[ii],{ii,1,NoSubstCnt}];

If[print\[Rho]AllFuncInfoVal,
(
Print["\[Rho]AllFunc::coeffRuleTbl = ", coeffRuleTbl];
Print["\[Rho]AllFunc::eqInitTbl = ", eqInitTbl];
Print["\[Rho]AllFunc::rRuleTbl = ", rRuleTbl];
Print["\[Rho]AllFunc::substMatrix = ", substMatrix];
)
];

If[!SilentRunValue,Print["TODO::\[Rho]AllFunc::Fix call to If[useZeroAEquationVal,eqVarTbl[[idxA]]=0];"]];

smVarTbl=substMatrix/. rRuleTbl; 
DsmVarTbl=Table[D[smVarTbl[[ii]],t],{ii,1,NoSubstCnt}];

If[useDerivativeForAggregateSubstVal,
(
If[!SilentRunValue,Print["\[Rho]AllFunc::Using derivatives for aggregate constraints."]];
eqTbl=Table[DsmVarTbl[[ii]]==If[SubstanceTypeMatrix[ii]!= SubstanceTypeSum,(EqMatrix[ii] /. rRuleTbl),D[(EqMatrix[ii] /. rRuleTbl),t]],{ii,1,NoSubstCnt}] /. Flatten[coeffRuleTbl];
),
(
If[!SilentRunValue,Print["\[Rho]AllFunc::Using aggregate constraints as they are."]];
eqTbl=Table[If[SubstanceTypeMatrix[ii]!= SubstanceTypeSum,DsmVarTbl[[ii]],smVarTbl[[ii]]]==(EqMatrix[ii] /. rRuleTbl),{ii,1,NoSubstCnt}] /. Flatten[coeffRuleTbl];
)
];

If[NoSumSubstCnt > 0,
(
If[useDerivativeForAggregateSubstVal,
(
If[!SilentRunValue,Print["\[Rho]AllFunc::Updating initial values for aggregate substances."]];
Do[
(
eqInitTbl[[SumSubstanceMatrix[ii]]]=SubstanceMatrix[SumSubstanceMatrix[ii]][0]==((EqMatrix[SumSubstanceMatrix[ii]] /. rRuleTbl) /. {t -> 0});
),
{ii,1,NoSumSubstCnt}
];
),
(
If[!SilentRunValue,Print["\[Rho]AllFunc::Deleting initial values for aggregate substances."]];
deleteIdLst=Table[SumSubstanceMatrix[ii],{ii,1,NoSumSubstCnt}];
eqInitTbl=Delete[eqInitTbl,deleteIdLst];

If[useFuncForAggregateSubstVal,
(
If[!SilentRunValue,Print["\[Rho]AllFunc::Using function for each aggregate substance."]];

(* New rules *)
newRuleTbl=Table[SubstanceMatrix[ii]-> SubstanceMatrix[ii][t],{ii,1,NoSubstCnt}];

(* Variables to be used in functions *)
argTbl=Delete[substMatrix,deleteIdLst];

(* Variables to be used as arguments in call to function *)
argCallTbl=Delete[smVarTbl,deleteIdLst];

argLen=Length[argTbl];
varName="$$$var";

(* Rule to replace substances in function definition *)
defRule=Table[argTbl[[ii]]-> ToString[argTbl[[ii]]] <> varName ,{ii,1,argLen}];

(* String of all arguments for declaration *)
argStr="";

(* String of all arguments for call *)
argCallStr="";

If[useNumericFuncForAggregateSubstVal,
(
If[!SilentRunValue,Print["\[Rho]AllFunc::Using numerical functions for aggregate variables."]];
),
(
If[!SilentRunValue,Print["\[Rho]AllFunc::Using regular functions for aggregate variables (will be substituted by definitions)."]];
)
];

Do[
(
If[useNumericFuncForAggregateSubstVal,
(
argStr=argStr <> ToString[argTbl[[ii]]] <> varName <> "_?NumericQ" <> If[ii<argLen,", ",""];
),
(
argStr=argStr <> ToString[argTbl[[ii]]] <> varName <> "_" <> If[ii<argLen,", ",""];
)
];

argCallStr=argCallStr <> ToString[argCallTbl[[ii]]] <> If[ii<argLen,", ",""];
),
{ii,1,argLen}
];

If[print\[Rho]AllFuncInfoVal,
(
Print["\[Rho]AllFunc::argTbl = ", argTbl];
Print["\[Rho]AllFunc::argCallTbl = ", argCallTbl];
Print["\[Rho]AllFunc::defRule = ", defRule];
Print["\[Rho]AllFunc::argStr = ", argStr];
Print["\[Rho]AllFunc::argCallStr = ", argCallStr];
)
];

Do[
(
funcTstrName=ToString[SubstanceMatrix[SumSubstanceMatrix[ii]]] ;
funcNameStr=funcTstrName <> "function";
funcValStr=ToString[((EqMatrix[SumSubstanceMatrix[ii]] /. defRule)/. Flatten[coeffRuleTbl])];
funcDeclStr=funcNameStr <> "[" <> argStr <> "]:=" <> funcValStr;
funcCallStr=funcNameStr <> "[" <> argCallStr <> "]";

(* funcTstr=funcTstrName <> "[" <> ToString[t] <> "_?NumericQ]:=" <> funcCallStr; *)
funcTstr=funcTstrName <> "[" <> ToString[t] <> "_]:=" <> funcCallStr;

ToExpression[funcDeclStr];
ToExpression[funcTstr];

newRuleTbl[[SumSubstanceMatrix[ii]]]=(SubstanceMatrix[SumSubstanceMatrix[ii]]-> ToExpression[funcCallStr]);

If[print\[Rho]AllFuncInfoVal,
(
Print["\[Rho]AllFunc::funcDeclStr = ", funcDeclStr];
Print["\[Rho]AllFunc::funcCallStr = ", funcCallStr];
Print["\[Rho]AllFunc::funcTstr = ", funcTstr];

Print["\[Rho]AllFunc::Definition[",funcNameStr,"] = ", ToExpression["Definition[" <> funcNameStr <> "]"]];
Print["\[Rho]AllFunc::Definition[",funcTstrName ,"] = ", ToExpression["Definition[" <> funcTstrName  <> "]"]];
)
];
),
{ii,1,NoSumSubstCnt}
];

rRuleTbl=newRuleTbl;

If[!SilentRunValue,Print["\[Rho]AllFunc::Reassigning eqTbl due to update in rules."]];
eqTbl=Table[If[SubstanceTypeMatrix[ii]!= SubstanceTypeSum,DsmVarTbl[[ii]],smVarTbl[[ii]]]==(EqMatrix[ii] /. rRuleTbl),{ii,1,NoSubstCnt}] /. Flatten[coeffRuleTbl];

eqTbl=Delete[eqTbl,deleteIdLst];

If[!SilentRunValue,Print["\[Rho]AllFunc::Reassigning substMatrix."]];
substMatrix=Delete[substMatrix,deleteIdLst];

If[print\[Rho]AllFuncInfoVal,
(
Print["\[Rho]AllFunc::newRuleTbl = ", newRuleTbl];
)
];
)
];
)
];
)
];

If[useWhenEventVal,
(
If[!SilentRunValue,Print["\[Rho]AllFunc::Using WhenEvent to constraint all concentrations as nonnegative numbers."]];
(* eqWE=Table[Apply[WhenEvent,{smVarTbl[[ii]]<0,smVarTbl[[ii]]\[Rule] 0}],{ii,1,NoSubstCnt}]; *)
(* eqWE=Table[WhenEvent[Evaluate[smVarTbl[[ii]]]<0,Evaluate[smVarTbl[[ii]]]\[Rule] 0],{ii,1,NoSubstCnt}]; *)

eqWE=Table[ToExpression["WhenEvent["<>ToString[smVarTbl[[ii]]]<>" < 0, "<> ToString[smVarTbl[[ii]]]<>" -> -"<>ToString[smVarTbl[[ii]]]<>"]"],{ii,1,NoSubstCnt}];
If[!SilentRunValue,Print["\[Rho]AllFunc::eqWE = ", Transpose[{eqWE}] // MatrixForm]];
eqAllTbl=Flatten[{eqTbl,eqInitTbl,eqWE}];
),
(
eqAllTbl=Flatten[{eqTbl,eqInitTbl}];
)
];

If[useHighPrecisionVal,(
If[!SilentRunValue,Print["\[Rho]AllFunc::Setting precision to ",clmWorkingPrecisionVal,"."]];
 eqAllTbl=SetPrecision[eqAllTbl,clmWorkingPrecisionVal];
)
];

If[doNotUseInterpolationFunctionVal,
(
If[!SilentRunValue,Print["\[Rho]AllFunc::Not using InterpolationFunction."]];
ndSolveLst={eqAllTbl,substMatrix,{t,tMaxVal,tMaxVal}};
),
(
ndSolveLst={eqAllTbl,substMatrix,{t,0,tMaxVal}};
)
];

If[useHighPrecisionVal,
(
ndSolveLst=Join[ndSolveLst,{MaxSteps  -> Infinity}];
),
(
If[(NumericQ[nMaxSteps]) || (nMaxSteps == Infinity),ndSolveLst=Join[ndSolveLst,{MaxSteps  -> nMaxSteps}]];
)
];

If[NumericQ[startingStepSizeVal],ndSolveLst=Join[ndSolveLst,{StartingStepSize  -> startingStepSizeVal}]];
If[NumericQ[accuracyGoalVal],ndSolveLst=Join[ndSolveLst,{AccuracyGoal  -> accuracyGoalVal}]];
If[NumericQ[precisionGoalVal],ndSolveLst=Join[ndSolveLst,{PrecisionGoal  -> precisionGoalVal}]];
If[NumericQ[workingPrecisionVal],ndSolveLst=Join[ndSolveLst,{WorkingPrecision  -> workingPrecisionVal}]];

If[monitorTypeVal== MonitorTypeTime,
(
If[!SilentRunValue,Print["\[Rho]AllFunc::Using Time StepMonitor."]];
monitorIndex=0;
useMonitor=True;
showProgress=True;

If[useShowProgressVal && (!doNotUseInterpolationFunctionVal),
(
If[!SilentRunValue,Print["\[Rho]AllFunc::Using show progress..."]];
Clear[ShowProgressFunction];

ShowProgressFunction[]:=Module[{},
showProgress=True;
];

Print[Button["Show progress",ShowProgressFunction[]]];
)
];

If[!SilentRunValue,Print["\[Rho]AllFunc::TODO::nuVal, nuPval, and nuCval are hardcoded. Move them into external function and pass its name via options."]];

monitor:=
(
monitorIndex++;

If[showProgress,
(
showProgress=False;
timeNowValue=AbsoluteTime[];
varVals=Table[SubstanceMatrix[ii][t],{ii,1,NoSubstCnt}];
mem=N[Round[(MemoryInUse[]/2^30)*1000]]/1000;
memMax=N[Round[(MaxMemoryUsed[]/2^30)*1000]]/1000;

(*
roLtot=TotalRoL[varVals];
roDtot=TotalRoD[varVals];
nuTotVal=nuValue[roLtot,roDtot];
*)

(*
rLVal=TotalRoLChain[varVals];
rRVal=TotalRoDChain[varVals];
nuVal=nuValue[rLVal,rRVal];

roPLval=TotalRoLPair[varVals];
roPDval=TotalRoDPair[varVals];
nuPval=nuValue[roPLval,roPDval];

roCLval=TotalRoLCryst[varVals];
roCDval=TotalRoDCryst[varVals];
nuCval=nuValue[roCLval,roCDval];
*)

rLValTbl=Table[TotalRoLChainLevel[varVals,ii],{ii,1,MaxChainLength}];
rRValTbl=Table[TotalRoDChainLevel[varVals,ii],{ii,1,MaxChainLength}];
nuValTbl=Table[nuValue[rLValTbl[[ii]],rRValTbl[[ii]]],{ii,1,MaxChainLength}];

roPLvalTbl=Table[TotalRoLPairLevel[varVals,ii],{ii,1,MaxChainLength}];
roPDvalTbl=Table[TotalRoDPairLevel[varVals,ii],{ii,1,MaxChainLength}];
nuPvalTbl=Table[nuValue[roPLvalTbl[[ii]],roPDvalTbl[[ii]]],{ii,1,MaxChainLength}];

roCLvalTbl=Table[TotalRoLCrystLevel[varVals,ii],{ii,1,MaxChainLength}];
roCDvalTbl=Table[TotalRoDCrystLevel[varVals,ii],{ii,1,MaxChainLength}];
nuCvalTbl=Table[nuValue[roCLvalTbl[[ii]],roCDvalTbl[[ii]]],{ii,1,MaxChainLength}];

(*
Print["step = ",monitorIndex, ", t = ", t, ", time = ",Round[(timeNowValue-timeStartValue)], ", nu = ", nuVal, ", nuP = ", nuPval, ", nuC = ", nuCval, ", mem: ", mem , " / ", memMax];
*)

Print["step = ",monitorIndex, ", t = ", t, ", time = ",Round[(timeNowValue-timeStartValue)], ", nu = ", nuValTbl // MatrixForm, ", nuP = ", nuPvalTbl // MatrixForm, ", nuC = ", nuCvalTbl // MatrixForm, ", mem: ", mem , " / ", memMax];

)
];

If[Mod[monitorIndex,monitorPrintFrequencyVal]==0,
(
timeNowValue=AbsoluteTime[];
mem=N[Round[(MemoryInUse[]/2^30)*1000]]/1000;
memMax=N[Round[(MaxMemoryUsed[]/2^30)*1000]]/1000;
Print["\[Rho]AllFunc::monitorIndex = ",monitorIndex, ", t = ", t, ", total run time = ",Round[(timeNowValue-timeStartValue)], ", memory: ", mem , " / ", memMax];
timePrevValue=timeNowValue;
)
];
);

ndSolveLst=Join[ndSolveLst,{StepMonitor :> monitor}];
)
];

If[monitorTypeVal== MonitorTypeData,
(
If[ToString[stepMonitorFunctionVal]!=  ToString[None],
(
If[!SilentRunValue,Print["\[Rho]AllFunc::Using Data StepMonitor."]];
monitorIndex=0;
useMonitor=True;
StepMonitorArrayCounter=0;
stepMonitorZeroVal=stepMonitorFunctionVal[0,0,initValues,rawOptions];
StepMonitorArray={stepMonitorZeroVal};
If[!SilentRunValue,Print["\[Rho]AllFunc::StepMonitorArray = ", StepMonitorArray]];

monitor:=
(
timeIndex=NStorageValue*(t/tMaxVal);

If[timeIndex>monitorIndex,
(
monitorIndex=Ceiling[timeIndex];

If[monitorIndex > 0,
(
varVals=Table[SubstanceMatrix[ii][t],{ii,1,NoSubstCnt}];
StepMonitorArray=Join[StepMonitorArray,{stepMonitorFunctionVal[monitorIndex,t,varVals,rawOptions]}];
StepMonitorArrayCounter++;
)
];
)
];
);

ndSolveLst=Join[ndSolveLst,{StepMonitor :> monitor}];

If[ToString[dynamicStepMonitorVal]!=  ToString[None],
(
If[!SilentRunValue,Print["\[Rho]AllFunc::Using dynamic StepMonitor: ", ToString[dynamicStepMonitorVal]]];
If[Length[dynamicStepMonitorVal]==0,
(
Print[DynamicModule[{},Dynamic[dynamicStepMonitorVal[StepMonitorArrayCounter]]]];
),
(
If[Length[dynamicStepMonitorVal]==3,
(
dynamicStepMonitorTimeVal=dynamicStepMonitorVal[[1]];
dynamicStepMonitorDataVal=dynamicStepMonitorVal[[2]];
dynamicStepMonitorPlotVal=dynamicStepMonitorVal[[3]];

If[ToString[dynamicStepMonitorTimeVal]!=  ToString[None],Print[DynamicModule[{},Dynamic[dynamicStepMonitorTimeVal[StepMonitorArrayCounter]],SaveDefinitions->True]]];
If[ToString[dynamicStepMonitorDataVal]!=  ToString[None],Print[DynamicModule[{},Dynamic[dynamicStepMonitorDataVal[StepMonitorArrayCounter]],SaveDefinitions->True]]];
If[ToString[dynamicStepMonitorPlotVal]!=  ToString[None],Print[DynamicModule[{},Dynamic[dynamicStepMonitorPlotVal[StepMonitorArrayCounter]],SaveDefinitions->True]]];
),
(
Print["\[Rho]AllFunc::Invalid dynamic StepMonitor."];
)
];
)
];

If[ToString[quitMinotorVal]!=  ToString[None],
(
QuitFunction[]:=Module[{},
If[BooleanQ[QuitRequested],
(
If[!QuitRequested,
(
QuitRequested=True;
FinishDynamic[];
quitMinotorVal[];
Quit[];
)
];
)
];
];
),
(
QuitFunction[]:=Module[{},
If[BooleanQ[QuitRequested],
(
If[!QuitRequested,
(
QuitRequested=True;
FinishDynamic[];
Quit[];
)
];
)
];
];
)
];
)
];
)
];

If[(ToString[evaluationMonitorFunctionVal]!=  ToString[None]) || (useOneMonitorVal && (ToString[stepMonitorFunctionVal]!=  ToString[None])),
(
If[!SilentRunValue,Print["\[Rho]AllFunc::Using EvaluationMonitor."]];
useMonitor=True;

If[!useOneMonitorVal,
(
If[!SilentRunValue,Print["\[Rho]AllFunc::Using separate EvaluationMonitor."]];
monitorEvalIndex=0;
EvaluationArrayCounter=0;
evalMonitorZeroVal=evaluationMonitorFunctionVal[0,0,initValues,rawOptions];
EvaluationMonitorArray={evalMonitorZeroVal};
If[!SilentRunValue,Print["\[Rho]AllFunc::evalMonitorZeroVal = ", EvaluationMonitorArray]];

monitorEval:=
(
timeIndexEval=NStorageValue*(t/tMaxVal);

If[timeIndexEval>monitorEvalIndex,
(
monitorEvalIndex=Ceiling[timeIndexEval];

If[monitorEvalIndex > 0,
(
varValsEval=Table[SubstanceMatrix[ii][t],{ii,1,NoSubstCnt}];
EvaluationMonitorArray=Join[EvaluationMonitorArray,{evaluationMonitorFunctionVal[monitorEvalIndex,t,varValsEval,rawOptions]}];
EvaluationArrayCounter++;
)
];
)
];
);
),
(
If[!SilentRunValue,Print["\[Rho]AllFunc::Using the same EvaluationMonitor and StepMonitor."]];
monitorEval:=monitor;
)
];

ndSolveLst=Join[ndSolveLst,{EvaluationMonitor :> monitorEval}];
)
];
)
];

If[!useMonitor,
(
If[!SilentRunValue,Print["\[Rho]AllFunc::NOT using any monitors."]];
)
];

If[Length[methodVal]  > 0,
(
ndSolveLst=Join[ndSolveLst,{Method->methodVal}];
)
];

If[print\[Rho]AllFuncInfoVal,
(
Print["\[Rho]AllFunc::rawOptions = ",rawOptions];

(*
Print["\[Rho]AllFunc::coeffValues = ", Chop[N[coeffValues]]];
Print["\[Rho]AllFunc::initValues = ",  Chop[N[initValues]]];
*)

(* Print["\[Rho]AllFunc::coeffArrayName = ", coeffArrayName // MatrixForm]; *)
Print["\[Rho]AllFunc::coeffRuleTbl = ", N[coeffRuleTbl] // MatrixForm];
Print["\[Rho]AllFunc::eqInitTbl = ", N[eqInitTbl] // MatrixForm];
(*
Print["\[Rho]AllFunc::rRuleTbl = ", rRuleTbl // MatrixForm];
Print["\[Rho]AllFunc::eqVarTbl = ", eqVarTbl // MatrixForm];
Print["\[Rho]AllFunc::smVarTbl = ", smVarTbl // MatrixForm];
Print["\[Rho]AllFunc::DsmVarTbl = ", DsmVarTbl // MatrixForm];
*)
Print["\[Rho]AllFunc::eqTbl = ", N[eqTbl] // MatrixForm];
Print["\[Rho]AllFunc::eqAllTbl = ", N[eqAllTbl] // MatrixForm];

Print["\[Rho]AllFunc::nMaxSteps = ",nMaxSteps];
Print["\[Rho]AllFunc::methodVal = ",methodVal];
Print["\[Rho]AllFunc::startingStepSizeVal = ",startingStepSizeVal];
Print["\[Rho]AllFunc::ndSolveLst = ",N[ndSolveLst]];
Print["\[Rho]AllFunc::ndSolveLst = ",InputForm[N[ndSolveLst]]];
Print["\[Rho]AllFunc::methodVal = ",methodVal];
)
];

(* nMaxSteps=Infinity; *)
(* Method\[Rule]"ExplicitRungeKutta",Method\[Rule]"ImplicitRungeKutta" *)
(* Method\[Rule]{"ExplicitRungeKutta","DifferenceOrder"\[Rule]5,"Coefficients"\[Rule]DOPRICoefficients,"StiffnessTest"\[Rule]False} *)
(* methodVal={"ExplicitRungeKutta","DifferenceOrder"\[Rule]5,"StiffnessTest"\[Rule]False}; *)
(* methodVal={"FixedStep",Method\[Rule]"ExplicitRungeKutta"}; *)
(* StartingStepSize\[Rule]1/10,Method\[Rule]{"FixedStep",Method\[Rule]"ExplicitRungeKutta"}]; StepDataPlot[sol,PlotRange\[Rule]{0,0.2}] *)

(*
nAccuracyGoal=20;
(* Print["\[Rho]AllFunc::Calling NDSolve."]; *)
sol=If[useHighPrecisionVal\[Equal] True,
NDSolve[eqAllTbl,SubstanceMatrix,{t,0,tMaxVal},AccuracyGoal\[Rule] 20,PrecisionGoal\[Rule] 20,WorkingPrecision\[Rule] 35,MaxSteps\[Rule] Infinity,StartingStepSize\[Rule]startingStepSizeVal,Method\[Rule]methodVal],
NDSolve[eqAllTbl,SubstanceMatrix,{t,0,tMaxVal},MaxSteps  \[Rule] nMaxSteps, AccuracyGoal \[Rule] nAccuracyGoal,StartingStepSize\[Rule]startingStepSizeVal,Method\[Rule]methodVal]
];
*)

If[!SilentRunValue,
(
PrintTimeUsed[];
Print["\[Rho]AllFunc::About to call NDSolve. FullLength[ndSolveLst] = ",Length[Flatten[ndSolveLst]]];
PrintTimeUsed[];
)
];

If[ToString[quitMinotorVal]!=  ToString[None], Print[Button["Quit",QuitFunction[]]]];

sol=Apply[NDSolve,ndSolveLst];
QuitRequested=True;

If[!SilentRunValue,
(
Print["\[Rho]AllFunc::Call to NDSolve completed."];
PrintTimeUsed[];
)
];

If[!SilentRunValue,If[roAllFuncDescriptionVal!="",Print["\[Rho]AllFunc::Description: ", roAllFuncDescriptionVal]]];

(*
Print["\[Rho]AllFunc::Calling StepDataPlot."];
Print[Show[StepDataPlot[sol]]];
Print["Definition[StepDataPlot] = ",Definition[StepDataPlot]];
Print["\[Rho]AllFunc::StepDataPlot completed."];
*)

rotMaxVal=Table[(SubstanceMatrix[ii][t] /.sol[[1]]) /. {t -> tMaxVal},{ii,1,NoSubstCnt}];
funcVal=Table[\[Eta]Func[ii,rotMaxVal] ,{ii,1,MaxGenerationLevel}];
retVal={rotMaxVal,funcVal,If[OutputSolution==True,sol,Indeterminate]};

(* Print["\[Rho]AllFunc::retVal = ", retVal]; *)

Return[retVal];
];

(* ============================================== *)

f[CalcGridArrVar:{{{_,___},___},{{_,___},___}},idxFunc_:NumericQ,solIdxVar_:NumericQ,xVarRange:{_,_,_,_},yVarRange:{_,_,_,_}]:=Module[{varN,clc,outptL,outptR,retVal,iii,jjj,jjj0,outptZeroY,outptHlp,xVar,xStart,xEnd,yVar,yStart,yEnd,pltPointsXvar,pltPointsYvar,\[Rho]AllValuesL,\[Rho]AllFuncValuesL,\[Rho]AllValuesR,\[Rho]AllFuncValuesR,CalcGridArrL,CalcGridArrR,output,\[Rho]AllValues,\[Rho]AllFuncValues},

If[solIdxVar<1 || solIdxVar > 2,(Print["f::Invalid solIdxVar = ", solIdxVar]; Return[Indeterminate])];

xVar=xVarRange[[1]];
xStart=xVarRange[[2]];
xEnd=xVarRange[[3]];
pltPointsXvar=xVarRange[[4]];

yVar=yVarRange[[1]];
yStart=yVarRange[[2]];
yEnd=yVarRange[[3]];
pltPointsYvar=yVarRange[[4]];

iii=Max[Min[1+Round[(pltPointsXvar-1)*(xVar-xStart)/(xEnd-xStart)],pltPointsXvar],1];
jjj=Max[Min[1+Round[(pltPointsYvar-1)*(yVar-yStart)/(yEnd-yStart)],pltPointsYvar],1];
(* jjj0=Max[Min[1+Round[(pltPointsYvar-1)*(-yStart)/(yEnd-yStart)],pltPointsYvar],1]; *)


output=CalcGridArrVar[[solIdxVar]][[iii,jjj]];
\[Rho]AllValues=\[Rho]AllGetValues[output];
\[Rho]AllFuncValues=\[Rho]AllGetFuncValues[output];

(*
Print["f::output = ", output];
Print["f::\[Rho]AllValues = ", \[Rho]AllValues];
Print["f::\[Rho]AllFuncValues = ", \[Rho]AllFuncValues];
*)

retVal=\[Rho]AllFuncValues[[idxFunc]];

(*
outptL=CalcGridArrVar[[1]][[iii,jjj]];
outptR=CalcGridArrVar[[2]][[iii,jjj]];

\[Rho]AllValuesL=\[Rho]AllGetValues[outptL];
\[Rho]AllFuncValuesL=\[Rho]AllGetFuncValues[outptL];

\[Rho]AllValuesR=\[Rho]AllGetValues[outptR];
\[Rho]AllFuncValuesR=\[Rho]AllGetFuncValues[outptR];

retVal={\[Rho]AllFuncValuesL[[1]],\[Rho]AllFuncValuesR[[1]]};
*)

(* Print["f::retVal = ", retVal]; *)
Return[retVal];
];
SetAttributes[f,NumericFunction];

(* ============================================== *)

GetTables[CalcGridArrVar:{{{_,___},___},{{_,___},___}},xRange:{_,_,_},yRange:{_,_,_}]:=Module[{retVal,xStart,xEnd,yStart,yEnd,pltPointsX,pltPointsY ,xVar,yVar,xIdx,yIdx,idx,\[Eta]Tbl,solIdx,\[Eta]ValTbl,\[Eta]BTbl,\[Eta]BValTbl},

xStart=xRange[[1]];
xEnd=xRange[[2]];
pltPointsX=xRange[[3]];

yStart=yRange[[1]];
yEnd=yRange[[2]];
pltPointsY=yRange[[3]];

\[Eta]Tbl=Table[0,{solIdx,1,2}];
\[Eta]BTbl=Table[0,{solIdx,1,2}];

(* (xRange[[1]]+(xRange[[2]]-xRange[[1]])*(xIdx-1)/(xRange[[3]]-1)) *)
(* (yRange[[1]]+(yRange[[2]]-yRange[[1]])*(yIdx-1)/(yRange[[3]]-1)) *)

(* DistributeDefinitions[CalcGridArrVar,f,solIdx,pltPointsX,pltPointsY,xRange,yRange]; *)

Do[
\[Eta]ValTbl=Flatten[Table[{xVar,yVar,f[CalcGridArrVar,1,solIdx,{xVar,xStart,xEnd,pltPointsX},{yVar,yStart,yEnd,pltPointsY}]},{xVar,xStart,xEnd,(xEnd-xStart)/(pltPointsX-1)},{yVar,yStart,yEnd,(yEnd-yStart)/(pltPointsY-1)}],1];


\[Eta]BValTbl=Flatten[Table[{xVar,yVar,f[CalcGridArrVar,2,solIdx,{xVar,xStart,xEnd,pltPointsX},{yVar,yStart,yEnd,pltPointsY}]},{xVar,xStart,xEnd,(xEnd-xStart)/(pltPointsX-1)},{yVar,yStart,yEnd,(yEnd-yStart)/(pltPointsY-1)}],1];

(* Print["GetTables::\[Eta]ValTbl", \[Eta]ValTbl]; *)

\[Eta]Tbl[[solIdx]]=Interpolation[\[Eta]ValTbl];
\[Eta]BTbl[[solIdx]]=Interpolation[\[Eta]BValTbl];
,{solIdx,1,2}
];

retVal={\[Eta]Tbl,\[Eta]BTbl};

Return[retVal];
];
SetAttributes[GetTables,NumericFunction];

(* ============================================== *)

GetModelType[modelVar:{__}]:=modelVar[[1]];
GetModelDescriptor[modelVar:{__}]:=modelVar[[2]];
GetModelName[modelVar:{__}]:=modelVar[[3]];
GetModelCoeffNewValues[modelVar:{__}]:=modelVar[[4]];
GetModelTMax[modelVar:{__}]:=modelVar[[5]];
GetModelRawOptions[modelVar:{__}]:=modelVar[[6]];

(* ============================================== *)

(* Creates all models by iterating through all generations. *)
CreateAllModels[coeffNewAllVal:{__},tMaxVal_?NumericQ,rawOptions___]:=Module[{modelDescriptor,len,ii,jj,modelDescriptorList,g00,kk},
ModelList={};
modelDescriptorList=Table[GetAllModelsDescriptorMatrix[jj],{jj,1,MaxGenerationLevel}];
(* Print["CreateAllModels::modelDescriptorList = ", modelDescriptorList]; *)

(* Ok. We need to sort of do a tensor multiplication. This seems to do the trick. *)
(* Print["CreateAllModels::g00"]; *)
g00=Table[{modelDescriptorList[[1]][[ii]]},{ii,1,Length[modelDescriptorList[[1]]]}];

For[kk=2, kk <= MaxGenerationLevel, kk++,
(
(* Print["CreateAllModels::kk = ", kk]; *)
len=Length[g00];
(* Print["CreateAllModels::len = ", len]; *)

g00=Flatten[Table[Join[g00[[ii]],{modelDescriptorList[[kk]][[jj]]}],{ii,1,len},{jj,1,Length[modelDescriptorList[[kk]]]}],1];
(*
Print["CreateAllModels::g00 = ", g00];
Print["CreateAllModels::g00[[1]] = ", g00[[1]]];
*)
)
];

len=Length[g00];
Print["CreateAllModels::Total number of models = ", len]; 

For[ii=1, ii <= len, ii++, 
(
modelDescriptor=g00[[ii]];
(* Print["CreateAllModels::modelDescriptor = ", modelDescriptor]; *)
CreateModel[modelDescriptor,coeffNewAllVal,tMaxVal,rawOptions];
)
];
Return[ModelList];
];

(* ============================================== *)

(* Creates a model based on a model descriptor and all coefficient values. *)
CreateModel[modelDescriptor:{__},coeffNewAllVal:{__},tMaxVal_?NumericQ,rawOptions___]:=Module[{modelName,model,coeffNewVal,idxAutoCatalysis,idxMutualAntagonism,idxPairMerging,idxCrystallization,kk,coeffNewVal1},
modelName=CreateModelName[modelDescriptor];

(* Print["CreateModel::modelName = ", modelName]; *)

coeffNewVal=CreateZeroCoefficientValues[];

(* Print["CreateModel::coeffNewVal (zero) = ", coeffNewVal]; *)

coeffNewVal[[coeffIdx\[Rho]0]]=coeffNewAllVal[[coeffIdx\[Rho]0]];
coeffNewVal[[coeffIdxRacemization]]=coeffNewAllVal[[coeffIdxRacemization]];
coeffNewVal[[coeffIdxGamma]]=coeffNewAllVal[[coeffIdxGamma]];
coeffNewVal[[coeffIdxGammaPlus]]=coeffNewAllVal[[coeffIdxGammaPlus]];
coeffNewVal[[coeffIdxGammaMinus]]=coeffNewAllVal[[coeffIdxGammaMinus]];
coeffNewVal[[coeffIdxAAStoXS]]=coeffNewAllVal[[coeffIdxAAStoXS]];
coeffNewVal[[coeffIdxXSto2AS]]=coeffNewAllVal[[coeffIdxXSto2AS]];

For[kk=1, kk <= MaxGenerationLevel, kk++,
(
coeffNewVal1=ApplyNewCoefficients[coeffNewVal,coeffNewAllVal,kk,modelDescriptor];

(* Print["CreateModel::coeffNewVal1 after kk = ", kk," : ", coeffNewVal1]; *)

coeffNewVal=coeffNewVal1;
)
];

(* Print["CreateModel::coeffNewVal = ", coeffNewVal]; *)
AddModel[NewCoeffModelType,modelDescriptor,modelName,coeffNewVal,tMaxVal,rawOptions];
];

(* ============================================== *)

CreateModelName[modelDescriptor:{__}]:=Module[{modelName,ii,modelLevelDescriptor},
modelName="";

For[ii=1, ii <= MaxGenerationLevel, ii++,
(
modelLevelDescriptor=GetModelLevelDescriptor[ii,modelDescriptor];
modelName=modelName <> strCRLF <> "Generation: " <> ToString[ii] <> ", Description: " <> GetGenerationName[ii,modelLevelDescriptor];
If[ii != MaxGenerationLevel,modelName=modelName <> ", "];
)
];

modelName=modelName<>strCRLF;

Return[modelName];
];

(* ============================================== *)

AddModel[modelType_?NumericQ,modelDescriptor:{__},modelName_,coeffNewValues:{__},tMaxVal_?NumericQ,rawOptions___]:=Module[{modelVal},
modelVal={{modelType,modelDescriptor,modelName,coeffNewValues,tMaxVal,rawOptions}};
ModelList=If[Length[ModelList] ==0,modelVal,Join[ModelList,modelVal]];
];

(* ============================================== *)

RunModels[modelListVar:{__}]:=RunModels[modelListVar,1,0];
RunModels[modelListVar:{__},startModelNumber_?IntegerQ]:=RunModels[modelListVar,startModelNumber,0];

RunModels[modelListVar:{__},startModelNumber_?IntegerQ,endModelNumber_?IntegerQ]:=Module[{len,firstModel,lastModel,modelRunListVar,ii},
len=Length[modelListVar];

lastModel=endModelNumber;
If[lastModel <startModelNumber,lastModel=len];

firstModel=startModelNumber;
If[firstModel < 1, firstModel=1];
If[firstModel > lastModel, firstModel=lastModel];

modelRunListVar=Table[ii,{ii,firstModel,lastModel}];

RunModels[modelListVar,modelRunListVar];
];

RunModels[modelListVar:{__},modelRunListVar:{___}]:=Module[{len,ii,jj, modelName,modelVal,rawOptions,coeffNewValues,modelType,tMaxVal, modelCnt,outputAllModelNamesVal,opts},
len=Length[modelListVar];

Print[strSeparatorCRLF,strSeparatorCRLF,"Running models..."];
Print[strSeparator];
(* TODO Implement usage of IsModelLevelDescriptorValid[generationLevel_?IntegerQ,modelLevelDescriptor:{__}] *)
(* TODO: Implement UsePrerequisites and utilize modelCnt *)
modelCnt=0;

For[ii=1, ii <= len,ii++,
(
modelVal=modelListVar[[ii]];

modelName=GetModelName[modelVal];
coeffNewValues=GetModelCoeffNewValues[modelVal];
rawOptions=GetModelRawOptions[modelVal];
opts=ProcessOptions[rawOptions];
outputAllModelNamesVal=OutputAllModelNames /. opts /. Options[CLMS];

If[outputAllModelNamesVal==True,
(
Print["RunModels::Model No: ",ii, ", Name: ", modelName];
OutputParams[Table[{coeffNewValues[[jj]]},{jj,1,NoCoeffCnt}],rawOptions];
Print[strSeparator];
)
];
)
];

Print[strSeparatorCRLF];

If[Length[modelRunListVar] ==0,(Print["Model list requested. Models will not be run."]; Return[];)];

For[ii=1, ii <= len,ii++,
If[MemberQ[modelRunListVar,ii]==True,
(
modelVal=modelListVar[[ii]];
Print["RunModels::modelVar = ",modelVal];

modelType=GetModelType[modelVal];
modelName=GetModelName[modelVal];
coeffNewValues=GetModelCoeffNewValues[modelVal];
rawOptions=GetModelRawOptions[modelVal];
tMaxVal=GetModelTMax[modelVal];

If[modelType== NewCoeffModelType,
(
RunNewCoeffModel[modelVal,ii,modelName,coeffNewValues,tMaxVal,rawOptions]
),
(
Print["RunModels::Unknown model type."]
)
];
)
];
];
];

(* ============================================== *)

GetCoeffName[idx_?NumericQ,UseCoeffTransform_?BooleanQ]:=Module[{retVal},
retVal=If[UseCoeffTransform== True,coeffNewArrayDisplayName[idx],coeffArrayDisplayName[idx]];
Return[retVal];
];

(* ============================================== *)
If[!SilentRunValue,Print["CLM_Calc::TransformCoeffValues and GetCoeffName::TODO::Fix to supoprt indexers..."]];
TransformCoeffValues[coeffNewValues:{__}]:=Module[{retVal,ii,coeffRuleArrayLen,coeffArray,coeffLen,coeffRuleTbl,coeffRule},
(* Print["TransformCoeffValues::Starting"]; *)

coeffLen=Length[coeffNewValues];
If[coeffLen!= NoCoeffCnt,(Print["TransformCoeffValues::Invalid coeffNewValues."]; Return[Indeterminate];)];

coeffRuleArrayLen=Length[coeffRuleArray];
(* Print["TransformCoeffValues::coeffRuleArrayLen = ", coeffRuleArrayLen]; *)

coeffArray=coeffArrayName;

For[ii=1,ii <= coeffRuleArrayLen,ii++,
(
coeffRule=GetCoeffRule[ii];
(* Print["TransformCoeffValues::coeffRule = ", coeffRule]; *)
coeffArray=(coeffArray /.coeffRule)
)
];

coeffRuleTbl=Table[coeffNewArrayName[[ii]] -> Flatten[{coeffNewValues[[ii]]}][[1]],{ii,1,NoCoeffCnt}];
(*
Print["TransformCoeffValues::coeffRuleTbl = ", coeffRuleTbl // MatrixForm];
Print["TransformCoeffValues::coeffArray = ", coeffArray // MatrixForm];
*)
retVal=coeffArray /. coeffRuleTbl;

(* Print["TransformCoeffValues::retVal = ", retVal // MatrixForm]; *)
Return[retVal];
];

(* ============================================== *)

\[Rho]TotalValueFunc[solution:{__},idxAtomVal_?NumericQ,tVal_?NumericQ]:=Module[{retVal,roAllVal,ii},
roAllVal=Table[\[Rho]AllValueFunc[solution,ii,tVal],{ii,1,NoSubstCnt}];
retVal=GetTotalNoOfAtoms[idxAtomVal,roAllVal];
Return[retVal];
];

(* ============================================== *)
If[!SilentRunValue,Print["CLM_Calc::RunNewCoeffModel::TODO::Update for indexers..."]];
RunNewCoeffModel[modelVar_,modelNumber_?IntegerQ,modelName_,coeffNewValues:{__},tMaxVal_?NumericQ,rawOptions___]:=Module[{paramNewValues,ii,initVal,coeffVal,sol,nMaxRecursion,BDIMAGESIZE,BDPLTTEXTOPTS,xNameLbl,yNameLbl,plotOptsT,tMaxDiv,tMaxDivFast,useHighPrecision,tt, plotFastVal,opts,silentWeakModelsVal,nuWeakValue,useVariableRacemizationForAllVal,outputAllModelNamesVal,print\[Rho]AllFuncInfoTriggerVal,coeffNewValuesHlp,modelDescriptor,optsWithModelDescriptor,useZSubstanceForRo0Val,doNotOutput3Dval,tPlot,interpFunc,f},
(* Print[strSeparatorCRLF,strSeparatorCRLF,"Running model: ", modelName]; *)

modelDescriptor=GetModelDescriptor[modelVar];
opts=ProcessOptions[rawOptions];
optsWithModelDescriptor=Join[{ModelDescriptorValue -> modelDescriptor},opts];
plotFastVal=PlotFast /. opts /. Options[CLMS];
silentWeakModelsVal=SilentWeakModels /. opts /. Options[CLMS];
useVariableRacemizationForAllVal=UseVariableRacemizationForAll /. opts /. Options[CLMS];
outputAllModelNamesVal=OutputAllModelNames /. opts /. Options[CLMS];
print\[Rho]AllFuncInfoTriggerVal =Print\[Rho]AllFuncInfoTrigger /. opts /. Options[CLMS];
useZSubstanceForRo0Val = UseZSubstanceForRo0 /.opts /. Options[CLMS];
doNotOutput3Dval=DoNotOutput3D /.opts /. Options[CLMS];

initVal=CreateZeroInitValues[];
If[useZSubstanceForRo0Val==True,
initVal[[idxZ]]=Max[Flatten[{coeffNewValues[[coeffIdx\[Rho]0]]}]],
initVal[[idxA]]=Max[Flatten[{coeffNewValues[[coeffIdx\[Rho]0]]}]],
initVal[[idxA]]=Max[Flatten[{coeffNewValues[[coeffIdx\[Rho]0]]}]]
];

coeffNewValuesHlp=ApplyAllRacemization[coeffNewValues,coeffNewValues[[coeffIdxRacemization]],optsWithModelDescriptor];
coeffNewValuesHlp=ApplyAllGamma[coeffNewValuesHlp,{coeffNewValuesHlp[[coeffIdxGamma]],coeffNewValuesHlp[[coeffIdxGammaPlus]],coeffNewValuesHlp[[coeffIdxGammaMinus]]},optsWithModelDescriptor];
coeffVal=TransformCoeffValues[coeffNewValuesHlp];

Print["RunNewCoeffModel::initVal[[idxA]] = ",N[initVal[[idxA]]]];
Print["RunNewCoeffModel::coeffNewValues = ",N[coeffNewValues]];
Print["RunNewCoeffModel::coeffVal = ",N[coeffVal]];

tMaxDiv=10;
tMaxDivFast=100;
useHighPrecision=False;

sol=If[print\[Rho]AllFuncInfoTriggerVal == True,\[Rho]AllFunc[coeffVal,initVal,tMaxVal,True,Join[{ModelDescriptorValue -> modelDescriptor,Print\[Rho]AllFuncInfo -> True}, opts]],
\[Rho]AllFunc[coeffVal,initVal,tMaxVal,True,optsWithModelDescriptor]
];

interpFunc=\[Rho]AllGetInterpolationFunctions[sol];
f=SubstanceMatrix[1] /.interpFunc[[1]];

tPlot=f["Domain"][[1,2]];
Print["tPlot = ", tPlot];

nuWeakValue=IsNuWeakValueTestPassed[sol,tPlot,optsWithModelDescriptor];

If[(nuWeakValue== True) || ((nuWeakValue== False) && (silentWeakModelsVal == False)),
(
Print[""];
Print[strSeparator];
Print[strSeparator];
Print["Model No: ",modelNumber, ", Name: ", modelName];
OutputParams[Table[{coeffNewValues[[ii]]},{ii,1,NoCoeffCnt}],optsWithModelDescriptor];

If[nuWeakValue == False,
(Print["\[Eta] values are too small, ignoring model..."])
];
)
];

If[nuWeakValue == False,
Return[]
];

(* Print["RunNewCoeffModel::Plotting"]; *)
nMaxRecursion=0;
BDIMAGESIZE=500;
BDPLTTEXTOPTS={FontFamily->"Courier",FontSize->14,FontWeight->"Bold"};

xNameLbl=t;
yNameLbl=" ";

plotOptsT:={PlotRange ->All,ImageSize -> BDIMAGESIZE ,LabelStyle -> BDPLTTEXTOPTS,AxesLabel->{xNameLbl} ,Frame -> True, GridLines -> Automatic,PlotStyle -> {{Thick, Black},{Thick, Dashed,Black},{Thick, Dotted,Black}},PlotLegends -> Placed[yNameLbl,Below], Axes -> False ,Epilog -> Text[xNameLbl,Scaled[{0.95,0.1}], BaseStyle-> BDPLTTEXTOPTS] };

(*
TODO - show total here ...
Print[strSeparatorSmall];
yNameLbl=SubstanceDisplayMatrix[[idxA]];

If[plotFastVal\[Equal]False,
Print[Plot[{\[Rho]AllValueFunc[sol,idxA,tt]},{tt,0,tPlot},Evaluate[plotOptsT]]],
(
Print[Plot[{\[Rho]AllValueFunc[sol,idxA,tt]},{tt,0,tPlot},Evaluate[plotOptsT]],Plot[{\[Rho]AllValueFunc[sol,idxA,tt]},{tt,0,tPlot/tMaxDivFast},Evaluate[plotOptsT]]]
)
];

*)

Print[strSeparatorSmall];
yNameLbl=AtomDisplayMatrix[idxAtomZ];

If[plotFastVal==False,
Print[Plot[{\[Rho]TotalValueFunc[sol,idxAtomZ,tt]},{tt,0,tPlot},Evaluate[plotOptsT]]],
(
Print[Plot[{\[Rho]TotalValueFunc[sol,idxAtomZ,tt]},{tt,0,tPlot},Evaluate[plotOptsT]],Plot[{\[Rho]TotalValueFunc[sol,idxAtomZ,tt]},{tt,0,tPlot/tMaxDivFast},Evaluate[plotOptsT]]]
)
];

Print[strSeparatorSmall];
yNameLbl=SubstanceDisplayMatrix[idxA];

If[plotFastVal==False,
Print[Plot[{\[Rho]AllValueFunc[sol,idxA,tt]},{tt,0,tPlot},Evaluate[plotOptsT]]],
(
Print[Plot[{\[Rho]AllValueFunc[sol,idxA,tt]},{tt,0,tPlot},Evaluate[plotOptsT]],Plot[{\[Rho]AllValueFunc[sol,idxA,tt]},{tt,0,tPlot/tMaxDivFast},Evaluate[plotOptsT]]]
)
];

Print[strSeparatorSmall];
yNameLbl={SubstanceDisplayMatrix[idxL],SubstanceDisplayMatrix[idxR]};

If[plotFastVal==False,
Print[Plot[{\[Rho]AllValueFunc[sol,idxL,tt],\[Rho]AllValueFunc[sol,idxR,tt]},{tt,0,tPlot},Evaluate[plotOptsT]]],
(
Print[Plot[{\[Rho]AllValueFunc[sol,idxL,tt],\[Rho]AllValueFunc[sol,idxR,tt]},{tt,0,tPlot},Evaluate[plotOptsT]],Plot[{\[Rho]AllValueFunc[sol,idxL,tt],\[Rho]AllValueFunc[sol,idxR,tt]},{tt,0,tPlot/tMaxDivFast},Evaluate[plotOptsT]]]
)
];

Print[strSeparatorSmall];
yNameLbl={SubstanceDisplayMatrix[idxBll],SubstanceDisplayMatrix[idxBrr],SubstanceDisplayMatrix[idxBlr]};

If[plotFastVal==False,
Print[Plot[{\[Rho]AllValueFunc[sol,idxBll,tt],\[Rho]AllValueFunc[sol,idxBrr,tt],\[Rho]AllValueFunc[sol,idxBlr,tt]},{tt,0,tPlot},Evaluate[plotOptsT]]],
(
Print[Plot[{\[Rho]AllValueFunc[sol,idxBll,tt],\[Rho]AllValueFunc[sol,idxBrr,tt],\[Rho]AllValueFunc[sol,idxBlr,tt]},{tt,0,tPlot},Evaluate[plotOptsT]],Plot[{\[Rho]AllValueFunc[sol,idxBll,tt],\[Rho]AllValueFunc[sol,idxBrr,tt],\[Rho]AllValueFunc[sol,idxBlr,tt]},{tt,0,tPlot/tMaxDivFast},Evaluate[plotOptsT]]]
)
];

(* Just Blr *)
Print[strSeparatorSmall];
yNameLbl={SubstanceDisplayMatrix[idxBlr]};

If[plotFastVal==False,
Print[Plot[{\[Rho]AllValueFunc[sol,idxBlr,tt]},{tt,0,tPlot},Evaluate[plotOptsT]]],
(
Print[Plot[{\[Rho]AllValueFunc[sol,idxBlr,tt]},{tt,0,tPlot},Evaluate[plotOptsT]],Plot[{\[Rho]AllValueFunc[sol,idxBlr,tt]},{tt,0,tPlot/tMaxDivFast},Evaluate[plotOptsT]]]
)
];

Print[strSeparatorSmall];
yNameLbl={SubstanceDisplayMatrix[idxCll],SubstanceDisplayMatrix[idxCrr]};

If[plotFastVal==False,
Print[Plot[{\[Rho]AllValueFunc[sol,idxCll,tt],\[Rho]AllValueFunc[sol,idxCrr,tt]},{tt,0,tPlot},Evaluate[plotOptsT]]],
(
Print[Plot[{\[Rho]AllValueFunc[sol,idxCll,tt],\[Rho]AllValueFunc[sol,idxCrr,tt]},{tt,0,tPlot},Evaluate[plotOptsT]],Plot[{\[Rho]AllValueFunc[sol,idxCll,tt],\[Rho]AllValueFunc[sol,idxCrr,tt]},{tt,0,tPlot/tMaxDivFast},Evaluate[plotOptsT]]]
)
];

Print[strSeparatorSmall];
yNameLbl={SubstanceDisplayMatrix[idxClr]};

If[plotFastVal==False,
Print[Plot[{\[Rho]AllValueFunc[sol,idxClr,tt]},{tt,0,tPlot},Evaluate[plotOptsT]]],
(
Print[Plot[{\[Rho]AllValueFunc[sol,idxClr,tt]},{tt,0,tPlot},Evaluate[plotOptsT]],Plot[{\[Rho]AllValueFunc[sol,idxClr,tt]},{tt,0,tPlot/tMaxDivFast},Evaluate[plotOptsT]]]
)
];

Print[strSeparatorSmall];
yNameLbl={SubstanceDisplayMatrix[idxCll],SubstanceDisplayMatrix[idxCrr],SubstanceDisplayMatrix[idxClr]};

If[plotFastVal==False,
Print[Plot[{\[Rho]AllValueFunc[sol,idxCll,tt],\[Rho]AllValueFunc[sol,idxCrr,tt],\[Rho]AllValueFunc[sol,idxClr,tt]},{tt,0,tPlot},Evaluate[plotOptsT]]],
(
Print[Plot[{\[Rho]AllValueFunc[sol,idxCll,tt],\[Rho]AllValueFunc[sol,idxCrr,tt],\[Rho]AllValueFunc[sol,idxClr,tt]},{tt,0,tPlot},Evaluate[plotOptsT]],Plot[{\[Rho]AllValueFunc[sol,idxCll,tt],\[Rho]AllValueFunc[sol,idxCrr,tt],\[Rho]AllValueFunc[sol,idxClr,tt]},{tt,0,tPlot/tMaxDivFast},Evaluate[plotOptsT]]]
)
];

Print[strSeparatorSmall];

yNameLbl={SubstanceDisplayMatrix[idxCll]-SubstanceDisplayMatrix[idxCrr]};
If[plotFastVal==False,
Print[Plot[{\[Rho]AllValueFunc[sol,idxCll,tt]-\[Rho]AllValueFunc[sol,idxCrr,tt]},{tt,0,tPlot},Evaluate[plotOptsT]]],
(
Print[Plot[{\[Rho]AllValueFunc[sol,idxCll,tt]-\[Rho]AllValueFunc[sol,idxCrr,tt]},{tt,0,tPlot},Evaluate[plotOptsT]],Plot[{\[Rho]AllValueFunc[sol,idxCll,tt]-\[Rho]AllValueFunc[sol,idxCrr,tt]},{tt,0,tPlot/tMaxDivFast},Evaluate[plotOptsT]]]
)
];

Print[strSeparatorSmall];

(*
yNameLbl=\[Eta];
If[plotFastVal\[Equal]False,
Print[Plot[{\[Eta]ValueFunc[1,sol,tt]},{tt,0,tPlot},Evaluate[plotOptsT]]],
(
Print[Plot[{\[Eta]ValueFunc[1,sol,tt]},{tt,0,tPlot},Evaluate[plotOptsT]],Plot[{\[Eta]ValueFunc[1,sol,tt]},{tt,0,tPlot/tMaxDivFast},Evaluate[plotOptsT]]]
)
];

Print[strSeparatorSmall];

yNameLbl=Subscript[\[Eta],B];
If[plotFastVal\[Equal]False,
Print[Plot[{\[Eta]ValueFunc[2,sol,tt]},{tt,0,tPlot},Evaluate[plotOptsT]]],
(
Print[Plot[{\[Eta]ValueFunc[2,sol,tt]},{tt,0,tPlot},Evaluate[plotOptsT]],Plot[{\[Eta]ValueFunc[2,sol,tt]},{tt,0,tPlot/tMaxDivFast},Evaluate[plotOptsT]]]
)
];
*)

yNameLbl={\[Eta],Subscript[\[Eta],B]};
If[plotFastVal==False,
Print[Plot[{\[Eta]ValueFunc[1,sol,tt],\[Eta]ValueFunc[2,sol,tt]},{tt,0,tPlot},Evaluate[plotOptsT]]],
(
Print[Plot[{\[Eta]ValueFunc[1,sol,tt],\[Eta]ValueFunc[2,sol,tt]},{tt,0,tPlot},Evaluate[plotOptsT]],Plot[{\[Eta]ValueFunc[1,sol,tt],\[Eta]ValueFunc[2,sol,tt]},{tt,0,tPlot/tMaxDivFast},Evaluate[plotOptsT]]]
)
];

Print[strSeparatorCRLF];

paramNewValues=coeffNewValues;
Print["RunNewCoeffModel::paramNewValues = ",N[paramNewValues]];

If[doNotOutput3Dval == False ,CalcPlotAll[paramNewValues,tPlot,optsWithModelDescriptor]];
];

(* ============================================== *)

