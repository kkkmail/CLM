(* ============================================== *)
(* :Summary: Main CLM chain module. *)
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
ClearAll["Global`*"];
(* ============================================== *)
maxEnantNumb=1;
maxChainLen=3;
silentRun=False;
(* ============================================== *)
CDFuncSelector=3;
DissolveSelector=1;
(* ============================================== *)
PathList={"W:\\Math\\CLM\\"};
Get["CLM_Init.m",Path-> PathList];
LoadModules[PathList, silentRun];
(* ============================================== *)
rndVal=RandomInteger[{0,10^12}];
SeedRandomValue=rndVal;
(* ============================================== *)
(* SeedRandomValue=495732029476; *)
SeedRandomValue=493028958696;
(* ============================================== *)
Print["SeedRandomValue = ", SeedRandomValue];
(* ============================================== *)
tMaxVal=1*10^6;
tNoOfSteps=40;
roTotInitVal=1000;
alphaVal=(9/10);
(* ============================================== *)
RunNDSolve=True;
(* ============================================== *)
(* UseLogScale=False; *)
(* ============================================== *)
plotDistributionsVal=False;
MaxDetailedOutputChainLen=3;
(* ============================================== *)
(* ChainModelY, ChainModelL, ChainModelGoodL, ChainModelGoodAllLD, ChainModelRandMixLD, ChainModelPropMixLD *)
ChainModelID=ChainModelPropMixLD;
(*ChainModelID=ChainModelY; *)
(* ============================================== *)
SynthMultiplier=1*10^-10;
DecayMultiplier=1*10^-10;
(* ============================================== *)
CatSynthMultiplier=10^0;
InvCatSynthMultiplier=10^0;
(* ============================================== *)
LigMultiplier=10^0;
InvLigMultiplier=10^0;
(* ============================================== *)
CatLigMultiplier=10^0;
InvCatLigMultiplier=10^0;
(* ============================================== *)
DiastMultiplier=10^0;
InvDiastMultiplier=10^0;
(* ============================================== *)
rMaxCoeffMultiplier=5*10^-3;
kCrystCoeffMultiplier=10^2;
kDissCoeffMultiplier=10^0;
(* ============================================== *)
CrystDecayCoeffMultiplier=10^1;
(* ============================================== *)
DirectCrystMinLen=1;
(* ============================================== *)
SynthCoeffParams={SynthMultiplier,1};
InvSynthCoeffParams={DecayMultiplier,1};
(* ============================================== *)
CatSynthR=1;
CatSynthMinLen=1;
(* ============================================== *)
CatLigC=0.05;
CatLigR=0.05;

CatLigMinLen=2;
(* ============================================== *)
(* ============================================== *)
ActivationCoeffParams={1,0.5};
DeactivationCoeffParams={10^-3,0.5};
(* ============================================== *)
(* Epimerization *)
(* EpimTotalRateDistribution=ParetoDistribution; *)
EpimTotalRateParams={1,0.5};
(* ============================================== *)
(* EpimCoeffDistribution=ParetoDistribution; *)
EpimCoeffParams={1,0.5};
(* ============================================== *)
(* EpimUnstableAAweight: varies between 0 and 1 and determines how stable is AA substance. *)
(* If value  0 then AA is completely stable (does not have any epimerization) and Aa is unstable. *)
(* If value  1 then Aa is completely stable (does not have any epimerization) and AA is unstable. *)
EpimUnstableAAweight=0.1;
(* ============================================== *)
(* EpimSTermAAweight determines the allocation between N and S terminals for AA (and Aa) like substances : *)
(* If value  0 then S terminal is completely stable and N terminal is unstable. *)
(* If value  1 then N terminal is completely stable and S terminal is unstable. *)
EpimSTermAAweight=0;
(* ============================================== *)
(* EpimTotalRateRandWeight: varies between 0 and 1 *)
(* The value determines linear allocation between deterministic and random value for total rate *)
(* If  0, then total rate of epimerization of a pair AB is completely determined by a function of two parameters: *)
(* EpimRatePairDet[A,B] where A and B are IDs of the relevant substances *)
(* If  1, then epimerization of a pair AB is completely random *)
EpimTotalRateRandWeight=0;
(* ============================================== *)
(* EpimTotalRateDiffWeight: varies between -1 and 1 *)
(* The coefficient determines the behaviour (linear allocation) of function EpimRatePairDet *)
(* If  0, then the half sum is taken *)
(* If  -1, then the first coefficient is used *)
(* If  1, then the second coefficient is used *)
EpimTotalRateDiffWeight=-1;
(* ============================================== *)
(* EpimCoeffRandWeight: varies between 0 and 1 *)
(* The value determines linear allocation between deterministic and random value for coefficients *)
(* If  0, then coefficients of epimerization of a pair AB are completely determined by a function of two parameters: *)
(* EpimCoeffPair[A,B] where A and B are IDs of the relevant substances *)
(* If  1, then epimerization of a pair AB is completely random *)
EpimCoeffRandWeight=0;
(* ============================================== *)
(* EpimCoeffDiffWeight: varies between -1 and 1 *)
(* The coefficient determines the behaviour (linear allocation) of function EpimCoeffPairDet *)
(* If  0, then the half sum is taken *)
(* If  -1, then the first coefficient is used *)
(* If  1, then the second coefficient is used *)
EpimCoeffDiffWeight=-1;
(* ============================================== *)
(* ============================================== *)
(* Ligation parameters. Default distribution: InverseGaussianDistribution. *)
(* LigTotalRateDistribution=ParetoDistribution; *)
LigTotalRateParams={LigMultiplier,0.5};
(* ============================================== *)
(* InvLigTotalRateDistribution=ParetoDistribution; *)
InvLigTotalRateParams={InvLigMultiplier,0.5};
(* ============================================== *)
(* LigCoeffDistribution=ParetoDistribution; *)
LigCoeffParams={1,0.5};
(* ============================================== *)
(* InvLigCoeffDistribution=ParetoDistribution; *)
InvLigCoeffParams={1,0.5};
(* ============================================== *)
(* LigNotAAweight: varies between 0 and 1 and determines relative rates of ligation reactions. *)
(* If value  0 then total rate is allocated to A+A  AA and zero to A+a  Aa *)
(* If value  1 then total rate is allocated to A+a  Aa and zero to A+A  AA *)
(* This weight is used during random generation of values of coefficients. *)
LigNotAAweight=0.1;
(* ============================================== *)
(* Almost the same as above but for inverse reactions. *)
(* If value  0 then total rate is allocated to Aa  A+a and zero to AA  A+A *)
(* If value  1 then total rate is allocated to AA  A+A and zero to Aa  A+a *)
InvLigAAweight=0.65;
(* ============================================== *)
(* LigTotalRateRandWeight: varies between 0 and 1 *)
(* The value determines linear allocation between deterministic and random value for total rate *)
(* If  0, then total rate of ligation of a pair A+B  AB is completely determined *)
(* by a function of two parameters: *)
(* LigRatePairDet[A,B] where A and B are IDs of the relevant substances *)
(* If  1, then total ligation rate of a pair A+B  AB is completely random *)
(* The same applies to inverse ligation *)
LigTotalRateRandWeight=0;
InvLigTotalRateRandWeight=0;
(* ============================================== *)
(* LigTotalRateDiffWeight: varies between -1 and 1 *)
(* The coefficient determines the behaviour (linear allocation) of function LigRatePairDet *)
(* If  0, then the half sum is taken *)
(* If  -1, then the first coefficient is used *)
(* If  1, then the second coefficient is used *)
LigTotalRateDiffWeight=-1;
InvLigTotalRateDiffWeight=-1;
(* ============================================== *)
(* LigCoeffRandWeight: varies between 0 and 1 *)
(* The value determines linear allocation between deterministic and random value for coefficients *)
(* If  0, then coefficients of ligation of a pair AB are completely determined by a function of two parameters: *)
(* LigCoeffPair[A,B] where A and B are IDs of the relevant substances *)
(* If  1, then ligation of a pair AB is completely random *)
(* The same applies to inverse ligation *)
LigCoeffRandWeight=0;
InvLigCoeffRandWeight=0;
(* ============================================== *)
(* LigCoeffDiffWeight: varies between -1 and 1 *)
(* The coefficient determines the behaviour (linear allocation) of function LigCoeffPairDet *)
(* If  0, then the half sum is taken *)
(* If  -1, then the first coefficient is used *)
(* If  1, then the second coefficient is used *)
LigCoeffDiffWeight=-1;
InvLigCoeffDiffWeight=-1;
(* ============================================== *)
(* ============================================== *)
YSubstWeightVal=1;
SigmaMultiplier=0;
(* ============================================== *)
(* Direct SYNTHESIS coefficients *)
coeffYtoAValue=SynthMultiplier*1.0;
coeffYtoBValue=SynthMultiplier*1.5;
coeffYtoCValue=SynthMultiplier*2.0;
coeffYtoDValue=SynthMultiplier*2.5;
coeffYtoEValue=SynthMultiplier*3.0;
coeffYtoFValue=SynthMultiplier*3.5;
(* ============================================== *)
(* Direct DECAY coefficients *)
coeffStoYValue=DecayMultiplier;
dCoeffStoYValue=1.0*10^-2;
(* ============================================== *)
(* Catalytic synthesis parameters. Default distribution: ParetoDistribution. *)
CatSynthCoeffParams={CatSynthMultiplier,10^1};
InvCatSynthCoeffParams={InvCatSynthMultiplier,10^1};
(* ============================================== *)
(* Catalytic ligation parameters. Default distributions: ParetoDistribution. *)
CatLigCoeffParams={CatLigMultiplier,10^1};
InvCatLigCoeffParams={InvCatLigMultiplier,10^1};
(* ============================================== *)
(* Diastereomer and inverse formation parameters. Default distributions: InverseGaussianDistribution. *)
DiastCoeffParams={DiastMultiplier,1};
InvDiastCoeffParams={InvDiastMultiplier,1};
(* ============================================== *)
(* Crystallization / dissolution parameters. *)
(* Default distributions: InverseGaussianDistribution, InverseGaussianDistribution, InverseGaussianDistribution. *)
(* Assume that average solubility falls with the length of peptides *)
rMaxCoeffDistribution=Table[InverseGaussianDistribution,{ii,1,maxChainLen}];
kCrystCoeffDistribution=Table[InverseGaussianDistribution,{ii,1,maxChainLen}];
kDissCoeffDistribution=Table[InverseGaussianDistribution,{ii,1,maxChainLen}];

rMaxCoeffParams=Table[{rMaxCoeffMultiplier/ii,1},{ii,1,maxChainLen}];
kCrystCoeffParams=Table[{kCrystCoeffMultiplier,1},{ii,1,maxChainLen}];
kDissCoeffParams=Table[{kDissCoeffMultiplier,1},{ii,1,maxChainLen}];
(* ============================================== *)
(* Crystal decay parameters. Default distribution: InverseGaussianDistribution. *)
CrystDecayCoeffParams={CrystDecayCoeffMultiplier,1};
(* ============================================== *)
(* ============================================== *)
unknownOptions:={PrintGamma-> False,RoTotalNorm -> roTotInitVal,PrintInitValues-> True};
(* ============================================== *)
debugOptions:={PrintPrepareEquationsInfo -> False,PrintρAllFuncInfo -> False};
(* ============================================== *)
(* NonNegativeTypeNone, NonNegativeTypeNumeric, NonNegativeTypeAnalytic *)
generalOptions:={UseHighPrecision-> True,NonNegativeType -> NonNegativeTypeNumeric,UseIdenticalEnantiomers-> False};
(* ============================================== *)
synthesisOptions:={InitializeSynthesis -> True,InitializeCatSynthesis -> True,UseCatSynthEnantGrouping->True,AssignSynthCoefficients -> False,AssignCatSynthCoefficients -> False,AssignWrongCatSynthReactions -> True};
(* ============================================== *)
activationOptions:={InitializeActivation-> True,AssignActivationCoefficients-> False};
(* ============================================== *)
ligationOptions:={InitializeLigation -> True,UseOnlySimpleLigation-> True,AssignLigCoefficients -> False};
(* ============================================== *)
catLigationOptions:={InitializeCatLigation -> False,UseCatLigEnantGrouping->False,AssignCatLigCoefficients -> True,AssignWrongCatLigReactions -> True};
(* ============================================== *)
pairFormationOptions:={InitializePairFormation -> True,UseSamePairFormCoeff-> True,UseSymmetricPairs-> False};
(* ============================================== *)
crystallizationOptions:={InitializeCrystallization -> True,InitializeCrystalDecay -> True,InitializeBasicCryst -> True, InitializeChainCryst -> True,InitializeTwoSubstCryst->True,TwoSubstCryst->  {{idxAA, idxAaa} (*{idxAA, idxAAa} {idxAA, idxAAA} *)},AssignBasicCrystCoefficients -> False,UseAllSubstForCryst-> False,AllSubstForCrystAlpha->alphaVal,InitializeDirectCrystallization-> False,AssignDirectCrystCoefficients->False};
(* ============================================== *)
epimerizationOptions:={InitializeEpimerization -> False,AssignEpimCoefficients -> False};
(* ============================================== *)
initialValuesOptions:={InitRandom ->initRandomVal,InitYsubst -> initYsubstVal,YSubstWeight ->YSubstWeightVal,InitLEnant -> initLEnantVal,InitDEnant -> initDEnantVal,InitDMultiplier -> 0.98};
(* ============================================== *)
NDSolveOptions:={ApplyCoeffRule-> False,NDSolveMethod -> {"EquationSimplification"->"Residual"},NDSolveUseDerivativeForAggregateSubst-> False,NDSolveUseFuncForAggregateSubst -> True,NDSolveUseNumericFuncForAggregateSubst -> False,DoNotUseInterpolationFunction->False};
(* ============================================== *)
NDSolveMonitorOptions:={NStorage-> 1000,PrintMonitorData-> False,MonitorPrintFrequency -> 1000,MonitorType -> MonitorTypeTime,StepMonitorFunction:> CalculateData,DynamicStepMonitor -> {OutputDynamicStepMonitorTime,OutputDynamicStepMonitorData,OutputDynamicStepMonitor},QuitMonitor->QuitMonitorFunction};
(* ============================================== *)
outputOptions:={ρAllFuncDescription:>description,TStartMultiplier -> 0};
(* ============================================== *)
(* OutputTypeNone, OutputTypeAll *)
runChainModelOptions:={RunModelOutputType -> OutputTypeAll};
(* ============================================== *)
(* plotSubstanceOptions:={PlotSubstances True,SubstancePlotList {{rA,ra},{rA٭,ra٭},{rAA,raa},{rAa,raA}}}; *)
plotSubstanceOptions:={PlotSubstances-> True,SubstancePlotList-> {{rY},{rA,ra},{rAA,raa},{rAa,raA},{rAAA,raaa},{rAAa,raaA},{rAaA,raAa},{rAaa,raAA},{rAA$Aaa,raa$aAA},{r$$$AA$Aaa,r$$$aa$aAA}}};
(* ============================================== *)
(* StepMonitorFunction CalculateData,DynamicStepMonitor  {OutputDynamicStepMonitorTime,OutputDynamicStepMonitorData,OutputDynamicStepMonitor},QuitMonitorQuitMonitorFunction *)
(* NonNegativeTypeNumeric, NonNegativeTypeAnalytic *)
(* MonitorType  None, MonitorTypeTime, MonitorTypeData *)
(* ============================================== *)
description="maxEnantNumb = " <> ToString[maxEnantNumb] <>", maxChainLen = " <> ToString[maxChainLen] <>", roTotInitVal = " <> ToString[roTotInitVal] <>", alphaVal = " <> ToString[N[alphaVal]];
(* ============================================== *)
options =Join[generalOptions,synthesisOptions,activationOptions,ligationOptions,catLigationOptions,pairFormationOptions,crystallizationOptions,epimerizationOptions,initialValuesOptions,debugOptions,NDSolveOptions,NDSolveMonitorOptions,outputOptions,runChainModelOptions,plotSubstanceOptions,unknownOptions];
Print["options = ", options];
(* ============================================== *)
Initialize[options];
(* ============================================== *)
InitializeChains[maxChainLen,maxEnantNumb,options];
(* ============================================== *)
(* ============================================== *)
(* ============================================== *)
coeff෴Y➔A=10^-3;
coeff෴A➔Y=10^-6;
coeff෴Y➔a=coeff෴Y➔A;
coeff෴a➔Y=coeff෴A➔Y;
(* ============================================== *)
coeff෴A➔A٭=1;
coeff෴A٭➔A=10^-4;
(* ============================================== *)
Λ˖=1;
Λ˗=1;
coeff෴A٭˖A➔AA=Λ˖;
coeff෴AA➔A˖A=Λ˗;
coeff෴a٭˖A➔aA=Λ˖;
coeff෴aA➔a˖A=Λ˗;
coeff෴A٭˖AA➔AAA=Λ˖;
coeff෴AAA➔A˖AA=Λ˗;
coeff෴a٭˖AA➔aAA=Λ˖;
coeff෴aAA➔a˖AA=Λ˗;
coeff෴A٭˖Aa➔AAa=Λ˖;
coeff෴AAa➔A˖Aa=Λ˗;
coeff෴a٭˖Aa➔aAa=Λ˖;
coeff෴aAa➔a˖Aa=Λ˗;
(* ============================================== *)

Γ˖=0*10^1;
Γ˗=0*10^-1;

γ˖=0.00;
γ˗=γ˖;

catIdx=4;

catIdxTbl=Table[0,{ii,1,7}];
catIdxTbl[[catIdx]]=1;

{mA,mAA,mAa,mAAA,mAAa,mAaA,mAaa}=catIdxTbl;
Print["{mA,mAA,mAa,mAAA,mAAa,mAaA,mAaa} = ", {mA,mAA,mAa,mAAA,mAAa,mAaA,mAaa} // MatrixForm];

coeff෴Y˖A➔A˖A=mA*Γ˖(1+γ˖);
coeff෴A˖A➔Y˖A=mA*Γ˗(1-γ˗);
coeff෴Y˖A➔a˖A=mA*Γ˖(1-γ˖);
coeff෴a˖A➔Y˖A=mA*Γ˗(1+γ˗);

coeff෴Y˖AA➔A˖AA=mAA*Γ˖(1+γ˖);
coeff෴A˖AA➔Y˖AA=mAA*Γ˗(1-γ˗);
coeff෴Y˖AA➔a˖AA=mAA*Γ˖(1-γ˖);
coeff෴a˖AA➔Y˖AA=mAA*Γ˗(1+γ˗);

coeff෴Y˖Aa➔A˖Aa=mAa*Γ˖(1+γ˖);
coeff෴A˖Aa➔Y˖Aa=mAa*Γ˗(1-γ˗);
coeff෴Y˖Aa➔a˖Aa=mAa*Γ˖(1-γ˖);
coeff෴a˖Aa➔Y˖Aa=mAa*Γ˗(1+γ˗);

coeff෴Y˖AAA➔A˖AAA=mAAA*Γ˖(1+γ˖);
coeff෴A˖AAA➔Y˖AAA=mAAA*Γ˗(1-γ˗);
coeff෴Y˖AAA➔a˖AAA=mAAA*Γ˖(1-γ˖);
coeff෴a˖AAA➔Y˖AAA=mAAA*Γ˗(1+γ˗);

coeff෴Y˖AAa➔A˖AAa=mAAa*Γ˖(1+γ˖);
coeff෴A˖AAa➔Y˖AAa=mAAa*Γ˗(1-γ˗);
coeff෴Y˖AAa➔a˖AAa=mAAa*Γ˖(1-γ˖);
coeff෴a˖AAa➔Y˖AAa=mAAa*Γ˗(1+γ˗);

coeff෴Y˖AaA➔A˖AaA=mAaA*Γ˖(1+γ˖);
coeff෴A˖AaA➔Y˖AaA=mAaA*Γ˗(1-γ˗);
coeff෴Y˖AaA➔a˖AaA=mAaA*Γ˖(1-γ˖);
coeff෴a˖AaA➔Y˖AaA=mAaA*Γ˗(1+γ˗);

coeff෴Y˖Aaa➔A˖Aaa=mAaa*Γ˖(1+γ˖);
coeff෴A˖Aaa➔Y˖Aaa=mAaa*Γ˗(1-γ˗);
coeff෴Y˖Aaa➔a˖Aaa=mAaa*Γ˖(1-γ˖);
coeff෴a˖Aaa➔Y˖Aaa=mAaa*Γ˗(1+γ˗);
(* ============================================== *)

Π˖=10;
Π˗=10;
π˖=0;
π˗=0;

Σ˖=10^3;
Σ˗=10^-2;

Δ˗=10^-3;
ρ˗=10^-3;
ρ˖=10^2;

r$AA$Aaa$Max=ρ˗;
kCryst$AA$Aaa=Σ˖;
kDiss$AA$Aaa=Σ˗;
coeff෴$$$AA$Aaa➔5Y=Δ˗;

r$AA$aAA$Max=ρ˖;
kCryst$AA$aAA=Σ˖;
kDiss$AA$aAA=Σ˗;
coeff෴$$$AA$aAA➔5Y=Δ˗;


r$AA$AAa$Max=ρ˗;
kCryst$AA$AAa=Σ˖;
kDiss$AA$AAa=Σ˗;
coeff෴$$$AA$AAa➔5Y=Δ˗;

r$AA$aaA$Max=ρ˖;
kCryst$AA$aaA=Σ˖;
kDiss$AA$aaA=Σ˗;
coeff෴$$$AA$aaA➔5Y=Δ˗;


r$AA$AAA$Max=ρ˗;
kCryst$AA$AAA=Σ˖;
kDiss$AA$AAA=Σ˗;
coeff෴$$$AA$AAA➔5Y=Δ˗;

r$AA$aaa$Max=ρ˖;
kCryst$AA$aaa=Σ˖;
kDiss$AA$aaa=Σ˗;
coeff෴$$$AA$aaa➔5Y=Δ˗;

(* ============================================== *)
(* ============================================== *)
(* ============================================== *)
PrintAllInfo[If[MaxChainLength<=MaxDetailedOutputChainLen,True,False],True,plotDistributionsVal,options];
(* ============================================== *)
ProcessLCatInfo[True,options];
(* ============================================== *)
(* ============================================== *)

If[RunNDSolve,
(
sol=RunChainModel[ChainModelID,tMaxVal,roTotInitVal,options];
nuLst=nuValueListAminoAcid[tMaxVal,sol];
nuLstSorted=Sort[nuLst];
Print["nuLst = ", nuLst // MatrixForm];

aaTotalLst=TotalAminoAcidList[tMaxVal,sol];
Print["Concentrations of amino acids = ", Table[{ii,DigitArrayL[[ii]],aaTotalLst[[ii]],DigitArrayD[[ii]],aaTotalLst[[ii+MaxEnantNo]]},{ii,1,MaxEnantNo}] // MatrixForm];

aaTotal=Sum[aaTotalLst[[ii]],{ii,1,2*MaxEnantNo}];
Print["aaTotal = ", aaTotal];

aaBothEnantNormLst=Table[MaxEnantNo*(aaTotalLst[[ii]]+aaTotalLst[[ii+MaxEnantNo]])/aaTotal,{ii,1,MaxEnantNo}];
aaBothEnantNormLstSorted=SortByFirst[nuLst,aaBothEnantNormLst];

Print[strSeparator];
Print[strSeparator];

Print["η at amino acid level."];
SetLegends[{"η"}];
Print[DiscretePlot[nuLstSorted[[ii]],{ii,1,MaxEnantNo},Evaluate[discrPlotOpts2]]];

Print[strSeparator];

SetLegends[{"η","ρ"}];
Print[DiscretePlot[{nuLstSorted[[ii]],aaBothEnantNormLstSorted[[ii]]},{ii,1,MaxEnantNo},Evaluate[discrPlotOpts2]]];

Print[strSeparator];

nuLst3D=Table[SortByFirst[nuLst,nuValueListAminoAcid[(tMaxVal*ii/tNoOfSteps),sol]],{ii,0,tNoOfSteps}];

xName="n";
yName="t";
zName="η";
Print[DiscretePlot3D[nuLst3D[[Round[tNoOfSteps*tt/tMaxVal]+1,ii]],{ii,1,MaxEnantNo},{tt,0,tMaxVal,(tMaxVal/tNoOfSteps)},Evaluate[discrPlot3DOpts]]];

Print[strSeparator];
Print[strSeparator];
Print[strSeparator];
(* ============================================== *)

Print["η amont all substances..."];
nuTotalLst=Table[nuValue[aaTotalLst[[ii]],aaTotalLst[[ii+MaxEnantNo]]],{ii,1,MaxEnantNo}];
nuTotalLstSorted=Sort[nuTotalLst];

SetLegends[{"η"}];
Print[DiscretePlot[nuTotalLstSorted[[ii]],{ii,1,MaxEnantNo},Evaluate[discrPlotOpts2]]];

aaBothEnantNormTotalLstSorted=SortByFirst[nuTotalLst,aaBothEnantNormLst];
SetLegends[{"η","ρ"}];
Print[DiscretePlot[{nuTotalLstSorted[[ii]],aaBothEnantNormTotalLstSorted[[ii]]},{ii,1,MaxEnantNo},Evaluate[discrPlotOpts2]]];

Print[strSeparator];
Print[strSeparator];
)
];
(* ============================================== *)
(* Print["InterpolationMonitorArray"," = ", Join[DataHeader[],InterpolationMonitorArray] // MatrixForm]; *)
(* ============================================== *)
PrintTimeUsed[];
Print["SeedRandomValue = ", SeedRandomValue];
(* ============================================== *)
(*
catSynthIdx=1;
dirSedIdx=24;
coeffRules={};
AddAllCoeffRules[catSynthIdx,dirSedIdx];
Print["coeffRules = ", coeffRules // MatrixForm];
*)
(* ============================================== *)
