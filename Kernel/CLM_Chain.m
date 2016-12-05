(* ============================================== *)
(* :Summary: CLM chain logic. *)
(* ============================================== *)
(* :Author: Konstantin K. Konstantinov *)
(* :Email: konstantin.k.konstantinov@gmail.com *)
(* :License type: GPL v3 or any later version, see http://www.gnu.org/licenses/ *)
(* :Copyright: K^3, 2013 - 2015 *)
(* :Version: Revision: 3.21.001, Date: 2015/12/12 *)
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
(* Letter to use instead of space in the names of variables *)
SpaceLetter=FromCharacterCode[3572];
(* ============================================== *)
Options[CLMChains]:={InitializeSynthesis -> True,InitializeCatSynthesis -> True,UseCatSynthEnantGrouping->True,PrintGamma-> False,InitializeActivation-> True,InitializeLigation -> True,InitializeCatLigation -> True,UseCatLigEnantGrouping->True,InitializePairFormation -> True,InitializeCrystallization -> True,InitializeCrystalDecay -> True,InitializeBasicCryst -> True, InitializeChainCryst -> True,InitializeTwoSubstCryst->False,UseSamePairFormCoeff -> False,UseSymmetricPairs -> False,TwoSubstCryst-> {}, InitializeDirectCrystallization-> False,UseAllPairsForDirectCryst->False,AssignDirectCrystCoefficients-> False,DiastereomerSeparator -> "$",InitializeEpimerization -> True,IndexPrefix-> "idx", ReactionPrefix-> "rIdx", CoeffPrefix-> "coeff" <> SpaceLetter,AssignSynthCoefficients -> False,AssignCatSynthCoefficients -> True,AssignWrongCatSynthReactions -> True,AssignActivationCoefficients-> True,AssignLigCoefficients -> True,AssignCatLigCoefficients -> True,AssignWrongCatLigReactions -> True,AssignBasicCrystCoefficients -> True,AssignChainCrystCoefficients ->True,AssignEpimCoefficients -> True,InitRandom ->True,InitYsubst -> False,YSubstWeight -> 1,InitLEnant -> True,InitDEnant -> False,TStartMultiplier -> 0,UseIdenticalEnantiomers -> False,UseOnlySimpleLigation -> False,GammaThreshold -> 0.7,PrintInitValues -> False,PlotSubstances-> False,SubstancePlotList -> {},RunModelOutputType-> 0,InitDMultiplier -> 0.99, RunChainModelPlotRange -> Automatic};
(* ============================================== *)
If[!SilentRunValue,Print["TODO::CLM_Chain::Possibly implement two different statistics of catalytic reactions: 1. Catalyst first and then reactions (currently implemented), 2. Just reactions..."]];
(* ============================================== *)
(* ToUpperCase and ToLowerCase do not work for Russian and some other letters *)
(* So we use English and Greek *)
UpperCaseLetterCodes=Join[Table[ii,{ii,65,87}],Table[ii,{ii,913,929}],Table[ii,{ii,931,937}](* ,Table[ii,{ii,1040,1071}] *)];
MaxEnantNoLimit=Length[UpperCaseLetterCodes];
(* ============================================== *)
(* Letter to write variable names like "A\:066d" *)
(* This is a different "star" even though it looks indistinguishable from the regular star in the comments *)
ActivationLetter=FromCharacterCode[1645];
(* ============================================== *)
(* Letters to write things like "\:02d6", "\:02d7", and "\.1a" in the names of variables *)  
PlusLetter=FromCharacterCode[726];
MinusLetter=FromCharacterCode[727];
ToLetter=FromCharacterCode[10132];

PiLetter="\[Pi]";
CapitalPiLetter="\[CapitalPi]";
SigmaLetter="\[Sigma]";
CapitalSigmaLetter="\[CapitalSigma]";
LambdaLetter="\[Lambda]";
CapitalLambdaLetter="\[CapitalLambda]";
GammaLetter="\[Gamma]";
CapitalGammaLetter="\[CapitalGamma]";
(* ============================================== *)
NoSimpleSubstCnt=Indeterminate;
(* ============================================== *)
DigitArray=Indeterminate;
DigitArrayL=Indeterminate;
DigitArrayD=Indeterminate;
EnantiomerRuleTbl={};
(* ============================================== *)
MaxEnantNo=Indeterminate;
MaxNonChiralSubstNo=Indeterminate;
(* ============================================== *)
MaxActivationLength=1;
(* ============================================== *)
PrintGammaValue=Indeterminate;
(* ============================================== *)
IndexPrefixValue=IndexPrefix /. Options[CLMChains];
ReactionPrefixValue=ReactionPrefix /. Options[CLMChains];
CoeffPrefixValue=CoeffPrefix /. Options[CLMChains];
DiastereomerSeparatorValue=DiastereomerSeparator /. Options[CLMChains];
(* ============================================== *)
(* SameChiralPolarizationQ checks if A and B have the same chiral polarization. *)
SameChiralPolarizationQ[substAid_?IntegerQ,substBid_?IntegerQ]:=Module[{aL,aD,bL,bD,retVal},
aL=GetChainNoOfL[substAid];
aD=GetChainNoOfD[substAid];

bL=GetChainNoOfL[substBid];
bD=GetChainNoOfD[substBid];

retVal=True;

If[(aL-aD)*(bL-bD)< 0,
(
retVal=False;
)
];

Return[retVal];
];
(* ============================================== *)
(* AminoAcidQ returns True if a substance is a one letter amino acid code. *)
AminoAcidQ[substName_?StringQ]:=Module[{retVal},
retVal=False;
If[MemberQ[DigitArray,substName],retVal=True];
Return[retVal];
];
(* ============================================== *)
(* ToActivated returns name (ID) of an activated substance: ABC \[Rule] ABC* *)
(* If the substance is already activated then it is returned ABC* \[Rule] ABC* *)
ToActivated[substName_?StringQ]:=Module[{lastChar,activatedSubstName},
lastChar=StringTake[substName,{-1}];
activatedSubstName=If[AminoAcidQ[lastChar],substName <>ActivationLetter,substName];
Return[activatedSubstName];
];
ToActivated[substID_?IntegerQ]:=Module[{substName,activatedSubstName,activatedSubstID},
substName=GetSubstanceName[substID];
activatedSubstName=ToActivated[substName];
activatedSubstID=GetSubstanceID[activatedSubstName];
Return[activatedSubstID];
];
(* ============================================== *)
(* ToDeactivated returns name (ID) of deactivated substance: ABC* \[Rule] ABC *)
(* If the substance is already deactivated then it is returned ABC \[Rule] ABC *)
ToDeactivated[substName_?StringQ]:=Module[{lastChar,deactivatedSubstName},
lastChar=StringTake[substName,{-1}];
deactivatedSubstName=If[lastChar==ActivationLetter,StringTake[substName,StringLength[substName]-1],substName];
Return[deactivatedSubstName];
];
ToDeactivated[substID_?IntegerQ]:=Module[{substName,deactivatedSubstName,deactivatedSubstID},
substName=GetSubstanceName[substID];
deactivatedSubstName=ToDeactivated[substName];
deactivatedSubstID=GetSubstanceID[deactivatedSubstName];
Return[deactivatedSubstID];
];
(* ============================================== *)
(* GetRightAminoAcid and GetLeftAminoAcid return the right and left amno acids of a peptide chain. *)
GetRightAminoAcid[substID_?IntegerQ]:=Module[{substName,aminoAcid,aminoAcidID},
substName=GetSubstanceName[ToDeactivated[substID]];
aminoAcid=StringTake[substName,{-1}];
aminoAcidID=GetSubstanceID[aminoAcid];

(*
Print["GetRightAminoAcid::substID = ", substID, ", substName = ", substName, ", aminoAcid = ", aminoAcid, ", aminoAcidID = ", aminoAcidID];
*)

Return[aminoAcidID];
];

GetLeftAminoAcid[substID_?IntegerQ]:=Module[{substName,aminoAcid,aminoAcidID},
substName=GetSubstanceName[ToDeactivated[substID]];
aminoAcid=StringTake[substName,{1}];
aminoAcidID=GetSubstanceID[aminoAcid];

(*
Print["GetLeftAminoAcid::substID = ", substID, ", substName = ", substName, ", aminoAcid = ", aminoAcid, ", aminoAcidID = ", aminoAcidID];
*)

Return[aminoAcidID];
];
(* ============================================== *)
SigmaMultiplier=2;
MuMultiplier=3;
(* ============================================== *)
MaxwellDistributionMultiplier=1/Sqrt[2*Pi];
(* ============================================== *)
DistributionMu[distribution_,params_?VectorQ]:=Module[{mu,paramVals},
paramVals=PrepareDistributionParameters[distribution,params];
mu=Mean[Apply[distribution,paramVals]];
Return[mu];
];
(* ============================================== *)
DistributionSigma[distribution_,params_?VectorQ]:=Module[{sigma,paramVals},
paramVals=PrepareDistributionParameters[distribution,params];
sigma=Abs[Sqrt[Variance[Apply[distribution,paramVals]]]];
If[!NumericQ[sigma],sigma=0];
Return[sigma];
];
(* ============================================== *)
DistributionPlotRange[distributionVec_?VectorQ,paramMatr_?MatrixQ]:=Module[{ii,retVal},
retVal=Mean[Table[DistributionPlotRange[distributionVec[[ii]],paramMatr[[ii]]],{ii,1,MaxChainLength}]];
Return[retVal];
];
(* ============================================== *)
DistributionPlotRange[distribution_,params_?VectorQ]:=Max[DistributionMu[distribution,params]+SigmaMultiplier*DistributionSigma[distribution,params],MuMultiplier*DistributionMu[distribution,params]];

(* ============================================== *)
(* Vector function to prepare distribution parameters *)
PrepareDistributionParameters[distributionVec_?VectorQ,paramMatr_?MatrixQ,base_?IntegerQ]:=Module[{len,retVal,distribution,params},
retVal=Indeterminate;

If[Length[distributionVec]== MaxChainLength && Length[paramMatr]== MaxChainLength && 1 <= base <= MaxChainLength,
(
distribution=distributionVec[[base]];
params=paramMatr[[base]];
retVal=PrepareDistributionParameters[distribution,params,base];
)
];

Return[retVal];
];
(* ============================================== *)
(* Function to prepare distribution parameters as we might need to rescale them somehow. *)
PrepareDistributionParameters[distribution_,params_?VectorQ]:=PrepareDistributionParameters[distribution,params,0];
PrepareDistributionParameters[distribution_,params_?VectorQ,base_?IntegerQ]:=Module[{paramVals,mu,eps,kVal,aVal},
paramVals=params;

If[ToString[distribution]== "InverseGaussianDistribution",
(
(* For InverseGaussianDistribution we use parameters as \[Mu] and \[Epsilon] instead of \[Mu] and \[Lambda] so that to have \[Lambda] = \[Mu]/\[Epsilon]^2. *)
(* In this case we have \[Epsilon] as a dimensionless "width" of the distribution with the variance \[Sigma]^2 = (\[Epsilon] \[Mu])^2. *)
paramVals={params[[1]],params[[1]]/params[[2]]^2};
)
];

If[ToString[distribution]== "MaxwellDistribution",
(
(* For MaxwellDistribution we rescale the parameters so that to have \[Mu] match the value of the parameter. *)
paramVals=MaxwellDistributionMultiplier*params;
)
];

If[ToString[distribution]== "ParetoDistribution",
(
(* For ParetoDistribution type I we rescale the parameters so that to have \[Mu] as mean and variance \[Sigma]^2 = (\[Epsilon] \[Mu])^2. *)
If[Length[params]==2 ,
(
mu=params[[1]];
eps=params[[2]];

If[(-1+eps^2)==0,
(
Print["PrepareDistributionParameters::Incorrect value of eps = ", eps, " for ParetoDistribution type I. Quitting..."];
Quit[];
)
];

kVal=Abs[((1+eps^2)*mu)/(-1+eps^2)];
aVal=Abs[1+(1+eps^2)/(-1+eps^2)];
paramVals={kVal,aVal};
),
(
If[3 <= Length[params] <= 4,
(
(* For ParetoDistribution[k,\[Alpha],\[Gamma],\[Mu]] (type IV) we do not rescale the parameters and always set \[Mu] to zero. *)
paramVals=Join[Take[params,3],{0}];
),
(
Print["PrepareDistributionParameters::ParetoDistribution::Incorrect length of params = ", params // MatrixForm];
Return[{Indeterminate, Indeterminate}];
)
];
)
];
)
];

Return[paramVals];
];
(* ============================================== *)
(* Vector function to prepare distribution parameters *)
PrepareDistributionShift[distributionVec_?VectorQ,paramMatr_?MatrixQ,base_?IntegerQ]:=Module[{len,retVal,distribution,params},
retVal=Indeterminate;

If[Length[distributionVec]== MaxChainLength && Length[paramMatr]== MaxChainLength && 1 <= base <= MaxChainLength,
(
distribution=distributionVec[[base]];
params=paramMatr[[base]];
retVal=PrepareDistributionShift[distribution,params,base];
)
];

Return[retVal];
];
(* ============================================== *)
(* For some distributions we need to shift the value of x *)
PrepareDistributionShift[distribution_,params_?VectorQ]:=PrepareDistributionShift[distribution,params,0];
PrepareDistributionShift[distribution_,params_?VectorQ,base_?IntegerQ]:=Module[{shiftVal,paramVals},
shiftVal=0;

If[ToString[distribution]== "ParetoDistribution" && Length[params]==2,
(
paramVals=PrepareDistributionParameters[distribution,params,base];
shiftVal=paramVals[[1]];
)
];

Return[shiftVal];
];
(* ============================================== *)
(* Vector Function, which creates random value of a coefficient *)
RandomCoefficientValue[distributionVec_?VectorQ,paramMatr_?MatrixQ]:=RandomCoefficientValue[distributionVec,paramMatr,0];
RandomCoefficientValue[distributionVec_?VectorQ,paramMatr_?MatrixQ,base_?IntegerQ]:=Module[{retVal,paramVals,shiftVal},
paramVals=PrepareDistributionParameters[distributionVec,paramMatr,base];
shiftVal=PrepareDistributionShift[distributionVec,paramMatr,base];
retVal=RandomVariate[Apply[distributionVec[[base]],paramVals]]-shiftVal;
Return[retVal];
];
(* ============================================== *)
(* Function, which creates random value of a coefficient *)
RandomCoefficientValue[distribution_,params_?VectorQ]:=RandomCoefficientValue[distribution,params,0];
RandomCoefficientValue[distribution_,params_?VectorQ,base_?IntegerQ]:=Module[{retVal,paramVals,shiftVal},
paramVals=PrepareDistributionParameters[distribution,params,base];
shiftVal=PrepareDistributionShift[distribution,params,base];
retVal=RandomVariate[Apply[distribution,paramVals]]-shiftVal;
Return[retVal];
];
(* ============================================== *)
(* PairedCoefficientValue creates one or two coefficients for a pair of "right" and "wrong" reactions *)
(* reacDescr is a List of reaction descriptor *)
(* reacDescrW is a List of "wrong" reaction descriptor (enantiomer of a catalyst) *)
(* IsWrong - if False, then return value for "right" reaction, if False then for "wrong" *)
(* funcName is the name of function to be used for storage if groupVal is defined *)
(* groupDescr is True / False / Indeterminate determines whether to choose Max / Min / Random *)
(* coeffFunc is a function to call to obtain value of a coefficient *)
(* coeffFuncparams is a List of parameters of coeffFunc *)
PairedCoefficientValue[reacDescr_?VectorQ,reacDescrW_?VectorQ,IsWrong_?BooleanQ,funcName_?StringQ,groupDescr_,coeffFunc_,coeffFuncParams_]:=Module[{groupVal,retVal,coeff,coeffW,coeff1,coeff2,info,infoW},
groupVal=ToString[groupDescr];

(* Just in case we apply canonical ordering. *)
info=Apply[CreateReactionInfo,reacDescr];
infoW=Apply[CreateReactionInfo,reacDescrW];

If[(groupVal== "True")|| (groupVal== "False"),
(
(* Grouping is defined and used *)
(* The catalysts are grouped - each catalyst is either explicitly "strong" or "weak" for ALL reactions *)
(* where it is a catalyst. True - strong, False - weak *)

coeff=ToExpression[funcName<>"[" <> ToString[info]<>"]"];
coeffW=ToExpression[funcName<>"[" <> ToString[infoW]<>"]"];

(* Print["PairedCoefficientValue::coeff = ", coeff, ", coeffW = ", coeffW]; *)

If[!NumericQ[coeff] || !NumericQ[coeffW],
(
(* The values of coefficients are NOT defined. *)
coeff1=Apply[coeffFunc,coeffFuncParams];
coeff2=Apply[coeffFunc,coeffFuncParams];

If[groupDescr,
(
(* "good" / "bad" *)
coeff=Max[coeff1,coeff2];
coeffW=Min[coeff1,coeff2];
),
(
(* "bad" / "good" *)
coeff=Min[coeff1,coeff2];
coeffW=Max[coeff1,coeff2];
)
];

ToExpression[funcName<>"[" <> ToString[info]<>"]="<>ToString[coeff]];
ToExpression[funcName<>"[" <> ToString[infoW]<>"]="<>ToString[coeffW]];
)
];

retVal=coeff;

(*
Print["PairedCoefficientValue::","reacDescr = ", ToSubstanceDisplay[reacDescr],", coeff = ",coeff,", reacDescrW = ", ToSubstanceDisplay[reacDescrW],", coeffW = ",coeffW,", retVal = ", retVal];
*)
),
(
(* Grouping is not defined *)
retVal=Apply[coeffFunc,coeffFuncParams];
)
];

(* Print[strSeparator]; *)

Return[retVal];
];
(* ============================================== *)
(* Functions to extract various values out of GammaPlus and GammaMinus *)
GammaGetCatalyst[gammaVal:{___}]:=gammaVal[[1]];
GammaGetReaction[gammaVal:{___}]:=gammaVal[[2]];
GammaGetReactionW[gammaVal:{___}]:=gammaVal[[3]];
GammaGetGammaVal[gammaVal:{___}]:=gammaVal[[4]];
GammaGetK1[gammaVal:{___}]:=gammaVal[[5]];
GammaGetK2[gammaVal:{___}]:=gammaVal[[6]];
(* ============================================== *)
(* Function to perform test if a random value exceeds a certain threshold *)
RandomTest[threshold_?NumericQ]:=Module[{retVal,rand},
rand=RandomReal[];
retVal=If[rand<=threshold,True,False];
Return[retVal];
];
(* ============================================== *)
(* Function to return catalyst and other substances from a given reaction info. *)
GetReactionCatalyst[reacInfo_?VectorQ]:=reacInfo[[Length[reacInfo]]];
GetReactionSubstA[reacInfo_?VectorQ]:=reacInfo[[1]];
GetReactionSubstB[reacInfo_?VectorQ]:=reacInfo[[2]];
GetReactionSubstAB[reacInfo_?VectorQ]:=reacInfo[[3]];
(* ============================================== *)
(* CreateReactionInfo creates reaction info record in *)
CreateReactionInfo[substAid_?IntegerQ,substBid_?IntegerQ,catalystSubstID_?IntegerQ]:=Module[{reacInfo,catEnantSubstID,Aid,Bid,substID},
catEnantSubstID=EnantiomerSubstanceID[catalystSubstID];

(* We store reaction info only for minimum of two enantiomers for a catalyst *)
(* The reaction is A+C \[Rule] B+C *)
If[catalystSubstID<= catEnantSubstID,
(
Aid=substAid;
Bid=substBid;
substID=catalystSubstID;
),
(
Aid=EnantiomerSubstanceID[substAid];
Bid=EnantiomerSubstanceID[substBid];
substID=catEnantSubstID;
)
];

reacInfo={Aid,Bid,substID};

Return[reacInfo];
];
(* ============================================== *)
CreateReactionInfo[substAid_?IntegerQ,substBid_?IntegerQ,substABid_?IntegerQ,catalystSubstID_?IntegerQ]:=Module[{reacInfo,catEnantSubstID,Aid,Bid,ABid,substID,enantSubstAid,enantSubstBid},
catEnantSubstID=EnantiomerSubstanceID[catalystSubstID];

(* We store reaction info only for minimum of two enantiomers for a catalyst *)
(* The reaction is A+B+C \[Rule] AB+C and due to explcit ordering there is no need to sort A, B??? *)
(* But we still sort (just in case) *)
If[catalystSubstID<= catEnantSubstID,
(
Aid=Min[substAid,substBid];
Bid=Max[substAid,substBid];
ABid=substABid;
substID=catalystSubstID;
),
(
enantSubstAid=EnantiomerSubstanceID[substAid];
enantSubstBid=EnantiomerSubstanceID[substBid];

Aid=Min[enantSubstAid,enantSubstBid];
Bid=Max[enantSubstAid,enantSubstBid];
ABid=EnantiomerSubstanceID[substABid];
substID=catEnantSubstID;
)
];

reacInfo={Aid,Bid,ABid,substID};

Return[reacInfo];
];
(* ============================================== *)

(* Functions, which return substance ID for pair or crystal of a certain signature *)
(* The info structure is returned by a call to PairCrystInfo[substID]] *)
GetPairLL[info_?VectorQ]:=info[[1]];
GetCrystLL[info_?VectorQ]:=info[[2]];
GetPairDD[info_?VectorQ]:=info[[3]];
GetCrystDD[info_?VectorQ]:=info[[4]];
GetPairLD[info_?VectorQ]:=info[[5]];
GetCrystLD[info_?VectorQ]:=info[[6]];
GetPairDL[info_?VectorQ]:=info[[7]];
GetCrystDL[info_?VectorQ]:=info[[8]];
(* ==============================================*)
Print["CLM_Chain::TODO::Check GetTotalRoPairLL, GetTotalRoPairDD, GetTotalRoPairLD, GetTotalRoCrystLL, GetTotalRoCrystDD, GetTotalRoCrystLD..."];
(* ==============================================*)
GetTotalRoPairLL[ro_?VectorQ]:=Module[{ii,retVal},
retVal=Sum[GetChainLength[ii]*ro[[GetPairLL[PairCrystInfo[ii]]]],{ii,MaxNonChiralSubstNo+1,NoSimpleSubstCnt}];
Return[retVal];
];
(* ==============================================*)
GetTotalRoPairDD[ro_?VectorQ]:=Module[{ii,retVal},
retVal=Sum[GetChainLength[ii]*ro[[GetPairDD[PairCrystInfo[ii]]]],{ii,MaxNonChiralSubstNo+1,NoSimpleSubstCnt}];
Return[retVal];
];
(* ==============================================*)
GetTotalRoPairLD[ro_?VectorQ]:=Module[{ii,retVal},
retVal=Sum[GetChainLength[ii]*ro[[GetPairLD[PairCrystInfo[ii]]]],{ii,MaxNonChiralSubstNo+1,NoSimpleSubstCnt}];
Return[retVal];
];
(* ==============================================*)
GetTotalRoCrystLL[ro_?VectorQ]:=Module[{ii,retVal},
retVal=Sum[GetChainLength[ii]*ro[[GetCrystLL[PairCrystInfo[ii]]]],{ii,MaxNonChiralSubstNo+1,NoSimpleSubstCnt}];
Return[retVal];
];
(* ==============================================*)
GetTotalRoCrystDD[ro_?VectorQ]:=Module[{ii,retVal},
retVal=Sum[GetChainLength[ii]*ro[[GetCrystDD[PairCrystInfo[ii]]]],{ii,MaxNonChiralSubstNo+1,NoSimpleSubstCnt}];
Return[retVal];
];
(* ==============================================*)
GetTotalRoCrystLD[ro_?VectorQ]:=Module[{ii,retVal},
retVal=Sum[GetChainLength[ii]*ro[[GetCrystLD[PairCrystInfo[ii]]]],{ii,MaxNonChiralSubstNo+1,NoSimpleSubstCnt}];
Return[retVal];
];
(* ============================================== *)