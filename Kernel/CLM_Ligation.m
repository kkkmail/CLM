(* ============================================== *)
(* :Summary: CLM ligation logic. *)
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
(* MaxChainLength is the maximum length of peptide chain *)
MaxChainLength=Indeterminate;
(* ============================================== *)
InitializeLigationValue=Indeterminate;
AssignLigCoefficientsValue=Indeterminate;
(* ============================================== *)
(* If True then only amino acids can attach to the chain (from the left). *)
(* If False then anything is possible *)
UseOnlySimpleLigationValue=Indeterminate;
(* ============================================== *)
(* ============================================== *)
(* Distribution parameters of total ligation rate for a single amino acid. *)
(* If activation is NOT used then it is determined as *)
(* rate[A+A \[Rule] AA] + rate[A+a \[Rule] Aa] *)
(* If activation IS used then it is determined as *)
(* rate[A*+A \[Rule] AA] + rate[A*+a \[Rule] Aa] *)
LigTotalRateDistribution=InverseGaussianDistribution;
LigTotalRateParams={1,1};
(* ============================================== *)
(* Same as above but for inverse reaction (activation is irrelevant) *)
(* rate[AA \[Rule] A+A] + rate[Aa \[Rule] A+a] *)
InvLigTotalRateDistribution=InverseGaussianDistribution;
InvLigTotalRateParams={1,1};
(* ============================================== *)
(* Distribution parameters of unweighted and not normalized ligation coefficients. *)
LigCoeffDistribution=InverseGaussianDistribution;
LigCoeffParams={1,1};
(* ============================================== *)
(* Distribution parameters of unweighted and not normalized inverse ligation coefficients. *)
InvLigCoeffDistribution=InverseGaussianDistribution;
InvLigCoeffParams={1,1};
(* ============================================== *)
(* LigNotAAweight: varies between 0 and 1 and determines relative rates of ligation reactions. *)
(* If value \[Equal] 0 then total rate is allocated to A+A \[Rule] AA and zero to A+a \[Rule] Aa *)
(* If value \[Equal] 1 then total rate is allocated to A+a \[Rule] Aa and zero to A+A \[Rule] AA *)
(* This weight is used during random generation of values of coefficients. *)
LigNotAAweight=Indeterminate;
(* ============================================== *)
(* Almost the same as above but for inverse reactions. *)
(* If value \[Equal] 0 then total rate is allocated to Aa \[Rule] A+a and zero to AA \[Rule] A+A *)
(* If value \[Equal] 1 then total rate is allocated to AA \[Rule] A+A and zero to Aa \[Rule] A+a *)
InvLigAAweight=Indeterminate;
(* ============================================== *)
(* ============================================== *)
(* Coefficients for pairs of amino acids. *)
(* ============================================== *)
(* LigTotalRateRandWeight: varies between 0 and 1 *)
(* The value determines linear allocation between deterministic and random value for total rate *)
(* If \[Equal] 0, then total rate of ligation of a pair A+B \[Rule] AB is completely determined *)
(* by a function of two parameters: *)
(* LigRatePairDet[A,B] where A and B are IDs of the relevant substances *)
(* If \[Equal] 1, then total ligation rate of a pair A+B \[Rule] AB is completely random *)
(* The same applies to inverse ligation *)
LigTotalRateRandWeight=Indeterminate;
InvLigTotalRateRandWeight=Indeterminate;
(* ============================================== *)
(* LigTotalRateDiffWeight: varies between -1 and 1 *)
(* The coefficient determines the behaviour (linear allocation) of function LigRatePairDet *)
(* If \[Equal] 0, then the half sum is taken *)
(* If \[Equal] -1, then the first coefficient is used *)
(* If \[Equal] 1, then the second coefficient is used *)
LigTotalRateDiffWeight=Indeterminate;
InvLigTotalRateDiffWeight=Indeterminate;
(* ============================================== *)
(* LigCoeffRandWeight: varies between 0 and 1 *)
(* The value determines linear allocation between deterministic and random value for coefficients *)
(* If \[Equal] 0, then coefficients of ligation of a pair AB are completely determined by a function of two parameters: *)
(* LigCoeffPair[A,B] where A and B are IDs of the relevant substances *)
(* If \[Equal] 1, then ligation of a pair AB is completely random *)
(* The same applies to inverse ligation *)
LigCoeffRandWeight=Indeterminate;
InvLigCoeffRandWeight=Indeterminate;
(* ============================================== *)
(* LigCoeffDiffWeight: varies between -1 and 1 *)
(* The coefficient determines the behaviour (linear allocation) of function LigCoeffPairDet *)
(* If \[Equal] 0, then the half sum is taken *)
(* If \[Equal] -1, then the first coefficient is used *)
(* If \[Equal] 1, then the second coefficient is used *)
LigCoeffDiffWeight=Indeterminate;
InvLigCoeffDiffWeight=Indeterminate;
(* ============================================== *)
(* ============================================== *)
(* LigTotalRateValue returns random value of total ligation rate (sum of 2 coefficients). *)
LigTotalRateValue[]:=Module[{retVal},
retVal=RandomCoefficientValue[LigTotalRateDistribution,LigTotalRateParams];
Return[retVal];
];
(* ============================================== *)
(* InvLigTotalRateValue returns random value of total inverse ligation rate (sum of 2 coefficients). *)
InvLigTotalRateValue[]:=Module[{retVal},
retVal=RandomCoefficientValue[InvLigTotalRateDistribution,InvLigTotalRateParams];
Return[retVal];
];
(* ============================================== *)
(* LigCoeffValue returns random UNWEIGHTED value of relative ligation coefficient. *)
(* See LigCoeffRandom BELOW for further description. *)
LigCoeffValue[]:=Module[{retVal},
retVal=RandomCoefficientValue[LigCoeffDistribution,LigCoeffParams];
Return[retVal];
];
(* ============================================== *)
(* InvLigCoeffValue returns random UNWEIGHTED value of relative inverse ligation coefficient. *)
(* See InvLigCoeffRandom BELOW for further description. *)
InvLigCoeffValue[]:=Module[{retVal},
retVal=RandomCoefficientValue[InvLigCoeffDistribution,InvLigCoeffParams];
Return[retVal];
];
(* ============================================== *)
(* Functions to extract total rate and split among all 2 ligation coefficients for given descriptor. *)
GetLigTotalRate[ligDescr_?VectorQ]:=ligDescr[[1]];
GetLigkApBtoAB[ligDescr_?VectorQ]:=ligDescr[[2]];
GetLigkApbtoAb[ligDescr_?VectorQ]:=ligDescr[[3]];
(* All ligation coefficients *)
GetLigCoeffTable[ligDescr_?VectorQ]:=Delete[ligDescr,1];
(* ============================================== *)
(* Functions to extract total rate and split among all 2 inverse ligation coefficients for given descriptor. *)
GetInvLigTotalRate[ligDescr_?VectorQ]:=ligDescr[[1]];
GetInvLigkABtoApB[ligDescr_?VectorQ]:=ligDescr[[2]];
GetInvLigkAbtoApb[ligDescr_?VectorQ]:=ligDescr[[3]];
(* All ligation coefficients *)
GetInvLigCoeffTable[ligDescr_?VectorQ]:=Delete[ligDescr,1];
(* ============================================== *)
(* GetLigDescriptor returns ligation descriptor for a given substance. *)
GetLigDescriptor[substAid_?IntegerQ]:=Module[{Aid,descr},
Aid=Min[substAid,EnantiomerSubstanceID[substAid]];
descr=LigDescriptorFunc[Aid];

If[!VectorQ[descr,NumericQ],
(
(* Generating new value. *)
descr=Join[{LigTotalRateValue[]},LigCoeffRandom[]];
LigDescriptorFunc[Aid]=descr;
)
];

Return[descr];
];
(* ============================================== *)
(* GetInvLigDescriptor returns inverse ligation descriptor for a given substance. *)
GetInvLigDescriptor[substAid_?IntegerQ]:=Module[{Aid,descr},
Aid=Min[substAid,EnantiomerSubstanceID[substAid]];
descr=InvLigDescriptorFunc[Aid];

If[!VectorQ[descr,NumericQ],
(
(* Generating new value. *)
descr=Join[{InvLigTotalRateValue[]},InvLigCoeffRandom[]];
InvLigDescriptorFunc[Aid]=descr;
)
];

Return[descr];
];
(* ============================================== *)
(* ============================================== *)
(* GetLigDescriptor always returns a descriptor for Min[A, E[A]] and Min[B, E[B]] *)
(* The descriptor will contain: *)
(* Total rate (sum of all 2 ligation rates). Call GetLigTotalRate[descriptor] to extract. *)
(* Normalized coefficient for the reaction A+B \[Rule] AB. Call GetLigkApBtoAB[descriptor] to extract. *)
(* Normalized coefficient for the reaction A+b \[Rule] Ab. Call GetLigkApbtoAb[descriptor] to extract. *)
(* If there is no descriptor for a given pair then it is calculated and stored. *)
(* We call ToDeactivated as we are only interested from whcih substances the pairs are formed *)
(* and not if one of them is activated. *)
(* ============================================== *)
GetLigDescriptor[substAid_?IntegerQ,substBid_?IntegerQ]:=Module[{Aid,Bid,descrA,descrB,rate,coeffA,coeffB,descr,coeff,rateA,rateB},
Aid=Min[ToDeactivated[substAid],EnantiomerSubstanceID[ToDeactivated[substAid]]];
Bid=Min[ToDeactivated[substBid],EnantiomerSubstanceID[ToDeactivated[substBid]]];

(*
(* Double check that canonical sort order of A vs. a and B vs. b coincides with enantiomeric content of A and B. *)
(* If not then we can't proceed further. *)
If[!SameChiralPolarizationQ[Aid,Bid],
(
Print["GetLigDescriptor::Invalid canonical sort order for A and B, substAid = ", substAid, ", substBid = ", substBid, ". Quitting..."];
Quit[];
)
];
*)

descrA=GetLigDescriptor[Aid];

(*
Print["    GetLigDescriptor::substAid = ", substAid, ", substBid = ", substBid, ", substAname = ", GetSubstanceName[substAid], ", substBname = ", GetSubstanceName[substBid]];
Print["    GetLigDescriptor::Aid = ", Aid, ", Bid = ", Bid, ", nameA = ", GetSubstanceName[Aid], ", nameB = ", GetSubstanceName[Bid]];
Print["    GetLigDescriptor::descrA = ", descrA // MatrixForm];
*)

If[Aid == Bid,
(
Return[descrA];
)
];

(* Attempting to get a numerical value *)
descr=LigDescriptorFunc[Aid,Bid];
(* Print["    GetLigDescriptor::descr = ", descr // MatrixForm]; *)

If[!VectorQ[descr,NumericQ],
(
(* Print["    GetLigDescriptor::Assigning new descriptor..."]; *)

descrB=GetLigDescriptor[Bid];

rateA=GetLigTotalRate[descrA];
rateB=GetLigTotalRate[descrB];
rate=LigTotalRatePair[rateA,rateB];

coeffA=GetLigCoeffTable[descrA];
coeffB=GetLigCoeffTable[descrB];
coeff=LigCoeffPair[coeffA,coeffB];

descr=Join[{rate},coeff];

(*
Print["    GetLigDescriptor::descrB = ", descrB // MatrixForm];
Print["    GetLigDescriptor::rateA = ", rateA, ", rateB = ", rateB, ", rate = ", rate];
Print["    GetLigDescriptor::coeffA = ", coeffA // MatrixForm, ", coeffB = ", coeffB // MatrixForm, ", coeff = ", coeff // MatrixForm];
Print["    GetLigDescriptor::new descr = ", descr // MatrixForm];
*)

LigDescriptorFunc[Aid,Bid]=descr;
)
];

Return[descr];
];
(* ============================================== *)
(* ============================================== *)
(* GetInvLigDescriptor always returns a canonically sorted descriptor for Min[A, E[A]] and Min[B, E[B]] *)
(* The descriptor will contain: *)
(* Total rate (sum of all 2 ligation rates). Call GetInvLigTotalRate[descriptor] to extract. *)
(* Normalized coefficient for the reaction AB \[Rule] A+B. Call GetInvLigkABtoApB[descriptor] to extract. *)
(* Normalized coefficient for the reaction Ab \[Rule] A+b. Call GetInvLigkAbtoApb[descriptor] to extract. *)
(* If there is no descriptor for a given pair then it is calculated and stored. *)
(* ============================================== *)
GetInvLigDescriptor[substAid_?IntegerQ,substBid_?IntegerQ]:=Module[{Aid,Bid,descrA,descrB,rate,coeffA,coeffB,descr,coeff,rateA,rateB},
Aid=Min[substAid,EnantiomerSubstanceID[substAid]];
Bid=Min[substBid,EnantiomerSubstanceID[substBid]];

(*
(* Double check that canonical sort order of A vs. a and B vs. b coincides with enantiomeric content of A and B. *)
(* If not then we can't proceed further. *)
If[!SameChiralPolarizationQ[Aid,Bid],
(
Print["GetInvLigDescriptor::Invalid canonical sort order for A and B, substAid = ", substAid, ", substBid = ", substBid, ". Quitting..."];
Quit[];
)
];
*)

descrA=GetInvLigDescriptor[Aid];

If[Aid == Bid,
(
Return[descrA];
)
];

(* Attempting to get a numerical value *)
descr=InvLigDescriptorFunc[Aid,Bid];

If[!VectorQ[descr,NumericQ],
(
descrB=GetInvLigDescriptor[Bid];

rateA=GetInvLigTotalRate[descrA];
rateB=GetInvLigTotalRate[descrB];
rate=InvLigTotalRatePair[rateA,rateB];

coeffA=GetInvLigCoeffTable[descrA];
coeffB=GetInvLigCoeffTable[descrB];
coeff=InvLigCoeffPair[coeffA,coeffB];

descr=Join[{rate},coeff];
InvLigDescriptorFunc[Aid,Bid]=descr;
)
];

Return[descr];
];
(* ============================================== *)
(* ============================================== *)
(* LigTotalRatePair returns total ligation rate for a pair AB based on the total rates of AA and BB *)
(* ============================================== *)
LigTotalRatePair[ligRateA_,ligRateB_]:=Module[{ligRate},
ligRate=LigTotalRatePairDet[ligRateA,ligRateB]*(1-LigTotalRateRandWeight)+LigTotalRateValue[]*LigTotalRateRandWeight;
Return[ligRate];
];
(* ============================================== *)
InvLigTotalRatePair[ligRateA_,ligRateB_]:=Module[{ligRate},
ligRate=InvLigTotalRatePairDet[ligRateA,ligRateB]*(1-InvLigTotalRateRandWeight)+InvLigTotalRateValue[]*InvLigTotalRateRandWeight;
Return[ligRate];
];
(* ============================================== *)
(* ============================================== *)
(* LigTotalRatePairDet returns deterministic part of the total ligation rate of a pair of *)
(* two diferent amino acids A and B based on the total ligation rates of A only and B only pairs. *)
(* ============================================== *)
LigTotalRatePairDet[ligRateA_,ligRateB_]:=Module[{ligRate},
ligRate=((ligRateA+ligRateB)/2)+LigTotalRateDiffWeight*((ligRateB-ligRateA)/2);
Return[ligRate];
];
(* ============================================== *)
InvLigTotalRatePairDet[ligRateA_,ligRateB_]:=Module[{ligRate},
ligRate=((ligRateA+ligRateB)/2)+InvLigTotalRateDiffWeight*((ligRateB-ligRateA)/2);
Return[ligRate];
];
(* ============================================== *)
(* ============================================== *)
(* LigCoeffPair returns weighted and normalized ligation coefficients for a pair AB *) 
(* based on the coefficients of AA and BB *)
(* ============================================== *)
LigCoeffPair[coeffA_?VectorQ,coeffB_?VectorQ]:=Module[{ligCoeff},
ligCoeff=LigCoeffPairDet[coeffA,coeffB]*(1-LigCoeffRandWeight)+LigCoeffRandom[]*LigCoeffRandWeight;
Return[ligCoeff];
];
(* ============================================== *)
InvLigCoeffPair[coeffA_?VectorQ,coeffB_?VectorQ]:=Module[{ligCoeff},
ligCoeff=InvLigCoeffPairDet[coeffA,coeffB]*(1-InvLigCoeffRandWeight)+InvLigCoeffRandom[]*InvLigCoeffRandWeight;
Return[ligCoeff];
];
(* ============================================== *)
(* ============================================== *)
(* LigCoeffPairDet returns deterministic part of weighted and normalized ligation coefficients of a pair of two diferent amino acids A and B based on the coefficients of A only and B only pairs. *)
(* ============================================== *)
LigCoeffPairDet[coeffA_?VectorQ,coeffB_?VectorQ]:=Module[{ligCoeff},
ligCoeff=((coeffA+coeffB)/2)+LigCoeffDiffWeight*((coeffB-coeffA)/2);
Return[ligCoeff];
];
(* ============================================== *)
InvLigCoeffPairDet[coeffA_?VectorQ,coeffB_?VectorQ]:=Module[{ligCoeff},
ligCoeff=((coeffA+coeffB)/2)+InvLigCoeffDiffWeight*((coeffB-coeffA)/2);
Return[ligCoeff];
];
(* ============================================== *)
(* ============================================== *)
(* LigCoeffRandom returns properly weighted and normalized random ligation coefficients. *)
(* The sum of all 2 coefficients must be equal to 1. *)
(* LigNotAAweight: varis between 0 and 1 and determines relative rates of ligation reactions. *)
(* If value \[Equal] 0 then total rate is allocated to A+A \[Rule] AA and zero to A+a \[Rule] Aa *)
(* If value \[Equal] 1 then total rate is allocated to A+a \[Rule] Aa and zero to A+A \[Rule] AA *)
(* ============================================== *)
LigCoeffRandom[]:=Module[{coeff,ii,norm,kApAtoAA,kApatoAa},
kApAtoAA=(1-LigNotAAweight)*LigCoeffValue[];
kApatoAa=LigNotAAweight*LigCoeffValue[];

coeff={kApAtoAA,kApatoAa};
norm=Sum[coeff[[ii]],{ii,1,2}];

If[norm > 0,
(
coeff=coeff/norm;
),
(
Print["LigCoeffRandom::Invalid coefficients. coeff = ", coeff // MatrixForm, ". Quitting..."];
coeff={Indeterminate,Indeterminate};
Quit[];
)
];

Return[coeff];
];
(* ============================================== *)
(* ============================================== *)
(* InvLigCoeffRandom returns properly weighted and normalized random inverse ligation coefficients. *)
(* The sum of all 2 coefficients must be equal to 1. *)
(* InvLigAAweight: varis between 0 and 1 and determines relative rates of inverse ligation reactions. *)
(* If value \[Equal] 0 then total rate is allocated to Aa \[Rule] A+a and zero to AA \[Rule] A+A *)
(* If value \[Equal] 1 then total rate is allocated to AA \[Rule] A+A and zero to Aa \[Rule] A+a *)
(* ============================================== *)
InvLigCoeffRandom[]:=Module[{coeff,ii,norm,kAAtoApA,kAatoApa},
kAAtoApA=InvLigAAweight*InvLigCoeffValue[];
kAatoApa=(1-InvLigAAweight)*LigCoeffValue[];

coeff={kAAtoApA,kAatoApa};
norm=Sum[coeff[[ii]],{ii,1,2}];

If[norm > 0,
(
coeff=coeff/norm;
),
(
Print["InvLigCoeffRandom::Invalid coefficients. coeff = ", coeff // MatrixForm, ". Quitting..."];
coeff={Indeterminate,Indeterminate};
Quit[];
)
];

Return[coeff];
];
(* ============================================== *)
(* ============================================== *)
(* LigRate return ligation rate for A+B \[Rule] AB reactions. *)
(* ============================================== *)
LigRate[substAid_?IntegerQ,substBid_?IntegerQ]:=Module[{descr,Aid,Bid,substEAid,rate,ArightID,BleftID},
(* Print["LigRate::Starting..."]; *)
descr=GetLigDescriptor[substAid,substBid];
substEAid=EnantiomerSubstanceID[substAid];

If[substAid<=substEAid,
(
Aid=substAid;
Bid=substBid;
),
(
Aid=substEAid;
Bid=EnantiomerSubstanceID[substBid];
)
];

(*
Print["LigRate::substAid = ", substAid, ", substBid = ", substBid, ", substAname = ", GetSubstanceName[substAid], ", substBname = ", GetSubstanceName[substBid]];
Print["LigRate::Aid = ", Aid, ", Bid = ", Bid, ", nameA = ", GetSubstanceName[Aid], ", nameB = ", GetSubstanceName[Bid]];
Print["LigRate::descr = ", descr // MatrixForm];
*)

If[SameChiralPolarizationQ[GetRightAminoAcid[Aid],GetLeftAminoAcid[Bid]],
(
(* We have something like A and B *)
(* Print["LigRate::We have something like A and B"]; *)
rate=GetLigTotalRate[descr]*GetLigkApBtoAB[descr];
),
(
(* We have something like A and b *)
(* Print["We have something like A and b"]; *)
rate=GetLigTotalRate[descr]*GetLigkApbtoAb[descr];
)
];

(*
Print["LigRate::rate = ",rate];
Print[strSeparatorSmall];
*)

Return[rate];
];
(* ============================================== *)
(* ============================================== *)
(* InvLigRate return ligation rate for AB \[Rule] A+B reactions. *)
(* ============================================== *)
InvLigRate[substAid_?IntegerQ,substBid_?IntegerQ]:=Module[{descr,Aid,Bid,substEAid,rate},
descr=GetInvLigDescriptor[substAid,substBid];

substEAid=EnantiomerSubstanceID[substAid];

If[substAid<=substEAid,
(
Aid=substAid;
Bid=substBid;
),
(
Aid=substEAid;
Bid=EnantiomerSubstanceID[substBid];
)
];

If[SameChiralPolarizationQ[GetRightAminoAcid[Aid],GetLeftAminoAcid[Bid]],
(
(* We have something like AB *)
rate=GetInvLigTotalRate[descr]*GetInvLigkABtoApB[descr];
),
(
(* We have something like Ab *)
rate=GetInvLigTotalRate[descr]*GetInvLigkAbtoApb[descr];
)
];

Return[rate];
];
(* ============================================== *)
(* ============================================== *)
(*
LigCoefficientValue[substAid_?IntegerQ,substBid_?IntegerQ,substABid_?IntegerQ]:=Module[{retVal,base},
base=GetChainLength[substABid];
retVal=RandomCoefficientValue[LigCoeffDistribution,LigCoeffParams,base];
Return[retVal];
];
*)
(* ============================================== *)
(*
InvLigCoefficientValue[substAid_?IntegerQ,substBid_?IntegerQ,substABid_?IntegerQ]:=Module[{retVal,base},
base=GetChainLength[substABid];
retVal=RandomCoefficientValue[InvLigCoeffDistribution,InvLigCoeffParams,base];
Return[retVal];
];
*)
(* ============================================== *)
CreateLigReactionInfo[substAid_?IntegerQ,substBid_?IntegerQ,substABid_?IntegerQ]:=Module[{reacInfo,enantSubstABid,Aid,Bid,substID},
enantSubstABid=EnantiomerSubstanceID[substABid];

(* We store reaction info only for minimum of two enantiomers *)
(* A+B are NOT sorted *)
If[substABid<= enantSubstABid,
(
Aid=substAid;
Bid=substBid;
substID=substABid;
),
(
Aid=EnantiomerSubstanceID[substAid];
Bid=EnantiomerSubstanceID[substBid];
substID=enantSubstABid;
)
];

reacInfo={Aid,Bid,substID};

Return[reacInfo];
];

(*
CreateLigReactionInfo[substAid_?IntegerQ,substBid_?IntegerQ,substABid_?IntegerQ]:=Module[{reacInfo,enantSubstABid,Aid,Bid,substID},
enantSubstABid=EnantiomerSubstanceID[substABid];

(* We store reaction info only for minimum of two enantiomers *)
(* A+B are sorted in inversed canonical order *)
If[substABid\[LessEqual] enantSubstABid,
(
Aid=Max[substAid,substBid];
Bid=Min[substAid,substBid];
substID=substABid;
),
(
Aid=Max[EnantiomerSubstanceID[substAid],EnantiomerSubstanceID[substBid]];
Bid=Min[EnantiomerSubstanceID[substAid],EnantiomerSubstanceID[substBid]];
substID=enantSubstABid;
)
];

reacInfo={Aid,Bid,substID};

Return[reacInfo];
];
*)
(* ============================================== *)
(* LigReactionRecordCheck returns true if there is a record for the reaction. *)
(* We need to be able to check if there is a reaction: A + B \[Rule] AB. *)
LigReactionRecordCheck[reacInfo_?VectorQ]:=Module[{retVal,substID},
retVal=False;
substID=GetReactionSubstAB[reacInfo];

(* Print["LigReactionRecordCheck::reacInfo = ", reacInfo, ", substID = ", substID, ", Apply[LigReacMatrix,reacInfo] = ", Apply[LigReacMatrix,reacInfo]]; *)

If[BooleanQ[Apply[LigReacMatrix,reacInfo]],retVal=True];
Return[retVal];
];
(* ============================================== *)
(* AddLigReaction adds reactions by Min[substAB, EnantiomerSubstanceID[substAB]] *)
(* if reacInfo was created by a call to CreateLigReactionInfo). *)
(* Returns True if reaction was successfully added. *)
(* Returns False if reaction is already in the list. *)
(* Uses substance IDs to simplify lookup of the data *)
AddLigReaction[reacInfo_?VectorQ]:=Module[{retVal,substID,base},
retVal=False;
substID=GetReactionSubstAB[reacInfo];

If[!LigReactionRecordCheck[reacInfo],
(
(* Reaction is NOT in the list, so we are adding it. *)
ToExpression[ToString[Apply[LigReacMatrix,reacInfo]]<>"=True"];
retVal=True;
)
];

Return[retVal];
];
(* ============================================== *)
PrintInitLigationInfo[]:=Module[{},
If[InitializeActivationValue && (!UseOnlySimpleLigationValue),
(
Print["InitializeAllLigationReactions::TODO::Only simple ligation is supported with activation / deactivation. Quitting..."]; 
Quit[];
)
];

If[InitializeLigationValue,
(
If[!SilentRunValue,Print["Initializing ligation..."]];
),
(
If[!SilentRunValue,Print["NOT initializing ligation..."]];
)
];

If[InitializeCatLigationValue,
(
If[!SilentRunValue,Print["Initializing catalytic ligation..."]];
),
(
If[!SilentRunValue,Print["NOT initializing catalytic ligation..."]];
)
];

If[!UseOnlySimpleLigationValue,
(
If[!SilentRunValue,Print["Using ligation of all possible lengths."]];
),
(
If[!SilentRunValue,Print["Using ligation of only chain + simple molecule."]];
)
];
];
(* ============================================== *)
AssignLigReaction[IsEnantiomer_?BooleanQ,IsInverse_?BooleanQ,substAid_?IntegerQ,substBid_?IntegerQ,substABid_?IntegerQ]:=AssignLigReaction[IsEnantiomer,IsInverse,substAid,substBid,substABid,1,1];

AssignLigReaction[IsEnantiomer_?BooleanQ,IsInverse_?BooleanQ,substAid_?IntegerQ,substBid_?IntegerQ,substABid_?IntegerQ,mult_,reacCountValue_]:=Module[{name,name1,reacName,substIdxName,substIdxName1,substIdxNameD,reacStringName,reacIdxName,coeffIdxName,coeffName,substAidE,substBidE,substABidE,nameE,name1E,reacNameE,retVal},

If[InitializeLigationValue,
(
name=GetSubstanceName[substAid];
name1=GetSubstanceName[substBid];
reacName=GetSubstanceName[substABid];

If[!IsEnantiomer,
(
substIdxName=IndexPrefixValue<>name;
substIdxName1=IndexPrefixValue<>name1;
substIdxNameD=IndexPrefixValue<>reacName;
),
(
(* Enantiomers *)
(* Enantiomers *)
substAidE=EnantiomerSubstanceID[substAid];
substBidE=EnantiomerSubstanceID[substBid];
substABidE=EnantiomerSubstanceID[substABid];

nameE=GetSubstanceName[substAidE];
name1E=GetSubstanceName[substBidE];
reacNameE=GetSubstanceName[substABidE];

substIdxName=IndexPrefixValue<>nameE;
substIdxName1=IndexPrefixValue<>name1E;
substIdxNameD=IndexPrefixValue<>reacNameE;
)
];

If[!IsInverse,
(
coeffIdxName=CoeffPrefixValue<>"Idx"<>name<>PlusLetter<>name1 <>ToLetter<>reacName;
coeffName=CoeffPrefixValue<>name<>PlusLetter<>name1 <>ToLetter<>reacName;

If[!IsEnantiomer,
(
(* Direct reaction *)
reacStringName=name <>" + " <> name1 <> " -> " <>reacName;
reacIdxName=ReactionPrefixValue<>name<>PlusLetter<>name1 <>ToLetter<>reacName;

ToExpression[coeffIdxName<>"=AddCoeffName["<> coeffName <>",Subscript[k,\""<>reacStringName<>"\"]]"];

(* We assign coefficients only once for a pair of enantiomers *)
If[AssignLigCoefficientsValue,
(
(* Print["Assigning value to coefficient, ", coeffName]; *)
ToExpression[coeffName <> "=LigRate[" <> ToString[substAid] <> ", " <> ToString[substBid]<> "]"];
)
];
),
(
reacStringName=nameE <>" + " <> name1E <> " -> " <>reacNameE;
reacIdxName=ReactionPrefixValue<>nameE<>PlusLetter<>name1E <>ToLetter<>reacNameE;
)
];

ToExpression[reacIdxName<>"=AddReaction[{{LigationReaction,\""<>reacStringName <>"\"},{{"<>substIdxName<>",1},{"<> substIdxName1<>",1}},{" <>ToString[mult*reacCountValue]<>" * "<> coeffName <> ",1,1},{{"<>substIdxNameD<>",1}}}]"];
),
(
coeffIdxName=CoeffPrefixValue<>"Idx"<>reacName <>ToLetter<>name<>PlusLetter<>name1;
coeffName=CoeffPrefixValue<>reacName <>ToLetter<>name<>PlusLetter<>name1;

If[!IsEnantiomer,
(
(* Inverse reaction *)
reacStringName=reacName <> " -> "<> name <>" + " <> name1 ;
reacIdxName=ReactionPrefixValue<>reacName <>ToLetter<>name<>PlusLetter<>name1;

ToExpression[coeffIdxName<>"=AddCoeffName["<> coeffName <>",Subscript[k,\""<>reacStringName<>"\"]]"];

(* We assign coefficients only once for a pair of enantiomers *)
If[AssignLigCoefficientsValue,
(
(* Print["Assigning value to coefficient, ", coeffName]; *)
ToExpression[coeffName <> "=InvLigRate[" <> ToString[substAid] <> ", " <> ToString[substBid]<> "]"];
)
];
),
(
reacStringName=reacNameE <> " -> "<> nameE <>" + " <> name1E ;
reacIdxName=ReactionPrefixValue<>reacNameE <>ToLetter<>nameE<>PlusLetter<>name1E;
)
];

ToExpression[reacIdxName<>"=AddReaction[{{InvLigationReaction,\""<>reacStringName <>"\"},{{"<>substIdxNameD<>",1}},{" <>coeffName <> ",1},{{"<>substIdxName<>",1},{"<> substIdxName1<>",1}}}]"];
)
];
)
];

retVal=ToExpression[reacIdxName];
Return[retVal];
];
(* ============================================== *)
InitializeAllLigationReactions[rawOpts___]:=Module[{chainLenCnt,len,chainLenCnt1Max,chainLenCnt1,chainLenD,ii,ii1,len1,substAid,substBid,mult,div,reacLst,reacCount,reacIdx,substABid,substAName,substBName,opts,reacInfo,maxChainLen1Limit,Aid,Bid,ABid},

opts=ProcessOptions[rawOpts];
UseOnlySimpleLigationValue=UseOnlySimpleLigation /. opts /.Options[CLMChains];

(* If UseOnlySimpleLigationValue \[Equal] True, then attachment of single molecules only is allowed. *)
maxChainLen1Limit=If[UseOnlySimpleLigationValue,1,MaxChainLength];

PrintInitLigationInfo[];
If[(!InitializeLigationValue) && (!InitializeCatLigationValue) ,Return[]];

For[chainLenCnt=1, chainLenCnt<MaxChainLength,chainLenCnt++,
(
If[!SilentRunValue,Print[strSeparatorCRLF]];
len=Length[AllChainsTbl[[chainLenCnt]]];
If[!SilentRunValue,Print["InitializeAllLigationReactions::chainLenCnt = ", chainLenCnt, ", len = ", len, ", Number of reactions (NoCnt) = ", NoCnt]];

chainLenCnt1Max=Min[chainLenCnt,MaxChainLength-chainLenCnt,maxChainLen1Limit];

For[chainLenCnt1=1, chainLenCnt1<=chainLenCnt1Max,chainLenCnt1++,
(
chainLenD=chainLenCnt+chainLenCnt1;

For[ii=1, ii<=len,ii++,
(
If[InitializeActivationValue,
(
len1=Length[AllChainsTbl[[chainLenCnt1]]];
),
(
len1=If[chainLenCnt==chainLenCnt1,ii,Length[AllChainsTbl[[chainLenCnt1]]]];
)
];

(*
Print["InitializeAllLigationReactions::chainLenCnt1 = ", chainLenCnt1, ", ii = ", ii, ", len1 = ", len1,", Length[ReactionMatrix] = ", NoCnt];
PrintTimeUsed[];
*)
For[ii1=1,ii1<=len1,ii1++,
(
(* Print["ii1 = ", ii1]; *)

If[InitializeActivationValue,
(
substAName=ToActivated[AllChainsTbl[[chainLenCnt1,ii1]]];
substBName=AllChainsTbl[[chainLenCnt,ii]];
),
(
substAName=AllChainsTbl[[chainLenCnt,ii]];
substBName=AllChainsTbl[[chainLenCnt1,ii1]];
)
];

substAid=GetSubstanceID[substAName];
substBid=GetSubstanceID[substBName];

mult=1;
div=1;

If[InitializeActivationValue,
(
(* Allowed reaction here is only A* + B \[Rule] AB *)
(* because A is a single amino acid and B can be a chain *)
(* Currently we do not support chain activation reactions. *)
reacLst={{substAid,substBid,GetSubstanceID[MergeChains[substAName,substBName]]}};
),
(
(* If A \[Equal] B, then there is only one variant: A + A \[Rule] AA *)
(* If A \[NotEqual] B, then there are two variants: A + B \[Rule] AB and B + A \[Rule] BA *)
If[substAid==substBid,
(
reacLst={{substAid,substBid,GetSubstanceID[MergeChains[substAName,substBName]]}};
),
(
reacLst={{substAid,substBid,GetSubstanceID[MergeChains[substAName,substBName]]},{substBid,substAid,GetSubstanceID[MergeChains[substBName,substAName]]}};
)
];
)
];

For[reacIdx=1,reacIdx <= Length[reacLst],reacIdx++,
(
Aid=reacLst[[reacIdx,1]];
Bid=reacLst[[reacIdx,2]];
ABid=reacLst[[reacIdx,3]];
reacInfo=CreateLigReactionInfo[Aid,Bid,ABid];

(* Print["InitializeAllLigationReactions::Aid = ", Aid, ", Bid = ", Bid, ", ABid = ", ABid, ", reacInfo = ", reacInfo]; *)

If[AddLigReaction[reacInfo],
(
(* We assign reactions only if we have not done that yet for enantiomers. *)

(* Print["InitializeAllLigationReactions::Adding reaction..."]; *)

(* Direct reaction *)
AssignLigReaction[False,False,Aid,Bid,ABid];
(* Inverse reaction *)
AssignLigReaction[False,True,ToDeactivated[Aid],ToDeactivated[Bid],ABid];
(* Enantiomers - Direct reaction *)
AssignLigReaction[True,False,Aid,Bid,ABid];
(* Enantiomers - Inverse reaction *)
AssignLigReaction[True,True,ToDeactivated[Aid],ToDeactivated[Bid],ABid];

If[((GetChainNoOfD[Aid]+GetChainNoOfD[Bid])==0) || ((GetChainNoOfL[Aid]+GetChainNoOfL[Bid])==0),
(
(* Pure L ligation reaction. *)
LLigReactionCnt[chainLenD]++;
)
];

InitializeCatLigReactions[Aid,Bid,ABid,chainLenD];
)
];
)
];
)
];
)
];
)
];

If[!SilentRunValue,
(
Print["InitializeAllLigationReactions::LLigReactionCnt = ", Table[{ii,LLigReactionCnt[ii]},{ii,1,MaxChainLength}] // MatrixForm];

Print["InitializeAllLigationReactions::LCatLigReactionCnt = ", Table[{ii,LCatLigReactionCnt[ii]},{ii,1,MaxChainLength}] // MatrixForm];

Print["InitializeAllLigationReactions::LCatLigCatalystCnt = ", Table[{ii,LCatLigCatalystCnt[ii]},{ii,1,MaxChainLength}] // MatrixForm];

PrintTimeUsed[];
)
];
)
];
If[!SilentRunValue,
(
Print["LLigReactionCnt, ..."];
Print["InitializeAllLigationReactions::Final number of reactions = ", NoCnt];
)
];
];
(* ============================================== *)


