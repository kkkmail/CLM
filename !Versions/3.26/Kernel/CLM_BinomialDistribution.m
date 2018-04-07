(* :Summary: CLM Distribution logic. *)
(* :Author: Konstantin K. Konstantinov *)
(* :Email: konstantin.k.konstantinov@gmail.com *)
(* :License type: GPL *)
(* :Copyright: K^3, 2013 - 2014 *)
(* :Version: Revision: 2.05.001, Date: 2014/09/10 *)
(* :Mathematica Version: 7.0 - 10.0 *)
(* ============================================== *)
(* Functions, which return parts of the chain descriptor *)
GetN[chainDescr_?VectorQ]:=chainDescr[[1]];
GetStart[chainDescr_?VectorQ]:=chainDescr[[2]];
GetEnd[chainDescr_?VectorQ]:=chainDescr[[3]];
GetKl[chainDescr_?VectorQ]:=chainDescr[[4]];
GetProb[chainDescr_?VectorQ]:=chainDescr[[5]];
(* ============================================== *)
CreateDescriptor[nn_,start_,end_,kL_,prob_]:={nn,start,end,kL,Simplify[prob]};
(* CreateDescriptor[nn_,start_,end_,kL_,prob_]:={nn,start,end,kL,prob}; *)
(* ============================================== *)
CanAddDescriptors[chainDescr1_?VectorQ,chainDescr2_?VectorQ]:=If[GetN[chainDescr1] ==  GetN[chainDescr2]&& GetStart[chainDescr1] ==  GetStart[chainDescr2]&&GetEnd[chainDescr1] ==  GetEnd[chainDescr2]&&GetKl[chainDescr1] ==  GetKl[chainDescr2] ,True,False,False];
(* ============================================== *)
AddDescriptors[chainDescr1_?VectorQ,chainDescr2_?VectorQ]:=Module[{retVal},
If[!CanAddDescriptors[chainDescr1,chainDescr2],
(
Print["AddDescriptors::Cannot add descriptors: ", chainDescr1 ," and ",chainDescr2];
Return[Indeterminate];
)
];
retVal=CreateDescriptor[GetN[chainDescr1],GetStart[chainDescr1],GetEnd[chainDescr1],GetKl[chainDescr1],GetProb[chainDescr1] +GetProb[chainDescr2]];
Return[retVal];
];
(* ============================================== *)
CanMergeDescriptors[chainDescr1_?VectorQ,chainDescr2_?VectorQ]:=If[GetN[chainDescr1] ==  GetN[chainDescr2]&&GetKl[chainDescr1] ==  GetKl[chainDescr2] ,True,False,False];
(* ============================================== *)
MergeDescriptors[chainDescr1_?VectorQ,chainDescr2_?VectorQ]:=Module[{retVal},
If[!CanMergeDescriptors[chainDescr1,chainDescr2],
(
Print["AddDescriptors::Cannot add descriptors: ", chainDescr1 ," and ",chainDescr2];
Return[Indeterminate];
)
];
retVal=CreateDescriptor[GetN[chainDescr1],Indeterminate,Indeterminate,GetKl[chainDescr1],GetProb[chainDescr1] +GetProb[chainDescr2]];
Return[retVal];
];
(* ============================================== *)
(* Returns True if element is L *)
IsL[elem_?BooleanQ]:=elem;
LValue=True;
DValue=False;
(* ============================================== *)
AddElement[chainDescr_?VectorQ,p_,q_]:=Module[{retVal,nn,start,end,kL,prob,pL,pD,\[Epsilon],pbLL,pbLD,pnbL,pbLLtot,pbLDtot,pnbD,pbDL,pbDD,pbDLtot,pbDDtot,LLdescr,LDdescr,LDdescr1,DDdescr},
nn=GetN[chainDescr];
start=GetStart[chainDescr];
end=GetEnd[chainDescr];
prob=GetProb[chainDescr];

If[start== DValue && end == LValue,
(
start=LValue;
end=DValue;
)
];

kL=GetKl[chainDescr];

pL=p;
pD=1-pL;

(*
Print["Probability to find L: ",pL];
Print["Probability to find D: ", pD];
*)

(* Print["Probability to bind L with L: \[Epsilon]*q"]; *)
pbLL=\[Epsilon]*q*pL;

(* Print["Probability to bind L with D: \[Epsilon]*(1-q)"]; *)
pbLD=\[Epsilon]*(1-q)*pD;

(* Print["Probability that L is not bound to anything after 1 encounter: pnbL"]; *)
pnbL=FullSimplify[1-pbLL-pbLD];
(* Print["pnbL = ", Simplify[pnbL]]; *)

(* Print["Total probability to have L bound with L"]; *)
pbLLtot=FullSimplify[Sum[pbLL*pnbL^nn,{nn,0,Infinity}]];
(* Print["pbLLtot = ", pbLLtot]; *)

(* Print["Total probability to have L bound with D"]; *)
pbLDtot=FullSimplify[Sum[pbLD*pnbL^nn,{nn,0,Infinity}]];
(* Print["pbLDtot = ", pbLDtot]; *)

(*
Print["Check: ", Simplify[pbLLtot+pbLDtot]];
Print[sep];
*)
(* ============================================== *)
(* Print["Probability to bind D with L: \[Epsilon]*(1-q)"]; *)
pbDL=\[Epsilon]*(1-q)*pL;

(* Print["Probability to bind D with D: \[Epsilon]*q"]; *)
pbDD=\[Epsilon]*q*pD;

(* Print["Probability that D is not bound to anything after 1 encounter: pnbD"]; *)
pnbD=FullSimplify[1-pbDL-pbDD];
(* Print["pnbD = ", Simplify[pnbD]]; *)

(* Print["Total probability to have D bound with L"]; *)
pbDLtot=FullSimplify[Sum[pbDL*pnbD^nn,{nn,0,Infinity}]];
(* Print["pbDLtot = ", pbDLtot]; *)

(* Print["Total probability to have D bound with D"]; *)
pbDDtot=FullSimplify[Sum[pbDD*pnbD^nn,{nn,0,Infinity}]];
(* Print["pbDDtot = ", pbDDtot]; *)

(*
Print["Check: ", Simplify[pbDLtot+pbDDtot]];
Print[sep];
*)

If[start==LValue && end==LValue,
(
LLdescr=CreateDescriptor[nn+1,LValue,LValue,kL+1,prob*pbLLtot];
LDdescr=CreateDescriptor[nn+1,LValue,DValue,kL,prob*pbLDtot];
retVal={LLdescr,LDdescr};
)
];

If[start==DValue && end==DValue,
(
LDdescr=CreateDescriptor[nn+1,LValue,DValue,kL+1,prob*pbDLtot];
DDdescr=CreateDescriptor[nn+1,DValue,DValue,kL,prob*pbDDtot];
retVal={LDdescr,DDdescr};
)
];

If[start==LValue && end==DValue,
(
LLdescr=CreateDescriptor[nn+1,LValue,LValue,kL+1,prob*(1/2)*pbDLtot];
LDdescr=CreateDescriptor[nn+1,LValue,DValue,kL+1,prob*(1/2)*pbLLtot];
LDdescr1=CreateDescriptor[nn+1,LValue,DValue,kL,prob*(1/2)*pbDDtot];
DDdescr=CreateDescriptor[nn+1,DValue,DValue,kL,prob*(1/2)*pbLDtot];
retVal={LLdescr,LDdescr,LDdescr1,DDdescr};
)
];

(* Print["AddElement::retVal = ", retVal]; *)
Return[retVal];
];
(* ============================================== *)
chainCnt=1;
IncreaseChain[chain_?MatrixQ,p_,q_]:=Module[{retVal,len,ii,tbl},
chainCnt++;
Print[" " <> FromCharacterCode[10] <> " " <> FromCharacterCode[10]];
Print["IncreaseChain::chainCnt = ", chainCnt];
len=Length[chain];
PrintTimeUsed[];
tbl=ParallelTable[AddElement[chain[[ii]],p,q],{ii,1,len}];
PrintTimeUsed[];
tbl=Flatten[tbl,1];
(* Print["IncreaseChain::tbl = ", tbl // MatrixForm]; *)

retVal=FlattenChain[tbl];
PrintTimeUsed[];
Return[retVal];
];
(* ============================================== *)
FlattenChain[chainVar_?MatrixQ]:=Module[{retVal,len,ii,idx,elem,chain},
chain=chainVar;
len=Length[chain];
retVal={};
idx=1;


For[idx = 1,idx <=len,idx++,
(
elem=chain[[idx]];
For[ii=0,ii<= len-idx-1,ii++,
(
If[CanAddDescriptors[elem,chain[[len-ii]]],
(
elem=AddDescriptors[elem,chain[[len-ii]]];
chain=Delete[chain,len-ii];
)
];
)
];

len=Length[chain];
retVal=Join[retVal,{elem}];
)
];

(* Print["FlattenChain::retVal = ", retVal // MatrixForm]; *)
Return[retVal];
];
(* ============================================== *)
MergeChain[chainVar_?MatrixQ]:=Module[{retVal,len,ii,idx,elem,chain},
chain=chainVar;
len=Length[chain];
retVal={};
idx=1;


For[idx = 1,idx <=len,idx++,
(
elem=chain[[idx]];
For[ii=0,ii<= len-idx-1,ii++,
(
If[CanMergeDescriptors[elem,chain[[len-ii]]],
(
elem=MergeDescriptors[elem,chain[[len-ii]]];
chain=Delete[chain,len-ii];
)
];
)
];

len=Length[chain];
retVal=Join[retVal,{elem[[5]]}];
)
];

Print["MergeChain::Check = ",Simplify[Sum[retVal[[ii]],{ii,1,Length[retVal]}]]];
(* Print["MergeChain::retVal = ", retVal // MatrixForm];*)
Return[retVal];
];
(* ============================================== *)
BuildDistribution[nnMax_?IntegerQ,p_,q_]:=Module[{distr,Ldescr,Ddescr,chain,ii,nn,sum},
Ldescr=CreateDescriptor[1,LValue,LValue,1,p];
Ddescr=CreateDescriptor[1,DValue,DValue,0,1-p];
chain={Ldescr,Ddescr};
distr={chain};
For[ii=1,ii < nnMax,ii++,
(
chain=IncreaseChain[chain,p,q];

nn=Length[chain];
sum=Simplify[Sum[(nn-ii)*GetProb[chain[[ii]]],{ii,1,nn}]];
Print["BuildDistribution::sum = ", sum];

distr=Join[distr,{chain}];
)
];

(* Print["BuildDistribution::distr = ", distr // MatrixForm]; *)
Return[distr];
];
(* ============================================== *)
