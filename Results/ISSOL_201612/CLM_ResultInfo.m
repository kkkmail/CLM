(* ============================================== *)
(* :Summary: CLM Result information. *)
(* ============================================== *)
(* :Author: Konstantin K. Konstantinov *)
(* :Email: konstantin.k.konstantinov@gmail.com *)
(* :License type: GPL v3 or any later version, see http://www.gnu.org/licenses/ *)
(* :Copyright: K^3, 2013 - 2016 *)
(* :Version: Revision: 3.24.001, Date: 2016/10/15 *)
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
(* Change resultID to plot one of the results from resultLst. *)
(* Change resultLst to have the names of the result files. *)
resultLst={"CLM_RD__ER_NA_g010pm_r100_Lm100.m","CLM_RD__ER_NA_g010pm_r100_Lm030.m", "CLM_RD__ER_A_g010pm_r126_Lm100.m", "CLM_RD__ER_A_g010pm_r117_Lm044.m"};

resultID=1;
(* ============================================== *)
(* Change PathList and PathListData to point to the location of the source code and the data files. *)
PathList={"W:\\Math\\CLM\\"};
Get["CLM_Common.m",Path-> PathList];
(* ============================================== *)
PathListData={"W:\\Math\\CLM\\Data\\"};
Get[resultLst[[resultID]],Path-> PathListData];
(* ============================================== *)
(*
BDIMAGESIZE=2560 ;
BDFONTSIZE=18;
*)

BDIMAGESIZE=(3/2) * 2560 ;
BDFONTSIZE=42;

BDFONTFAMILY="CourierNew";
BDFONTWEIGHT="Bold";
(* BDPLTTEXTOPTS={FontFamily->BDFONTFAMILY,FontSize->BDFONTSIZE,FontWeight->BDFONTWEIGHT}; *)
BDPLTTEXTOPTS={FontFamily->BDFONTFAMILY,FontSize->BDFONTSIZE,FontWeight->"Bold"};
(* ============================================== *)
thresholdLst={0.1,0.4,0.6,0.8};
(* ============================================== *)
(* CLM$Subst$5$Unsorted is an unsorted list of substances of length up to 5 as produced by CLM evolution runs. *)
(* CLM$Pairs$5$Unsorted is an unsorted list of pairs of substances of length up to 5 as produced by CLM evolution runs. *)
(* ============================================== *)
(* SortOrderType are as used in Excel file CLM_ER_Template_323__007.xlsx or above *)
Print["TODO::CLM_ResultInfo::Rename and describe sort order types."];
(* From sheet CmpFilter *)
SortOrderPairType3=3;

(* From sheet CmpTr *)
SortOrderSubstType4=4;
(* ============================================== *)
(* SubstValue returns the numeric value (sort order) for a given substance string *)
SubstValue[strSubst_?StringQ, leftGreaterThanRight_?BooleanQ]:=SubstValue[strSubst, leftGreaterThanRight, False];
SubstValue[strSubst_?StringQ, leftGreaterThanRight_?BooleanQ,useTwoOne_?BooleanQ]:=Module[{substVal, s},
If[useTwoOne,
(
s=If[leftGreaterThanRight,StringReplace[strSubst,{"A"-> "2", "a" -> "1"}],StringReplace[strSubst,{"A"-> "1", "a" -> "2"}]];
),
(
s=If[leftGreaterThanRight,StringReplace[strSubst,{"A"-> "1", "a" -> "0"}],StringReplace[strSubst,{"A"-> "0", "a" -> "1"}]];
)
];

substVal=ToExpression[s];
Return[substVal];
];
(* ============================================== *)
SortOrderSubstValue[strSubst_?StringQ,sortOrderTypeID_?IntegerQ]:=Module[{sortOrder,soSubst,no$A,no$a,len},
len=StringLength[strSubst];
no$A = StringCount[strSubst,"A"];
no$a= StringCount[strSubst,"a"];

If[sortOrderTypeID==SortOrderSubstType4,
(
soSubst=SubstValue[strSubst,False];
sortOrder=(len*10+no$a+soSubst/10^5)*10^5;
),
(
Print["CLM_ResultInfo::SortOrderPairValue::Unsupported sortOrderTypeID = ", sortOrderTypeID];
Return[Indeterminate];
)
];

Return[sortOrder];
];
(* ============================================== *)
PairInfo$First$Len[pi_?VectorQ]:=pi[[1]];
PairInfo$First$A[pi_?VectorQ]:=pi[[2]];
PairInfo$First$a[pi_?VectorQ]:=pi[[3]];
PairInfo$First$SO[pi_?VectorQ]:=pi[[4]];
PairInfo$Second$Len[pi_?VectorQ]:=pi[[5]];
PairInfo$Second$A[pi_?VectorQ]:=pi[[6]];
PairInfo$Second$a[pi_?VectorQ]:=pi[[7]];
PairInfo$Second$SO[pi_?VectorQ]:=pi[[8]];
(* ============================================== *)
(* PairInfo returns information for a given pair string *)
PairInfo[strPair_?StringQ, leftGreaterThanRight_?BooleanQ,useTwoOne_?BooleanQ]:=PairInfo[strPair, leftGreaterThanRight,False];
PairInfo[strPair_?StringQ, leftGreaterThanRight_?BooleanQ,useTwoOne_?BooleanQ]:=Module[{pairInfo,posLst,first,second,soFirst,soSecond,lenFirst, lenSecond,first$A,first$a, sec$A, sec$a},
posLst=StringPosition[strPair,"+",1, IgnoreCase->False];

first=StringTake[strPair, posLst[[1,1]]-2];
second=StringTake[strPair, {posLst[[1,1]]+2, StringLength[strPair]}];

lenFirst=StringLength[first];
lenSecond=StringLength[second];

first$A = StringCount[first,"A"];
first$a= StringCount[first,"a"];
sec$A= StringCount[second,"A"];
sec$a= StringCount[second,"a"];

soFirst=SubstValue[first,leftGreaterThanRight,useTwoOne];
soSecond=SubstValue[second,leftGreaterThanRight,useTwoOne];

pairInfo={lenFirst,first$A,first$a,soFirst,lenSecond,sec$A,sec$a,soSecond};
Return[pairInfo];
];
(* ============================================== *)
(* SortOrderPairValue returns the numeric value (sort order) for a given pair string *)
SortOrderPairValue[strPair_?StringQ, sortOrderTypeID_?IntegerQ]:=Module[{pi,sortOrder,first$Len,first$A,first$a,first$so,second$Len,sec$A,sec$a,second$so},
pi=PairInfo[strPair,True];

first$Len=PairInfo$First$Len[pi];
first$A =PairInfo$First$A[pi];
first$a= PairInfo$First$a[pi];
first$so=PairInfo$First$SO[pi];

second$Len=PairInfo$Second$Len[pi];
sec$A= PairInfo$Second$A[pi];
sec$a=PairInfo$Second$a[pi];
second$so=PairInfo$Second$SO[pi];

If[sortOrderTypeID==SortOrderPairType3,
(
sortOrder=(Max[first$Len,second$Len]*2*10^9+(first$Len+second$Len)*10^8+(9-first$a)*10^7+(9-sec$a)*10^6+first$so+second$so/10^5)*10^5;
),
(
Print["CLM_ResultInfo::SortOrderPairValue::Unsupported sortOrderTypeID = ", sortOrderTypeID];
Return[Indeterminate];
)
];

Return[sortOrder];
];
(* ============================================== *)
SubstNameLst={};
SubstPairNameLst={};
SortedSubstNameLst={};

SubstNameLen=0;
PairNameLen=0;

SubstIdLst={};
EnantSubstIdLst={};
PairIdLst={};
EnantPairIdLst={};

SubstSortOrder={};
PairSortOrder={};

cnAllSorted={};
cpmAllSorted={};
(* ============================================== *)
InitializeResultInfo[strSubstLst_?VectorQ, strPairLst_?VectorQ]:=InitializeResultInfo[strSubstLst, strPairLst, SortOrderSubstType4,SortOrderPairType3];
InitializeResultInfo[strSubstLst_?VectorQ, strPairLst_?VectorQ, sortOrderSubstTypeID_?IntegerQ,sortOrderTypeID_?IntegerQ]:=Module[{ii},
SubstNameLst=strSubstLst;
SubstPairNameLst=strPairLst;

SubstNameLen=Length[strSubstLst];
PairNameLen=Length[strPairLst];

SubstIdLst=Table[ SubstValue[strSubstLst[[ii]], True, True],{ii,1,SubstNameLen}];
EnantSubstIdLst=Table[ SubstValue[strSubstLst[[ii]], False, True],{ii,1,SubstNameLen}];

PairIdLst=Table[{PairInfo$First$SO[PairInfo[strPairLst[[ii]],  True, True]],PairInfo$Second$SO[PairInfo[strPairLst[[ii]],  True, True]]},{ii,1,PairNameLen}];

EnantPairIdLst=Table[{PairInfo$First$SO[PairInfo[strPairLst[[ii]],  False, True]],PairInfo$Second$SO[PairInfo[strPairLst[[ii]],  False, True]]},{ii,1,PairNameLen}];

SubstSortOrder=Table[SortOrderSubstValue[strSubstLst[[ii]], sortOrderSubstTypeID],{ii,1,SubstNameLen}];
PairSortOrder=Table[SortOrderPairValue[strPairLst[[ii]], sortOrderTypeID],{ii,1,PairNameLen}];

SortedSubstNameLst=SortByFirst[SubstSortOrder,SubstNameLst];

(*
Print["InitializeResultInfo::SubstIdLst = ", SubstIdLst // MatrixForm];
Print["InitializeResultInfo::PairIdLst = ", PairIdLst // MatrixForm];

Print["InitializeResultInfo::SubstSortOrder = ", SubstSortOrder // MatrixForm];
Print["InitializeResultInfo::PairSortOrder = ", PairSortOrder // MatrixForm];
*)

SortData[];
];
(* ============================================== *)
(* GetPairID returns is (number) of the pair based on two numeric IDs of the substances *)
GetPairID[firstID_?IntegerQ,secondID_?IntegerQ]:=Module[{pairID,pos},
pos=Position[PairIdLst,{firstID,secondID}];

If[Length[pos]>0,
(
pairID=pos[[1,1]];
),
(
pos=Position[PairIdLst,{secondID,firstID}];
pairID=If[Length[pos]>0,pos[[1,1]],Indeterminate];
)
];

If[pairID===Indeterminate,
(
(*
Print["..."];
Print["GetPairID::firstID = ", firstID, ", secondID = ", secondID, ", pos = ", pos, ", Length[pos] = ", Length[pos], ", pairID = ", pairID];
*)
pos=Position[EnantPairIdLst,{firstID,secondID}];

If[Length[pos]>0,
(
pairID=pos[[1,1]];
),
(
pos=Position[EnantPairIdLst,{secondID,firstID}];
pairID=If[Length[pos]>0,pos[[1,1]],Indeterminate];
)
];
 (*
Print["GetPairID::Enant::firstID = ", firstID, ", secondID = ", secondID, ", pos = ", pos, ", Length[pos] = ", Length[pos], ", pairID = ", pairID];
*)
)
];
(*
Print["GetPairID::Final::firstID = ", firstID, ", secondID = ", secondID, ", pos = ", pos, ", Length[pos] = ", Length[pos], ", pairID = ", pairID];
*)

Return[pairID];
];
(* ============================================== *)
SortXYData[data_]:=Transpose[SortByFirst[SubstSortOrder,Transpose[SortByFirst[SubstSortOrder,data]]]];
(* ============================================== *)
SortData[]:=Module[{},
cnAllSorted=SortByFirst[PairSortOrder,cnAll];
cpmAllSorted=Transpose[SortByFirst[SubstSortOrder,Transpose[SortByFirst[PairSortOrder,cpmAll]]]];
Return[];
];
(* ============================================== *)
(* ============================================== *)
CLM$Subst$5$Unsorted={"A","AA","Aa","AAA","AAa","AaA","Aaa","AAAA","AAAa","AAaA","AAaa","AaAA","AaAa","AaaA","Aaaa","AAAAA","AAAAa","AAAaA","AAAaa","AAaAA","AAaAa","AAaaA","AAaaa","AaAAA","AaAAa","AaAaA","AaAaa","AaaAA","AaaAa","AaaaA","Aaaaa","a","aa","aA","aaa","aaA","aAa","aAA","aaaa","aaaA","aaAa","aaAA","aAaa","aAaA","aAAa","aAAA","aaaaa","aaaaA","aaaAa","aaaAA","aaAaa","aaAaA","aaAAa","aaAAA","aAaaa","aAaaA","aAaAa","aAaAA","aAAaa","aAAaA","aAAAa","aAAAA"};
(* ============================================== *)
CLM$Pairs$5$Unsorted={"A + A","A + AA","AA + AA","A + Aa","AA + Aa","Aa + Aa","A + AAA","AA + AAA","Aa + AAA","AAA + AAA","A + AAa","AA + AAa","Aa + AAa","AAA + AAa","AAa + AAa","A + AaA","AA + AaA","Aa + AaA","AAA + AaA","AAa + AaA","AaA + AaA","A + Aaa","AA + Aaa","Aa + Aaa","AAA + Aaa","AAa + Aaa","AaA + Aaa","Aaa + Aaa","A + AAAA","AA + AAAA","Aa + AAAA","AAA + AAAA","AAa + AAAA","AaA + AAAA","Aaa + AAAA","AAAA + AAAA","A + AAAa","AA + AAAa","Aa + AAAa","AAA + AAAa","AAa + AAAa","AaA + AAAa","Aaa + AAAa","AAAA + AAAa","AAAa + AAAa","A + AAaA","AA + AAaA","Aa + AAaA","AAA + AAaA","AAa + AAaA","AaA + AAaA","Aaa + AAaA","AAAA + AAaA","AAAa + AAaA","AAaA + AAaA","A + AAaa","AA + AAaa","Aa + AAaa","AAA + AAaa","AAa + AAaa","AaA + AAaa","Aaa + AAaa","AAAA + AAaa","AAAa + AAaa","AAaA + AAaa","AAaa + AAaa","A + AaAA","AA + AaAA","Aa + AaAA","AAA + AaAA","AAa + AaAA","AaA + AaAA","Aaa + AaAA","AAAA + AaAA","AAAa + AaAA","AAaA + AaAA","AAaa + AaAA","AaAA + AaAA","A + AaAa","AA + AaAa","Aa + AaAa","AAA + AaAa","AAa + AaAa","AaA + AaAa","Aaa + AaAa","AAAA + AaAa","AAAa + AaAa","AAaA + AaAa","AAaa + AaAa","AaAA + AaAa","AaAa + AaAa","A + AaaA","AA + AaaA","Aa + AaaA","AAA + AaaA","AAa + AaaA","AaA + AaaA","Aaa + AaaA","AAAA + AaaA","AAAa + AaaA","AAaA + AaaA","AAaa + AaaA","AaAA + AaaA","AaAa + AaaA","AaaA + AaaA","A + Aaaa","AA + Aaaa","Aa + Aaaa","AAA + Aaaa","AAa + Aaaa","AaA + Aaaa","Aaa + Aaaa","AAAA + Aaaa","AAAa + Aaaa","AAaA + Aaaa","AAaa + Aaaa","AaAA + Aaaa","AaAa + Aaaa","AaaA + Aaaa","Aaaa + Aaaa","A + AAAAA","AA + AAAAA","Aa + AAAAA","AAA + AAAAA","AAa + AAAAA","AaA + AAAAA","Aaa + AAAAA","AAAA + AAAAA","AAAa + AAAAA","AAaA + AAAAA","AAaa + AAAAA","AaAA + AAAAA","AaAa + AAAAA","AaaA + AAAAA","Aaaa + AAAAA","AAAAA + AAAAA","A + AAAAa","AA + AAAAa","Aa + AAAAa","AAA + AAAAa","AAa + AAAAa","AaA + AAAAa","Aaa + AAAAa","AAAA + AAAAa","AAAa + AAAAa","AAaA + AAAAa","AAaa + AAAAa","AaAA + AAAAa","AaAa + AAAAa","AaaA + AAAAa","Aaaa + AAAAa","AAAAA + AAAAa","AAAAa + AAAAa","A + AAAaA","AA + AAAaA","Aa + AAAaA","AAA + AAAaA","AAa + AAAaA","AaA + AAAaA","Aaa + AAAaA","AAAA + AAAaA","AAAa + AAAaA","AAaA + AAAaA","AAaa + AAAaA","AaAA + AAAaA","AaAa + AAAaA","AaaA + AAAaA","Aaaa + AAAaA","AAAAA + AAAaA","AAAAa + AAAaA","AAAaA + AAAaA","A + AAAaa","AA + AAAaa","Aa + AAAaa","AAA + AAAaa","AAa + AAAaa","AaA + AAAaa","Aaa + AAAaa","AAAA + AAAaa","AAAa + AAAaa","AAaA + AAAaa","AAaa + AAAaa","AaAA + AAAaa","AaAa + AAAaa","AaaA + AAAaa","Aaaa + AAAaa","AAAAA + AAAaa","AAAAa + AAAaa","AAAaA + AAAaa","AAAaa + AAAaa","A + AAaAA","AA + AAaAA","Aa + AAaAA","AAA + AAaAA","AAa + AAaAA","AaA + AAaAA","Aaa + AAaAA","AAAA + AAaAA","AAAa + AAaAA","AAaA + AAaAA","AAaa + AAaAA","AaAA + AAaAA","AaAa + AAaAA","AaaA + AAaAA","Aaaa + AAaAA","AAAAA + AAaAA","AAAAa + AAaAA","AAAaA + AAaAA","AAAaa + AAaAA","AAaAA + AAaAA","A + AAaAa","AA + AAaAa","Aa + AAaAa","AAA + AAaAa","AAa + AAaAa","AaA + AAaAa","Aaa + AAaAa","AAAA + AAaAa","AAAa + AAaAa","AAaA + AAaAa","AAaa + AAaAa","AaAA + AAaAa","AaAa + AAaAa","AaaA + AAaAa","Aaaa + AAaAa","AAAAA + AAaAa","AAAAa + AAaAa","AAAaA + AAaAa","AAAaa + AAaAa","AAaAA + AAaAa","AAaAa + AAaAa","A + AAaaA","AA + AAaaA","Aa + AAaaA","AAA + AAaaA","AAa + AAaaA","AaA + AAaaA","Aaa + AAaaA","AAAA + AAaaA","AAAa + AAaaA","AAaA + AAaaA","AAaa + AAaaA","AaAA + AAaaA","AaAa + AAaaA","AaaA + AAaaA","Aaaa + AAaaA","AAAAA + AAaaA","AAAAa + AAaaA","AAAaA + AAaaA","AAAaa + AAaaA","AAaAA + AAaaA","AAaAa + AAaaA","AAaaA + AAaaA","A + AAaaa","AA + AAaaa","Aa + AAaaa","AAA + AAaaa","AAa + AAaaa","AaA + AAaaa","Aaa + AAaaa","AAAA + AAaaa","AAAa + AAaaa","AAaA + AAaaa","AAaa + AAaaa","AaAA + AAaaa","AaAa + AAaaa","AaaA + AAaaa","Aaaa + AAaaa","AAAAA + AAaaa","AAAAa + AAaaa","AAAaA + AAaaa","AAAaa + AAaaa","AAaAA + AAaaa","AAaAa + AAaaa","AAaaA + AAaaa","AAaaa + AAaaa","A + AaAAA","AA + AaAAA","Aa + AaAAA","AAA + AaAAA","AAa + AaAAA","AaA + AaAAA","Aaa + AaAAA","AAAA + AaAAA","AAAa + AaAAA","AAaA + AaAAA","AAaa + AaAAA","AaAA + AaAAA","AaAa + AaAAA","AaaA + AaAAA","Aaaa + AaAAA","AAAAA + AaAAA","AAAAa + AaAAA","AAAaA + AaAAA","AAAaa + AaAAA","AAaAA + AaAAA","AAaAa + AaAAA","AAaaA + AaAAA","AAaaa + AaAAA","AaAAA + AaAAA","A + AaAAa","AA + AaAAa","Aa + AaAAa","AAA + AaAAa","AAa + AaAAa","AaA + AaAAa","Aaa + AaAAa","AAAA + AaAAa","AAAa + AaAAa","AAaA + AaAAa","AAaa + AaAAa","AaAA + AaAAa","AaAa + AaAAa","AaaA + AaAAa","Aaaa + AaAAa","AAAAA + AaAAa","AAAAa + AaAAa","AAAaA + AaAAa","AAAaa + AaAAa","AAaAA + AaAAa","AAaAa + AaAAa","AAaaA + AaAAa","AAaaa + AaAAa","AaAAA + AaAAa","AaAAa + AaAAa","A + AaAaA","AA + AaAaA","Aa + AaAaA","AAA + AaAaA","AAa + AaAaA","AaA + AaAaA","Aaa + AaAaA","AAAA + AaAaA","AAAa + AaAaA","AAaA + AaAaA","AAaa + AaAaA","AaAA + AaAaA","AaAa + AaAaA","AaaA + AaAaA","Aaaa + AaAaA","AAAAA + AaAaA","AAAAa + AaAaA","AAAaA + AaAaA","AAAaa + AaAaA","AAaAA + AaAaA","AAaAa + AaAaA","AAaaA + AaAaA","AAaaa + AaAaA","AaAAA + AaAaA","AaAAa + AaAaA","AaAaA + AaAaA","A + AaAaa","AA + AaAaa","Aa + AaAaa","AAA + AaAaa","AAa + AaAaa","AaA + AaAaa","Aaa + AaAaa","AAAA + AaAaa","AAAa + AaAaa","AAaA + AaAaa","AAaa + AaAaa","AaAA + AaAaa","AaAa + AaAaa","AaaA + AaAaa","Aaaa + AaAaa","AAAAA + AaAaa","AAAAa + AaAaa","AAAaA + AaAaa","AAAaa + AaAaa","AAaAA + AaAaa","AAaAa + AaAaa","AAaaA + AaAaa","AAaaa + AaAaa","AaAAA + AaAaa","AaAAa + AaAaa","AaAaA + AaAaa","AaAaa + AaAaa","A + AaaAA","AA + AaaAA","Aa + AaaAA","AAA + AaaAA","AAa + AaaAA","AaA + AaaAA","Aaa + AaaAA","AAAA + AaaAA","AAAa + AaaAA","AAaA + AaaAA","AAaa + AaaAA","AaAA + AaaAA","AaAa + AaaAA","AaaA + AaaAA","Aaaa + AaaAA","AAAAA + AaaAA","AAAAa + AaaAA","AAAaA + AaaAA","AAAaa + AaaAA","AAaAA + AaaAA","AAaAa + AaaAA","AAaaA + AaaAA","AAaaa + AaaAA","AaAAA + AaaAA","AaAAa + AaaAA","AaAaA + AaaAA","AaAaa + AaaAA","AaaAA + AaaAA","A + AaaAa","AA + AaaAa","Aa + AaaAa","AAA + AaaAa","AAa + AaaAa","AaA + AaaAa","Aaa + AaaAa","AAAA + AaaAa","AAAa + AaaAa","AAaA + AaaAa","AAaa + AaaAa","AaAA + AaaAa","AaAa + AaaAa","AaaA + AaaAa","Aaaa + AaaAa","AAAAA + AaaAa","AAAAa + AaaAa","AAAaA + AaaAa","AAAaa + AaaAa","AAaAA + AaaAa","AAaAa + AaaAa","AAaaA + AaaAa","AAaaa + AaaAa","AaAAA + AaaAa","AaAAa + AaaAa","AaAaA + AaaAa","AaAaa + AaaAa","AaaAA + AaaAa","AaaAa + AaaAa","A + AaaaA","AA + AaaaA","Aa + AaaaA","AAA + AaaaA","AAa + AaaaA","AaA + AaaaA","Aaa + AaaaA","AAAA + AaaaA","AAAa + AaaaA","AAaA + AaaaA","AAaa + AaaaA","AaAA + AaaaA","AaAa + AaaaA","AaaA + AaaaA","Aaaa + AaaaA","AAAAA + AaaaA","AAAAa + AaaaA","AAAaA + AaaaA","AAAaa + AaaaA","AAaAA + AaaaA","AAaAa + AaaaA","AAaaA + AaaaA","AAaaa + AaaaA","AaAAA + AaaaA","AaAAa + AaaaA","AaAaA + AaaaA","AaAaa + AaaaA","AaaAA + AaaaA","AaaAa + AaaaA","AaaaA + AaaaA","A + Aaaaa","AA + Aaaaa","Aa + Aaaaa","AAA + Aaaaa","AAa + Aaaaa","AaA + Aaaaa","Aaa + Aaaaa","AAAA + Aaaaa","AAAa + Aaaaa","AAaA + Aaaaa","AAaa + Aaaaa","AaAA + Aaaaa","AaAa + Aaaaa","AaaA + Aaaaa","Aaaa + Aaaaa","AAAAA + Aaaaa","AAAAa + Aaaaa","AAAaA + Aaaaa","AAAaa + Aaaaa","AAaAA + Aaaaa","AAaAa + Aaaaa","AAaaA + Aaaaa","AAaaa + Aaaaa","AaAAA + Aaaaa","AaAAa + Aaaaa","AaAaA + Aaaaa","AaAaa + Aaaaa","AaaAA + Aaaaa","AaaAa + Aaaaa","AaaaA + Aaaaa","Aaaaa + Aaaaa","A + a","A + aa","AA + aa","A + aA","AA + aA","Aa + aA","A + aaa","AA + aaa","Aa + aaa","AAA + aaa","A + aaA","AA + aaA","Aa + aaA","AAA + aaA","AAa + aaA","A + aAa","AA + aAa","Aa + aAa","AAA + aAa","AAa + aAa","AaA + aAa","A + aAA","AA + aAA","Aa + aAA","AAA + aAA","AAa + aAA","AaA + aAA","Aaa + aAA","A + aaaa","AA + aaaa","Aa + aaaa","AAA + aaaa","AAa + aaaa","AaA + aaaa","Aaa + aaaa","AAAA + aaaa","A + aaaA","AA + aaaA","Aa + aaaA","AAA + aaaA","AAa + aaaA","AaA + aaaA","Aaa + aaaA","AAAA + aaaA","AAAa + aaaA","A + aaAa","AA + aaAa","Aa + aaAa","AAA + aaAa","AAa + aaAa","AaA + aaAa","Aaa + aaAa","AAAA + aaAa","AAAa + aaAa","AAaA + aaAa","A + aaAA","AA + aaAA","Aa + aaAA","AAA + aaAA","AAa + aaAA","AaA + aaAA","Aaa + aaAA","AAAA + aaAA","AAAa + aaAA","AAaA + aaAA","AAaa + aaAA","A + aAaa","AA + aAaa","Aa + aAaa","AAA + aAaa","AAa + aAaa","AaA + aAaa","Aaa + aAaa","AAAA + aAaa","AAAa + aAaa","AAaA + aAaa","AAaa + aAaa","AaAA + aAaa","A + aAaA","AA + aAaA","Aa + aAaA","AAA + aAaA","AAa + aAaA","AaA + aAaA","Aaa + aAaA","AAAA + aAaA","AAAa + aAaA","AAaA + aAaA","AAaa + aAaA","AaAA + aAaA","AaAa + aAaA","A + aAAa","AA + aAAa","Aa + aAAa","AAA + aAAa","AAa + aAAa","AaA + aAAa","Aaa + aAAa","AAAA + aAAa","AAAa + aAAa","AAaA + aAAa","AAaa + aAAa","AaAA + aAAa","AaAa + aAAa","AaaA + aAAa","A + aAAA","AA + aAAA","Aa + aAAA","AAA + aAAA","AAa + aAAA","AaA + aAAA","Aaa + aAAA","AAAA + aAAA","AAAa + aAAA","AAaA + aAAA","AAaa + aAAA","AaAA + aAAA","AaAa + aAAA","AaaA + aAAA","Aaaa + aAAA","A + aaaaa","AA + aaaaa","Aa + aaaaa","AAA + aaaaa","AAa + aaaaa","AaA + aaaaa","Aaa + aaaaa","AAAA + aaaaa","AAAa + aaaaa","AAaA + aaaaa","AAaa + aaaaa","AaAA + aaaaa","AaAa + aaaaa","AaaA + aaaaa","Aaaa + aaaaa","AAAAA + aaaaa","A + aaaaA","AA + aaaaA","Aa + aaaaA","AAA + aaaaA","AAa + aaaaA","AaA + aaaaA","Aaa + aaaaA","AAAA + aaaaA","AAAa + aaaaA","AAaA + aaaaA","AAaa + aaaaA","AaAA + aaaaA","AaAa + aaaaA","AaaA + aaaaA","Aaaa + aaaaA","AAAAA + aaaaA","AAAAa + aaaaA","A + aaaAa","AA + aaaAa","Aa + aaaAa","AAA + aaaAa","AAa + aaaAa","AaA + aaaAa","Aaa + aaaAa","AAAA + aaaAa","AAAa + aaaAa","AAaA + aaaAa","AAaa + aaaAa","AaAA + aaaAa","AaAa + aaaAa","AaaA + aaaAa","Aaaa + aaaAa","AAAAA + aaaAa","AAAAa + aaaAa","AAAaA + aaaAa","A + aaaAA","AA + aaaAA","Aa + aaaAA","AAA + aaaAA","AAa + aaaAA","AaA + aaaAA","Aaa + aaaAA","AAAA + aaaAA","AAAa + aaaAA","AAaA + aaaAA","AAaa + aaaAA","AaAA + aaaAA","AaAa + aaaAA","AaaA + aaaAA","Aaaa + aaaAA","AAAAA + aaaAA","AAAAa + aaaAA","AAAaA + aaaAA","AAAaa + aaaAA","A + aaAaa","AA + aaAaa","Aa + aaAaa","AAA + aaAaa","AAa + aaAaa","AaA + aaAaa","Aaa + aaAaa","AAAA + aaAaa","AAAa + aaAaa","AAaA + aaAaa","AAaa + aaAaa","AaAA + aaAaa","AaAa + aaAaa","AaaA + aaAaa","Aaaa + aaAaa","AAAAA + aaAaa","AAAAa + aaAaa","AAAaA + aaAaa","AAAaa + aaAaa","AAaAA + aaAaa","A + aaAaA","AA + aaAaA","Aa + aaAaA","AAA + aaAaA","AAa + aaAaA","AaA + aaAaA","Aaa + aaAaA","AAAA + aaAaA","AAAa + aaAaA","AAaA + aaAaA","AAaa + aaAaA","AaAA + aaAaA","AaAa + aaAaA","AaaA + aaAaA","Aaaa + aaAaA","AAAAA + aaAaA","AAAAa + aaAaA","AAAaA + aaAaA","AAAaa + aaAaA","AAaAA + aaAaA","AAaAa + aaAaA","A + aaAAa","AA + aaAAa","Aa + aaAAa","AAA + aaAAa","AAa + aaAAa","AaA + aaAAa","Aaa + aaAAa","AAAA + aaAAa","AAAa + aaAAa","AAaA + aaAAa","AAaa + aaAAa","AaAA + aaAAa","AaAa + aaAAa","AaaA + aaAAa","Aaaa + aaAAa","AAAAA + aaAAa","AAAAa + aaAAa","AAAaA + aaAAa","AAAaa + aaAAa","AAaAA + aaAAa","AAaAa + aaAAa","AAaaA + aaAAa","A + aaAAA","AA + aaAAA","Aa + aaAAA","AAA + aaAAA","AAa + aaAAA","AaA + aaAAA","Aaa + aaAAA","AAAA + aaAAA","AAAa + aaAAA","AAaA + aaAAA","AAaa + aaAAA","AaAA + aaAAA","AaAa + aaAAA","AaaA + aaAAA","Aaaa + aaAAA","AAAAA + aaAAA","AAAAa + aaAAA","AAAaA + aaAAA","AAAaa + aaAAA","AAaAA + aaAAA","AAaAa + aaAAA","AAaaA + aaAAA","AAaaa + aaAAA","A + aAaaa","AA + aAaaa","Aa + aAaaa","AAA + aAaaa","AAa + aAaaa","AaA + aAaaa","Aaa + aAaaa","AAAA + aAaaa","AAAa + aAaaa","AAaA + aAaaa","AAaa + aAaaa","AaAA + aAaaa","AaAa + aAaaa","AaaA + aAaaa","Aaaa + aAaaa","AAAAA + aAaaa","AAAAa + aAaaa","AAAaA + aAaaa","AAAaa + aAaaa","AAaAA + aAaaa","AAaAa + aAaaa","AAaaA + aAaaa","AAaaa + aAaaa","AaAAA + aAaaa","A + aAaaA","AA + aAaaA","Aa + aAaaA","AAA + aAaaA","AAa + aAaaA","AaA + aAaaA","Aaa + aAaaA","AAAA + aAaaA","AAAa + aAaaA","AAaA + aAaaA","AAaa + aAaaA","AaAA + aAaaA","AaAa + aAaaA","AaaA + aAaaA","Aaaa + aAaaA","AAAAA + aAaaA","AAAAa + aAaaA","AAAaA + aAaaA","AAAaa + aAaaA","AAaAA + aAaaA","AAaAa + aAaaA","AAaaA + aAaaA","AAaaa + aAaaA","AaAAA + aAaaA","AaAAa + aAaaA","A + aAaAa","AA + aAaAa","Aa + aAaAa","AAA + aAaAa","AAa + aAaAa","AaA + aAaAa","Aaa + aAaAa","AAAA + aAaAa","AAAa + aAaAa","AAaA + aAaAa","AAaa + aAaAa","AaAA + aAaAa","AaAa + aAaAa","AaaA + aAaAa","Aaaa + aAaAa","AAAAA + aAaAa","AAAAa + aAaAa","AAAaA + aAaAa","AAAaa + aAaAa","AAaAA + aAaAa","AAaAa + aAaAa","AAaaA + aAaAa","AAaaa + aAaAa","AaAAA + aAaAa","AaAAa + aAaAa","AaAaA + aAaAa","A + aAaAA","AA + aAaAA","Aa + aAaAA","AAA + aAaAA","AAa + aAaAA","AaA + aAaAA","Aaa + aAaAA","AAAA + aAaAA","AAAa + aAaAA","AAaA + aAaAA","AAaa + aAaAA","AaAA + aAaAA","AaAa + aAaAA","AaaA + aAaAA","Aaaa + aAaAA","AAAAA + aAaAA","AAAAa + aAaAA","AAAaA + aAaAA","AAAaa + aAaAA","AAaAA + aAaAA","AAaAa + aAaAA","AAaaA + aAaAA","AAaaa + aAaAA","AaAAA + aAaAA","AaAAa + aAaAA","AaAaA + aAaAA","AaAaa + aAaAA","A + aAAaa","AA + aAAaa","Aa + aAAaa","AAA + aAAaa","AAa + aAAaa","AaA + aAAaa","Aaa + aAAaa","AAAA + aAAaa","AAAa + aAAaa","AAaA + aAAaa","AAaa + aAAaa","AaAA + aAAaa","AaAa + aAAaa","AaaA + aAAaa","Aaaa + aAAaa","AAAAA + aAAaa","AAAAa + aAAaa","AAAaA + aAAaa","AAAaa + aAAaa","AAaAA + aAAaa","AAaAa + aAAaa","AAaaA + aAAaa","AAaaa + aAAaa","AaAAA + aAAaa","AaAAa + aAAaa","AaAaA + aAAaa","AaAaa + aAAaa","AaaAA + aAAaa","A + aAAaA","AA + aAAaA","Aa + aAAaA","AAA + aAAaA","AAa + aAAaA","AaA + aAAaA","Aaa + aAAaA","AAAA + aAAaA","AAAa + aAAaA","AAaA + aAAaA","AAaa + aAAaA","AaAA + aAAaA","AaAa + aAAaA","AaaA + aAAaA","Aaaa + aAAaA","AAAAA + aAAaA","AAAAa + aAAaA","AAAaA + aAAaA","AAAaa + aAAaA","AAaAA + aAAaA","AAaAa + aAAaA","AAaaA + aAAaA","AAaaa + aAAaA","AaAAA + aAAaA","AaAAa + aAAaA","AaAaA + aAAaA","AaAaa + aAAaA","AaaAA + aAAaA","AaaAa + aAAaA","A + aAAAa","AA + aAAAa","Aa + aAAAa","AAA + aAAAa","AAa + aAAAa","AaA + aAAAa","Aaa + aAAAa","AAAA + aAAAa","AAAa + aAAAa","AAaA + aAAAa","AAaa + aAAAa","AaAA + aAAAa","AaAa + aAAAa","AaaA + aAAAa","Aaaa + aAAAa","AAAAA + aAAAa","AAAAa + aAAAa","AAAaA + aAAAa","AAAaa + aAAAa","AAaAA + aAAAa","AAaAa + aAAAa","AAaaA + aAAAa","AAaaa + aAAAa","AaAAA + aAAAa","AaAAa + aAAAa","AaAaA + aAAAa","AaAaa + aAAAa","AaaAA + aAAAa","AaaAa + aAAAa","AaaaA + aAAAa","A + aAAAA","AA + aAAAA","Aa + aAAAA","AAA + aAAAA","AAa + aAAAA","AaA + aAAAA","Aaa + aAAAA","AAAA + aAAAA","AAAa + aAAAA","AAaA + aAAAA","AAaa + aAAAA","AaAA + aAAAA","AaAa + aAAAA","AaaA + aAAAA","Aaaa + aAAAA","AAAAA + aAAAA","AAAAa + aAAAA","AAAaA + aAAAA","AAAaa + aAAAA","AAaAA + aAAAA","AAaAa + aAAAA","AAaaA + aAAAA","AAaaa + aAAAA","AaAAA + aAAAA","AaAAa + aAAAA","AaAaA + aAAAA","AaAaa + aAAAA","AaaAA + aAAAA","AaaAa + aAAAA","AaaaA + aAAAA","Aaaaa + aAAAA"};
(* ============================================== *)
InitializeResultInfo[CLM$Subst$5$Unsorted, CLM$Pairs$5$Unsorted];
(*
substSorted=SortByFirst[ResultInfoSubstValLst,CLM$Subst$5$Unsorted];
Print["substSorted = ", substSorted // MatrixForm];

pairSorted=SortByFirst[ResultInfoPairValLst,CLM$Pairs$5$Unsorted];
Print["pairSorted = ", pairSorted // MatrixForm];
*)
(*

(*
Print["cnAllSorted = ", cnAllSorted // MatrixForm];
Print["cpmAllSorted = ", cpmAllSorted // MatrixForm];
*)
Print["cpmAllSorted2 = ", cpmAllSorted2 // MatrixForm];
*)

GetPairID[SubstIdLst[[1]],SubstIdLst[[1]]];

(* ============================================== *)
CountCatalysts[firstID_?IntegerQ,secondID_?IntegerQ,threshold_?NumericQ]:=Module[{pairID,cpmVal, cnt},
pairID=GetPairID[SubstIdLst[[firstID]],SubstIdLst[[secondID]]];
cpmVal=cpmAll[[pairID]];

cnt=Count[cpmVal,u_/;u>=threshold];

(* Print["CountCatalysts::firstID = ", firstID, ", secondID = ", secondID, ", pairID = ", pairID, ", cnt = ",cnt]; *)

(*
Print["CountCatalysts::cpmVal = ", cpmVal // MatrixForm];
*)
Return[cnt];
];
(* ============================================== *)
xName="x";
yName="y";
(* ============================================== *)
zNameCat=Subscript["N","c"];
zMaxCat=62;

zNamePair="η";
zMaxPair=1;

zNamePair2=Subscript["N","p"];
zMaxPair2=400;
(* ============================================== *)
myblend=(Blend[{{0,White},{0.2,Red},{0.6,Yellow},{0.8,Green},{1.0,Blue}},#]&);

g[zMaxValue_]:=Plot[1,{x,0,zMaxValue}, PlotStyle->Thickness[5], Frame -> True, AspectRatio -> 0.02,GridLines->Automatic, ColorFunction->Function[{x,y},myblend[x/zMaxValue]],ColorFunctionScaling->False, FrameTicks->{{None,None},{Automatic,None}},LabelStyle -> BDPLTTEXTOPTS, ImageSize->500];

(*Print[g[zMaxCat]]; *)
(* ============================================== *)
DoDiscretePlot3D[data_?MatrixQ, xLabels_, yLabels_, zName_, zMax_,swapX_?BooleanQ, swapY_?BooleanQ]:=Module[{discrPlot3DOpts2,discrPlot3DOpts2swapX,discrPlot3DOpts2swapY,discrPlot3DOpts2swapXY,xTicks, yTicks,xTicks2, yTicks2,xRot,yRot, xLen, yLen,discrPlot3DOpts},
xRot=54;
yRot=-20;

xLen=Length[xLabels];
yLen=Length[yLabels];

If[xLen>0,
(
xTicks=Table[{ii,Rotate[xLabels[[ii]],xRot Degree,{Right,Center}]},{ii,1,xLen}];
xTicks2=Table[{ii,Rotate[xLabels[[xLen+1-ii]],xRot Degree,{Right,Center}]},{ii,1,xLen}];
),
(
xTicks=Automatic;
xTicks2=Automatic;
),
(
xLen=Length[data];
)
];

If[yLen > 0,
(
yTicks=Table[{ii,Rotate[yLabels[[ii]],yRot Degree,{Right,Center}]},{ii,1,yLen}];
yTicks2=Table[{ii,Rotate[yLabels[[yLen+1-ii]],yRot Degree,{Right,Center}]},{ii,1,yLen}];
),
(
yTicks=Automatic;
yTicks2=Automatic;
),
(
yLen=Length[data[[1]]];
)
];

discrPlot3DOpts2:={PlotRange->All,ImageSize -> BDIMAGESIZE,ExtentSize->Full,LabelStyle -> BDPLTTEXTOPTS,AxesLabel->{xName,yName,zName}, Ticks -> {xTicks, yTicks, Automatic},ColorFunction->Function[{x,y,z},myblend[z/zMax]], ColorFunctionScaling->False};

discrPlot3DOpts2swapX:={PlotRange->All,ImageSize -> BDIMAGESIZE,ExtentSize->Full,LabelStyle -> BDPLTTEXTOPTS,AxesLabel->{xName,yName,zName}, Ticks -> {xTicks2, yTicks, Automatic},ColorFunction->Function[{x,y,z},myblend[z/zMax]], ColorFunctionScaling->False};

discrPlot3DOpts2swapY:={PlotRange->All,ImageSize -> BDIMAGESIZE,ExtentSize->Full,LabelStyle -> BDPLTTEXTOPTS,AxesLabel->{xName,yName,zName}, Ticks -> {xTicks, yTicks2, Automatic},ColorFunction->Function[{x,y,z},myblend[z/zMax]], ColorFunctionScaling->False};

discrPlot3DOpts2swapXY:={PlotRange->All,ImageSize -> BDIMAGESIZE,ExtentSize->Full,LabelStyle -> BDPLTTEXTOPTS,AxesLabel->{xName,yName,zName}, Ticks -> {xTicks2, yTicks2, Automatic},ColorFunction->Function[{x,y,z},myblend[z/zMax]], ColorFunctionScaling->False};

If[!swapX,
(
If[!swapY,
(
discrPlot3DOpts=discrPlot3DOpts2;
),
(
discrPlot3DOpts=discrPlot3DOpts2swapY;
)
];
),
(
If[!swapY,
(
discrPlot3DOpts=discrPlot3DOpts2swapX;
),
(
discrPlot3DOpts=discrPlot3DOpts2swapXY;
)
];
)
];

Print[Legended[DiscretePlot3D[data[[ii,jj]],{ii,1,xLen},{jj,1,yLen},Evaluate[discrPlot3DOpts]],Placed[g[zMax],Below]]];
];
(* ============================================== *)
(* ============================================== *)
(*
(* Return No of the first subst based on the labels *)
GetPairIDs[labels_,pairs_,id_?IntegerQ]:=Module[{s,posLst, first, second,pos},
s=pairs[[id]];
posLst=StringPosition[s,"+",1, IgnoreCaseFalse];
first=StringTake[s, posLst[[1,1]]-2];
second=StringTake[s, {posLst[[1,1]]+2, StringLength[s]}];

pos=Flatten[{Position[labels,first],Position[labels,second]}];

Print["s = ", s,",  StringLength[s] = ", StringLength[s],  ", id = ", id, ", posLst = ", posLst, ", first = ", first, ", second = ", second, ", pos = ", pos];

Return[pos];
];
*)
(* ============================================== *)
(*
substLen=Length[labels5];
pairLen=Length[pairs5];

GetPairIDs[labels5,pairs5,3];

pairList=Table[GetPairIDs[labels5,pairs5,ii],{ii,1,pairLen}];

Print["pairList = ", pairList // MatrixForm];
*)
(* ============================================== *)
(* ============================================== *)
Print[strSeparator];

If[Length[cnAll]>0,
(
Print["Model name: ",CLMRD$ModelName, ", file: ",CLMRD$FileName,", Cnn"];
cnTable=Table[cnAll[[GetPairID[SubstIdLst[[ii]],SubstIdLst[[jj]]]]],{ii,1,SubstNameLen},{jj,1,SubstNameLen}]; 
cnTableSorted=SortXYData[cnTable];
(*
Print["subst sorted = ", {SortedSubstNameLst} // MatrixForm];
Print["cnTableSorted = ", cnTableSorted // MatrixForm];
*)
xName="";
yName="";
DoDiscretePlot3D[cnTableSorted, SortedSubstNameLst, SortedSubstNameLst,zNamePair, zMaxPair,False, False];
Print[strSeparator];)
];
(* ============================================== *)
If[Length[cpmAll]>0,
(
Do[
(
(* ============================================== *)
threshold=thresholdLst[[cnt]];
Print["Model name: ",CLMRD$ModelName, ", file: ",CLMRD$FileName,", Cpm, threshold = ", threshold];
catCnt=Table[CountCatalysts[ii,jj,threshold],{ii,1,SubstNameLen},{jj,1,SubstNameLen}]; 

(* Print["catCnt = ", catCnt // MatrixForm]; *)

catCntSorted=SortXYData[catCnt];
(*
Print["subst sorted = ", {SortedSubstNameLst} // MatrixForm];
Print["catCntSorted = ", catCntSorted // MatrixForm];
*)
xName="";
yName="";
DoDiscretePlot3D[catCntSorted, SortedSubstNameLst, SortedSubstNameLst,zNameCat, zMaxCat,False, False];
Print[strSeparator];
),
{cnt,1,Length[thresholdLst]}
];

(* ============================================== *)
Print["Model name: ",CLMRD$ModelName, ", file: ",CLMRD$FileName,", pairs for catalysts and threshold."];
thresholdVal=Table[th,{th,0.1,0.9,0.05}];

(*
Print["Length[cpmAllSorted] = ", Length[cpmAllSorted]];
Print["Length[cpmAllSorted[[1]]] = ", Length[cpmAllSorted[[1]]]];
*)

pairThreshold={Table[Sum[If[cnAllSorted[[ii]]>=thresholdVal[[kk]],1,0],{ii,1,PairNameLen}],{kk,1,Length[thresholdVal]}]};
varThreshold=ParallelTable[Sum[If[cpmAllSorted[[ii,jj]]>=thresholdVal[[kk]],1,0],{ii,1,PairNameLen}],{jj,1,SubstNameLen},{kk,1,Length[thresholdVal]}];
varThreshold1=Join[pairThreshold,varThreshold];
SortedSubstNameLst1=Join[{"--"},SortedSubstNameLst];
(*
Print["Length[varThreshold] = ", Length[varThreshold]];
Print["pairThreshold = ", pairThreshold // MatrixForm];
Print["varThreshold1 = ", varThreshold1 // MatrixForm];
*)
xName="";
yName=Subscript[η,t];
DoDiscretePlot3D[varThreshold, SortedSubstNameLst, thresholdVal,zNamePair2,zMaxPair2,False, False];
Print[strSeparator];

xName="";
yName=Subscript[η,t];
DoDiscretePlot3D[varThreshold1, SortedSubstNameLst1, thresholdVal,zNamePair2,zMaxPair2,False, False];
Print[strSeparator];

(*
varThreshold2=ParallelTable[Sum[If[(cpmAllSorted[[ii,jj]]>=thresholdVal[[kk]]) && (cnAllSorted[[jj]]<thresholdVal[[kk]]),cpmAllSorted[[ii,jj]],0],{ii,1,PairNameLen}],{jj,1,SubstNameLen},{kk,1,Length[thresholdVal]}];
DoDiscretePlot3D[varThreshold2, SortedSubstNameLst, thresholdVal,zNamePair2,zMaxPair2,False, False];
*)
(* ============================================== *)
)
];





