(* ============================================== *)
(* :Summary: CLM direct sedimentation logic. *)
(* ============================================== *)
(* :Author: Konstantin K. Konstantinov *)
(* :Email: konstantin.k.konstantinov@gmail.com *)
(* :License type: GPL v3 or any later version, see http://www.gnu.org/licenses/ *)
(* :Copyright: K^3, 2013 - 2015 *)
(* :Version: Revision: 3.18.001, Date: 2015/11/23 *)
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
InitializeDirectCrystallizationValue=Indeterminate;
(* ============================================== *)
AssignDirectCrystCoefficientsValue=Indeterminate;
(* ============================================== *)
UseAllPairsForDirectCrystValue=Indeterminate;
(* ============================================== *)
kDirectCrystCoeffDistribution=InverseGaussianDistribution;
kDirectCrystCoeffParams={1,1};
DirectCrystMinLen=1;
DirectCrystSecondSubstMinLen=1;
(* ============================================== *)
kDirectCrystCoefficientValue[substAid_?IntegerQ,substBid_?IntegerQ]:=Module[{retVal,base},
base=(GetChainLength[substAid]-1)/2;
retVal=RandomCoefficientValue[kDirectCrystCoeffDistribution,kDirectCrystCoeffParams,base];
Return[retVal];
];
(* ============================================== *)
(* ============================================== *)
(* Simplified direct sedimentation is A + B \[Rule] (NA + NB) * Y *)
AssignDirectCrystReactions[substIdVal_?IntegerQ,subst1Id1Val_?IntegerQ,multiplier_?IntegerQ,allocateCoeff_?BooleanQ]:=Module[{substIDlst,substEiDlst,substId,subst1Id,name,name1,base,base1,substLen,substDecayID,substDecayName,retVal,nameCoeff,name1Coeff,baseSubstId,coeffIdxName,coeffName,reacStringName,reacIdxName},

(* Print["AssignDirectCrystReactions::Starting..."]; *)

(* We sort the substances in canonic ordering (by IDs) *)
substIDlst=Sort[{substIdVal,subst1Id1Val}];
substEiDlst=Sort[{EnantiomerSubstanceID[substIdVal],EnantiomerSubstanceID[subst1Id1Val]}];

substId=substIDlst[[1]];
subst1Id=substIDlst[[2]];

name=GetSubstanceName[substId];
name1=GetSubstanceName[subst1Id];

base=GetChainLength[substId];
base1=GetChainLength[subst1Id];
substLen=base+base1;

substDecayID=idxY;
substDecayName=GetSubstanceName[substDecayID];

retVal={};

If[substId!=subst1Id,
(
If[substIDlst[[1]]<= substEiDlst[[1]],
(
nameCoeff=name;
name1Coeff=name1;
),
(
nameCoeff=GetSubstanceName[ substEiDlst[[1]]];
name1Coeff=GetSubstanceName[ substEiDlst[[2]]];
)
];
),
(
baseSubstId=Min[EnantiomerSubstanceID[substId],substId];
nameCoeff=GetSubstanceName[baseSubstId];
name1Coeff=nameCoeff;
)
];

(* Direct crystallization  *)
coeffIdxName=CoeffPrefixValue<>"Idx"<>nameCoeff<>PlusLetter<>name1Coeff <>ToLetter<>ToString[substLen]<>substDecayName;
coeffName=CoeffPrefixValue<>nameCoeff<>PlusLetter<>name1Coeff <>ToLetter<>ToString[substLen]<>substDecayName;
reacStringName=name <>" + " <> name1 <> " -> " <>ToString[substLen]<>substDecayName;
reacIdxName=ReactionPrefixValue<>name<>PlusLetter<>name1 <>ToLetter<>ToString[substLen]<>substDecayName;

(*
Print["AssignDirectCrystReactions::reacIdxName = ", reacIdxName, ", reacStringName = ", reacStringName, ", coeffName = ", coeffName,", substId = ", substId, ", subst1Id = ", subst1Id];
*)

If[allocateCoeff,
(
ToExpression[coeffIdxName<>"=AddCoeffName["<> coeffName <>",Subscript[k,\""<>reacStringName<>"\"]]"];

If[AssignDirectCrystCoefficientsValue,
(
ToExpression[coeffName <> "=kDirectCrystCoefficientValue[" <>ToString[substId] <> ", " <> ToString[subst1Id]<>"]"];
)
];
)
];

ToExpression[reacIdxName<>"=AddReaction[{{DirectCrystReaction,\""<>reacStringName <>"\"},{{"<>ToString[substId]<>",1},{"<> ToString[subst1Id]<>",1}},{" <>ToString[multiplier]<> coeffName <> ",1,1},{{"<>ToString[substDecayID]<>"," <> ToString[substLen]<>"}}}]"];

retVal=reacIdxName;
Return[retVal];
];
(* ============================================== *)
InitializeDirectCrystReactions[rawOpts___]:=Module[{opts,ii,idxCval,idxChainStart,idxChainEnd,idxChain,len,reacAA,substPairAA,substCrystAA,reacaa,substPairaa,substCrystaa,reacAa,substPairAa,substCrystAa,idxChainB,substBlst,lenB,jj,substBid,idxB,BlstLen,Aid,Bid,enantAid,enantBid,kk},
opts=ProcessOptions[rawOpts];

InitializeDirectCrystallizationValue=InitializeDirectCrystallization/. opts /.Options[CLMChains];
AssignDirectCrystCoefficientsValue=AssignDirectCrystCoefficients/. opts /.Options[CLMChains];
UseAllPairsForDirectCrystValue=UseAllPairsForDirectCryst/. opts /.Options[CLMChains];

If[InitializeDirectCrystallizationValue && (!(InitializeBasicCrystValue || InitializeChainCrystValue)),
(
If[!SilentRunValue,Print["InitializeDirectCrystReactions::Initializing direct crystallization reactions..."]];
idxChainStart=DirectCrystMinLen;
idxChainEnd=MaxChainLength;
),
(
If[!SilentRunValue,Print["InitializeDirectCrystReactions::Not initializing direct crystallization reactions..."]];
Return[];
)
];

Do[
(
len=Length[AllChainsTbl[[idxChain]]]/2;

Do[
(
Aid=GetSubstanceID[AllChainsTbl[[idxChain,ii]]];
enantAid=EnantiomerSubstanceID[Aid];

If[UseAllPairsForDirectCrystValue,
(
substBlst={};

Do[
(
If[idxChainB==idxChain,
(
lenB=ii;
),
(
lenB=Length[AllChainsTbl[[idxChainB]]]/2;
)
];

If[!SilentRunValue,Print["InitializeDirectCrystReactions::idxChain = ", idxChain,", ii = ", ii,", idxChainB = ", idxChainB,", lenB = ", lenB]];

Do[
(
substBid=GetSubstanceID[AllChainsTbl[[idxChainB,jj]]];
substBlst=Join[substBlst,{substBid}];
),{jj,1,lenB}
];
),{idxChainB,DirectCrystSecondSubstMinLen,idxChain}
];
),
(
substBlst={EnantiomerSubstanceID[Aid]};
)
];

BlstLen=Length[substBlst];

If[!SilentRunValue,Print["InitializeDirectCrystReactions::A = ", SubstanceMatrix[Aid],", substBlst = ", Table[{kk,substBlst[[kk]],SubstanceMatrix[substBlst[[kk]]]},{kk,1,BlstLen}] // MatrixForm]];

Do[
(
Bid=substBlst[[idxB]];
enantBid=EnantiomerSubstanceID[Bid];

(* Direct crystallization reactions like like A+B \[Rule] 2Y *)
AssignDirectCrystReactions[Aid,Bid,1,True];

(* Direct crystallization reactions like like a+b \[Rule] 2Y *)
AssignDirectCrystReactions[enantAid,enantBid,1,False];

(* Direct crystallization reactions like like A+b \[Rule] 2Y *)
AssignDirectCrystReactions[Aid,enantBid,1,True];

(* Direct crystallization reactions like like a+B \[Rule] 2Y *)
AssignDirectCrystReactions[enantAid,Bid,1,False];

),{idxB,1,BlstLen}
];
),{ii,len}
];
),{idxChain,idxChainStart,idxChainEnd}
];
];
(* ============================================== *)
