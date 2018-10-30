(* :Summary: 1st generation CLM model. *)
(* :Author: Konstantin K. Konstantinov *)
(* :Email: konstantin.k.konstantinov@gmail.com *)
(* :License type: GPL *)
(* :Copyright: K^3, 2013 - 2014 *)
(* :Version: Revision: 2.05.001, Date: 2014/09/10 *)
(* :Mathematica Version: 7.0 - 10.0 *)
(* ============================================== *)
idxGenerationSimple=NextGenerationNo[];
SetMaxGenerationLevel[idxGenerationSimple];
IsGenerationDefined[idxGenerationSimple]:=True;
(* ============================================== *)
InitializeGeneration[idxGenerationSimple]:=Module[{},
(* Atoms *)
Print[strSeparatorCRLF,"Atoms."];
idxAtomZ=AddAtom[aZ,Superscript[Subscript[\[Rho],A],total]];

(* Substances *)
Print[strSeparatorCRLF,"Substances."];

(* We don't count Z substance, so mathematically it does not have any atoms in it. *)
idxZ=AddSubstance[rZ,Subscript[\[Rho],Z],{{idxAtomZ,0}}];
idxA=AddSubstance[rA,Subscript[\[Rho],A],{{idxAtomZ,1}}];
idxL=AddSubstance[rL,Subscript[\[Rho],L],{{idxAtomZ,nnn}}];
idxR=AddSubstance[rR,Subscript[\[Rho],D],{{idxAtomZ,nnn}}];
idxCl=AddSubstance[rCl,Subscript[\[Rho],Cl],{{idxAtomZ,nnn}}];
idxCr=AddSubstance[rCr,Subscript[\[Rho],Cd],{{idxAtomZ,nnn}}];

coeffIdx\[Rho]0=AddCoeffName[\[Rho]0,Subscript[\[Rho],0]];
coeffIdxRacemization=AddCoeffName[m,m];

Print["Functions"];
idxFuncNuS=NextFuncNo[];

Print["Gamma parameters."];
(* X = NS || S *)

coeffIdxGammaPlus=AddCoeffName[coeffGammaPlus,Subscript[\[Gamma],"+"]];
(* coeffAAStoXS = (coeffAAStoSS + coeffAAStoNSS)/2 *)
coeffIdxAAStoXS=AddCoeffName[coeffAAStoXS,Subscript[k,nnn A+S->X+S]];

coeffIdxGammaMinus=AddCoeffName[coeffGammaMinus,Subscript[\[Gamma],"-"]];
(* coeffXSto2AS = (coeffSSto2AS + coeffSNSto2ANS)/2 *)
coeffIdxXSto2AS=AddCoeffName[coeffXSto2AS,Subscript[k,X+S->nnn A+S]];

coeffIdxGamma=AddCoeffName[coeffGamma,\[Gamma]];

(* GammaPlus = (k1-k2)/(k1+k2) *)
GammaPlusFunc[coeffNewVal:{__}]:=((coeffNewVal[[coeffIdxAAStoSS]]-coeffNewVal[[coeffIdxAAStoNSS]])/(coeffNewVal[[coeffIdxAAStoSS]]+coeffNewVal[[coeffIdxAAStoNSS]]));
GetCoeffAAStoSS[gammaPlusVar_,coeffAAStoXSvar_]:=coeffAAStoXSvar*(1+gammaPlusVar);
GetCoeffAAStoNSS[gammaPlusVar_,coeffAAStoXSvar_]:=coeffAAStoXSvar*(1-gammaPlusVar);

(* GammaMunis = (k(-2)-k(-1))/(k(-2)+k(-1)) *)
GammaMunisFunc[coeffNewVal:{__}]:=((coeffNewVal[[coeffIdxSNSto2ANS]]-coeffNewVal[[coeffIdxSSto2AS]])/(coeffNewVal[[coeffIdxSSto2AS]]+coeffNewVal[[coeffIdxSNSto2ANS]]));
GetCoeffSNSto2ANS[gammaMinusVar_,coeffXSto2ASvar_]:=coeffXSto2ASvar*(1+gammaMinusVar);
GetCoeffSSto2AS[gammaMinusVar_,coeffXSto2ASvar_]:=coeffXSto2ASvar*(1-gammaMinusVar);

(* K = (k(-1)+k(-2))/(k1+k2) *)
KrevFunc[coeffNewVal:{__}]:=((coeffNewVal[[coeffIdxSSto2AS]]+coeffNewVal[[coeffIdxSNSto2ANS]])/(coeffNewVal[[coeffIdxAAStoSS]]+coeffNewVal[[coeffIdxAAStoNSS]]));

(* {{reactionFunc,reactionDescription},{{source1,number1,pow1},...},{parameters,...},{{result1,number1},...}} *)

Print[strSeparatorCRLF,"Synthesis of A from Zero."];
Print["Z -> A"];
rIdxZtoA=AddReaction[{{StandardReaction,"Z -> A"},{{idxZ,0}},{coeffZtoA,1},{{idxA,1}}}];
coeffIdxZtoA=AddCoeffName[coeffZtoA,Subscript[k,Z->A]];

Print[strSeparatorCRLF,"Direct Synthesis: k3 - Avetisov / Goldanskij."];
Print[nnnStr <>"A -> L"];
rIdxAAtoL=AddReaction[{{StandardReaction,nnnStr <>"A -> L"},{{idxA,nnn}},{coeffAAtoL,nnn},{{idxL,1}}}];
coeffIdxAAtoL=AddCoeffName[coeffAAtoL,Subscript[k,nnn A->L]];

Print[nnnStr <> "A -> R"];
rIdxAAtoR=AddReaction[{{StandardReaction,nnnStr <> "A -> R"},{{idxA,nnn}},{coeffAAtoR,nnn},{{idxR,1}}}];
coeffIdxAAtoR=AddCoeffName[coeffAAtoR,Subscript[k,nnn A->R]];

coeffIdxAAtoS=coeffIdxAAtoL;
coeffIdx\[CapitalDelta]AAtoS=coeffIdxAAtoR;
AddCoeffRule[{coeffIdxAAtoS,coeffIdx\[CapitalDelta]AAtoS},{coeffAAtoS,coeff\[CapitalDelta]AAtoS},{Subscript[k,nnn A->S],Subscript[\[CapitalDelta]k,nnn A->S]},{coeffAAtoL -> coeffAAtoS*(1-coeff\[CapitalDelta]AAtoS),coeffAAtoR -> coeffAAtoS*(1+coeff\[CapitalDelta]AAtoS)}];

Print[strSeparatorCRLF,"Basic Autocatalysis: k1 - Avetisov / Goldanskij."];
Print[nnnStr <> "A + L -> 2L"];
rIdxAALtoLL=AddReaction[{{StandardReaction,nnnStr <> "A + L -> 2L"},{{idxA,nnn},{idxL,1}},{coeffAALtoLL,nnn,1},{{idxL,2}}}];
coeffIdxAALtoLL=AddCoeffName[coeffAALtoLL,Subscript[k,nnn A+L->2L]];

Print[nnnStr <> "A + R -> 2R"];
rIdxAARtoRR=AddReaction[{{StandardReaction,nnnStr <> "A + R -> 2R"},{{idxA,nnn},{idxR,1}},{coeffAARtoRR,nnn,1},{{idxR,2}}}];
coeffIdxAARtoRR=AddCoeffName[coeffAARtoRR,Subscript[k,nnn A+R->2R]];

coeffIdxAAStoSS=coeffIdxAALtoLL;
coeffIdx\[CapitalDelta]AAStoSS=coeffIdxAARtoRR;
AddCoeffRule[{coeffIdxAAStoSS,coeffIdx\[CapitalDelta]AAStoSS},{coeffAAStoSS,coeff\[CapitalDelta]AAStoSS},{Subscript[k,nnn A+S->2S],Subscript[\[CapitalDelta]k,nnn A+S->2S]},{coeffAALtoLL -> coeffAAStoSS*(1-coeff\[CapitalDelta]AAStoSS),coeffAARtoRR -> coeffAAStoSS*(1+coeff\[CapitalDelta]AAStoSS)}];

Print[strSeparatorCRLF,"Wrong Autocatalysis: k2 - Avetisov / Goldanskij."];
Print[nnnStr <> "A + L -> R + L"];
rIdxAALtoRL=AddReaction[{{StandardReaction,nnnStr <> "A + L -> R + L"},{{idxA,nnn},{idxL,1}},{coeffAALtoRL,nnn,1},{{idxR,1},{idxL,1}}}];
coeffIdxAALtoRL=AddCoeffName[coeffAALtoRL,Subscript[k,nnn A+L->R+L]];

Print[nnnStr <> "A + R -> L + R"];
rIdxAARtoLR=AddReaction[{{StandardReaction,nnnStr <> "A + R -> L + R"},{{idxA,nnn},{idxR,1}},{coeffAARtoLR,nnn,1},{{idxL,1},{idxR,1}}}];
coeffIdxAARtoLR=AddCoeffName[coeffAARtoLR,Subscript[k,nnn A+R->L+R]];

coeffIdxAAStoNSS=coeffIdxAALtoRL;
coeffIdx\[CapitalDelta]AAStoNSS=coeffIdxAARtoLR;
AddCoeffRule[{coeffIdxAAStoNSS,coeffIdx\[CapitalDelta]AAStoNSS},{coeffAAStoNSS,coeff\[CapitalDelta]AAStoNSS},{Subscript[k,nnn A+S->NS+S],Subscript[\[CapitalDelta]k,nnn A+S->NS+S]},{coeffAALtoRL -> coeffAAStoNSS*(1-coeff\[CapitalDelta]AAStoNSS),coeffAARtoLR -> coeffAAStoNSS*(1+coeff\[CapitalDelta]AAStoNSS)}];

Print[strSeparatorCRLF,"Mutual antagonism / Reversed Autocatalysis: k(-2) - Avetisov / Goldanskij."];
Print["L + R -> " <> nnnStr <> "A + R"];
rIdxLRto2AR=AddReaction[{{StandardReaction,"L + R -> " <> nnnStr <> "A + R"},{{idxL,1},{idxR,1}},{coeffLRto2AR,1,1},{{idxA,nnn},{idxR,1}}}];
coeffIdxLRto2AR=AddCoeffName[coeffLRto2AR,Subscript[k,L+R->nnn A+R]];

Print["R + L -> " <> nnnStr <> "A + L"];
rIdxRLto2AL=NAddReaction[{{StandardReaction,"R + L -> " <> nnnStr <> "A + L"},{{idxR,1},{idxL,1}},{coeffRLto2AL,1,1},{{idxA,nnn},{idxL,1}}}];
coeffIdxRLto2AL=AddCoeffName[coeffRLto2AL,Subscript[k,R+L->nnn A+L]];

coeffIdxSNSto2ANS=coeffIdxLRto2AR;
coeffIdx\[CapitalDelta]SNSto2ANS=coeffIdxRLto2AL;
AddCoeffRule[{coeffIdxSNSto2ANS,coeffIdx\[CapitalDelta]SNSto2ANS},{coeffSNSto2ANS,coeff\[CapitalDelta]SNSto2ANS},{Subscript[k,S+NS->nnn A+NS],Subscript[\[CapitalDelta]k,S+NS->nnn A+NS]},{coeffLRto2AR -> coeffSNSto2ANS*(1-coeff\[CapitalDelta]SNSto2ANS),coeffRLto2AL -> coeffSNSto2ANS*(1+coeff\[CapitalDelta]SNSto2ANS)}];

Print[strSeparatorCRLF,"Wrong Mutual antagonism / Reversed Autocatalysis: k(-1) - Avetisov / Goldanskij."];
Print["L + L -> " <> nnnStr <> "A + L"];
rIdxLLto2AL=AddReaction[{{StandardReaction,"L + L -> " <> nnnStr <> "A + L"},{{idxL,2}},{coeffLLto2AL,2},{{idxA,nnn},{idxL,1}}}];
coeffIdxLLto2AL=AddCoeffName[coeffLLto2AL,Subscript[k,L+L->nnn A+R]];

Print["R + R -> " <> nnnStr <> "A + R"];
rIdxRRto2AR=AddReaction[{{StandardReaction,"R + R -> " <> nnnStr <> "A + R"},{{idxR,2}},{coeffRRto2AR,2},{{idxA,nnn},{idxR,1}}}];
coeffIdxRRto2AR=AddCoeffName[coeffRRto2AR,Subscript[k,R+L->nnn A+L]];

coeffIdxSSto2AS=coeffIdxLLto2AL;
coeffIdx\[CapitalDelta]SSto2AS=coeffIdxRRto2AR;
AddCoeffRule[{coeffIdxSSto2AS,coeffIdx\[CapitalDelta]SSto2AS},{coeffSSto2AS,coeff\[CapitalDelta]SSto2AS},{Subscript[k,S+S->nnn A+S],Subscript[\[CapitalDelta]k,S+S->nnn A+S]},{coeffLLto2AL -> coeffSSto2AS*(1-coeff\[CapitalDelta]SSto2AS),coeffRRto2AR -> coeffSSto2AS*(1+coeff\[CapitalDelta]SSto2AS)}];

Print[strSeparatorCRLF,"Racemization."];
Print["L -> R"];
rIdxLtoR=AddReaction[{{StandardReaction,"L -> R"},{{idxL,1}},{coeffLtoR,1},{{idxR,1}}}];
coeffIdxLtoR=AddCoeffName[coeffLtoR,Subscript[k,L->R]];

Print["R -> L"];
rIdxRtoL=AddReaction[{{StandardReaction,"R -> L"},{{idxR,1}},{coeffRtoL,1},{{idxL,1}}}];
coeffIdxRtoL=AddCoeffName[coeffRtoL,Subscript[k,R->L]];

coeffIdxStoNS=coeffIdxLtoR;
coeffIdx\[CapitalDelta]StoNS=coeffIdxRtoL;
AddCoeffRule[{coeffIdxStoNS,coeffIdx\[CapitalDelta]StoNS},{coeffStoNS,coeff\[CapitalDelta]StoNS},{Subscript[k,S->NS],Subscript[\[CapitalDelta]k,S->NS]},{coeffLtoR -> coeffStoNS*(1-coeff\[CapitalDelta]StoNS),coeffRtoL -> coeffStoNS*(1+coeff\[CapitalDelta]StoNS)}];

Print[strSeparatorCRLF,"Spontaneous decay: k(-3) - Avetisov / Goldanskij."];
Print["L -> " <> nnnStr <> "A"];
rIdxLtoAA=AddReaction[{{StandardReaction,"L -> " <> nnnStr <> "A"},{{idxL,1}},{coeffLtoAA,1},{{idxA,nnn}}}];
coeffIdxLtoAA=AddCoeffName[coeffLtoAA,Subscript[k,L->nnn A]];

Print["R -> " <> nnnStr <> "A"];
rIdxRtoAA=AddReaction[{{StandardReaction,"R -> " <> nnnStr <> "A"},{{idxR,1}},{coeffRtoAA,1},{{idxA,nnn}}}];
coeffIdxRtoAA=AddCoeffName[coeffRtoAA,Subscript[k,R->nnn A]];

coeffIdxStoAA=coeffIdxLtoAA;
coeffIdx\[CapitalDelta]StoAA=coeffIdxRtoAA;
AddCoeffRule[{coeffIdxStoAA,coeffIdx\[CapitalDelta]StoAA},{coeffStoAA,coeff\[CapitalDelta]StoAA},{Subscript[k,S->nnn A],Subscript[\[CapitalDelta]k,S->nnn A]},{coeffLtoAA -> coeffStoAA*(1-coeff\[CapitalDelta]StoAA),coeffRtoAA -> coeffStoAA*(1+coeff\[CapitalDelta]StoAA)}];

Print[strSeparatorCRLF,"Crystallization & dissolution."];
Print["L -> Cl"];
rIdxLtoCl=NAddReaction[{{CrystallizeDissolveReaction,"L -> Cl"}, {{idxL,1},{idxCl,0}},{rLMax,kCrystL,kDissL},{{idxCl,1}}}];
coeffIdxrLMax=AddCoeffName[rLMax,Superscript[Subscript[\[Rho],L],max]];
coeffIdxkCrystL=AddCoeffName[kCrystL,Subscript[c,L]];
coeffIdxkDissL=AddCoeffName[kDissL,Subscript[d,L]];

Print["R -> Cr"];
rIdxRtoCr=AddReaction[{{CrystallizeDissolveReaction,"R -> Cr"}, {{idxR,1},{idxCr,0}},{rRMax,kCrystR,kDissR},{{idxCr,1}}}];
coeffIdxrRMax=AddCoeffName[rRMax,Superscript[Subscript[\[Rho],R],max]];
coeffIdxkCrystR=AddCoeffName[kCrystR,Subscript[c,R]];
coeffIdxkDissR=AddCoeffName[kDissR,Subscript[d,R]];

coeffIdxrSMax=coeffIdxrLMax;
coeffIdx\[CapitalDelta]rSMax=coeffIdxrRMax;
AddCoeffRule[{coeffIdxrSMax,coeffIdx\[CapitalDelta]rSMax},{rSMax,\[CapitalDelta]rSMax},{Superscript[Subscript[\[Rho],S],max],Superscript[Subscript[\[CapitalDelta]\[Rho],S],max]},{rLMax -> rSMax*(1-\[CapitalDelta]rSMax),rRMax -> rSMax*(1+\[CapitalDelta]rSMax)}];

coeffIdxkCrystS=coeffIdxkCrystL;
coeffIdx\[CapitalDelta]kCrystS=coeffIdxkCrystR;
AddCoeffRule[{coeffIdxkCrystS,coeffIdx\[CapitalDelta]kCrystS},{kCrystS,\[CapitalDelta]kCrystS},{Subscript[c,S],Subscript[\[CapitalDelta]c,S]},{kCrystL -> kCrystS*(1-\[CapitalDelta]kCrystS),kCrystR -> kCrystS*(1+\[CapitalDelta]kCrystS)}];

coeffIdxkDissS=coeffIdxkDissL;
coeffIdx\[CapitalDelta]kDissS=coeffIdxkDissR;
AddCoeffRule[{coeffIdxkDissS,coeffIdx\[CapitalDelta]kDissS},{kDissS,\[CapitalDelta]kDissS},{Subscript[d,S],Subscript[\[CapitalDelta]d,S]},{kDissL -> kDissS*(1-\[CapitalDelta]kDissS),kDissR -> kDissS*(1+\[CapitalDelta]kDissS)}];

Print["Cl -> " <> nnnStr <> "A"];
rIdxClto2A=AddReaction[{{StandardReaction,"Cl -> " <> nnnStr <> "A"},{{idxCl,1}},{coeffClto2A,1},{{idxA,nnn}}}];
coeffIdxClto2A=AddCoeffName[coeffClto2A,Subscript[k,Cl->nnn A]];

Print["Cr -> " <> nnnStr <> "A"];
rIdxCrto2A=AddReaction[{{StandardReaction,"Cr -> " <> nnnStr <> "A"},{{idxCr,1}},{coeffCrto2A,1},{{idxA,nnn}}}];
coeffIdxCrto2A=AddCoeffName[coeffCrto2A,Subscript[k,Cr->nnn A]];

coeffIdxCsto2A=coeffIdxClto2A;
coeffIdx\[CapitalDelta]Csto2A=coeffIdxCrto2A;
AddCoeffRule[{coeffIdxCsto2A,coeffIdx\[CapitalDelta]Csto2A},{coeffCsto2A,coeff\[CapitalDelta]Csto2A},{Subscript[k,Cs->nnn A],Subscript[\[CapitalDelta]k,Cs->nnn A]},{coeffClto2A -> coeffCsto2A*(1-coeff\[CapitalDelta]Csto2A),coeffCrto2A -> coeffCsto2A*(1+coeff\[CapitalDelta]Csto2A)}];

Return[True];
];
(* ============================================== *)
(* \[Eta]Func is a chiral polarization or L and R *)
\[Eta]Func[idxGenerationSimple,\[Rho]AllVal:{__}]:=Module[{\[Rho]AllValLen,retVal,rLVal,rRVal},
\[Rho]AllValLen=Length[\[Rho]AllVal];

If[\[Rho]AllValLen!= idxMax,(Print["\[Eta]Func[",idxGenerationSimple ,"]::Invalid \[Rho]AllVal."]; Return[Indeterminate];)
];

rLVal=\[Rho]AllVal[[idxL]];
rRVal=\[Rho]AllVal[[idxR]];

retVal=If[rLVal>=0 && rRVal >=0,If[(rLVal+rRVal)>0,(rLVal-rRVal)/(rLVal+rRVal),0,0],Indeterminate,Indeterminate];

Return[retVal];
];
(* ============================================== *)
ApplyNewCoefficients[coeffVar:{__},coeffAllVal:{__},idxGenerationSimple,modelDescriptor:{__}]:=Module[{modelLevelDescriptor,idxAutoCatalysis,idxMutualAntagonism,idxCrystallization,coeffVal},

(* Print["ApplyNewCoefficients::G1 was called."]; *)

modelLevelDescriptor=GetModelLevelDescriptor[idxGenerationSimple,modelDescriptor];

(* Print["ApplyNewCoefficients::G1::modelLevelDescriptor = ", modelLevelDescriptor]; *)

idxAutoCatalysis=GetG1AutoCatalysisType[modelLevelDescriptor];
idxMutualAntagonism=GetG1MutualAntagonismType[modelLevelDescriptor];
idxCrystallization=GetG1CrystallizationType[modelLevelDescriptor];

(*
Print["ApplyNewCoefficients::G1::idxAutoCatalysis = ", idxAutoCatalysis];
Print["ApplyNewCoefficients::G1::idxMutualAntagonism = ", idxMutualAntagonism];
Print["ApplyNewCoefficients::G1::idxCrystallization = ", idxCrystallization];
*)

coeffVal=coeffVar;
(* Basic Synthesis. *)
coeffVal[[coeffIdxZtoA]]=coeffAllVal[[coeffIdxZtoA]];

(* Direct Synthesis. *)
coeffVal[[coeffIdxAAtoS]]=coeffAllVal[[coeffIdxAAtoS]];
coeffVal[[coeffIdx\[CapitalDelta]AAtoS]]=coeffAllVal[[coeffIdx\[CapitalDelta]AAtoS]];

(* Basic Autocatalysis. *)
If[(idxAutoCatalysis ==idxG1AutoCatalysisBasic),
(
coeffVal[[coeffIdxAAStoSS]]=coeffAllVal[[coeffIdxAAStoSS]];
coeffVal[[coeffIdx\[CapitalDelta]AAStoSS]]=coeffAllVal[[coeffIdx\[CapitalDelta]AAStoSS]];

(* Wrong Autocatalysis. *)
coeffVal[[coeffIdxAAStoNSS]]=coeffAllVal[[coeffIdxAAStoNSS]];
coeffVal[[coeffIdx\[CapitalDelta]AAStoNSS]]=coeffAllVal[[coeffIdx\[CapitalDelta]AAStoNSS]];
)
];

(* Mutual antagonism / Reversed Autocatalysis. *)
If[(idxMutualAntagonism ==idxG1MutualAntagonismBasic) ,
(
coeffVal[[coeffIdxSNSto2ANS]]=coeffAllVal[[coeffIdxSNSto2ANS]];
coeffVal[[coeffIdx\[CapitalDelta]SNSto2ANS]]=coeffAllVal[[coeffIdx\[CapitalDelta]SNSto2ANS]];

(* Wrong Mutual antagonism / Reversed Autocatalysis. *)
coeffVal[[coeffIdxSSto2AS]]=coeffAllVal[[coeffIdxSSto2AS]];
coeffVal[[coeffIdx\[CapitalDelta]SSto2AS]]=coeffAllVal[[coeffIdx\[CapitalDelta]SSto2AS]];
)
];

(* Crystallization. *)
If[(idxCrystallization ==idxG1CrystallizationS) ,
(Print["ApplyCoefficients::G1::idxG1CrystallizationS is not implemented yet !!!"]; Abort[];)
];

(* Spontaneous decay. *)
coeffVal[[coeffIdxStoAA]]=coeffAllVal[[coeffIdxStoAA]];
coeffVal[[coeffIdx\[CapitalDelta]StoAA]]=coeffAllVal[[coeffIdx\[CapitalDelta]StoAA]];

(* Racemization. *)
coeffVal[[coeffIdxStoNS]]=coeffAllVal[[coeffIdxStoNS]];
coeffVal[[coeffIdx\[CapitalDelta]StoNS]]=coeffAllVal[[coeffIdx\[CapitalDelta]StoNS]];

Return[coeffVal];
];
(* ============================================== *)
(* Type of Autocatalysis to model. *)
idxG1AutoCatalysisNone=1;
idxG1AutoCatalysisBasic=2;

idxG1AutoCatalysisMax=2;
G1AutoCatalysisMatrix={idxG1AutoCatalysisNone,idxG1AutoCatalysisBasic};

(* Type of Mutual Antagonism to model. *)
idxG1MutualAntagonismNone=1;
idxG1MutualAntagonismBasic=2;

idxG1MutualAntagonismMax=2;
G1MutualAntagonismMatrix={idxG1MutualAntagonismNone,idxG1MutualAntagonismBasic};

(* Type of Crystallization to model. *)
idxG1CrystallizationNone=1;
idxG1CrystallizationS=2;

idxG1CrystallizationMax=1;
G1CrystallizationMatrix={idxG1CrystallizationNone};
(* ============================================== *)
GetG1AutoCatalysisType[modelDescriptor:{__}]:=modelDescriptor[[1]];
GetG1MutualAntagonismType[modelDescriptor:{__}]:=modelDescriptor[[2]];
GetG1CrystallizationType[modelDescriptor:{__}]:=modelDescriptor[[3]];
(* ============================================== *)
GetG1AutoCatalysisName[idx_?IntegerQ]:=Module[{retVal},
retVal="!!! Undefined !!!";
If[idx== idxG1AutoCatalysisNone,retVal="None"];
If[idx== idxG1AutoCatalysisBasic,retVal="Basic"];

Return[retVal];
];
(* ============================================== *)
GetG1MutualAntagonismName[idx_?IntegerQ]:=Module[{retVal},
retVal="!!! Undefined !!!";
If[idx== idxG1MutualAntagonismNone,retVal="None"];
If[idx== idxG1MutualAntagonismBasic,retVal="Basic"];

Return[retVal];
];
(* ============================================== *)
GetG1CrystallizationName[idx_?IntegerQ]:=Module[{retVal},
retVal="!!! Undefined !!!";
If[idx== idxG1CrystallizationNone,retVal="None"];
If[idx== idxG1CrystallizationS,retVal="S"];

Return[retVal];
];
(* ============================================== *)
GetAllModelsDescriptorMatrix[idxGenerationSimple]:=Flatten[Table[{G1AutoCatalysisMatrix[[ii]],G1MutualAntagonismMatrix[[jj]],G1CrystallizationMatrix[[ll]]},{ii,1,idxG1AutoCatalysisMax},{jj,1,idxG1MutualAntagonismMax},{ll,1,idxG1CrystallizationMax}],2];
(* ============================================== *)
IsModelLevelDescriptorValid[idxGenerationSimple,modelLevelDescriptor:{__}]:=True;
(* ============================================== *)
GetGenerationName[idxGenerationSimple,modelLevelDescriptor:{__}]:=Module[{retVal,idxAutoCatalysis,idxMutualAntagonism,idxCrystallization},
(* Print["GetGenerationName::G1::modelLevelDescriptor = ", modelLevelDescriptor]; *)

retVal="!!! Undefined !!!";

If[Length[modelLevelDescriptor] == 3,
(
idxAutoCatalysis=GetG1AutoCatalysisType[modelLevelDescriptor];
idxMutualAntagonism=GetG1MutualAntagonismType[modelLevelDescriptor];
idxCrystallization=GetG1CrystallizationType[modelLevelDescriptor];
(*
Print["GetGenerationName::G1::idxAutoCatalysis = ", idxAutoCatalysis];
Print["GetGenerationName::G1::idxMutualAntagonism = ", idxMutualAntagonism];
Print["GetGenerationName::G1::idxCrystallization = ", idxCrystallization];
*)
retVal="Autocatalysis: " <> GetG1AutoCatalysisName[idxAutoCatalysis] <> ", Mutual Antagonism: " <> GetG1MutualAntagonismName[idxMutualAntagonism] <> ", Crystallization: " <> GetG1CrystallizationName[idxCrystallization];
),
(
retVal="GetGenerationName::G1::Incorrect modelLevelDescriptor";
)
];

Return[retVal];
];
(* ============================================== *)
(* New values are used!!! *)
ApplyRacemization[values:{__},mValue_,idxGenerationSimple,rawOptions___]:=Module[{retVal,opts,multStoNSval,useVariableRacemizationForAllVal},
(* Print["ApplyRacemization::G1 was called."]; *)
retVal=values;
opts=ProcessOptions[rawOptions];
useVariableRacemizationForAllVal=UseVariableRacemizationForAll /. opts /. Options[CLMS];

If[useVariableRacemizationForAllVal== True,
(
multStoNSval = MultStoNS /.opts /. Options[CLMS];
retVal[[coeffIdxStoNS]]=multStoNSval*mValue;
)
];

Return[retVal];
];
(* ============================================== *)
