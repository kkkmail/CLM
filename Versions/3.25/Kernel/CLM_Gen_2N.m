(* :Summary: 2nd generation CLM model. *)
(* :Author: Konstantin K. Konstantinov *)
(* :Email: konstantin.k.konstantinov@gmail.com *)
(* :License type: GPL *)
(* :Copyright: K^3, 2013 - 2014 *)
(* :Version: Revision: 2.05.001, Date: 2014/09/10 *)
(* :Mathematica Version: 7.0 - 10.0 *)
(* ============================================== *)
Print["CLM_Gen_2N::TODO::Check and fix NextGenerationNo[] calls."];
idxGenerationSecond=NextGenerationNo[];
SetMaxGenerationLevel[idxGenerationSecond];
IsGenerationDefined[idxGenerationSecond]:=True;
(* ============================================== *)
InitializeGeneration[idxGenerationSecond]:=Module[{},
Print[strSeparatorCRLF,"Substances - 2nd generation."];

idxBlr=AddSubstance[rBlr,Subscript[\[Rho],LD],{{idxAtomZ,2*nnn}}];
idxBll=AddSubstance[rBll,Subscript[\[Rho],LL],{{idxAtomZ,2*nnn}}];
idxBrr=AddSubstance[rBrr,Subscript[\[Rho],DD],{{idxAtomZ,2*nnn}}];
idxClr=AddSubstance[rClr,Subscript[\[Rho],Cld],{{idxAtomZ,2*nnn}}];
idxCll=AddSubstance[rCll,Subscript[\[Rho],Cll],{{idxAtomZ,2*nnn}}];
idxCrr=AddSubstance[rCrr,Subscript[\[Rho],Cdd],{{idxAtomZ,2*nnn}}];

(* Signature of the ReactionInfo object is as follows: *)
(* {{reactionFunc,reactionDescription},{{source1,number1,pow1},...},{parameters,...},{{result1,number1},...}} *)

Print[strSeparatorCRLF,"Soai type autocatalysis."];
Print[nnnStr <>"A + Bll -> L + Bll"];
rIdxAABlltoLBll=AddReaction[{{StandardReaction,nnnStr <> "A + Bll -> L + Bll"},{{idxA,nnn},{idxBll,1}},{coeffAABlltoLBll,nnn,1},{{idxL,1},{idxBll,1}}}];
coeffIdxAABlltoLBll=AddCoeffName[coeffAABlltoLBll,Subscript[k,nnn A+LL->L+LL]];

Print[nnnStr <> "A + Brr -> R + Brr"];
rIdxAABrrtoRBrr=AddReaction[{{StandardReaction,nnnStr <> "A + Brr -> R + Brr"},{{idxA,nnn},{idxBrr,1}},{coeffAABrrtoRBrr,nnn,1},{{idxR,1},{idxBrr,1}}}];
coeffIdxAABrrtoRBrr=AddCoeffName[coeffAABrrtoRBrr,Subscript[k,nnn A+RR->R+RR]];

coeffIdxAABsstoSBss=coeffIdxAABlltoLBll;
coeffIdx\[CapitalDelta]AABsstoSBss=coeffIdxAABrrtoRBrr;
AddCoeffRule[{coeffIdxAABsstoSBss,coeffIdx\[CapitalDelta]AABsstoSBss},{coeffAABsstoSBss,coeff\[CapitalDelta]AABsstoSBss},{Subscript[k,nnn A+SS->S+SS],Subscript[\[CapitalDelta]k,nnn A+SS->S+SS]},{coeffAABlltoLBll -> coeffAABsstoSBss*(1-coeff\[CapitalDelta]AABsstoSBss),coeffAABrrtoRBrr -> coeffAABsstoSBss*(1+coeff\[CapitalDelta]AABsstoSBss)}];

Print["TODO: Add various wrong Soai (and other) autocalysis types & reversed reactions."];

Print[strSeparatorCRLF,"Second Generation Basic autocatalysis."];
Print["2L + Bll -> 2Bll"];
rIdxLLBllto2Bll=AddReaction[{{StandardReaction,"2L + Bll -> 2Bll"},{{idxL,2},{idxBll,1}},{coeffLLBllto2Bll,2,1},{{idxBll,2}}}];
coeffIdxLLBllto2Bll=AddCoeffName[coeffLLBllto2Bll,Subscript[k,2L+LL->2LL]];

Print["2R + Brr -> 2Brr"];
rIdxRRBrrto2Brr=AddReaction[{{StandardReaction,"2R + Brr -> 2Brr"},{{idxR,2},{idxBrr,1}},{coeffRRBrrto2Brr,2,1},{{idxBrr,2}}}];
coeffIdxRRBrrto2Brr=AddCoeffName[coeffRRBrrto2Brr,Subscript[k,2R+RR->2RR]];

coeffIdxSSBssto2Bss=coeffIdxLLBllto2Bll;
coeffIdx\[CapitalDelta]SSBssto2Bss=coeffIdxRRBrrto2Brr;
AddCoeffRule[{coeffIdxSSBssto2Bss,coeffIdx\[CapitalDelta]SSBssto2Bss},{coeffSSBssto2Bss,coeff\[CapitalDelta]SSBssto2Bss},{Subscript[k,2S+SS->2SS],Subscript[\[CapitalDelta]k,2S+SS->2SS]},{coeffLLBllto2Bll -> coeffSSBssto2Bss*(1-coeff\[CapitalDelta]SSBssto2Bss),coeffRRBrrto2Brr -> coeffSSBssto2Bss*(1+coeff\[CapitalDelta]SSBssto2Bss)}];

Print[strSeparatorCRLF,"Second Level autocatalysis."];
Print[nnn2Str <> "A + Bll -> 2Bll"];
rIdx4ABllto2Bll=AddReaction[{{StandardReaction,nnn2Str <> "A + Bll -> 2Bll"},{{idxA,2*nnn},{idxBll,1}},{coeff4ABllto2Bll,2*nnn,1},{{idxBll,2}}}];
coeffIdx4ABllto2Bll=AddCoeffName[coeff4ABllto2Bll,Subscript[k,2 nnn A+Bll->2Bll]];

Print[nnn2Str <> "A + Brr -> 2Brr"];
rIdx4ABrrto2Brr=AddReaction[{{StandardReaction,nnn2Str <> "A + Brr -> 2Brr"},{{idxA,2*nnn},{idxBrr,1}},{coeff4ABrrto2Brr,2*nnn,1},{{idxBrr,2}}}];
coeffIdx4ABrrto2Brr=AddCoeffName[coeff4ABrrto2Brr,Subscript[k,2 nnn A+Brr->2Brr]];

coeffIdx4ABssto2Bss=coeffIdx4ABllto2Bll;
coeffIdx\[CapitalDelta]4ABssto2Bss=coeffIdx4ABrrto2Brr;
AddCoeffRule[{coeffIdx4ABssto2Bss,coeffIdx\[CapitalDelta]4ABssto2Bss},{coeff4ABssto2Bss,coeff\[CapitalDelta]4ABssto2Bss},{Subscript[k,2 nnn A+SS->S+SS],Subscript[\[CapitalDelta]k,2 nnn A+SS->S+SS]},{coeff4ABllto2Bll -> coeff4ABssto2Bss*(1-coeff\[CapitalDelta]4ABssto2Bss),coeff4ABrrto2Brr -> coeff4ABssto2Bss*(1+coeff\[CapitalDelta]4ABssto2Bss)}];

Print[strSeparatorCRLF,"Mutual antagonism (induced decay) of Bll - Brr into 2L - 2R."];
Print["Bll + Brr -> 2L + Brr"];
rIdxBllBrrto2LBrr=AddReaction[{{StandardReaction,"Bll + Brr -> 2L + Brr"},{{idxBll,1},{idxBrr,1}},{coeffBllBrrto2LBrr,1,1},{{idxL,2},{idxBrr,1}}}];
coeffIdxBllBrrto2LBrr=AddCoeffName[coeffBllBrrto2LBrr,Subscript[k,Bll+Brr->2L+Brr]];

Print["Brr + Bll -> 2R + Bll"];
rIdxBrrBllto2RBll=AddReaction[{{StandardReaction,"Brr + Bll -> 2R + Bll"},{{idxBrr,1},{idxBll,1}},{coeffBrrBllto2RBll,1,1},{{idxR,2},{idxBll,1}}}];
coeffIdxBrrBllto2RBll=AddCoeffName[coeffBrrBllto2RBll,Subscript[k,Brr+Bll->2R+Bll]];

coeffIdxBssNBssto2SNBss=coeffIdxBllBrrto2LBrr;
coeffIdx\[CapitalDelta]BssNBssto2SNBss=coeffIdxBrrBllto2RBll;
AddCoeffRule[{coeffIdxBssNBssto2SNBss,coeffIdx\[CapitalDelta]BssNBssto2SNBss},{coeffBssNBssto2SNBss,coeff\[CapitalDelta]BssNBssto2SNBss},{Subscript[k,Bss+NBss->2S+NBss],Subscript[\[CapitalDelta]k,Bss+NBss->2S+NBss]},{coeffBllBrrto2LBrr -> coeffBssNBssto2SNBss*(1-coeff\[CapitalDelta]BssNBssto2SNBss),coeffBrrBllto2RBll -> coeffBssNBssto2SNBss*(1+coeff\[CapitalDelta]BssNBssto2SNBss)}];

Print[strSeparatorCRLF,"Inter generation mutual antagonism of Bll - Brr into 2L - 2R."];
Print["Bll + R -> 2L + R"];
rIdxBllRto2LR=AddReaction[{{StandardReaction,"Bll + R -> 2L + R"},{{idxBll,1},{idxR,1}},{coeffBllRto2LR,1,1},{{idxL,2},{idxR,1}}}];
coeffIdxBllRto2LR=AddCoeffName[coeffBllRto2LR,Subscript[k,Bll+R->2L+R]];

Print["Brr + L -> 2R + L"];
rIdxBrrLto2RL=AddReaction[{{StandardReaction,"Brr + L -> 2R + L"},{{idxBrr,1},{idxL,1}},{coeffBrrLto2RL,1,1},{{idxR,2},{idxL,1}}}];
coeffIdxBrrLto2RL=AddCoeffName[coeffBrrLto2RL,Subscript[k,Brr+L->2R+L]];

coeffIdxBssNSto2SNS=coeffIdxBllRto2LR;
coeffIdx\[CapitalDelta]BssNSto2SNS=coeffIdxBrrLto2RL;
AddCoeffRule[{coeffIdxBssNSto2SNS,coeffIdx\[CapitalDelta]BssNSto2SNS},{coeffBssNSto2SNS,coeff\[CapitalDelta]BssNSto2SNS},{Subscript[k,Bss+NS->2S+NS],Subscript[\[CapitalDelta]k,Bss+NS->2S+NS]},{coeffBllRto2LR -> coeffBssNSto2SNS*(1-coeff\[CapitalDelta]BssNSto2SNS),coeffBrrLto2RL -> coeffBssNSto2SNS*(1+coeff\[CapitalDelta]BssNSto2SNS)}];

Print[strSeparatorCRLF,"Pair merging."];
Print["L + R -> Blr"];
rIdxLRtoBlr=AddReaction[{{StandardReaction,"L + R -> Blr"}, {{idxL,1},{idxR,1}},{coeffLRtoBlr,1,1},{{idxBlr,1}}}];
coeffIdxLRtoBlr=AddCoeffName[coeffLRtoBlr,Subscript[k,L+R->LR]];

Print["Blr -> L + R"];
rIdxBlrtoLR=AddReaction[{{StandardReaction,"Blr -> L + R"},{{idxBlr,1}},{coeffBlrtoLR,1},{{idxL,1},{idxR,1}}}];
coeffIdxBlrtoLR=AddCoeffName[coeffBlrtoLR,Subscript[k,LR->L+R]];

Print["L + L -> Bll"];
rIdxLLtoBll=AddReaction[{{StandardReaction,"L + L -> Bll"}, {{idxL,2}},{coeffLLtoBll,2},{{idxBll,1}}}];
coeffIdxLLtoBll=AddCoeffName[coeffLLtoBll,Subscript[k,L+L->LL]];

Print["R + R -> Brr"];
rIdxRRtoBrr=AddReaction[{{StandardReaction,"R + R -> Brr"}, {{idxR,2}},{coeffRRtoBrr,2},{{idxBrr,1}}}];
coeffIdxRRtoBrr=AddCoeffName[coeffRRtoBrr,Subscript[k,R+R->RR]];

coeffIdxSStoBss=coeffIdxLLtoBll;
coeffIdx\[CapitalDelta]SStoBss=coeffIdxRRtoBrr;
AddCoeffRule[{coeffIdxSStoBss,coeffIdx\[CapitalDelta]SStoBss},{coeffSStoBss,coeff\[CapitalDelta]SStoBss},{Subscript[k,S+S->SS],Subscript[\[CapitalDelta]k,S+S->SS]},{coeffLLtoBll -> coeffSStoBss*(1-coeff\[CapitalDelta]SStoBss),coeffRRtoBrr -> coeffSStoBss*(1+coeff\[CapitalDelta]SStoBss)}];

Print["Bll -> L + L"];
rIdxBlltoLL=AddReaction[{{StandardReaction,"Bll -> L + L"},{{idxBll,1}},{coeffBlltoLL,1},{{idxL,2}}}];
coeffIdxBlltoLL=AddCoeffName[coeffBlltoLL,Subscript[k,LL->L+L]];

Print["Brr -> R + R"];
rIdxBrrtoRR=AddReaction[{{StandardReaction,"Brr -> R + R"},{{idxBrr,1}},{coeffBrrtoRR,1},{{idxR,2}}}];
coeffIdxBrrtoRR=AddCoeffName[coeffBrrtoRR,Subscript[k,RR->R+R]];

coeffIdxBsstoSS=coeffIdxBlltoLL;
coeffIdx\[CapitalDelta]BsstoSS=coeffIdxBrrtoRR;
AddCoeffRule[{coeffIdxBsstoSS,coeffIdx\[CapitalDelta]BsstoSS},{coeffBsstoSS,coeff\[CapitalDelta]BsstoSS},{Subscript[k,SS->S+S],Subscript[\[CapitalDelta]k,SS->S+S]},{coeffBlltoLL -> coeffBsstoSS*(1-coeff\[CapitalDelta]BsstoSS),coeffBrrtoRR -> coeffBsstoSS*(1+coeff\[CapitalDelta]BsstoSS)}];

Print[strSeparatorCRLF,"Pair full decay."];
Print["Blr -> " <> nnn2Str <> "A"];
rIdxBlrto4A=AddReaction[{{StandardReaction,"Blr -> " <> nnn2Str <> "A"},{{idxBlr,1}},{coeffBlrto4A,1},{{idxA,2*nnn}}}];
coeffIdxBlrto4A=AddCoeffName[coeffBlrto4A,Subscript[k,LR->2 nnn A]];

Print["Bll -> " <> nnn2Str <> "A"];
rIdxBllto4A=AddReaction[{{StandardReaction,"Bll -> " <> nnn2Str <> "A"},{{idxBll,1}},{coeffBllto4A,1},{{idxA,2*nnn}}}];
coeffIdxBllto4A=AddCoeffName[coeffBllto4A,Subscript[k,LL->2 nnn A]];

Print["Brr -> " <> nnn2Str <> "A"];
rIdxBrrto4A=AddReaction[{{StandardReaction,"Brr -> " <> nnn2Str <> "A"},{{idxBrr,1}},{coeffBrrto4A,1},{{idxA,2*nnn}}}];
coeffIdxBrrto4A=AddCoeffName[coeffBrrto4A,Subscript[k,RR->2 nnn A]];

coeffIdxBssto4A=coeffIdxBllto4A;
coeffIdx\[CapitalDelta]Bssto4A=coeffIdxBrrto4A;
AddCoeffRule[{coeffIdxBssto4A,coeffIdx\[CapitalDelta]Bssto4A},{coeffBssto4A,coeff\[CapitalDelta]Bssto4A},{Subscript[k,SS->2 nnn A],Subscript[\[CapitalDelta]k,SS->2 nnn A]},{coeffBllto4A -> coeffBssto4A*(1-coeff\[CapitalDelta]Bssto4A),coeffBrrto4A -> coeffBssto4A*(1+coeff\[CapitalDelta]Bssto4A)}];

Print[strSeparatorCRLF,"Racemization of Bll, Blr, Brr."];
Print["Bll -> Blr"];
rIdxBlltoBlr=AddReaction[{{StandardReaction,"Bll -> Blr"},{{idxBll,1}},{coeffBlltoBlr,1},{{idxBlr,1}}}];
coeffIdxBlltoBlr=AddCoeffName[coeffBlltoBlr,Subscript[k,LL->LR]];

Print["Brr -> Blr"];
rIdxBrrtoBlr=AddReaction[{{StandardReaction,"Brr -> Blr"},{{idxBrr,1}},{coeffBrrtoBlr,1},{{idxBlr,1}}}];
coeffIdxBrrtoBlr=AddCoeffName[coeffBrrtoBlr,Subscript[k,RR->LR]];

coeffIdxBsstoBlr=coeffIdxBlltoBlr;
coeffIdx\[CapitalDelta]BsstoBlr=coeffIdxBrrtoBlr;
AddCoeffRule[{coeffIdxBsstoBlr,coeffIdx\[CapitalDelta]BsstoBlr},{coeffBsstoBlr,coeff\[CapitalDelta]BsstoBlr},{Subscript[k,SS->LR],Subscript[\[CapitalDelta]k,SS->LR]},{coeffBlltoBlr -> coeffBsstoBlr*(1-coeff\[CapitalDelta]BsstoBlr),coeffBrrtoBlr -> coeffBsstoBlr*(1+coeff\[CapitalDelta]BsstoBlr)}];


Print["Blr -> Bll"];
rIdxBlrtoBll=AddReaction[{{StandardReaction,"Blr -> Bll"},{{idxBlr,1}},{coeffBlrtoBll,1},{{idxBll,1}}}];
coeffIdxBlrtoBll=AddCoeffName[coeffBlrtoBll,Subscript[k,LR->LL]];

Print["Blr -> Brr"];
rIdxBlrtoBrr=AddReaction[{{StandardReaction,"Blr -> Brr"},{{idxBlr,1}},{coeffBlrtoBrr,1},{{idxBrr,1}}}];
coeffIdxBlrtoBrr=AddCoeffName[coeffBlrtoBrr,Subscript[k,LR->RR]];

coeffIdxBlrtoBss=coeffIdxBlrtoBll;
coeffIdx\[CapitalDelta]BlrtoBss=coeffIdxBlrtoBrr;
AddCoeffRule[{coeffIdxBlrtoBss,coeffIdx\[CapitalDelta]BlrtoBss},{coeffBlrtoBss,coeff\[CapitalDelta]BlrtoBss},{Subscript[k,LR->SS],Subscript[\[CapitalDelta]k,LR->SS]},{coeffBlrtoBll -> coeffBlrtoBss*(1-coeff\[CapitalDelta]BlrtoBss),coeffBlrtoBrr -> coeffBlrtoBss*(1+coeff\[CapitalDelta]BlrtoBss)}];

Print[strSeparatorCRLF,"Crystallization & dissolution."];
Print["Blr -> Clr"]; 
rIdxBlrtoClr=AddReaction[{{CrystallizeDissolveReaction,"Blr -> Clr"}, {{idxBlr,1},{idxClr,0}},{rBlrMax,kCrystBlr,kDissBlr},{{idxClr,1}}}];
coeffIdxrBlrMax=AddCoeffName[rBlrMax,Superscript[Subscript[\[Rho],LR],max]];
coeffIdxkCrystBlr=AddCoeffName[kCrystBlr,Subscript[c,LR]];
coeffIdxkDissBlr=AddCoeffName[kDissBlr,Subscript[d,LR]];

Print["Bll -> Cll"];
rIdxBlltoCll=AddReaction[{{CrystallizeDissolveReaction,"Bll -> Cll"}, {{idxBll,1},{idxCll,0}},{rBllMax,kCrystBll,kDissBll},{{idxCll,1}}}];
coeffIdxrBllMax=AddCoeffName[rBllMax,Superscript[Subscript[\[Rho],LL],max]];
coeffIdxkCrystBll=AddCoeffName[kCrystBll,Subscript[c,LL]];
coeffIdxkDissBll=AddCoeffName[kDissBll,Subscript[d,LL]];

Print["Brr -> Crr"];
rIdxBrrtoCrr=AddReaction[{{CrystallizeDissolveReaction,"Brr -> Crr"}, {{idxBrr,1},{idxCrr,0}},{rBrrMax,kCrystBrr,kDissBrr},{{idxCrr,1}}}];
coeffIdxrBrrMax=AddCoeffName[rBrrMax,Superscript[Subscript[\[Rho],RR],max]];
coeffIdxkCrystBrr=AddCoeffName[kCrystBrr,Subscript[c,RR]];
coeffIdxkDissBrr=AddCoeffName[kDissBrr,Subscript[d,RR]];

coeffIdxrBssMax=coeffIdxrBllMax;
coeffIdx\[CapitalDelta]rBssMax=coeffIdxrBrrMax;
AddCoeffRule[{coeffIdxrBssMax,coeffIdx\[CapitalDelta]rBssMax},{rBssMax,\[CapitalDelta]rBssMax},{Superscript[Subscript[\[Rho],SS],max],Superscript[Subscript[\[CapitalDelta]\[Rho],SS],max]},{rBllMax -> rBssMax*(1-\[CapitalDelta]rBssMax),rBrrMax -> rBssMax*(1+\[CapitalDelta]rBssMax)}];

coeffIdxkCrystBss=coeffIdxkCrystBll;
coeffIdx\[CapitalDelta]kCrystBss=coeffIdxkCrystBrr;
AddCoeffRule[{coeffIdxkCrystBss,coeffIdx\[CapitalDelta]kCrystBss},{kCrystBss,\[CapitalDelta]kCrystBss},{Subscript[c,SS],Subscript[\[CapitalDelta]c,SS]},{kCrystBll -> kCrystBss*(1-\[CapitalDelta]kCrystBss),kCrystBrr -> kCrystBss*(1+\[CapitalDelta]kCrystBss)}];

coeffIdxkDissBss=coeffIdxkDissBll;
coeffIdx\[CapitalDelta]kDissBss=coeffIdxkDissBrr;
AddCoeffRule[{coeffIdxkDissBss,coeffIdx\[CapitalDelta]kDissBss},{kDissBss,\[CapitalDelta]kDissBss},{Subscript[d,SS],Subscript[\[CapitalDelta]d,SS]},{kDissBll -> kDissBss*(1-\[CapitalDelta]kDissBss),kDissBrr -> kDissBss*(1+\[CapitalDelta]kDissBss)}];

Print[strSeparatorCRLF,"Racemization of Cll, Clr, Crr."];
Print["Cll -> Clr"];
rIdxClltoClr=AddReaction[{{StandardReaction,"Cll -> Clr"},{{idxCll,1}},{coeffClltoClr,1},{{idxClr,1}}}];
coeffIdxClltoClr=AddCoeffName[coeffClltoClr,Subscript[k,Cll->Clr]];

Print["Crr -> Clr"];
rIdxCrrtoClr=AddReaction[{{StandardReaction,"Crr -> Clr"},{{idxCrr,1}},{coeffCrrtoClr,1},{{idxClr,1}}}];
coeffIdxCrrtoClr=AddCoeffName[coeffCrrtoClr,Subscript[k,Crr->Clr]];

coeffIdxCsstoClr=coeffIdxClltoClr;
coeffIdx\[CapitalDelta]CsstoClr=coeffIdxCrrtoClr;
AddCoeffRule[{coeffIdxCsstoClr,coeffIdx\[CapitalDelta]CsstoClr},{coeffCsstoClr,coeff\[CapitalDelta]CsstoClr},{Subscript[k,Css->Clr],Subscript[\[CapitalDelta]k,Css->Clr]},{coeffClltoClr -> coeffCsstoClr*(1-coeff\[CapitalDelta]CsstoClr),coeffCrrtoClr -> coeffCsstoClr*(1+coeff\[CapitalDelta]CsstoClr)}];

Print["Clr -> Cll"];
rIdxClrtoCll=AddReaction[{{StandardReaction,"Clr -> Cll"},{{idxClr,1}},{coeffClrtoCll,1},{{idxCll,1}}}];
coeffIdxClrtoCll=AddCoeffName[coeffClrtoCll,Subscript[k,Clr->Cll]];

Print["Clr -> Crr"];
rIdxClrtoCrr=AddReaction[{{StandardReaction,"Clr -> Crr"},{{idxClr,1}},{coeffClrtoCrr,1},{{idxCrr,1}}}];
coeffIdxClrtoCrr=AddCoeffName[coeffClrtoCrr,Subscript[k,Clr->Crr]];

coeffIdxClrtoCss=coeffIdxClrtoCll;
coeffIdx\[CapitalDelta]ClrtoCss=coeffIdxClrtoCrr;
AddCoeffRule[{coeffIdxClrtoCss,coeffIdx\[CapitalDelta]ClrtoCss},{coeffClrtoCss,coeff\[CapitalDelta]ClrtoCss},{Subscript[k,Clr->Css],Subscript[\[CapitalDelta]k,Clr->Css]},{coeffClrtoCll -> coeffClrtoCss*(1-coeff\[CapitalDelta]ClrtoCss),coeffClrtoCrr -> coeffClrtoCss*(1+coeff\[CapitalDelta]ClrtoCss)}];

Print[strSeparatorCRLF,"Crystal full decay."];
Print["Clr -> " <> nnn2Str <> "A"];
rIdxClrto4A=AddReaction[{{StandardReaction,"Clr -> " <> nnn2Str <> "A"},{{idxClr,1}},{coeffClrto4A,1},{{idxA,2*nnn}}}];
coeffIdxClrto4A=AddCoeffName[coeffClrto4A,Subscript[k,Clr->2 nnn A]];

Print["Cll -> " <> nnn2Str <> "A"];
rIdxCllto4A=AddReaction[{{StandardReaction,"Cll -> " <> nnn2Str <> "A"},{{idxCll,1}},{coeffCllto4A,1},{{idxA,2*nnn}}}];
coeffIdxCllto4A=AddCoeffName[coeffCllto4A,Subscript[k,Cll->2 nnn A]];

Print["Crr -> " <> nnn2Str <> "A"];
rIdxCrrto4A=AddReaction[{{StandardReaction,"Crr -> " <> nnn2Str <> "A"},{{idxCrr,1}},{coeffCrrto4A,1},{{idxA,2*nnn}}}];
coeffIdxCrrto4A=AddCoeffName[coeffCrrto4A,Subscript[k,Crr->2 nnn A]];

coeffIdxCssto4A=coeffIdxCllto4A;
coeffIdx\[CapitalDelta]Cssto4A=coeffIdxCrrto4A;
AddCoeffRule[{coeffIdxCssto4A,coeffIdx\[CapitalDelta]Cssto4A},{coeffCssto4A,coeff\[CapitalDelta]Cssto4A},{Subscript[k,Css->2 nnn A],Subscript[\[CapitalDelta]k,Css->2 nnn A]},{coeffCllto4A -> coeffCssto4A*(1-coeff\[CapitalDelta]Cssto4A),coeffCrrto4A -> coeffCssto4A*(1+coeff\[CapitalDelta]Cssto4A)}];

Return[True];
];
(* ============================================== *)
(* \[Eta]Func is a chiral polarization or Bll and Brr *)
\[Eta]Func[idxGenerationSecond,\[Rho]AllVal:{__}]:=Module[{\[Rho]AllValLen,retVal,rLVal,rRVal},
\[Rho]AllValLen=Length[\[Rho]AllVal];

If[\[Rho]AllValLen!= idxMax,(Print["\[Eta]Func[",idxGenerationSecond ,"]::Invalid \[Rho]AllVal."]; Return[Indeterminate];)
];

rLVal=\[Rho]AllVal[[idxBll]];
rRVal=\[Rho]AllVal[[idxBrr]];

retVal=If[rLVal>=0 && rRVal >=0,If[(rLVal+rRVal)>0,(rLVal-rRVal)/(rLVal+rRVal),0,0],Indeterminate,Indeterminate];

Return[retVal];
];
(* ============================================== *)
ApplyNewCoefficients[coeffVar:{__},coeffAllVal:{__},idxGenerationSecond,modelDescriptor:{__}]:=Module[{modelLevelDescriptor,idxAutoCatalysis,idxMutualAntagonism,idxCrystallization,idxPairMerging,coeffVal},

(* Print["ApplyNewCoefficients::G2 was called."]; *)

modelLevelDescriptor=GetModelLevelDescriptor[idxGenerationSecond,modelDescriptor];
(* Print["ApplyNewCoefficients::G2::modelLevelDescriptor = ", modelLevelDescriptor]; *)

idxPairMerging=GetG2PairMergingType[modelLevelDescriptor];
idxAutoCatalysis=GetG2AutoCatalysisType[modelLevelDescriptor];
idxMutualAntagonism=GetG2MutualAntagonismType[modelLevelDescriptor];
idxCrystallization=GetG2CrystallizationType[modelLevelDescriptor];

coeffVal=coeffVar;

(* Pair merging. *)
If[(idxPairMerging ==idxG2PairMergingAll),
(
coeffVal[[coeffIdxLRtoBlr]]=coeffAllVal[[coeffIdxLRtoBlr]];
coeffVal[[coeffIdxBlrtoLR]]=coeffAllVal[[coeffIdxBlrtoLR]];

coeffVal[[coeffIdxSStoBss]]=coeffAllVal[[coeffIdxSStoBss]];
coeffVal[[coeffIdx\[CapitalDelta]SStoBss]]=coeffAllVal[[coeffIdx\[CapitalDelta]SStoBss]];

coeffVal[[coeffIdxBsstoSS]]=coeffAllVal[[coeffIdxBsstoSS]];
coeffVal[[coeffIdx\[CapitalDelta]BsstoSS]]=coeffAllVal[[coeffIdx\[CapitalDelta]BsstoSS]];

(* ======================================================\[Equal] *)
(* ======================================================\[Equal] *)

(* Soai type autocatalysis. *)
If[(idxAutoCatalysis ==idxG2AutoCatalysisSoai) ,
(
coeffVal[[coeffIdxAABsstoSBss]]=coeffAllVal[[coeffIdxAABsstoSBss]];
coeffVal[[coeffIdx\[CapitalDelta]AABsstoSBss]]=coeffAllVal[[coeffIdx\[CapitalDelta]AABsstoSBss]];
)
];

(* Basic autocatalysis. *)
If[(idxAutoCatalysis ==idxG2AutoCatalysisBasic),
(
coeffVal[[coeffIdxSSBssto2Bss]]=coeffAllVal[[coeffIdxSSBssto2Bss]];
coeffVal[[coeffIdx\[CapitalDelta]SSBssto2Bss]]=coeffAllVal[[coeffIdx\[CapitalDelta]SSBssto2Bss]];
)
];

(* Second Level autocatalysis. *)
If[(idxAutoCatalysis ==idxG2AutoCatalysisSecondLevel),
(
coeffVal[[coeffIdx4ABssto2Bss]]=coeffAllVal[[coeffIdx4ABssto2Bss]];
coeffVal[[coeffIdx\[CapitalDelta]4ABssto2Bss]]=coeffAllVal[[coeffIdx\[CapitalDelta]4ABssto2Bss]];
)
];

(* Basic Mutual Antagonism. *)
If[(idxMutualAntagonism ==idxG2MutualAntagonismBasic) || (idxMutualAntagonism ==idxG2MutualAntagonismAll) ,
(
coeffVal[[coeffIdxBssNBssto2SNBss]]=coeffAllVal[[coeffIdxBssNBssto2SNBss]];
coeffVal[[coeffIdx\[CapitalDelta]BssNBssto2SNBss]]=coeffAllVal[[coeffIdx\[CapitalDelta]BssNBssto2SNBss]];
)
];

(* Inter Generation Mutual Antagonism. *)
If[(idxMutualAntagonism ==idxG2MutualAntagonismInterGeneration) || (idxMutualAntagonism ==idxG2MutualAntagonismAll) ,
(
coeffVal[[coeffIdxBssNSto2SNS]]=coeffAllVal[[coeffIdxBssNSto2SNS]];
coeffVal[[coeffIdx\[CapitalDelta]BssNSto2SNS]]=coeffAllVal[[coeffIdx\[CapitalDelta]BssNSto2SNS]];
)
];

(* TODO - add deltas *)
(* Crystallization of Blr & decaly of Blr into 4A. *)
If[(idxCrystallization ==idxG2CrystallizationBlr) ||(idxCrystallization ==idxG2CrystallizationAll),
(
coeffVal[[coeffIdxrBlrMax]]=coeffAllVal[[coeffIdxrBlrMax]];
coeffVal[[coeffIdxkCrystBlr]]=coeffAllVal[[coeffIdxkCrystBlr]];
coeffVal[[coeffIdxkDissBlr]]=coeffAllVal[[coeffIdxkDissBlr]];

coeffVal[[coeffIdxClrto4A]]=coeffAllVal[[coeffIdxClrto4A]];
)
];

(* Crystallization of Bss & decaly of Bss into 4A. *)
If[idxCrystallization ==idxG2CrystallizationAll,
(
coeffVal[[coeffIdxrBssMax]]=coeffAllVal[[coeffIdxrBssMax]];
coeffVal[[coeffIdxkCrystBss]]=coeffAllVal[[coeffIdxkCrystBss]];
coeffVal[[coeffIdxkDissBss]]=coeffAllVal[[coeffIdxkDissBss]];

coeffVal[[coeffIdxrSMax]]=coeffAllVal[[coeffIdxrSMax]];
coeffVal[[coeffIdxkCrystS]]=coeffAllVal[[coeffIdxkCrystS]];
coeffVal[[coeffIdxkDissS]]=coeffAllVal[[coeffIdxkDissS]];

coeffVal[[coeffIdxCssto4A]]=coeffAllVal[[coeffIdxCssto4A]];
)
];

(* Racemization Bss \[Rule] Blr *)
coeffVal[[coeffIdxBsstoBlr]]=coeffAllVal[[coeffIdxBsstoBlr]];
coeffVal[[coeffIdx\[CapitalDelta]BsstoBlr]]=coeffAllVal[[coeffIdx\[CapitalDelta]BsstoBlr]];

(* Racemization Blr \[Rule] Bss *)
coeffVal[[coeffIdxBlrtoBss]]=coeffAllVal[[coeffIdxBlrtoBss]];
coeffVal[[coeffIdx\[CapitalDelta]BlrtoBss]]=coeffAllVal[[coeffIdx\[CapitalDelta]BlrtoBss]];

(* Racemization Css \[Rule] Clr *)
coeffVal[[coeffIdxCsstoClr]]=coeffAllVal[[coeffIdxCsstoClr]];
coeffVal[[coeffIdx\[CapitalDelta]CsstoClr]]=coeffAllVal[[coeffIdx\[CapitalDelta]CsstoClr]];

(* Racemization Clr \[Rule] Css *)
coeffVal[[coeffIdxClrtoCss]]=coeffAllVal[[coeffIdxClrtoCss]];
coeffVal[[coeffIdx\[CapitalDelta]ClrtoCss]]=coeffAllVal[[coeffIdx\[CapitalDelta]ClrtoCss]];

(* Decay Blr \[Rule] 4A *)
coeffVal[[coeffIdxBlrto4A]]=coeffAllVal[[coeffIdxBlrto4A]];

(* Decay Bss \[Rule] 4A *)
coeffVal[[coeffIdxBssto4A]]=coeffAllVal[[coeffIdxBssto4A]];
coeffVal[[coeffIdx\[CapitalDelta]Bssto4A]]=coeffAllVal[[coeffIdx\[CapitalDelta]Bssto4A]];

(* ======================================================\[Equal] *)
(* ======================================================\[Equal] *)
)
];

Return[coeffVal];
];
(* ============================================== *)
(* Type of Pair Merging to model. *)
idxG2PairMergingNone=1;
idxG2PairMergingAll=2;

idxG2PairMergingStart=1;
idxG2PairMergingMax=2;
G2PairMergingMatrix={idxG2PairMergingNone,idxG2PairMergingAll};
(* ============================================== *)
(* Type of Autocatalysis to model. *)
idxG2AutoCatalysisNone=1;
idxG2AutoCatalysisSoai=2;
idxG2AutoCatalysisBasic=3;
idxG2AutoCatalysisSecondLevel=4;

idxG2AutoCatalysisMax=4;
G2AutoCatalysisMatrix={idxG2AutoCatalysisNone,idxG2AutoCatalysisSoai,idxG2AutoCatalysisBasic,idxG2AutoCatalysisSecondLevel};
(* ============================================== *)
(* Type of Mutual Antagonism to model. *)
idxG2MutualAntagonismNone=1;
idxG2MutualAntagonismBasic=2;
idxG2MutualAntagonismInterGeneration=3;
idxG2MutualAntagonismAll=4;

idxG2MutualAntagonismMax=4;
G2MutualAntagonismMatrix={idxG2MutualAntagonismNone,idxG2MutualAntagonismBasic,idxG2MutualAntagonismInterGeneration,idxG2MutualAntagonismAll};
(* ============================================== *)
(* Type of Crystallization to model. *)
idxG2CrystallizationNone=1;
idxG2CrystallizationBlr=2;
idxG2CrystallizationAll=3;

idxG2CrystallizationMax=idxG2CrystallizationAll;
G2CrystallizationMatrix={idxG2CrystallizationNone,idxG2CrystallizationBlr,idxG2CrystallizationAll};
(* ============================================== *)
GetG2PairMergingType[modelLevelDescriptor:{__}]:=modelLevelDescriptor[[1]];
GetG2AutoCatalysisType[modelLevelDescriptor:{__}]:=modelLevelDescriptor[[2]];
GetG2MutualAntagonismType[modelLevelDescriptor:{__}]:=modelLevelDescriptor[[3]];
GetG2CrystallizationType[modelLevelDescriptor:{__}]:=modelLevelDescriptor[[4]];
(* ============================================== *)
GetG2PairMergingName[idx_?IntegerQ]:=Module[{retVal},
retVal="!!! Undefined !!!";
If[idx== idxG2PairMergingNone,retVal="None"];
If[idx== idxG2PairMergingAll,retVal="All"];

Return[retVal];
];
(* ============================================== *)
GetG2AutoCatalysisName[idx_?IntegerQ]:=Module[{retVal},
retVal="!!! Undefined !!!";
If[idx== idxG2AutoCatalysisNone,retVal="None"];
If[idx== idxG2AutoCatalysisSoai,retVal="Soai"];
If[idx== idxG2AutoCatalysisBasic,retVal="Basic"];
If[idx== idxG2AutoCatalysisSecondLevel,retVal="Second Level"];

Return[retVal];
];
(* ============================================== *)
GetG2MutualAntagonismName[idx_?IntegerQ]:=Module[{retVal},
retVal="!!! Undefined !!!";
If[idx== idxG2MutualAntagonismNone,retVal="None"];
If[idx== idxG2MutualAntagonismBasic,retVal="Basic"];
If[idx== idxG2MutualAntagonismInterGeneration,retVal="Inter Generation"];
If[idx== idxG2MutualAntagonismAll,retVal="All"];

Return[retVal];
];
(* ============================================== *)
GetG2CrystallizationName[idx_?IntegerQ]:=Module[{retVal},
retVal="!!! Undefined !!!";
If[idx== idxG2CrystallizationNone,retVal="None"];
If[idx== idxG2CrystallizationBlr,retVal="Blr"];
If[idx== idxG2CrystallizationAll,retVal="All"];

Return[retVal];
];
(* ============================================== *)
GetAllModelsDescriptorMatrix[idxGenerationSecond]:=Flatten[Table[{G2PairMergingMatrix[[kk]],G2AutoCatalysisMatrix[[ii]],G2MutualAntagonismMatrix[[jj]],G2CrystallizationMatrix[[ll]]},{kk,idxG2PairMergingStart,idxG2PairMergingMax},{ii,1,idxG2AutoCatalysisMax},{jj,1,idxG2MutualAntagonismMax},{ll,1,idxG2CrystallizationMax}],3];
(* ============================================== *)
IsModelLevelDescriptorValid[idxGenerationSecond,modelLevelDescriptor:{__}]:=Module[{retVal,idxPairMerging,idxAutoCatalysis,idxMutualAntagonism,idxCrystallization},
retVal=False;

idxPairMerging=GetG2PairMergingType[modelLevelDescriptor];
idxAutoCatalysis=GetG2AutoCatalysisType[modelLevelDescriptor];
idxMutualAntagonism=GetG2MutualAntagonismType[modelLevelDescriptor];
idxCrystallization=GetG2CrystallizationType[modelLevelDescriptor];

(* If Pair Merging \[Equal] None then the modelis valid only of all other parameters are set to None as well. *)

retVal=If[(idxPairMerging== idxG2PairMergingNone),
If[(idxAutoCatalysis == idxG2AutoCatalysisNone) && (idxMutualAntagonism ==idxG2MutualAntagonismNone) && (idxCrystallization ==idxG2CrystallizationNone),True,False,False],
True,
False
];

Return[retVal];
];
(* ============================================== *)
GetGenerationName[idxGenerationSecond,modelLevelDescriptor:{__}]:=Module[{retVal,idxAutoCatalysis,idxMutualAntagonism,idxPairMerging,idxCrystallization},
(* Print["GetGenerationName::G2::modelLevelDescriptor = ", modelLevelDescriptor]; *)

retVal="!!! Undefined !!!";

If[Length[modelLevelDescriptor] == 4,
(
idxPairMerging=GetG2PairMergingType[modelLevelDescriptor];
idxAutoCatalysis=GetG2AutoCatalysisType[modelLevelDescriptor];
idxMutualAntagonism=GetG2MutualAntagonismType[modelLevelDescriptor];
idxCrystallization=GetG2CrystallizationType[modelLevelDescriptor];

retVal="Pair Merging: " <> GetG2PairMergingName[idxPairMerging] <> ", Autocatalysis: " <> GetG2AutoCatalysisName[idxAutoCatalysis] <> ", Mutual Antagonism: " <> GetG2MutualAntagonismName[idxMutualAntagonism] <> ", Crystallization: " <> GetG2CrystallizationName[idxCrystallization];
),
(
retVal="GetGenerationName::G2::Incorrect modelLevelDescriptor";
)
];

Return[retVal];
];
(* ============================================== *)
(* New values are used!!! *)
ApplyRacemization[values:{__},mValue_,idxGenerationSecond,rawOptions___]:=Module[{retVal,opts,useVariableRacemizationForAllVal,multBsstoBlrVal,multBlrtoBssVal,multCsstoClrVal,multClrtoCssVal,modelDescriptorVal,modelLevelDescriptor,idxPairMerging},
(* Print["ApplyRacemization::G2 was called."]; *)
retVal=values;
opts=ProcessOptions[rawOptions];
useVariableRacemizationForAllVal=UseVariableRacemizationForAll /. opts /. Options[CLMS];
modelDescriptorVal=ModelDescriptorValue /. opts /. Options[CLMS];

If[modelDescriptorVal=!= Undefined,
(
modelLevelDescriptor=GetModelLevelDescriptor[idxGenerationSecond,modelDescriptorVal];
idxPairMerging=GetG2PairMergingType[modelLevelDescriptor];

If[idxPairMerging !=idxG2PairMergingAll,
(
(* If there is no pair merging then there is nothing to do for racemization. *)
Return[retVal];
)
];
)
];

If[useVariableRacemizationForAllVal== True,
(
multBsstoBlrVal = MultBsstoBlr /.opts /. Options[CLMS];
multBlrtoBssVal = MultBlrtoBss /.opts /. Options[CLMS];
multCsstoClrVal = MultCsstoClr /.opts /. Options[CLMS];
multClrtoCssVal = MultClrtoCss /.opts /. Options[CLMS];

(* Racemization Bss \[Rule] Blr *)
retVal[[coeffIdxBsstoBlr]]=2*multBsstoBlrVal*mValue;

(* Racemization Blr \[Rule] Bss *)
retVal[[coeffIdxBlrtoBss]]=multBlrtoBssVal*mValue;

(* Racemization Css \[Rule] Clr *)
retVal[[coeffIdxCsstoClr]]=2*multCsstoClrVal*mValue;

(* Racemization Clr \[Rule] Css *)
retVal[[coeffIdxClrtoCss]]=multClrtoCssVal*mValue;
)
];

Return[retVal];
];
(* ============================================== *)

