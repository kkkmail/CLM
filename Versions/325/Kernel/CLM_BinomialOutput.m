(* :Summary: Binomial Output CLM logic. *)
(* :Author: Konstantin K. Konstantinov *)
(* :Email: konstantin.k.konstantinov@gmail.com *)
(* :License type: GPL *)
(* :Copyright: K^3, 2013 - 2015 *)
(* :Version: Revision: 2.25.001, Date: 2015/01/27 *)
(* :Mathematica Version: 7.0 - 10.0 *)
(* ============================================== *)
OutputBinomialAllInfo[idxkAPtoSPfunc_?IntegerQ,ChainLen_?IntegerQ,rawOptions___]:=Module[{},
kAPtoSPlstVal=Table[kAPtoSPfunc[idxkAPtoSPfunc,ii,ChainLen,opts],{ii,0,ChainLen}];
(* Print["kAPtoSPlstVal = ", N[kAPtoSPlstVal] // MatrixForm]; *)

kSPtoAPlstVal=Table[kSPtoAPfunc[kSPtoAPfuncUnitIdx,ii,ChainLen,opts],{ii,0,ChainLen}];

(* ============================================== *)

Print["kAPtoSPfunc"];
xName="k";
SetLegends[{"\!\(\*SubscriptBox[\(k\), \(A+M -> S+M\)]\)"}];
Print[DiscretePlot[kAPtoSPfunc[idxkAPtoSPfunc,x+ChainLen/2,ChainLen,rawOptions],{x,-ChainLen/2,ChainLen/2},Evaluate[discrPlotOpts2]]];
(* Print[DiscretePlot[kAPtoSPfunc[x+ChainLen/2,ChainLen],{x,-Max[ChainLen/20,1],Max[ChainLen/20,1]},Evaluate[discrPlotOpts]]]; *)

(* xName="Log[k]"; *)
SetLegends[{"\!\(\*SubscriptBox[\(Log[k]\), \(A+M -> S+M\)]\)"}];
Print[DiscretePlot[Log[10,kAPtoSPfunc[idxkAPtoSPfunc,x+ChainLen/2,ChainLen,rawOptions]],{x,-ChainLen/2,ChainLen/2},Evaluate[discrPlotOpts2]]];
Print[strSeparator];

Print["Subscript[\[Gamma], +]"];
SetLegends[{"\!\(\*SubscriptBox[\(\[Gamma]\), \(+\)]\)"}];
Print[DiscretePlot[GammaFunc[kAPtoSPlstVal,Round[x+(ChainLen/2)]],{x,-ChainLen/2,ChainLen/2},Evaluate[discrPlotOpts2]]];
(* Print[DiscretePlot[GammaFunc[kAPtoSPfunc,Round[x+(ChainLen/2)],ChainLen],{x,-ChainLen/2,ChainLen/2},Evaluate[discrPlotOpts2]]]; *)

Print["Subscript[k, +]"];
SetLegends[{"\!\(\*SubscriptBox[\(k\), \(+\)]\)"}];
Print[DiscretePlot[kFunc[kAPtoSPlstVal,Round[x+(ChainLen/2)]],{x,-ChainLen/2,ChainLen/2},Evaluate[discrPlotOpts2]]];
(* Print[DiscretePlot[kFunc[kAPtoSPfunc,Round[x+(ChainLen/2)],ChainLen],{x,-ChainLen/2,ChainLen/2},Evaluate[discrPlotOpts2]]]; *)

(*
Print["Subscript[k, -]"];
SetLegends[{"\!\(\*SubscriptBox[\(k\), \(-\)]\)"}];
Print[DiscretePlot[kmFunc[kSPtoAPlstVal,Round[x+(ChainLen/2)]],{x,-ChainLen/2,ChainLen/2},Evaluate[discrPlotOpts2]]];
(* Print[DiscretePlot[kmFunc[kAPtoSPfunc,Round[x+(ChainLen/2)],ChainLen],{x,-ChainLen/2,ChainLen/2},Evaluate[discrPlotOpts2]]]; *)
*)

(*
SetLegends[{"\!\(\*SubscriptBox[\(\[Gamma]\), \(+\)]\)"}];
nnnn=5;
Print[DiscretePlot[GammaFunc[kAPtoSPfunc,Round[x+(nnnn/2)],nnnn],{x,-nnnn/2,nnnn/2},Evaluate[discrPlotOpts2]]];
nnnn=10;
Print[DiscretePlot[GammaFunc[kAPtoSPfunc,Round[x+(nnnn/2)],nnnn],{x,-nnnn/2,nnnn/2},Evaluate[discrPlotOpts2]]];

Print["Subscript[\[Gamma], +][ChainLen] = ", N[GammaFunc[kAPtoSPlstVal,ChainLen]]];
Print[strSeparator];

Print[DiscretePlot3D[1+GammaFunc[kAPtoSPfunc,Round[x+chainLen/2],chainLen],{x,-ChainLen/2,ChainLen/2},{chainLen,1,ChainLen},Evaluate[discrPlot3DOpts]]];
Print[Plot3D[GammaFunc[kAPtoSPfunc,Round[x+chainLen/2],Round[chainLen]],{x,-ChainLen/2,ChainLen/2},{chainLen,1,ChainLen}]];
Print[strSeparator];
*)

(*
Print["Subscript[\[Gamma], -]"];
SetLegends[{"\!\(\*SubscriptBox[\(\[Gamma]\), \(-\)]\)"}];
Print[DiscretePlot[GammaFunc[kSPtoAPlstVal,x+(ChainLen/2)],{x,-ChainLen/2,ChainLen/2},Evaluate[discrPlotOpts2]]];
Print[strSeparator];

Print["kSPtoAPfunc"];
SetLegends[{"\!\(\*SubscriptBox[\(k\), \(S+M \[Rule] A+M\)]\)"}];
Print[DiscretePlot[kSPtoAPfunc[kSPtoAPfuncUnitIdx,Round[x+(ChainLen/2)],ChainLen,rawOptions],{x,-ChainLen/2,ChainLen/2},Evaluate[discrPlotOpts2]]];
Print[strSeparator];
*)

Print["Peptide distribution"];
pList={0.1,0.5,0.7,0.9};
SetLegends[Table["\!\(\*SubscriptBox[\(B\), \(" <> ToString[ChainLen]<> "," <> ToString[p] <> ",k\)]\)",{p,pList}]];
Print[DiscretePlot[Evaluate@Table[PDF[BinomialDistribution[ChainLen,p],k],{p,pList}],{k,0,ChainLen},Evaluate[discrPlotOpts]]];
Print[strSeparator];

Print["Derivative of distribution"];
SetLegends[Table["\!\(\*SubscriptBox[\(D\), \(" <> ToString[ChainLen]<> "," <> ToString[p] <> ",k\)]\)",{p,pList}]];
Print[DiscretePlot[Evaluate@Table[DBinomialDistribution[ChainLen,p,k],{p,pList}],{k,0,ChainLen},Evaluate[discrPlotOpts]]];
Print[strSeparator];

Print["Peptide distribution and its derivative/Sqrt[N] for symmetric case."];
legends={"\!\(\*SubscriptBox[\(k\), \(A+M -> S+M\)]\)","\!\(\*SubscriptBox[\(D\), \(" <> ToString[ChainLen]<> "," <> ToString[0.5] <> ",k\)]\)","\!\(\*SubscriptBox[\(B\), \(" <> ToString[ChainLen]<> "," <> ToString[0.5] <> ",k\)]\)"};
Print["legends = ", legends // MatrixForm];
SetLegends[legends];
Print[DiscretePlot[{kAPtoSPfunc[idxkAPtoSPfunc,k,ChainLen,rawOptions],Evaluate@DBinomialDistribution[ChainLen,1/2,k],Evaluate@PDF[BinomialDistribution[ChainLen,1/2],k]},{k,0,ChainLen},Evaluate[discrPlotOpts]]];
PrintTimeUsed[];

(* ============================================== *)

CalculateBifurcation[1/2,1];
PrintTimeUsed[];
(* ============================================== *)
(*
roTval=1;
legends={"Bifurcation (roT = " <> ToString[roTval] <>")"};
Print["legends = ", legends // MatrixForm];
SetLegends[legends];

DistributeDefinitions[CalculateBifurcation,NNN,roTval,kAPtoSPfunc,DBinomialDistribution,kAtoS];
cbTbl=ParallelTable[{CalculateBifurcation[N[(k/NNN)],roTval]},{k,0,NNN}];
PrintTimeUsed[];

(* Print[DiscretePlot[{CalculateBifurcation[N[(k/NNN)],roTval]},{k,0,NNN},Evaluate[discrPlotOpts]]]; *)
Print[DiscretePlot[{cbTbl[[k+1]]},{k,0,NNN},Evaluate[discrPlotOpts]]];
PrintTimeUsed[];
*)
(* ============================================== *)
(*
roTval=10;
legends={"Bifurcation (roT = " <> ToString[roTval] <>")"};
Print["legends = ", legends // MatrixForm];
SetLegends[legends];
Print[DiscretePlot[{CalculateBifurcation[N[(k/NNN)],roTval]},{k,0,NNN},Evaluate[discrPlotOpts]]];
PrintTimeUsed[];
*)

];
(* ============================================== *)

