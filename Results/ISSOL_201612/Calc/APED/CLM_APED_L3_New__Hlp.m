sep="===================";
Print[sep];

BDIMAGESIZE=500;
xName="x";
yName="y";
zName="z";

BDFONTSIZE=16;
BDFONTFAMILY="Courier";
BDFONTWEIGHT="Bold";

BDPLTTEXTOPTS={FontFamily->BDFONTFAMILY,FontSize->BDFONTSIZE,FontWeight->BDFONTWEIGHT};
lightingVal=Automatic;


FindGamma2[aVal_,eVal_,hVal_,αVal_,βVal_]:=Module[{α,β, γ,pθA٭,a,e,h,sol,γVal,ii,epsMaxVal,precVal},
precVal=100;
α=SetPrecision[αVal,precVal];
β=SetPrecision[βVal,precVal];
pθA٭=1;
a=SetPrecision[aVal,precVal];
e=SetPrecision[eVal,precVal];
h=SetPrecision[hVal,precVal];

γVal=-(((-1+α)*α*pθA٭)/(4*a*β+(-1+α)*β*pθA٭));

epsMaxVal=epsMax2[aVal,eVal,hVal,αVal,βVal,γVal];

If[(1/aVal) > epsMaxVal,
(
γVal=Indeterminate;
)
];

Return[γVal];
];

FindGamma3[aVal_,eVal_,hVal_,αVal_,βVal_]:=Module[{expr,α,β, γ,pθA٭,a,e,h,sol,γVal,ii,epsMaxVal,precVal},
precVal=100;
α=SetPrecision[αVal,precVal];
β=SetPrecision[βVal,precVal];
pθA٭=1;
a=SetPrecision[aVal,precVal];
e=SetPrecision[eVal,precVal];
h=SetPrecision[hVal,precVal];

expr=-(α-β γ) (h (2 h β+pθA٭ (-α+β))+e (-pθA٭ (1+α) (-1+γ)+2 h (1+β γ))) (4 a h (h β+e (1+β γ))-pθA٭ (1+α) (pθA٭ (h α+e (1+α)+h β+e (1+α) γ)+2 h (h β+e (1+β γ))))+2 (pθA٭ α (-1+β γ) (pθA٭ (h α+e (1+α)+h β+e (1+α) γ)+2 h (h β+e (1+β γ))) (h (2 h β+pθA٭ (-α+β))+e (-pθA٭ (1+α) (-1+γ)+2 h (1+β γ)))+2 a h (pθA٭ (h α+e (1+α)+h β+e (1+α) γ) (α+β γ) (e+h β+e β γ)+(h β+e (1+β γ)) (h (-pθA٭ (α+β) (α-β γ)+2 h β (α+β γ))+e (-pθA٭ (-1+α) (-1+γ) (α-β γ)+2 h (1+β γ) (α+β γ)))));

sol=NSolve[{expr, γ >=0},γ,Reals];
γVal=Indeterminate;

γVal=If[Length[sol]== 1,γ /. sol[[1]], Indeterminate,Indeterminate];

epsMaxVal=epsMax3[aVal,eVal,hVal,αVal,βVal,γVal];

If[(1/aVal) > epsMaxVal,
(
γVal=Indeterminate;
)
];

Return[γVal];
];

delta3[aVal_,eVal_,hVal_,αVal_,βVal_,γVal_]:=Module[{α,β, γ,a,e,h,dlt,BB,DD,EE,precVal},
precVal=100;
α=SetPrecision[αVal,precVal];
β=SetPrecision[βVal,precVal];
γ=SetPrecision[γVal,precVal];
a=SetPrecision[aVal,precVal];
e=SetPrecision[eVal,precVal];
h=SetPrecision[hVal,precVal];

BB=  (e (1+β γ)+h β);
DD=( e (1+α)+h β);
EE= (e (1+α) γ+h α);

dlt=4*a*(DD+EE)/(h*BB*(1+α));
Return[dlt];
];

epsMax2[aVal_,eVal_,hVal_,αVal_,βVal_,γVal_]:=Module[{α,β, γ,pθA٭,a,e,h,eps,precVal},
precVal=100;
α=SetPrecision[αVal,precVal];
β=SetPrecision[βVal,precVal];
γ=SetPrecision[γVal,precVal];
pθA٭=1;
a=SetPrecision[aVal,precVal];
e=SetPrecision[eVal,precVal];
h=SetPrecision[hVal,precVal];

eps=2/(1+α);
Return[eps];
];

epsMax3[aVal_,eVal_,hVal_,αVal_,βVal_,γVal_]:=Module[{α,β, γ,pθA٭,a,e,h,eps,precVal},
precVal=100;
α=SetPrecision[αVal,precVal];
β=SetPrecision[βVal,precVal];
γ=SetPrecision[γVal,precVal];
pθA٭=1;
a=SetPrecision[aVal,precVal];
e=SetPrecision[eVal,precVal];
h=SetPrecision[hVal,precVal];

eps=4/((1+α)*Sqrt[1+delta3[a,e,h,α,β,γ]]);
Return[eps];
];

αVal=0.3;
βVal=0.1;

aVal=1;
eVal=1;
hVal=1;

γVal=FindGamma3[aVal,eVal,hVal,αVal,βVal];
deltaVal=delta3[aVal,eVal,hVal,αVal,βVal,γVal];
epsMaxVal=epsMax3[aVal,eVal,hVal,αVal,βVal,γVal];

Print["aVal = ", aVal, ", eVal = ", eVal, ", hVal = ", hVal,", αVal = ", αVal,", βVal = ", βVal];
Print["deltaVal = ", N[deltaVal],", epsMaxVal = ",N[epsMaxVal], ", (pθA٭/a) = ", (1/aVal),", γVal = ",N[γVal]];

plotOpts:={PlotTheme->{"Classic","ClassicLights"} , PlotRange -> All ,ImageSize -> BDIMAGESIZE,LabelStyle -> BDPLTTEXTOPTS,AxesLabel:>{β,α,""},PlotLabel:>Text[Style[zName,FontFamily->BDFONTFAMILY,FontSize->BDFONTSIZE,FontWeight->BDFONTWEIGHT]]};

plotOpts2:={PlotTheme->{"Classic","ClassicLights"}  ,ImageSize -> BDIMAGESIZE,LabelStyle -> BDPLTTEXTOPTS,AxesLabel:>{β,α,""},PlotLabel:>Text[Style[zName,FontFamily->BDFONTFAMILY,FontSize->BDFONTSIZE,FontWeight->BDFONTWEIGHT]]};


Print["APED-3: γ"];
zName=γ;
Print[Plot3D[FindGamma3[aVal,eVal,hVal,αVal,βVal],{βVal,0,2},{αVal,0.01,1}, Evaluate[plotOpts]]];

zName=αβγ;
Print[Plot3D[αVal*βVal*FindGamma3[aVal,eVal,hVal,αVal,βVal],{βVal,0,2},{αVal,0.01,1}, Evaluate[plotOpts]]];

Print["APED-2: γ"];
zName=γ;
Print[Plot3D[FindGamma2[aVal,eVal,hVal,αVal,βVal],{βVal,0,2},{αVal,0.01,1}, Evaluate[plotOpts2]]];

zName=αβγ;
Print[Plot3D[αVal*βVal*FindGamma2[aVal,eVal,hVal,αVal,βVal],{βVal,0,1},{αVal,0.01,1}, Evaluate[plotOpts]]];

(*
Print["ϵ"];
zName=ϵ/(p*Subscript[θ,Superscript[A,"*"]])-1;
Print[Plot3D[epsMax[aVal,eVal,hVal,αVal,βVal,FindGamma[aVal,eVal,hVal,αVal,βVal]]-1,{βVal,0,1},{αVal,0.01,1}, Evaluate[plotOpts]]];

Print["δ"];
zName=δ;
Print[Plot3D[delta[aVal,eVal,hVal,αVal,βVal,FindGamma[aVal,eVal,hVal,αVal,βVal]],{βVal,0,1},{αVal,0.01,1}, Evaluate[plotOpts]]];
*)

