(* ============================================== *)
(* :Summary: Bifurcation after test evolution run. *)
(* ============================================== *)
(* :Author: Konstantin K. Konstantinov *)
(* :Email: konstantin.k.konstantinov@gmail.com *)
(* :License type: GPL v3 or any later version, see http://www.gnu.org/licenses/ *)
(* :Copyright: K^3, 2013 - 2015 *)
(* :Version: Revision: 3.15.001, Date: 2015/04/23 *)
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
maxChainLen=3;
useActivation=True;
tMaxVal=1.*10^3;
(* ============================================== *)
PathList={"W:\\Math\\CLM\\"};
Get["CLM_Init.m",Path-> PathList];
LoadModules[PathList, True];
(* ============================================== *)
tEvPlotMultiplier=200;
(* ============================================== *)
(* Bifurcation tests produce coefficients and initial values of rho (perturbed in the direction of the largest Re[eigenvalue]) *)
(* ============================================== *)
epsRule={Ε˖->7.5*10^0,ϵ˖->-0.1, λ˖->0,λ˗->0};
(* ============================================== *)
(* *)
coeffRulesBase={coeff෴Y➔A->1.*10^-10,coeff෴A➔Y->1.*10^-10,coeff෴Y➔a->1.*10^-10,coeff෴a➔Y->1.*10^-10,coeff෴Y˖A➔A˖A->99.7915,coeff෴A˖A➔Y˖A->0.0001,coeff෴Y˖A➔a˖A->99.7915,coeff෴a˖A➔Y˖A->0.0001,coeff෴Y˖AA➔A˖AA->0,coeff෴A˖AA➔Y˖AA->0,coeff෴Y˖AA➔a˖AA->0,coeff෴a˖AA➔Y˖AA->0,coeff෴Y˖Aa➔A˖Aa->0,coeff෴A˖Aa➔Y˖Aa->0,coeff෴Y˖Aa➔a˖Aa->0,coeff෴a˖Aa➔Y˖Aa->0,coeff෴Y˖AAA➔A˖AAA->0,coeff෴A˖AAA➔Y˖AAA->0,coeff෴Y˖AAA➔a˖AAA->0,coeff෴a˖AAA➔Y˖AAA->0,coeff෴Y˖AAa➔A˖AAa->0,coeff෴A˖AAa➔Y˖AAa->0,coeff෴Y˖AAa➔a˖AAa->0,coeff෴a˖AAa➔Y˖AAa->0,coeff෴Y˖AaA➔A˖AaA->0,coeff෴A˖AaA➔Y˖AaA->0,coeff෴Y˖AaA➔a˖AaA->0,coeff෴a˖AaA➔Y˖AaA->0,coeff෴Y˖Aaa➔A˖Aaa->0,coeff෴A˖Aaa➔Y˖Aaa->0,coeff෴Y˖Aaa➔a˖Aaa->0,coeff෴a˖Aaa➔Y˖Aaa->0,coeff෴A➔A٭->100,coeff෴A٭➔A->0.0001,coeff෴A٭˖A➔AA->(1+λ˖)*100,coeff෴AA➔A˖A->(1-λ˗)*100,coeff෴a٭˖A➔aA->(1-λ˖)*100,coeff෴aA➔a˖A->(1+λ˗)*100,coeff෴A٭˖AA➔AAA->100,coeff෴AAA➔A˖AA->100,coeff෴a٭˖AA➔aAA->100,coeff෴aAA➔a˖AA->100,coeff෴A٭˖Aa➔AAa->100,coeff෴AAa➔A˖Aa->100,coeff෴a٭˖Aa➔aAa->100,coeff෴aAa➔a˖Aa->100,coeff෴A˖A➔2Y->0,coeff෴A˖a➔2Y->0,coeff෴A˖AA➔3Y->0,coeff෴A˖aa➔3Y->0,coeff෴AA˖AA➔4Y->0,coeff෴AA˖aa➔4Y->0,coeff෴A˖Aa➔3Y->0,coeff෴A˖aA➔3Y->0,coeff෴AA˖Aa➔4Y->0,coeff෴AA˖aA➔4Y->0,coeff෴Aa˖Aa➔4Y->0,coeff෴Aa˖aA➔4Y->0,coeff෴A˖AAA➔4Y->0,coeff෴A˖aaa➔4Y->0,coeff෴AA˖AAA➔5Y->0,coeff෴AA˖aaa➔5Y->0,coeff෴Aa˖AAA➔5Y->0,coeff෴Aa˖aaa➔5Y->0,coeff෴AAA˖AAA➔6Y->0,coeff෴AAA˖aaa➔6Y->0,coeff෴A˖AAa➔4Y->0,coeff෴A˖aaA➔4Y->0,coeff෴AA˖AAa➔5Y->0,coeff෴AA˖aaA➔5Y->0,coeff෴Aa˖AAa➔5Y->0,coeff෴Aa˖aaA➔5Y->0,coeff෴AAA˖AAa➔6Y->0,coeff෴AAA˖aaA➔6Y->0,coeff෴AAa˖AAa➔6Y->0,coeff෴AAa˖aaA➔6Y->0,coeff෴A˖AaA➔4Y->0,coeff෴A˖aAa➔4Y->0,coeff෴AA˖AaA➔5Y->0,coeff෴AA˖aAa➔5Y->0,coeff෴Aa˖AaA➔5Y->0,coeff෴Aa˖aAa➔5Y->0,coeff෴AAA˖AaA➔6Y->0,coeff෴AAA˖aAa➔6Y->0,coeff෴AAa˖AaA➔6Y->0,coeff෴AAa˖aAa➔6Y->0,coeff෴AaA˖AaA➔6Y->0,coeff෴AaA˖aAa➔6Y->0,coeff෴A˖Aaa➔4Y->0,coeff෴A˖aAA➔4Y->0,coeff෴AA˖Aaa➔5Y->99669.7,coeff෴AA˖aAA➔5Y->0,coeff෴Aa˖Aaa➔5Y->0,coeff෴Aa˖aAA➔5Y->0,coeff෴AAA˖Aaa➔6Y->0,coeff෴AAA˖aAA➔6Y->0,coeff෴AAa˖Aaa➔6Y->0,coeff෴AAa˖aAA➔6Y->0,coeff෴AaA˖Aaa➔6Y->0,coeff෴AaA˖aAA➔6Y->0,coeff෴Aaa˖Aaa➔6Y->0,coeff෴Aaa˖aAA➔6Y->0,coeff෴Aa➔aa->(1+ϵ˖) Ε˖,coeff෴Aa➔AA->0,coeff෴AA➔aA->(1-ϵ˖) Ε˖,coeff෴AA➔Aa->0,coeff෴Aaa➔aaa->(1+ϵ˖) Ε˖,coeff෴AAA➔aAA->(1-ϵ˖) Ε˖,coeff෴AaA➔aaA->(1+ϵ˖) Ε˖,coeff෴AAa➔aAa->(1-ϵ˖) Ε˖};

coeffRules=coeffRulesBase /. epsRule;
(* ============================================== *)
(* *)
rhoRules={rZ->0.,rY->0.162527,rX->0.,rA->17.1606,rA٭->0.327796,ra->17.1524,ra٭->0.327639,rAA->3.39932,rAa->5.62248,raA->5.62248,raa->3.39422,rAAA->1.11424,rAAa->1.84301,rAaA->1.84301,rAaa->0.000328292,raAA->0.00032912,raAa->1.84216,raaA->1.84216,raaa->1.11212};
(* ============================================== *)
(* BifurcationPostRun[maxChainLen,useActivation,tMaxVal,coeffRules, rhoRules]; *)
(* ============================================== *)
NuEpimFunc[EE_?NumericQ,ee_?NumericQ,tMaxVal_?NumericQ]:=NuEpimLigFunc[EE,ee,0,0,tMaxVal];
(* ============================================== *)
NuEpimLigFunc[EE_?NumericQ,ee_?NumericQ,lp_?NumericQ,lm_?NumericQ,tMaxVal_?NumericQ]:=Module[{epsRule,coeffRules,coeffValues,allRho,sol,interpFunc,f,tDomain,nuVal},
epsRule={Ε˖->EE,ϵ˖->ee, λ˖->lp,λ˗->lm};
coeffRules=coeffRulesBase /. epsRule;
coeffValues=Table[coeffArrayName[kk],{kk,1,NoCoeffCnt}] /. coeffRules;
allRho=Table[Max[SubstanceMatrix[kk] /. rhoRules,0],{kk,1,NoSubstCnt}];
sol=ρAllFunc[coeffValues,allRho,tMaxVal,True,ApplyCoeffRule-> True,NDSolveMethod-> None];
interpFunc=ρAllGetInterpolationFunctions[sol];
f=SubstanceMatrix[1] /.interpFunc[[1]];
tDomain=f["Domain"][[1,2]];
nuVal=Abs[nuValue[ρTotalLValueFunc[sol,tDomain],ρTotalDValueFunc[sol,tDomain]]];
Return[nuVal];
];
(* ============================================== *)
PlotEpim3D[tMaxVal_?NumericQ]:=Module[{},
Print[Plot3D[NuEpimFunc[10^lgEE,ee,tMaxVal],{lgEE,0.5,1.5},{ee,-1,1}, PlotPoints->5, PlotRange -> All,ImageSize->BDIMAGESIZE,PlotTheme->{"Classic","ClassicLights"}]];
];
(* ============================================== *)
CalculatePlot3D[pltPoints_?IntegerQ,xStart_?NumericQ,xEnd_?NumericQ,yStart_?NumericQ,yEnd_?NumericQ,tMaxVal_?NumericQ]:=Module[{pltPointsX,pltPointsY,meshVar,CalcGridArr,MyTable,MyApproximateFunc,plotOpts,xName,yName,zName,f},

pltPointsY=pltPointsX=pltPoints;
meshVar={pltPointsX-2, pltPointsY-2};

xName="lg(E)";
yName="ϵ";
zName="η";

CalcGridArr=ParallelTable[NuEpimFunc[10^(xStart+(i-1)*(xEnd-xStart)/(pltPointsX-1)),yStart+(j-1)*(yEnd-yStart)/(pltPointsY-1),tMaxVal],{i,1,pltPointsX},{j,1,pltPointsY}];
WaitAll[CalcGridArr];

f[xVar_?NumericQ,yVar_?NumericQ]:=Module[{iii,jjj,retval},
iii=Max[Min[1+Round[(pltPointsX-1)*(xVar-xStart)/(xEnd-xStart)],pltPointsX],1];
jjj=Max[Min[1+Round[(pltPointsY-1)*(yVar-yStart)/(yEnd-yStart)],pltPointsY],1];
retval=CalcGridArr[[iii,jjj]];
Return[retval];
];

MyTable=Flatten[Table[{xVar,yVar,f[xVar,yVar]},{xVar,xStart,xEnd,(xEnd-xStart)/(pltPointsX-1)},{yVar,yStart,yEnd,(yEnd-yStart)/(pltPointsY-1)}],1];
MyApproximateFunc=Interpolation[MyTable];

(* ,Mesh  meshVar *)
plotOpts={AxesLabel->{xName,yName,""},PlotLabel->zName,Compiled->False,LabelStyle->BDPLTTEXTOPTS,ImageSize->BDIMAGESIZE,MaxRecursion->0,{PlotTheme->{"Classic","ClassicLights"}}};

Print[Plot3D[MyApproximateFunc[xVar,yVar],{xVar,xStart,xEnd},{yVar,yStart,yEnd},Evaluate[plotOpts]]]; 
];
(* ============================================== *)
CalculatePlot3DLig[pltPoints_?IntegerQ,eeVal_?NumericQ,xStart_?NumericQ,xEnd_?NumericQ,yStart_?NumericQ,yEnd_?NumericQ,lmSign_?BooleanQ,tMaxVal_?NumericQ]:=Module[{pltPointsX,pltPointsY,meshVar,CalcGridArr,MyTable,MyApproximateFunc,plotOpts,xName,yName,i,j,zName,f,xValue,yValue,lms},

pltPointsY=pltPointsX=pltPoints;
meshVar={pltPointsX-2, pltPointsY-2};

xName="lg(E)";
yName="λ";
zName="η";

xValue[idx_?IntegerQ]:=(xStart+(idx-1)*(xEnd-xStart)/(pltPointsX-1));
yValue[idx_?IntegerQ]:=(yStart+(idx-1)*(yEnd-yStart)/(pltPointsY-1));
lms=If[lmSign,1,-1];

CalcGridArr=ParallelTable[NuEpimLigFunc[10^xValue[i],eeVal,yValue[j],lms*yValue[j],tMaxVal],{i,1,pltPointsX},{j,1,pltPointsY}];
WaitAll[CalcGridArr];

f[xVar_?NumericQ,yVar_?NumericQ]:=Module[{iii,jjj,retval},
iii=Max[Min[1+Round[(pltPointsX-1)*(xVar-xStart)/(xEnd-xStart)],pltPointsX],1];
jjj=Max[Min[1+Round[(pltPointsY-1)*(yVar-yStart)/(yEnd-yStart)],pltPointsY],1];
retval=CalcGridArr[[iii,jjj]];
Return[retval];
];

MyTable=Flatten[Table[{xVar,yVar,f[xVar,yVar]},{xVar,xStart,xEnd,(xEnd-xStart)/(pltPointsX-1)},{yVar,yStart,yEnd,(yEnd-yStart)/(pltPointsY-1)}],1];
MyApproximateFunc=Interpolation[MyTable];

(* ,Mesh  meshVar *)
plotOpts={AxesLabel->{xName,yName,""},PlotLabel->zName,Compiled->False,LabelStyle->BDPLTTEXTOPTS,ImageSize->BDIMAGESIZE,MaxRecursion->0,{PlotTheme->{"Classic","ClassicLights"}}};

Print[Plot3D[MyApproximateFunc[xVar,yVar],{xVar,xStart,xEnd},{yVar,yStart,yEnd},Evaluate[plotOpts]]]; 
];
(* ============================================== *)
BifurcationRun[maxChainLen,useActivation,tMaxVal,False,{{}},True];
(* PlotEpim3D[tMaxVal]; *)
ϵVal=0.5;
(* ============================================== *)
plotPoints=25;
Emin=0.5;
Emax=1.5;
(* ============================================== *)
Print["Dependence on ϵ"];
CalculatePlot3D[plotPoints,Emin,Emax,-1,1,tMaxVal];
PrintTimeUsed[];
(* ============================================== *)
Print["Dependence on λ: λ˖ = λ, λ˗ = λ for ϵ = ", ϵVal];
CalculatePlot3DLig[plotPoints,Emin,Emax,ϵVal,-1,1,True,tMaxVal];
PrintTimeUsed[];
(* ============================================== *)
Print["Dependence on λ: λ˖ = λ, λ˗ = -λ for ϵ = ", ϵVal];
CalculatePlot3DLig[plotPoints,Emin,Emax,ϵVal,-1,1,False,tMaxVal];
PrintTimeUsed[];
(* ============================================== *)
