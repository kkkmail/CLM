(* ============================================== *)
(* :Summary: APED-3 + DC(True, 24) *)
(* ============================================== *)
(* :Author: Konstantin K. Konstantinov *)
(* :Email: konstantin.k.konstantinov@gmail.com *)
(* :License type: GPL v3 or any later version, see http://www.gnu.org/licenses/ *)
(* :Copyright: K^3, 2013 - 2015 *)
(* :Version: Revision: 3.17.002, Date: 2015/11/17 *)
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
sep="===================";
BDIMAGESIZE=500;
xName="x";
yName="y";
zName="z";

BDFONTSIZE=16;
BDFONTFAMILY="Courier";
BDFONTWEIGHT="Bold";

BDPLTTEXTOPTS={FontFamily->BDFONTFAMILY,FontSize->BDFONTSIZE,FontWeight->BDFONTWEIGHT};
lightingVal=Automatic;

plotOpts3D:={PlotTheme->{"Classic","ClassicLights"} (*, PlotRange  All *) ,ImageSize -> BDIMAGESIZE,LabelStyle -> BDPLTTEXTOPTS,AxesLabel:>{xName,yName,""},PlotLabel:>Text[Style[zName,FontFamily->BDFONTFAMILY,FontSize->BDFONTSIZE,FontWeight->BDFONTWEIGHT]],PlotPoints->50,MaxRecursion->1};

plotOpts2D:={(*, PlotRange  All *) ImageSize -> BDIMAGESIZE,LabelStyle -> BDPLTTEXTOPTS,AxesLabel:>{xName,""},PlotLabel:>Text[Style[yName,FontFamily->BDFONTFAMILY,FontSize->BDFONTSIZE,FontWeight->BDFONTWEIGHT]], PlotStyle-> Thick, Frame -> True, GridLines-> Automatic,PlotPoints->200,MaxRecursion->1};
Print[sep];
(* ============================================== *)
MaxReCoeffVal$1$24[r0Var_?NumericQ, c˖Var_?NumericQ, c˗Var_?NumericQ, a˖Var_?NumericQ, a˗Var_?NumericQ, Ε˖Var_?NumericQ, ϵ˖Var_?NumericQ, Λ˖Var_?NumericQ, λ˖Var_?NumericQ, Λ˗Var_?NumericQ, λ˗Var_?NumericQ, Σ˗Var_?NumericQ]:=Module[{r0, c˖, c˗, a˖, a˗, Ε˖, ϵ˖, Λ˖, λ˖, Λ˗, λ˗, Σ˗, eqTbl,sol,len,ii,coeffTable,coeffVal,retVal,precisionVal,eValTbl,eValLen,jj,eSys,eVecTbl,maxPrec,rY,θA, θA٭, θAA, θAa, θAAA, θAAa, θAaA, θAaa,cnt},
precisionVal = 100;
maxPrec = $MaxPrecision;
$MaxPrecision = Infinity;

r0=SetPrecision[r0Var, precisionVal];
c˖=SetPrecision[c˖Var, precisionVal];
c˗=SetPrecision[c˗Var, precisionVal];
a˖=SetPrecision[a˖Var, precisionVal];
a˗=SetPrecision[a˗Var, precisionVal];
Ε˖=SetPrecision[Ε˖Var, precisionVal];
ϵ˖=SetPrecision[ϵ˖Var, precisionVal];
Λ˖=SetPrecision[Λ˖Var, precisionVal];
λ˖=SetPrecision[λ˖Var, precisionVal];
Λ˗=SetPrecision[Λ˗Var, precisionVal];
λ˗=SetPrecision[λ˗Var, precisionVal];
Σ˗=SetPrecision[Σ˗Var, precisionVal];

eqTbl={-r0 + rY + θA + θA٭ + 2*θAa + 2*θAA + 3*θAaa + 3*θAaA + 3*θAAa + 3*θAAA == 0, 2*c˖*rY - a˖*θA - c˗*θA + a˗*θA٭ - θA*θA٭*Λ˖ + 2*θAa*Λ˗ + 2*θAA*Λ˗ + θAaa*Λ˗ + θAaA*Λ˗ + θAAa*Λ˗ + θAAA*Λ˗ + 2*θAa*λ˗*Λ˗ - 2*θAA*λ˗*Λ˗ + θAaa*λ˗*Λ˗ + θAaA*λ˗*Λ˗ - θAAa*λ˗*Λ˗ - θAAA*λ˗*Λ˗ == 0, a˖*θA - θA٭*(a˗ + (θA + θAa + θAA)*Λ˖) == 0, Ε˖*((1 + ϵ˖)*θAa + (-1 + ϵ˖)*θAA) - θA٭*θAA*Λ˖ + (θA*θA٭*(1 + λ˖)*Λ˖)/2 - θAA*Λ˗ + θAaa*Λ˗ + θAAA*Λ˗ + θAA*λ˗*Λ˗ + θAaa*λ˗*Λ˗ - θAAA*λ˗*Λ˗ - θAA*θAaa*Σ˗ == 0, -(Ε˖*((1 + ϵ˖)*θAa + (-1 + ϵ˖)*θAA)) - θA٭*θAa*Λ˖ - (θA*θA٭*(-1 + λ˖)*Λ˖)/2 - θAa*Λ˗ + θAaA*Λ˗ + θAAa*Λ˗ - θAa*λ˗*Λ˗ + θAaA*λ˗*Λ˗ - θAAa*λ˗*Λ˗ == 0, Ε˖*((1 + ϵ˖)*θAaa + (-1 + ϵ˖)*θAAA) + (θA٭*θAA*(1 + λ˖)*Λ˖ + 2*θAAA*(-1 + λ˗)*Λ˗)/2 == 0, Ε˖*((1 + ϵ˖)*θAaA + (-1 + ϵ˖)*θAAa) + (θA٭*θAa*(1 + λ˖)*Λ˖ + 2*θAAa*(-1 + λ˗)*Λ˗)/2 == 0, (-2*Ε˖*((1 + ϵ˖)*θAaA + (-1 + ϵ˖)*θAAa) - θA٭*θAa*(-1 + λ˖)*Λ˖ - 2*θAaA*(1 + λ˗)*Λ˗)/2 == 0, (-2*Ε˖*((1 + ϵ˖)*θAaa + (-1 + ϵ˖)*θAAA) - θA٭*θAA*(-1 + λ˖)*Λ˖ - 2*θAaa*(Λ˗ + λ˗*Λ˗ + θAA*Σ˗))/2 == 0, rY >= 0, θA >= 0, θA٭ >= 0, θAA >= 0, θAa >= 0, θAAA >= 0, θAAa >= 0, θAaA >= 0, θAaa >= 0};

coeffTable = {{-a˖ - c˗ - θA٭*Λ˖, a˗ - θA*λ˖*Λ˖, 2*Λ˗ - 2*λ˗*Λ˗, 0, Λ˗ - λ˗*Λ˗, Λ˗ - λ˗*Λ˗, Λ˗ + λ˗*Λ˗, Λ˗ + λ˗*Λ˗}, {a˖ - θA٭*λ˖*Λ˖, -a˗ - θA*Λ˖ - θAa*Λ˖ - θAA*Λ˖, -(θA٭*λ˖*Λ˖), -(θA٭*λ˖*Λ˖), 0, 0, 0, 0}, {(θA٭*Λ˖ + θA٭*λ˖*Λ˖)/2, (θA*Λ˖ + θA*λ˖*Λ˖ - 2*θAA*λ˖*Λ˖)/2, (-1 + ϵ˖)*Ε˖ - θA٭*Λ˖ - Λ˗ + λ˗*Λ˗ - θAaa*Σ˗, -((1 + ϵ˖)*Ε˖), (2*Λ˗ - 2*λ˗*Λ˗)/2, 0, 0, (-2*Λ˗ - 2*λ˗*Λ˗ - 2*θAA*Σ˗)/2}, {(-(θA٭*Λ˖) + θA٭*λ˖*Λ˖)/2, (θA*Λ˖ - θA*λ˖*Λ˖ - 2*θAa*λ˖*Λ˖)/2, (-1 + ϵ˖)*Ε˖, -Ε˖ - ϵ˖*Ε˖ - θA٭*Λ˖ - Λ˗ - λ˗*Λ˗, 0, (2*Λ˗ - 2*λ˗*Λ˗)/2, (-2*Λ˗ - 2*λ˗*Λ˗)/2, 0}, {0, (θAA*Λ˖ + θAA*λ˖*Λ˖)/2, (θA٭*Λ˖ + θA٭*λ˖*Λ˖)/2, 0, (-1 + ϵ˖)*Ε˖ + (-1 + λ˗)*Λ˗, 0, 0, -((1 + ϵ˖)*Ε˖)}, {0, (θAa*Λ˖ + θAa*λ˖*Λ˖)/2, 0, (θA٭*Λ˖ + θA٭*λ˖*Λ˖)/2, 0, (-1 + ϵ˖)*Ε˖ + (-1 + λ˗)*Λ˗, -((1 + ϵ˖)*Ε˖), 0}, {0, (θAa*Λ˖ - θAa*λ˖*Λ˖)/2, 0, (-(θA٭*Λ˖) + θA٭*λ˖*Λ˖)/2, 0, (-1 + ϵ˖)*Ε˖, -Ε˖ - ϵ˖*Ε˖ - Λ˗ - λ˗*Λ˗, 0}, {0, (θAA*Λ˖ - θAA*λ˖*Λ˖)/2, (-(θA٭*Λ˖) + θA٭*λ˖*Λ˖ - 2*θAaa*Σ˗)/2, 0, (-1 + ϵ˖)*Ε˖, 0, 0, -Ε˖ - ϵ˖*Ε˖ - Λ˗ - λ˗*Λ˗ - θAA*Σ˗}};

sol = NSolve[eqTbl,{rY,θA, θA٭, θAA, θAa, θAAA, θAAa, θAaA, θAaa}, Reals, WorkingPrecision->50];
len=Length[sol];
retVal=Indeterminate;
cnt=0;

Do[
(
coeffVal=coeffTable /. sol[[ii]];
eSys=Eigensystem[coeffVal];
eValTbl=eSys[[1]];
eVecTbl=eSys[[2]];
eValLen=Length[eValTbl];

If[cnt==0 && eValLen >= 1,
(
retVal=Re[eValTbl][[1]];
)
];

cnt++;

Do[
(
If[Re[eValTbl][[jj]]>retVal,
(
retVal=Re[eValTbl][[jj]];
)
];
),{jj,1,eValLen}
];
),{ii,1,len}
];
$MaxPrecision = maxPrec;
Return[retVal];
];
(* ============================================== *)
MaxReCoeffVal$1$24Log[r0Var_?NumericQ, c˖Var_?NumericQ, c˗Var_?NumericQ, a˖Var_?NumericQ, a˗Var_?NumericQ, Ε˖Var_?NumericQ, ϵ˖Var_?NumericQ, Λ˖Var_?NumericQ, λ˖Var_?NumericQ, Λ˗Var_?NumericQ, λ˗Var_?NumericQ, Σ˗Var_?NumericQ]:=MaxReCoeffVal$1$24Log[r0Var, c˖Var, c˗Var, a˖Var, a˗Var, Ε˖Var, ϵ˖Var, Λ˖Var, λ˖Var, Λ˗Var, λ˗Var, Σ˗Var, False];

MaxReCoeffVal$1$24Log[r0Var_?NumericQ, c˖Var_?NumericQ, c˗Var_?NumericQ, a˖Var_?NumericQ, a˗Var_?NumericQ, Ε˖Var_?NumericQ, ϵ˖Var_?NumericQ, Λ˖Var_?NumericQ, λ˖Var_?NumericQ, Λ˗Var_?NumericQ, λ˗Var_?NumericQ, Σ˗Var_?NumericQ, onlyPositive_?BooleanQ]:=Module[{r0, c˖, c˗, a˖, a˗, Ε˖, ϵ˖, Λ˖, λ˖, Λ˗, λ˗, Σ˗, eqTbl,sol,len,ii,coeffTable,coeffVal,retVal,precisionVal,eValTbl,eValLen,jj,eSys,eVecTbl,maxPrec,rY,θA, θA٭, θAA, θAa, θAAA, θAAa, θAaA, θAaa,cnt,coeffs},

precisionVal = 200;
maxPrec = $MaxPrecision;
$MaxPrecision = Infinity;

r0=SetPrecision[r0Var, precisionVal];
c˖=SetPrecision[c˖Var, precisionVal];
c˗=SetPrecision[c˗Var, precisionVal];
a˖=SetPrecision[a˖Var, precisionVal];
a˗=SetPrecision[a˗Var, precisionVal];
Ε˖=SetPrecision[Ε˖Var, precisionVal];
ϵ˖=SetPrecision[ϵ˖Var, precisionVal];
Λ˖=SetPrecision[Λ˖Var, precisionVal];
λ˖=SetPrecision[λ˖Var, precisionVal];
Λ˗=SetPrecision[Λ˗Var, precisionVal];
λ˗=SetPrecision[λ˗Var, precisionVal];
Σ˗=SetPrecision[Σ˗Var, precisionVal];

eqTbl={-10^r0 + rY + θA + θA٭ + 2*θAa + 2*θAA + 3*θAaa + 3*θAaA + 3*θAAa + 3*θAAA == 0, 2^(1 + c˖)*5^c˖*rY + 10^a˗*θA٭ - θA*(10^a˖ + 10^c˗ + 10^Λ˖*θA٭) + 2^(1 + Λ˗)*5^Λ˗*θAa + 2^(1 + Λ˗)*5^Λ˗*θAA + 10^Λ˗*θAaa + 10^Λ˗*θAaA + 10^Λ˗*θAAa + 10^Λ˗*θAAA + 2^(1 + Λ˗)*5^Λ˗*θAa*λ˗ - 2^(1 + Λ˗)*5^Λ˗*θAA*λ˗ + 10^Λ˗*θAaa*λ˗ + 10^Λ˗*θAaA*λ˗ - 10^Λ˗*θAAa*λ˗ - 10^Λ˗*θAAA*λ˗ == 0, θA*(10^a˖ - 10^Λ˖*θA٭) - θA٭*(10^a˗ + 10^Λ˖*θAa + 10^Λ˖*θAA) == 0, 10^Ε˖*(1 + ϵ˖)*θAa - 10^Ε˖*θAA - 10^Λ˗*θAA + 10^Ε˖*ϵ˖*θAA - 10^Λ˖*θA٭*θAA + 10^Λ˗*θAaa - 10^Σ˗*θAA*θAaa + 10^Λ˗*θAAA + 2^(-1 + Λ˖)*5^Λ˖*θA*θA٭*(1 + λ˖) + 10^Λ˗*θAA*λ˗ + 10^Λ˗*θAaa*λ˗ - 10^Λ˗*θAAA*λ˗ == 0, 10^Ε˖*θAA - 10^Ε˖*ϵ˖*θAA + 10^Λ˗*θAaA + 10^Λ˗*θAAa - 2^(-1 + Λ˖)*5^Λ˖*θA*θA٭*(-1 + λ˖) + 10^Λ˗*θAaA*λ˗ - 10^Λ˗*θAAa*λ˗ - θAa*(10^Ε˖ + 10^Λ˗ + 10^Ε˖*ϵ˖ + 10^Λ˖*θA٭ + 10^Λ˗*λ˗) == 0, 10^Ε˖*(1 + ϵ˖)*θAaa + 2^(-1 + Λ˖)*5^Λ˖*θA٭*θAA*(1 + λ˖) + θAAA*(-10^Ε˖ - 10^Λ˗ + 10^Ε˖*ϵ˖ + 10^Λ˗*λ˗) == 0, 10^Ε˖*(1 + ϵ˖)*θAaA + 2^(-1 + Λ˖)*5^Λ˖*θA٭*θAa*(1 + λ˖) + θAAa*(-10^Ε˖ - 10^Λ˗ + 10^Ε˖*ϵ˖ + 10^Λ˗*λ˗) == 0, -(10^Ε˖*(-1 + ϵ˖)*θAAa) - 2^(-1 + Λ˖)*5^Λ˖*θA٭*θAa*(-1 + λ˖) - θAaA*(10^Ε˖ + 10^Λ˗ + 10^Ε˖*ϵ˖ + 10^Λ˗*λ˗) == 0, -(10^Ε˖*(-1 + ϵ˖)*θAAA) - 2^(-1 + Λ˖)*5^Λ˖*θA٭*θAA*(-1 + λ˖) - θAaa*(10^Ε˖ + 10^Λ˗ + 10^Ε˖*ϵ˖ + 10^Σ˗*θAA + 10^Λ˗*λ˗) == 0, rY >= 0, θA >= 0, θA٭ >= 0, θAA >= 0, θAa >= 0, θAAA >= 0, θAAa >= 0, θAaA >= 0, θAaa >= 0};

coeffTable = {{-10^a˖ - 10^c˗ - 10^Λ˖*θA٭, 10^a˗ - 10^Λ˖*θA*λ˖, 2^(1 + Λ˗)*5^Λ˗ - 2^(1 + Λ˗)*5^Λ˗*λ˗, 0, 10^Λ˗ - 10^Λ˗*λ˗, 10^Λ˗ - 10^Λ˗*λ˗, 10^Λ˗ + 10^Λ˗*λ˗, 10^Λ˗ + 10^Λ˗*λ˗}, {10^a˖ - 10^Λ˖*θA٭*λ˖, -10^a˗ - 10^Λ˖*θA - 10^Λ˖*θAa - 10^Λ˖*θAA, -(10^Λ˖*θA٭*λ˖), -(10^Λ˖*θA٭*λ˖), 0, 0, 0, 0}, {2^(-1 + Λ˖)*5^Λ˖*θA٭ + 2^(-1 + Λ˖)*5^Λ˖*θA٭*λ˖, 2^(-1 + Λ˖)*5^Λ˖*θA + 2^(-1 + Λ˖)*5^Λ˖*θA*λ˖ - 10^Λ˖*θAA*λ˖, -10^Ε˖ - 10^Λ˗ + 10^Ε˖*ϵ˖ - 10^Λ˖*θA٭ - 10^Σ˗*θAaa + 10^Λ˗*λ˗, -(10^Ε˖*(1 + ϵ˖)), 10^Λ˗ - 10^Λ˗*λ˗, 0, 0, -10^Λ˗ - 10^Σ˗*θAA - 10^Λ˗*λ˗}, {-(2^(-1 + Λ˖)*5^Λ˖*θA٭) + 2^(-1 + Λ˖)*5^Λ˖*θA٭*λ˖, 2^(-1 + Λ˖)*5^Λ˖*θA - 2^(-1 + Λ˖)*5^Λ˖*θA*λ˖ - 10^Λ˖*θAa*λ˖, 10^Ε˖*(-1 + ϵ˖), -10^Ε˖ - 10^Λ˗ - 10^Ε˖*ϵ˖ - 10^Λ˖*θA٭ - 10^Λ˗*λ˗, 0, 10^Λ˗ - 10^Λ˗*λ˗, -10^Λ˗ - 10^Λ˗*λ˗, 0}, {0, 2^(-1 + Λ˖)*5^Λ˖*θAA + 2^(-1 + Λ˖)*5^Λ˖*θAA*λ˖, 2^(-1 + Λ˖)*5^Λ˖*θA٭ + 2^(-1 + Λ˖)*5^Λ˖*θA٭*λ˖, 0, -10^Ε˖ - 10^Λ˗ + 10^Ε˖*ϵ˖ + 10^Λ˗*λ˗, 0, 0, -(10^Ε˖*(1 + ϵ˖))}, {0, 2^(-1 + Λ˖)*5^Λ˖*θAa + 2^(-1 + Λ˖)*5^Λ˖*θAa*λ˖, 0, 2^(-1 + Λ˖)*5^Λ˖*θA٭ + 2^(-1 + Λ˖)*5^Λ˖*θA٭*λ˖, 0, -10^Ε˖ - 10^Λ˗ + 10^Ε˖*ϵ˖ + 10^Λ˗*λ˗, -(10^Ε˖*(1 + ϵ˖)), 0}, {0, (10^Λ˖*θAa - 10^Λ˖*θAa*λ˖)/2, 0, (-(10^Λ˖*θA٭) + 10^Λ˖*θA٭*λ˖)/2, 0, 10^Ε˖*(-1 + ϵ˖), -10^Ε˖ - 10^Λ˗ - 10^Ε˖*ϵ˖ - 10^Λ˗*λ˗, 0}, {0, (10^Λ˖*θAA - 10^Λ˖*θAA*λ˖)/2, (-(10^Λ˖*θA٭) - 2^(1 + Σ˗)*5^Σ˗*θAaa + 10^Λ˖*θA٭*λ˖)/2, 0, 10^Ε˖*(-1 + ϵ˖), 0, 0, -10^Ε˖ - 10^Λ˗ - 10^Ε˖*ϵ˖ - 10^Σ˗*θAA - 10^Λ˗*λ˗}};

sol = NSolve[eqTbl,{rY,θA, θA٭, θAA, θAa, θAAA, θAAa, θAaA, θAaa}, Reals, WorkingPrecision->100];

coeffs = {{"r0" -> N[r0]}, {"c˖" -> N[c˖]}, {"c˗" -> N[c˗]}, {"a˖" -> N[a˖]}, {"a˗" -> N[a˗]}, {"Ε˖" -> N[Ε˖]}, {"ϵ˖" -> N[ϵ˖]}, {"Λ˖" -> N[Λ˖]}, {"λ˖" -> N[λ˖]}, {"Λ˗" -> N[Λ˗]}, {"λ˗" -> N[λ˗]}, {"Σ˗" -> N[Σ˗]}};

(*
Print["coeffs = ",coeffs, ", sol = ", N[sol]];
Print[sep];
*)

len=Length[sol];
retVal=Indeterminate;
cnt=0;

Do[
(
coeffVal=coeffTable /. sol[[ii]];
eSys=Eigensystem[coeffVal];
eValTbl=eSys[[1]];
eVecTbl=eSys[[2]];
eValLen=Length[eValTbl];

If[cnt==0 && eValLen >= 1,
(
retVal=Re[eValTbl][[1]];
)
];

cnt++;

Do[
(
If[Re[eValTbl][[jj]]>retVal,
(
retVal=Re[eValTbl][[jj]];
)
];
),{jj,1,eValLen}
];
),{ii,1,len}
];

$MaxPrecision = maxPrec;

If[onlyPositive && retVal <0,
(
retVal=Indeterminate;
)
];

Return[retVal];
];
(* ============================================== *)
coeffs:= {{"r0" -> N[r0LogVal]}, {"c˖" -> N[c˖LogVal]}, {"c˗" -> N[c˗LogVal]}, {"a˖" -> N[a˖LogVal]}, {"a˗" -> N[a˗LogVal]}, {"Ε˖" -> N[Ε˖LogVal]}, {"ϵ˖" -> N[ϵ˖Val]}, {"Λ˖" -> N[Λ˖LogVal]}, {"λ˖" -> N[λ˖Val]}, {"Λ˗" -> N[Λ˗LogVal]}, {"λ˗" -> N[λ˗Val]}, {"Σ˗" -> N[Σ˗LogVal]}};
(* ============================================== *)
(*
r0Val=10^2;
 c˖Val=10^-3;
 c˗Val=10^-10;
 a˖Val=10^2;
 a˗Val=10^-4;
 Ε˖Val=0;
 ϵ˖Val=0;
 Λ˖Val=10^2;
 λ˖Val=0;
 Λ˗Val=10^2;
 λ˗Val=0;
 Σ˗Val=10^5;

N[MaxReCoeffVal$1$24[r0Val, c˖Val, c˗Val, a˖Val, a˗Val, Ε˖Val, ϵ˖Val, Λ˖Val, λ˖Val, Λ˗Val, λ˗Val, Σ˗Val]]
*)
(* ============================================== *)
(*
r0LogVal=2;
c˖LogVal=-3;
c˗LogVal=-10;
a˖LogVal=0;
a˗LogVal=-4;
Ε˖LogVal=-4.995;
ϵ˖Val=0.9;
Λ˖LogVal=0;
λ˖Val=0.9;
Λ˗LogVal=0;
λ˗Val=-0.9;
Σ˗LogVal=-4.995;

N[MaxReCoeffVal$1$24Log[r0LogVal, c˖LogVal, c˗LogVal, a˖LogVal, a˗LogVal, Ε˖LogVal, ϵ˖Val, Λ˖LogVal, λ˖Val, Λ˗LogVal, λ˗Val, Σ˗LogVal]]
*)
(* ============================================== *)


r0LogVal=2;
 c˖LogVal=-3;
 c˗LogVal=-10;
 a˖LogVal=0;
 a˗LogVal=-4;
 Ε˖LogVal=-10;
 ϵ˖Val=0;
 Λ˖LogVal=2;
 λ˖Val=0;
 Λ˗LogVal=2;
 λ˗Val=0;
 Σ˗LogVal=5;

(* ============================================== *)
(*
r0LogVal=2;
 c˖LogVal=-3;
 c˗LogVal=-10;
 a˖LogVal=0;
 a˗LogVal=-4;
 Ε˖LogVal=0;
 ϵ˖Val=0.9;
 Λ˖LogVal=0;
 λ˖Val=0.9;
 Λ˗LogVal=0;
 λ˗Val=-0.9;
 Σ˗LogVal=-10;


N[MaxReCoeffVal$1$24Log[r0LogVal, c˖LogVal, c˗LogVal, a˖LogVal, a˗LogVal, Ε˖LogVal, ϵ˖Val, Λ˖LogVal, λ˖Val, Λ˗LogVal, λ˗Val, Σ˗LogVal]]
*)
(* ============================================== *)
(*
r0LogVal=2;
 c˖LogVal=-3;
 c˗LogVal=-10;
 a˖LogVal=0;
 a˗LogVal=-4;
 Ε˖LogVal=-10;
 ϵ˖Val=0;
 Λ˖LogVal=1;
 λ˖Val=0;
 Λ˗LogVal=1;
 λ˗Val=0;
 Σ˗LogVal=5;

Print[sep];
*)
(* ============================================== *)
(*
r0LogVal=2;
 c˖LogVal=-3;
 c˗LogVal=-10;
 a˖LogVal=0;
 a˗LogVal=-4;
 Ε˖LogVal=0;
 ϵ˖Val=0.9;
 Λ˖LogVal=0;
 λ˖Val=0.9;
 Λ˗LogVal=0;
 λ˗Val=-0.9;
 Σ˗LogVal=5;

Print["coeffs = ", coeffs // MatrixForm];
xName=lg[Σ˗];
yName=Subscript[λ,"max"];

Print[Plot[MaxReCoeffVal$1$24Log[r0LogVal, c˖LogVal, c˗LogVal, a˖LogVal, a˗LogVal, Ε˖LogVal, ϵ˖Val, Λ˖LogVal, λ˖Val, Λ˗LogVal, λ˗Val, Σ˗LogVal],{Σ˗LogVal,-5,5}, Evaluate[plotOpts2D]]];

Print[sep];
*)
(* ============================================== *)
(*
r0LogVal=2;
 c˖LogVal=-3;
 c˗LogVal=-10;
 a˖LogVal=0;
 a˗LogVal=-4;
 Ε˖LogVal=-10;
 ϵ˖Val=0;
 Λ˖LogVal=0;
 λ˖Val=0;
 Λ˗LogVal=0;
 λ˗Val=0;
 Σ˗LogVal=5;

Print["coeffs = ", coeffs // MatrixForm];
xName=lg[Σ˗];
yName=Subscript[λ,"max"];

Print[Plot[MaxReCoeffVal$1$24Log[r0LogVal, c˖LogVal, c˗LogVal, a˖LogVal, a˗LogVal, Ε˖LogVal, ϵ˖Val, Λ˖LogVal, λ˖Val, Λ˗LogVal, λ˗Val, Σ˗LogVal],{Σ˗LogVal,-5,5}, Evaluate[plotOpts2D]]];

Print[sep];
*)
(* ============================================== *)
(* ============================================== *)
(* ============================================== *)
(*
Print["DC (r0, Σ˗)"];
 Ε˖LogVal=-5;
 ϵ˖Val=0;
 Λ˖LogVal=0;
 λ˖Val=0;
 Λ˗LogVal=0;
 λ˗Val=-0;
 Σ˗LogVal=5;

xName=lg[Subscript[ρ,"0"]];
yName=lg[Σ˗];
zName=Subscript[λ,"max"];

Print["coeffs = ", coeffs // MatrixForm];

Print[Plot3D[MaxReCoeffVal$1$24Log[r0LogVal, c˖LogVal, c˗LogVal, a˖LogVal, a˗LogVal, Ε˖LogVal, ϵ˖Val, Λ˖LogVal, λ˖Val, Λ˗LogVal, λ˗Val, Σ˗LogVal],{r0LogVal,-4,4},{Σ˗LogVal,-5,5},Evaluate[plotOpts3D]]];

Print[Plot3D[MaxReCoeffVal$1$24Log[r0LogVal, c˖LogVal, c˗LogVal, a˖LogVal, a˗LogVal, Ε˖LogVal, ϵ˖Val, Λ˖LogVal, λ˖Val, Λ˗LogVal, λ˗Val, Σ˗LogVal,True],{r0LogVal,-4,4},{Σ˗LogVal,-5,5},Evaluate[plotOpts3D]]];

Print[Plot3D[MaxReCoeffVal$1$24Log[r0LogVal, c˖LogVal, c˗LogVal, a˖LogVal, a˗LogVal, Ε˖LogVal, ϵ˖Val, Λ˖LogVal, λ˖Val, Λ˗LogVal, λ˗Val, Σ˗LogVal,True],{r0LogVal,-1,4},{Σ˗LogVal,-2,5},Evaluate[plotOpts3D]]];

Print[sep];
*)
(* ============================================== *)
(*
Print["APED (λ˖, λ˗)"];
 Ε˖LogVal=2;
 ϵ˖Val=0.9;
 Λ˖LogVal=0;
 λ˖Val=0.5;
 Λ˗LogVal=0;
 λ˗Val=-0.9;
 Σ˗LogVal=-5;

xName=λ˖;
yName=λ˗;
zName=Subscript[λ,"max"];

Print["coeffs = ", coeffs // MatrixForm];

Print[Plot3D[MaxReCoeffVal$1$24Log[r0LogVal, c˖LogVal, c˗LogVal, a˖LogVal, a˗LogVal, Ε˖LogVal, ϵ˖Val, Λ˖LogVal, λ˖Val, Λ˗LogVal, λ˗Val, Σ˗LogVal],{λ˖Val,-1,1},{λ˗Val,-1,1},Evaluate[plotOpts3D]]];

Print[Plot3D[MaxReCoeffVal$1$24Log[r0LogVal, c˖LogVal, c˗LogVal, a˖LogVal, a˗LogVal, Ε˖LogVal, ϵ˖Val, Λ˖LogVal, λ˖Val, Λ˗LogVal, λ˗Val, Σ˗LogVal,True],{λ˖Val,-1,1},{λ˗Val,-1,1},Evaluate[plotOpts3D]]];


Print[Plot3D[MaxReCoeffVal$1$24Log[r0LogVal, c˖LogVal, c˗LogVal, a˖LogVal, a˗LogVal, Ε˖LogVal, ϵ˖Val, Λ˖LogVal, λ˖Val, Λ˗LogVal, λ˗Val, Σ˗LogVal],{λ˖Val,0,1},{λ˗Val,-1,0.5},Evaluate[plotOpts3D]]];

Print[Plot3D[MaxReCoeffVal$1$24Log[r0LogVal, c˖LogVal, c˗LogVal, a˖LogVal, a˗LogVal, Ε˖LogVal, ϵ˖Val, Λ˖LogVal, λ˖Val, Λ˗LogVal, λ˗Val, Σ˗LogVal,True],{λ˖Val,0,1},{λ˗Val,-1,0.5},Evaluate[plotOpts3D]]];

Print[sep];
*)
(* ============================================== *)
(*
Print["APED ( ϵ˖, λ˗)"];
 Ε˖LogVal=2;
 ϵ˖Val=0.9;
 Λ˖LogVal=0;
 λ˖Val=0.5;
 Λ˗LogVal=0;
 λ˗Val=-0.9;
 Σ˗LogVal=-5;

xName= ϵ˖;
yName=λ˗;
zName=Subscript[λ,"max"];

Print["coeffs = ", coeffs // MatrixForm];

Print[Plot3D[MaxReCoeffVal$1$24Log[r0LogVal, c˖LogVal, c˗LogVal, a˖LogVal, a˗LogVal, Ε˖LogVal, ϵ˖Val, Λ˖LogVal, λ˖Val, Λ˗LogVal, λ˗Val, Σ˗LogVal],{ϵ˖Val,-1,1},{λ˗Val,-1,1},Evaluate[plotOpts3D]]];

Print[Plot3D[MaxReCoeffVal$1$24Log[r0LogVal, c˖LogVal, c˗LogVal, a˖LogVal, a˗LogVal, Ε˖LogVal, ϵ˖Val, Λ˖LogVal, λ˖Val, Λ˗LogVal, λ˗Val, Σ˗LogVal,True],{ϵ˖Val,-1,1},{λ˗Val,-1,1},Evaluate[plotOpts3D]]];

Print[Plot3D[MaxReCoeffVal$1$24Log[r0LogVal, c˖LogVal, c˗LogVal, a˖LogVal, a˗LogVal, Ε˖LogVal, ϵ˖Val, Λ˖LogVal, λ˖Val, Λ˗LogVal, λ˗Val, Σ˗LogVal],{ϵ˖Val,0,1},{λ˗Val,-1,1},Evaluate[plotOpts3D]]];

Print[Plot3D[MaxReCoeffVal$1$24Log[r0LogVal, c˖LogVal, c˗LogVal, a˖LogVal, a˗LogVal, Ε˖LogVal, ϵ˖Val, Λ˖LogVal, λ˖Val, Λ˗LogVal, λ˗Val, Σ˗LogVal,True],{ϵ˖Val,0,1},{λ˗Val,-1,1},Evaluate[plotOpts3D]]];

Print[sep];
*)
(* ============================================== *)
(*
Print["DC (λ˖, Σ˗)"];
 Ε˖LogVal=-5;
 ϵ˖Val=0;
 Λ˖LogVal=0;
 λ˖Val=0;
 Λ˗LogVal=0;
 λ˗Val=-0;
 Σ˗LogVal=5;

xName=λ˖;
yName=lg[Σ˗];
zName=Subscript[λ,"max"];

Print["coeffs = ", coeffs // MatrixForm];

Print[Plot3D[MaxReCoeffVal$1$24Log[r0LogVal, c˖LogVal, c˗LogVal, a˖LogVal, a˗LogVal, Ε˖LogVal, ϵ˖Val, Λ˖LogVal, λ˖Val, Λ˗LogVal, λ˗Val, Σ˗LogVal],{λ˖Val,-1,1},{Σ˗LogVal,-5,5},Evaluate[plotOpts3D]]];

Print[Plot3D[MaxReCoeffVal$1$24Log[r0LogVal, c˖LogVal, c˗LogVal, a˖LogVal, a˗LogVal, Ε˖LogVal, ϵ˖Val, Λ˖LogVal, λ˖Val, Λ˗LogVal, λ˗Val, Σ˗LogVal,True],{λ˖Val,-1,1},{Σ˗LogVal,-5,5},Evaluate[plotOpts3D]]];

Print[sep];
*)
(* ============================================== *)
(*
Print["DC (λ˗, Σ˗)"];
 Ε˖LogVal=-5;
 ϵ˖Val=0;
 Λ˖LogVal=0;
 λ˖Val=0;
 Λ˗LogVal=0;
 λ˗Val=-0;
 Σ˗LogVal=5;

xName=λ˗;
yName=lg[Σ˗];
zName=Subscript[λ,"max"];

Print["coeffs = ", coeffs // MatrixForm];

Print[Plot3D[MaxReCoeffVal$1$24Log[r0LogVal, c˖LogVal, c˗LogVal, a˖LogVal, a˗LogVal, Ε˖LogVal, ϵ˖Val, Λ˖LogVal, λ˖Val, Λ˗LogVal, λ˗Val, Σ˗LogVal],{λ˗Val,-1,1},{Σ˗LogVal,-5,5},Evaluate[plotOpts3D]]];

Print[Plot3D[MaxReCoeffVal$1$24Log[r0LogVal, c˖LogVal, c˗LogVal, a˖LogVal, a˗LogVal, Ε˖LogVal, ϵ˖Val, Λ˖LogVal, λ˖Val, Λ˗LogVal, λ˗Val, Σ˗LogVal,True],{λ˗Val,-1,1},{Σ˗LogVal,-5,5},Evaluate[plotOpts3D]]];

Print[sep];
*)
(* ============================================== *)
(*
Print["DC (λ˖, λ˗)"];
 Ε˖LogVal=-5;
 ϵ˖Val=0;
 Λ˖LogVal=0;
 λ˖Val=0;
 Λ˗LogVal=0;
 λ˗Val=0;
 Σ˗LogVal=5;

xName=λ˖;
yName=λ˗;
zName=Subscript[λ,"max"];

Print["coeffs = ", coeffs // MatrixForm];

Print[Plot3D[MaxReCoeffVal$1$24Log[r0LogVal, c˖LogVal, c˗LogVal, a˖LogVal, a˗LogVal, Ε˖LogVal, ϵ˖Val, Λ˖LogVal, λ˖Val, Λ˗LogVal, λ˗Val, Σ˗LogVal],{λ˖Val,-1,1},{λ˗Val,-1,1},Evaluate[plotOpts3D]]];

Print[Plot3D[MaxReCoeffVal$1$24Log[r0LogVal, c˖LogVal, c˗LogVal, a˖LogVal, a˗LogVal, Ε˖LogVal, ϵ˖Val, Λ˖LogVal, λ˖Val, Λ˗LogVal, λ˗Val, Σ˗LogVal,True],{λ˖Val,-1,1},{λ˗Val,-1,1},Evaluate[plotOpts3D]]];

Print[sep];
*)
(* ============================================== *)
Print["APED + DC (Ε˖, Σ˗) - 1"];
 Ε˖LogVal=-5;
 ϵ˖Val=0.9;
 Λ˖LogVal=0;
 λ˖Val=0.5;
 Λ˗LogVal=0;
 λ˗Val=-0.9;
 Σ˗LogVal=5;

xName=lg[ Ε˖];
yName=lg[Σ˗];
zName=Subscript[λ,"max"];

Print["coeffs = ", coeffs // MatrixForm];

Print[Plot3D[MaxReCoeffVal$1$24Log[r0LogVal, c˖LogVal, c˗LogVal, a˖LogVal, a˗LogVal, Ε˖LogVal, ϵ˖Val, Λ˖LogVal, λ˖Val, Λ˗LogVal, λ˗Val, Σ˗LogVal],{Ε˖LogVal,-4,4},{Σ˗LogVal,-5,7},Evaluate[plotOpts3D]]];

Print[Plot3D[MaxReCoeffVal$1$24Log[r0LogVal, c˖LogVal, c˗LogVal, a˖LogVal, a˗LogVal, Ε˖LogVal, ϵ˖Val, Λ˖LogVal, λ˖Val, Λ˗LogVal, λ˗Val, Σ˗LogVal,True],{Ε˖LogVal,-4,4},{Σ˗LogVal,-5,7},Evaluate[plotOpts3D]]];

Print[sep];
(* ============================================== *)
Print["APED + DC (Ε˖, Σ˗) - 2"];
 Ε˖LogVal=-5;
 ϵ˖Val=0.5;
 Λ˖LogVal=0;
 λ˖Val=0.5;
 Λ˗LogVal=0;
 λ˗Val=-0.9;
 Σ˗LogVal=5;

xName=lg[ Ε˖];
yName=lg[Σ˗];
zName=Subscript[λ,"max"];

Print["coeffs = ", coeffs // MatrixForm];

Print[Plot3D[MaxReCoeffVal$1$24Log[r0LogVal, c˖LogVal, c˗LogVal, a˖LogVal, a˗LogVal, Ε˖LogVal, ϵ˖Val, Λ˖LogVal, λ˖Val, Λ˗LogVal, λ˗Val, Σ˗LogVal],{Ε˖LogVal,-4,4},{Σ˗LogVal,-5,7},Evaluate[plotOpts3D]]];

Print[Plot3D[MaxReCoeffVal$1$24Log[r0LogVal, c˖LogVal, c˗LogVal, a˖LogVal, a˗LogVal, Ε˖LogVal, ϵ˖Val, Λ˖LogVal, λ˖Val, Λ˗LogVal, λ˗Val, Σ˗LogVal,True],{Ε˖LogVal,-4,4},{Σ˗LogVal,-5,7},Evaluate[plotOpts3D]]];

Print[sep];
(* ============================================== *)
Print["APED + DC (Ε˖, Σ˗) - 3"];
 Ε˖LogVal=-5;
 ϵ˖Val=0.2;
 Λ˖LogVal=0;
 λ˖Val=0.5;
 Λ˗LogVal=0;
 λ˗Val=-0.9;
 Σ˗LogVal=5;

xName=lg[ Ε˖];
yName=lg[Σ˗];
zName=Subscript[λ,"max"];

Print["coeffs = ", coeffs // MatrixForm];

Print[Plot3D[MaxReCoeffVal$1$24Log[r0LogVal, c˖LogVal, c˗LogVal, a˖LogVal, a˗LogVal, Ε˖LogVal, ϵ˖Val, Λ˖LogVal, λ˖Val, Λ˗LogVal, λ˗Val, Σ˗LogVal],{Ε˖LogVal,-4,4},{Σ˗LogVal,-5,7},Evaluate[plotOpts3D]]];

Print[Plot3D[MaxReCoeffVal$1$24Log[r0LogVal, c˖LogVal, c˗LogVal, a˖LogVal, a˗LogVal, Ε˖LogVal, ϵ˖Val, Λ˖LogVal, λ˖Val, Λ˗LogVal, λ˗Val, Σ˗LogVal,True],{Ε˖LogVal,-4,4},{Σ˗LogVal,-5,7},Evaluate[plotOpts3D]]];

Print[sep];
(* ============================================== *)
Print["APED + DC (Ε˖, Σ˗) - 4"];
 Ε˖LogVal=-5;
 ϵ˖Val=0.5;
 Λ˖LogVal=0;
 λ˖Val=0.5;
 Λ˗LogVal=0;
 λ˗Val=-0.5;
 Σ˗LogVal=5;

xName=lg[ Ε˖];
yName=lg[Σ˗];
zName=Subscript[λ,"max"];

Print["coeffs = ", coeffs // MatrixForm];

Print[Plot3D[MaxReCoeffVal$1$24Log[r0LogVal, c˖LogVal, c˗LogVal, a˖LogVal, a˗LogVal, Ε˖LogVal, ϵ˖Val, Λ˖LogVal, λ˖Val, Λ˗LogVal, λ˗Val, Σ˗LogVal],{Ε˖LogVal,-4,4},{Σ˗LogVal,-5,7},Evaluate[plotOpts3D]]];

Print[Plot3D[MaxReCoeffVal$1$24Log[r0LogVal, c˖LogVal, c˗LogVal, a˖LogVal, a˗LogVal, Ε˖LogVal, ϵ˖Val, Λ˖LogVal, λ˖Val, Λ˗LogVal, λ˗Val, Σ˗LogVal,True],{Ε˖LogVal,-4,4},{Σ˗LogVal,-5,7},Evaluate[plotOpts3D]]];

Print[sep];
(* ============================================== *)
Print["APED + DC (Ε˖, Σ˗) - 5"];
 Ε˖LogVal=-5;
 ϵ˖Val=0.5;
 Λ˖LogVal=0;
 λ˖Val=0.5;
 Λ˗LogVal=0;
 λ˗Val=-0.2;
 Σ˗LogVal=5;

xName=lg[ Ε˖];
yName=lg[Σ˗];
zName=Subscript[λ,"max"];

Print["coeffs = ", coeffs // MatrixForm];

Print[Plot3D[MaxReCoeffVal$1$24Log[r0LogVal, c˖LogVal, c˗LogVal, a˖LogVal, a˗LogVal, Ε˖LogVal, ϵ˖Val, Λ˖LogVal, λ˖Val, Λ˗LogVal, λ˗Val, Σ˗LogVal],{Ε˖LogVal,-4,4},{Σ˗LogVal,-5,7},Evaluate[plotOpts3D]]];

Print[Plot3D[MaxReCoeffVal$1$24Log[r0LogVal, c˖LogVal, c˗LogVal, a˖LogVal, a˗LogVal, Ε˖LogVal, ϵ˖Val, Λ˖LogVal, λ˖Val, Λ˗LogVal, λ˗Val, Σ˗LogVal,True],{Ε˖LogVal,-4,4},{Σ˗LogVal,-5,7},Evaluate[plotOpts3D]]];

Print[sep];
(* ============================================== *)
Print["APED + DC (Ε˖, Σ˗) - 6"];
 Ε˖LogVal=-5;
 ϵ˖Val=0.5;
 Λ˖LogVal=0;
 λ˖Val=0.2;
 Λ˗LogVal=0;
 λ˗Val=-0.2;
 Σ˗LogVal=5;

xName=lg[ Ε˖];
yName=lg[Σ˗];
zName=Subscript[λ,"max"];

Print["coeffs = ", coeffs // MatrixForm];

Print[Plot3D[MaxReCoeffVal$1$24Log[r0LogVal, c˖LogVal, c˗LogVal, a˖LogVal, a˗LogVal, Ε˖LogVal, ϵ˖Val, Λ˖LogVal, λ˖Val, Λ˗LogVal, λ˗Val, Σ˗LogVal],{Ε˖LogVal,-4,4},{Σ˗LogVal,-5,7},Evaluate[plotOpts3D]]];

Print[Plot3D[MaxReCoeffVal$1$24Log[r0LogVal, c˖LogVal, c˗LogVal, a˖LogVal, a˗LogVal, Ε˖LogVal, ϵ˖Val, Λ˖LogVal, λ˖Val, Λ˗LogVal, λ˗Val, Σ˗LogVal,True],{Ε˖LogVal,-4,4},{Σ˗LogVal,-5,7},Evaluate[plotOpts3D]]];

Print[sep];
(* ============================================== *)
Print["APED + DC (Ε˖, Σ˗) - 7"];
 Ε˖LogVal=-5;
 ϵ˖Val=0.2;
 Λ˖LogVal=0;
 λ˖Val=0.2;
 Λ˗LogVal=0;
 λ˗Val=-0.2;
 Σ˗LogVal=5;

xName=lg[ Ε˖];
yName=lg[Σ˗];
zName=Subscript[λ,"max"];

Print["coeffs = ", coeffs // MatrixForm];

Print[Plot3D[MaxReCoeffVal$1$24Log[r0LogVal, c˖LogVal, c˗LogVal, a˖LogVal, a˗LogVal, Ε˖LogVal, ϵ˖Val, Λ˖LogVal, λ˖Val, Λ˗LogVal, λ˗Val, Σ˗LogVal],{Ε˖LogVal,-4,4},{Σ˗LogVal,-5,7},Evaluate[plotOpts3D]]];

Print[Plot3D[MaxReCoeffVal$1$24Log[r0LogVal, c˖LogVal, c˗LogVal, a˖LogVal, a˗LogVal, Ε˖LogVal, ϵ˖Val, Λ˖LogVal, λ˖Val, Λ˗LogVal, λ˗Val, Σ˗LogVal,True],{Ε˖LogVal,-4,4},{Σ˗LogVal,-5,7},Evaluate[plotOpts3D]]];

Print[sep];
(* ============================================== *)
(*
Print["Anti-APED + DC (Ε˖, Σ˗)"];
 Ε˖LogVal=-5;
 ϵ˖Val=0.9;
 Λ˖LogVal=0;
 λ˖Val=-0.5;
 Λ˗LogVal=0;
 λ˗Val=0.9;
 Σ˗LogVal=5;

xName=lg[ Ε˖];
yName=lg[Σ˗];
zName=Subscript[λ,"max"];

Print["coeffs = ", coeffs // MatrixForm];

Print[Plot3D[MaxReCoeffVal$1$24Log[r0LogVal, c˖LogVal, c˗LogVal, a˖LogVal, a˗LogVal, Ε˖LogVal, ϵ˖Val, Λ˖LogVal, λ˖Val, Λ˗LogVal, λ˗Val, Σ˗LogVal],{Ε˖LogVal,-4,4},{Σ˗LogVal,-5,5},Evaluate[plotOpts3D]]];

Print[Plot3D[MaxReCoeffVal$1$24Log[r0LogVal, c˖LogVal, c˗LogVal, a˖LogVal, a˗LogVal, Ε˖LogVal, ϵ˖Val, Λ˖LogVal, λ˖Val, Λ˗LogVal, λ˗Val, Σ˗LogVal,True],{Ε˖LogVal,-4,4},{Σ˗LogVal,-5,5},Evaluate[plotOpts3D]]];

Print[sep];
*)
(* ============================================== *)
(* ============================================== *)
(* ============================================== *)


===================
APED + DC (Ε˖, Σ˗) - 1
coeffs = (r0->2.
c˖->-3.
c˗->-10.
a˖->0.
a˗->-4.
Ε˖->-5.
ϵ˖->0.9
Λ˖->0.
λ˖->0.5
Λ˗->0.
λ˗->-0.9
Σ˗->5.

)


===================
APED + DC (Ε˖, Σ˗) - 2
coeffs = (r0->2.
c˖->-3.
c˗->-10.
a˖->0.
a˗->-4.
Ε˖->-5.
ϵ˖->0.5
Λ˖->0.
λ˖->0.5
Λ˗->0.
λ˗->-0.9
Σ˗->5.

)


===================
APED + DC (Ε˖, Σ˗) - 3
coeffs = (r0->2.
c˖->-3.
c˗->-10.
a˖->0.
a˗->-4.
Ε˖->-5.
ϵ˖->0.2
Λ˖->0.
λ˖->0.5
Λ˗->0.
λ˗->-0.9
Σ˗->5.

)


===================
APED + DC (Ε˖, Σ˗) - 4
coeffs = (r0->2.
c˖->-3.
c˗->-10.
a˖->0.
a˗->-4.
Ε˖->-5.
ϵ˖->0.5
Λ˖->0.
λ˖->0.5
Λ˗->0.
λ˗->-0.5
Σ˗->5.

)


===================
APED + DC (Ε˖, Σ˗) - 5
coeffs = (r0->2.
c˖->-3.
c˗->-10.
a˖->0.
a˗->-4.
Ε˖->-5.
ϵ˖->0.5
Λ˖->0.
λ˖->0.5
Λ˗->0.
λ˗->-0.2
Σ˗->5.

)


===================
APED + DC (Ε˖, Σ˗) - 6
coeffs = (r0->2.
c˖->-3.
c˗->-10.
a˖->0.
a˗->-4.
Ε˖->-5.
ϵ˖->0.5
Λ˖->0.
λ˖->0.2
Λ˗->0.
λ˗->-0.2
Σ˗->5.

)


===================
APED + DC (Ε˖, Σ˗) - 7
coeffs = (r0->2.
c˖->-3.
c˗->-10.
a˖->0.
a˗->-4.
Ε˖->-5.
ϵ˖->0.2
Λ˖->0.
λ˖->0.2
Λ˗->0.
λ˗->-0.2
Σ˗->5.

)


===================