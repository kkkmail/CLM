(* ============================================== *)
(* :Summary: Various calculations (final bits of them) for CLM. *)
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

(* Mathematica can't figure out the precedence of the definitions. So it stores them consecutively and without an obvious way to change that order. But we need certain definitons to be BEFORE some others. So, here is the hack. *)
(* ============================================== *)
(* \[Eta]Func is a chiral polarization or L and R of the corresponding generation *)
\[Eta]Func[generationLevel_?IntegerQ,\[Rho]AllVal:{__}]:=Indeterminate;
(* ============================================== *)
(* Returns True if a model level descriptor is valid. This is to avoid unnedeed repetitions. *)
IsModelLevelDescriptorValid[generationLevel_?IntegerQ,modelLevelDescriptor:{__}]:=False;
(* ============================================== *)
GetGenerationName[generationLevel_?IntegerQ,modelLevelDescriptor:{__}]:="!!! Undefined !!!";
(* ============================================== *)
ApplyNewCoefficients[coeffVal:{__},coeffAllVal:{__},generationLevel_?IntegerQ,modelDescriptor:{__}]:=Module[{},
Print["ApplyNewCoefficients::G0 was called."];
Return[coeffVal];
];
(* ============================================== *)
ApplyRacemization[values:{__},mValue_,generationLevel_?IntegerQ,rawOptions___]:=Module[{},
Print["ApplyRacemization::G0 was called."];
Return[values];
];
(* ============================================== *)



