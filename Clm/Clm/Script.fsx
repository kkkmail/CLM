#load "Substances.fs"
open Clm.Substances

let n = NumberOfAminoAcids.OneAminoAcid
let m = MaxPeptideLength.TwoMax

let subst = ChiralAminoAcid.getAminoAcids n
printfn "subst = %A" subst

//let peptides = Peptide.getPeptides m n 
//printfn "peptides = %A" peptides
//printfn "peptides (sorted) = %A" (peptides |> List.sort)

let g0 = (fun __ -> Some 1.0)
let g = (fun __ -> (None, Some 1.0))

//let synth = synthesisReactions n g
//printfn "synth = %A" synth

//let lig = ligationReactions m n g
//printfn "lig = %A" lig

let sed = sedimentationReactions m n g0
printfn "sed = %A" sed

