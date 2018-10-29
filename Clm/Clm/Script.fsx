#load "Substances.fs"
open Clm.Substances

let n = NumberOfAminoAcids.TwoAminoAcids
let m = 3

let subst = ChiralAminoAcid.getAminoAcids n
printfn "subst = %A" subst

let peptides = Peptide.getPeptides m n 
printfn "peptides = %A" peptides
printfn "peptides (sorted) = %A" (peptides |> List.sort)

