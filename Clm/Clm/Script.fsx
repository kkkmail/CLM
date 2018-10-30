#load "Substances.fs"
open Clm.Substances

let n = NumberOfAminoAcids.OneAminoAcid
let m = MaxPeptideLength.TwoMax

let rates = 
    [
         (Synthesis, (fun __ -> (Some (ReactionRate 1.0), None)) |> ReactionRateProvider)
         (CatalyticSynthesis, (fun __ -> (Some (ReactionRate 1.0), None)) |> ReactionRateProvider)
         (Ligation, (fun __ -> (Some (ReactionRate 1.0), None)) |> ReactionRateProvider)
         (SedimentationDirect, (fun __ -> (Some (ReactionRate 1.0), None)) |> ReactionRateProvider)
    ]


let modelParams = 
    {
        numberOfAminoAcids = n
        maxPeptideLength = m
        reactionRates = rates
    }

let model = ClmModel modelParams

let subst = ChiralAminoAcid.getAminoAcids n
printfn "subst = %A" subst

//let peptides = Peptide.getPeptides m n 
//printfn "peptides = %A" peptides
//printfn "peptides (sorted) = %A" (peptides |> List.sort)

//let synth = synthesisReactions n g
//printfn "synth = %A" synth

//let lig = ligationReactions m n g
//printfn "lig = %A" lig

let sed = model.sedimentationDirect
printfn "sed = %A" sed

