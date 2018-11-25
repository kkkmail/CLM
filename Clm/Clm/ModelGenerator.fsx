#r "./bin/Debug/MathNet.Numerics.dll"
#r "./bin/Debug/MathNet.Numerics.FSharp.dll"
#load "Substances.fs"
#load "ReactionTypes.fs"
#load "ReactionRates.fs"
#load "Reactions.fs"
#load "Model.fs"

open System.IO
open Clm.Substances
open Clm.ReactionRates
open Clm.Model
open System

let n = NumberOfAminoAcids.OneAminoAcid
let m = MaxPeptideLength.ThreeMax

let seed = 12345
let rnd = new Random(seed)

let sdMult = 100.0
let sdThreshold = 0.01
let saMult = 0.1

let synthModel = ReactionRateProvider.defaultSynthesisModel rnd 0.01 0.001
let catSynthModel = ReactionRateProvider.defaultCatalyticSynthesisModel rnd synthModel (Some 0.01) 1000.0
let ligModel = ReactionRateProvider.defaultLigationModel rnd 1.0 1.0
let sdModel = ReactionRateProvider.defaultSedimentationDirectModel rnd sdThreshold sdMult
let saModel = ReactionRateProvider.defaultSedimentationAllModel rnd saMult

let rates = 
    [
         //synthModel |> SynthesisRateModel
         //catSynthModel |> CatalyticSynthesisRateModel
         ligModel |> LigationRateModel
         //(CatalyticLigation, (fun __ -> (Some (ReactionRate 5.0), Some (ReactionRate 0.5))) |> ReactionRateProvider)
         //sdModel |> SedimentationDirectRateModel
         //saModel |> SedimentationAllRateModel
    ]


let modelParams = 
    {
        seedValue = Some seed
        numberOfAminoAcids = n
        maxPeptideLength = m
        reactionRateModels = rates
    }


printfn "Creating model..."
#time
let model = ClmModel modelParams
#time

printfn "allSubstances.Length = %A" model.allSubstances.Length
printfn "allReactions.Length = %A" model.allReactions.Length

printfn "Generating..."
#time
let s = model.generateCode()
#time

printfn "Writing..."
#time
File.WriteAllLines("c:\\Temp\ModelData.fs", s)
#time
printfn "Done"
