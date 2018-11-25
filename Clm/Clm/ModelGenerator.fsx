#r "./bin/Debug/MathNet.Numerics.dll"
#r "./bin/Debug/MathNet.Numerics.FSharp.dll"
#r "../packages/FSharp.Collections.ParallelSeq.1.1.2/lib/net45/FSharp.Collections.ParallelSeq.dll"
#load "Substances.fs"
#load "ReactionTypes.fs"
#load "ReactionRates.fs"
#load "Reactions.fs"
#load "Model.fs"

open System.IO
open Clm.Substances
open Clm.ReactionTypes
open Clm.ReactionRates
open Clm.Reactions
open Clm.Model
open System.Numerics
open MathNet.Numerics.LinearAlgebra
open System

let n = NumberOfAminoAcids.OneAminoAcid
let m = MaxPeptideLength.TwoMax

let seed = 12345
let rnd = new Random(seed)

let sdMult = 100.0
let sdThreshold = 0.01
let saMult = 0.1

let synthModel = ReactionRateProvider.defaultSynthesisModel rnd 0.01 0.001
let catSynthModel = ReactionRateProvider.defaultCatalyticSynthesisModel rnd synthModel None 1000.0
let sdModel = ReactionRateProvider.defaultSedimentationDirectModel rnd sdThreshold sdMult
let saModel = ReactionRateProvider.defaultSedimentationAllModel rnd saMult

let rates = 
    [
         synthModel |> SynthesisRateModel
         ////catSynthModel |> CatalyticSynthesisRateModel
         //(Ligation, (fun __ -> (Some (ReactionRate 1.0), Some (ReactionRate 0.1))) |> ReactionRateProvider)
         //(CatalyticLigation, (fun __ -> (Some (ReactionRate 5.0), Some (ReactionRate 0.5))) |> ReactionRateProvider)
         sdModel |> SedimentationDirectRateModel
         saModel |> SedimentationAllRateModel
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
