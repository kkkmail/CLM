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

let n = NumberOfAminoAcids.TwoAminoAcids
let m = MaxPeptideLength.ThreeMax

let seed = 12345
let rnd = new Random(seed)

let sdMult = 100.0
let sdThreshold = 0.01
let saMult = 0.1

let synthProvider = ReactionRateProvider.defaultSynthesisModel rnd 0.01 0.001
let sdProvider = ReactionRateProvider.defaultSedimentationDirectModel rnd sdThreshold sdMult
let saProvider = ReactionRateProvider.defaultSedimentationAllModel rnd saMult

let rates = 
    [
         (SynthesisName, synthProvider)
         //(CatalyticSynthesis, (fun __ -> (Some (ReactionRate 10.0), Some (ReactionRate 0.01))) |> ReactionRateProvider)
         //(Ligation, (fun __ -> (Some (ReactionRate 1.0), Some (ReactionRate 0.1))) |> ReactionRateProvider)
         //(CatalyticLigation, (fun __ -> (Some (ReactionRate 5.0), Some (ReactionRate 0.5))) |> ReactionRateProvider)
         (SedimentationDirectName, sdProvider)
         (SedimentationAllName, saProvider)
    ]


let modelParams = 
    {
        seedValue = Some seed
        numberOfAminoAcids = n
        maxPeptideLength = m
        reactionRates = rates
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
