﻿#r "./bin/Debug/MathNet.Numerics.dll"
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

//let n = NumberOfAminoAcids.OneAminoAcid
//let m = MaxPeptideLength.TwoMax

let rnd = new Random()

let sdMult = 1000.0
let sdThreshold = 0.01
let triangularDistr (r : Random) = 1.0 - sqrt(1.0 - r.NextDouble())
let sdDistr() = ((if rnd.NextDouble() < sdThreshold then sdMult * (triangularDistr rnd) |> ReactionRate |> Some else None), None)

let rates = 
    [
         //(Synthesis, (fun __ -> (Some (ReactionRate 0.001), Some (ReactionRate 0.0001))) |> ReactionRateProvider)
         //(CatalyticSynthesis, (fun __ -> (Some (ReactionRate 10.0), Some (ReactionRate 0.01))) |> ReactionRateProvider)
         //(Ligation, (fun __ -> (Some (ReactionRate 1.0), Some (ReactionRate 0.1))) |> ReactionRateProvider)
         //(CatalyticLigation, (fun __ -> (Some (ReactionRate 5.0), Some (ReactionRate 0.5))) |> ReactionRateProvider)
         (SedimentationDirectName, (fun __ -> sdDistr()) |> ReactionRateProvider)
    ]


let modelParams = 
    {
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

//let peptides = Peptide.getPeptides m n 
//printfn "peptides = %A" peptides
//printfn "peptides (sorted) = %A" (peptides |> List.sort)

//let synth = synthesisReactions n g
//printfn "synth = %A" synth

//let lig = ligationReactions m n g
//printfn "lig = %A" lig

//let sed = model.sedimentationDirect
//printfn "sed.Length = %A" sed.Length

//printfn "model.allReactions.Length = %A" model.allReactions.Length
//model.allReactions |> List.map (fun r -> printfn "%A" r.name)

//printfn "Getting x..."
//let x = model.updateAllReacions |> Map.toList |> List.head |> snd
//printfn "x = %A" x

//let i = model.allSubstances |> List.map (fun s -> s, 0.0) |> Map.ofList

//printfn "Getting y..."
//let y = x i
//printfn "y = %A" y

//printfn "model.updateAllReacions.Length = %A" (model.updateAllReacions |> Map.toList).Length


// |> vector

//#time
//let d = model.getGradient input
//#time

//printfn "d.Count = %A" d.Length

//let input1 = model.allSubstances |> List.map (fun s -> 0.0) |> vector

//#time
//let d1 = model.getGradient input1
//#time

//printfn "d1.Count = %A" d1.Length

printfn "Generating..."
#time
let s = model.generateCode()
#time

//printfn "s = \n%A" s

printfn "Writing..."
#time
File.WriteAllLines("c:\\Temp\ModelData.fs", s)
#time
printfn "Done"
