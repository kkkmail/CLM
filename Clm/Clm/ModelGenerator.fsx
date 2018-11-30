#r "./bin/Debug/MathNet.Numerics.dll"
#r "./bin/Debug/MathNet.Numerics.FSharp.dll"
#load "Substances.fs"
#load "ReactionTypes.fs"
#load "ReactionRates.fs"
#load "Reactions.fs"
#load "DataLocation.fs"
#load "Model.fs"

open System.IO
open Clm.Substances
open Clm.ReactionRates
open Clm.DataLocation
open Clm.Model
open System

let n = NumberOfAminoAcids.NineAminoAcids
let m = MaxPeptideLength.ThreeMax

let seed = (new Random()).Next()
//let seed = 842740526
let rnd = new Random(seed)

let synthModel = ReactionRateProvider.defaultSynthesisModel rnd 0.001 0.0001
let catSynthModel = ReactionRateProvider.defaultCatalyticSynthesisModel rnd synthModel (Some 0.0005) 1000.0
let ligModel = ReactionRateProvider.defaultLigationModel rnd 0.001 0.0001
let catLigModel = ReactionRateProvider.defaultCatalyticLigationModel rnd ligModel (Some 0.0001) 1000.0
let sdModel = ReactionRateProvider.defaultSedimentationDirectModel rnd 0.0001 100.0
let saModel = ReactionRateProvider.defaultSedimentationAllModel rnd 0.1

let rates = 
    [
         synthModel |> SynthesisRateModel
         catSynthModel |> CatalyticSynthesisRateModel
         ligModel |> LigationRateModel
         catLigModel |> CatalyticLigationRateModel
         sdModel |> SedimentationDirectRateModel
         //saModel |> SedimentationAllRateModel
    ]


let modelGenerationParams = 
    {
        versionNumber = "1.0.0.0"
        seedValue = Some seed
        numberOfAminoAcids = n
        maxPeptideLength = m
        reactionRateModels = rates
        updateFuncType = UseFunctions
        modelLocationData = ModelLocationInputData.defaultValue
    }


printfn "Creating model..."
printfn "Starting at: %A" DateTime.Now
#time
let model = ClmModel modelGenerationParams
#time

printfn "allSubstances.Length = %A" model.allSubstances.Length
printfn "allReactions.Length = %A" model.allReactions.Length

printfn "Generating..."
#time
let s = model.generateCode()
#time

let info = model.locationInfo

printfn "Writing..."
#time
File.WriteAllLines(info.outputFile, s)
#time
printfn "Done."

printfn "Updating %A..." modelGenerationParams.modelLocationData.allModelsFile
File.AppendAllLines(modelGenerationParams.modelLocationData.allModelsFile, [ model.allModelData ])
printfn "Done."
