//===========================================================
printfn "Starting..."
#load "References.fsx"
//===========================================================
open System
open Microsoft.FSharp.Core
open Model.ModelData
open OdeSolvers.Solver
open OdeSolvers.Visualization
//===========================================================
let y00 = 1000.0
let tEnd = 10000.0
let useTempFolder = true
//===========================================================
let y0 = y00 * (2.0 * (double modelDataParamsWithExtraData.modelDataParams.modelInfo.numberOfAminoAcids.length))
printfn "Solving for n = %A, y0 = %A..." numberOfSubstances y0
printfn "Starting at: %A" DateTime.Now

printfn "Calling defaultInit."
let i = defaultInit numberOfSubstances y0

printfn "Calling update."
let d = update i

printfn "Calling nSolve..."
#time
let result = nSolve tEnd update numberOfSubstances y0
#time

printfn "Plotting."
let plotter = new Plotter({ PlotDataInfo.defaultValue with useTempFolder = useTempFolder }, modelDataParamsWithExtraData, result)
//plotter.plotAll()
plotter.plotAminoAcids()
plotter.plotTotalSubst()
plotter.plotEnantiomericExcess()
printfn "Completed."
