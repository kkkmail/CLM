//===========================================================
printfn "Starting..."
#load "References.fsx"
//===========================================================
open System
open Microsoft.FSharp.Core
open Model.ModelData
open OdeSolvers.Solver
open OdeSolvers.Visualization

let y0 = 1000.0
let tEnd = 10.0

printfn "Solving for n = %A..." numberOfSubstances
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
let plotter = new Plotter(modelDataParams, result)
//plotter.plotAll()
plotter.plotAminoAcids()
plotter.plotTotalSubst()
plotter.plotEnantiomericExcess()
printfn "Completed."
