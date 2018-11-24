//===========================================================
printfn "Starting..."
#load "References.fsx"
//===========================================================
open Microsoft.FSharp.Core
open Model.ModelData
open OdeSolvers.Solver
open OdeSolvers.Visualization

let y0 = 1000.0
let tEnd = 100.0

printfn "Solving for n = %A..." numberOfSubstances

#time
let result = nSolve tEnd update numberOfSubstances y0
#time

printfn "Plotting."
let plotter = new Plotter(modelDataParams, result)
plotter.plotAminoAcids()
plotter.plotTotalSubst()
plotter.plotEnantiomericExcess()
printfn "Completed."
