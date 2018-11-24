//===========================================================
printfn "Starting..."
#load "References.fsx"
//===========================================================
open System
open Microsoft.FSharp.Core
open Clm.Substances
open Clm.Model
open Model.ModelData
open OdeSolvers.Solver
open FSharp.Plotly
open OdeSolvers.Visualization

let n = numberOfSubstances
let noOfOutputPoints = 100
let tEnd = 100.0
let odeParams = { OdeParams.defaultValue with endTime = tEnd; noOfOutputPoints = Some noOfOutputPoints }

let f (x : double[]) (t : double) : double[] = update x

let y0 = 1000.0
let mult = 0.01
let rnd = new Random(12345)
let i0 = [ for i in 1..(n-1) -> mult * rnd.NextDouble() ]
let i = (y0 - (i0 |> List.sum)) :: i0 |> Array.ofList

printfn "Solving for n = %A..." n

#time
let result = nSolve odeParams f i
#time

printfn "Plotting."
let plotter = new Plotter(modelDataParams, result)
plotter.plotAminoAcids()
plotter.plotTotalSubst()
plotter.plotEnantiomericExcess()
printfn "Completed."
