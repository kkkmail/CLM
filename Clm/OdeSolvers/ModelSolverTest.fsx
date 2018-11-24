//===========================================================
printfn "Starting..."
#load "References.fsx"
#r "./bin/Debug/Clm.dll"
#r "./bin/Debug/Model.dll"
//===========================================================
open System
open Microsoft.FSharp.Core
open FSharp.Plotly
open Model.ModelData
open Clm.Substances
open OdeSolvers.Solver
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
let plotter = Plotter(modelDataParams, result)
plotter.plotAminoAcids()
plotter.plotTotalSubst()
printfn "Completed."
