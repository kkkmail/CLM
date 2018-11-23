﻿//===========================================================
printfn "Starting..."
#load "References.fsx"
#r "./bin/Debug/Model.dll"
//===========================================================
open System
open OdeSolvers.Solver
open Microsoft.FSharp.Core
open FSharp.Plotly
open Model.ModelData

let n = numberOfSubstances
let noOfOutputPoints = 100
let tEnd = 1000.0
let odeParams = { OdeParams.defaultValue with endTime = tEnd; noOfOutputPoints = Some noOfOutputPoints }


let plot (r : OdeResult) =
    let description = "Some description"
    let fn = [ for i in 0..n - 1 -> i ]
    let tIdx = [ for i in 0..noOfOutputPoints -> i ]

    let getFuncData i = 
        tIdx
        |> List.map (fun t -> r.t.[t], r.x.[t,i])

    //FSharp.Plotly
    Chart.Combine (fn |> List.map (fun i -> Chart.Line(getFuncData i, Name = i.ToString())))
    |> Chart.withX_AxisStyle("t", MinMax = (0.0, tEnd))
    |> Chart.ShowWithDescription description


let f (x : double[]) (t : double) : double[] = update x

let mult = 10.0
let rnd = new Random(12345)
let i = [| for i in 1..n -> mult * rnd.NextDouble() |]

printfn "Solving for n = %A..." n

#time
let result = nSolve odeParams f i
#time

let r1 = result.x.[1,*]
printfn "r1 = %A" r1

printfn "Plotting."
plot result
printfn "Completed."
