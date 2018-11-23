//===========================================================
printfn "Starting..."
#load "References.fsx"
#r "./bin/Debug/Clm.dll"
#r "./bin/Debug/Model.dll"
//===========================================================
open System
open OdeSolvers.Solver
open Microsoft.FSharp.Core
open FSharp.Plotly
open Model.ModelData
open Clm.Substances

let n = numberOfSubstances
let noOfOutputPoints = 100
let tEnd = 1000.0
let odeParams = { OdeParams.defaultValue with endTime = tEnd; noOfOutputPoints = Some noOfOutputPoints }


let plotAll (r : OdeResult) =
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

let plotAminoAcids (r : OdeResult) =
    let description = "Some description"
    let fn = [ for i in 0..(numberOfAminoAcids.length * 2 - 1) -> i ]

    let name i = 
        if i < numberOfAminoAcids.length 
        then AminoAcid.toString i
        else (AminoAcid.toString (i - numberOfAminoAcids.length)).ToLower()

    let tIdx = [ for i in 0..noOfOutputPoints -> i ]

    let d t = getTotals r.x.[t,*]

    let getFuncData i = 
        tIdx |> List.map (fun t -> r.t.[t], r.x.[t,i])

    //FSharp.Plotly
    Chart.Combine (fn |> List.map (fun i -> Chart.Line(getFuncData i, Name = name i)))
    |> Chart.withX_AxisStyle("t", MinMax = (0.0, tEnd))
    |> Chart.ShowWithDescription description


let f (x : double[]) (t : double) : double[] = update x

let mult = 1.0
let rnd = new Random(12345)
let i = [| for i in 1..n -> mult * rnd.NextDouble() |]

printfn "Solving for n = %A..." n

#time
let result = nSolve odeParams f i
#time

let r1 = result.x.[1,*]
printfn "r1 = %A" r1

printfn "Plotting."
plotAminoAcids result
printfn "Completed."
