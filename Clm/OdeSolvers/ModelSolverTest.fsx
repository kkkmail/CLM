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
open Clm.Substances

let n = numberOfSubstances
let noOfOutputPoints = 100
let tEnd = 100.0
let odeParams = { OdeParams.defaultValue with endTime = tEnd; noOfOutputPoints = Some noOfOutputPoints }
let description = sprintf "Number of amino acids: %A, number of peptides: %A, number of substances: %A." numberOfAminoAcids.length maxPeptideLength.length numberOfSubstances


let plotAll (r : OdeResult) =
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
    let fn = [ for i in 0..(numberOfAminoAcids.length * 2 - 1) -> i ]

    let name i = 
        let idx = i / 2

        if idx * 2 = i 
        then AminoAcid.toString idx
        else (AminoAcid.toString idx).ToLower()

    let tIdx = [ for i in 0..noOfOutputPoints -> i ]

    let a = tIdx |> Array.ofList |> Array.map (fun t -> getTotals r.x.[t,*])

    let d t i = 
        let idx = i / 2

        if idx * 2 = i 
        then a.[t].[idx] |> fst
        else a.[t].[idx] |> snd

    let getFuncData i = 
        tIdx |> List.map (fun t -> r.t.[t], d t i)

    //FSharp.Plotly
    Chart.Combine (fn |> List.map (fun i -> Chart.Line(getFuncData i, Name = name i)))
    |> Chart.withX_AxisStyle("t", MinMax = (0.0, tEnd))
    |> Chart.ShowWithDescription description


let plotTotalSubst (r : OdeResult) =
    let tIdx = [ for i in 0..noOfOutputPoints -> i ]
    let totalData = tIdx |> List.map (fun t -> r.t.[t], getTotalSubst r.x.[t,*])
    let yData = tIdx |> List.map (fun t -> r.t.[t], r.x.[t,0])

    //FSharp.Plotly
    Chart.Combine([ Chart.Line(totalData, Name = "Total"); Chart.Line(yData, Name = Substance.food.name) ])
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

//let r1 = result.x.[1,*]
//printfn "r1 = %A" r1

printfn "Plotting."
plotAminoAcids result
plotTotalSubst result
printfn "Completed."
