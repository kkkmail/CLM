namespace OdeSolvers

open System
open OdeSolvers.Solver
open Microsoft.FSharp.Core
open FSharp.Plotly
open Clm.Substances

module Visualization = 

    type VisualizationParams = 
        {
            noOfOutputPoints : int
            numberOfSubstances : int
            numberOfAminoAcids : NumberOfAminoAcids
            maxPeptideLength : MaxPeptideLength
            odeResult : OdeResult
            tEnd : double
            getTotals : array<double> -> array<double * double>
            getTotalSubst : array<double> -> double
        }


    type Visualization (p : VisualizationParams) =
        let description = sprintf "Number of amino acids: %A, number of peptides: %A, number of substances: %A." p.numberOfAminoAcids.length p.maxPeptideLength.length p.numberOfSubstances


        let plotAllImpl (r : OdeResult) =
            let fn = [ for i in 0..p.numberOfSubstances - 1 -> i ]
            let tIdx = [ for i in 0..p.noOfOutputPoints -> i ]

            let getFuncData i = 
                tIdx
                |> List.map (fun t -> r.t.[t], r.x.[t,i])

            //FSharp.Plotly
            Chart.Combine (fn |> List.map (fun i -> Chart.Line(getFuncData i, Name = i.ToString())))
            |> Chart.withX_AxisStyle("t", MinMax = (0.0, p.tEnd))
            |> Chart.ShowWithDescription description


        let plotAminoAcidsImpl (r : OdeResult) =
            let fn = [ for i in 0..(p.numberOfAminoAcids.length * 2 - 1) -> i ]

            let name i = 
                let idx = i / 2

                if idx * 2 = i 
                then AminoAcid.toString idx
                else (AminoAcid.toString idx).ToLower()

            let tIdx = [ for i in 0..p.noOfOutputPoints -> i ]

            let a = tIdx |> Array.ofList |> Array.map (fun t -> p.getTotals r.x.[t,*])

            let d t i = 
                let idx = i / 2

                if idx * 2 = i 
                then a.[t].[idx] |> fst
                else a.[t].[idx] |> snd

            let getFuncData i = 
                tIdx |> List.map (fun t -> r.t.[t], d t i)

            //FSharp.Plotly
            Chart.Combine (fn |> List.map (fun i -> Chart.Line(getFuncData i, Name = name i)))
            |> Chart.withX_AxisStyle("t", MinMax = (0.0, p.tEnd))
            |> Chart.ShowWithDescription description


        let plotTotalSubstImpl (r : OdeResult) =
            let tIdx = [ for i in 0..p.noOfOutputPoints -> i ]
            let totalData = tIdx |> List.map (fun t -> r.t.[t], p.getTotalSubst r.x.[t,*])
            let yData = tIdx |> List.map (fun t -> r.t.[t], r.x.[t,0])

            //FSharp.Plotly
            Chart.Combine([ Chart.Line(totalData, Name = "Total"); Chart.Line(yData, Name = Substance.food.name) ])
            |> Chart.withX_AxisStyle("t", MinMax = (0.0, p.tEnd))
            |> Chart.ShowWithDescription description


        member this.plotAll() = plotAllImpl p.odeResult
        member this.plotAminoAcids() = plotAminoAcidsImpl p.odeResult
        member this.plotTotalSubst() = plotTotalSubstImpl p.odeResult

