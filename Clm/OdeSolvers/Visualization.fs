namespace OdeSolvers

open System
open Clm.Substances
open Clm.Model
open OdeSolvers.Solver
open Microsoft.FSharp.Core
open FSharp.Plotly

module Visualization = 

    type Plotter(p : ModelDataParams, o : OdeResult) =
        let description = 
            [
                "end time: ", sprintf "%A" o.endTime
                "y0:", sprintf "%A" o.y0
                "number of amino acids: ", sprintf "%A" p.numberOfAminoAcids.length
                "max peptide length: ", sprintf "%A" p.maxPeptideLength.length
                "number of substances: ", sprintf "%A" p.maxPeptideLength.length
            ]
            @
            (p.allReactions |> List.map (fun (r, c) -> r.name + ": ", c.ToString()))
            @
            (p.allRawReactions |> List.map (fun (r, c) -> r.name + " (raw): ", c.ToString()))
            |> List.map (fun (n, d) -> n + d)
            |> String.concat ", "

        let plotAllImpl (r : OdeResult) =
            let fn = [ for i in 0..p.numberOfSubstances - 1 -> i ]
            let tIdx = [ for i in 0..o.noOfOutputPoints -> i ]

            let getFuncData i = 
                tIdx
                |> List.map (fun t -> r.t.[t], r.x.[t,i])

            Chart.Combine (fn |> List.map (fun i -> Chart.Line(getFuncData i, Name = i.ToString())))
            |> Chart.withX_AxisStyle("t", MinMax = (o.startTime, o.endTime))
            |> Chart.ShowWithDescription description


        let plotAminoAcidsImpl (r : OdeResult) =
            let fn = [ for i in 0..(p.numberOfAminoAcids.length * 2 - 1) -> i ]

            let name i = 
                let idx = i / 2
                if idx * 2 = i then AminoAcid.toString idx else (AminoAcid.toString idx).ToLower()

            let tIdx = [ for i in 0..o.noOfOutputPoints -> i ]
            let a = tIdx |> Array.ofList |> Array.map (fun t -> p.getTotals r.x.[t,*])

            let d t i = 
                let idx = i / 2
                if idx * 2 = i then a.[t].[idx] |> fst else a.[t].[idx] |> snd

            let getFuncData i = tIdx |> List.map (fun t -> r.t.[t], d t i)

            Chart.Combine (fn |> List.map (fun i -> Chart.Line(getFuncData i, Name = name i)))
            |> Chart.withX_AxisStyle("t", MinMax = (o.startTime, o.endTime))
            |> Chart.ShowWithDescription description


        let plotEnantiomericExcessImpl (r : OdeResult) =
            let fn = [ for i in 0..(p.numberOfAminoAcids.length - 1) -> i ]

            let name (i : int) = 
                let l = AminoAcid.toString i
                let d = l.ToLower()
                "(" + l + " - " + d + ") / (" + l + " + " + d + ")"

            let tIdx = [ for i in 0..o.noOfOutputPoints -> i ]
            let a = tIdx |> Array.ofList |> Array.map (fun t -> p.getTotals r.x.[t,*])

            let d t i = 
                let (l, d) = a.[t].[i]
                if (l + d) > 0.0 then (l - d) / (l + d) else 0.0

            let getFuncData i = tIdx |> List.map (fun t -> r.t.[t], d t i)

            Chart.Combine (fn |> List.map (fun i -> Chart.Line(getFuncData i, Name = name i)))
            |> Chart.withX_AxisStyle("t", MinMax = (o.startTime, o.endTime))
            |> Chart.ShowWithDescription description


        let plotTotalSubstImpl (r : OdeResult) =
            let tIdx = [ for i in 0..o.noOfOutputPoints -> i ]
            let totalData = tIdx |> List.map (fun t -> r.t.[t], p.getTotalSubst r.x.[t,*])
            let yData = tIdx |> List.map (fun t -> r.t.[t], r.x.[t,0])
            let minData = tIdx |> List.map (fun t -> r.t.[t], r.x.[t,*] |> Array.min)

            let levelData level = 
                let levelSubst = 
                    p.allSubst
                    |> List.filter (fun s -> s.length = level)
                    |> List.map (fun s -> p.allInd.[s])

                let xData t =
                    let d = r.x.[t,*]
                    levelSubst |> List.sumBy (fun i -> (double level) * d.[i])

                tIdx |> List.map (fun t -> r.t.[t], xData t)


            Chart.Combine(
                    [ Chart.Line(totalData, Name = "Total"); Chart.Line(minData, Name = "Min"); Chart.Line(yData, Name = Substance.food.name) ]
                    @ [ for level in 1..p.maxPeptideLength.length -> Chart.Line(levelData level, Name = level.ToString()) ]
                    )
            |> Chart.withX_AxisStyle("t", MinMax = (o.startTime, o.endTime))
            |> Chart.ShowWithDescription description


        member __.plotAll() = plotAllImpl o
        member __.plotAminoAcids() = plotAminoAcidsImpl o
        member __.plotTotalSubst() = plotTotalSubstImpl o
        member __.plotEnantiomericExcess() = plotEnantiomericExcessImpl o
