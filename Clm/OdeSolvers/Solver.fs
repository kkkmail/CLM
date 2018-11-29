﻿namespace OdeSolvers

open Microsoft.FSharp.Core
open System

module Solver = 

    type OdeParams = 
        {
            startTime : double
            endTime : double
            stepSize : double
            eps : double
            noOfOutputPoints : int option
        }

        static member defaultValue =
            {
                startTime = 0.0
                endTime = 10.0
                stepSize = 0.01
                eps = 0.00001
                noOfOutputPoints = Some 1000
            }


    type OdeResult = 
        {
            y0 : double
            noOfOutputPoints : int
            startTime : double
            endTime : double
            t : double[]
            x : double[,]
        }


    let defaultInit n y0 = 
        let mult = 0.01
        let rnd = new Random(12345)
        let i0 = [ for i in 1..(n-1) -> (mult * y0 / (double n)) * rnd.NextDouble() ]
        (y0 - (i0 |> List.sum)) :: i0 |> Array.ofList


    /// F# wrapper around Alglib ODE solver.
    //let nSolve (p : OdeParams) (f : double[] -> double -> double[]) (i : array<double>) : OdeResult = 
    let nSolve tEnd (g : double[] -> double[]) n y0 : OdeResult = 
        printfn "nSolve::Starting."
        let p = { OdeParams.defaultValue with endTime = tEnd }
        let f (x : double[]) (_ : double) : double[] = g x
        let i = defaultInit n y0

        let eps = p.eps
        let h = p.stepSize

        let nt = 
            match p.noOfOutputPoints with 
            | Some p when p >= 2 -> p
            | _ -> 2

        let x : array<double> = [| for i in 0..nt -> p.startTime + (p.endTime - p.startTime) * (double i) / (double nt) |]

        printfn "nSolve::About to call alglib.ndimensional_ode_rp."
        let d = alglib.ndimensional_ode_rp (fun x t y _ -> f x t |> Array.mapi(fun i e -> y.[i] <- e) |> ignore)

        printfn "nSolve::About to call alglib.odesolverrkck."
        let mutable s = alglib.odesolverrkck(i, x, eps, h)

        printfn "nSolve::About to call alglib.odesolversolve."
        do alglib.odesolversolve(s, d, null)
        let mutable (m, xtbl, ytbl, rep) = alglib.odesolverresults(s)

        {
            y0 = y0
            noOfOutputPoints = nt
            startTime = p.startTime
            endTime = p.endTime
            t = xtbl
            x = ytbl 
        }
