namespace OdeSolvers

open Microsoft.FSharp.Core

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
                noOfOutputPoints = None
            }

    type OdeResult = 
        {
            noOfOutputPoints : int
            startTime : double
            endTime : double
            t : double[]
            x : double[,]
        }


    /// F# wrapper around Alglib ODE solver.
    let nSolve (p : OdeParams) (f : double[] -> double -> double[]) (i : array<double>) : OdeResult = 
        let eps = p.eps
        let h = p.stepSize

        let nt = 
            match p.noOfOutputPoints with 
            | Some p when p >= 2 -> p
            | _ -> 2

        let x : array<double> = [| for i in 0..nt -> p.startTime + (p.endTime - p.startTime) * (double i) / (double nt) |]
        let d = alglib.ndimensional_ode_rp (fun x t y _ -> f x t |> Array.mapi(fun i e -> y.[i] <- e) |> ignore)
        let mutable s = alglib.odesolverrkck(i, x, eps, h)
        do alglib.odesolversolve(s, d, null)
        let mutable (m, xtbl, ytbl, rep) = alglib.odesolverresults(s)

        {
            noOfOutputPoints = nt
            startTime = p.startTime
            endTime = p.endTime
            t = xtbl
            x = ytbl 
        }
