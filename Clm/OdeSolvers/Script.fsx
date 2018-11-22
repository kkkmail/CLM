//===========================================================
printfn "Starting..."
#load "References.fsx"
//===========================================================
open System
open OdeSolvers.Solver
open Microsoft.FSharp.Core

let n = 10
let tEnd = 1000.0
let odeParams = { OdeParams.defaultValue with endTime = tEnd }

let f (x : double[]) (t : double) : double[] = 
    let mult = -0.01 * (1.0 + 4.0 * cos(Math.PI * t / 4.0))
    x |> Array.mapi (fun i _ -> mult * x.[if (i + 1) < n then (i + 1) else 0])

let i = [| for i in 1..n -> double i |]

printfn "Solving for n = %A..." n

#time
let result1 = nSolve odeParams f i
#time

let r1 = result1.x.[1,*]
printfn "r1 = %A" r1
printfn "Completed."

