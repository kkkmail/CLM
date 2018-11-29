open System
open Microsoft.FSharp.Core
open Model.ModelData

[<EntryPoint>]
let main argv = 
    let defaultInit n y0 = 
        let mult = 0.01
        let rnd = new Random(12345)
        let i0 = [ for i in 1..(n-1) -> (mult * y0 / (double n)) * rnd.NextDouble() ]
        (y0 - (i0 |> List.sum)) :: i0 |> Array.ofList

    printfn "Calling defaultInit."
    let i = defaultInit numberOfSubstances 1000.0

    printfn "Calling update."
    let d = update i
    printfn "Succeeded."

    0
