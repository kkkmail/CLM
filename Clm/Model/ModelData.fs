namespace Model

module ModelData = 

    let getTotals (x : array<double>) = 
        [|
            // A
            (
                [|
                    x.[1] // A
                    2.0 * x.[3] // AA
                    x.[4] // Aa
                    x.[5] // aA
                    3.0 * x.[7] // AAA
                    2.0 * x.[8] // AAa
                    2.0 * x.[9] // AaA
                    x.[10] // Aaa
                    2.0 * x.[11] // aAA
                    x.[12] // aAa
                    x.[13] // aaA
                |]
                |> Array.sum
                ,
                [|
                    x.[2] // a
                    x.[4] // Aa
                    x.[5] // aA
                    2.0 * x.[6] // aa
                    x.[8] // AAa
                    x.[9] // AaA
                    2.0 * x.[10] // Aaa
                    x.[11] // aAA
                    2.0 * x.[12] // aAa
                    2.0 * x.[13] // aaA
                    3.0 * x.[14] // aaa
                |]
                |> Array.sum
            )
        |]


    let update (x : array<double>) : array<double> = 
        let xSum = 
            [|
                x.[1] // A
                x.[2] // a
                x.[3] // AA
                x.[4] // Aa
                x.[5] // aA
                x.[6] // aa
                x.[7] // AAA
                x.[8] // AAa
                x.[9] // AaA
                x.[10] // Aaa
                x.[11] // aAA
                x.[12] // aAa
                x.[13] // aaA
                x.[14] // aaa
            |]
            |> Array.sum

        [|

            // 0 - Y
            [|
                6.0 * 162.878211755629 * x.[13] * x.[9] // aaA + AaA | SedimentationDirect: aaA + AaA -> 6 Y
                6.0 * 162.878211755629 * x.[8] * x.[12] // AAa + aAa | SedimentationDirect: AAa + aAa -> 6 Y
                5.0 * 277.553801030124 * x.[5] * x.[12] // aA + aAa | SedimentationDirect: aA + aAa -> 5 Y
                5.0 * 277.553801030124 * x.[4] * x.[9] // Aa + AaA | SedimentationDirect: Aa + AaA -> 5 Y
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 1 - A
            [|
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 2 - a
            [|
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 3 - AA
            [|
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 4 - Aa
            [|
                -277.553801030124 * x.[4] * x.[9] // Aa + AaA | SedimentationDirect: Aa + AaA -> 5 Y
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 5 - aA
            [|
                -277.553801030124 * x.[5] * x.[12] // aA + aAa | SedimentationDirect: aA + aAa -> 5 Y
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 6 - aa
            [|
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 7 - AAA
            [|
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 8 - AAa
            [|
                -162.878211755629 * x.[8] * x.[12] // AAa + aAa | SedimentationDirect: AAa + aAa -> 6 Y
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 9 - AaA
            [|
                -162.878211755629 * x.[13] * x.[9] // aaA + AaA | SedimentationDirect: aaA + AaA -> 6 Y
                -277.553801030124 * x.[4] * x.[9] // Aa + AaA | SedimentationDirect: Aa + AaA -> 5 Y
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 10 - Aaa
            [|
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 11 - aAA
            [|
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 12 - aAa
            [|
                -162.878211755629 * x.[8] * x.[12] // AAa + aAa | SedimentationDirect: AAa + aAa -> 6 Y
                -277.553801030124 * x.[5] * x.[12] // aA + aAa | SedimentationDirect: aA + aAa -> 5 Y
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 13 - aaA
            [|
                -162.878211755629 * x.[13] * x.[9] // aaA + AaA | SedimentationDirect: aaA + AaA -> 6 Y
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 14 - aaa
            [|
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0

        |]

