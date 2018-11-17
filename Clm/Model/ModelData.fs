namespace Model

module ModelData = 

    let getTotals (x : array<double>) = 
        [
            // A
            (
                [
                    x.[0] // A
                    2.0 * x.[1] // AA
                ]
                |> List.sum
                ,
                [
                    x.[0]
                    2.0 * x.[1]
                ]
                |> List.sum
            )
        ]
        //|> List.fold (fun (al, ar) (l, r) -> (al + l, ar + r)) (0.0, 0.0)

    let update (x : array<double>) : array<double> = 
        [|
            // 0 - Y
            [|
                4.0 * 1000.0 * x.[5] * x.[3] // aA + AA | SedimentationDirect: aA + AA -> 4 Y
                4.0 * 1000.0 * x.[4] * x.[6] // Aa + aa | SedimentationDirect: Aa + aa -> 4 Y
                4.0 * 1000.0 * x.[5] * x.[4] // aA + Aa | SedimentationDirect: aA + Aa -> 4 Y
                4.0 * 1000.0 * x.[4] * x.[5] // Aa + aA | SedimentationDirect: Aa + aA -> 4 Y
                4.0 * 1000.0 * x.[5] * x.[5] // aA + aA | SedimentationDirect: aA + aA -> 4 Y
                4.0 * 1000.0 * x.[4] * x.[4] // Aa + Aa | SedimentationDirect: Aa + Aa -> 4 Y
                4.0 * 1000.0 * x.[6] * x.[3] // aa + AA | SedimentationDirect: aa + AA -> 4 Y
                4.0 * 1000.0 * x.[3] * x.[6] // AA + aa | SedimentationDirect: AA + aa -> 4 Y
                4.0 * 1000.0 * x.[6] * x.[4] // aa + Aa | SedimentationDirect: aa + Aa -> 4 Y
                4.0 * 1000.0 * x.[3] * x.[5] // AA + aA | SedimentationDirect: AA + aA -> 4 Y
                4.0 * 1000.0 * x.[6] * x.[5] // aa + aA | SedimentationDirect: aa + aA -> 4 Y
                4.0 * 1000.0 * x.[3] * x.[4] // AA + Aa | SedimentationDirect: AA + Aa -> 4 Y
                4.0 * 1000.0 * x.[6] * x.[6] // aa + aa | SedimentationDirect: aa + aa -> 4 Y
                4.0 * 1000.0 * x.[3] * x.[3] // AA + AA | SedimentationDirect: AA + AA -> 4 Y
                3.0 * 1000.0 * x.[2] * x.[3] // a + AA | SedimentationDirect: a + AA -> 3 Y
                3.0 * 1000.0 * x.[1] * x.[6] // A + aa | SedimentationDirect: A + aa -> 3 Y
                3.0 * 1000.0 * x.[2] * x.[4] // a + Aa | SedimentationDirect: a + Aa -> 3 Y
                3.0 * 1000.0 * x.[1] * x.[5] // A + aA | SedimentationDirect: A + aA -> 3 Y
                3.0 * 1000.0 * x.[2] * x.[5] // a + aA | SedimentationDirect: a + aA -> 3 Y
                3.0 * 1000.0 * x.[1] * x.[4] // A + Aa | SedimentationDirect: A + Aa -> 3 Y
                3.0 * 1000.0 * x.[2] * x.[6] // a + aa | SedimentationDirect: a + aa -> 3 Y
                3.0 * 1000.0 * x.[1] * x.[3] // A + AA | SedimentationDirect: A + AA -> 3 Y
                2.0 * 1000.0 * x.[2] * x.[1] // a + A | SedimentationDirect: a + A -> 2 Y
                2.0 * 1000.0 * x.[1] * x.[2] // A + a | SedimentationDirect: A + a -> 2 Y
                2.0 * 1000.0 * x.[2] * x.[2] // a + a | SedimentationDirect: a + a -> 2 Y
                2.0 * 1000.0 * x.[1] * x.[1] // A + A | SedimentationDirect: A + A -> 2 Y
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0

            // 1 - A
            [|
                -1000.0 * x.[1] * x.[6] // A + aa | SedimentationDirect: A + aa -> 3 Y
                -1000.0 * x.[1] * x.[5] // A + aA | SedimentationDirect: A + aA -> 3 Y
                -1000.0 * x.[1] * x.[4] // A + Aa | SedimentationDirect: A + Aa -> 3 Y
                -1000.0 * x.[1] * x.[3] // A + AA | SedimentationDirect: A + AA -> 3 Y
                -1000.0 * x.[2] * x.[1] // a + A | SedimentationDirect: a + A -> 2 Y
                -1000.0 * x.[1] * x.[2] // A + a | SedimentationDirect: A + a -> 2 Y
                -1000.0 * x.[1] * x.[1] // A + A | SedimentationDirect: A + A -> 2 Y
                -1000.0 * x.[1] * x.[1] // A + A | SedimentationDirect: A + A -> 2 Y
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0

            // 2 - a
            [|
                -1000.0 * x.[2] * x.[3] // a + AA | SedimentationDirect: a + AA -> 3 Y
                -1000.0 * x.[2] * x.[4] // a + Aa | SedimentationDirect: a + Aa -> 3 Y
                -1000.0 * x.[2] * x.[5] // a + aA | SedimentationDirect: a + aA -> 3 Y
                -1000.0 * x.[2] * x.[6] // a + aa | SedimentationDirect: a + aa -> 3 Y
                -1000.0 * x.[2] * x.[1] // a + A | SedimentationDirect: a + A -> 2 Y
                -1000.0 * x.[1] * x.[2] // A + a | SedimentationDirect: A + a -> 2 Y
                -1000.0 * x.[2] * x.[2] // a + a | SedimentationDirect: a + a -> 2 Y
                -1000.0 * x.[2] * x.[2] // a + a | SedimentationDirect: a + a -> 2 Y
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0

            // 3 - AA
            [|
                -1000.0 * x.[5] * x.[3] // aA + AA | SedimentationDirect: aA + AA -> 4 Y
                -1000.0 * x.[6] * x.[3] // aa + AA | SedimentationDirect: aa + AA -> 4 Y
                -1000.0 * x.[3] * x.[6] // AA + aa | SedimentationDirect: AA + aa -> 4 Y
                -1000.0 * x.[3] * x.[5] // AA + aA | SedimentationDirect: AA + aA -> 4 Y
                -1000.0 * x.[3] * x.[4] // AA + Aa | SedimentationDirect: AA + Aa -> 4 Y
                -1000.0 * x.[3] * x.[3] // AA + AA | SedimentationDirect: AA + AA -> 4 Y
                -1000.0 * x.[3] * x.[3] // AA + AA | SedimentationDirect: AA + AA -> 4 Y
                -1000.0 * x.[2] * x.[3] // a + AA | SedimentationDirect: a + AA -> 3 Y
                -1000.0 * x.[1] * x.[3] // A + AA | SedimentationDirect: A + AA -> 3 Y
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0

            // 4 - Aa
            [|
                -1000.0 * x.[4] * x.[6] // Aa + aa | SedimentationDirect: Aa + aa -> 4 Y
                -1000.0 * x.[5] * x.[4] // aA + Aa | SedimentationDirect: aA + Aa -> 4 Y
                -1000.0 * x.[4] * x.[5] // Aa + aA | SedimentationDirect: Aa + aA -> 4 Y
                -1000.0 * x.[4] * x.[4] // Aa + Aa | SedimentationDirect: Aa + Aa -> 4 Y
                -1000.0 * x.[4] * x.[4] // Aa + Aa | SedimentationDirect: Aa + Aa -> 4 Y
                -1000.0 * x.[6] * x.[4] // aa + Aa | SedimentationDirect: aa + Aa -> 4 Y
                -1000.0 * x.[3] * x.[4] // AA + Aa | SedimentationDirect: AA + Aa -> 4 Y
                -1000.0 * x.[2] * x.[4] // a + Aa | SedimentationDirect: a + Aa -> 3 Y
                -1000.0 * x.[1] * x.[4] // A + Aa | SedimentationDirect: A + Aa -> 3 Y
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0

            // 5 - aA
            [|
                -1000.0 * x.[5] * x.[3] // aA + AA | SedimentationDirect: aA + AA -> 4 Y
                -1000.0 * x.[5] * x.[4] // aA + Aa | SedimentationDirect: aA + Aa -> 4 Y
                -1000.0 * x.[4] * x.[5] // Aa + aA | SedimentationDirect: Aa + aA -> 4 Y
                -1000.0 * x.[5] * x.[5] // aA + aA | SedimentationDirect: aA + aA -> 4 Y
                -1000.0 * x.[5] * x.[5] // aA + aA | SedimentationDirect: aA + aA -> 4 Y
                -1000.0 * x.[3] * x.[5] // AA + aA | SedimentationDirect: AA + aA -> 4 Y
                -1000.0 * x.[6] * x.[5] // aa + aA | SedimentationDirect: aa + aA -> 4 Y
                -1000.0 * x.[1] * x.[5] // A + aA | SedimentationDirect: A + aA -> 3 Y
                -1000.0 * x.[2] * x.[5] // a + aA | SedimentationDirect: a + aA -> 3 Y
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0

            // 6 - aa
            [|
                -1000.0 * x.[4] * x.[6] // Aa + aa | SedimentationDirect: Aa + aa -> 4 Y
                -1000.0 * x.[6] * x.[3] // aa + AA | SedimentationDirect: aa + AA -> 4 Y
                -1000.0 * x.[3] * x.[6] // AA + aa | SedimentationDirect: AA + aa -> 4 Y
                -1000.0 * x.[6] * x.[4] // aa + Aa | SedimentationDirect: aa + Aa -> 4 Y
                -1000.0 * x.[6] * x.[5] // aa + aA | SedimentationDirect: aa + aA -> 4 Y
                -1000.0 * x.[6] * x.[6] // aa + aa | SedimentationDirect: aa + aa -> 4 Y
                -1000.0 * x.[6] * x.[6] // aa + aa | SedimentationDirect: aa + aa -> 4 Y
                -1000.0 * x.[1] * x.[6] // A + aa | SedimentationDirect: A + aa -> 3 Y
                -1000.0 * x.[2] * x.[6] // a + aa | SedimentationDirect: a + aa -> 3 Y
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0
        |]
