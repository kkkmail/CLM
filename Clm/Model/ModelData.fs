namespace Model

open Clm.Substances

open Clm.Model


module ModelData = 

    let seedValue = 12345
    let numberOfAminoAcids = NumberOfAminoAcids.OneAminoAcid
    let maxPeptideLength = MaxPeptideLength.TwoMax
    let numberOfSubstances = 7



    let getTotalSubst (x : array<double>) = 
        [|
            x.[0] // Y
            x.[1] // A
            x.[2] // a
            2.0 * x.[3] // AA
            2.0 * x.[4] // Aa
            2.0 * x.[5] // aA
            2.0 * x.[6] // aa
        |]
         |> Array.sum



    let getTotals (x : array<double>) = 
        [|
            // A
            (
                [|
                    x.[1] // A
                    2.0 * x.[3] // AA
                    x.[4] // Aa
                    x.[5] // aA
                |]
                |> Array.sum
                ,
                [|
                    x.[2] // a
                    x.[4] // Aa
                    x.[5] // aA
                    2.0 * x.[6] // aa
                |]
                |> Array.sum
            )
        |]


    let update (x : array<double>) : array<double> = 
        let xSum = (x |> Array.sum) - x.[0]

        let xSumN = 
            [|
                1.0 * x.[1] // A
                1.0 * x.[2] // a
                2.0 * x.[3] // AA
                2.0 * x.[4] // Aa
                2.0 * x.[5] // aA
                2.0 * x.[6] // aa
            |]
            |> Array.sum

        let xSumSquaredN = 
            [|
                1.0 * x.[1] * x.[1] // A
                1.0 * x.[2] * x.[2] // a
                2.0 * x.[3] * x.[3] // AA
                2.0 * x.[4] * x.[4] // Aa
                2.0 * x.[5] * x.[5] // aA
                2.0 * x.[6] * x.[6] // aa
            |]
            |> Array.sum

        [|

            // 0 - Y
            [|

                0.0356902075166968 * x.[2] * x.[3] // a + AA | CatalyticSynthesisName: Y + AA <-> a + AA
                -0.356902075166968 * x.[0] * x.[3] // Y + AA | CatalyticSynthesisName: Y + AA <-> a + AA
                0.0356902075166968 * x.[1] * x.[6] // A + aa | CatalyticSynthesisName: Y + aa <-> A + aa
                -0.356902075166968 * x.[0] * x.[6] // Y + aa | CatalyticSynthesisName: Y + aa <-> A + aa
                0.0886291247762224 * x.[2] * x.[4] // a + Aa | CatalyticSynthesisName: Y + Aa <-> a + Aa
                -0.886291247762224 * x.[0] * x.[4] // Y + Aa | CatalyticSynthesisName: Y + Aa <-> a + Aa
                0.0886291247762224 * x.[1] * x.[5] // A + aA | CatalyticSynthesisName: Y + aA <-> A + aA
                -0.886291247762224 * x.[0] * x.[5] // Y + aA | CatalyticSynthesisName: Y + aA <-> A + aA
                0.0874394187622607 * x.[2] * x.[5] // a + aA | CatalyticSynthesisName: Y + aA <-> a + aA
                -0.874394187622607 * x.[0] * x.[5] // Y + aA | CatalyticSynthesisName: Y + aA <-> a + aA
                0.0874394187622607 * x.[1] * x.[4] // A + Aa | CatalyticSynthesisName: Y + Aa <-> A + Aa
                -0.874394187622607 * x.[0] * x.[4] // Y + Aa | CatalyticSynthesisName: Y + Aa <-> A + Aa
                0.0358280171806389 * x.[2] * x.[6] // a + aa | CatalyticSynthesisName: Y + aa <-> a + aa
                -0.358280171806389 * x.[0] * x.[6] // Y + aa | CatalyticSynthesisName: Y + aa <-> a + aa
                0.0358280171806389 * x.[1] * x.[3] // A + AA | CatalyticSynthesisName: Y + AA <-> A + AA
                -0.358280171806389 * x.[0] * x.[3] // Y + AA | CatalyticSynthesisName: Y + AA <-> A + AA
                0.001 * x.[2] // a | SynthesisName: Y <-> a
                -0.01 * x.[0] // Y | SynthesisName: Y <-> a
                0.001 * x.[1] // A | SynthesisName: Y <-> A
                -0.01 * x.[0] // Y | SynthesisName: Y <-> A
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 1 - A
            [|

                -0.0356902075166968 * x.[1] * x.[6] // A + aa | CatalyticSynthesisName: Y + aa <-> A + aa
                0.356902075166968 * x.[0] * x.[6] // Y + aa | CatalyticSynthesisName: Y + aa <-> A + aa
                -0.0886291247762224 * x.[1] * x.[5] // A + aA | CatalyticSynthesisName: Y + aA <-> A + aA
                0.886291247762224 * x.[0] * x.[5] // Y + aA | CatalyticSynthesisName: Y + aA <-> A + aA
                -0.0874394187622607 * x.[1] * x.[4] // A + Aa | CatalyticSynthesisName: Y + Aa <-> A + Aa
                0.874394187622607 * x.[0] * x.[4] // Y + Aa | CatalyticSynthesisName: Y + Aa <-> A + Aa
                -0.0358280171806389 * x.[1] * x.[3] // A + AA | CatalyticSynthesisName: Y + AA <-> A + AA
                0.358280171806389 * x.[0] * x.[3] // Y + AA | CatalyticSynthesisName: Y + AA <-> A + AA
                -0.001 * x.[1] // A | SynthesisName: Y <-> A
                0.01 * x.[0] // Y | SynthesisName: Y <-> A
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 2 - a
            [|

                -0.0356902075166968 * x.[2] * x.[3] // a + AA | CatalyticSynthesisName: Y + AA <-> a + AA
                0.356902075166968 * x.[0] * x.[3] // Y + AA | CatalyticSynthesisName: Y + AA <-> a + AA
                -0.0886291247762224 * x.[2] * x.[4] // a + Aa | CatalyticSynthesisName: Y + Aa <-> a + Aa
                0.886291247762224 * x.[0] * x.[4] // Y + Aa | CatalyticSynthesisName: Y + Aa <-> a + Aa
                -0.0874394187622607 * x.[2] * x.[5] // a + aA | CatalyticSynthesisName: Y + aA <-> a + aA
                0.874394187622607 * x.[0] * x.[5] // Y + aA | CatalyticSynthesisName: Y + aA <-> a + aA
                -0.0358280171806389 * x.[2] * x.[6] // a + aa | CatalyticSynthesisName: Y + aa <-> a + aa
                0.358280171806389 * x.[0] * x.[6] // Y + aa | CatalyticSynthesisName: Y + aa <-> a + aa
                -0.001 * x.[2] // a | SynthesisName: Y <-> a
                0.01 * x.[0] // Y | SynthesisName: Y <-> a
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 3 - AA
            [|

            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 4 - Aa
            [|

            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 5 - aA
            [|

            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 6 - aa
            [|

            |]
            |> Array.fold (fun acc r -> acc + r) 0.0

        |]


    let modelDataParams = 
        {
            numberOfSubstances = 7
            numberOfAminoAcids = OneAminoAcid
            maxPeptideLength = TwoMax
            getTotals = getTotals
            getTotalSubst = getTotalSubst
        }

