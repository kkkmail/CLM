namespace Model


open Clm.Substances
open Clm.Model

open Clm.ReactionTypes

module ModelData = 
    let seedValue = 1106184041
    let numberOfAminoAcids = NumberOfAminoAcids.TwoAminoAcids
    let maxPeptideLength = MaxPeptideLength.TwoMax
    let numberOfSubstances = 21

    let aminoAcids = AminoAcid.getAminoAcids numberOfAminoAcids
    let chiralAminoAcids = ChiralAminoAcid.getAminoAcids numberOfAminoAcids
    let peptides = Peptide.getPeptides maxPeptideLength numberOfAminoAcids

    let allSubst = 
        [ Substance.food ]
        @
        (chiralAminoAcids |> List.map (fun a -> Chiral a))
        @
        (peptides |> List.map (fun p -> PeptideChain p))

    let allInd = allSubst |> List.mapi (fun i s -> (s, i)) |> Map.ofList


    let getTotalSubst (x : array<double>) = 
        [|
            x.[0] // Y
            x.[1] // A
            x.[2] // B
            x.[3] // a
            x.[4] // b
            2.0 * x.[5] // AA
            2.0 * x.[6] // AB
            2.0 * x.[7] // Aa
            2.0 * x.[8] // Ab
            2.0 * x.[9] // BA
            2.0 * x.[10] // BB
            2.0 * x.[11] // Ba
            2.0 * x.[12] // Bb
            2.0 * x.[13] // aA
            2.0 * x.[14] // aB
            2.0 * x.[15] // aa
            2.0 * x.[16] // ab
            2.0 * x.[17] // bA
            2.0 * x.[18] // bB
            2.0 * x.[19] // ba
            2.0 * x.[20] // bb
        |]
        |> Array.sum


    let getTotals (x : array<double>) = 
        [|
            // A
            (
                [|
                    x.[1] // A
                    2.0 * x.[5] // AA
                    x.[6] // AB
                    x.[7] // Aa
                    x.[8] // Ab
                    x.[9] // BA
                    x.[13] // aA
                    x.[17] // bA
                |]
                |> Array.sum
                ,
                [|
                    x.[3] // a
                    x.[7] // Aa
                    x.[11] // Ba
                    x.[13] // aA
                    x.[14] // aB
                    2.0 * x.[15] // aa
                    x.[16] // ab
                    x.[19] // ba
                |]
                |> Array.sum
            )

            // B
            (
                [|
                    x.[2] // B
                    x.[6] // AB
                    x.[9] // BA
                    2.0 * x.[10] // BB
                    x.[11] // Ba
                    x.[12] // Bb
                    x.[14] // aB
                    x.[18] // bB
                |]
                |> Array.sum
                ,
                [|
                    x.[4] // b
                    x.[8] // Ab
                    x.[12] // Bb
                    x.[16] // ab
                    x.[17] // bA
                    x.[18] // bB
                    x.[19] // ba
                    2.0 * x.[20] // bb
                |]
                |> Array.sum
            )
        |]


    let update (x : array<double>) : array<double> = 

        printfn "update::Starting..."

        let xSum = (x |> Array.sum) - x.[0]


        let xSumN = 
            [|
                1.0 * x.[1] // A
                1.0 * x.[2] // B
                1.0 * x.[3] // a
                1.0 * x.[4] // b
                2.0 * x.[5] // AA
                2.0 * x.[6] // AB
                2.0 * x.[7] // Aa
                2.0 * x.[8] // Ab
                2.0 * x.[9] // BA
                2.0 * x.[10] // BB
                2.0 * x.[11] // Ba
                2.0 * x.[12] // Bb
                2.0 * x.[13] // aA
                2.0 * x.[14] // aB
                2.0 * x.[15] // aa
                2.0 * x.[16] // ab
                2.0 * x.[17] // bA
                2.0 * x.[18] // bB
                2.0 * x.[19] // ba
                2.0 * x.[20] // bb
            |]
            |> Array.sum


        let xSumSquaredN = 
            [|
                1.0 * x.[1] * x.[1] // A
                1.0 * x.[2] * x.[2] // B
                1.0 * x.[3] * x.[3] // a
                1.0 * x.[4] * x.[4] // b
                2.0 * x.[5] * x.[5] // AA
                2.0 * x.[6] * x.[6] // AB
                2.0 * x.[7] * x.[7] // Aa
                2.0 * x.[8] * x.[8] // Ab
                2.0 * x.[9] * x.[9] // BA
                2.0 * x.[10] * x.[10] // BB
                2.0 * x.[11] * x.[11] // Ba
                2.0 * x.[12] * x.[12] // Bb
                2.0 * x.[13] * x.[13] // aA
                2.0 * x.[14] * x.[14] // aB
                2.0 * x.[15] * x.[15] // aa
                2.0 * x.[16] * x.[16] // ab
                2.0 * x.[17] * x.[17] // bA
                2.0 * x.[18] * x.[18] // bB
                2.0 * x.[19] * x.[19] // ba
                2.0 * x.[20] * x.[20] // bb
            |]
            |> Array.sum

        [|

            // 0 - Y
            [|

                0.0001 * x.[4] // b | synthesis: Y <-> b
                -0.001 * x.[0] // Y | synthesis: Y <-> b
                0.0001 * x.[2] // B | synthesis: Y <-> B
                -0.001 * x.[0] // Y | synthesis: Y <-> B
                0.0001 * x.[3] // a | synthesis: Y <-> a
                -0.001 * x.[0] // Y | synthesis: Y <-> a
                0.0001 * x.[1] // A | synthesis: Y <-> A
                -0.001 * x.[0] // Y | synthesis: Y <-> A
            |]
            |> Array.sum


            // 1 - A
            [|

                0.0001 * x.[17] // bA | ligation: b + A <-> bA
                -0.001 * x.[4] * x.[1] // b + A | ligation: b + A <-> bA
                0.0001 * x.[8] // Ab | ligation: A + b <-> Ab
                -0.001 * x.[1] * x.[4] // A + b | ligation: A + b <-> Ab
                0.0001 * x.[13] // aA | ligation: a + A <-> aA
                -0.001 * x.[3] * x.[1] // a + A | ligation: a + A <-> aA
                0.0001 * x.[7] // Aa | ligation: A + a <-> Aa
                -0.001 * x.[1] * x.[3] // A + a | ligation: A + a <-> Aa
                0.0001 * x.[6] // AB | ligation: A + B <-> AB
                -0.001 * x.[1] * x.[2] // A + B | ligation: A + B <-> AB
                0.0001 * x.[5] // AA | ligation: A + A <-> AA
                0.0001 * x.[5] // AA | ligation: A + A <-> AA
                -0.001 * x.[1] * x.[1] // A + A | ligation: A + A <-> AA
                -0.001 * x.[1] * x.[1] // A + A | ligation: A + A <-> AA
                -0.0001 * x.[1] // A | synthesis: Y <-> A
                0.001 * x.[0] // Y | synthesis: Y <-> A
            |]
            |> Array.sum


            // 2 - B
            [|

                0.0001 * x.[18] // bB | ligation: b + B <-> bB
                -0.001 * x.[4] * x.[2] // b + B | ligation: b + B <-> bB
                0.0001 * x.[12] // Bb | ligation: B + b <-> Bb
                -0.001 * x.[2] * x.[4] // B + b | ligation: B + b <-> Bb
                0.0001 * x.[11] // Ba | ligation: B + a <-> Ba
                -0.001 * x.[2] * x.[3] // B + a | ligation: B + a <-> Ba
                0.0001 * x.[10] // BB | ligation: B + B <-> BB
                0.0001 * x.[10] // BB | ligation: B + B <-> BB
                -0.001 * x.[2] * x.[2] // B + B | ligation: B + B <-> BB
                -0.001 * x.[2] * x.[2] // B + B | ligation: B + B <-> BB
                0.0001 * x.[14] // aB | ligation: a + B <-> aB
                -0.001 * x.[3] * x.[2] // a + B | ligation: a + B <-> aB
                0.0001 * x.[6] // AB | ligation: A + B <-> AB
                -0.001 * x.[1] * x.[2] // A + B | ligation: A + B <-> AB
                -0.0001 * x.[2] // B | synthesis: Y <-> B
                0.001 * x.[0] // Y | synthesis: Y <-> B
            |]
            |> Array.sum


            // 3 - a
            [|

                0.0001 * x.[11] // Ba | ligation: B + a <-> Ba
                -0.001 * x.[2] * x.[3] // B + a | ligation: B + a <-> Ba
                0.0001 * x.[14] // aB | ligation: a + B <-> aB
                -0.001 * x.[3] * x.[2] // a + B | ligation: a + B <-> aB
                0.0001 * x.[13] // aA | ligation: a + A <-> aA
                -0.001 * x.[3] * x.[1] // a + A | ligation: a + A <-> aA
                0.0001 * x.[7] // Aa | ligation: A + a <-> Aa
                -0.001 * x.[1] * x.[3] // A + a | ligation: A + a <-> Aa
                0.0001 * x.[16] // ab | ligation: a + b <-> ab
                -0.001 * x.[3] * x.[4] // a + b | ligation: a + b <-> ab
                0.0001 * x.[15] // aa | ligation: a + a <-> aa
                0.0001 * x.[15] // aa | ligation: a + a <-> aa
                -0.001 * x.[3] * x.[3] // a + a | ligation: a + a <-> aa
                -0.001 * x.[3] * x.[3] // a + a | ligation: a + a <-> aa
                -0.0001 * x.[3] // a | synthesis: Y <-> a
                0.001 * x.[0] // Y | synthesis: Y <-> a
            |]
            |> Array.sum


            // 4 - b
            [|

                0.0001 * x.[18] // bB | ligation: b + B <-> bB
                -0.001 * x.[4] * x.[2] // b + B | ligation: b + B <-> bB
                0.0001 * x.[12] // Bb | ligation: B + b <-> Bb
                -0.001 * x.[2] * x.[4] // B + b | ligation: B + b <-> Bb
                0.0001 * x.[17] // bA | ligation: b + A <-> bA
                -0.001 * x.[4] * x.[1] // b + A | ligation: b + A <-> bA
                0.0001 * x.[20] // bb | ligation: b + b <-> bb
                0.0001 * x.[20] // bb | ligation: b + b <-> bb
                -0.001 * x.[4] * x.[4] // b + b | ligation: b + b <-> bb
                -0.001 * x.[4] * x.[4] // b + b | ligation: b + b <-> bb
                0.0001 * x.[8] // Ab | ligation: A + b <-> Ab
                -0.001 * x.[1] * x.[4] // A + b | ligation: A + b <-> Ab
                0.0001 * x.[16] // ab | ligation: a + b <-> ab
                -0.001 * x.[3] * x.[4] // a + b | ligation: a + b <-> ab
                -0.0001 * x.[4] // b | synthesis: Y <-> b
                0.001 * x.[0] // Y | synthesis: Y <-> b
            |]
            |> Array.sum


            // 5 - AA
            [|

                -0.0001 * x.[5] // AA | ligation: A + A <-> AA
                0.001 * x.[1] * x.[1] // A + A | ligation: A + A <-> AA
            |]
            |> Array.sum


            // 6 - AB
            [|

                -0.0001 * x.[6] // AB | ligation: A + B <-> AB
                0.001 * x.[1] * x.[2] // A + B | ligation: A + B <-> AB
            |]
            |> Array.sum


            // 7 - Aa
            [|

                -0.0001 * x.[7] // Aa | ligation: A + a <-> Aa
                0.001 * x.[1] * x.[3] // A + a | ligation: A + a <-> Aa
            |]
            |> Array.sum


            // 8 - Ab
            [|

                -0.0001 * x.[8] // Ab | ligation: A + b <-> Ab
                0.001 * x.[1] * x.[4] // A + b | ligation: A + b <-> Ab
            |]
            |> Array.sum


            // 9 - BA
            [|

            |]
            |> Array.sum


            // 10 - BB
            [|

                -0.0001 * x.[10] // BB | ligation: B + B <-> BB
                0.001 * x.[2] * x.[2] // B + B | ligation: B + B <-> BB
            |]
            |> Array.sum


            // 11 - Ba
            [|

                -0.0001 * x.[11] // Ba | ligation: B + a <-> Ba
                0.001 * x.[2] * x.[3] // B + a | ligation: B + a <-> Ba
            |]
            |> Array.sum


            // 12 - Bb
            [|

                -0.0001 * x.[12] // Bb | ligation: B + b <-> Bb
                0.001 * x.[2] * x.[4] // B + b | ligation: B + b <-> Bb
            |]
            |> Array.sum


            // 13 - aA
            [|

                -0.0001 * x.[13] // aA | ligation: a + A <-> aA
                0.001 * x.[3] * x.[1] // a + A | ligation: a + A <-> aA
            |]
            |> Array.sum


            // 14 - aB
            [|

                -0.0001 * x.[14] // aB | ligation: a + B <-> aB
                0.001 * x.[3] * x.[2] // a + B | ligation: a + B <-> aB
            |]
            |> Array.sum


            // 15 - aa
            [|

                -0.0001 * x.[15] // aa | ligation: a + a <-> aa
                0.001 * x.[3] * x.[3] // a + a | ligation: a + a <-> aa
            |]
            |> Array.sum


            // 16 - ab
            [|

                -0.0001 * x.[16] // ab | ligation: a + b <-> ab
                0.001 * x.[3] * x.[4] // a + b | ligation: a + b <-> ab
            |]
            |> Array.sum


            // 17 - bA
            [|

                -0.0001 * x.[17] // bA | ligation: b + A <-> bA
                0.001 * x.[4] * x.[1] // b + A | ligation: b + A <-> bA
            |]
            |> Array.sum


            // 18 - bB
            [|

                -0.0001 * x.[18] // bB | ligation: b + B <-> bB
                0.001 * x.[4] * x.[2] // b + B | ligation: b + B <-> bB
            |]
            |> Array.sum


            // 19 - ba
            [|

            |]
            |> Array.sum


            // 20 - bb
            [|

                -0.0001 * x.[20] // bb | ligation: b + b <-> bb
                0.001 * x.[4] * x.[4] // b + b | ligation: b + b <-> bb
            |]
            |> Array.sum

        |]


    let modelDataParams = 
        {
            seedValue = seedValue
            numberOfSubstances = 21
            numberOfAminoAcids = TwoAminoAcids
            maxPeptideLength = TwoMax
            getTotals = getTotals
            getTotalSubst = getTotalSubst
            allSubst = allSubst
            allInd = allInd

            allRawReactions = 
                [
                    (SynthesisName, 4)
                    (CatalyticSynthesisName, 64)
                    (LigationName, 7)
                    (CatalyticLigationName, 112)
                    (SedimentationDirectName, 139)
                    (SedimentationAllName, 4)
                ]

            allReactions = 
                [
                    (SynthesisName, 4)
                    (LigationName, 14)
                ]
        }

