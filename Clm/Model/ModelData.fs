namespace Model

open Clm.Substances
open Clm.Model
open Clm.ReactionTypes
open Clm.ReactionRates

module ModelData = 
    let seedValue = 232432000
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



    // 0 - Y
    let d0 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|

            0.001 * x.[4] // b | synthesis: Y <-> b
            -5E-05 * x.[0] // Y | synthesis: Y <-> b
            0.001 * x.[2] // B | synthesis: Y <-> B
            -5E-05 * x.[0] // Y | synthesis: Y <-> B
            0.001 * x.[3] // a | synthesis: Y <-> a
            -5E-05 * x.[0] // Y | synthesis: Y <-> a
            0.001 * x.[1] // A | synthesis: Y <-> A
            -5E-05 * x.[0] // Y | synthesis: Y <-> A
        |]
        |> Array.sum


    // 1 - A
    let d1 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|

            -0.001 * x.[1] // A | synthesis: Y <-> A
            5E-05 * x.[0] // Y | synthesis: Y <-> A
        |]
        |> Array.sum


    // 2 - B
    let d2 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|

            -0.001 * x.[2] // B | synthesis: Y <-> B
            5E-05 * x.[0] // Y | synthesis: Y <-> B
        |]
        |> Array.sum


    // 3 - a
    let d3 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|

            -0.001 * x.[3] // a | synthesis: Y <-> a
            5E-05 * x.[0] // Y | synthesis: Y <-> a
        |]
        |> Array.sum


    // 4 - b
    let d4 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|

            -0.001 * x.[4] // b | synthesis: Y <-> b
            5E-05 * x.[0] // Y | synthesis: Y <-> b
        |]
        |> Array.sum


    // 5 - AA
    let d5 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|

        |]
        |> Array.sum


    // 6 - AB
    let d6 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|

        |]
        |> Array.sum


    // 7 - Aa
    let d7 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|

        |]
        |> Array.sum


    // 8 - Ab
    let d8 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|

        |]
        |> Array.sum


    // 9 - BA
    let d9 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|

        |]
        |> Array.sum


    // 10 - BB
    let d10 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|

        |]
        |> Array.sum


    // 11 - Ba
    let d11 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|

        |]
        |> Array.sum


    // 12 - Bb
    let d12 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|

        |]
        |> Array.sum


    // 13 - aA
    let d13 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|

        |]
        |> Array.sum


    // 14 - aB
    let d14 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|

        |]
        |> Array.sum


    // 15 - aa
    let d15 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|

        |]
        |> Array.sum


    // 16 - ab
    let d16 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|

        |]
        |> Array.sum


    // 17 - bA
    let d17 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|

        |]
        |> Array.sum


    // 18 - bB
    let d18 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|

        |]
        |> Array.sum


    // 19 - ba
    let d19 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|

        |]
        |> Array.sum


    // 20 - bb
    let d20 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|

        |]
        |> Array.sum

    let update (x : array<double>) : array<double> = 

        // printfn "update::Starting..."

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
            d0 x xSum xSumN xSumSquaredN
            d1 x xSum xSumN xSumSquaredN
            d2 x xSum xSumN xSumSquaredN
            d3 x xSum xSumN xSumSquaredN
            d4 x xSum xSumN xSumSquaredN
            d5 x xSum xSumN xSumSquaredN
            d6 x xSum xSumN xSumSquaredN
            d7 x xSum xSumN xSumSquaredN
            d8 x xSum xSumN xSumSquaredN
            d9 x xSum xSumN xSumSquaredN
            d10 x xSum xSumN xSumSquaredN
            d11 x xSum xSumN xSumSquaredN
            d12 x xSum xSumN xSumSquaredN
            d13 x xSum xSumN xSumSquaredN
            d14 x xSum xSumN xSumSquaredN
            d15 x xSum xSumN xSumSquaredN
            d16 x xSum xSumN xSumSquaredN
            d17 x xSum xSumN xSumSquaredN
            d18 x xSum xSumN xSumSquaredN
            d19 x xSum xSumN xSumSquaredN
            d20 x xSum xSumN xSumSquaredN
        |]


    let modelDataParamsWithExtraData = 
        {
            modelDataParams = 
                {
                    modelInfo = 
                        {
                            fileStructureVersionNumber = "1.0.0.0"
                            versionNumber = "1.0.1.0"
                            seedValue = seedValue
                            modelName = "20181201_003"
                            numberOfSubstances = 21
                            numberOfAminoAcids = TwoAminoAcids
                            maxPeptideLength = TwoMax
                        }

                    allParams = 
                        [
                            {
                                synthesisDistribution = DeltaDistribution({ threshold = None }) |> Delta
                                forwardScale = Some 5E-05
                                backwardScale = Some 0.001
                            }
                            |> SynthesisRateParam

                        ]
                }

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
                ]
        }

