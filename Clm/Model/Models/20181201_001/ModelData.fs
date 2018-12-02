namespace Model

open Clm.Substances
open Clm.Model
open Clm.ReactionTypes
open Clm.ReactionRates

module ModelData = 
    let seedValue = 629334335
    let numberOfAminoAcids = NumberOfAminoAcids.OneAminoAcid
    let maxPeptideLength = MaxPeptideLength.TwoMax
    let numberOfSubstances = 7

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



    // 0 - Y
    let d0 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|

            0.001 * x.[2] // a | synthesis: Y <-> a
            -0.0001 * x.[0] // Y | synthesis: Y <-> a
            0.001 * x.[1] // A | synthesis: Y <-> A
            -0.0001 * x.[0] // Y | synthesis: Y <-> A
        |]
        |> Array.sum


    // 1 - A
    let d1 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|

            -0.001 * x.[1] // A | synthesis: Y <-> A
            0.0001 * x.[0] // Y | synthesis: Y <-> A
        |]
        |> Array.sum


    // 2 - a
    let d2 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|

            -0.001 * x.[2] // a | synthesis: Y <-> a
            0.0001 * x.[0] // Y | synthesis: Y <-> a
        |]
        |> Array.sum


    // 3 - AA
    let d3 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|

        |]
        |> Array.sum


    // 4 - Aa
    let d4 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|

        |]
        |> Array.sum


    // 5 - aA
    let d5 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|

        |]
        |> Array.sum


    // 6 - aa
    let d6 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|

        |]
        |> Array.sum

    let update (x : array<double>) : array<double> = 

        // printfn "update::Starting..."

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
            d0 x xSum xSumN xSumSquaredN
            d1 x xSum xSumN xSumSquaredN
            d2 x xSum xSumN xSumSquaredN
            d3 x xSum xSumN xSumSquaredN
            d4 x xSum xSumN xSumSquaredN
            d5 x xSum xSumN xSumSquaredN
            d6 x xSum xSumN xSumSquaredN
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
                            modelName = "20181201_001"
                            numberOfSubstances = 7
                            numberOfAminoAcids = OneAminoAcid
                            maxPeptideLength = TwoMax
                        }

                    allParams = 
                        [
                            {
                                synthesisDistribution = DeltaDistribution({ threshold = None }) |> Delta
                                forwardScale = Some 0.0001
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
                    (SynthesisName, 2)
                    (CatalyticSynthesisName, 8)
                    (LigationName, 2)
                    (CatalyticLigationName, 8)
                    (SedimentationDirectName, 13)
                    (SedimentationAllName, 2)
                ]

            allReactions = 
                [
                    (SynthesisName, 2)
                ]
        }

