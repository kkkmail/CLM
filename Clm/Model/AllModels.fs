namespace Model

open Clm.Substances
open Clm.Model
open Clm.ReactionRates

module AllModels = 

    /// !!! This file grows automatically at the end. Do not modify without extreme need !!!
    let allModelData : list<ModelDataParams> = 
        []

        @
        [
            {
                modelInfo = 
                    {
                        versionNumber = "1.0.0.0"
                        seedValue = 1843395616
                        modelName = "20181129_004"
                        numberOfSubstances = 4369
                        numberOfAminoAcids = NumberOfAminoAcids.EightAminoAcids
                        maxPeptideLength = MaxPeptideLength.ThreeMax
                    }

                allParams = 
                    [
                        {
                            synthesisDistribution = DeltaDistribution({ threshold = None }) |> Delta
                            forwardScale = Some 0.001
                            backwardScale = Some 0.0001
                        }
                        |> SynthesisRateParam

                        {
                            catSynthDistribution = TriangularDistribution(1266829552, { threshold = Some 0.0005 }) |> Triangular
                            multiplier = 1000.0
                            maxEe = 0.05
                        }
                        |> CatalyticSynthesisRateParam

                        {
                            ligationDistribution = DeltaDistribution({ threshold = None }) |> Delta
                            forwardScale = Some 0.001
                            backwardScale = Some 0.0001
                        }
                        |> LigationRateParam

                        {
                            catLigationDistribution = TriangularDistribution(291246254, { threshold = Some 0.0001 }) |> Triangular
                            multiplier = 1000.0
                            maxEe = 0.05
                        }
                        |> CatalyticLigationRateParam

                        {
                            sedimentationDirectDistribution = TriangularDistribution(109624530, { threshold = Some 0.0001 }) |> Triangular
                            forwardScale = Some 100.0
                        }
                        |> SedimentationDirectRateParam

                    ]
            }
        ]

        @
        [
            {
                modelInfo = 
                    {
                        versionNumber = "1.0.0.0"
                        seedValue = 1082891382
                        modelName = "20181129_005"
                        numberOfSubstances = 8421
                        numberOfAminoAcids = NumberOfAminoAcids.TenAminoAcids
                        maxPeptideLength = MaxPeptideLength.ThreeMax
                    }

                allParams = 
                    [
                        {
                            synthesisDistribution = DeltaDistribution({ threshold = None }) |> Delta
                            forwardScale = Some 0.001
                            backwardScale = Some 0.0001
                        }
                        |> SynthesisRateParam

                        {
                            catSynthDistribution = TriangularDistribution(430135867, { threshold = Some 0.0005 }) |> Triangular
                            multiplier = 1000.0
                            maxEe = 0.05
                        }
                        |> CatalyticSynthesisRateParam

                        {
                            ligationDistribution = DeltaDistribution({ threshold = None }) |> Delta
                            forwardScale = Some 0.001
                            backwardScale = Some 0.0001
                        }
                        |> LigationRateParam

                        {
                            catLigationDistribution = TriangularDistribution(266190779, { threshold = Some 0.0001 }) |> Triangular
                            multiplier = 1000.0
                            maxEe = 0.05
                        }
                        |> CatalyticLigationRateParam

                        {
                            sedimentationDirectDistribution = TriangularDistribution(1670086807, { threshold = Some 0.0001 }) |> Triangular
                            forwardScale = Some 100.0
                        }
                        |> SedimentationDirectRateParam

                    ]
            }
        ]

        @
        [
            {
                modelInfo = 
                    {
                        versionNumber = "1.0.0.0"
                        seedValue = 1907012401
                        modelName = "20181130_001"
                        numberOfSubstances = 6175
                        numberOfAminoAcids = NumberOfAminoAcids.NineAminoAcids
                        maxPeptideLength = MaxPeptideLength.ThreeMax
                    }

                allParams = 
                    [
                        {
                            synthesisDistribution = DeltaDistribution({ threshold = None }) |> Delta
                            forwardScale = Some 0.001
                            backwardScale = Some 0.0001
                        }
                        |> SynthesisRateParam

                        {
                            catSynthDistribution = TriangularDistribution(1033457174, { threshold = Some 0.0005 }) |> Triangular
                            multiplier = 1000.0
                            maxEe = 0.05
                        }
                        |> CatalyticSynthesisRateParam

                        {
                            ligationDistribution = DeltaDistribution({ threshold = None }) |> Delta
                            forwardScale = Some 0.001
                            backwardScale = Some 0.0001
                        }
                        |> LigationRateParam

                        {
                            catLigationDistribution = TriangularDistribution(527950606, { threshold = Some 0.0001 }) |> Triangular
                            multiplier = 1000.0
                            maxEe = 0.05
                        }
                        |> CatalyticLigationRateParam

                        {
                            sedimentationDirectDistribution = TriangularDistribution(1629601449, { threshold = Some 0.0001 }) |> Triangular
                            forwardScale = Some 100.0
                        }
                        |> SedimentationDirectRateParam

                    ]
            }
        ]

        @
        [
            {
                modelInfo = 
                    {
                        versionNumber = "1.0.0.0"
                        seedValue = 967770849
                        modelName = "20181130_002"
                        numberOfSubstances = 6175
                        numberOfAminoAcids = NumberOfAminoAcids.NineAminoAcids
                        maxPeptideLength = MaxPeptideLength.ThreeMax
                    }

                allParams = 
                    [
                        {
                            synthesisDistribution = DeltaDistribution({ threshold = None }) |> Delta
                            forwardScale = Some 0.001
                            backwardScale = Some 0.0001
                        }
                        |> SynthesisRateParam

                        {
                            catSynthDistribution = TriangularDistribution(1241572347, { threshold = Some 0.0005 }) |> Triangular
                            multiplier = 1000.0
                            maxEe = 0.05
                        }
                        |> CatalyticSynthesisRateParam

                        {
                            ligationDistribution = DeltaDistribution({ threshold = None }) |> Delta
                            forwardScale = Some 0.001
                            backwardScale = Some 0.0001
                        }
                        |> LigationRateParam

                        {
                            catLigationDistribution = TriangularDistribution(1019762416, { threshold = Some 0.0001 }) |> Triangular
                            multiplier = 1000.0
                            maxEe = 0.05
                        }
                        |> CatalyticLigationRateParam

                        {
                            sedimentationDirectDistribution = TriangularDistribution(1617734990, { threshold = Some 0.0001 }) |> Triangular
                            forwardScale = Some 100.0
                        }
                        |> SedimentationDirectRateParam

                    ]
            }
        ]

        @
        [
            {
                modelInfo = 
                    {
                        versionNumber = "1.0.0.0"
                        seedValue = 1048669266
                        modelName = "20181130_003"
                        numberOfSubstances = 6175
                        numberOfAminoAcids = NumberOfAminoAcids.NineAminoAcids
                        maxPeptideLength = MaxPeptideLength.ThreeMax
                    }

                allParams = 
                    [
                        {
                            synthesisDistribution = DeltaDistribution({ threshold = None }) |> Delta
                            forwardScale = Some 0.001
                            backwardScale = Some 0.0001
                        }
                        |> SynthesisRateParam

                        {
                            catSynthDistribution = TriangularDistribution(1066828363, { threshold = Some 0.0005 }) |> Triangular
                            multiplier = 1000.0
                            maxEe = 0.05
                        }
                        |> CatalyticSynthesisRateParam

                        {
                            ligationDistribution = DeltaDistribution({ threshold = None }) |> Delta
                            forwardScale = Some 0.001
                            backwardScale = Some 0.0001
                        }
                        |> LigationRateParam

                        {
                            catLigationDistribution = TriangularDistribution(335497233, { threshold = Some 0.0001 }) |> Triangular
                            multiplier = 1000.0
                            maxEe = 0.05
                        }
                        |> CatalyticLigationRateParam

                        {
                            sedimentationDirectDistribution = TriangularDistribution(1470949099, { threshold = Some 0.0001 }) |> Triangular
                            forwardScale = Some 100.0
                        }
                        |> SedimentationDirectRateParam

                    ]
            }
        ]
