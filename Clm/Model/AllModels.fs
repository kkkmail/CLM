namespace Model

open Clm.Substances
open Clm.Model
open Clm.ReactionRates

module AllModels = 

    /// !!! Automatically added at the end. Do not modify without extreme need !!!
    let allModelData : list<ModelDataParams> = 
        []

        @
        [
            {
                modelInfo = 
                    {
                        versionNumber = "1.0.0.0"
                        seedValue = 1049272498
                        modelName = "20181129_06"
                        numberOfSubstances = 85
                        numberOfAminoAcids = NumberOfAminoAcids.TwoAminoAcids
                        maxPeptideLength = MaxPeptideLength.ThreeMax
                    }

                allParams = 
                    [
                        {
                            synthesisDistribution = DeltaDistribution(351993203, { threshold = None }) |> Delta
                            forwardScale = Some 0.001
                            backwardScale = Some 0.0001
                        }
                        |> SynthesisRateParam

                        {
                            catSynthDistribution = UniformDistribution(1408221336, { threshold = Some 0.0005 }) |> Uniform
                            multiplier = 1000.0
                            maxEe = 0.05
                        }
                        |> CatalyticSynthesisRateParam

                        {
                            ligationDistribution = DeltaDistribution(1306605989, { threshold = None }) |> Delta
                            forwardScale = Some 0.001
                            backwardScale = Some 0.0001
                        }
                        |> LigationRateParam

                        {
                            catLigationDistribution = UniformDistribution(1103789466, { threshold = Some 0.0001 }) |> Uniform
                            multiplier = 1000.0
                            maxEe = 0.05
                        }
                        |> CatalyticLigationRateParam

                        {
                            sedimentationDirectDistribution = TriangularDistribution(388918053, { threshold = Some 0.0001 }) |> Triangular
                            forwardScale = Some 100.0
                        }
                        |> SedimentationDirectRateParam

                        {
                            sedimentationAllDistribution = UniformDistribution(372444012, { threshold = None }) |> Uniform
                            forwardScale = Some 0.1
                        }
                        |> SedimentationAllRateParam

                    ]
            }
        ]
