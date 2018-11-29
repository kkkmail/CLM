namespace Model

open System
open Clm.Substances
open Clm.Model
open Clm.ReactionTypes

module AllModels = 

    let allModelData = 
        [
            {
                seedValue = 0
                modelName = String.Empty
                numberOfSubstances = 0
                numberOfAminoAcids = NumberOfAminoAcids.OneAminoAcid
                maxPeptideLength = MaxPeptideLength.TwoMax
                allRates = []
            }
        ]

