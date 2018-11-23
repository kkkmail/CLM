﻿namespace Clm

open System
open System.Collections.Generic
open FSharp.Collections
open System.Numerics
open MathNet.Numerics.LinearAlgebra
open Clm.Substances
open Clm.ReactionTypes

module ReactionRates = 

    type ReactionRate = 
        | ReactionRate of double


    type DistributionParams = 
        {
            threshold : double
        }


    [<AbstractClass>]
    type DistributionBase(seed : int, p : DistributionParams, d : Random -> double) = 
        let rnd = new Random(seed)
        let rndBool = new Random(rnd.Next())
        let isDefined() = if rndBool.NextDouble() < p.threshold then true else false
        let nextDoubleImpl() = d(rnd)

        member __.nextDouble = nextDoubleImpl

        member __.nextDoubleOpt() = 
            match isDefined() with 
            | true -> nextDoubleImpl() |> Some
            | false -> None


    type UniformDistribution (seed : int, p : DistributionParams) = 
        inherit DistributionBase (seed, p, fun r -> r.NextDouble())


    type TriangularDistribution (seed : int, p : DistributionParams) = 
        inherit DistributionBase(seed, p, fun r -> 1.0 - sqrt(1.0 - r.NextDouble()))


    type Distribution =
        | Uniform of UniformDistribution
        | Triangular of TriangularDistribution

        member this.nextDouble = 
            match this with
            | Uniform d -> d.nextDouble
            | Triangular d -> d.nextDouble

        member this.nextDoubleOpt = 
            match this with
            | Uniform d -> d.nextDoubleOpt
            | Triangular d -> d.nextDoubleOpt


    type RelatedReactions = 
        {
            primary : (ReactionRate option * ReactionRate option)
            similar : list<ReactionInfo * (ReactionRate option * ReactionRate option)>
        }


    type CatalystModelRandomParam = 
        {
            distribution : Distribution
            forwardScale : double option
            backwardScale : double option
        }


    type CatalystModelSimilarityParam = 
        {
            dummy : int
        }


    let noRates = 
        {
            primary = (None, None)
            similar = []
        }


    let getRates fo bo ro = 
        match ro with 
        | Some r -> 
            let g so = 
                match so with
                | Some s -> s * r |> ReactionRate |> Some
                | None -> None

            {
                primary = (g fo, g bo)
                similar = []
            }
        | None -> noRates

    /// Models, which describe catalytic reactions.
    type CatalystModel = 
        | CatalystModelRandom of CatalystModelRandomParam
        | CatalystModelSimilarity of CatalystModelSimilarityParam // If substance is a catalyst for some amino acid X, then there are some [separate] chances that it could be a catalyst for other amino acids.

        member this.getRates (r : ReactionInfo) =
            match r.reactionName with 
            | CatalyticSynthesisName -> 
                match this with 
                | CatalystModelRandom p -> p.distribution.nextDoubleOpt() |> getRates p.forwardScale p.backwardScale
                | CatalystModelSimilarity _ -> failwith ""
            | _ -> noRates

    type SedimentationDirectRandomParam = 
        {
            sedimentationDirectDistribution : Distribution
            forwardScale : double option
        }

    type SedimentationDirectModel = 
        | SedimentationDirectRandom of SedimentationDirectRandomParam

        member this.getRates (r : ReactionInfo) =
            match r.reactionName with 
            | SedimentationDirectName -> 
                match this with 
                | SedimentationDirectRandom p -> p.sedimentationDirectDistribution.nextDoubleOpt() |> getRates p.forwardScale None
            | _ -> noRates


    type ReactionRateModel = 
        | SedimentationDirectRateModel of SedimentationDirectModel

        member this.getRates (r : ReactionInfo) = 
            match this with 
            | SedimentationDirectRateModel m -> m.getRates r


    type ReactionRateProviderParams = 
        {
            //seedValue : int option
            rateModel: ReactionRateModel
        }


    type ReactionRateProvider (rateModel: ReactionRateModel) =
        let rateDictionary = new Dictionary<ReactionInfo, (ReactionRate option * ReactionRate option)>()
        let calculateRates (r : ReactionInfo) : RelatedReactions = rateModel.getRates r

        let getRatesImpl r = 
            match rateDictionary.TryGetValue r with 
            | true, rates -> rates
            | false, _ -> 
                let x = calculateRates r
                rateDictionary.Add(r, x.primary)
                rateDictionary.Add(r.enantiomer, x.primary)
                x.similar |> List.map (fun (i, e) -> if rateDictionary.ContainsKey i |> not then rateDictionary.Add(i, e)) |> ignore
                x.similar |> List.map (fun (i, e) -> if rateDictionary.ContainsKey i.enantiomer |> not then rateDictionary.Add(i.enantiomer, e)) |> ignore
                x.primary

        member __.getRates r = getRatesImpl r
