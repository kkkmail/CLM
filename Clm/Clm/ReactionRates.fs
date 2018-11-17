namespace Clm

open System
open System.Collections.Generic
open FSharp.Collections
open System.Numerics
open MathNet.Numerics.LinearAlgebra
open Clm.Substances

module ReactionRates = 

    /// Abstraction over all known statistical distrubiton parameters.
    type DistributionParams =
        {
            seed : int option
            seedBool : int option
            trueProbability : double
        }


    /// Abstraction over statistical distributons.
    type Distribution (distributionParams : DistributionParams) =
        let boolRnd = 
            match distributionParams.seedBool with 
            | Some s -> new Random(s)
            | None -> new Random()

        /// Gives next double from the distrubution.
        /// It is NOT [0, 1) based but, rather, is based on the parameters of the distribution.
        /// This is crucial for long-tailed distributions, which often do not have standard deviation and may not even have mean defined.
        member dstr.nextDouble() : double = 0.0

        /// Gives next bool from the distribution.
        member dstr.nextBool() = boolRnd.NextDouble() < distributionParams.trueProbability


    type RelatedReactions = 
        {
            primary : (ReactionRate option * ReactionRate option)
            similar : list<ReactionInfo * (ReactionRate option * ReactionRate option)>
        }


    type CatalystModelRandomParam = 
        {
            distribution : Distribution
            forwardScale : double
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


    /// Models, which describe catalytic reactions.
    type CatalystModel = 
        | CatalystModelRandom of CatalystModelRandomParam
        | CatalystModelSimilarity of CatalystModelSimilarityParam // If substance is a catalyst for some amino acid X, then there are some [separate] chances that it could be a catalyst for other amino acids.

        member this.getRates (r : ReactionInfo) =
            match this with 
            | CatalystModelRandom p -> 
                match p.distribution.nextBool() with 
                | true ->
                    let f = p.forwardScale * p.distribution.nextDouble() |> ReactionRate |> Some
                    let b = 
                        match p.backwardScale with 
                        | Some s -> s * p.distribution.nextDouble() |> ReactionRate |> Some
                        | None -> None
                    {
                        primary = (f, b)
                        similar = []
                    }
                | false -> noRates
            | CatalystModelSimilarity p -> failwith ""

    //| ReactionRateProvider of (ReactionInfo -> (ReactionRate option * ReactionRate option))


    //type ReactionRateInfo = 
    //    {
    //        forward : ReactionRate option
    //        backward : ReactionRate option
    //    }


    type ReactionRateGenerator() =
        let rateDictionary = new Dictionary<ReactionInfo, (ReactionRate option * ReactionRate option)>()

        let calculateRates (r : ReactionInfo) : RelatedReactions = 
            failwith ""

        let getRates r = 
            match rateDictionary.TryGetValue r with 
            | true, rates -> rates
            | false, _ -> 
                let x = calculateRates r
                x.similar |> List.map (fun (i, e) -> if rateDictionary.ContainsKey i |> not then rateDictionary.Add(i, e)) |> ignore
                x.primary


        member this.getReactionRates r = getRates r



    type ReactionRates () = 
        let x = 1

        member this.a = 0

