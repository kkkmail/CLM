namespace Clm

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


    //type SeedParams = 
    //    {
    //        seed : int
    //        seedBool : int
    //    }

    type DistributionParams = 
        {
            threshold : double
            scale : double
        }


    [<AbstractClass>]
    type DistributionBase(seed : int, p : DistributionParams, d : Random -> double) = 
        let rnd = new Random(seed)
        let rndBool = new Random(rnd.Next())
        let isDefined() = if rndBool.NextDouble() < p.threshold then true else false

        let netxDoubleImpl() = 
            match isDefined() with 
            | true -> p.scale * d(rnd) |> Some
            | false -> None

        member __.netxDouble = netxDoubleImpl


    type UniformDistribution (seed : int, p : DistributionParams) = 
        inherit DistributionBase (seed, p, fun r -> r.NextDouble())


    type TriangularDistribution (seed : int, p : DistributionParams) = 
        inherit DistributionBase(seed, p, fun r -> 1.0 - sqrt(1.0 - r.NextDouble()))


    /// Abstraction over all used statistical distrubiton parameters.
    type DistributionData =
        {
            trueProbability : double
            scale : double
            distribution : int -> DistributionParams -> DistributionBase
        }


    /// Abstraction over statistical distributons.
    type Distribution (p : DistributionData) =
        let distr = p.distribution

        /// Gives next double from the distrubution.
        /// It is NOT [0, 1) based but, rather, is based on the parameters of the distribution.
        /// This is crucial for long-tailed distributions, which often do not have standard deviation and may not even have mean defined.
        member dstr.nextDouble() : double = 0.0


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


    /// Models, which describe catalytic reactions.
    type CatalystModel = 
        | CatalystModelRandom of CatalystModelRandomParam
        | CatalystModelSimilarity of CatalystModelSimilarityParam // If substance is a catalyst for some amino acid X, then there are some [separate] chances that it could be a catalyst for other amino acids.

        member this.getRates (r : ReactionInfo) =
            match this with 
            | CatalystModelRandom p -> 
                match p.distribution.nextBool() with 
                | true ->
                    let g so = 
                        match so with
                        | Some s -> s * p.distribution.nextDouble() |> ReactionRate |> Some
                        | None -> None

                    {
                        primary = (g p.forwardScale, g p.backwardScale)
                        similar = []
                    }
                | false -> noRates
            | CatalystModelSimilarity _ -> failwith ""

    //| ReactionRateProvider of (ReactionInfo -> (ReactionRate option * ReactionRate option))


    //type ReactionRateInfo = 
    //    {
    //        forward : ReactionRate option
    //        backward : ReactionRate option
    //    }




    ///// Returns [optional] forward and backward reaction rates.
    //type ReactionRateProvider = // (p : ReactionRateProviderParams) = 
    //    | ReactionRateProvider of (ReactionInfo -> (ReactionRate option * ReactionRate option))

    //    member this.getRates r = 
    //        let (ReactionRateProvider p) = this
    //        p r


    type ReactionRateProviderParams = 
        {
            seedValue : int option
            distribution: DistributionParams
        }


    type ReactionRateProvider (p : ReactionRateProviderParams) =
        let seedMain = 
            match p.seedValue with 
            | Some s -> s
            | None -> (new Random()).Next()


        let rateDictionary = new Dictionary<ReactionInfo, (ReactionRate option * ReactionRate option)>()

        let calculateRates (r : ReactionInfo) : RelatedReactions = 
            failwith ""

        let getRatesImpl r = 
            match rateDictionary.TryGetValue r with 
            | true, rates -> rates
            | false, _ -> 
                let x = calculateRates r
                x.similar |> List.map (fun (i, e) -> if rateDictionary.ContainsKey i |> not then rateDictionary.Add(i, e)) |> ignore
                x.primary


        member this.getRates r = getRatesImpl r
