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


    type DistributionParams = 
        {
            threshold : double option
        }


    [<AbstractClass>]
    type DistributionBase(seed : int, p : DistributionParams, d : Random -> double) = 
        let rnd = new Random(seed)
        let rndBool = new Random(rnd.Next())
        let isDefined() = 
            match p.threshold with
            | Some t -> if rndBool.NextDouble() < t then true else false
            | None -> true

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

    type SyntethisModelParam = 
        {
            synthesisDistribution : Distribution
            forwardScale : double option
            backwardScale : double option
        }


    type SyntethisModel = 
        | SyntethisModel of SyntethisModelParam

        member this.getRates (r : ReactionInfo) =
            match r.reactionName with 
            | SynthesisName -> 
                let (SyntethisModel p) = this
                p.synthesisDistribution.nextDoubleOpt() |> getRates p.forwardScale p.backwardScale
            | _ -> noRates


    type CatalystModelRandomParam = 
        {
            distribution : Distribution
            forwardScale : double option
            backwardScale : double option
            enantioselectivityParam : double
        }


    type CatalystModelSimilarityParam = 
        {
            dummy : int
        }


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

    type SedimentationAllRandomParam = 
        {
            sedimentationAllDistribution : Distribution
            forwardScale : double option
        }


    type SedimentationAllModel = 
        | SedimentationAllRandom of SedimentationAllRandomParam

        member this.getRates (r : ReactionInfo) =
            match r.reactionName with 
            | SedimentationAllName -> 
                match this with 
                | SedimentationAllRandom p -> p.sedimentationAllDistribution.nextDouble() |> Some |> getRates p.forwardScale None
            | _ -> noRates


    type ReactionRateModel = 
        | SynthesisRateModel of SyntethisModel
        | SedimentationDirectRateModel of SedimentationDirectModel
        | SedimentationAllRateModel of SedimentationAllModel

        member this.getRates (r : ReactionInfo) = 
            match this with 
            | SynthesisRateModel m -> m.getRates r
            | SedimentationDirectRateModel m -> m.getRates r
            | SedimentationAllRateModel  m -> m.getRates r


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
                if rateDictionary.ContainsKey r.enantiomer |> not then rateDictionary.Add(r.enantiomer, x.primary)
                x.similar |> List.map (fun (i, e) -> if rateDictionary.ContainsKey i |> not then rateDictionary.Add(i, e)) |> ignore
                x.similar |> List.map (fun (i, e) -> if rateDictionary.ContainsKey i.enantiomer |> not then rateDictionary.Add(i.enantiomer, e)) |> ignore
                x.primary

        member __.getRates r = getRatesImpl r

        static member defaultSynthesisModel (rnd : Random) forward backward =
            {
                synthesisDistribution = UniformDistribution(rnd.Next(), { threshold = None }) |> Uniform
                forwardScale = Some forward
                backwardScale = Some backward
            }
            |> SyntethisModel
            |> SynthesisRateModel
            |> ReactionRateProvider


        static member defaultSedimentationDirectModel (rnd : Random) threshold mult =
            {
                sedimentationDirectDistribution = TriangularDistribution(rnd.Next(), { threshold = Some threshold }) |> Triangular
                forwardScale = Some mult
            }
            |> SedimentationDirectRandom
            |> SedimentationDirectRateModel
            |> ReactionRateProvider

        static member defaultSedimentationAllModel (rnd : Random) mult =
            {
                sedimentationAllDistribution = UniformDistribution(rnd.Next(), { threshold = None }) |> Uniform
                forwardScale = Some mult
            }
            |> SedimentationAllRandom
            |> SedimentationAllRateModel
            |> ReactionRateProvider

