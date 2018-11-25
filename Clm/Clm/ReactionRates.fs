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
        let nextDoubleFromZeroToOneImpl() = rnd.NextDouble()

        member __.nextDouble = nextDoubleImpl
        member __.nextDoubleFromZeroToOne = nextDoubleFromZeroToOneImpl

        member __.nextDoubleOpt() = 
            match isDefined() with 
            | true -> nextDoubleImpl() |> Some
            | false -> None


    type DeltaDistribution (seed : int, p : DistributionParams) = 
        inherit DistributionBase (seed, p, fun _ -> 1.0)


    type UniformDistribution (seed : int, p : DistributionParams) = 
        inherit DistributionBase (seed, p, fun r -> r.NextDouble())


    type TriangularDistribution (seed : int, p : DistributionParams) = 
        inherit DistributionBase(seed, p, fun r -> 1.0 - sqrt(1.0 - r.NextDouble()))


    type Distribution =
        | Delta of DeltaDistribution
        | Uniform of UniformDistribution
        | Triangular of TriangularDistribution

        member this.nextDouble = 
            match this with
            | Delta d -> d.nextDouble
            | Uniform d -> d.nextDouble
            | Triangular d -> d.nextDouble

        member this.nextDoubleOpt = 
            match this with
            | Delta d -> d.nextDoubleOpt
            | Uniform d -> d.nextDoubleOpt
            | Triangular d -> d.nextDoubleOpt

        member this.nextDoubleFromZeroToOne =
            match this with
            | Delta d -> d.nextDoubleFromZeroToOne
            | Uniform d -> d.nextDoubleFromZeroToOne
            | Triangular d -> d.nextDoubleFromZeroToOne


    type RelatedReactions = 
        {
            primary : (ReactionRate option * ReactionRate option)
            similar : list<Reaction * (ReactionRate option * ReactionRate option)>
        }


    let noRates = 
        {
            primary = (None, None)
            similar = []
        }


    let getRates (fo, rf) (bo, rb) = 
        let g so ro = 
            match so, ro with
            | Some s, Some r -> s * r |> ReactionRate |> Some
            | _ -> None

        {
            primary = (g fo rf, g bo rb)
            similar = []
        }


    let getForwardRates (fo, rf) = getRates (fo, rf) (None, None)


    type SyntethisParam = 
        {
            synthesisDistribution : Distribution
            forwardScale : double option
            backwardScale : double option
        }


    type SyntethisModel = 
        | SyntethisModel of SyntethisParam

        member this.getRates (r : Reaction) =
            match r.name with 
            | SynthesisName -> 
                let (SyntethisModel p) = this
                let d = p.synthesisDistribution
                getRates (p.forwardScale, d.nextDouble() |> Some) (p.backwardScale, d.nextDouble() |> Some)
            | _ -> noRates


    type CatalyticSynthesisRandomParam = 
        {
            catSynthDistribution : Distribution
            forwardScale : double option
            backwardScale : double option
            maxEe : double
        }


    /// Models, which describe catalytic reactions.
    type CatalyticSynthesisModel = 
        | CatalyticSynthesisRandom of CatalyticSynthesisRandomParam
        //| CatalystModelSimilarity of CatalystModelSimilarityParam // If substance is a catalyst for some amino acid X, then there are some [separate] chances that it could be a catalyst for other amino acids.

        member this.getRates (r : Reaction) =
            match r.name with 
            | CatalyticSynthesisName -> 
                match this with 
                | CatalyticSynthesisRandom p -> 
                    let d = p.catSynthDistribution
                    match d.nextDoubleOpt() with 
                    | Some rf -> 
                        let rb = d.nextDouble()
                        let ee = p.maxEe * (d.nextDoubleFromZeroToOne() - 0.5)

                        //p.distribution.nextDoubleOpt() |> getRates p.forwardScale p.backwardScale
                        failwith ""
                    | None -> noRates
            | _ -> noRates


    type SedimentationDirectRandomParam = 
        {
            sedimentationDirectDistribution : Distribution
            forwardScale : double option
        }


    type SedimentationDirectModel = 
        | SedimentationDirectRandom of SedimentationDirectRandomParam

        member this.getRates (r : Reaction) =
            match r.name with
            | SedimentationDirectName ->
                match this with 
                | SedimentationDirectRandom p -> getForwardRates (p.forwardScale, p.sedimentationDirectDistribution.nextDoubleOpt())
            | _ -> noRates

    type SedimentationAllRandomParam = 
        {
            sedimentationAllDistribution : Distribution
            forwardScale : double option
        }


    type SedimentationAllModel = 
        | SedimentationAllRandom of SedimentationAllRandomParam

        member this.getRates (r : Reaction) =
            match r.name with 
            | SedimentationAllName -> 
                match this with 
                | SedimentationAllRandom p -> getForwardRates (p.forwardScale, p.sedimentationAllDistribution.nextDouble() |> Some)
            | _ -> noRates


    type ReactionRateModel = 
        | SynthesisRateModel of SyntethisModel
        | SedimentationDirectRateModel of SedimentationDirectModel
        | SedimentationAllRateModel of SedimentationAllModel

        member this.getRates (r : Reaction) = 
            match this with 
            | SynthesisRateModel m -> m.getRates r
            | SedimentationDirectRateModel m -> m.getRates r
            | SedimentationAllRateModel  m -> m.getRates r


    type ReactionRateProviderParams = 
        {
            rateModel: ReactionRateModel
        }


    type ReactionRateProvider (rateModel: ReactionRateModel) =
        let rateDictionary = new Dictionary<Reaction, (ReactionRate option * ReactionRate option)>()
        let calculateRates (r : Reaction) : RelatedReactions = rateModel.getRates r

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
                synthesisDistribution = DeltaDistribution(rnd.Next(), { threshold = None }) |> Delta
                //synthesisDistribution = UniformDistribution(rnd.Next(), { threshold = None }) |> Uniform
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
