namespace Clm

open System
open System.Numerics
open MathNet.Numerics.LinearAlgebra
open Clm.Substances

module Reactions = 

    type ReactionName = 
        | SynthesisName
        | CatalyticSynthesisName
        | LigationName
        | CatalyticLigationName
        | SedimentationDirectName
        | SedimentationAllName

        member this.name = sprintf "%A" this


    type ReactionInfo =
        {
            //reactionType : ReactionType
            reactionName : ReactionName
            input : list<Substance * int>
            output : list<Substance * int>
        }

        member info.enantiomer = 
            let e (i : list<Substance * int>) = i |> List.map (fun (s, n) -> (s.enantiomer, n))

            {
                //reactionType = info.reactionType
                reactionName = info.reactionName
                input = info.input |> e
                output = info.output |> e
            }

        member this.getName a = 
            let g (l : list<Substance * int>) = 
                l
                |> List.map (fun (s, n) -> (if n = 1 then "" else n.ToString() + " ") + s.name)
                |> String.concat " + "

            this.reactionName.name + ": " + (g this.input) + a + (g this.output)


    type ReactionRate = 
        | ReactionRate of double


    /// Returns [optional] forward and backward reaction rates.
    type ReactionRateProvider = 
        | ReactionRateProvider of (ReactionInfo -> (ReactionRate option * ReactionRate option))

        member this.getRates r = 
            let (ReactionRateProvider p) = this
            p r


    type SynthesisReaction = 
        static member create a = 
            {
                //reactionType = Synthesis
                reactionName = ReactionName.SynthesisName
                input = [ (Substance.food, 1) ]
                output = [ (Substance.chiralL a, 1) ]
            }


    type CatalyticSynthesisReaction = 
        static member create (a, c) = 
            let p = c |> PeptideChain
            {
                reactionName = ReactionName.CatalyticSynthesisName
                input = [ (Substance.food, 1); (p, 1) ]
                output = [ (Substance.chiralL a, 1); (p, 1) ]
            }


    type LigationReaction = 
        static member create (a, b) = 
            {
                reactionName = ReactionName.LigationName
                input = [ (Substance.fromList a, 1); (Substance.fromList b, 1) ]
                output = [ (Substance.fromList (a @ b), 1) ]
            }


    type CatalyticLigationReaction = 
        static member create ((a, b), c) = 
            let p = c |> PeptideChain
            {
                reactionName = ReactionName.CatalyticLigationName
                input = [ (Substance.fromList a, 1); (Substance.fromList b, 1); (p, 1) ]
                output = [ (Substance.fromList (a @ b), 1); (p, 1) ]
            }


    type SedimentationDirectReaction = 
        static member create (a, b) = 
            {
                reactionName = ReactionName.SedimentationDirectName
                input = [ (Substance.fromList a, 1); (Substance.fromList b, 1) ]
                output = [ (FoodSubst.y |> Food, a.Length + b.Length) ]
            }


    type ReactionType = 
        | Synthesis of SynthesisReaction
        | CatalyticSynthesis of CatalyticSynthesisReaction
        | Ligation of LigationReaction
        | CatalyticLigation of CatalyticLigationReaction
        | SedimentationDirect of int
        | SedimentationAll of int

        member rt.name = 
            match rt with 
            | Synthesis _ -> "S"
            | CatalyticSynthesis _ -> "CS"
            | Ligation _ -> "L"
            | CatalyticLigation _ -> "CL"
            | SedimentationDirect _ -> "SD"
            | SedimentationAll _ -> "SA"


    type ForwardReaction =
        {
            reactionInfo : ReactionInfo
            forwardRate : ReactionRate
        }

        member reaction.enantiomer = { reaction with reactionInfo = reaction.reactionInfo.enantiomer }

        static member tryCreate g i = 
            match g i with 
            | Some f ->
                {
                    reactionInfo = i
                    forwardRate = f
                } 
                |> Forward
                |> Some
            | None -> None


    and BackwardReaction =
        {
            reactionInfo : ReactionInfo
            backwardRate : ReactionRate
        }

        member reaction.enantiomer = { reaction with reactionInfo = reaction.reactionInfo.enantiomer }

        static member tryCreate g i = 
            match g i with 
            | Some b ->
                {
                    reactionInfo = i
                    backwardRate = b
                } 
                |> Backward
                |> Some
            | None -> None


    and ReversibleReaction =
        {
            reactionInfo : ReactionInfo
            forwardRate : ReactionRate
            backwardRate : ReactionRate
        }

        member reaction.enantiomer = { reaction with reactionInfo = reaction.reactionInfo.enantiomer }

        static member tryCreate (g : ReactionRateProvider) i = 
            match g.getRates i with 
            | Some f, Some b ->
                {
                    reactionInfo = i
                    forwardRate = f
                    backwardRate = b
                } 
                |> Reversible
                |> Some
            | Some f, None -> ForwardReaction.tryCreate (fun _ -> Some f) i
            | None, Some b -> BackwardReaction.tryCreate (fun _ -> Some b) i
            | None, None -> None


    and Reaction =
        | Forward of ForwardReaction
        | Backward of BackwardReaction
        | Reversible of ReversibleReaction

        member reaction.enantiomer =
            match reaction with 
            | Forward r -> r.enantiomer |> Forward
            | Backward r -> r.enantiomer |> Backward
            | Reversible r -> r.enantiomer |> Reversible

        member this.name = 
            let a, i =
                match this with
                | Forward r -> " -> ", r.reactionInfo
                | Backward r -> " <- ", r.reactionInfo
                | Reversible r -> " <-> ", r.reactionInfo

            i.getName a

        static member tryCreateReaction g i = 
            match ReversibleReaction.tryCreate g i with 
            | Some r -> Some [ r; r.enantiomer ]
            | None -> None
