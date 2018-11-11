namespace Clm

open System
open System.Numerics
open MathNet.Numerics.LinearAlgebra

module Substances = 

    type NumberOfAminoAcids = 
        | OneAminoAcid
        | TwoAminoAcids
        | ThreeAminoAcids
        | FourAminoAcids
        | FiveAminoAcids
        | SixAminoAcids
        | SevenAminoAcids
        | EightAminoAcids
        | NineAminoAcids
        | TenAminoAcids
        | ElevenAminoAcids
        | TwelveAminoAcids
        | ThirteenAminoAcids
        | FourteenAminoAcids
        | FifteenAminoAcids
        | SixteenAminoAcids

        member this.length = 
            match this with
            | OneAminoAcid -> 1
            | TwoAminoAcids -> 2
            | ThreeAminoAcids -> 3
            | FourAminoAcids -> 4
            | FiveAminoAcids -> 5
            | SixAminoAcids -> 6
            | SevenAminoAcids -> 7
            | EightAminoAcids -> 8
            | NineAminoAcids -> 9
            | TenAminoAcids -> 10
            | ElevenAminoAcids -> 11
            | TwelveAminoAcids -> 12
            | ThirteenAminoAcids -> 13
            | FourteenAminoAcids -> 14
            | FifteenAminoAcids -> 15
            | SixteenAminoAcids -> 16


    type MaxPeptideLength = 
        | TwoMax
        | ThreeMax
        | FourMax
        | FiveMax

        member this.length = 
            match this with 
            | TwoMax -> 2
            | ThreeMax -> 3
            | FourMax -> 4
            | FiveMax -> 5


    type FoodSubst =
        | FoodSubst

        member __.length = 0
        member __.name = "Y"
        static member y = FoodSubst


    type AminoAcid = 
        | A01
        | A02
        | A03
        | A04
        | A05
        | A06
        | A07
        | A08
        | A09
        | A10
        | A11
        | A12
        | A13
        | A14
        | A15
        | A16

        static member private all = 
            [
                A01
                A02
                A03
                A04
                A05
                A06
                A07
                A08
                A09
                A10
                A11
                A12
                A13
                A14
                A15
                A16
            ]

        member aminoAcid.name = 
            match aminoAcid with 
            | A01 -> "A"
            | A02 -> "B"
            | A03 -> "C"
            | A04 -> "D"
            | A05 -> "E"
            | A06 -> "F"
            | A07 -> "G"
            | A08 -> "H"
            | A09 -> "I"
            | A10 -> "J"
            | A11 -> "K"
            | A12 -> "L"
            | A13 -> "M"
            | A14 -> "N"
            | A15 -> "O"
            | A16 -> "P"

        static member toString (a : AminoAcid) = sprintf "%A" a

        static member names = AminoAcid.all |> List.map (fun e -> e, e.name) |> Map.ofList

        static member getAminoAcids (n :NumberOfAminoAcids) = 
            AminoAcid.all
            |> List.take n.length


    type ChiralAminoAcid = 
        | L of AminoAcid
        | R of AminoAcid

        member __.length = 1

        member aminoAcid.isL = 
            match aminoAcid with 
            | L _ -> true
            | R _ -> false

        member aminoAcid.isR = aminoAcid.isL |> not

        member aminoAcid.enantiomer = 
            match aminoAcid with 
            | L a -> R a
            | R a -> L a

        static member getAminoAcids n = 
            (AminoAcid.getAminoAcids n |> List.map (fun a -> L a))
            @
            (AminoAcid.getAminoAcids n |> List.map (fun a -> R a))

        member aminoAcid.name =
            match aminoAcid with 
            | L a -> a.name
            | R a -> a.name.ToLower()


    /// TODO 20181029 Check.
    let orderPairs (a : list<ChiralAminoAcid>, b : list<ChiralAminoAcid>) = 
        if a.Length < b.Length
        then (a, b)
        else 
            if a.Length > b.Length
            then (b, a)
            else 
                if a <= b then (a, b)
                else (b, a)


    type Peptide = 
        | Peptide of list<ChiralAminoAcid>

        member peptide.length = 
            let (Peptide p) = peptide
            p.Length

        member peptide.enantiomer = 
            let (Peptide p) = peptide
            p |> List.map (fun a -> a.enantiomer) |> Peptide

        member peptide.aminoAcids = 
            let (Peptide p) = peptide
            p

        member peptide.name = 
            peptide.aminoAcids
            |> List.map (fun a -> a.name)
            |> String.concat ""


        static member private create m n = 
            let rec makePeptide acc l = 
                //printfn "makePeptide::l = %A" l
                //printfn "makePeptide::acc = %A" acc
                match l with 
                | [] -> acc
                | h :: t -> 
                    match acc with 
                    | [] -> makePeptide (h |> List.map (fun e -> [e])) t
                    | _ -> 
                        //let pairs = (List.allPairs h acc)
                        //printfn "makePeptide::pairs = %A" pairs
                        //let x = pairs |> List.map (fun e -> e)
                        makePeptide ((List.allPairs h acc) |> List.map (fun (a, e) -> a :: e)) t

            let aa = ChiralAminoAcid.getAminoAcids n
            //printfn "aa = %A" aa
            [ for _ in 1..m -> aa ]
            |> makePeptide []
            |> List.map (fun e -> Peptide e)

        /// Peptides start from length 2.
        static member getPeptides (m : MaxPeptideLength) n = 
            [ for i in 2..m.length -> Peptide.create i n]
            |> List.concat


    type Substance = 
        | Food of FoodSubst
        | Chiral of ChiralAminoAcid
        | PeptideChain of Peptide

        member substance.enantiomer = 
            match substance with 
            | Food f -> f |> Food
            | Chiral c -> c.enantiomer |> Chiral
            | PeptideChain p -> p.enantiomer |> PeptideChain

        member substance.name = 
            match substance with 
            | Food f -> f.name
            | Chiral c -> c.name
            | PeptideChain p -> p.name


    /// Maps substances to array / vector indices.
    type SubstanceMap = Map<Substance, int>


    type ReactionType = 
        | Synthesis
        | CatalyticSynthesis
        | Ligation
        | CatalyticLigation
        | SedimentationDirect
        //| Sedimentation
        //| SedimentationRemoval

        member rt.name = sprintf "%A" rt


    type ReactionInfo =
        {
            reactionType : ReactionType
            input : list<Substance * int>
            output : list<Substance * int>
        }

        member info.enantiomer = 
            let e (i : list<Substance * int>) = i |> List.map (fun (s, n) -> (s.enantiomer, n))

            {
                reactionType = info.reactionType
                input = info.input |> e
                output = info.output |> e
            }

        member this.getName a = 
            let g (l : list<Substance * int>) = 
                l
                |> List.map (fun (s, n) -> (if n = 1 then "" else n.ToString() + " ") + s.name)
                |> String.concat " + "

            this.reactionType.name + ": " + (g this.input) + a + (g this.output)


    type ReactionRate = 
        | ReactionRate of double


    /// Returns [optional] forward and backward reaction rates.
    type ReactionRateProvider = 
        | ReactionRateProvider of (ReactionInfo -> (ReactionRate option * ReactionRate option))


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

        static member tryCreate (ReactionRateProvider g) i = 
            match g i with 
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
