namespace Clm

module Substances = 

    [<Literal>]
    let MaxPeptideLength = 3


    type NumberOfAminoAcids = 
        | OneAminoAcid
        | TwoAminoAcids
        | ThreeAminoAcids
        | FourAminoAcids
        | FiveAminoAcids
        | SixAminoAcids


    type FoodSubst =
        | FoodSubst

        member __.length = 0
        static member y = FoodSubst


    type AminoAcid = 
        | A
        | B
        //| C
        //| D
        //| E
        //| F

        static member private all = 
            [
                A
                B
                //C
                //B
                //E
                //F
            ]

        static member getAminoAcids (n :NumberOfAminoAcids) = 
            AminoAcid.all


    type ChiralAminoAcid = 
        | L of AminoAcid
        | R of AminoAcid

        member __.length = 1

        member aminoAcid.enantiomer = 
            match aminoAcid with 
            | L a -> R a
            | R a -> L a

        static member getAminoAcids n = 
            (AminoAcid.getAminoAcids n |> List.map (fun a -> L a))
            @
            (AminoAcid.getAminoAcids n |> List.map (fun a -> R a))


    /// TODO 20181029 Check. Perhaps it is a default comparision anyway.
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

        static member private create m n = 
            let rec makePeptide acc l = 
                match l with 
                | [] -> acc
                | h :: t -> makePeptide (List.allPairs h acc |> List.map (fun (a, e) -> a :: e)) t

            // Peptides start from length 2.
            let aa = ChiralAminoAcid.getAminoAcids n
            [ for _ in 2..m -> aa ]
            |> makePeptide []
            |> List.map (fun e -> Peptide e)

        static member getPeptides m n = 
            [ for i in 1..m -> Peptide.create i n]
            |> List.concat


    type Substance = 
        | Food of FoodSubst
        | Chiral of ChiralAminoAcid
        | Peptide of Peptide

        member substance.enantiomer = 
            match substance with 
            | Food f -> f |> Food
            | Chiral c -> c.enantiomer |> Chiral
            | Peptide p -> p.enantiomer |> Peptide


    type ReactionType = 
        | Synthesis
        | CatalyticSynthesis
        | Ligation
        //| CatalyticLigation
        | Sedimentation
        | SedimentationRemoval


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


    type ReactionRate = double


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

        static member tryCreate g i = 
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


    let synthesisReactions n g = 
        let createSynthesisReaction g a = 
            let i = 
                {
                    reactionType = Synthesis
                    input = [ (FoodSubst.y |> Food, 1) ]
                    output = [ ( a |> L |> Chiral, 1) ]
                }

            match ReversibleReaction.tryCreate g i with 
            | Some r -> Some [ r; r.enantiomer ]
            | None -> None

        AminoAcid.getAminoAcids n
        |> List.map (fun e -> createSynthesisReaction g e)
        |> List.choose id
        |> List.concat


    let ligationReactions m n g = 
        let a = ChiralAminoAcid.getAminoAcids n |> List.map (fun a -> [ a ])
        let p = a @ (Peptide.getPeptides m n |> List.map (fun p -> p.aminoAcids))

        let pairs = 
            List.zip p p
            |> List.map (fun (a, b) -> orderPairs (a, b))


        0
