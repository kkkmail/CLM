namespace Clm

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


    type ReactionType = 
        | Synthesis
        | CatalyticSynthesis
        | Ligation
        | SedimentationDirect
        //| CatalyticLigation
        //| Sedimentation
        //| SedimentationRemoval


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


    type ReactionRate = 
        | ReactionRate of double


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



    type ModelParams = 
        {
            numberOfAminoAcids : NumberOfAminoAcids
            maxPeptideLength : MaxPeptideLength
            reactionRates : List<ReactionType * ReactionRateProvider>
        }


    type ClmModel (modelParams : ModelParams) = 
        let fromList (a : list<ChiralAminoAcid>) = 
            match a.Length with 
            | 1 -> Chiral a.Head
            | _ -> Peptide a |> PeptideChain

        let chiralL a = a |> L |> Chiral
        let rateProviders = modelParams.reactionRates |> Map.ofList
        let food = FoodSubst.y |> Food
        let aminoAcids = AminoAcid.getAminoAcids modelParams.numberOfAminoAcids
        let chiralAminoAcids = ChiralAminoAcid.getAminoAcids modelParams.numberOfAminoAcids
        let peptides = Peptide.getPeptides modelParams.maxPeptideLength modelParams.numberOfAminoAcids
        let allChains = (chiralAminoAcids |> List.map (fun a -> [ a ])) @ (peptides |> List.map (fun p -> p.aminoAcids))

        let allPairs =
            List.allPairs allChains allChains
            |> List.map (fun (a, b) -> orderPairs (a, b))
            |> List.filter (fun (a, _) -> a.Head.isL)
            |> List.distinct

        let tryCreateReaction g i = 
            match ReversibleReaction.tryCreate g i with 
            | Some r -> Some [ r; r.enantiomer ]
            | None -> None

        let createReactions c l = 
            l
            |> List.map (fun e -> c e)
            |> List.choose id
            |> List.concat


        let synth = 
            match rateProviders.TryFind Synthesis with
            | Some g -> 
                let create a = 
                    {
                        reactionType = Synthesis
                        input = [ (food, 1) ]
                        output = [ (chiralL a, 1) ]
                    }
                    |> tryCreateReaction g
                aminoAcids |> createReactions create
            | None -> []


        let catSynth = 
            match rateProviders.TryFind CatalyticSynthesis with
            | Some g -> 
                let ap = List.allPairs aminoAcids peptides

                let create (a, c) = 
                    let p = c |> PeptideChain
                    {
                        reactionType = CatalyticSynthesis
                        input = [ (food, 1); (p, 1) ]
                        output = [ (chiralL a, 1); (p, 1) ]
                    }
                    |> tryCreateReaction g
                ap |> createReactions create
            | None -> []


        let lig =
            match rateProviders.TryFind Ligation with
            | Some g -> 
                let pairs = allPairs |> List.filter (fun (a, b) -> a.Length + b.Length <= modelParams.maxPeptideLength.length)

                let create (a, b) = 
                        {
                            reactionType = Ligation
                            input = [ (fromList a, 1); (fromList b, 1) ]
                            output = [ (fromList (a @ b), 1) ]
                        }
                        |> tryCreateReaction g
                pairs |> createReactions create
            | None -> []


        let sed = 
            match rateProviders.TryFind SedimentationDirect with 
            | Some g -> 
                let create (a, b) = 
                    {
                        reactionType = SedimentationDirect
                        input = [ (fromList a, 1); (fromList b, 1) ]
                        output = [ (FoodSubst.y |> Food, a.Length + b.Length) ]
                    }
                    |> tryCreateReaction g
                allPairs |> createReactions create
            | None -> []


        member this.synthesisReactions = synth
        member this.catSynthesisReactions = catSynth
        member this.ligationReactions = lig
        member this.sedimentationReactions = sed

