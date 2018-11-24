namespace Clm

open Substances

module ReactionTypes = 

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
            reactionName : ReactionName
            input : list<Substance * int>
            output : list<Substance * int>
        }

        member info.enantiomer = 
            let e (i : list<Substance * int>) = i |> List.map (fun (s, n) -> (s.enantiomer, n))

            {
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


    type SynthesisReaction = 
        | SynthesisReaction of ChiralAminoAcid

        member r.info = 
            let (SynthesisReaction a) = r
            {
                reactionName = ReactionName.SynthesisName
                input = [ (Substance.food, 1) ]
                output = [ (Chiral a, 1) ]
            }


    type SynthCatalyst = 
        | SynthCatalyst of Peptide


    type CatalyticSynthesisReaction = 
        | CatalyticSynthesisReaction of (SynthesisReaction * SynthCatalyst)

        member r.info = 
            let (CatalyticSynthesisReaction ((SynthesisReaction a), (SynthCatalyst c))) = r
            let p = c |> PeptideChain
            {
                reactionName = ReactionName.CatalyticSynthesisName
                input = [ (Substance.food, 1); (p, 1) ]
                output = [ (Chiral a, 1); (p, 1) ]
            }


    type LigationReaction = 
        | LigationReaction of (list<ChiralAminoAcid> * list<ChiralAminoAcid>)

        member r.info = 
            let (LigationReaction (a, b)) = r

            {
                reactionName = ReactionName.LigationName
                input = [ (Substance.fromList a, 1); (Substance.fromList b, 1) ]
                output = [ (Substance.fromList (a @ b), 1) ]
            }


    type LigCatalyst =
        | LigCatalyst of Peptide


    type CatalyticLigationReaction = 
        | CatalyticLigationReaction of (LigationReaction * LigCatalyst)

        member r.info = 
            let (CatalyticLigationReaction (LigationReaction (a, b), LigCatalyst c)) = r

            let p = c |> PeptideChain
            {
                reactionName = ReactionName.CatalyticLigationName
                input = [ (Substance.fromList a, 1); (Substance.fromList b, 1); (p, 1) ]
                output = [ (Substance.fromList (a @ b), 1); (p, 1) ]
            }


    type SedimentationDirectReaction = 
        | SedimentationDirectReaction of (list<ChiralAminoAcid> * list<ChiralAminoAcid>)

        member r.info = 
            let (SedimentationDirectReaction (a, b)) = r

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
        | SedimentationDirect of SedimentationDirectReaction
        | SedimentationAll

        member rt.name = 
            match rt with 
            | Synthesis _ -> "S"
            | CatalyticSynthesis _ -> "CS"
            | Ligation _ -> "L"
            | CatalyticLigation _ -> "CL"
            | SedimentationDirect _ -> "SD"
            | SedimentationAll -> "SA"
