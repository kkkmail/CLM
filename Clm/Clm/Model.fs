namespace Clm

open System
open System.Collections.Generic
open FSharp.Collections
open System.Numerics
open MathNet.Numerics.LinearAlgebra
open Clm.Substances


module Model = 


    type ModelParams = 
        {
            numberOfAminoAcids : NumberOfAminoAcids
            maxPeptideLength : MaxPeptideLength
            reactionRates : List<ReactionType * ReactionRateProvider>
        }


    //type EquationPart = 
    //    {
    //        part : string
    //        reaction : Reaction
    //    }

    //    member p.ToString() = 
    //        ""


    //type Equation = 
    //    {
    //        parts : list<string>
    //    }

    //    static member header = "            [|"
    //    static member footer = "            |]\n            |> Array.fold (fun acc r -> acc + r) 0.0"

    //    member private e.substComment s = "            // " + s.ToString() + "\n"

    //    member e.toString() = 
    //        ""

    //    static member empty = 
    //        {
    //            parts = []
    //        }


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

        // For flexibility. Update when necessary.
        let synthCatalysts = peptides
        let ligCatalysts = peptides

        let allChains = (chiralAminoAcids |> List.map (fun a -> [ a ])) @ (peptides |> List.map (fun p -> p.aminoAcids))

        let allPairs =
            List.allPairs allChains allChains
            |> List.map (fun (a, b) -> orderPairs (a, b))
            |> List.filter (fun (a, _) -> a.Head.isL)
            |> List.distinct

        let ligationPairs = allPairs |> List.filter (fun (a, b) -> a.Length + b.Length <= modelParams.maxPeptideLength.length)

        let allSubst = 
            [ food ]
            @
            (chiralAminoAcids |> List.map (fun a -> Chiral a))
            @
            (peptides |> List.map (fun p -> PeptideChain p))

        let allInd = allSubst |> List.mapi (fun i s -> (s, i)) |> Map.ofList


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
                let ap = List.allPairs aminoAcids synthCatalysts

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
                let create (a, b) = 
                    {
                        reactionType = Ligation
                        input = [ (fromList a, 1); (fromList b, 1) ]
                        output = [ (fromList (a @ b), 1) ]
                    }
                    |> tryCreateReaction g
                ligationPairs |> createReactions create
            | None -> []


        let catLig =
            match rateProviders.TryFind CatalyticLigation with
            | Some g -> 
                let ap = List.allPairs ligationPairs ligCatalysts

                let create ((a, b), c) = 
                    let p = c |> PeptideChain
                    {
                        reactionType = CatalyticLigation
                        input = [ (fromList a, 1); (fromList b, 1); (p, 1) ]
                        output = [ (fromList (a @ b), 1); (p, 1) ]
                    }
                    |> tryCreateReaction g
                ap |> createReactions create
            | None -> []


        let sedDir = 
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


        let allReac = synth @ catSynth @ lig @ catLig @ sedDir

        //let concentration (i : Map<Substance, double>) s = match i.TryFind s with | Some x -> x | None -> 0.0
        //
        //let mutable counter = 0
        //
        //let updateReac (r : Reaction) (m0 : Map<Substance, Map<Substance, double> -> double>) : Map<Substance, Map<Substance, double> -> double> = 
        //
        //    //printfn "updateReac::counter = %A" counter
        //    counter <- counter + 1
        //
        //    let getRate (l : list<Substance * int>) (i : Map<Substance, double>) : double = 
        //        l |> List.fold (fun acc (e, n) -> acc * (pown (concentration i e) n)) 1.0
        //
        //    let updateReactionForSubst 
        //        ((s, n) : Substance * int) 
        //        (rate : Map<Substance, double> -> double) 
        //        (v : Map<Substance, Map<Substance, double> -> double>) 
        //        : Map<Substance, Map<Substance, double> -> double> = 
        //        let c (i : Map<Substance, double>) : double = (float n) * (rate i)
        //
        //        match v.TryFind s with 
        //        | Some w -> v.Add (s, fun i -> (w i) + (c i))
        //        | None -> v.Add (s, c)
        //
        //    let update iRate oRate (info : ReactionInfo) m = 
        //        printfn "update::counter = %A" counter
        //        let mi = info.input |> List.fold (fun acc e -> updateReactionForSubst e iRate acc) m
        //        info.output |> List.fold (fun acc e -> updateReactionForSubst e oRate acc) mi
        //
        //    let updateForward (info : ReactionInfo) (ReactionRate rate) m = 
        //        let iRate g = -(getRate info.input g) * rate
        //        let oRate g = (getRate info.output g) * rate
        //        update iRate oRate info m
        //
        //    let updateBackward (info : ReactionInfo) (ReactionRate rate) m = 
        //        let iRate g = (getRate info.input g) * rate
        //        let oRate g = -(getRate info.output g) * rate
        //        update iRate oRate info m
        //
        //    match r with 
        //    | Forward f -> updateForward f.reactionInfo f.forwardRate m0
        //    | Backward b -> updateBackward b.reactionInfo b.backwardRate m0
        //    | Reversible rv ->
        //        updateForward rv.reactionInfo rv.forwardRate m0
        //        |> updateBackward rv.reactionInfo rv.backwardRate
        //
        //let updateAllReac = allReac |> List.fold (fun acc r -> updateReac r acc) Map.empty
        //
        //let updateAllReacVec (i : Vector<double>) = //: Vector<double> = 
        //    printfn "updateAllReacVec::Starting."
        //
        //    let im = allSubst |> List.map (fun s -> 
        //        printfn "in::s = %A, allInd.[s] = %A, i.[allInd.[s]] = %A" s allInd.[s] i.[allInd.[s]]
        //        (s, i.[allInd.[s]])) |> Map.ofList
        //    printfn "updateAllReacVec::im.Count: %A" im.Count
        //
        //    let mutable counter = 0
        //
        //    let c (i : Map<Substance, Map<Substance, double> -> double>) s = 
        //        //printfn "c::counter = %A" counter
        //        counter <- counter + 1
        //        match i.TryFind s with 
        //        | Some x -> 
        //            //printfn "c::x = %A" x
        //            x im 
        //        | None -> 0.0
        //
        //    let om = 
        //        allSubst
        //        |> List.map(fun s -> 
        //            printfn "om::s = %A" s
        //            c updateAllReac s)
        //        //|> vector
        //    om
        //    //[ 0.0 ] |> vector

        let reactDictionary = new Dictionary<Substance, list<string>>(allSubst.Length)

        let xName = "x"
        let substComment (s : Substance) = "            // " + (allInd.[s]).ToString() + " - " + s.ToString() + "\n"
        let reactionComment (r : Reaction) = " // " + r.ToString() + "\n"
        let x (s : Substance) = xName + ".[" + (allInd.[s]).ToString() + "]"

        let rate (l : list<Substance * int>) (ReactionRate r) = 
            let toFloat (s : string) = 
                match s.Contains(".") with 
                | true -> s
                | false -> s + ".0"

            let toPown s n = 
                match n with 
                | 1 -> x s
                | _ -> "(pown " + (x s) + " " + n.ToString() + ")"

            let a = l |> List.fold(fun acc (s, n) -> acc + (if acc <> "" then " * " else "") + (toPown s n)) ""
            (r.ToString() |> toFloat) + " * " + a + " // " + l.ToString() + "\n"

        //let processReaction (r : Reaction) (m : Map<Substance, Equation>) : Map<Substance, Equation> =
        let processReaction (r : Reaction) =
            let toMult (i : int) = 
                match i with 
                | 1 -> ""
                | _ -> i.ToString() + ".0 * "

            let updateMap (s : Substance) (e : string) = 
                match reactDictionary.TryGetValue s with 
                | (true, w) -> reactDictionary.[s] <- (e :: w)
                | (false, _) -> reactDictionary.Add (s, [ e ])

            let update i o r f = 
                let shift = "                "
                let (iSign, oSign) = if f then "-", "" else "", "-"
                let fwd = rate i r
                i |> List.iter (fun (s, n) -> updateMap s (shift + iSign + (toMult n) + fwd))
                o |> List.iter (fun (s, n) -> updateMap s (shift + oSign + (toMult n) + fwd))

            match r with
            | Forward f -> update f.reactionInfo.input f.reactionInfo.output f.forwardRate true
            | Backward b -> update b.reactionInfo.input b.reactionInfo.output b.backwardRate false
            | Reversible rv ->
                update rv.reactionInfo.input rv.reactionInfo.output rv.forwardRate true
                update rv.reactionInfo.input rv.reactionInfo.output rv.backwardRate false


        let generate () = 
            let t0 = DateTime.Now
            printfn "t0 = %A" t0
            allReac |> List.iter (fun r -> processReaction r)

            let t1 = DateTime.Now
            printfn "t1 = %A" t1
            printfn "t1 - t0 = %A" (t1 - t0).TotalSeconds

            let getReaction s = 
                match reactDictionary.TryGetValue s with 
                | (true, r) -> r |> List.rev |> List.fold (fun acc e -> acc + e) ""
                | (false, _) -> ""

            let a = 
                allSubst
                |> List.fold (fun acc s -> acc + "\n" + (substComment s) +  "            [|\n" + (getReaction s) + "            |]\n            |> Array.fold (fun acc r -> acc + r) 0.0\n") ""

            let t2 = DateTime.Now
            printfn "t2 = %A" t2
            printfn "t2 - t1 = %A" (t2 - t1).TotalSeconds

            "namespace Model\n\nmodule ModelData = \n\n    let update (x : array<double>) : array<double> = \n" +
            "        [|" + a + "        |]\n"


        member model.allSubstances = allSubst
        member model.synthesis = synth
        member model.catalyticSynthesis = catSynth
        member model.ligation = lig
        member model.catalyticLigation = catLig
        member model.sedimentationDirect = sedDir
        member model.allReactions = allReac
        //member model.getGradient v = updateAllReacVec v
        //member model.updateAllReacions = updateAllReac

        member model.generateCode() = generate()
