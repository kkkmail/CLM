namespace Clm

open System
open System.Collections.Generic
open FSharp.Collections
open System.Numerics
open MathNet.Numerics.LinearAlgebra
open Clm.Substances
open Clm.Reactions


module Model = 


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


        let allNamesMap = 
            allSubst
            |> List.map (fun s -> s, s.name)
            |> Map.ofList


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


        let allReac = 
            synth @ catSynth @ lig @ catLig @ sedDir
            |> List.distinct


        let allReacMap = 
            allReac
            |> List.map (fun e -> e, e.name)
            |> Map.ofList


        let substToString s = allNamesMap.[s]
        let reactToString r = allReacMap.[r]
        let lstToString (l : list<Substance * int>) = 
            l
            |> List.map (fun (s, n) -> (if n = 1 then "" else n.ToString() + " ") + (substToString s))
            |> String.concat " + "

        let xName = "x"
        let xSumName = "xSum"
        let substComment (s : Substance) = "            // " + (allInd.[s]).ToString() + " - " + (substToString s) + "\n"
        //let reactionComment (r : Reaction) = " // " + (reactToString r) + "\n"
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
            (r.ToString() |> toFloat) + " * " + a + " // " + (lstToString l) // + "\n"

        let toMult i = 
            match i with 
            | 1 -> String.Empty
            | _ -> i.ToString() + ".0 * "

        let processReaction (r : Reaction) : list<Substance * string> =
            let update i o r f rc : list<Substance * string> = 
                let catalysts = 
                    (o |> List.map (fun (s, n) -> s, -n)) @ i
                    |> List.groupBy (fun (s, _) -> s)
                    |> List.map (fun (s, e) -> (s, e |> List.fold (fun acc (_, n) -> acc + n) 0))
                    |> List.filter (fun (_, n) -> n = 0)
                    |> List.map (fun (s, _) -> s)
                    |> Set.ofList

                let shift = "                "
                let (iSign, oSign) = if f then "-", "" else "", "-"
                let fwd = rate i r
                (
                    i
                    |> List.filter (fun (s, _) -> catalysts.Contains s |> not)
                    |> List.map (fun (s, n) -> (s, (shift + iSign + (toMult n) + fwd + " | " + rc + "\n")))
                )
                @
                (
                    o
                    |> List.filter (fun (s, _) -> catalysts.Contains s |> not)
                    |> List.map (fun (s, n) -> (s, (shift + oSign + (toMult n) + fwd+ " | " + rc + "\n")))
                )

            let rc = reactToString r

            match r with
            | Forward f -> update f.reactionInfo.input f.reactionInfo.output f.forwardRate true rc
            | Backward b -> update b.reactionInfo.input b.reactionInfo.output b.backwardRate false rc
            | Reversible rv ->
                (update rv.reactionInfo.input rv.reactionInfo.output rv.forwardRate true rc)
                @
                (update rv.reactionInfo.output rv.reactionInfo.input rv.backwardRate true rc)


        let generateTotals () = 
            let g a =
                allSubst
                |> List.map (fun s -> match s.noOfAminoAcid a with | Some i -> Some (s, i) | None -> None)
                |> List.choose id
                |> List.map (fun (s, i) -> "                    " + (toMult i) + (x s) + " // " + (substToString s))

            let gg (v : list<string>) = 
                let a = v |> String.concat "\n"
                "                [|\n" + a + "\n                |]\n                |> Array.sum\n"

            let gg1 ((a : AminoAcid), l, r) = 
                "            // " + a.name + "\n            (\n" + (gg l) + "                ,\n" + (gg r) + "            )\n"

            let y =
                aminoAcids
                |> List.map (fun a -> a, L a |> g, R a |> g)
                |> List.map (fun (a, l, r) -> gg1 (a, l, r))

            let x =
                y
                |> String.concat "\n"

            "    let getTotals (x : array<double>) = \n" +
            "        [|\n" +
            x +
            "        |]\n"


        let generate () = 
            let t0 = DateTime.Now
            printfn "t0 = %A" t0

            let r0 = 
                allReac 
                |> List.map (fun r -> processReaction r)

            printfn "r0.Length = %A" r0.Length
            let t11 = DateTime.Now
            printfn "t11 = %A" t11
            printfn "t11 - t0 = %A" (t11 - t0).TotalSeconds

            let reactions = 
                r0
                |> List.concat
                |> List.groupBy (fun (s, _) -> s)
                |> Map.ofList

            let t1 = DateTime.Now
            printfn "t1 = %A" t1
            printfn "t1 - t11 = %A" (t1 - t11).TotalSeconds

            let getReaction s = 
                match reactions.TryFind s with 
                //| Some r -> r |> List.rev |> List.fold (fun acc (s, e) -> acc + e) ""
                | Some r -> r |> List.rev |> List.map (fun (_, e) -> e) |> String.concat ""
                | None -> ""

            let a = 
                allSubst
                |> List.map (fun s -> "\n" + (substComment s) +  "            [|\n" + (getReaction s) + "            |]\n            |> Array.fold (fun acc r -> acc + r) 0.0\n")


            let t2 = DateTime.Now
            printfn "t2 = %A" t2
            printfn "t2 - t1 = %A" (t2 - t1).TotalSeconds

            let totalCode = generateTotals ()

            let sc = 
                allSubst
                |> List.filter (fun s -> not s.isFood)
                |> List.map (fun s -> "                " + (x s) + " // " + (substToString s))
                |> String.concat "\n"

            let sumCode = "        let " + xSumName + " = \n            [|\n" + sc + "\n            |]\n            |> Array.sum\n\n"


            let updateCode = 
                [ "    let update (x : array<double>) : array<double> = \n" + sumCode + "        [|" ]
                @
                a
                @
                [ "        |]\n" ]

            [ "namespace Model\n\nmodule ModelData = \n\n" + totalCode + "\n"] @ updateCode


        member model.allSubstances = allSubst
        member model.synthesis = synth
        member model.catalyticSynthesis = catSynth
        member model.ligation = lig
        member model.catalyticLigation = catLig
        member model.sedimentationDirect = sedDir
        member model.allReactions = allReac
        member model.generateCode() = generate()
