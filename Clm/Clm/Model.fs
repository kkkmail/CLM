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
            seedValue : int option
            numberOfAminoAcids : NumberOfAminoAcids
            maxPeptideLength : MaxPeptideLength
            reactionRates : List<ReactionName * ReactionRateProvider>
        }


    type ClmModel (modelParams : ModelParams) = 

        /// As of 20181122 F# still has a problem with a new line.
        let nl = "\r\n"

        let seedValue = 
            match modelParams.seedValue with 
            | Some s -> s
            | None -> 
                let r = new Random()
                r.Next()

        let rateProviders = modelParams.reactionRates |> Map.ofList
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
            [ Substance.food ]
            @
            (chiralAminoAcids |> List.map (fun a -> Chiral a))
            @
            (peptides |> List.map (fun p -> PeptideChain p))


        let allInd = allSubst |> List.mapi (fun i s -> (s, i)) |> Map.ofList


        let allNamesMap = 
            allSubst
            |> List.map (fun s -> s, s.name)
            |> Map.ofList


        let createReactions c n l = 
            match rateProviders.TryFind n with
            | Some g -> 
                let create a = c a |> Reaction.tryCreateReaction g
                l
                |> List.map create
                |> List.choose id
                |> List.concat
            | None -> []


        let synth = createReactions SynthesisReaction.create SynthesisName aminoAcids
        let catSynth = createReactions CatalyticSynthesisReaction.create CatalyticSynthesisName (List.allPairs aminoAcids synthCatalysts)
        let lig = createReactions LigationReaction.create LigationName ligationPairs
        let catLig = createReactions CatalyticLigationReaction.create CatalyticLigationName (List.allPairs ligationPairs ligCatalysts)
        let sedDir = createReactions SedimentationDirectReaction.create SedimentationDirectName allPairs


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
        let xSumSquaredName = "xSumSquared"
        let substComment (s : Substance) = "            // " + (allInd.[s]).ToString() + " - " + (substToString s) + nl
        //let reactionComment (r : Reaction) = " // " + (reactToString r) + nl
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
            (r.ToString() |> toFloat) + " * " + a + " // " + (lstToString l) // + nl

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
                    |> List.map (fun (s, n) -> (s, (shift + iSign + (toMult n) + fwd + " | " + rc + nl)))
                )
                @
                (
                    o
                    |> List.filter (fun (s, _) -> catalysts.Contains s |> not)
                    |> List.map (fun (s, n) -> (s, (shift + oSign + (toMult n) + fwd+ " | " + rc + nl)))
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
                let a = v |> String.concat nl
                "                [|" + nl + a + nl + "                |]" + nl + "                |> Array.sum" + nl

            let gg1 ((a : AminoAcid), l, r) = 
                "            // " + a.name + nl + "            (" + nl + (gg l) + "                ," + nl + (gg r) + "            )" + nl

            let y =
                aminoAcids
                |> List.map (fun a -> a, L a |> g, R a |> g)
                |> List.map (fun (a, l, r) -> gg1 (a, l, r))

            let x =
                y
                |> String.concat nl

            "    let getTotals (x : array<double>) = " + nl +
            "        [|" + nl +
            x +
            "        |]" + nl


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
                |> List.map (fun s -> "" + nl + (substComment s) +  "            [|" + nl + (getReaction s) + "            |]" + nl + "            |> Array.fold (fun acc r -> acc + r) 0.0" + nl)


            let t2 = DateTime.Now
            printfn "t2 = %A" t2
            printfn "t2 - t1 = %A" (t2 - t1).TotalSeconds

            let totalCode = generateTotals ()

            //let sc = 
            //    allSubst
            //    |> List.filter (fun s -> not s.isFood)
            //    |> List.map (fun s -> "                " + (x s) + " // " + (substToString s))
            //    |> String.concat "" + nl

            let sumCode = "        let " + xSumName + " = " + xName + " |> Array.sum" + nl
            let sumSquaredCode = "        let " + xSumSquaredName + " = " + xName + " |> Array.map (fun e -> e * e) |> Array.sum" + nl + nl


            let updateCode = 
                [ "    let update (x : array<double>) : array<double> = " + nl + sumCode + sumSquaredCode + "        [|" ]
                @
                a
                @
                [ "        |]" + nl ]

            let paramCode = 
                "    let seedValue = " + seedValue.ToString() + nl + 
                "    let numberOfAminoAcids = NumberOfAminoAcids." + (modelParams.numberOfAminoAcids.ToString()) + nl + 
                "    let maxPeptideLength = MaxPeptideLength." + (modelParams.maxPeptideLength.ToString()) + nl + nl

            [ "namespace Model" + nl + "open Clm.Substances" + nl + nl + "module ModelData = " + nl + paramCode + nl + totalCode + nl] @ updateCode


        member model.allSubstances = allSubst
        member model.synthesis = synth
        member model.catalyticSynthesis = catSynth
        member model.ligation = lig
        member model.catalyticLigation = catLig
        member model.sedimentationDirect = sedDir
        member model.allReactions = allReac
        member model.generateCode() = generate()
