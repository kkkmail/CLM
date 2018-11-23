namespace Model
open Clm.Substances

module ModelData = 
    let seedValue = 12345
    let numberOfAminoAcids = NumberOfAminoAcids.TwoAminoAcids
    let maxPeptideLength = MaxPeptideLength.ThreeMax
    let numberOfSubstances = 85


    let getTotals (x : array<double>) = 
        [|
            // A
            (
                [|
                    x.[1] // A
                    2.0 * x.[5] // AA
                    x.[6] // AB
                    x.[7] // Aa
                    x.[8] // Ab
                    x.[9] // BA
                    x.[13] // aA
                    x.[17] // bA
                    3.0 * x.[21] // AAA
                    2.0 * x.[22] // AAB
                    2.0 * x.[23] // AAa
                    2.0 * x.[24] // AAb
                    2.0 * x.[25] // ABA
                    x.[26] // ABB
                    x.[27] // ABa
                    x.[28] // ABb
                    2.0 * x.[29] // AaA
                    x.[30] // AaB
                    x.[31] // Aaa
                    x.[32] // Aab
                    2.0 * x.[33] // AbA
                    x.[34] // AbB
                    x.[35] // Aba
                    x.[36] // Abb
                    2.0 * x.[37] // BAA
                    x.[38] // BAB
                    x.[39] // BAa
                    x.[40] // BAb
                    x.[41] // BBA
                    x.[45] // BaA
                    x.[49] // BbA
                    2.0 * x.[53] // aAA
                    x.[54] // aAB
                    x.[55] // aAa
                    x.[56] // aAb
                    x.[57] // aBA
                    x.[61] // aaA
                    x.[65] // abA
                    2.0 * x.[69] // bAA
                    x.[70] // bAB
                    x.[71] // bAa
                    x.[72] // bAb
                    x.[73] // bBA
                    x.[77] // baA
                    x.[81] // bbA
                |]
                |> Array.sum
                ,
                [|
                    x.[3] // a
                    x.[7] // Aa
                    x.[11] // Ba
                    x.[13] // aA
                    x.[14] // aB
                    2.0 * x.[15] // aa
                    x.[16] // ab
                    x.[19] // ba
                    x.[23] // AAa
                    x.[27] // ABa
                    x.[29] // AaA
                    x.[30] // AaB
                    2.0 * x.[31] // Aaa
                    x.[32] // Aab
                    x.[35] // Aba
                    x.[39] // BAa
                    x.[43] // BBa
                    x.[45] // BaA
                    x.[46] // BaB
                    2.0 * x.[47] // Baa
                    x.[48] // Bab
                    x.[51] // Bba
                    x.[53] // aAA
                    x.[54] // aAB
                    2.0 * x.[55] // aAa
                    x.[56] // aAb
                    x.[57] // aBA
                    x.[58] // aBB
                    2.0 * x.[59] // aBa
                    x.[60] // aBb
                    2.0 * x.[61] // aaA
                    2.0 * x.[62] // aaB
                    3.0 * x.[63] // aaa
                    2.0 * x.[64] // aab
                    x.[65] // abA
                    x.[66] // abB
                    2.0 * x.[67] // aba
                    x.[68] // abb
                    x.[71] // bAa
                    x.[75] // bBa
                    x.[77] // baA
                    x.[78] // baB
                    2.0 * x.[79] // baa
                    x.[80] // bab
                    x.[83] // bba
                |]
                |> Array.sum
            )

            // B
            (
                [|
                    x.[2] // B
                    x.[6] // AB
                    x.[9] // BA
                    2.0 * x.[10] // BB
                    x.[11] // Ba
                    x.[12] // Bb
                    x.[14] // aB
                    x.[18] // bB
                    x.[22] // AAB
                    x.[25] // ABA
                    2.0 * x.[26] // ABB
                    x.[27] // ABa
                    x.[28] // ABb
                    x.[30] // AaB
                    x.[34] // AbB
                    x.[37] // BAA
                    2.0 * x.[38] // BAB
                    x.[39] // BAa
                    x.[40] // BAb
                    2.0 * x.[41] // BBA
                    3.0 * x.[42] // BBB
                    2.0 * x.[43] // BBa
                    2.0 * x.[44] // BBb
                    x.[45] // BaA
                    2.0 * x.[46] // BaB
                    x.[47] // Baa
                    x.[48] // Bab
                    x.[49] // BbA
                    2.0 * x.[50] // BbB
                    x.[51] // Bba
                    x.[52] // Bbb
                    x.[54] // aAB
                    x.[57] // aBA
                    2.0 * x.[58] // aBB
                    x.[59] // aBa
                    x.[60] // aBb
                    x.[62] // aaB
                    x.[66] // abB
                    x.[70] // bAB
                    x.[73] // bBA
                    2.0 * x.[74] // bBB
                    x.[75] // bBa
                    x.[76] // bBb
                    x.[78] // baB
                    x.[82] // bbB
                |]
                |> Array.sum
                ,
                [|
                    x.[4] // b
                    x.[8] // Ab
                    x.[12] // Bb
                    x.[16] // ab
                    x.[17] // bA
                    x.[18] // bB
                    x.[19] // ba
                    2.0 * x.[20] // bb
                    x.[24] // AAb
                    x.[28] // ABb
                    x.[32] // Aab
                    x.[33] // AbA
                    x.[34] // AbB
                    x.[35] // Aba
                    2.0 * x.[36] // Abb
                    x.[40] // BAb
                    x.[44] // BBb
                    x.[48] // Bab
                    x.[49] // BbA
                    x.[50] // BbB
                    x.[51] // Bba
                    2.0 * x.[52] // Bbb
                    x.[56] // aAb
                    x.[60] // aBb
                    x.[64] // aab
                    x.[65] // abA
                    x.[66] // abB
                    x.[67] // aba
                    2.0 * x.[68] // abb
                    x.[69] // bAA
                    x.[70] // bAB
                    x.[71] // bAa
                    2.0 * x.[72] // bAb
                    x.[73] // bBA
                    x.[74] // bBB
                    x.[75] // bBa
                    2.0 * x.[76] // bBb
                    x.[77] // baA
                    x.[78] // baB
                    x.[79] // baa
                    2.0 * x.[80] // bab
                    2.0 * x.[81] // bbA
                    2.0 * x.[82] // bbB
                    2.0 * x.[83] // bba
                    3.0 * x.[84] // bbb
                |]
                |> Array.sum
            )
        |]


    let update (x : array<double>) : array<double> = 
        let xSum = x |> Array.sum
        let xSumSquared = x |> Array.map (fun e -> e * e) |> Array.sum

        [|

            // 0 - Y
            [|
                6.0 * 179.54704695951 * x.[76] * x.[42] // bBb + BBB | SedimentationDirectName: bBb + BBB -> 6 Y
                6.0 * 179.54704695951 * x.[50] * x.[84] // BbB + bbb | SedimentationDirectName: BbB + bbb -> 6 Y
                6.0 * 75.9615840450121 * x.[76] * x.[35] // bBb + Aba | SedimentationDirectName: bBb + Aba -> 6 Y
                6.0 * 75.9615840450121 * x.[50] * x.[57] // BbB + aBA | SedimentationDirectName: BbB + aBA -> 6 Y
                6.0 * 203.315239059183 * x.[82] * x.[27] // bbB + ABa | SedimentationDirectName: bbB + ABa -> 6 Y
                6.0 * 203.315239059183 * x.[44] * x.[65] // BBb + abA | SedimentationDirectName: BBb + abA -> 6 Y
                6.0 * 111.827526720811 * x.[81] * x.[40] // bbA + BAb | SedimentationDirectName: bbA + BAb -> 6 Y
                6.0 * 111.827526720811 * x.[43] * x.[78] // BBa + baB | SedimentationDirectName: BBa + baB -> 6 Y
                6.0 * 124.97743307692 * x.[84] * x.[34] // bbb + AbB | SedimentationDirectName: bbb + AbB -> 6 Y
                6.0 * 124.97743307692 * x.[42] * x.[60] // BBB + aBb | SedimentationDirectName: BBB + aBb -> 6 Y
                6.0 * 151.873530633643 * x.[83] * x.[74] // bba + bBB | SedimentationDirectName: bba + bBB -> 6 Y
                6.0 * 151.873530633643 * x.[41] * x.[52] // BBA + Bbb | SedimentationDirectName: BBA + Bbb -> 6 Y
                6.0 * 203.15272434543 * x.[58] * x.[40] // aBB + BAb | SedimentationDirectName: aBB + BAb -> 6 Y
                6.0 * 203.15272434543 * x.[36] * x.[78] // Abb + baB | SedimentationDirectName: Abb + baB -> 6 Y
                6.0 * 584.584523985377 * x.[57] * x.[47] // aBA + Baa | SedimentationDirectName: aBA + Baa -> 6 Y
                6.0 * 584.584523985377 * x.[35] * x.[69] // Aba + bAA | SedimentationDirectName: Aba + bAA -> 6 Y
                6.0 * 777.817604984622 * x.[59] * x.[25] // aBa + ABA | SedimentationDirectName: aBa + ABA -> 6 Y
                6.0 * 777.817604984622 * x.[33] * x.[67] // AbA + aba | SedimentationDirectName: AbA + aba -> 6 Y
                6.0 * 444.986243599349 * x.[55] * x.[31] // aAa + Aaa | SedimentationDirectName: aAa + Aaa -> 6 Y
                6.0 * 444.986243599349 * x.[29] * x.[53] // AaA + aAA | SedimentationDirectName: AaA + aAA -> 6 Y
                6.0 * 734.172518615685 * x.[65] * x.[32] // abA + Aab | SedimentationDirectName: abA + Aab -> 6 Y
                6.0 * 734.172518615685 * x.[27] * x.[54] // ABa + aAB | SedimentationDirectName: ABa + aAB -> 6 Y
                5.0 * 272.917476778003 * x.[17] * x.[35] // bA + Aba | SedimentationDirectName: bA + Aba -> 5 Y
                5.0 * 272.917476778003 * x.[11] * x.[57] // Ba + aBA | SedimentationDirectName: Ba + aBA -> 5 Y
                5.0 * 540.764426321166 * x.[20] * x.[54] // bb + aAB | SedimentationDirectName: bb + aAB -> 5 Y
                5.0 * 540.764426321166 * x.[10] * x.[32] // BB + Aab | SedimentationDirectName: BB + Aab -> 5 Y
                5.0 * 124.492769460629 * x.[19] * x.[38] // ba + BAB | SedimentationDirectName: ba + BAB -> 5 Y
                5.0 * 124.492769460629 * x.[9] * x.[80] // BA + bab | SedimentationDirectName: BA + bab -> 5 Y
                5.0 * 706.753181586129 * x.[19] * x.[25] // ba + ABA | SedimentationDirectName: ba + ABA -> 5 Y
                5.0 * 706.753181586129 * x.[9] * x.[67] // BA + aba | SedimentationDirectName: BA + aba -> 5 Y
                5.0 * 368.637857212933 * x.[19] * x.[71] // ba + bAa | SedimentationDirectName: ba + bAa -> 5 Y
                5.0 * 368.637857212933 * x.[9] * x.[45] // BA + BaA | SedimentationDirectName: BA + BaA -> 5 Y
                4.0 * 191.967096834449 * x.[14] * x.[11] // aB + Ba | SedimentationDirectName: aB + Ba -> 4 Y
                4.0 * 191.967096834449 * x.[8] * x.[17] // Ab + bA | SedimentationDirectName: Ab + bA -> 4 Y
                5.0 * 392.919001150608 * x.[16] * x.[80] // ab + bab | SedimentationDirectName: ab + bab -> 5 Y
                5.0 * 392.919001150608 * x.[6] * x.[38] // AB + BAB | SedimentationDirectName: AB + BAB -> 5 Y
                5.0 * 170.45126751113 * x.[16] * x.[57] // ab + aBA | SedimentationDirectName: ab + aBA -> 5 Y
                5.0 * 170.45126751113 * x.[6] * x.[35] // AB + Aba | SedimentationDirectName: AB + Aba -> 5 Y
                3.0 * 203.042514176972 * x.[4] * x.[10] // b + BB | SedimentationDirectName: b + BB -> 3 Y
                3.0 * 203.042514176972 * x.[2] * x.[20] // B + bb | SedimentationDirectName: B + bb -> 3 Y
                2.0 * 45.0310328441251 * x.[4] * x.[1] // b + A | SedimentationDirectName: b + A -> 2 Y
                2.0 * 45.0310328441251 * x.[2] * x.[3] // B + a | SedimentationDirectName: B + a -> 2 Y
                4.0 * 320.69030234827 * x.[3] * x.[42] // a + BBB | SedimentationDirectName: a + BBB -> 4 Y
                4.0 * 320.69030234827 * x.[1] * x.[84] // A + bbb | SedimentationDirectName: A + bbb -> 4 Y
                4.0 * 18.0423188083245 * x.[3] * x.[37] // a + BAA | SedimentationDirectName: a + BAA -> 4 Y
                4.0 * 18.0423188083245 * x.[1] * x.[79] // A + baa | SedimentationDirectName: A + baa -> 4 Y
                9.66868671107511E-06 * x.[4] // b | SynthesisName: Y <-> b
                -9.6686867110751E-05 * x.[0] // Y | SynthesisName: Y <-> b
                9.66868671107511E-06 * x.[2] // B | SynthesisName: Y <-> B
                -9.6686867110751E-05 * x.[0] // Y | SynthesisName: Y <-> B
                2.4793709127602E-05 * x.[3] // a | SynthesisName: Y <-> a
                -0.00024793709127602 * x.[0] // Y | SynthesisName: Y <-> a
                2.4793709127602E-05 * x.[1] // A | SynthesisName: Y <-> A
                -0.00024793709127602 * x.[0] // Y | SynthesisName: Y <-> A
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 1 - A
            [|
                -45.0310328441251 * x.[4] * x.[1] // b + A | SedimentationDirectName: b + A -> 2 Y
                -320.69030234827 * x.[1] * x.[84] // A + bbb | SedimentationDirectName: A + bbb -> 4 Y
                -18.0423188083245 * x.[1] * x.[79] // A + baa | SedimentationDirectName: A + baa -> 4 Y
                -2.4793709127602E-05 * x.[1] // A | SynthesisName: Y <-> A
                0.00024793709127602 * x.[0] // Y | SynthesisName: Y <-> A
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 2 - B
            [|
                -203.042514176972 * x.[2] * x.[20] // B + bb | SedimentationDirectName: B + bb -> 3 Y
                -45.0310328441251 * x.[2] * x.[3] // B + a | SedimentationDirectName: B + a -> 2 Y
                -9.66868671107511E-06 * x.[2] // B | SynthesisName: Y <-> B
                9.6686867110751E-05 * x.[0] // Y | SynthesisName: Y <-> B
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 3 - a
            [|
                -45.0310328441251 * x.[2] * x.[3] // B + a | SedimentationDirectName: B + a -> 2 Y
                -320.69030234827 * x.[3] * x.[42] // a + BBB | SedimentationDirectName: a + BBB -> 4 Y
                -18.0423188083245 * x.[3] * x.[37] // a + BAA | SedimentationDirectName: a + BAA -> 4 Y
                -2.4793709127602E-05 * x.[3] // a | SynthesisName: Y <-> a
                0.00024793709127602 * x.[0] // Y | SynthesisName: Y <-> a
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 4 - b
            [|
                -203.042514176972 * x.[4] * x.[10] // b + BB | SedimentationDirectName: b + BB -> 3 Y
                -45.0310328441251 * x.[4] * x.[1] // b + A | SedimentationDirectName: b + A -> 2 Y
                -9.66868671107511E-06 * x.[4] // b | SynthesisName: Y <-> b
                9.6686867110751E-05 * x.[0] // Y | SynthesisName: Y <-> b
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 5 - AA
            [|
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 6 - AB
            [|
                -392.919001150608 * x.[6] * x.[38] // AB + BAB | SedimentationDirectName: AB + BAB -> 5 Y
                -170.45126751113 * x.[6] * x.[35] // AB + Aba | SedimentationDirectName: AB + Aba -> 5 Y
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 7 - Aa
            [|
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 8 - Ab
            [|
                -191.967096834449 * x.[8] * x.[17] // Ab + bA | SedimentationDirectName: Ab + bA -> 4 Y
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 9 - BA
            [|
                -124.492769460629 * x.[9] * x.[80] // BA + bab | SedimentationDirectName: BA + bab -> 5 Y
                -706.753181586129 * x.[9] * x.[67] // BA + aba | SedimentationDirectName: BA + aba -> 5 Y
                -368.637857212933 * x.[9] * x.[45] // BA + BaA | SedimentationDirectName: BA + BaA -> 5 Y
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 10 - BB
            [|
                -540.764426321166 * x.[10] * x.[32] // BB + Aab | SedimentationDirectName: BB + Aab -> 5 Y
                -203.042514176972 * x.[4] * x.[10] // b + BB | SedimentationDirectName: b + BB -> 3 Y
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 11 - Ba
            [|
                -272.917476778003 * x.[11] * x.[57] // Ba + aBA | SedimentationDirectName: Ba + aBA -> 5 Y
                -191.967096834449 * x.[14] * x.[11] // aB + Ba | SedimentationDirectName: aB + Ba -> 4 Y
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 12 - Bb
            [|
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 13 - aA
            [|
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 14 - aB
            [|
                -191.967096834449 * x.[14] * x.[11] // aB + Ba | SedimentationDirectName: aB + Ba -> 4 Y
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 15 - aa
            [|
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 16 - ab
            [|
                -392.919001150608 * x.[16] * x.[80] // ab + bab | SedimentationDirectName: ab + bab -> 5 Y
                -170.45126751113 * x.[16] * x.[57] // ab + aBA | SedimentationDirectName: ab + aBA -> 5 Y
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 17 - bA
            [|
                -272.917476778003 * x.[17] * x.[35] // bA + Aba | SedimentationDirectName: bA + Aba -> 5 Y
                -191.967096834449 * x.[8] * x.[17] // Ab + bA | SedimentationDirectName: Ab + bA -> 4 Y
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 18 - bB
            [|
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 19 - ba
            [|
                -124.492769460629 * x.[19] * x.[38] // ba + BAB | SedimentationDirectName: ba + BAB -> 5 Y
                -706.753181586129 * x.[19] * x.[25] // ba + ABA | SedimentationDirectName: ba + ABA -> 5 Y
                -368.637857212933 * x.[19] * x.[71] // ba + bAa | SedimentationDirectName: ba + bAa -> 5 Y
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 20 - bb
            [|
                -540.764426321166 * x.[20] * x.[54] // bb + aAB | SedimentationDirectName: bb + aAB -> 5 Y
                -203.042514176972 * x.[2] * x.[20] // B + bb | SedimentationDirectName: B + bb -> 3 Y
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 21 - AAA
            [|
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 22 - AAB
            [|
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 23 - AAa
            [|
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 24 - AAb
            [|
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 25 - ABA
            [|
                -777.817604984622 * x.[59] * x.[25] // aBa + ABA | SedimentationDirectName: aBa + ABA -> 6 Y
                -706.753181586129 * x.[19] * x.[25] // ba + ABA | SedimentationDirectName: ba + ABA -> 5 Y
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 26 - ABB
            [|
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 27 - ABa
            [|
                -203.315239059183 * x.[82] * x.[27] // bbB + ABa | SedimentationDirectName: bbB + ABa -> 6 Y
                -734.172518615685 * x.[27] * x.[54] // ABa + aAB | SedimentationDirectName: ABa + aAB -> 6 Y
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 28 - ABb
            [|
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 29 - AaA
            [|
                -444.986243599349 * x.[29] * x.[53] // AaA + aAA | SedimentationDirectName: AaA + aAA -> 6 Y
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 30 - AaB
            [|
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 31 - Aaa
            [|
                -444.986243599349 * x.[55] * x.[31] // aAa + Aaa | SedimentationDirectName: aAa + Aaa -> 6 Y
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 32 - Aab
            [|
                -734.172518615685 * x.[65] * x.[32] // abA + Aab | SedimentationDirectName: abA + Aab -> 6 Y
                -540.764426321166 * x.[10] * x.[32] // BB + Aab | SedimentationDirectName: BB + Aab -> 5 Y
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 33 - AbA
            [|
                -777.817604984622 * x.[33] * x.[67] // AbA + aba | SedimentationDirectName: AbA + aba -> 6 Y
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 34 - AbB
            [|
                -124.97743307692 * x.[84] * x.[34] // bbb + AbB | SedimentationDirectName: bbb + AbB -> 6 Y
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 35 - Aba
            [|
                -75.9615840450121 * x.[76] * x.[35] // bBb + Aba | SedimentationDirectName: bBb + Aba -> 6 Y
                -584.584523985377 * x.[35] * x.[69] // Aba + bAA | SedimentationDirectName: Aba + bAA -> 6 Y
                -272.917476778003 * x.[17] * x.[35] // bA + Aba | SedimentationDirectName: bA + Aba -> 5 Y
                -170.45126751113 * x.[6] * x.[35] // AB + Aba | SedimentationDirectName: AB + Aba -> 5 Y
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 36 - Abb
            [|
                -203.15272434543 * x.[36] * x.[78] // Abb + baB | SedimentationDirectName: Abb + baB -> 6 Y
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 37 - BAA
            [|
                -18.0423188083245 * x.[3] * x.[37] // a + BAA | SedimentationDirectName: a + BAA -> 4 Y
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 38 - BAB
            [|
                -124.492769460629 * x.[19] * x.[38] // ba + BAB | SedimentationDirectName: ba + BAB -> 5 Y
                -392.919001150608 * x.[6] * x.[38] // AB + BAB | SedimentationDirectName: AB + BAB -> 5 Y
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 39 - BAa
            [|
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 40 - BAb
            [|
                -111.827526720811 * x.[81] * x.[40] // bbA + BAb | SedimentationDirectName: bbA + BAb -> 6 Y
                -203.15272434543 * x.[58] * x.[40] // aBB + BAb | SedimentationDirectName: aBB + BAb -> 6 Y
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 41 - BBA
            [|
                -151.873530633643 * x.[41] * x.[52] // BBA + Bbb | SedimentationDirectName: BBA + Bbb -> 6 Y
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 42 - BBB
            [|
                -179.54704695951 * x.[76] * x.[42] // bBb + BBB | SedimentationDirectName: bBb + BBB -> 6 Y
                -124.97743307692 * x.[42] * x.[60] // BBB + aBb | SedimentationDirectName: BBB + aBb -> 6 Y
                -320.69030234827 * x.[3] * x.[42] // a + BBB | SedimentationDirectName: a + BBB -> 4 Y
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 43 - BBa
            [|
                -111.827526720811 * x.[43] * x.[78] // BBa + baB | SedimentationDirectName: BBa + baB -> 6 Y
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 44 - BBb
            [|
                -203.315239059183 * x.[44] * x.[65] // BBb + abA | SedimentationDirectName: BBb + abA -> 6 Y
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 45 - BaA
            [|
                -368.637857212933 * x.[9] * x.[45] // BA + BaA | SedimentationDirectName: BA + BaA -> 5 Y
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 46 - BaB
            [|
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 47 - Baa
            [|
                -584.584523985377 * x.[57] * x.[47] // aBA + Baa | SedimentationDirectName: aBA + Baa -> 6 Y
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 48 - Bab
            [|
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 49 - BbA
            [|
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 50 - BbB
            [|
                -179.54704695951 * x.[50] * x.[84] // BbB + bbb | SedimentationDirectName: BbB + bbb -> 6 Y
                -75.9615840450121 * x.[50] * x.[57] // BbB + aBA | SedimentationDirectName: BbB + aBA -> 6 Y
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 51 - Bba
            [|
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 52 - Bbb
            [|
                -151.873530633643 * x.[41] * x.[52] // BBA + Bbb | SedimentationDirectName: BBA + Bbb -> 6 Y
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 53 - aAA
            [|
                -444.986243599349 * x.[29] * x.[53] // AaA + aAA | SedimentationDirectName: AaA + aAA -> 6 Y
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 54 - aAB
            [|
                -734.172518615685 * x.[27] * x.[54] // ABa + aAB | SedimentationDirectName: ABa + aAB -> 6 Y
                -540.764426321166 * x.[20] * x.[54] // bb + aAB | SedimentationDirectName: bb + aAB -> 5 Y
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 55 - aAa
            [|
                -444.986243599349 * x.[55] * x.[31] // aAa + Aaa | SedimentationDirectName: aAa + Aaa -> 6 Y
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 56 - aAb
            [|
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 57 - aBA
            [|
                -75.9615840450121 * x.[50] * x.[57] // BbB + aBA | SedimentationDirectName: BbB + aBA -> 6 Y
                -584.584523985377 * x.[57] * x.[47] // aBA + Baa | SedimentationDirectName: aBA + Baa -> 6 Y
                -272.917476778003 * x.[11] * x.[57] // Ba + aBA | SedimentationDirectName: Ba + aBA -> 5 Y
                -170.45126751113 * x.[16] * x.[57] // ab + aBA | SedimentationDirectName: ab + aBA -> 5 Y
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 58 - aBB
            [|
                -203.15272434543 * x.[58] * x.[40] // aBB + BAb | SedimentationDirectName: aBB + BAb -> 6 Y
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 59 - aBa
            [|
                -777.817604984622 * x.[59] * x.[25] // aBa + ABA | SedimentationDirectName: aBa + ABA -> 6 Y
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 60 - aBb
            [|
                -124.97743307692 * x.[42] * x.[60] // BBB + aBb | SedimentationDirectName: BBB + aBb -> 6 Y
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 61 - aaA
            [|
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 62 - aaB
            [|
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 63 - aaa
            [|
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 64 - aab
            [|
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 65 - abA
            [|
                -203.315239059183 * x.[44] * x.[65] // BBb + abA | SedimentationDirectName: BBb + abA -> 6 Y
                -734.172518615685 * x.[65] * x.[32] // abA + Aab | SedimentationDirectName: abA + Aab -> 6 Y
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 66 - abB
            [|
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 67 - aba
            [|
                -777.817604984622 * x.[33] * x.[67] // AbA + aba | SedimentationDirectName: AbA + aba -> 6 Y
                -706.753181586129 * x.[9] * x.[67] // BA + aba | SedimentationDirectName: BA + aba -> 5 Y
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 68 - abb
            [|
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 69 - bAA
            [|
                -584.584523985377 * x.[35] * x.[69] // Aba + bAA | SedimentationDirectName: Aba + bAA -> 6 Y
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 70 - bAB
            [|
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 71 - bAa
            [|
                -368.637857212933 * x.[19] * x.[71] // ba + bAa | SedimentationDirectName: ba + bAa -> 5 Y
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 72 - bAb
            [|
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 73 - bBA
            [|
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 74 - bBB
            [|
                -151.873530633643 * x.[83] * x.[74] // bba + bBB | SedimentationDirectName: bba + bBB -> 6 Y
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 75 - bBa
            [|
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 76 - bBb
            [|
                -179.54704695951 * x.[76] * x.[42] // bBb + BBB | SedimentationDirectName: bBb + BBB -> 6 Y
                -75.9615840450121 * x.[76] * x.[35] // bBb + Aba | SedimentationDirectName: bBb + Aba -> 6 Y
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 77 - baA
            [|
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 78 - baB
            [|
                -111.827526720811 * x.[43] * x.[78] // BBa + baB | SedimentationDirectName: BBa + baB -> 6 Y
                -203.15272434543 * x.[36] * x.[78] // Abb + baB | SedimentationDirectName: Abb + baB -> 6 Y
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 79 - baa
            [|
                -18.0423188083245 * x.[1] * x.[79] // A + baa | SedimentationDirectName: A + baa -> 4 Y
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 80 - bab
            [|
                -124.492769460629 * x.[9] * x.[80] // BA + bab | SedimentationDirectName: BA + bab -> 5 Y
                -392.919001150608 * x.[16] * x.[80] // ab + bab | SedimentationDirectName: ab + bab -> 5 Y
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 81 - bbA
            [|
                -111.827526720811 * x.[81] * x.[40] // bbA + BAb | SedimentationDirectName: bbA + BAb -> 6 Y
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 82 - bbB
            [|
                -203.315239059183 * x.[82] * x.[27] // bbB + ABa | SedimentationDirectName: bbB + ABa -> 6 Y
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 83 - bba
            [|
                -151.873530633643 * x.[83] * x.[74] // bba + bBB | SedimentationDirectName: bba + bBB -> 6 Y
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 84 - bbb
            [|
                -179.54704695951 * x.[50] * x.[84] // BbB + bbb | SedimentationDirectName: BbB + bbb -> 6 Y
                -124.97743307692 * x.[84] * x.[34] // bbb + AbB | SedimentationDirectName: bbb + AbB -> 6 Y
                -320.69030234827 * x.[1] * x.[84] // A + bbb | SedimentationDirectName: A + bbb -> 4 Y
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0

        |]

