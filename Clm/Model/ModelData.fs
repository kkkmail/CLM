namespace Model
open Clm.Substances

module ModelData = 
    let numberOfAminoAcids = NumberOfAminoAcids.TwoAminoAcids
    let maxPeptideLength = MaxPeptideLength.ThreeMax


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
                6.0 * 408.716301394858 * x.[74] * x.[41] // bBB + BBA | SedimentationDirectName: bBB + BBA -> 6 Y
                6.0 * 408.716301394858 * x.[52] * x.[83] // Bbb + bba | SedimentationDirectName: Bbb + bba -> 6 Y
                6.0 * 608.680453766052 * x.[70] * x.[30] // bAB + AaB | SedimentationDirectName: bAB + AaB -> 6 Y
                6.0 * 608.680453766052 * x.[48] * x.[56] // Bab + aAb | SedimentationDirectName: Bab + aAb -> 6 Y
                6.0 * 341.582113491609 * x.[72] * x.[73] // bAb + bBA | SedimentationDirectName: bAb + bBA -> 6 Y
                6.0 * 341.582113491609 * x.[46] * x.[51] // BaB + Bba | SedimentationDirectName: BaB + Bba -> 6 Y
                6.0 * 35.8042021208086 * x.[72] * x.[70] // bAb + bAB | SedimentationDirectName: bAb + bAB -> 6 Y
                6.0 * 35.8042021208086 * x.[46] * x.[48] // BaB + Bab | SedimentationDirectName: BaB + Bab -> 6 Y
                6.0 * 305.045232456692 * x.[84] * x.[39] // bbb + BAa | SedimentationDirectName: bbb + BAa -> 6 Y
                6.0 * 305.045232456692 * x.[42] * x.[77] // BBB + baA | SedimentationDirectName: BBB + baA -> 6 Y
                6.0 * 67.6503705387438 * x.[83] * x.[27] // bba + ABa | SedimentationDirectName: bba + ABa -> 6 Y
                6.0 * 67.6503705387438 * x.[41] * x.[65] // BBA + abA | SedimentationDirectName: BBA + abA -> 6 Y
                6.0 * 24.4573733175658 * x.[80] * x.[78] // bab + baB | SedimentationDirectName: bab + baB -> 6 Y
                6.0 * 24.4573733175658 * x.[38] * x.[40] // BAB + BAb | SedimentationDirectName: BAB + BAb -> 6 Y
                6.0 * 503.865603997812 * x.[80] * x.[77] // bab + baA | SedimentationDirectName: bab + baA -> 6 Y
                6.0 * 503.865603997812 * x.[38] * x.[39] // BAB + BAa | SedimentationDirectName: BAB + BAa -> 6 Y
                6.0 * 716.711788725713 * x.[79] * x.[50] // baa + BbB | SedimentationDirectName: baa + BbB -> 6 Y
                6.0 * 716.711788725713 * x.[37] * x.[76] // BAA + bBb | SedimentationDirectName: BAA + bBb -> 6 Y
                6.0 * 265.537114234532 * x.[58] * x.[35] // aBB + Aba | SedimentationDirectName: aBB + Aba -> 6 Y
                6.0 * 265.537114234532 * x.[36] * x.[57] // Abb + aBA | SedimentationDirectName: Abb + aBA -> 6 Y
                6.0 * 209.617910062433 * x.[59] * x.[74] // aBa + bBB | SedimentationDirectName: aBa + bBB -> 6 Y
                6.0 * 209.617910062433 * x.[33] * x.[52] // AbA + Bbb | SedimentationDirectName: AbA + Bbb -> 6 Y
                6.0 * 466.811836506603 * x.[54] * x.[48] // aAB + Bab | SedimentationDirectName: aAB + Bab -> 6 Y
                6.0 * 466.811836506603 * x.[32] * x.[70] // Aab + bAB | SedimentationDirectName: Aab + bAB -> 6 Y
                6.0 * 284.258204211213 * x.[53] * x.[25] // aAA + ABA | SedimentationDirectName: aAA + ABA -> 6 Y
                6.0 * 284.258204211213 * x.[31] * x.[67] // Aaa + aba | SedimentationDirectName: Aaa + aba -> 6 Y
                6.0 * 443.411977513777 * x.[53] * x.[81] // aAA + bbA | SedimentationDirectName: aAA + bbA -> 6 Y
                6.0 * 443.411977513777 * x.[31] * x.[43] // Aaa + BBa | SedimentationDirectName: Aaa + BBa -> 6 Y
                6.0 * 125.454054605852 * x.[66] * x.[79] // abB + baa | SedimentationDirectName: abB + baa -> 6 Y
                6.0 * 125.454054605852 * x.[28] * x.[37] // ABb + BAA | SedimentationDirectName: ABb + BAA -> 6 Y
                6.0 * 59.787474785308 * x.[66] * x.[53] // abB + aAA | SedimentationDirectName: abB + aAA -> 6 Y
                6.0 * 59.787474785308 * x.[28] * x.[31] // ABb + Aaa | SedimentationDirectName: ABb + Aaa -> 6 Y
                6.0 * 786.866005568544 * x.[68] * x.[49] // abb + BbA | SedimentationDirectName: abb + BbA -> 6 Y
                6.0 * 786.866005568544 * x.[26] * x.[75] // ABB + bBa | SedimentationDirectName: ABB + bBa -> 6 Y
                6.0 * 656.891126246687 * x.[62] * x.[47] // aaB + Baa | SedimentationDirectName: aaB + Baa -> 6 Y
                6.0 * 656.891126246687 * x.[24] * x.[69] // AAb + bAA | SedimentationDirectName: AAb + bAA -> 6 Y
                6.0 * 417.882902705259 * x.[62] * x.[22] // aaB + AAB | SedimentationDirectName: aaB + AAB -> 6 Y
                6.0 * 417.882902705259 * x.[24] * x.[64] // AAb + aab | SedimentationDirectName: AAb + aab -> 6 Y
                6.0 * 146.322224924238 * x.[64] * x.[59] // aab + aBa | SedimentationDirectName: aab + aBa -> 6 Y
                6.0 * 146.322224924238 * x.[22] * x.[33] // AAB + AbA | SedimentationDirectName: AAB + AbA -> 6 Y
                6.0 * 299.945151250537 * x.[64] * x.[67] // aab + aba | SedimentationDirectName: aab + aba -> 6 Y
                6.0 * 299.945151250537 * x.[22] * x.[25] // AAB + ABA | SedimentationDirectName: AAB + ABA -> 6 Y
                5.0 * 522.024739919589 * x.[19] * x.[75] // ba + bBa | SedimentationDirectName: ba + bBa -> 5 Y
                5.0 * 522.024739919589 * x.[9] * x.[49] // BA + BbA | SedimentationDirectName: BA + BbA -> 5 Y
                5.0 * 78.4791994822839 * x.[19] * x.[84] // ba + bbb | SedimentationDirectName: ba + bbb -> 5 Y
                5.0 * 78.4791994822839 * x.[9] * x.[42] // BA + BBB | SedimentationDirectName: BA + BBB -> 5 Y
                5.0 * 610.61001797884 * x.[13] * x.[39] // aA + BAa | SedimentationDirectName: aA + BAa -> 5 Y
                5.0 * 610.61001797884 * x.[7] * x.[77] // Aa + baA | SedimentationDirectName: Aa + baA -> 5 Y
                4.0 * 495.840454067306 * x.[13] * x.[13] // aA + aA | SedimentationDirectName: aA + aA -> 4 Y
                4.0 * 495.840454067306 * x.[7] * x.[7] // Aa + Aa | SedimentationDirectName: Aa + Aa -> 4 Y
                5.0 * 5.56934902074113 * x.[15] * x.[49] // aa + BbA | SedimentationDirectName: aa + BbA -> 5 Y
                5.0 * 5.56934902074113 * x.[5] * x.[75] // AA + bBa | SedimentationDirectName: AA + bBa -> 5 Y
                5.0 * 49.1088730009344 * x.[15] * x.[56] // aa + aAb | SedimentationDirectName: aa + aAb -> 5 Y
                5.0 * 49.1088730009344 * x.[5] * x.[30] // AA + AaB | SedimentationDirectName: AA + AaB -> 5 Y
                5.0 * 679.919499073657 * x.[15] * x.[67] // aa + aba | SedimentationDirectName: aa + aba -> 5 Y
                5.0 * 679.919499073657 * x.[5] * x.[25] // AA + ABA | SedimentationDirectName: AA + ABA -> 5 Y
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 1 - A
            [|
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 2 - B
            [|
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 3 - a
            [|
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 4 - b
            [|
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 5 - AA
            [|
                -5.56934902074113 * x.[5] * x.[75] // AA + bBa | SedimentationDirectName: AA + bBa -> 5 Y
                -49.1088730009344 * x.[5] * x.[30] // AA + AaB | SedimentationDirectName: AA + AaB -> 5 Y
                -679.919499073657 * x.[5] * x.[25] // AA + ABA | SedimentationDirectName: AA + ABA -> 5 Y
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 6 - AB
            [|
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 7 - Aa
            [|
                -610.61001797884 * x.[7] * x.[77] // Aa + baA | SedimentationDirectName: Aa + baA -> 5 Y
                -495.840454067306 * x.[7] * x.[7] // Aa + Aa | SedimentationDirectName: Aa + Aa -> 4 Y
                -495.840454067306 * x.[7] * x.[7] // Aa + Aa | SedimentationDirectName: Aa + Aa -> 4 Y
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 8 - Ab
            [|
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 9 - BA
            [|
                -522.024739919589 * x.[9] * x.[49] // BA + BbA | SedimentationDirectName: BA + BbA -> 5 Y
                -78.4791994822839 * x.[9] * x.[42] // BA + BBB | SedimentationDirectName: BA + BBB -> 5 Y
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 10 - BB
            [|
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 11 - Ba
            [|
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 12 - Bb
            [|
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 13 - aA
            [|
                -610.61001797884 * x.[13] * x.[39] // aA + BAa | SedimentationDirectName: aA + BAa -> 5 Y
                -495.840454067306 * x.[13] * x.[13] // aA + aA | SedimentationDirectName: aA + aA -> 4 Y
                -495.840454067306 * x.[13] * x.[13] // aA + aA | SedimentationDirectName: aA + aA -> 4 Y
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 14 - aB
            [|
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 15 - aa
            [|
                -5.56934902074113 * x.[15] * x.[49] // aa + BbA | SedimentationDirectName: aa + BbA -> 5 Y
                -49.1088730009344 * x.[15] * x.[56] // aa + aAb | SedimentationDirectName: aa + aAb -> 5 Y
                -679.919499073657 * x.[15] * x.[67] // aa + aba | SedimentationDirectName: aa + aba -> 5 Y
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 16 - ab
            [|
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 17 - bA
            [|
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 18 - bB
            [|
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 19 - ba
            [|
                -522.024739919589 * x.[19] * x.[75] // ba + bBa | SedimentationDirectName: ba + bBa -> 5 Y
                -78.4791994822839 * x.[19] * x.[84] // ba + bbb | SedimentationDirectName: ba + bbb -> 5 Y
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 20 - bb
            [|
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 21 - AAA
            [|
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 22 - AAB
            [|
                -417.882902705259 * x.[62] * x.[22] // aaB + AAB | SedimentationDirectName: aaB + AAB -> 6 Y
                -146.322224924238 * x.[22] * x.[33] // AAB + AbA | SedimentationDirectName: AAB + AbA -> 6 Y
                -299.945151250537 * x.[22] * x.[25] // AAB + ABA | SedimentationDirectName: AAB + ABA -> 6 Y
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 23 - AAa
            [|
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 24 - AAb
            [|
                -656.891126246687 * x.[24] * x.[69] // AAb + bAA | SedimentationDirectName: AAb + bAA -> 6 Y
                -417.882902705259 * x.[24] * x.[64] // AAb + aab | SedimentationDirectName: AAb + aab -> 6 Y
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 25 - ABA
            [|
                -284.258204211213 * x.[53] * x.[25] // aAA + ABA | SedimentationDirectName: aAA + ABA -> 6 Y
                -299.945151250537 * x.[22] * x.[25] // AAB + ABA | SedimentationDirectName: AAB + ABA -> 6 Y
                -679.919499073657 * x.[5] * x.[25] // AA + ABA | SedimentationDirectName: AA + ABA -> 5 Y
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 26 - ABB
            [|
                -786.866005568544 * x.[26] * x.[75] // ABB + bBa | SedimentationDirectName: ABB + bBa -> 6 Y
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 27 - ABa
            [|
                -67.6503705387438 * x.[83] * x.[27] // bba + ABa | SedimentationDirectName: bba + ABa -> 6 Y
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 28 - ABb
            [|
                -125.454054605852 * x.[28] * x.[37] // ABb + BAA | SedimentationDirectName: ABb + BAA -> 6 Y
                -59.787474785308 * x.[28] * x.[31] // ABb + Aaa | SedimentationDirectName: ABb + Aaa -> 6 Y
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 29 - AaA
            [|
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 30 - AaB
            [|
                -608.680453766052 * x.[70] * x.[30] // bAB + AaB | SedimentationDirectName: bAB + AaB -> 6 Y
                -49.1088730009344 * x.[5] * x.[30] // AA + AaB | SedimentationDirectName: AA + AaB -> 5 Y
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 31 - Aaa
            [|
                -284.258204211213 * x.[31] * x.[67] // Aaa + aba | SedimentationDirectName: Aaa + aba -> 6 Y
                -443.411977513777 * x.[31] * x.[43] // Aaa + BBa | SedimentationDirectName: Aaa + BBa -> 6 Y
                -59.787474785308 * x.[28] * x.[31] // ABb + Aaa | SedimentationDirectName: ABb + Aaa -> 6 Y
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 32 - Aab
            [|
                -466.811836506603 * x.[32] * x.[70] // Aab + bAB | SedimentationDirectName: Aab + bAB -> 6 Y
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 33 - AbA
            [|
                -209.617910062433 * x.[33] * x.[52] // AbA + Bbb | SedimentationDirectName: AbA + Bbb -> 6 Y
                -146.322224924238 * x.[22] * x.[33] // AAB + AbA | SedimentationDirectName: AAB + AbA -> 6 Y
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 34 - AbB
            [|
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 35 - Aba
            [|
                -265.537114234532 * x.[58] * x.[35] // aBB + Aba | SedimentationDirectName: aBB + Aba -> 6 Y
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 36 - Abb
            [|
                -265.537114234532 * x.[36] * x.[57] // Abb + aBA | SedimentationDirectName: Abb + aBA -> 6 Y
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 37 - BAA
            [|
                -716.711788725713 * x.[37] * x.[76] // BAA + bBb | SedimentationDirectName: BAA + bBb -> 6 Y
                -125.454054605852 * x.[28] * x.[37] // ABb + BAA | SedimentationDirectName: ABb + BAA -> 6 Y
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 38 - BAB
            [|
                -24.4573733175658 * x.[38] * x.[40] // BAB + BAb | SedimentationDirectName: BAB + BAb -> 6 Y
                -503.865603997812 * x.[38] * x.[39] // BAB + BAa | SedimentationDirectName: BAB + BAa -> 6 Y
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 39 - BAa
            [|
                -305.045232456692 * x.[84] * x.[39] // bbb + BAa | SedimentationDirectName: bbb + BAa -> 6 Y
                -503.865603997812 * x.[38] * x.[39] // BAB + BAa | SedimentationDirectName: BAB + BAa -> 6 Y
                -610.61001797884 * x.[13] * x.[39] // aA + BAa | SedimentationDirectName: aA + BAa -> 5 Y
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 40 - BAb
            [|
                -24.4573733175658 * x.[38] * x.[40] // BAB + BAb | SedimentationDirectName: BAB + BAb -> 6 Y
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 41 - BBA
            [|
                -408.716301394858 * x.[74] * x.[41] // bBB + BBA | SedimentationDirectName: bBB + BBA -> 6 Y
                -67.6503705387438 * x.[41] * x.[65] // BBA + abA | SedimentationDirectName: BBA + abA -> 6 Y
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 42 - BBB
            [|
                -305.045232456692 * x.[42] * x.[77] // BBB + baA | SedimentationDirectName: BBB + baA -> 6 Y
                -78.4791994822839 * x.[9] * x.[42] // BA + BBB | SedimentationDirectName: BA + BBB -> 5 Y
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 43 - BBa
            [|
                -443.411977513777 * x.[31] * x.[43] // Aaa + BBa | SedimentationDirectName: Aaa + BBa -> 6 Y
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 44 - BBb
            [|
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 45 - BaA
            [|
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 46 - BaB
            [|
                -341.582113491609 * x.[46] * x.[51] // BaB + Bba | SedimentationDirectName: BaB + Bba -> 6 Y
                -35.8042021208086 * x.[46] * x.[48] // BaB + Bab | SedimentationDirectName: BaB + Bab -> 6 Y
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 47 - Baa
            [|
                -656.891126246687 * x.[62] * x.[47] // aaB + Baa | SedimentationDirectName: aaB + Baa -> 6 Y
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 48 - Bab
            [|
                -608.680453766052 * x.[48] * x.[56] // Bab + aAb | SedimentationDirectName: Bab + aAb -> 6 Y
                -35.8042021208086 * x.[46] * x.[48] // BaB + Bab | SedimentationDirectName: BaB + Bab -> 6 Y
                -466.811836506603 * x.[54] * x.[48] // aAB + Bab | SedimentationDirectName: aAB + Bab -> 6 Y
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 49 - BbA
            [|
                -786.866005568544 * x.[68] * x.[49] // abb + BbA | SedimentationDirectName: abb + BbA -> 6 Y
                -522.024739919589 * x.[9] * x.[49] // BA + BbA | SedimentationDirectName: BA + BbA -> 5 Y
                -5.56934902074113 * x.[15] * x.[49] // aa + BbA | SedimentationDirectName: aa + BbA -> 5 Y
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 50 - BbB
            [|
                -716.711788725713 * x.[79] * x.[50] // baa + BbB | SedimentationDirectName: baa + BbB -> 6 Y
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 51 - Bba
            [|
                -341.582113491609 * x.[46] * x.[51] // BaB + Bba | SedimentationDirectName: BaB + Bba -> 6 Y
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 52 - Bbb
            [|
                -408.716301394858 * x.[52] * x.[83] // Bbb + bba | SedimentationDirectName: Bbb + bba -> 6 Y
                -209.617910062433 * x.[33] * x.[52] // AbA + Bbb | SedimentationDirectName: AbA + Bbb -> 6 Y
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 53 - aAA
            [|
                -284.258204211213 * x.[53] * x.[25] // aAA + ABA | SedimentationDirectName: aAA + ABA -> 6 Y
                -443.411977513777 * x.[53] * x.[81] // aAA + bbA | SedimentationDirectName: aAA + bbA -> 6 Y
                -59.787474785308 * x.[66] * x.[53] // abB + aAA | SedimentationDirectName: abB + aAA -> 6 Y
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 54 - aAB
            [|
                -466.811836506603 * x.[54] * x.[48] // aAB + Bab | SedimentationDirectName: aAB + Bab -> 6 Y
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 55 - aAa
            [|
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 56 - aAb
            [|
                -608.680453766052 * x.[48] * x.[56] // Bab + aAb | SedimentationDirectName: Bab + aAb -> 6 Y
                -49.1088730009344 * x.[15] * x.[56] // aa + aAb | SedimentationDirectName: aa + aAb -> 5 Y
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 57 - aBA
            [|
                -265.537114234532 * x.[36] * x.[57] // Abb + aBA | SedimentationDirectName: Abb + aBA -> 6 Y
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 58 - aBB
            [|
                -265.537114234532 * x.[58] * x.[35] // aBB + Aba | SedimentationDirectName: aBB + Aba -> 6 Y
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 59 - aBa
            [|
                -209.617910062433 * x.[59] * x.[74] // aBa + bBB | SedimentationDirectName: aBa + bBB -> 6 Y
                -146.322224924238 * x.[64] * x.[59] // aab + aBa | SedimentationDirectName: aab + aBa -> 6 Y
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 60 - aBb
            [|
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 61 - aaA
            [|
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 62 - aaB
            [|
                -656.891126246687 * x.[62] * x.[47] // aaB + Baa | SedimentationDirectName: aaB + Baa -> 6 Y
                -417.882902705259 * x.[62] * x.[22] // aaB + AAB | SedimentationDirectName: aaB + AAB -> 6 Y
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 63 - aaa
            [|
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 64 - aab
            [|
                -417.882902705259 * x.[24] * x.[64] // AAb + aab | SedimentationDirectName: AAb + aab -> 6 Y
                -146.322224924238 * x.[64] * x.[59] // aab + aBa | SedimentationDirectName: aab + aBa -> 6 Y
                -299.945151250537 * x.[64] * x.[67] // aab + aba | SedimentationDirectName: aab + aba -> 6 Y
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 65 - abA
            [|
                -67.6503705387438 * x.[41] * x.[65] // BBA + abA | SedimentationDirectName: BBA + abA -> 6 Y
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 66 - abB
            [|
                -125.454054605852 * x.[66] * x.[79] // abB + baa | SedimentationDirectName: abB + baa -> 6 Y
                -59.787474785308 * x.[66] * x.[53] // abB + aAA | SedimentationDirectName: abB + aAA -> 6 Y
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 67 - aba
            [|
                -284.258204211213 * x.[31] * x.[67] // Aaa + aba | SedimentationDirectName: Aaa + aba -> 6 Y
                -299.945151250537 * x.[64] * x.[67] // aab + aba | SedimentationDirectName: aab + aba -> 6 Y
                -679.919499073657 * x.[15] * x.[67] // aa + aba | SedimentationDirectName: aa + aba -> 5 Y
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 68 - abb
            [|
                -786.866005568544 * x.[68] * x.[49] // abb + BbA | SedimentationDirectName: abb + BbA -> 6 Y
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 69 - bAA
            [|
                -656.891126246687 * x.[24] * x.[69] // AAb + bAA | SedimentationDirectName: AAb + bAA -> 6 Y
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 70 - bAB
            [|
                -608.680453766052 * x.[70] * x.[30] // bAB + AaB | SedimentationDirectName: bAB + AaB -> 6 Y
                -35.8042021208086 * x.[72] * x.[70] // bAb + bAB | SedimentationDirectName: bAb + bAB -> 6 Y
                -466.811836506603 * x.[32] * x.[70] // Aab + bAB | SedimentationDirectName: Aab + bAB -> 6 Y
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 71 - bAa
            [|
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 72 - bAb
            [|
                -341.582113491609 * x.[72] * x.[73] // bAb + bBA | SedimentationDirectName: bAb + bBA -> 6 Y
                -35.8042021208086 * x.[72] * x.[70] // bAb + bAB | SedimentationDirectName: bAb + bAB -> 6 Y
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 73 - bBA
            [|
                -341.582113491609 * x.[72] * x.[73] // bAb + bBA | SedimentationDirectName: bAb + bBA -> 6 Y
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 74 - bBB
            [|
                -408.716301394858 * x.[74] * x.[41] // bBB + BBA | SedimentationDirectName: bBB + BBA -> 6 Y
                -209.617910062433 * x.[59] * x.[74] // aBa + bBB | SedimentationDirectName: aBa + bBB -> 6 Y
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 75 - bBa
            [|
                -786.866005568544 * x.[26] * x.[75] // ABB + bBa | SedimentationDirectName: ABB + bBa -> 6 Y
                -522.024739919589 * x.[19] * x.[75] // ba + bBa | SedimentationDirectName: ba + bBa -> 5 Y
                -5.56934902074113 * x.[5] * x.[75] // AA + bBa | SedimentationDirectName: AA + bBa -> 5 Y
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 76 - bBb
            [|
                -716.711788725713 * x.[37] * x.[76] // BAA + bBb | SedimentationDirectName: BAA + bBb -> 6 Y
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 77 - baA
            [|
                -305.045232456692 * x.[42] * x.[77] // BBB + baA | SedimentationDirectName: BBB + baA -> 6 Y
                -503.865603997812 * x.[80] * x.[77] // bab + baA | SedimentationDirectName: bab + baA -> 6 Y
                -610.61001797884 * x.[7] * x.[77] // Aa + baA | SedimentationDirectName: Aa + baA -> 5 Y
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 78 - baB
            [|
                -24.4573733175658 * x.[80] * x.[78] // bab + baB | SedimentationDirectName: bab + baB -> 6 Y
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 79 - baa
            [|
                -716.711788725713 * x.[79] * x.[50] // baa + BbB | SedimentationDirectName: baa + BbB -> 6 Y
                -125.454054605852 * x.[66] * x.[79] // abB + baa | SedimentationDirectName: abB + baa -> 6 Y
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 80 - bab
            [|
                -24.4573733175658 * x.[80] * x.[78] // bab + baB | SedimentationDirectName: bab + baB -> 6 Y
                -503.865603997812 * x.[80] * x.[77] // bab + baA | SedimentationDirectName: bab + baA -> 6 Y
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 81 - bbA
            [|
                -443.411977513777 * x.[53] * x.[81] // aAA + bbA | SedimentationDirectName: aAA + bbA -> 6 Y
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 82 - bbB
            [|
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 83 - bba
            [|
                -408.716301394858 * x.[52] * x.[83] // Bbb + bba | SedimentationDirectName: Bbb + bba -> 6 Y
                -67.6503705387438 * x.[83] * x.[27] // bba + ABa | SedimentationDirectName: bba + ABa -> 6 Y
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 84 - bbb
            [|
                -305.045232456692 * x.[84] * x.[39] // bbb + BAa | SedimentationDirectName: bbb + BAa -> 6 Y
                -78.4791994822839 * x.[19] * x.[84] // ba + bbb | SedimentationDirectName: ba + bbb -> 5 Y
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0

        |]

