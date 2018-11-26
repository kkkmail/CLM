namespace Model

open Clm.Substances

open Clm.Model


module ModelData = 

    let seedValue = 12345
    let numberOfAminoAcids = NumberOfAminoAcids.TwoAminoAcids
    let maxPeptideLength = MaxPeptideLength.ThreeMax
    let numberOfSubstances = 85
    let kW = 0.0433854974077016 / 84.0



    let getTotalSubst (x : array<double>) = 
        [|
            x.[0] // Y
            x.[1] // A
            x.[2] // B
            x.[3] // a
            x.[4] // b
            2.0 * x.[5] // AA
            2.0 * x.[6] // AB
            2.0 * x.[7] // Aa
            2.0 * x.[8] // Ab
            2.0 * x.[9] // BA
            2.0 * x.[10] // BB
            2.0 * x.[11] // Ba
            2.0 * x.[12] // Bb
            2.0 * x.[13] // aA
            2.0 * x.[14] // aB
            2.0 * x.[15] // aa
            2.0 * x.[16] // ab
            2.0 * x.[17] // bA
            2.0 * x.[18] // bB
            2.0 * x.[19] // ba
            2.0 * x.[20] // bb
            3.0 * x.[21] // AAA
            3.0 * x.[22] // AAB
            3.0 * x.[23] // AAa
            3.0 * x.[24] // AAb
            3.0 * x.[25] // ABA
            3.0 * x.[26] // ABB
            3.0 * x.[27] // ABa
            3.0 * x.[28] // ABb
            3.0 * x.[29] // AaA
            3.0 * x.[30] // AaB
            3.0 * x.[31] // Aaa
            3.0 * x.[32] // Aab
            3.0 * x.[33] // AbA
            3.0 * x.[34] // AbB
            3.0 * x.[35] // Aba
            3.0 * x.[36] // Abb
            3.0 * x.[37] // BAA
            3.0 * x.[38] // BAB
            3.0 * x.[39] // BAa
            3.0 * x.[40] // BAb
            3.0 * x.[41] // BBA
            3.0 * x.[42] // BBB
            3.0 * x.[43] // BBa
            3.0 * x.[44] // BBb
            3.0 * x.[45] // BaA
            3.0 * x.[46] // BaB
            3.0 * x.[47] // Baa
            3.0 * x.[48] // Bab
            3.0 * x.[49] // BbA
            3.0 * x.[50] // BbB
            3.0 * x.[51] // Bba
            3.0 * x.[52] // Bbb
            3.0 * x.[53] // aAA
            3.0 * x.[54] // aAB
            3.0 * x.[55] // aAa
            3.0 * x.[56] // aAb
            3.0 * x.[57] // aBA
            3.0 * x.[58] // aBB
            3.0 * x.[59] // aBa
            3.0 * x.[60] // aBb
            3.0 * x.[61] // aaA
            3.0 * x.[62] // aaB
            3.0 * x.[63] // aaa
            3.0 * x.[64] // aab
            3.0 * x.[65] // abA
            3.0 * x.[66] // abB
            3.0 * x.[67] // aba
            3.0 * x.[68] // abb
            3.0 * x.[69] // bAA
            3.0 * x.[70] // bAB
            3.0 * x.[71] // bAa
            3.0 * x.[72] // bAb
            3.0 * x.[73] // bBA
            3.0 * x.[74] // bBB
            3.0 * x.[75] // bBa
            3.0 * x.[76] // bBb
            3.0 * x.[77] // baA
            3.0 * x.[78] // baB
            3.0 * x.[79] // baa
            3.0 * x.[80] // bab
            3.0 * x.[81] // bbA
            3.0 * x.[82] // bbB
            3.0 * x.[83] // bba
            3.0 * x.[84] // bbb
        |]
         |> Array.sum



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
        let xSum = (x |> Array.sum) - x.[0]

        let xSumN = 
            [|
                1.0 * x.[1] // A
                1.0 * x.[2] // B
                1.0 * x.[3] // a
                1.0 * x.[4] // b
                2.0 * x.[5] // AA
                2.0 * x.[6] // AB
                2.0 * x.[7] // Aa
                2.0 * x.[8] // Ab
                2.0 * x.[9] // BA
                2.0 * x.[10] // BB
                2.0 * x.[11] // Ba
                2.0 * x.[12] // Bb
                2.0 * x.[13] // aA
                2.0 * x.[14] // aB
                2.0 * x.[15] // aa
                2.0 * x.[16] // ab
                2.0 * x.[17] // bA
                2.0 * x.[18] // bB
                2.0 * x.[19] // ba
                2.0 * x.[20] // bb
                3.0 * x.[21] // AAA
                3.0 * x.[22] // AAB
                3.0 * x.[23] // AAa
                3.0 * x.[24] // AAb
                3.0 * x.[25] // ABA
                3.0 * x.[26] // ABB
                3.0 * x.[27] // ABa
                3.0 * x.[28] // ABb
                3.0 * x.[29] // AaA
                3.0 * x.[30] // AaB
                3.0 * x.[31] // Aaa
                3.0 * x.[32] // Aab
                3.0 * x.[33] // AbA
                3.0 * x.[34] // AbB
                3.0 * x.[35] // Aba
                3.0 * x.[36] // Abb
                3.0 * x.[37] // BAA
                3.0 * x.[38] // BAB
                3.0 * x.[39] // BAa
                3.0 * x.[40] // BAb
                3.0 * x.[41] // BBA
                3.0 * x.[42] // BBB
                3.0 * x.[43] // BBa
                3.0 * x.[44] // BBb
                3.0 * x.[45] // BaA
                3.0 * x.[46] // BaB
                3.0 * x.[47] // Baa
                3.0 * x.[48] // Bab
                3.0 * x.[49] // BbA
                3.0 * x.[50] // BbB
                3.0 * x.[51] // Bba
                3.0 * x.[52] // Bbb
                3.0 * x.[53] // aAA
                3.0 * x.[54] // aAB
                3.0 * x.[55] // aAa
                3.0 * x.[56] // aAb
                3.0 * x.[57] // aBA
                3.0 * x.[58] // aBB
                3.0 * x.[59] // aBa
                3.0 * x.[60] // aBb
                3.0 * x.[61] // aaA
                3.0 * x.[62] // aaB
                3.0 * x.[63] // aaa
                3.0 * x.[64] // aab
                3.0 * x.[65] // abA
                3.0 * x.[66] // abB
                3.0 * x.[67] // aba
                3.0 * x.[68] // abb
                3.0 * x.[69] // bAA
                3.0 * x.[70] // bAB
                3.0 * x.[71] // bAa
                3.0 * x.[72] // bAb
                3.0 * x.[73] // bBA
                3.0 * x.[74] // bBB
                3.0 * x.[75] // bBa
                3.0 * x.[76] // bBb
                3.0 * x.[77] // baA
                3.0 * x.[78] // baB
                3.0 * x.[79] // baa
                3.0 * x.[80] // bab
                3.0 * x.[81] // bbA
                3.0 * x.[82] // bbB
                3.0 * x.[83] // bba
                3.0 * x.[84] // bbb
            |]
            |> Array.sum

        let xSumSquaredN = 
            [|
                1.0 * x.[1] * x.[1] // A
                1.0 * x.[2] * x.[2] // B
                1.0 * x.[3] * x.[3] // a
                1.0 * x.[4] * x.[4] // b
                2.0 * x.[5] * x.[5] // AA
                2.0 * x.[6] * x.[6] // AB
                2.0 * x.[7] * x.[7] // Aa
                2.0 * x.[8] * x.[8] // Ab
                2.0 * x.[9] * x.[9] // BA
                2.0 * x.[10] * x.[10] // BB
                2.0 * x.[11] * x.[11] // Ba
                2.0 * x.[12] * x.[12] // Bb
                2.0 * x.[13] * x.[13] // aA
                2.0 * x.[14] * x.[14] // aB
                2.0 * x.[15] * x.[15] // aa
                2.0 * x.[16] * x.[16] // ab
                2.0 * x.[17] * x.[17] // bA
                2.0 * x.[18] * x.[18] // bB
                2.0 * x.[19] * x.[19] // ba
                2.0 * x.[20] * x.[20] // bb
                3.0 * x.[21] * x.[21] // AAA
                3.0 * x.[22] * x.[22] // AAB
                3.0 * x.[23] * x.[23] // AAa
                3.0 * x.[24] * x.[24] // AAb
                3.0 * x.[25] * x.[25] // ABA
                3.0 * x.[26] * x.[26] // ABB
                3.0 * x.[27] * x.[27] // ABa
                3.0 * x.[28] * x.[28] // ABb
                3.0 * x.[29] * x.[29] // AaA
                3.0 * x.[30] * x.[30] // AaB
                3.0 * x.[31] * x.[31] // Aaa
                3.0 * x.[32] * x.[32] // Aab
                3.0 * x.[33] * x.[33] // AbA
                3.0 * x.[34] * x.[34] // AbB
                3.0 * x.[35] * x.[35] // Aba
                3.0 * x.[36] * x.[36] // Abb
                3.0 * x.[37] * x.[37] // BAA
                3.0 * x.[38] * x.[38] // BAB
                3.0 * x.[39] * x.[39] // BAa
                3.0 * x.[40] * x.[40] // BAb
                3.0 * x.[41] * x.[41] // BBA
                3.0 * x.[42] * x.[42] // BBB
                3.0 * x.[43] * x.[43] // BBa
                3.0 * x.[44] * x.[44] // BBb
                3.0 * x.[45] * x.[45] // BaA
                3.0 * x.[46] * x.[46] // BaB
                3.0 * x.[47] * x.[47] // Baa
                3.0 * x.[48] * x.[48] // Bab
                3.0 * x.[49] * x.[49] // BbA
                3.0 * x.[50] * x.[50] // BbB
                3.0 * x.[51] * x.[51] // Bba
                3.0 * x.[52] * x.[52] // Bbb
                3.0 * x.[53] * x.[53] // aAA
                3.0 * x.[54] * x.[54] // aAB
                3.0 * x.[55] * x.[55] // aAa
                3.0 * x.[56] * x.[56] // aAb
                3.0 * x.[57] * x.[57] // aBA
                3.0 * x.[58] * x.[58] // aBB
                3.0 * x.[59] * x.[59] // aBa
                3.0 * x.[60] * x.[60] // aBb
                3.0 * x.[61] * x.[61] // aaA
                3.0 * x.[62] * x.[62] // aaB
                3.0 * x.[63] * x.[63] // aaa
                3.0 * x.[64] * x.[64] // aab
                3.0 * x.[65] * x.[65] // abA
                3.0 * x.[66] * x.[66] // abB
                3.0 * x.[67] * x.[67] // aba
                3.0 * x.[68] * x.[68] // abb
                3.0 * x.[69] * x.[69] // bAA
                3.0 * x.[70] * x.[70] // bAB
                3.0 * x.[71] * x.[71] // bAa
                3.0 * x.[72] * x.[72] // bAb
                3.0 * x.[73] * x.[73] // bBA
                3.0 * x.[74] * x.[74] // bBB
                3.0 * x.[75] * x.[75] // bBa
                3.0 * x.[76] * x.[76] // bBb
                3.0 * x.[77] * x.[77] // baA
                3.0 * x.[78] * x.[78] // baB
                3.0 * x.[79] * x.[79] // baa
                3.0 * x.[80] * x.[80] // bab
                3.0 * x.[81] * x.[81] // bbA
                3.0 * x.[82] * x.[82] // bbB
                3.0 * x.[83] * x.[83] // bba
                3.0 * x.[84] * x.[84] // bbb
            |]
            |> Array.sum

        [|

            // 0 - Y
            [|
                kW * (2.0 * xSum * xSumN - xSumSquaredN)
                6.0 * 25.7923415732481 * x.[73] * x.[42] // bBA + BBB | SedimentationDirectName: bBA + BBB -> 6 Y
                6.0 * 25.7923415732481 * x.[51] * x.[84] // Bba + bbb | SedimentationDirectName: Bba + bbb -> 6 Y
                6.0 * 34.0491662737595 * x.[69] * x.[28] // bAA + ABb | SedimentationDirectName: bAA + ABb -> 6 Y
                6.0 * 34.0491662737595 * x.[47] * x.[66] // Baa + abB | SedimentationDirectName: Baa + abB -> 6 Y
                6.0 * 48.5001542234325 * x.[82] * x.[30] // bbB + AaB | SedimentationDirectName: bbB + AaB -> 6 Y
                6.0 * 48.5001542234325 * x.[44] * x.[56] // BBb + aAb | SedimentationDirectName: BBb + aAb -> 6 Y
                6.0 * 53.5824645556566 * x.[83] * x.[34] // bba + AbB | SedimentationDirectName: bba + AbB -> 6 Y
                6.0 * 53.5824645556566 * x.[41] * x.[60] // BBA + aBb | SedimentationDirectName: BBA + aBb -> 6 Y
                6.0 * 53.9468979738308 * x.[77] * x.[50] // baA + BbB | SedimentationDirectName: baA + BbB -> 6 Y
                6.0 * 53.9468979738308 * x.[39] * x.[76] // BAa + bBb | SedimentationDirectName: BAa + bBb -> 6 Y
                6.0 * 0.970384360209309 * x.[77] * x.[45] // baA + BaA | SedimentationDirectName: baA + BaA -> 6 Y
                6.0 * 0.970384360209309 * x.[39] * x.[71] // BAa + bAa | SedimentationDirectName: BAa + bAa -> 6 Y
                6.0 * 63.7870836148508 * x.[80] * x.[26] // bab + ABB | SedimentationDirectName: bab + ABB -> 6 Y
                6.0 * 63.7870836148508 * x.[38] * x.[68] // BAB + abb | SedimentationDirectName: BAB + abb -> 6 Y
                6.0 * 22.1393742250857 * x.[79] * x.[51] // baa + Bba | SedimentationDirectName: baa + Bba -> 6 Y
                6.0 * 22.1393742250857 * x.[37] * x.[73] // BAA + bBA | SedimentationDirectName: BAA + bBA -> 6 Y
                6.0 * 80.7994733689242 * x.[58] * x.[81] // aBB + bbA | SedimentationDirectName: aBB + bbA -> 6 Y
                6.0 * 80.7994733689242 * x.[36] * x.[43] // Abb + BBa | SedimentationDirectName: Abb + BBa -> 6 Y
                6.0 * 74.0453128255527 * x.[54] * x.[23] // aAB + AAa | SedimentationDirectName: aAB + AAa -> 6 Y
                6.0 * 74.0453128255527 * x.[32] * x.[61] // Aab + aaA | SedimentationDirectName: Aab + aaA -> 6 Y
                6.0 * 34.1421760416434 * x.[53] * x.[25] // aAA + ABA | SedimentationDirectName: aAA + ABA -> 6 Y
                6.0 * 34.1421760416434 * x.[31] * x.[67] // Aaa + aba | SedimentationDirectName: Aaa + aba -> 6 Y
                6.0 * 21.3893445489625 * x.[56] * x.[25] // aAb + ABA | SedimentationDirectName: aAb + ABA -> 6 Y
                6.0 * 21.3893445489625 * x.[30] * x.[67] // AaB + aba | SedimentationDirectName: AaB + aba -> 6 Y
                6.0 * 82.6337629450791 * x.[56] * x.[34] // aAb + AbB | SedimentationDirectName: aAb + AbB -> 6 Y
                6.0 * 82.6337629450791 * x.[30] * x.[60] // AaB + aBb | SedimentationDirectName: AaB + aBb -> 6 Y
                6.0 * 29.7789028491576 * x.[66] * x.[30] // abB + AaB | SedimentationDirectName: abB + AaB -> 6 Y
                6.0 * 29.7789028491576 * x.[28] * x.[56] // ABb + aAb | SedimentationDirectName: ABb + aAb -> 6 Y
                6.0 * 7.14868381414817 * x.[68] * x.[72] // abb + bAb | SedimentationDirectName: abb + bAb -> 6 Y
                6.0 * 7.14868381414817 * x.[26] * x.[46] // ABB + BaB | SedimentationDirectName: ABB + BaB -> 6 Y
                6.0 * 21.091020549205 * x.[67] * x.[47] // aba + Baa | SedimentationDirectName: aba + Baa -> 6 Y
                6.0 * 21.091020549205 * x.[25] * x.[69] // ABA + bAA | SedimentationDirectName: ABA + bAA -> 6 Y
                6.0 * 12.558847104467 * x.[67] * x.[27] // aba + ABa | SedimentationDirectName: aba + ABa -> 6 Y
                6.0 * 12.558847104467 * x.[25] * x.[65] // ABA + abA | SedimentationDirectName: ABA + abA -> 6 Y
                6.0 * 3.21393117441257 * x.[67] * x.[21] // aba + AAA | SedimentationDirectName: aba + AAA -> 6 Y
                6.0 * 3.21393117441257 * x.[25] * x.[63] // ABA + aaa | SedimentationDirectName: ABA + aaa -> 6 Y
                6.0 * 32.5643983434404 * x.[62] * x.[58] // aaB + aBB | SedimentationDirectName: aaB + aBB -> 6 Y
                6.0 * 32.5643983434404 * x.[24] * x.[36] // AAb + Abb | SedimentationDirectName: AAb + Abb -> 6 Y
                5.0 * 38.5573268466612 * x.[17] * x.[40] // bA + BAb | SedimentationDirectName: bA + BAb -> 5 Y
                5.0 * 38.5573268466612 * x.[11] * x.[78] // Ba + baB | SedimentationDirectName: Ba + baB -> 5 Y
                5.0 * 66.4258069092912 * x.[17] * x.[79] // bA + baa | SedimentationDirectName: bA + baa -> 5 Y
                5.0 * 66.4258069092912 * x.[11] * x.[37] // Ba + BAA | SedimentationDirectName: Ba + BAA -> 5 Y
                5.0 * 21.5306142530327 * x.[19] * x.[37] // ba + BAA | SedimentationDirectName: ba + BAA -> 5 Y
                5.0 * 21.5306142530327 * x.[9] * x.[79] // BA + baa | SedimentationDirectName: BA + baa -> 5 Y
                5.0 * 47.2125087225556 * x.[14] * x.[65] // aB + abA | SedimentationDirectName: aB + abA -> 5 Y
                5.0 * 47.2125087225556 * x.[8] * x.[27] // Ab + ABa | SedimentationDirectName: Ab + ABa -> 5 Y
                5.0 * 3.19070828671931 * x.[13] * x.[55] // aA + aAa | SedimentationDirectName: aA + aAa -> 5 Y
                5.0 * 3.19070828671931 * x.[7] * x.[29] // Aa + AaA | SedimentationDirectName: Aa + AaA -> 5 Y
                5.0 * 74.8402332401258 * x.[13] * x.[67] // aA + aba | SedimentationDirectName: aA + aba -> 5 Y
                5.0 * 74.8402332401258 * x.[7] * x.[25] // Aa + ABA | SedimentationDirectName: Aa + ABA -> 5 Y
                5.0 * 69.5562059032595 * x.[16] * x.[62] // ab + aaB | SedimentationDirectName: ab + aaB -> 5 Y
                5.0 * 69.5562059032595 * x.[6] * x.[24] // AB + AAb | SedimentationDirectName: AB + AAb -> 5 Y
                5.0 * 15.8725584321517 * x.[15] * x.[42] // aa + BBB | SedimentationDirectName: aa + BBB -> 5 Y
                5.0 * 15.8725584321517 * x.[5] * x.[84] // AA + bbb | SedimentationDirectName: AA + bbb -> 5 Y
                5.0 * 25.9563678497145 * x.[15] * x.[41] // aa + BBA | SedimentationDirectName: aa + BBA -> 5 Y
                5.0 * 25.9563678497145 * x.[5] * x.[83] // AA + bba | SedimentationDirectName: AA + bba -> 5 Y
                5.0 * 55.9295153352086 * x.[15] * x.[35] // aa + Aba | SedimentationDirectName: aa + Aba -> 5 Y
                5.0 * 55.9295153352086 * x.[5] * x.[57] // AA + aBA | SedimentationDirectName: AA + aBA -> 5 Y
                0.001 * x.[4] // b | SynthesisName: Y <-> b
                -0.01 * x.[0] // Y | SynthesisName: Y <-> b
                0.001 * x.[2] // B | SynthesisName: Y <-> B
                -0.01 * x.[0] // Y | SynthesisName: Y <-> B
                0.001 * x.[3] // a | SynthesisName: Y <-> a
                -0.01 * x.[0] // Y | SynthesisName: Y <-> a
                0.001 * x.[1] // A | SynthesisName: Y <-> A
                -0.01 * x.[0] // Y | SynthesisName: Y <-> A
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 1 - A
            [|
                -kW * (2.0 * xSum - x.[1]) * x.[1]
                2.4361881551451 * x.[24] * x.[80] // AAb + bab | CatalyticLigationName: A + Ab + bab <-> AAb + bab
                -2.4361881551451 * x.[1] * x.[8] * x.[80] // A + Ab + bab | CatalyticLigationName: A + Ab + bab <-> AAb + bab
                2.44902258657842 * x.[24] * x.[38] // AAb + BAB | CatalyticLigationName: A + Ab + BAB <-> AAb + BAB
                -2.44902258657842 * x.[1] * x.[8] * x.[38] // A + Ab + BAB | CatalyticLigationName: A + Ab + BAB <-> AAb + BAB
                7.1440001903487 * x.[8] * x.[20] // Ab + bb | CatalyticLigationName: A + b + bb <-> Ab + bb
                -7.1440001903487 * x.[1] * x.[4] * x.[20] // A + b + bb | CatalyticLigationName: A + b + bb <-> Ab + bb
                0.1 * x.[17] // bA | LigationName: b + A <-> bA
                -0.1 * x.[4] * x.[1] // b + A | LigationName: b + A <-> bA
                0.1 * x.[36] // Abb | LigationName: A + bb <-> Abb
                -0.1 * x.[1] * x.[20] // A + bb | LigationName: A + bb <-> Abb
                0.1 * x.[35] // Aba | LigationName: A + ba <-> Aba
                -0.1 * x.[1] * x.[19] // A + ba | LigationName: A + ba <-> Aba
                0.1 * x.[34] // AbB | LigationName: A + bB <-> AbB
                -0.1 * x.[1] * x.[18] // A + bB | LigationName: A + bB <-> AbB
                0.1 * x.[33] // AbA | LigationName: A + bA <-> AbA
                -0.1 * x.[1] * x.[17] // A + bA | LigationName: A + bA <-> AbA
                0.1 * x.[32] // Aab | LigationName: A + ab <-> Aab
                -0.1 * x.[1] * x.[16] // A + ab | LigationName: A + ab <-> Aab
                0.1 * x.[31] // Aaa | LigationName: A + aa <-> Aaa
                -0.1 * x.[1] * x.[15] // A + aa | LigationName: A + aa <-> Aaa
                0.1 * x.[30] // AaB | LigationName: A + aB <-> AaB
                -0.1 * x.[1] * x.[14] // A + aB | LigationName: A + aB <-> AaB
                0.1 * x.[29] // AaA | LigationName: A + aA <-> AaA
                -0.1 * x.[1] * x.[13] // A + aA | LigationName: A + aA <-> AaA
                0.1 * x.[28] // ABb | LigationName: A + Bb <-> ABb
                -0.1 * x.[1] * x.[12] // A + Bb | LigationName: A + Bb <-> ABb
                0.1 * x.[27] // ABa | LigationName: A + Ba <-> ABa
                -0.1 * x.[1] * x.[11] // A + Ba | LigationName: A + Ba <-> ABa
                0.1 * x.[26] // ABB | LigationName: A + BB <-> ABB
                -0.1 * x.[1] * x.[10] // A + BB | LigationName: A + BB <-> ABB
                0.1 * x.[25] // ABA | LigationName: A + BA <-> ABA
                -0.1 * x.[1] * x.[9] // A + BA | LigationName: A + BA <-> ABA
                0.1 * x.[24] // AAb | LigationName: A + Ab <-> AAb
                -0.1 * x.[1] * x.[8] // A + Ab | LigationName: A + Ab <-> AAb
                0.1 * x.[23] // AAa | LigationName: A + Aa <-> AAa
                -0.1 * x.[1] * x.[7] // A + Aa | LigationName: A + Aa <-> AAa
                0.1 * x.[22] // AAB | LigationName: A + AB <-> AAB
                -0.1 * x.[1] * x.[6] // A + AB | LigationName: A + AB <-> AAB
                0.1 * x.[21] // AAA | LigationName: A + AA <-> AAA
                -0.1 * x.[1] * x.[5] // A + AA | LigationName: A + AA <-> AAA
                0.1 * x.[8] // Ab | LigationName: A + b <-> Ab
                -0.1 * x.[1] * x.[4] // A + b | LigationName: A + b <-> Ab
                0.1 * x.[13] // aA | LigationName: a + A <-> aA
                -0.1 * x.[3] * x.[1] // a + A | LigationName: a + A <-> aA
                0.1 * x.[7] // Aa | LigationName: A + a <-> Aa
                -0.1 * x.[1] * x.[3] // A + a | LigationName: A + a <-> Aa
                0.1 * x.[6] // AB | LigationName: A + B <-> AB
                -0.1 * x.[1] * x.[2] // A + B | LigationName: A + B <-> AB
                0.1 * x.[5] // AA | LigationName: A + A <-> AA
                0.1 * x.[5] // AA | LigationName: A + A <-> AA
                -0.1 * x.[1] * x.[1] // A + A | LigationName: A + A <-> AA
                -0.1 * x.[1] * x.[1] // A + A | LigationName: A + A <-> AA
                -0.001 * x.[1] // A | SynthesisName: Y <-> A
                0.01 * x.[0] // Y | SynthesisName: Y <-> A
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 2 - B
            [|
                -kW * (2.0 * xSum - x.[2]) * x.[2]
                8.55145106427448 * x.[10] * x.[74] // BB + bBB | CatalyticLigationName: B + B + bBB <-> BB + bBB
                8.55145106427448 * x.[10] * x.[74] // BB + bBB | CatalyticLigationName: B + B + bBB <-> BB + bBB
                -8.55145106427448 * x.[2] * x.[2] * x.[74] // B + B + bBB | CatalyticLigationName: B + B + bBB <-> BB + bBB
                -8.55145106427448 * x.[2] * x.[2] * x.[74] // B + B + bBB | CatalyticLigationName: B + B + bBB <-> BB + bBB
                8.78688612493533 * x.[10] * x.[52] // BB + Bbb | CatalyticLigationName: B + B + Bbb <-> BB + Bbb
                8.78688612493533 * x.[10] * x.[52] // BB + Bbb | CatalyticLigationName: B + B + Bbb <-> BB + Bbb
                -8.78688612493533 * x.[2] * x.[2] * x.[52] // B + B + Bbb | CatalyticLigationName: B + B + Bbb <-> BB + Bbb
                -8.78688612493533 * x.[2] * x.[2] * x.[52] // B + B + Bbb | CatalyticLigationName: B + B + Bbb <-> BB + Bbb
                7.1440001903487 * x.[14] * x.[10] // aB + BB | CatalyticLigationName: a + B + BB <-> aB + BB
                -7.1440001903487 * x.[3] * x.[2] * x.[10] // a + B + BB | CatalyticLigationName: a + B + BB <-> aB + BB
                0.1 * x.[52] // Bbb | LigationName: B + bb <-> Bbb
                -0.1 * x.[2] * x.[20] // B + bb | LigationName: B + bb <-> Bbb
                0.1 * x.[51] // Bba | LigationName: B + ba <-> Bba
                -0.1 * x.[2] * x.[19] // B + ba | LigationName: B + ba <-> Bba
                0.1 * x.[50] // BbB | LigationName: B + bB <-> BbB
                -0.1 * x.[2] * x.[18] // B + bB | LigationName: B + bB <-> BbB
                0.1 * x.[49] // BbA | LigationName: B + bA <-> BbA
                -0.1 * x.[2] * x.[17] // B + bA | LigationName: B + bA <-> BbA
                0.1 * x.[48] // Bab | LigationName: B + ab <-> Bab
                -0.1 * x.[2] * x.[16] // B + ab | LigationName: B + ab <-> Bab
                0.1 * x.[47] // Baa | LigationName: B + aa <-> Baa
                -0.1 * x.[2] * x.[15] // B + aa | LigationName: B + aa <-> Baa
                0.1 * x.[46] // BaB | LigationName: B + aB <-> BaB
                -0.1 * x.[2] * x.[14] // B + aB | LigationName: B + aB <-> BaB
                0.1 * x.[45] // BaA | LigationName: B + aA <-> BaA
                -0.1 * x.[2] * x.[13] // B + aA | LigationName: B + aA <-> BaA
                0.1 * x.[44] // BBb | LigationName: B + Bb <-> BBb
                -0.1 * x.[2] * x.[12] // B + Bb | LigationName: B + Bb <-> BBb
                0.1 * x.[43] // BBa | LigationName: B + Ba <-> BBa
                -0.1 * x.[2] * x.[11] // B + Ba | LigationName: B + Ba <-> BBa
                0.1 * x.[42] // BBB | LigationName: B + BB <-> BBB
                -0.1 * x.[2] * x.[10] // B + BB | LigationName: B + BB <-> BBB
                0.1 * x.[41] // BBA | LigationName: B + BA <-> BBA
                -0.1 * x.[2] * x.[9] // B + BA | LigationName: B + BA <-> BBA
                0.1 * x.[40] // BAb | LigationName: B + Ab <-> BAb
                -0.1 * x.[2] * x.[8] // B + Ab | LigationName: B + Ab <-> BAb
                0.1 * x.[39] // BAa | LigationName: B + Aa <-> BAa
                -0.1 * x.[2] * x.[7] // B + Aa | LigationName: B + Aa <-> BAa
                0.1 * x.[38] // BAB | LigationName: B + AB <-> BAB
                -0.1 * x.[2] * x.[6] // B + AB | LigationName: B + AB <-> BAB
                0.1 * x.[37] // BAA | LigationName: B + AA <-> BAA
                -0.1 * x.[2] * x.[5] // B + AA | LigationName: B + AA <-> BAA
                0.1 * x.[18] // bB | LigationName: b + B <-> bB
                -0.1 * x.[4] * x.[2] // b + B | LigationName: b + B <-> bB
                0.1 * x.[12] // Bb | LigationName: B + b <-> Bb
                -0.1 * x.[2] * x.[4] // B + b | LigationName: B + b <-> Bb
                0.1 * x.[11] // Ba | LigationName: B + a <-> Ba
                -0.1 * x.[2] * x.[3] // B + a | LigationName: B + a <-> Ba
                0.1 * x.[10] // BB | LigationName: B + B <-> BB
                0.1 * x.[10] // BB | LigationName: B + B <-> BB
                -0.1 * x.[2] * x.[2] // B + B | LigationName: B + B <-> BB
                -0.1 * x.[2] * x.[2] // B + B | LigationName: B + B <-> BB
                0.1 * x.[14] // aB | LigationName: a + B <-> aB
                -0.1 * x.[3] * x.[2] // a + B | LigationName: a + B <-> aB
                0.1 * x.[6] // AB | LigationName: A + B <-> AB
                -0.1 * x.[1] * x.[2] // A + B | LigationName: A + B <-> AB
                -0.001 * x.[2] // B | SynthesisName: Y <-> B
                0.01 * x.[0] // Y | SynthesisName: Y <-> B
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 3 - a
            [|
                -kW * (2.0 * xSum - x.[3]) * x.[3]
                2.4361881551451 * x.[62] * x.[38] // aaB + BAB | CatalyticLigationName: a + aB + BAB <-> aaB + BAB
                -2.4361881551451 * x.[3] * x.[14] * x.[38] // a + aB + BAB | CatalyticLigationName: a + aB + BAB <-> aaB + BAB
                2.44902258657842 * x.[62] * x.[80] // aaB + bab | CatalyticLigationName: a + aB + bab <-> aaB + bab
                -2.44902258657842 * x.[3] * x.[14] * x.[80] // a + aB + bab | CatalyticLigationName: a + aB + bab <-> aaB + bab
                7.1440001903487 * x.[14] * x.[10] // aB + BB | CatalyticLigationName: a + B + BB <-> aB + BB
                -7.1440001903487 * x.[3] * x.[2] * x.[10] // a + B + BB | CatalyticLigationName: a + B + BB <-> aB + BB
                0.1 * x.[11] // Ba | LigationName: B + a <-> Ba
                -0.1 * x.[2] * x.[3] // B + a | LigationName: B + a <-> Ba
                0.1 * x.[58] // aBB | LigationName: a + BB <-> aBB
                -0.1 * x.[3] * x.[10] // a + BB | LigationName: a + BB <-> aBB
                0.1 * x.[57] // aBA | LigationName: a + BA <-> aBA
                -0.1 * x.[3] * x.[9] // a + BA | LigationName: a + BA <-> aBA
                0.1 * x.[60] // aBb | LigationName: a + Bb <-> aBb
                -0.1 * x.[3] * x.[12] // a + Bb | LigationName: a + Bb <-> aBb
                0.1 * x.[59] // aBa | LigationName: a + Ba <-> aBa
                -0.1 * x.[3] * x.[11] // a + Ba | LigationName: a + Ba <-> aBa
                0.1 * x.[54] // aAB | LigationName: a + AB <-> aAB
                -0.1 * x.[3] * x.[6] // a + AB | LigationName: a + AB <-> aAB
                0.1 * x.[53] // aAA | LigationName: a + AA <-> aAA
                -0.1 * x.[3] * x.[5] // a + AA | LigationName: a + AA <-> aAA
                0.1 * x.[56] // aAb | LigationName: a + Ab <-> aAb
                -0.1 * x.[3] * x.[8] // a + Ab | LigationName: a + Ab <-> aAb
                0.1 * x.[55] // aAa | LigationName: a + Aa <-> aAa
                -0.1 * x.[3] * x.[7] // a + Aa | LigationName: a + Aa <-> aAa
                0.1 * x.[66] // abB | LigationName: a + bB <-> abB
                -0.1 * x.[3] * x.[18] // a + bB | LigationName: a + bB <-> abB
                0.1 * x.[65] // abA | LigationName: a + bA <-> abA
                -0.1 * x.[3] * x.[17] // a + bA | LigationName: a + bA <-> abA
                0.1 * x.[68] // abb | LigationName: a + bb <-> abb
                -0.1 * x.[3] * x.[20] // a + bb | LigationName: a + bb <-> abb
                0.1 * x.[67] // aba | LigationName: a + ba <-> aba
                -0.1 * x.[3] * x.[19] // a + ba | LigationName: a + ba <-> aba
                0.1 * x.[62] // aaB | LigationName: a + aB <-> aaB
                -0.1 * x.[3] * x.[14] // a + aB | LigationName: a + aB <-> aaB
                0.1 * x.[61] // aaA | LigationName: a + aA <-> aaA
                -0.1 * x.[3] * x.[13] // a + aA | LigationName: a + aA <-> aaA
                0.1 * x.[64] // aab | LigationName: a + ab <-> aab
                -0.1 * x.[3] * x.[16] // a + ab | LigationName: a + ab <-> aab
                0.1 * x.[63] // aaa | LigationName: a + aa <-> aaa
                -0.1 * x.[3] * x.[15] // a + aa | LigationName: a + aa <-> aaa
                0.1 * x.[14] // aB | LigationName: a + B <-> aB
                -0.1 * x.[3] * x.[2] // a + B | LigationName: a + B <-> aB
                0.1 * x.[13] // aA | LigationName: a + A <-> aA
                -0.1 * x.[3] * x.[1] // a + A | LigationName: a + A <-> aA
                0.1 * x.[7] // Aa | LigationName: A + a <-> Aa
                -0.1 * x.[1] * x.[3] // A + a | LigationName: A + a <-> Aa
                0.1 * x.[16] // ab | LigationName: a + b <-> ab
                -0.1 * x.[3] * x.[4] // a + b | LigationName: a + b <-> ab
                0.1 * x.[15] // aa | LigationName: a + a <-> aa
                0.1 * x.[15] // aa | LigationName: a + a <-> aa
                -0.1 * x.[3] * x.[3] // a + a | LigationName: a + a <-> aa
                -0.1 * x.[3] * x.[3] // a + a | LigationName: a + a <-> aa
                -0.001 * x.[3] // a | SynthesisName: Y <-> a
                0.01 * x.[0] // Y | SynthesisName: Y <-> a
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 4 - b
            [|
                -kW * (2.0 * xSum - x.[4]) * x.[4]
                8.55145106427448 * x.[20] * x.[52] // bb + Bbb | CatalyticLigationName: b + b + Bbb <-> bb + Bbb
                8.55145106427448 * x.[20] * x.[52] // bb + Bbb | CatalyticLigationName: b + b + Bbb <-> bb + Bbb
                -8.55145106427448 * x.[4] * x.[4] * x.[52] // b + b + Bbb | CatalyticLigationName: b + b + Bbb <-> bb + Bbb
                -8.55145106427448 * x.[4] * x.[4] * x.[52] // b + b + Bbb | CatalyticLigationName: b + b + Bbb <-> bb + Bbb
                8.78688612493533 * x.[20] * x.[74] // bb + bBB | CatalyticLigationName: b + b + bBB <-> bb + bBB
                8.78688612493533 * x.[20] * x.[74] // bb + bBB | CatalyticLigationName: b + b + bBB <-> bb + bBB
                -8.78688612493533 * x.[4] * x.[4] * x.[74] // b + b + bBB | CatalyticLigationName: b + b + bBB <-> bb + bBB
                -8.78688612493533 * x.[4] * x.[4] * x.[74] // b + b + bBB | CatalyticLigationName: b + b + bBB <-> bb + bBB
                7.1440001903487 * x.[8] * x.[20] // Ab + bb | CatalyticLigationName: A + b + bb <-> Ab + bb
                -7.1440001903487 * x.[1] * x.[4] * x.[20] // A + b + bb | CatalyticLigationName: A + b + bb <-> Ab + bb
                0.1 * x.[74] // bBB | LigationName: b + BB <-> bBB
                -0.1 * x.[4] * x.[10] // b + BB | LigationName: b + BB <-> bBB
                0.1 * x.[73] // bBA | LigationName: b + BA <-> bBA
                -0.1 * x.[4] * x.[9] // b + BA | LigationName: b + BA <-> bBA
                0.1 * x.[76] // bBb | LigationName: b + Bb <-> bBb
                -0.1 * x.[4] * x.[12] // b + Bb | LigationName: b + Bb <-> bBb
                0.1 * x.[75] // bBa | LigationName: b + Ba <-> bBa
                -0.1 * x.[4] * x.[11] // b + Ba | LigationName: b + Ba <-> bBa
                0.1 * x.[70] // bAB | LigationName: b + AB <-> bAB
                -0.1 * x.[4] * x.[6] // b + AB | LigationName: b + AB <-> bAB
                0.1 * x.[69] // bAA | LigationName: b + AA <-> bAA
                -0.1 * x.[4] * x.[5] // b + AA | LigationName: b + AA <-> bAA
                0.1 * x.[72] // bAb | LigationName: b + Ab <-> bAb
                -0.1 * x.[4] * x.[8] // b + Ab | LigationName: b + Ab <-> bAb
                0.1 * x.[71] // bAa | LigationName: b + Aa <-> bAa
                -0.1 * x.[4] * x.[7] // b + Aa | LigationName: b + Aa <-> bAa
                0.1 * x.[82] // bbB | LigationName: b + bB <-> bbB
                -0.1 * x.[4] * x.[18] // b + bB | LigationName: b + bB <-> bbB
                0.1 * x.[81] // bbA | LigationName: b + bA <-> bbA
                -0.1 * x.[4] * x.[17] // b + bA | LigationName: b + bA <-> bbA
                0.1 * x.[84] // bbb | LigationName: b + bb <-> bbb
                -0.1 * x.[4] * x.[20] // b + bb | LigationName: b + bb <-> bbb
                0.1 * x.[83] // bba | LigationName: b + ba <-> bba
                -0.1 * x.[4] * x.[19] // b + ba | LigationName: b + ba <-> bba
                0.1 * x.[78] // baB | LigationName: b + aB <-> baB
                -0.1 * x.[4] * x.[14] // b + aB | LigationName: b + aB <-> baB
                0.1 * x.[77] // baA | LigationName: b + aA <-> baA
                -0.1 * x.[4] * x.[13] // b + aA | LigationName: b + aA <-> baA
                0.1 * x.[80] // bab | LigationName: b + ab <-> bab
                -0.1 * x.[4] * x.[16] // b + ab | LigationName: b + ab <-> bab
                0.1 * x.[79] // baa | LigationName: b + aa <-> baa
                -0.1 * x.[4] * x.[15] // b + aa | LigationName: b + aa <-> baa
                0.1 * x.[18] // bB | LigationName: b + B <-> bB
                -0.1 * x.[4] * x.[2] // b + B | LigationName: b + B <-> bB
                0.1 * x.[12] // Bb | LigationName: B + b <-> Bb
                -0.1 * x.[2] * x.[4] // B + b | LigationName: B + b <-> Bb
                0.1 * x.[17] // bA | LigationName: b + A <-> bA
                -0.1 * x.[4] * x.[1] // b + A | LigationName: b + A <-> bA
                0.1 * x.[20] // bb | LigationName: b + b <-> bb
                0.1 * x.[20] // bb | LigationName: b + b <-> bb
                -0.1 * x.[4] * x.[4] // b + b | LigationName: b + b <-> bb
                -0.1 * x.[4] * x.[4] // b + b | LigationName: b + b <-> bb
                0.1 * x.[8] // Ab | LigationName: A + b <-> Ab
                -0.1 * x.[1] * x.[4] // A + b | LigationName: A + b <-> Ab
                0.1 * x.[16] // ab | LigationName: a + b <-> ab
                -0.1 * x.[3] * x.[4] // a + b | LigationName: a + b <-> ab
                -0.001 * x.[4] // b | SynthesisName: Y <-> b
                0.01 * x.[0] // Y | SynthesisName: Y <-> b
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 5 - AA
            [|
                -kW * (2.0 * xSum - x.[5]) * x.[5]
                -15.8725584321517 * x.[5] * x.[84] // AA + bbb | SedimentationDirectName: AA + bbb -> 5 Y
                -25.9563678497145 * x.[5] * x.[83] // AA + bba | SedimentationDirectName: AA + bba -> 5 Y
                -55.9295153352086 * x.[5] * x.[57] // AA + aBA | SedimentationDirectName: AA + aBA -> 5 Y
                0.1 * x.[69] // bAA | LigationName: b + AA <-> bAA
                -0.1 * x.[4] * x.[5] // b + AA | LigationName: b + AA <-> bAA
                0.1 * x.[37] // BAA | LigationName: B + AA <-> BAA
                -0.1 * x.[2] * x.[5] // B + AA | LigationName: B + AA <-> BAA
                0.1 * x.[53] // aAA | LigationName: a + AA <-> aAA
                -0.1 * x.[3] * x.[5] // a + AA | LigationName: a + AA <-> aAA
                0.1 * x.[21] // AAA | LigationName: A + AA <-> AAA
                -0.1 * x.[1] * x.[5] // A + AA | LigationName: A + AA <-> AAA
                -0.1 * x.[5] // AA | LigationName: A + A <-> AA
                0.1 * x.[1] * x.[1] // A + A | LigationName: A + A <-> AA
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 6 - AB
            [|
                -kW * (2.0 * xSum - x.[6]) * x.[6]
                -69.5562059032595 * x.[6] * x.[24] // AB + AAb | SedimentationDirectName: AB + AAb -> 5 Y
                0.1 * x.[70] // bAB | LigationName: b + AB <-> bAB
                -0.1 * x.[4] * x.[6] // b + AB | LigationName: b + AB <-> bAB
                0.1 * x.[38] // BAB | LigationName: B + AB <-> BAB
                -0.1 * x.[2] * x.[6] // B + AB | LigationName: B + AB <-> BAB
                0.1 * x.[54] // aAB | LigationName: a + AB <-> aAB
                -0.1 * x.[3] * x.[6] // a + AB | LigationName: a + AB <-> aAB
                0.1 * x.[22] // AAB | LigationName: A + AB <-> AAB
                -0.1 * x.[1] * x.[6] // A + AB | LigationName: A + AB <-> AAB
                -0.1 * x.[6] // AB | LigationName: A + B <-> AB
                0.1 * x.[1] * x.[2] // A + B | LigationName: A + B <-> AB
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 7 - Aa
            [|
                -kW * (2.0 * xSum - x.[7]) * x.[7]
                -3.19070828671931 * x.[7] * x.[29] // Aa + AaA | SedimentationDirectName: Aa + AaA -> 5 Y
                -74.8402332401258 * x.[7] * x.[25] // Aa + ABA | SedimentationDirectName: Aa + ABA -> 5 Y
                0.1 * x.[71] // bAa | LigationName: b + Aa <-> bAa
                -0.1 * x.[4] * x.[7] // b + Aa | LigationName: b + Aa <-> bAa
                0.1 * x.[39] // BAa | LigationName: B + Aa <-> BAa
                -0.1 * x.[2] * x.[7] // B + Aa | LigationName: B + Aa <-> BAa
                0.1 * x.[55] // aAa | LigationName: a + Aa <-> aAa
                -0.1 * x.[3] * x.[7] // a + Aa | LigationName: a + Aa <-> aAa
                0.1 * x.[23] // AAa | LigationName: A + Aa <-> AAa
                -0.1 * x.[1] * x.[7] // A + Aa | LigationName: A + Aa <-> AAa
                -0.1 * x.[7] // Aa | LigationName: A + a <-> Aa
                0.1 * x.[1] * x.[3] // A + a | LigationName: A + a <-> Aa
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 8 - Ab
            [|
                -kW * (2.0 * xSum - x.[8]) * x.[8]
                -47.2125087225556 * x.[8] * x.[27] // Ab + ABa | SedimentationDirectName: Ab + ABa -> 5 Y
                2.4361881551451 * x.[24] * x.[80] // AAb + bab | CatalyticLigationName: A + Ab + bab <-> AAb + bab
                -2.4361881551451 * x.[1] * x.[8] * x.[80] // A + Ab + bab | CatalyticLigationName: A + Ab + bab <-> AAb + bab
                2.44902258657842 * x.[24] * x.[38] // AAb + BAB | CatalyticLigationName: A + Ab + BAB <-> AAb + BAB
                -2.44902258657842 * x.[1] * x.[8] * x.[38] // A + Ab + BAB | CatalyticLigationName: A + Ab + BAB <-> AAb + BAB
                -7.1440001903487 * x.[8] * x.[20] // Ab + bb | CatalyticLigationName: A + b + bb <-> Ab + bb
                7.1440001903487 * x.[1] * x.[4] * x.[20] // A + b + bb | CatalyticLigationName: A + b + bb <-> Ab + bb
                0.1 * x.[72] // bAb | LigationName: b + Ab <-> bAb
                -0.1 * x.[4] * x.[8] // b + Ab | LigationName: b + Ab <-> bAb
                0.1 * x.[40] // BAb | LigationName: B + Ab <-> BAb
                -0.1 * x.[2] * x.[8] // B + Ab | LigationName: B + Ab <-> BAb
                0.1 * x.[56] // aAb | LigationName: a + Ab <-> aAb
                -0.1 * x.[3] * x.[8] // a + Ab | LigationName: a + Ab <-> aAb
                0.1 * x.[24] // AAb | LigationName: A + Ab <-> AAb
                -0.1 * x.[1] * x.[8] // A + Ab | LigationName: A + Ab <-> AAb
                -0.1 * x.[8] // Ab | LigationName: A + b <-> Ab
                0.1 * x.[1] * x.[4] // A + b | LigationName: A + b <-> Ab
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 9 - BA
            [|
                -kW * (2.0 * xSum - x.[9]) * x.[9]
                -21.5306142530327 * x.[9] * x.[79] // BA + baa | SedimentationDirectName: BA + baa -> 5 Y
                0.1 * x.[73] // bBA | LigationName: b + BA <-> bBA
                -0.1 * x.[4] * x.[9] // b + BA | LigationName: b + BA <-> bBA
                0.1 * x.[41] // BBA | LigationName: B + BA <-> BBA
                -0.1 * x.[2] * x.[9] // B + BA | LigationName: B + BA <-> BBA
                0.1 * x.[57] // aBA | LigationName: a + BA <-> aBA
                -0.1 * x.[3] * x.[9] // a + BA | LigationName: a + BA <-> aBA
                0.1 * x.[25] // ABA | LigationName: A + BA <-> ABA
                -0.1 * x.[1] * x.[9] // A + BA | LigationName: A + BA <-> ABA
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 10 - BB
            [|
                -kW * (2.0 * xSum - x.[10]) * x.[10]
                -8.55145106427448 * x.[10] * x.[74] // BB + bBB | CatalyticLigationName: B + B + bBB <-> BB + bBB
                8.55145106427448 * x.[2] * x.[2] * x.[74] // B + B + bBB | CatalyticLigationName: B + B + bBB <-> BB + bBB
                -8.78688612493533 * x.[10] * x.[52] // BB + Bbb | CatalyticLigationName: B + B + Bbb <-> BB + Bbb
                8.78688612493533 * x.[2] * x.[2] * x.[52] // B + B + Bbb | CatalyticLigationName: B + B + Bbb <-> BB + Bbb
                0.1 * x.[74] // bBB | LigationName: b + BB <-> bBB
                -0.1 * x.[4] * x.[10] // b + BB | LigationName: b + BB <-> bBB
                0.1 * x.[42] // BBB | LigationName: B + BB <-> BBB
                -0.1 * x.[2] * x.[10] // B + BB | LigationName: B + BB <-> BBB
                -0.1 * x.[10] // BB | LigationName: B + B <-> BB
                0.1 * x.[2] * x.[2] // B + B | LigationName: B + B <-> BB
                0.1 * x.[58] // aBB | LigationName: a + BB <-> aBB
                -0.1 * x.[3] * x.[10] // a + BB | LigationName: a + BB <-> aBB
                0.1 * x.[26] // ABB | LigationName: A + BB <-> ABB
                -0.1 * x.[1] * x.[10] // A + BB | LigationName: A + BB <-> ABB
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 11 - Ba
            [|
                -kW * (2.0 * xSum - x.[11]) * x.[11]
                -38.5573268466612 * x.[11] * x.[78] // Ba + baB | SedimentationDirectName: Ba + baB -> 5 Y
                -66.4258069092912 * x.[11] * x.[37] // Ba + BAA | SedimentationDirectName: Ba + BAA -> 5 Y
                0.1 * x.[75] // bBa | LigationName: b + Ba <-> bBa
                -0.1 * x.[4] * x.[11] // b + Ba | LigationName: b + Ba <-> bBa
                0.1 * x.[43] // BBa | LigationName: B + Ba <-> BBa
                -0.1 * x.[2] * x.[11] // B + Ba | LigationName: B + Ba <-> BBa
                -0.1 * x.[11] // Ba | LigationName: B + a <-> Ba
                0.1 * x.[2] * x.[3] // B + a | LigationName: B + a <-> Ba
                0.1 * x.[59] // aBa | LigationName: a + Ba <-> aBa
                -0.1 * x.[3] * x.[11] // a + Ba | LigationName: a + Ba <-> aBa
                0.1 * x.[27] // ABa | LigationName: A + Ba <-> ABa
                -0.1 * x.[1] * x.[11] // A + Ba | LigationName: A + Ba <-> ABa
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 12 - Bb
            [|
                -kW * (2.0 * xSum - x.[12]) * x.[12]
                0.1 * x.[76] // bBb | LigationName: b + Bb <-> bBb
                -0.1 * x.[4] * x.[12] // b + Bb | LigationName: b + Bb <-> bBb
                0.1 * x.[44] // BBb | LigationName: B + Bb <-> BBb
                -0.1 * x.[2] * x.[12] // B + Bb | LigationName: B + Bb <-> BBb
                -0.1 * x.[12] // Bb | LigationName: B + b <-> Bb
                0.1 * x.[2] * x.[4] // B + b | LigationName: B + b <-> Bb
                0.1 * x.[60] // aBb | LigationName: a + Bb <-> aBb
                -0.1 * x.[3] * x.[12] // a + Bb | LigationName: a + Bb <-> aBb
                0.1 * x.[28] // ABb | LigationName: A + Bb <-> ABb
                -0.1 * x.[1] * x.[12] // A + Bb | LigationName: A + Bb <-> ABb
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 13 - aA
            [|
                -kW * (2.0 * xSum - x.[13]) * x.[13]
                -3.19070828671931 * x.[13] * x.[55] // aA + aAa | SedimentationDirectName: aA + aAa -> 5 Y
                -74.8402332401258 * x.[13] * x.[67] // aA + aba | SedimentationDirectName: aA + aba -> 5 Y
                0.1 * x.[45] // BaA | LigationName: B + aA <-> BaA
                -0.1 * x.[2] * x.[13] // B + aA | LigationName: B + aA <-> BaA
                0.1 * x.[77] // baA | LigationName: b + aA <-> baA
                -0.1 * x.[4] * x.[13] // b + aA | LigationName: b + aA <-> baA
                0.1 * x.[29] // AaA | LigationName: A + aA <-> AaA
                -0.1 * x.[1] * x.[13] // A + aA | LigationName: A + aA <-> AaA
                0.1 * x.[61] // aaA | LigationName: a + aA <-> aaA
                -0.1 * x.[3] * x.[13] // a + aA | LigationName: a + aA <-> aaA
                -0.1 * x.[13] // aA | LigationName: a + A <-> aA
                0.1 * x.[3] * x.[1] // a + A | LigationName: a + A <-> aA
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 14 - aB
            [|
                -kW * (2.0 * xSum - x.[14]) * x.[14]
                -47.2125087225556 * x.[14] * x.[65] // aB + abA | SedimentationDirectName: aB + abA -> 5 Y
                2.4361881551451 * x.[62] * x.[38] // aaB + BAB | CatalyticLigationName: a + aB + BAB <-> aaB + BAB
                -2.4361881551451 * x.[3] * x.[14] * x.[38] // a + aB + BAB | CatalyticLigationName: a + aB + BAB <-> aaB + BAB
                2.44902258657842 * x.[62] * x.[80] // aaB + bab | CatalyticLigationName: a + aB + bab <-> aaB + bab
                -2.44902258657842 * x.[3] * x.[14] * x.[80] // a + aB + bab | CatalyticLigationName: a + aB + bab <-> aaB + bab
                -7.1440001903487 * x.[14] * x.[10] // aB + BB | CatalyticLigationName: a + B + BB <-> aB + BB
                7.1440001903487 * x.[3] * x.[2] * x.[10] // a + B + BB | CatalyticLigationName: a + B + BB <-> aB + BB
                0.1 * x.[46] // BaB | LigationName: B + aB <-> BaB
                -0.1 * x.[2] * x.[14] // B + aB | LigationName: B + aB <-> BaB
                0.1 * x.[78] // baB | LigationName: b + aB <-> baB
                -0.1 * x.[4] * x.[14] // b + aB | LigationName: b + aB <-> baB
                0.1 * x.[30] // AaB | LigationName: A + aB <-> AaB
                -0.1 * x.[1] * x.[14] // A + aB | LigationName: A + aB <-> AaB
                0.1 * x.[62] // aaB | LigationName: a + aB <-> aaB
                -0.1 * x.[3] * x.[14] // a + aB | LigationName: a + aB <-> aaB
                -0.1 * x.[14] // aB | LigationName: a + B <-> aB
                0.1 * x.[3] * x.[2] // a + B | LigationName: a + B <-> aB
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 15 - aa
            [|
                -kW * (2.0 * xSum - x.[15]) * x.[15]
                -15.8725584321517 * x.[15] * x.[42] // aa + BBB | SedimentationDirectName: aa + BBB -> 5 Y
                -25.9563678497145 * x.[15] * x.[41] // aa + BBA | SedimentationDirectName: aa + BBA -> 5 Y
                -55.9295153352086 * x.[15] * x.[35] // aa + Aba | SedimentationDirectName: aa + Aba -> 5 Y
                0.1 * x.[47] // Baa | LigationName: B + aa <-> Baa
                -0.1 * x.[2] * x.[15] // B + aa | LigationName: B + aa <-> Baa
                0.1 * x.[79] // baa | LigationName: b + aa <-> baa
                -0.1 * x.[4] * x.[15] // b + aa | LigationName: b + aa <-> baa
                0.1 * x.[31] // Aaa | LigationName: A + aa <-> Aaa
                -0.1 * x.[1] * x.[15] // A + aa | LigationName: A + aa <-> Aaa
                0.1 * x.[63] // aaa | LigationName: a + aa <-> aaa
                -0.1 * x.[3] * x.[15] // a + aa | LigationName: a + aa <-> aaa
                -0.1 * x.[15] // aa | LigationName: a + a <-> aa
                0.1 * x.[3] * x.[3] // a + a | LigationName: a + a <-> aa
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 16 - ab
            [|
                -kW * (2.0 * xSum - x.[16]) * x.[16]
                -69.5562059032595 * x.[16] * x.[62] // ab + aaB | SedimentationDirectName: ab + aaB -> 5 Y
                0.1 * x.[48] // Bab | LigationName: B + ab <-> Bab
                -0.1 * x.[2] * x.[16] // B + ab | LigationName: B + ab <-> Bab
                0.1 * x.[80] // bab | LigationName: b + ab <-> bab
                -0.1 * x.[4] * x.[16] // b + ab | LigationName: b + ab <-> bab
                0.1 * x.[32] // Aab | LigationName: A + ab <-> Aab
                -0.1 * x.[1] * x.[16] // A + ab | LigationName: A + ab <-> Aab
                0.1 * x.[64] // aab | LigationName: a + ab <-> aab
                -0.1 * x.[3] * x.[16] // a + ab | LigationName: a + ab <-> aab
                -0.1 * x.[16] // ab | LigationName: a + b <-> ab
                0.1 * x.[3] * x.[4] // a + b | LigationName: a + b <-> ab
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 17 - bA
            [|
                -kW * (2.0 * xSum - x.[17]) * x.[17]
                -38.5573268466612 * x.[17] * x.[40] // bA + BAb | SedimentationDirectName: bA + BAb -> 5 Y
                -66.4258069092912 * x.[17] * x.[79] // bA + baa | SedimentationDirectName: bA + baa -> 5 Y
                0.1 * x.[49] // BbA | LigationName: B + bA <-> BbA
                -0.1 * x.[2] * x.[17] // B + bA | LigationName: B + bA <-> BbA
                0.1 * x.[81] // bbA | LigationName: b + bA <-> bbA
                -0.1 * x.[4] * x.[17] // b + bA | LigationName: b + bA <-> bbA
                -0.1 * x.[17] // bA | LigationName: b + A <-> bA
                0.1 * x.[4] * x.[1] // b + A | LigationName: b + A <-> bA
                0.1 * x.[33] // AbA | LigationName: A + bA <-> AbA
                -0.1 * x.[1] * x.[17] // A + bA | LigationName: A + bA <-> AbA
                0.1 * x.[65] // abA | LigationName: a + bA <-> abA
                -0.1 * x.[3] * x.[17] // a + bA | LigationName: a + bA <-> abA
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 18 - bB
            [|
                -kW * (2.0 * xSum - x.[18]) * x.[18]
                0.1 * x.[50] // BbB | LigationName: B + bB <-> BbB
                -0.1 * x.[2] * x.[18] // B + bB | LigationName: B + bB <-> BbB
                0.1 * x.[82] // bbB | LigationName: b + bB <-> bbB
                -0.1 * x.[4] * x.[18] // b + bB | LigationName: b + bB <-> bbB
                -0.1 * x.[18] // bB | LigationName: b + B <-> bB
                0.1 * x.[4] * x.[2] // b + B | LigationName: b + B <-> bB
                0.1 * x.[34] // AbB | LigationName: A + bB <-> AbB
                -0.1 * x.[1] * x.[18] // A + bB | LigationName: A + bB <-> AbB
                0.1 * x.[66] // abB | LigationName: a + bB <-> abB
                -0.1 * x.[3] * x.[18] // a + bB | LigationName: a + bB <-> abB
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 19 - ba
            [|
                -kW * (2.0 * xSum - x.[19]) * x.[19]
                -21.5306142530327 * x.[19] * x.[37] // ba + BAA | SedimentationDirectName: ba + BAA -> 5 Y
                0.1 * x.[51] // Bba | LigationName: B + ba <-> Bba
                -0.1 * x.[2] * x.[19] // B + ba | LigationName: B + ba <-> Bba
                0.1 * x.[83] // bba | LigationName: b + ba <-> bba
                -0.1 * x.[4] * x.[19] // b + ba | LigationName: b + ba <-> bba
                0.1 * x.[35] // Aba | LigationName: A + ba <-> Aba
                -0.1 * x.[1] * x.[19] // A + ba | LigationName: A + ba <-> Aba
                0.1 * x.[67] // aba | LigationName: a + ba <-> aba
                -0.1 * x.[3] * x.[19] // a + ba | LigationName: a + ba <-> aba
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 20 - bb
            [|
                -kW * (2.0 * xSum - x.[20]) * x.[20]
                -8.55145106427448 * x.[20] * x.[52] // bb + Bbb | CatalyticLigationName: b + b + Bbb <-> bb + Bbb
                8.55145106427448 * x.[4] * x.[4] * x.[52] // b + b + Bbb | CatalyticLigationName: b + b + Bbb <-> bb + Bbb
                -8.78688612493533 * x.[20] * x.[74] // bb + bBB | CatalyticLigationName: b + b + bBB <-> bb + bBB
                8.78688612493533 * x.[4] * x.[4] * x.[74] // b + b + bBB | CatalyticLigationName: b + b + bBB <-> bb + bBB
                0.1 * x.[52] // Bbb | LigationName: B + bb <-> Bbb
                -0.1 * x.[2] * x.[20] // B + bb | LigationName: B + bb <-> Bbb
                0.1 * x.[84] // bbb | LigationName: b + bb <-> bbb
                -0.1 * x.[4] * x.[20] // b + bb | LigationName: b + bb <-> bbb
                -0.1 * x.[20] // bb | LigationName: b + b <-> bb
                0.1 * x.[4] * x.[4] // b + b | LigationName: b + b <-> bb
                0.1 * x.[36] // Abb | LigationName: A + bb <-> Abb
                -0.1 * x.[1] * x.[20] // A + bb | LigationName: A + bb <-> Abb
                0.1 * x.[68] // abb | LigationName: a + bb <-> abb
                -0.1 * x.[3] * x.[20] // a + bb | LigationName: a + bb <-> abb
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 21 - AAA
            [|
                -kW * (2.0 * xSum - x.[21]) * x.[21]
                -3.21393117441257 * x.[67] * x.[21] // aba + AAA | SedimentationDirectName: aba + AAA -> 6 Y
                -0.1 * x.[21] // AAA | LigationName: A + AA <-> AAA
                0.1 * x.[1] * x.[5] // A + AA | LigationName: A + AA <-> AAA
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 22 - AAB
            [|
                -kW * (2.0 * xSum - x.[22]) * x.[22]
                -0.1 * x.[22] // AAB | LigationName: A + AB <-> AAB
                0.1 * x.[1] * x.[6] // A + AB | LigationName: A + AB <-> AAB
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 23 - AAa
            [|
                -kW * (2.0 * xSum - x.[23]) * x.[23]
                -74.0453128255527 * x.[54] * x.[23] // aAB + AAa | SedimentationDirectName: aAB + AAa -> 6 Y
                -0.1 * x.[23] // AAa | LigationName: A + Aa <-> AAa
                0.1 * x.[1] * x.[7] // A + Aa | LigationName: A + Aa <-> AAa
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 24 - AAb
            [|
                -kW * (2.0 * xSum - x.[24]) * x.[24]
                -32.5643983434404 * x.[24] * x.[36] // AAb + Abb | SedimentationDirectName: AAb + Abb -> 6 Y
                -69.5562059032595 * x.[6] * x.[24] // AB + AAb | SedimentationDirectName: AB + AAb -> 5 Y
                -2.4361881551451 * x.[24] * x.[80] // AAb + bab | CatalyticLigationName: A + Ab + bab <-> AAb + bab
                2.4361881551451 * x.[1] * x.[8] * x.[80] // A + Ab + bab | CatalyticLigationName: A + Ab + bab <-> AAb + bab
                -2.44902258657842 * x.[24] * x.[38] // AAb + BAB | CatalyticLigationName: A + Ab + BAB <-> AAb + BAB
                2.44902258657842 * x.[1] * x.[8] * x.[38] // A + Ab + BAB | CatalyticLigationName: A + Ab + BAB <-> AAb + BAB
                -0.1 * x.[24] // AAb | LigationName: A + Ab <-> AAb
                0.1 * x.[1] * x.[8] // A + Ab | LigationName: A + Ab <-> AAb
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 25 - ABA
            [|
                -kW * (2.0 * xSum - x.[25]) * x.[25]
                -34.1421760416434 * x.[53] * x.[25] // aAA + ABA | SedimentationDirectName: aAA + ABA -> 6 Y
                -21.3893445489625 * x.[56] * x.[25] // aAb + ABA | SedimentationDirectName: aAb + ABA -> 6 Y
                -21.091020549205 * x.[25] * x.[69] // ABA + bAA | SedimentationDirectName: ABA + bAA -> 6 Y
                -12.558847104467 * x.[25] * x.[65] // ABA + abA | SedimentationDirectName: ABA + abA -> 6 Y
                -3.21393117441257 * x.[25] * x.[63] // ABA + aaa | SedimentationDirectName: ABA + aaa -> 6 Y
                -74.8402332401258 * x.[7] * x.[25] // Aa + ABA | SedimentationDirectName: Aa + ABA -> 5 Y
                -0.1 * x.[25] // ABA | LigationName: A + BA <-> ABA
                0.1 * x.[1] * x.[9] // A + BA | LigationName: A + BA <-> ABA
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 26 - ABB
            [|
                -kW * (2.0 * xSum - x.[26]) * x.[26]
                -63.7870836148508 * x.[80] * x.[26] // bab + ABB | SedimentationDirectName: bab + ABB -> 6 Y
                -7.14868381414817 * x.[26] * x.[46] // ABB + BaB | SedimentationDirectName: ABB + BaB -> 6 Y
                -0.1 * x.[26] // ABB | LigationName: A + BB <-> ABB
                0.1 * x.[1] * x.[10] // A + BB | LigationName: A + BB <-> ABB
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 27 - ABa
            [|
                -kW * (2.0 * xSum - x.[27]) * x.[27]
                -12.558847104467 * x.[67] * x.[27] // aba + ABa | SedimentationDirectName: aba + ABa -> 6 Y
                -47.2125087225556 * x.[8] * x.[27] // Ab + ABa | SedimentationDirectName: Ab + ABa -> 5 Y
                -0.1 * x.[27] // ABa | LigationName: A + Ba <-> ABa
                0.1 * x.[1] * x.[11] // A + Ba | LigationName: A + Ba <-> ABa
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 28 - ABb
            [|
                -kW * (2.0 * xSum - x.[28]) * x.[28]
                -34.0491662737595 * x.[69] * x.[28] // bAA + ABb | SedimentationDirectName: bAA + ABb -> 6 Y
                -29.7789028491576 * x.[28] * x.[56] // ABb + aAb | SedimentationDirectName: ABb + aAb -> 6 Y
                -0.1 * x.[28] // ABb | LigationName: A + Bb <-> ABb
                0.1 * x.[1] * x.[12] // A + Bb | LigationName: A + Bb <-> ABb
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 29 - AaA
            [|
                -kW * (2.0 * xSum - x.[29]) * x.[29]
                -3.19070828671931 * x.[7] * x.[29] // Aa + AaA | SedimentationDirectName: Aa + AaA -> 5 Y
                -0.1 * x.[29] // AaA | LigationName: A + aA <-> AaA
                0.1 * x.[1] * x.[13] // A + aA | LigationName: A + aA <-> AaA
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 30 - AaB
            [|
                -kW * (2.0 * xSum - x.[30]) * x.[30]
                -48.5001542234325 * x.[82] * x.[30] // bbB + AaB | SedimentationDirectName: bbB + AaB -> 6 Y
                -21.3893445489625 * x.[30] * x.[67] // AaB + aba | SedimentationDirectName: AaB + aba -> 6 Y
                -82.6337629450791 * x.[30] * x.[60] // AaB + aBb | SedimentationDirectName: AaB + aBb -> 6 Y
                -29.7789028491576 * x.[66] * x.[30] // abB + AaB | SedimentationDirectName: abB + AaB -> 6 Y
                -0.1 * x.[30] // AaB | LigationName: A + aB <-> AaB
                0.1 * x.[1] * x.[14] // A + aB | LigationName: A + aB <-> AaB
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 31 - Aaa
            [|
                -kW * (2.0 * xSum - x.[31]) * x.[31]
                -34.1421760416434 * x.[31] * x.[67] // Aaa + aba | SedimentationDirectName: Aaa + aba -> 6 Y
                -0.1 * x.[31] // Aaa | LigationName: A + aa <-> Aaa
                0.1 * x.[1] * x.[15] // A + aa | LigationName: A + aa <-> Aaa
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 32 - Aab
            [|
                -kW * (2.0 * xSum - x.[32]) * x.[32]
                -74.0453128255527 * x.[32] * x.[61] // Aab + aaA | SedimentationDirectName: Aab + aaA -> 6 Y
                -0.1 * x.[32] // Aab | LigationName: A + ab <-> Aab
                0.1 * x.[1] * x.[16] // A + ab | LigationName: A + ab <-> Aab
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 33 - AbA
            [|
                -kW * (2.0 * xSum - x.[33]) * x.[33]
                -0.1 * x.[33] // AbA | LigationName: A + bA <-> AbA
                0.1 * x.[1] * x.[17] // A + bA | LigationName: A + bA <-> AbA
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 34 - AbB
            [|
                -kW * (2.0 * xSum - x.[34]) * x.[34]
                -53.5824645556566 * x.[83] * x.[34] // bba + AbB | SedimentationDirectName: bba + AbB -> 6 Y
                -82.6337629450791 * x.[56] * x.[34] // aAb + AbB | SedimentationDirectName: aAb + AbB -> 6 Y
                -0.1 * x.[34] // AbB | LigationName: A + bB <-> AbB
                0.1 * x.[1] * x.[18] // A + bB | LigationName: A + bB <-> AbB
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 35 - Aba
            [|
                -kW * (2.0 * xSum - x.[35]) * x.[35]
                -55.9295153352086 * x.[15] * x.[35] // aa + Aba | SedimentationDirectName: aa + Aba -> 5 Y
                -0.1 * x.[35] // Aba | LigationName: A + ba <-> Aba
                0.1 * x.[1] * x.[19] // A + ba | LigationName: A + ba <-> Aba
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 36 - Abb
            [|
                -kW * (2.0 * xSum - x.[36]) * x.[36]
                -80.7994733689242 * x.[36] * x.[43] // Abb + BBa | SedimentationDirectName: Abb + BBa -> 6 Y
                -32.5643983434404 * x.[24] * x.[36] // AAb + Abb | SedimentationDirectName: AAb + Abb -> 6 Y
                -0.1 * x.[36] // Abb | LigationName: A + bb <-> Abb
                0.1 * x.[1] * x.[20] // A + bb | LigationName: A + bb <-> Abb
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 37 - BAA
            [|
                -kW * (2.0 * xSum - x.[37]) * x.[37]
                -22.1393742250857 * x.[37] * x.[73] // BAA + bBA | SedimentationDirectName: BAA + bBA -> 6 Y
                -66.4258069092912 * x.[11] * x.[37] // Ba + BAA | SedimentationDirectName: Ba + BAA -> 5 Y
                -21.5306142530327 * x.[19] * x.[37] // ba + BAA | SedimentationDirectName: ba + BAA -> 5 Y
                -0.1 * x.[37] // BAA | LigationName: B + AA <-> BAA
                0.1 * x.[2] * x.[5] // B + AA | LigationName: B + AA <-> BAA
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 38 - BAB
            [|
                -kW * (2.0 * xSum - x.[38]) * x.[38]
                -63.7870836148508 * x.[38] * x.[68] // BAB + abb | SedimentationDirectName: BAB + abb -> 6 Y
                -0.1 * x.[38] // BAB | LigationName: B + AB <-> BAB
                0.1 * x.[2] * x.[6] // B + AB | LigationName: B + AB <-> BAB
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 39 - BAa
            [|
                -kW * (2.0 * xSum - x.[39]) * x.[39]
                -53.9468979738308 * x.[39] * x.[76] // BAa + bBb | SedimentationDirectName: BAa + bBb -> 6 Y
                -0.970384360209309 * x.[39] * x.[71] // BAa + bAa | SedimentationDirectName: BAa + bAa -> 6 Y
                -0.1 * x.[39] // BAa | LigationName: B + Aa <-> BAa
                0.1 * x.[2] * x.[7] // B + Aa | LigationName: B + Aa <-> BAa
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 40 - BAb
            [|
                -kW * (2.0 * xSum - x.[40]) * x.[40]
                -38.5573268466612 * x.[17] * x.[40] // bA + BAb | SedimentationDirectName: bA + BAb -> 5 Y
                -0.1 * x.[40] // BAb | LigationName: B + Ab <-> BAb
                0.1 * x.[2] * x.[8] // B + Ab | LigationName: B + Ab <-> BAb
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 41 - BBA
            [|
                -kW * (2.0 * xSum - x.[41]) * x.[41]
                -53.5824645556566 * x.[41] * x.[60] // BBA + aBb | SedimentationDirectName: BBA + aBb -> 6 Y
                -25.9563678497145 * x.[15] * x.[41] // aa + BBA | SedimentationDirectName: aa + BBA -> 5 Y
                -0.1 * x.[41] // BBA | LigationName: B + BA <-> BBA
                0.1 * x.[2] * x.[9] // B + BA | LigationName: B + BA <-> BBA
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 42 - BBB
            [|
                -kW * (2.0 * xSum - x.[42]) * x.[42]
                -25.7923415732481 * x.[73] * x.[42] // bBA + BBB | SedimentationDirectName: bBA + BBB -> 6 Y
                -15.8725584321517 * x.[15] * x.[42] // aa + BBB | SedimentationDirectName: aa + BBB -> 5 Y
                -0.1 * x.[42] // BBB | LigationName: B + BB <-> BBB
                0.1 * x.[2] * x.[10] // B + BB | LigationName: B + BB <-> BBB
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 43 - BBa
            [|
                -kW * (2.0 * xSum - x.[43]) * x.[43]
                -80.7994733689242 * x.[36] * x.[43] // Abb + BBa | SedimentationDirectName: Abb + BBa -> 6 Y
                -0.1 * x.[43] // BBa | LigationName: B + Ba <-> BBa
                0.1 * x.[2] * x.[11] // B + Ba | LigationName: B + Ba <-> BBa
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 44 - BBb
            [|
                -kW * (2.0 * xSum - x.[44]) * x.[44]
                -48.5001542234325 * x.[44] * x.[56] // BBb + aAb | SedimentationDirectName: BBb + aAb -> 6 Y
                -0.1 * x.[44] // BBb | LigationName: B + Bb <-> BBb
                0.1 * x.[2] * x.[12] // B + Bb | LigationName: B + Bb <-> BBb
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 45 - BaA
            [|
                -kW * (2.0 * xSum - x.[45]) * x.[45]
                -0.970384360209309 * x.[77] * x.[45] // baA + BaA | SedimentationDirectName: baA + BaA -> 6 Y
                -0.1 * x.[45] // BaA | LigationName: B + aA <-> BaA
                0.1 * x.[2] * x.[13] // B + aA | LigationName: B + aA <-> BaA
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 46 - BaB
            [|
                -kW * (2.0 * xSum - x.[46]) * x.[46]
                -7.14868381414817 * x.[26] * x.[46] // ABB + BaB | SedimentationDirectName: ABB + BaB -> 6 Y
                -0.1 * x.[46] // BaB | LigationName: B + aB <-> BaB
                0.1 * x.[2] * x.[14] // B + aB | LigationName: B + aB <-> BaB
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 47 - Baa
            [|
                -kW * (2.0 * xSum - x.[47]) * x.[47]
                -34.0491662737595 * x.[47] * x.[66] // Baa + abB | SedimentationDirectName: Baa + abB -> 6 Y
                -21.091020549205 * x.[67] * x.[47] // aba + Baa | SedimentationDirectName: aba + Baa -> 6 Y
                -0.1 * x.[47] // Baa | LigationName: B + aa <-> Baa
                0.1 * x.[2] * x.[15] // B + aa | LigationName: B + aa <-> Baa
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 48 - Bab
            [|
                -kW * (2.0 * xSum - x.[48]) * x.[48]
                -0.1 * x.[48] // Bab | LigationName: B + ab <-> Bab
                0.1 * x.[2] * x.[16] // B + ab | LigationName: B + ab <-> Bab
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 49 - BbA
            [|
                -kW * (2.0 * xSum - x.[49]) * x.[49]
                -0.1 * x.[49] // BbA | LigationName: B + bA <-> BbA
                0.1 * x.[2] * x.[17] // B + bA | LigationName: B + bA <-> BbA
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 50 - BbB
            [|
                -kW * (2.0 * xSum - x.[50]) * x.[50]
                -53.9468979738308 * x.[77] * x.[50] // baA + BbB | SedimentationDirectName: baA + BbB -> 6 Y
                -0.1 * x.[50] // BbB | LigationName: B + bB <-> BbB
                0.1 * x.[2] * x.[18] // B + bB | LigationName: B + bB <-> BbB
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 51 - Bba
            [|
                -kW * (2.0 * xSum - x.[51]) * x.[51]
                -25.7923415732481 * x.[51] * x.[84] // Bba + bbb | SedimentationDirectName: Bba + bbb -> 6 Y
                -22.1393742250857 * x.[79] * x.[51] // baa + Bba | SedimentationDirectName: baa + Bba -> 6 Y
                -0.1 * x.[51] // Bba | LigationName: B + ba <-> Bba
                0.1 * x.[2] * x.[19] // B + ba | LigationName: B + ba <-> Bba
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 52 - Bbb
            [|
                -kW * (2.0 * xSum - x.[52]) * x.[52]
                -0.1 * x.[52] // Bbb | LigationName: B + bb <-> Bbb
                0.1 * x.[2] * x.[20] // B + bb | LigationName: B + bb <-> Bbb
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 53 - aAA
            [|
                -kW * (2.0 * xSum - x.[53]) * x.[53]
                -34.1421760416434 * x.[53] * x.[25] // aAA + ABA | SedimentationDirectName: aAA + ABA -> 6 Y
                -0.1 * x.[53] // aAA | LigationName: a + AA <-> aAA
                0.1 * x.[3] * x.[5] // a + AA | LigationName: a + AA <-> aAA
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 54 - aAB
            [|
                -kW * (2.0 * xSum - x.[54]) * x.[54]
                -74.0453128255527 * x.[54] * x.[23] // aAB + AAa | SedimentationDirectName: aAB + AAa -> 6 Y
                -0.1 * x.[54] // aAB | LigationName: a + AB <-> aAB
                0.1 * x.[3] * x.[6] // a + AB | LigationName: a + AB <-> aAB
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 55 - aAa
            [|
                -kW * (2.0 * xSum - x.[55]) * x.[55]
                -3.19070828671931 * x.[13] * x.[55] // aA + aAa | SedimentationDirectName: aA + aAa -> 5 Y
                -0.1 * x.[55] // aAa | LigationName: a + Aa <-> aAa
                0.1 * x.[3] * x.[7] // a + Aa | LigationName: a + Aa <-> aAa
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 56 - aAb
            [|
                -kW * (2.0 * xSum - x.[56]) * x.[56]
                -48.5001542234325 * x.[44] * x.[56] // BBb + aAb | SedimentationDirectName: BBb + aAb -> 6 Y
                -21.3893445489625 * x.[56] * x.[25] // aAb + ABA | SedimentationDirectName: aAb + ABA -> 6 Y
                -82.6337629450791 * x.[56] * x.[34] // aAb + AbB | SedimentationDirectName: aAb + AbB -> 6 Y
                -29.7789028491576 * x.[28] * x.[56] // ABb + aAb | SedimentationDirectName: ABb + aAb -> 6 Y
                -0.1 * x.[56] // aAb | LigationName: a + Ab <-> aAb
                0.1 * x.[3] * x.[8] // a + Ab | LigationName: a + Ab <-> aAb
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 57 - aBA
            [|
                -kW * (2.0 * xSum - x.[57]) * x.[57]
                -55.9295153352086 * x.[5] * x.[57] // AA + aBA | SedimentationDirectName: AA + aBA -> 5 Y
                -0.1 * x.[57] // aBA | LigationName: a + BA <-> aBA
                0.1 * x.[3] * x.[9] // a + BA | LigationName: a + BA <-> aBA
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 58 - aBB
            [|
                -kW * (2.0 * xSum - x.[58]) * x.[58]
                -80.7994733689242 * x.[58] * x.[81] // aBB + bbA | SedimentationDirectName: aBB + bbA -> 6 Y
                -32.5643983434404 * x.[62] * x.[58] // aaB + aBB | SedimentationDirectName: aaB + aBB -> 6 Y
                -0.1 * x.[58] // aBB | LigationName: a + BB <-> aBB
                0.1 * x.[3] * x.[10] // a + BB | LigationName: a + BB <-> aBB
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 59 - aBa
            [|
                -kW * (2.0 * xSum - x.[59]) * x.[59]
                -0.1 * x.[59] // aBa | LigationName: a + Ba <-> aBa
                0.1 * x.[3] * x.[11] // a + Ba | LigationName: a + Ba <-> aBa
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 60 - aBb
            [|
                -kW * (2.0 * xSum - x.[60]) * x.[60]
                -53.5824645556566 * x.[41] * x.[60] // BBA + aBb | SedimentationDirectName: BBA + aBb -> 6 Y
                -82.6337629450791 * x.[30] * x.[60] // AaB + aBb | SedimentationDirectName: AaB + aBb -> 6 Y
                -0.1 * x.[60] // aBb | LigationName: a + Bb <-> aBb
                0.1 * x.[3] * x.[12] // a + Bb | LigationName: a + Bb <-> aBb
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 61 - aaA
            [|
                -kW * (2.0 * xSum - x.[61]) * x.[61]
                -74.0453128255527 * x.[32] * x.[61] // Aab + aaA | SedimentationDirectName: Aab + aaA -> 6 Y
                -0.1 * x.[61] // aaA | LigationName: a + aA <-> aaA
                0.1 * x.[3] * x.[13] // a + aA | LigationName: a + aA <-> aaA
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 62 - aaB
            [|
                -kW * (2.0 * xSum - x.[62]) * x.[62]
                -32.5643983434404 * x.[62] * x.[58] // aaB + aBB | SedimentationDirectName: aaB + aBB -> 6 Y
                -69.5562059032595 * x.[16] * x.[62] // ab + aaB | SedimentationDirectName: ab + aaB -> 5 Y
                -2.4361881551451 * x.[62] * x.[38] // aaB + BAB | CatalyticLigationName: a + aB + BAB <-> aaB + BAB
                2.4361881551451 * x.[3] * x.[14] * x.[38] // a + aB + BAB | CatalyticLigationName: a + aB + BAB <-> aaB + BAB
                -2.44902258657842 * x.[62] * x.[80] // aaB + bab | CatalyticLigationName: a + aB + bab <-> aaB + bab
                2.44902258657842 * x.[3] * x.[14] * x.[80] // a + aB + bab | CatalyticLigationName: a + aB + bab <-> aaB + bab
                -0.1 * x.[62] // aaB | LigationName: a + aB <-> aaB
                0.1 * x.[3] * x.[14] // a + aB | LigationName: a + aB <-> aaB
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 63 - aaa
            [|
                -kW * (2.0 * xSum - x.[63]) * x.[63]
                -3.21393117441257 * x.[25] * x.[63] // ABA + aaa | SedimentationDirectName: ABA + aaa -> 6 Y
                -0.1 * x.[63] // aaa | LigationName: a + aa <-> aaa
                0.1 * x.[3] * x.[15] // a + aa | LigationName: a + aa <-> aaa
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 64 - aab
            [|
                -kW * (2.0 * xSum - x.[64]) * x.[64]
                -0.1 * x.[64] // aab | LigationName: a + ab <-> aab
                0.1 * x.[3] * x.[16] // a + ab | LigationName: a + ab <-> aab
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 65 - abA
            [|
                -kW * (2.0 * xSum - x.[65]) * x.[65]
                -12.558847104467 * x.[25] * x.[65] // ABA + abA | SedimentationDirectName: ABA + abA -> 6 Y
                -47.2125087225556 * x.[14] * x.[65] // aB + abA | SedimentationDirectName: aB + abA -> 5 Y
                -0.1 * x.[65] // abA | LigationName: a + bA <-> abA
                0.1 * x.[3] * x.[17] // a + bA | LigationName: a + bA <-> abA
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 66 - abB
            [|
                -kW * (2.0 * xSum - x.[66]) * x.[66]
                -34.0491662737595 * x.[47] * x.[66] // Baa + abB | SedimentationDirectName: Baa + abB -> 6 Y
                -29.7789028491576 * x.[66] * x.[30] // abB + AaB | SedimentationDirectName: abB + AaB -> 6 Y
                -0.1 * x.[66] // abB | LigationName: a + bB <-> abB
                0.1 * x.[3] * x.[18] // a + bB | LigationName: a + bB <-> abB
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 67 - aba
            [|
                -kW * (2.0 * xSum - x.[67]) * x.[67]
                -34.1421760416434 * x.[31] * x.[67] // Aaa + aba | SedimentationDirectName: Aaa + aba -> 6 Y
                -21.3893445489625 * x.[30] * x.[67] // AaB + aba | SedimentationDirectName: AaB + aba -> 6 Y
                -21.091020549205 * x.[67] * x.[47] // aba + Baa | SedimentationDirectName: aba + Baa -> 6 Y
                -12.558847104467 * x.[67] * x.[27] // aba + ABa | SedimentationDirectName: aba + ABa -> 6 Y
                -3.21393117441257 * x.[67] * x.[21] // aba + AAA | SedimentationDirectName: aba + AAA -> 6 Y
                -74.8402332401258 * x.[13] * x.[67] // aA + aba | SedimentationDirectName: aA + aba -> 5 Y
                -0.1 * x.[67] // aba | LigationName: a + ba <-> aba
                0.1 * x.[3] * x.[19] // a + ba | LigationName: a + ba <-> aba
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 68 - abb
            [|
                -kW * (2.0 * xSum - x.[68]) * x.[68]
                -63.7870836148508 * x.[38] * x.[68] // BAB + abb | SedimentationDirectName: BAB + abb -> 6 Y
                -7.14868381414817 * x.[68] * x.[72] // abb + bAb | SedimentationDirectName: abb + bAb -> 6 Y
                -0.1 * x.[68] // abb | LigationName: a + bb <-> abb
                0.1 * x.[3] * x.[20] // a + bb | LigationName: a + bb <-> abb
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 69 - bAA
            [|
                -kW * (2.0 * xSum - x.[69]) * x.[69]
                -34.0491662737595 * x.[69] * x.[28] // bAA + ABb | SedimentationDirectName: bAA + ABb -> 6 Y
                -21.091020549205 * x.[25] * x.[69] // ABA + bAA | SedimentationDirectName: ABA + bAA -> 6 Y
                -0.1 * x.[69] // bAA | LigationName: b + AA <-> bAA
                0.1 * x.[4] * x.[5] // b + AA | LigationName: b + AA <-> bAA
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 70 - bAB
            [|
                -kW * (2.0 * xSum - x.[70]) * x.[70]
                -0.1 * x.[70] // bAB | LigationName: b + AB <-> bAB
                0.1 * x.[4] * x.[6] // b + AB | LigationName: b + AB <-> bAB
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 71 - bAa
            [|
                -kW * (2.0 * xSum - x.[71]) * x.[71]
                -0.970384360209309 * x.[39] * x.[71] // BAa + bAa | SedimentationDirectName: BAa + bAa -> 6 Y
                -0.1 * x.[71] // bAa | LigationName: b + Aa <-> bAa
                0.1 * x.[4] * x.[7] // b + Aa | LigationName: b + Aa <-> bAa
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 72 - bAb
            [|
                -kW * (2.0 * xSum - x.[72]) * x.[72]
                -7.14868381414817 * x.[68] * x.[72] // abb + bAb | SedimentationDirectName: abb + bAb -> 6 Y
                -0.1 * x.[72] // bAb | LigationName: b + Ab <-> bAb
                0.1 * x.[4] * x.[8] // b + Ab | LigationName: b + Ab <-> bAb
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 73 - bBA
            [|
                -kW * (2.0 * xSum - x.[73]) * x.[73]
                -25.7923415732481 * x.[73] * x.[42] // bBA + BBB | SedimentationDirectName: bBA + BBB -> 6 Y
                -22.1393742250857 * x.[37] * x.[73] // BAA + bBA | SedimentationDirectName: BAA + bBA -> 6 Y
                -0.1 * x.[73] // bBA | LigationName: b + BA <-> bBA
                0.1 * x.[4] * x.[9] // b + BA | LigationName: b + BA <-> bBA
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 74 - bBB
            [|
                -kW * (2.0 * xSum - x.[74]) * x.[74]
                -0.1 * x.[74] // bBB | LigationName: b + BB <-> bBB
                0.1 * x.[4] * x.[10] // b + BB | LigationName: b + BB <-> bBB
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 75 - bBa
            [|
                -kW * (2.0 * xSum - x.[75]) * x.[75]
                -0.1 * x.[75] // bBa | LigationName: b + Ba <-> bBa
                0.1 * x.[4] * x.[11] // b + Ba | LigationName: b + Ba <-> bBa
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 76 - bBb
            [|
                -kW * (2.0 * xSum - x.[76]) * x.[76]
                -53.9468979738308 * x.[39] * x.[76] // BAa + bBb | SedimentationDirectName: BAa + bBb -> 6 Y
                -0.1 * x.[76] // bBb | LigationName: b + Bb <-> bBb
                0.1 * x.[4] * x.[12] // b + Bb | LigationName: b + Bb <-> bBb
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 77 - baA
            [|
                -kW * (2.0 * xSum - x.[77]) * x.[77]
                -53.9468979738308 * x.[77] * x.[50] // baA + BbB | SedimentationDirectName: baA + BbB -> 6 Y
                -0.970384360209309 * x.[77] * x.[45] // baA + BaA | SedimentationDirectName: baA + BaA -> 6 Y
                -0.1 * x.[77] // baA | LigationName: b + aA <-> baA
                0.1 * x.[4] * x.[13] // b + aA | LigationName: b + aA <-> baA
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 78 - baB
            [|
                -kW * (2.0 * xSum - x.[78]) * x.[78]
                -38.5573268466612 * x.[11] * x.[78] // Ba + baB | SedimentationDirectName: Ba + baB -> 5 Y
                -0.1 * x.[78] // baB | LigationName: b + aB <-> baB
                0.1 * x.[4] * x.[14] // b + aB | LigationName: b + aB <-> baB
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 79 - baa
            [|
                -kW * (2.0 * xSum - x.[79]) * x.[79]
                -22.1393742250857 * x.[79] * x.[51] // baa + Bba | SedimentationDirectName: baa + Bba -> 6 Y
                -66.4258069092912 * x.[17] * x.[79] // bA + baa | SedimentationDirectName: bA + baa -> 5 Y
                -21.5306142530327 * x.[9] * x.[79] // BA + baa | SedimentationDirectName: BA + baa -> 5 Y
                -0.1 * x.[79] // baa | LigationName: b + aa <-> baa
                0.1 * x.[4] * x.[15] // b + aa | LigationName: b + aa <-> baa
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 80 - bab
            [|
                -kW * (2.0 * xSum - x.[80]) * x.[80]
                -63.7870836148508 * x.[80] * x.[26] // bab + ABB | SedimentationDirectName: bab + ABB -> 6 Y
                -0.1 * x.[80] // bab | LigationName: b + ab <-> bab
                0.1 * x.[4] * x.[16] // b + ab | LigationName: b + ab <-> bab
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 81 - bbA
            [|
                -kW * (2.0 * xSum - x.[81]) * x.[81]
                -80.7994733689242 * x.[58] * x.[81] // aBB + bbA | SedimentationDirectName: aBB + bbA -> 6 Y
                -0.1 * x.[81] // bbA | LigationName: b + bA <-> bbA
                0.1 * x.[4] * x.[17] // b + bA | LigationName: b + bA <-> bbA
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 82 - bbB
            [|
                -kW * (2.0 * xSum - x.[82]) * x.[82]
                -48.5001542234325 * x.[82] * x.[30] // bbB + AaB | SedimentationDirectName: bbB + AaB -> 6 Y
                -0.1 * x.[82] // bbB | LigationName: b + bB <-> bbB
                0.1 * x.[4] * x.[18] // b + bB | LigationName: b + bB <-> bbB
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 83 - bba
            [|
                -kW * (2.0 * xSum - x.[83]) * x.[83]
                -53.5824645556566 * x.[83] * x.[34] // bba + AbB | SedimentationDirectName: bba + AbB -> 6 Y
                -25.9563678497145 * x.[5] * x.[83] // AA + bba | SedimentationDirectName: AA + bba -> 5 Y
                -0.1 * x.[83] // bba | LigationName: b + ba <-> bba
                0.1 * x.[4] * x.[19] // b + ba | LigationName: b + ba <-> bba
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 84 - bbb
            [|
                -kW * (2.0 * xSum - x.[84]) * x.[84]
                -25.7923415732481 * x.[51] * x.[84] // Bba + bbb | SedimentationDirectName: Bba + bbb -> 6 Y
                -15.8725584321517 * x.[5] * x.[84] // AA + bbb | SedimentationDirectName: AA + bbb -> 5 Y
                -0.1 * x.[84] // bbb | LigationName: b + bb <-> bbb
                0.1 * x.[4] * x.[20] // b + bb | LigationName: b + bb <-> bbb
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0

        |]


    let modelDataParams = 
        {
            numberOfSubstances = 85
            numberOfAminoAcids = TwoAminoAcids
            maxPeptideLength = ThreeMax
            getTotals = getTotals
            getTotalSubst = getTotalSubst
        }

