namespace Model

open Clm.Substances
open Clm.Model


module ModelData = 

    let seedValue = 12345
    let numberOfAminoAcids = NumberOfAminoAcids.TwoAminoAcids
    let maxPeptideLength = MaxPeptideLength.ThreeMax
    let numberOfSubstances = 85



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
                5.63057838270002E-05 * (2.0 * xSum * xSumN - xSumSquaredN)
                6.0 * 17.954704695951 * x.[76] * x.[42] // bBb + BBB | SedimentationDirectName: bBb + BBB -> 6 Y
                6.0 * 17.954704695951 * x.[50] * x.[84] // BbB + bbb | SedimentationDirectName: BbB + bbb -> 6 Y
                6.0 * 7.59615840450121 * x.[76] * x.[35] // bBb + Aba | SedimentationDirectName: bBb + Aba -> 6 Y
                6.0 * 7.59615840450121 * x.[50] * x.[57] // BbB + aBA | SedimentationDirectName: BbB + aBA -> 6 Y
                6.0 * 20.3315239059183 * x.[82] * x.[27] // bbB + ABa | SedimentationDirectName: bbB + ABa -> 6 Y
                6.0 * 20.3315239059183 * x.[44] * x.[65] // BBb + abA | SedimentationDirectName: BBb + abA -> 6 Y
                6.0 * 11.1827526720811 * x.[81] * x.[40] // bbA + BAb | SedimentationDirectName: bbA + BAb -> 6 Y
                6.0 * 11.1827526720811 * x.[43] * x.[78] // BBa + baB | SedimentationDirectName: BBa + baB -> 6 Y
                6.0 * 12.497743307692 * x.[84] * x.[34] // bbb + AbB | SedimentationDirectName: bbb + AbB -> 6 Y
                6.0 * 12.497743307692 * x.[42] * x.[60] // BBB + aBb | SedimentationDirectName: BBB + aBb -> 6 Y
                6.0 * 15.1873530633643 * x.[83] * x.[74] // bba + bBB | SedimentationDirectName: bba + bBB -> 6 Y
                6.0 * 15.1873530633643 * x.[41] * x.[52] // BBA + Bbb | SedimentationDirectName: BBA + Bbb -> 6 Y
                6.0 * 20.315272434543 * x.[58] * x.[40] // aBB + BAb | SedimentationDirectName: aBB + BAb -> 6 Y
                6.0 * 20.315272434543 * x.[36] * x.[78] // Abb + baB | SedimentationDirectName: Abb + baB -> 6 Y
                6.0 * 58.4584523985377 * x.[57] * x.[47] // aBA + Baa | SedimentationDirectName: aBA + Baa -> 6 Y
                6.0 * 58.4584523985377 * x.[35] * x.[69] // Aba + bAA | SedimentationDirectName: Aba + bAA -> 6 Y
                6.0 * 77.7817604984622 * x.[59] * x.[25] // aBa + ABA | SedimentationDirectName: aBa + ABA -> 6 Y
                6.0 * 77.7817604984622 * x.[33] * x.[67] // AbA + aba | SedimentationDirectName: AbA + aba -> 6 Y
                6.0 * 44.4986243599349 * x.[55] * x.[31] // aAa + Aaa | SedimentationDirectName: aAa + Aaa -> 6 Y
                6.0 * 44.4986243599349 * x.[29] * x.[53] // AaA + aAA | SedimentationDirectName: AaA + aAA -> 6 Y
                6.0 * 73.4172518615685 * x.[65] * x.[32] // abA + Aab | SedimentationDirectName: abA + Aab -> 6 Y
                6.0 * 73.4172518615685 * x.[27] * x.[54] // ABa + aAB | SedimentationDirectName: ABa + aAB -> 6 Y
                5.0 * 27.2917476778003 * x.[17] * x.[35] // bA + Aba | SedimentationDirectName: bA + Aba -> 5 Y
                5.0 * 27.2917476778003 * x.[11] * x.[57] // Ba + aBA | SedimentationDirectName: Ba + aBA -> 5 Y
                5.0 * 54.0764426321166 * x.[20] * x.[54] // bb + aAB | SedimentationDirectName: bb + aAB -> 5 Y
                5.0 * 54.0764426321166 * x.[10] * x.[32] // BB + Aab | SedimentationDirectName: BB + Aab -> 5 Y
                5.0 * 12.4492769460629 * x.[19] * x.[38] // ba + BAB | SedimentationDirectName: ba + BAB -> 5 Y
                5.0 * 12.4492769460629 * x.[9] * x.[80] // BA + bab | SedimentationDirectName: BA + bab -> 5 Y
                5.0 * 70.6753181586129 * x.[19] * x.[25] // ba + ABA | SedimentationDirectName: ba + ABA -> 5 Y
                5.0 * 70.6753181586129 * x.[9] * x.[67] // BA + aba | SedimentationDirectName: BA + aba -> 5 Y
                5.0 * 36.8637857212933 * x.[19] * x.[71] // ba + bAa | SedimentationDirectName: ba + bAa -> 5 Y
                5.0 * 36.8637857212933 * x.[9] * x.[45] // BA + BaA | SedimentationDirectName: BA + BaA -> 5 Y
                4.0 * 19.1967096834449 * x.[14] * x.[11] // aB + Ba | SedimentationDirectName: aB + Ba -> 4 Y
                4.0 * 19.1967096834449 * x.[8] * x.[17] // Ab + bA | SedimentationDirectName: Ab + bA -> 4 Y
                5.0 * 39.2919001150608 * x.[16] * x.[80] // ab + bab | SedimentationDirectName: ab + bab -> 5 Y
                5.0 * 39.2919001150608 * x.[6] * x.[38] // AB + BAB | SedimentationDirectName: AB + BAB -> 5 Y
                5.0 * 17.045126751113 * x.[16] * x.[57] // ab + aBA | SedimentationDirectName: ab + aBA -> 5 Y
                5.0 * 17.045126751113 * x.[6] * x.[35] // AB + Aba | SedimentationDirectName: AB + Aba -> 5 Y
                3.0 * 20.3042514176972 * x.[4] * x.[10] // b + BB | SedimentationDirectName: b + BB -> 3 Y
                3.0 * 20.3042514176972 * x.[2] * x.[20] // B + bb | SedimentationDirectName: B + bb -> 3 Y
                2.0 * 4.50310328441251 * x.[4] * x.[1] // b + A | SedimentationDirectName: b + A -> 2 Y
                2.0 * 4.50310328441251 * x.[2] * x.[3] // B + a | SedimentationDirectName: B + a -> 2 Y
                4.0 * 32.069030234827 * x.[3] * x.[42] // a + BBB | SedimentationDirectName: a + BBB -> 4 Y
                4.0 * 32.069030234827 * x.[1] * x.[84] // A + bbb | SedimentationDirectName: A + bbb -> 4 Y
                4.0 * 1.80423188083245 * x.[3] * x.[37] // a + BAA | SedimentationDirectName: a + BAA -> 4 Y
                4.0 * 1.80423188083245 * x.[1] * x.[79] // A + baa | SedimentationDirectName: A + baa -> 4 Y
                9.6686867110751E-05 * x.[4] // b | SynthesisName: Y <-> b
                -0.00096686867110751 * x.[0] // Y | SynthesisName: Y <-> b
                9.6686867110751E-05 * x.[2] // B | SynthesisName: Y <-> B
                -0.00096686867110751 * x.[0] // Y | SynthesisName: Y <-> B
                0.00024793709127602 * x.[3] // a | SynthesisName: Y <-> a
                -0.0024793709127602 * x.[0] // Y | SynthesisName: Y <-> a
                0.00024793709127602 * x.[1] // A | SynthesisName: Y <-> A
                -0.0024793709127602 * x.[0] // Y | SynthesisName: Y <-> A
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 1 - A
            [|
                -5.63057838270002E-05 * (2.0 * xSum - x.[1]) * x.[1]
                -4.50310328441251 * x.[4] * x.[1] // b + A | SedimentationDirectName: b + A -> 2 Y
                -32.069030234827 * x.[1] * x.[84] // A + bbb | SedimentationDirectName: A + bbb -> 4 Y
                -1.80423188083245 * x.[1] * x.[79] // A + baa | SedimentationDirectName: A + baa -> 4 Y
                -0.00024793709127602 * x.[1] // A | SynthesisName: Y <-> A
                0.0024793709127602 * x.[0] // Y | SynthesisName: Y <-> A
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 2 - B
            [|
                -5.63057838270002E-05 * (2.0 * xSum - x.[2]) * x.[2]
                -20.3042514176972 * x.[2] * x.[20] // B + bb | SedimentationDirectName: B + bb -> 3 Y
                -4.50310328441251 * x.[2] * x.[3] // B + a | SedimentationDirectName: B + a -> 2 Y
                -9.6686867110751E-05 * x.[2] // B | SynthesisName: Y <-> B
                0.00096686867110751 * x.[0] // Y | SynthesisName: Y <-> B
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 3 - a
            [|
                -5.63057838270002E-05 * (2.0 * xSum - x.[3]) * x.[3]
                -4.50310328441251 * x.[2] * x.[3] // B + a | SedimentationDirectName: B + a -> 2 Y
                -32.069030234827 * x.[3] * x.[42] // a + BBB | SedimentationDirectName: a + BBB -> 4 Y
                -1.80423188083245 * x.[3] * x.[37] // a + BAA | SedimentationDirectName: a + BAA -> 4 Y
                -0.00024793709127602 * x.[3] // a | SynthesisName: Y <-> a
                0.0024793709127602 * x.[0] // Y | SynthesisName: Y <-> a
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 4 - b
            [|
                -5.63057838270002E-05 * (2.0 * xSum - x.[4]) * x.[4]
                -20.3042514176972 * x.[4] * x.[10] // b + BB | SedimentationDirectName: b + BB -> 3 Y
                -4.50310328441251 * x.[4] * x.[1] // b + A | SedimentationDirectName: b + A -> 2 Y
                -9.6686867110751E-05 * x.[4] // b | SynthesisName: Y <-> b
                0.00096686867110751 * x.[0] // Y | SynthesisName: Y <-> b
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 5 - AA
            [|
                -5.63057838270002E-05 * (2.0 * xSum - x.[5]) * x.[5]
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 6 - AB
            [|
                -5.63057838270002E-05 * (2.0 * xSum - x.[6]) * x.[6]
                -39.2919001150608 * x.[6] * x.[38] // AB + BAB | SedimentationDirectName: AB + BAB -> 5 Y
                -17.045126751113 * x.[6] * x.[35] // AB + Aba | SedimentationDirectName: AB + Aba -> 5 Y
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 7 - Aa
            [|
                -5.63057838270002E-05 * (2.0 * xSum - x.[7]) * x.[7]
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 8 - Ab
            [|
                -5.63057838270002E-05 * (2.0 * xSum - x.[8]) * x.[8]
                -19.1967096834449 * x.[8] * x.[17] // Ab + bA | SedimentationDirectName: Ab + bA -> 4 Y
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 9 - BA
            [|
                -5.63057838270002E-05 * (2.0 * xSum - x.[9]) * x.[9]
                -12.4492769460629 * x.[9] * x.[80] // BA + bab | SedimentationDirectName: BA + bab -> 5 Y
                -70.6753181586129 * x.[9] * x.[67] // BA + aba | SedimentationDirectName: BA + aba -> 5 Y
                -36.8637857212933 * x.[9] * x.[45] // BA + BaA | SedimentationDirectName: BA + BaA -> 5 Y
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 10 - BB
            [|
                -5.63057838270002E-05 * (2.0 * xSum - x.[10]) * x.[10]
                -54.0764426321166 * x.[10] * x.[32] // BB + Aab | SedimentationDirectName: BB + Aab -> 5 Y
                -20.3042514176972 * x.[4] * x.[10] // b + BB | SedimentationDirectName: b + BB -> 3 Y
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 11 - Ba
            [|
                -5.63057838270002E-05 * (2.0 * xSum - x.[11]) * x.[11]
                -27.2917476778003 * x.[11] * x.[57] // Ba + aBA | SedimentationDirectName: Ba + aBA -> 5 Y
                -19.1967096834449 * x.[14] * x.[11] // aB + Ba | SedimentationDirectName: aB + Ba -> 4 Y
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 12 - Bb
            [|
                -5.63057838270002E-05 * (2.0 * xSum - x.[12]) * x.[12]
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 13 - aA
            [|
                -5.63057838270002E-05 * (2.0 * xSum - x.[13]) * x.[13]
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 14 - aB
            [|
                -5.63057838270002E-05 * (2.0 * xSum - x.[14]) * x.[14]
                -19.1967096834449 * x.[14] * x.[11] // aB + Ba | SedimentationDirectName: aB + Ba -> 4 Y
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 15 - aa
            [|
                -5.63057838270002E-05 * (2.0 * xSum - x.[15]) * x.[15]
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 16 - ab
            [|
                -5.63057838270002E-05 * (2.0 * xSum - x.[16]) * x.[16]
                -39.2919001150608 * x.[16] * x.[80] // ab + bab | SedimentationDirectName: ab + bab -> 5 Y
                -17.045126751113 * x.[16] * x.[57] // ab + aBA | SedimentationDirectName: ab + aBA -> 5 Y
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 17 - bA
            [|
                -5.63057838270002E-05 * (2.0 * xSum - x.[17]) * x.[17]
                -27.2917476778003 * x.[17] * x.[35] // bA + Aba | SedimentationDirectName: bA + Aba -> 5 Y
                -19.1967096834449 * x.[8] * x.[17] // Ab + bA | SedimentationDirectName: Ab + bA -> 4 Y
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 18 - bB
            [|
                -5.63057838270002E-05 * (2.0 * xSum - x.[18]) * x.[18]
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 19 - ba
            [|
                -5.63057838270002E-05 * (2.0 * xSum - x.[19]) * x.[19]
                -12.4492769460629 * x.[19] * x.[38] // ba + BAB | SedimentationDirectName: ba + BAB -> 5 Y
                -70.6753181586129 * x.[19] * x.[25] // ba + ABA | SedimentationDirectName: ba + ABA -> 5 Y
                -36.8637857212933 * x.[19] * x.[71] // ba + bAa | SedimentationDirectName: ba + bAa -> 5 Y
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 20 - bb
            [|
                -5.63057838270002E-05 * (2.0 * xSum - x.[20]) * x.[20]
                -54.0764426321166 * x.[20] * x.[54] // bb + aAB | SedimentationDirectName: bb + aAB -> 5 Y
                -20.3042514176972 * x.[2] * x.[20] // B + bb | SedimentationDirectName: B + bb -> 3 Y
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 21 - AAA
            [|
                -5.63057838270002E-05 * (2.0 * xSum - x.[21]) * x.[21]
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 22 - AAB
            [|
                -5.63057838270002E-05 * (2.0 * xSum - x.[22]) * x.[22]
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 23 - AAa
            [|
                -5.63057838270002E-05 * (2.0 * xSum - x.[23]) * x.[23]
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 24 - AAb
            [|
                -5.63057838270002E-05 * (2.0 * xSum - x.[24]) * x.[24]
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 25 - ABA
            [|
                -5.63057838270002E-05 * (2.0 * xSum - x.[25]) * x.[25]
                -77.7817604984622 * x.[59] * x.[25] // aBa + ABA | SedimentationDirectName: aBa + ABA -> 6 Y
                -70.6753181586129 * x.[19] * x.[25] // ba + ABA | SedimentationDirectName: ba + ABA -> 5 Y
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 26 - ABB
            [|
                -5.63057838270002E-05 * (2.0 * xSum - x.[26]) * x.[26]
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 27 - ABa
            [|
                -5.63057838270002E-05 * (2.0 * xSum - x.[27]) * x.[27]
                -20.3315239059183 * x.[82] * x.[27] // bbB + ABa | SedimentationDirectName: bbB + ABa -> 6 Y
                -73.4172518615685 * x.[27] * x.[54] // ABa + aAB | SedimentationDirectName: ABa + aAB -> 6 Y
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 28 - ABb
            [|
                -5.63057838270002E-05 * (2.0 * xSum - x.[28]) * x.[28]
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 29 - AaA
            [|
                -5.63057838270002E-05 * (2.0 * xSum - x.[29]) * x.[29]
                -44.4986243599349 * x.[29] * x.[53] // AaA + aAA | SedimentationDirectName: AaA + aAA -> 6 Y
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 30 - AaB
            [|
                -5.63057838270002E-05 * (2.0 * xSum - x.[30]) * x.[30]
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 31 - Aaa
            [|
                -5.63057838270002E-05 * (2.0 * xSum - x.[31]) * x.[31]
                -44.4986243599349 * x.[55] * x.[31] // aAa + Aaa | SedimentationDirectName: aAa + Aaa -> 6 Y
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 32 - Aab
            [|
                -5.63057838270002E-05 * (2.0 * xSum - x.[32]) * x.[32]
                -73.4172518615685 * x.[65] * x.[32] // abA + Aab | SedimentationDirectName: abA + Aab -> 6 Y
                -54.0764426321166 * x.[10] * x.[32] // BB + Aab | SedimentationDirectName: BB + Aab -> 5 Y
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 33 - AbA
            [|
                -5.63057838270002E-05 * (2.0 * xSum - x.[33]) * x.[33]
                -77.7817604984622 * x.[33] * x.[67] // AbA + aba | SedimentationDirectName: AbA + aba -> 6 Y
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 34 - AbB
            [|
                -5.63057838270002E-05 * (2.0 * xSum - x.[34]) * x.[34]
                -12.497743307692 * x.[84] * x.[34] // bbb + AbB | SedimentationDirectName: bbb + AbB -> 6 Y
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 35 - Aba
            [|
                -5.63057838270002E-05 * (2.0 * xSum - x.[35]) * x.[35]
                -7.59615840450121 * x.[76] * x.[35] // bBb + Aba | SedimentationDirectName: bBb + Aba -> 6 Y
                -58.4584523985377 * x.[35] * x.[69] // Aba + bAA | SedimentationDirectName: Aba + bAA -> 6 Y
                -27.2917476778003 * x.[17] * x.[35] // bA + Aba | SedimentationDirectName: bA + Aba -> 5 Y
                -17.045126751113 * x.[6] * x.[35] // AB + Aba | SedimentationDirectName: AB + Aba -> 5 Y
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 36 - Abb
            [|
                -5.63057838270002E-05 * (2.0 * xSum - x.[36]) * x.[36]
                -20.315272434543 * x.[36] * x.[78] // Abb + baB | SedimentationDirectName: Abb + baB -> 6 Y
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 37 - BAA
            [|
                -5.63057838270002E-05 * (2.0 * xSum - x.[37]) * x.[37]
                -1.80423188083245 * x.[3] * x.[37] // a + BAA | SedimentationDirectName: a + BAA -> 4 Y
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 38 - BAB
            [|
                -5.63057838270002E-05 * (2.0 * xSum - x.[38]) * x.[38]
                -12.4492769460629 * x.[19] * x.[38] // ba + BAB | SedimentationDirectName: ba + BAB -> 5 Y
                -39.2919001150608 * x.[6] * x.[38] // AB + BAB | SedimentationDirectName: AB + BAB -> 5 Y
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 39 - BAa
            [|
                -5.63057838270002E-05 * (2.0 * xSum - x.[39]) * x.[39]
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 40 - BAb
            [|
                -5.63057838270002E-05 * (2.0 * xSum - x.[40]) * x.[40]
                -11.1827526720811 * x.[81] * x.[40] // bbA + BAb | SedimentationDirectName: bbA + BAb -> 6 Y
                -20.315272434543 * x.[58] * x.[40] // aBB + BAb | SedimentationDirectName: aBB + BAb -> 6 Y
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 41 - BBA
            [|
                -5.63057838270002E-05 * (2.0 * xSum - x.[41]) * x.[41]
                -15.1873530633643 * x.[41] * x.[52] // BBA + Bbb | SedimentationDirectName: BBA + Bbb -> 6 Y
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 42 - BBB
            [|
                -5.63057838270002E-05 * (2.0 * xSum - x.[42]) * x.[42]
                -17.954704695951 * x.[76] * x.[42] // bBb + BBB | SedimentationDirectName: bBb + BBB -> 6 Y
                -12.497743307692 * x.[42] * x.[60] // BBB + aBb | SedimentationDirectName: BBB + aBb -> 6 Y
                -32.069030234827 * x.[3] * x.[42] // a + BBB | SedimentationDirectName: a + BBB -> 4 Y
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 43 - BBa
            [|
                -5.63057838270002E-05 * (2.0 * xSum - x.[43]) * x.[43]
                -11.1827526720811 * x.[43] * x.[78] // BBa + baB | SedimentationDirectName: BBa + baB -> 6 Y
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 44 - BBb
            [|
                -5.63057838270002E-05 * (2.0 * xSum - x.[44]) * x.[44]
                -20.3315239059183 * x.[44] * x.[65] // BBb + abA | SedimentationDirectName: BBb + abA -> 6 Y
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 45 - BaA
            [|
                -5.63057838270002E-05 * (2.0 * xSum - x.[45]) * x.[45]
                -36.8637857212933 * x.[9] * x.[45] // BA + BaA | SedimentationDirectName: BA + BaA -> 5 Y
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 46 - BaB
            [|
                -5.63057838270002E-05 * (2.0 * xSum - x.[46]) * x.[46]
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 47 - Baa
            [|
                -5.63057838270002E-05 * (2.0 * xSum - x.[47]) * x.[47]
                -58.4584523985377 * x.[57] * x.[47] // aBA + Baa | SedimentationDirectName: aBA + Baa -> 6 Y
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 48 - Bab
            [|
                -5.63057838270002E-05 * (2.0 * xSum - x.[48]) * x.[48]
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 49 - BbA
            [|
                -5.63057838270002E-05 * (2.0 * xSum - x.[49]) * x.[49]
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 50 - BbB
            [|
                -5.63057838270002E-05 * (2.0 * xSum - x.[50]) * x.[50]
                -17.954704695951 * x.[50] * x.[84] // BbB + bbb | SedimentationDirectName: BbB + bbb -> 6 Y
                -7.59615840450121 * x.[50] * x.[57] // BbB + aBA | SedimentationDirectName: BbB + aBA -> 6 Y
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 51 - Bba
            [|
                -5.63057838270002E-05 * (2.0 * xSum - x.[51]) * x.[51]
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 52 - Bbb
            [|
                -5.63057838270002E-05 * (2.0 * xSum - x.[52]) * x.[52]
                -15.1873530633643 * x.[41] * x.[52] // BBA + Bbb | SedimentationDirectName: BBA + Bbb -> 6 Y
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 53 - aAA
            [|
                -5.63057838270002E-05 * (2.0 * xSum - x.[53]) * x.[53]
                -44.4986243599349 * x.[29] * x.[53] // AaA + aAA | SedimentationDirectName: AaA + aAA -> 6 Y
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 54 - aAB
            [|
                -5.63057838270002E-05 * (2.0 * xSum - x.[54]) * x.[54]
                -73.4172518615685 * x.[27] * x.[54] // ABa + aAB | SedimentationDirectName: ABa + aAB -> 6 Y
                -54.0764426321166 * x.[20] * x.[54] // bb + aAB | SedimentationDirectName: bb + aAB -> 5 Y
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 55 - aAa
            [|
                -5.63057838270002E-05 * (2.0 * xSum - x.[55]) * x.[55]
                -44.4986243599349 * x.[55] * x.[31] // aAa + Aaa | SedimentationDirectName: aAa + Aaa -> 6 Y
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 56 - aAb
            [|
                -5.63057838270002E-05 * (2.0 * xSum - x.[56]) * x.[56]
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 57 - aBA
            [|
                -5.63057838270002E-05 * (2.0 * xSum - x.[57]) * x.[57]
                -7.59615840450121 * x.[50] * x.[57] // BbB + aBA | SedimentationDirectName: BbB + aBA -> 6 Y
                -58.4584523985377 * x.[57] * x.[47] // aBA + Baa | SedimentationDirectName: aBA + Baa -> 6 Y
                -27.2917476778003 * x.[11] * x.[57] // Ba + aBA | SedimentationDirectName: Ba + aBA -> 5 Y
                -17.045126751113 * x.[16] * x.[57] // ab + aBA | SedimentationDirectName: ab + aBA -> 5 Y
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 58 - aBB
            [|
                -5.63057838270002E-05 * (2.0 * xSum - x.[58]) * x.[58]
                -20.315272434543 * x.[58] * x.[40] // aBB + BAb | SedimentationDirectName: aBB + BAb -> 6 Y
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 59 - aBa
            [|
                -5.63057838270002E-05 * (2.0 * xSum - x.[59]) * x.[59]
                -77.7817604984622 * x.[59] * x.[25] // aBa + ABA | SedimentationDirectName: aBa + ABA -> 6 Y
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 60 - aBb
            [|
                -5.63057838270002E-05 * (2.0 * xSum - x.[60]) * x.[60]
                -12.497743307692 * x.[42] * x.[60] // BBB + aBb | SedimentationDirectName: BBB + aBb -> 6 Y
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 61 - aaA
            [|
                -5.63057838270002E-05 * (2.0 * xSum - x.[61]) * x.[61]
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 62 - aaB
            [|
                -5.63057838270002E-05 * (2.0 * xSum - x.[62]) * x.[62]
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 63 - aaa
            [|
                -5.63057838270002E-05 * (2.0 * xSum - x.[63]) * x.[63]
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 64 - aab
            [|
                -5.63057838270002E-05 * (2.0 * xSum - x.[64]) * x.[64]
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 65 - abA
            [|
                -5.63057838270002E-05 * (2.0 * xSum - x.[65]) * x.[65]
                -20.3315239059183 * x.[44] * x.[65] // BBb + abA | SedimentationDirectName: BBb + abA -> 6 Y
                -73.4172518615685 * x.[65] * x.[32] // abA + Aab | SedimentationDirectName: abA + Aab -> 6 Y
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 66 - abB
            [|
                -5.63057838270002E-05 * (2.0 * xSum - x.[66]) * x.[66]
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 67 - aba
            [|
                -5.63057838270002E-05 * (2.0 * xSum - x.[67]) * x.[67]
                -77.7817604984622 * x.[33] * x.[67] // AbA + aba | SedimentationDirectName: AbA + aba -> 6 Y
                -70.6753181586129 * x.[9] * x.[67] // BA + aba | SedimentationDirectName: BA + aba -> 5 Y
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 68 - abb
            [|
                -5.63057838270002E-05 * (2.0 * xSum - x.[68]) * x.[68]
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 69 - bAA
            [|
                -5.63057838270002E-05 * (2.0 * xSum - x.[69]) * x.[69]
                -58.4584523985377 * x.[35] * x.[69] // Aba + bAA | SedimentationDirectName: Aba + bAA -> 6 Y
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 70 - bAB
            [|
                -5.63057838270002E-05 * (2.0 * xSum - x.[70]) * x.[70]
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 71 - bAa
            [|
                -5.63057838270002E-05 * (2.0 * xSum - x.[71]) * x.[71]
                -36.8637857212933 * x.[19] * x.[71] // ba + bAa | SedimentationDirectName: ba + bAa -> 5 Y
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 72 - bAb
            [|
                -5.63057838270002E-05 * (2.0 * xSum - x.[72]) * x.[72]
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 73 - bBA
            [|
                -5.63057838270002E-05 * (2.0 * xSum - x.[73]) * x.[73]
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 74 - bBB
            [|
                -5.63057838270002E-05 * (2.0 * xSum - x.[74]) * x.[74]
                -15.1873530633643 * x.[83] * x.[74] // bba + bBB | SedimentationDirectName: bba + bBB -> 6 Y
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 75 - bBa
            [|
                -5.63057838270002E-05 * (2.0 * xSum - x.[75]) * x.[75]
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 76 - bBb
            [|
                -5.63057838270002E-05 * (2.0 * xSum - x.[76]) * x.[76]
                -17.954704695951 * x.[76] * x.[42] // bBb + BBB | SedimentationDirectName: bBb + BBB -> 6 Y
                -7.59615840450121 * x.[76] * x.[35] // bBb + Aba | SedimentationDirectName: bBb + Aba -> 6 Y
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 77 - baA
            [|
                -5.63057838270002E-05 * (2.0 * xSum - x.[77]) * x.[77]
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 78 - baB
            [|
                -5.63057838270002E-05 * (2.0 * xSum - x.[78]) * x.[78]
                -11.1827526720811 * x.[43] * x.[78] // BBa + baB | SedimentationDirectName: BBa + baB -> 6 Y
                -20.315272434543 * x.[36] * x.[78] // Abb + baB | SedimentationDirectName: Abb + baB -> 6 Y
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 79 - baa
            [|
                -5.63057838270002E-05 * (2.0 * xSum - x.[79]) * x.[79]
                -1.80423188083245 * x.[1] * x.[79] // A + baa | SedimentationDirectName: A + baa -> 4 Y
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 80 - bab
            [|
                -5.63057838270002E-05 * (2.0 * xSum - x.[80]) * x.[80]
                -12.4492769460629 * x.[9] * x.[80] // BA + bab | SedimentationDirectName: BA + bab -> 5 Y
                -39.2919001150608 * x.[16] * x.[80] // ab + bab | SedimentationDirectName: ab + bab -> 5 Y
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 81 - bbA
            [|
                -5.63057838270002E-05 * (2.0 * xSum - x.[81]) * x.[81]
                -11.1827526720811 * x.[81] * x.[40] // bbA + BAb | SedimentationDirectName: bbA + BAb -> 6 Y
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 82 - bbB
            [|
                -5.63057838270002E-05 * (2.0 * xSum - x.[82]) * x.[82]
                -20.3315239059183 * x.[82] * x.[27] // bbB + ABa | SedimentationDirectName: bbB + ABa -> 6 Y
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 83 - bba
            [|
                -5.63057838270002E-05 * (2.0 * xSum - x.[83]) * x.[83]
                -15.1873530633643 * x.[83] * x.[74] // bba + bBB | SedimentationDirectName: bba + bBB -> 6 Y
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 84 - bbb
            [|
                -5.63057838270002E-05 * (2.0 * xSum - x.[84]) * x.[84]
                -17.954704695951 * x.[50] * x.[84] // BbB + bbb | SedimentationDirectName: BbB + bbb -> 6 Y
                -12.497743307692 * x.[84] * x.[34] // bbb + AbB | SedimentationDirectName: bbb + AbB -> 6 Y
                -32.069030234827 * x.[1] * x.[84] // A + bbb | SedimentationDirectName: A + bbb -> 4 Y
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

