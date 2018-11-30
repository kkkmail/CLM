namespace Model

open Clm.Substances
open Clm.Model
open Clm.ReactionTypes
open Clm.ReactionRates

module ModelData = 
    let seedValue = 1330536848
    let numberOfAminoAcids = NumberOfAminoAcids.TwoAminoAcids
    let maxPeptideLength = MaxPeptideLength.ThreeMax
    let numberOfSubstances = 85

    let aminoAcids = AminoAcid.getAminoAcids numberOfAminoAcids
    let chiralAminoAcids = ChiralAminoAcid.getAminoAcids numberOfAminoAcids
    let peptides = Peptide.getPeptides maxPeptideLength numberOfAminoAcids

    let allSubst = 
        [ Substance.food ]
        @
        (chiralAminoAcids |> List.map (fun a -> Chiral a))
        @
        (peptides |> List.map (fun p -> PeptideChain p))

    let allInd = allSubst |> List.mapi (fun i s -> (s, i)) |> Map.ofList
    let kW = 0.0370283071124127 / 84.0


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



    // 0 - Y
    let d0 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            kW * (2.0 * xSum * xSumN - xSumSquaredN)
            0.0001 * x.[4] // b | synthesis: Y <-> b
            -0.001 * x.[0] // Y | synthesis: Y <-> b
            0.0001 * x.[2] // B | synthesis: Y <-> B
            -0.001 * x.[0] // Y | synthesis: Y <-> B
            0.0001 * x.[3] // a | synthesis: Y <-> a
            -0.001 * x.[0] // Y | synthesis: Y <-> a
            0.0001 * x.[1] // A | synthesis: Y <-> A
            -0.001 * x.[0] // Y | synthesis: Y <-> A
        |]
        |> Array.sum


    // 1 - A
    let d1 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -kW * (2.0 * xSum - x.[1]) * x.[1]
            0.0961330595979871 * x.[34] * x.[17] // AbB + bA | catalytic ligation: A + bB + bA <-> AbB + bA
            -0.961330595979871 * x.[1] * x.[18] * x.[17] // A + bB + bA | catalytic ligation: A + bB + bA <-> AbB + bA
            0.0001 * x.[17] // bA | ligation: b + A <-> bA
            -0.001 * x.[4] * x.[1] // b + A | ligation: b + A <-> bA
            0.0001 * x.[36] // Abb | ligation: A + bb <-> Abb
            -0.001 * x.[1] * x.[20] // A + bb | ligation: A + bb <-> Abb
            0.0001 * x.[35] // Aba | ligation: A + ba <-> Aba
            -0.001 * x.[1] * x.[19] // A + ba | ligation: A + ba <-> Aba
            0.0001 * x.[34] // AbB | ligation: A + bB <-> AbB
            -0.001 * x.[1] * x.[18] // A + bB | ligation: A + bB <-> AbB
            0.0001 * x.[33] // AbA | ligation: A + bA <-> AbA
            -0.001 * x.[1] * x.[17] // A + bA | ligation: A + bA <-> AbA
            0.0001 * x.[32] // Aab | ligation: A + ab <-> Aab
            -0.001 * x.[1] * x.[16] // A + ab | ligation: A + ab <-> Aab
            0.0001 * x.[31] // Aaa | ligation: A + aa <-> Aaa
            -0.001 * x.[1] * x.[15] // A + aa | ligation: A + aa <-> Aaa
            0.0001 * x.[30] // AaB | ligation: A + aB <-> AaB
            -0.001 * x.[1] * x.[14] // A + aB | ligation: A + aB <-> AaB
            0.0001 * x.[29] // AaA | ligation: A + aA <-> AaA
            -0.001 * x.[1] * x.[13] // A + aA | ligation: A + aA <-> AaA
            0.0001 * x.[28] // ABb | ligation: A + Bb <-> ABb
            -0.001 * x.[1] * x.[12] // A + Bb | ligation: A + Bb <-> ABb
            0.0001 * x.[27] // ABa | ligation: A + Ba <-> ABa
            -0.001 * x.[1] * x.[11] // A + Ba | ligation: A + Ba <-> ABa
            0.0001 * x.[26] // ABB | ligation: A + BB <-> ABB
            -0.001 * x.[1] * x.[10] // A + BB | ligation: A + BB <-> ABB
            0.0001 * x.[25] // ABA | ligation: A + BA <-> ABA
            -0.001 * x.[1] * x.[9] // A + BA | ligation: A + BA <-> ABA
            0.0001 * x.[24] // AAb | ligation: A + Ab <-> AAb
            -0.001 * x.[1] * x.[8] // A + Ab | ligation: A + Ab <-> AAb
            0.0001 * x.[23] // AAa | ligation: A + Aa <-> AAa
            -0.001 * x.[1] * x.[7] // A + Aa | ligation: A + Aa <-> AAa
            0.0001 * x.[22] // AAB | ligation: A + AB <-> AAB
            -0.001 * x.[1] * x.[6] // A + AB | ligation: A + AB <-> AAB
            0.0001 * x.[21] // AAA | ligation: A + AA <-> AAA
            -0.001 * x.[1] * x.[5] // A + AA | ligation: A + AA <-> AAA
            0.0001 * x.[8] // Ab | ligation: A + b <-> Ab
            -0.001 * x.[1] * x.[4] // A + b | ligation: A + b <-> Ab
            0.0001 * x.[13] // aA | ligation: a + A <-> aA
            -0.001 * x.[3] * x.[1] // a + A | ligation: a + A <-> aA
            0.0001 * x.[7] // Aa | ligation: A + a <-> Aa
            -0.001 * x.[1] * x.[3] // A + a | ligation: A + a <-> Aa
            0.0001 * x.[6] // AB | ligation: A + B <-> AB
            -0.001 * x.[1] * x.[2] // A + B | ligation: A + B <-> AB
            0.0001 * x.[5] // AA | ligation: A + A <-> AA
            0.0001 * x.[5] // AA | ligation: A + A <-> AA
            -0.001 * x.[1] * x.[1] // A + A | ligation: A + A <-> AA
            -0.001 * x.[1] * x.[1] // A + A | ligation: A + A <-> AA
            -0.0001 * x.[1] // A | synthesis: Y <-> A
            0.001 * x.[0] // Y | synthesis: Y <-> A
        |]
        |> Array.sum


    // 2 - B
    let d2 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -kW * (2.0 * xSum - x.[2]) * x.[2]
            0.0001 * x.[52] // Bbb | ligation: B + bb <-> Bbb
            -0.001 * x.[2] * x.[20] // B + bb | ligation: B + bb <-> Bbb
            0.0001 * x.[51] // Bba | ligation: B + ba <-> Bba
            -0.001 * x.[2] * x.[19] // B + ba | ligation: B + ba <-> Bba
            0.0001 * x.[50] // BbB | ligation: B + bB <-> BbB
            -0.001 * x.[2] * x.[18] // B + bB | ligation: B + bB <-> BbB
            0.0001 * x.[49] // BbA | ligation: B + bA <-> BbA
            -0.001 * x.[2] * x.[17] // B + bA | ligation: B + bA <-> BbA
            0.0001 * x.[48] // Bab | ligation: B + ab <-> Bab
            -0.001 * x.[2] * x.[16] // B + ab | ligation: B + ab <-> Bab
            0.0001 * x.[47] // Baa | ligation: B + aa <-> Baa
            -0.001 * x.[2] * x.[15] // B + aa | ligation: B + aa <-> Baa
            0.0001 * x.[46] // BaB | ligation: B + aB <-> BaB
            -0.001 * x.[2] * x.[14] // B + aB | ligation: B + aB <-> BaB
            0.0001 * x.[45] // BaA | ligation: B + aA <-> BaA
            -0.001 * x.[2] * x.[13] // B + aA | ligation: B + aA <-> BaA
            0.0001 * x.[44] // BBb | ligation: B + Bb <-> BBb
            -0.001 * x.[2] * x.[12] // B + Bb | ligation: B + Bb <-> BBb
            0.0001 * x.[43] // BBa | ligation: B + Ba <-> BBa
            -0.001 * x.[2] * x.[11] // B + Ba | ligation: B + Ba <-> BBa
            0.0001 * x.[42] // BBB | ligation: B + BB <-> BBB
            -0.001 * x.[2] * x.[10] // B + BB | ligation: B + BB <-> BBB
            0.0001 * x.[41] // BBA | ligation: B + BA <-> BBA
            -0.001 * x.[2] * x.[9] // B + BA | ligation: B + BA <-> BBA
            0.0001 * x.[40] // BAb | ligation: B + Ab <-> BAb
            -0.001 * x.[2] * x.[8] // B + Ab | ligation: B + Ab <-> BAb
            0.0001 * x.[39] // BAa | ligation: B + Aa <-> BAa
            -0.001 * x.[2] * x.[7] // B + Aa | ligation: B + Aa <-> BAa
            0.0001 * x.[38] // BAB | ligation: B + AB <-> BAB
            -0.001 * x.[2] * x.[6] // B + AB | ligation: B + AB <-> BAB
            0.0001 * x.[37] // BAA | ligation: B + AA <-> BAA
            -0.001 * x.[2] * x.[5] // B + AA | ligation: B + AA <-> BAA
            0.0001 * x.[18] // bB | ligation: b + B <-> bB
            -0.001 * x.[4] * x.[2] // b + B | ligation: b + B <-> bB
            0.0001 * x.[12] // Bb | ligation: B + b <-> Bb
            -0.001 * x.[2] * x.[4] // B + b | ligation: B + b <-> Bb
            0.0001 * x.[11] // Ba | ligation: B + a <-> Ba
            -0.001 * x.[2] * x.[3] // B + a | ligation: B + a <-> Ba
            0.0001 * x.[10] // BB | ligation: B + B <-> BB
            0.0001 * x.[10] // BB | ligation: B + B <-> BB
            -0.001 * x.[2] * x.[2] // B + B | ligation: B + B <-> BB
            -0.001 * x.[2] * x.[2] // B + B | ligation: B + B <-> BB
            0.0001 * x.[14] // aB | ligation: a + B <-> aB
            -0.001 * x.[3] * x.[2] // a + B | ligation: a + B <-> aB
            0.0001 * x.[6] // AB | ligation: A + B <-> AB
            -0.001 * x.[1] * x.[2] // A + B | ligation: A + B <-> AB
            -0.0001 * x.[2] // B | synthesis: Y <-> B
            0.001 * x.[0] // Y | synthesis: Y <-> B
        |]
        |> Array.sum


    // 3 - a
    let d3 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -kW * (2.0 * xSum - x.[3]) * x.[3]
            0.0961330595979871 * x.[60] * x.[11] // aBb + Ba | catalytic ligation: a + Bb + Ba <-> aBb + Ba
            -0.961330595979871 * x.[3] * x.[12] * x.[11] // a + Bb + Ba | catalytic ligation: a + Bb + Ba <-> aBb + Ba
            0.0001 * x.[11] // Ba | ligation: B + a <-> Ba
            -0.001 * x.[2] * x.[3] // B + a | ligation: B + a <-> Ba
            0.0001 * x.[58] // aBB | ligation: a + BB <-> aBB
            -0.001 * x.[3] * x.[10] // a + BB | ligation: a + BB <-> aBB
            0.0001 * x.[57] // aBA | ligation: a + BA <-> aBA
            -0.001 * x.[3] * x.[9] // a + BA | ligation: a + BA <-> aBA
            0.0001 * x.[60] // aBb | ligation: a + Bb <-> aBb
            -0.001 * x.[3] * x.[12] // a + Bb | ligation: a + Bb <-> aBb
            0.0001 * x.[59] // aBa | ligation: a + Ba <-> aBa
            -0.001 * x.[3] * x.[11] // a + Ba | ligation: a + Ba <-> aBa
            0.0001 * x.[54] // aAB | ligation: a + AB <-> aAB
            -0.001 * x.[3] * x.[6] // a + AB | ligation: a + AB <-> aAB
            0.0001 * x.[53] // aAA | ligation: a + AA <-> aAA
            -0.001 * x.[3] * x.[5] // a + AA | ligation: a + AA <-> aAA
            0.0001 * x.[56] // aAb | ligation: a + Ab <-> aAb
            -0.001 * x.[3] * x.[8] // a + Ab | ligation: a + Ab <-> aAb
            0.0001 * x.[55] // aAa | ligation: a + Aa <-> aAa
            -0.001 * x.[3] * x.[7] // a + Aa | ligation: a + Aa <-> aAa
            0.0001 * x.[66] // abB | ligation: a + bB <-> abB
            -0.001 * x.[3] * x.[18] // a + bB | ligation: a + bB <-> abB
            0.0001 * x.[65] // abA | ligation: a + bA <-> abA
            -0.001 * x.[3] * x.[17] // a + bA | ligation: a + bA <-> abA
            0.0001 * x.[68] // abb | ligation: a + bb <-> abb
            -0.001 * x.[3] * x.[20] // a + bb | ligation: a + bb <-> abb
            0.0001 * x.[67] // aba | ligation: a + ba <-> aba
            -0.001 * x.[3] * x.[19] // a + ba | ligation: a + ba <-> aba
            0.0001 * x.[62] // aaB | ligation: a + aB <-> aaB
            -0.001 * x.[3] * x.[14] // a + aB | ligation: a + aB <-> aaB
            0.0001 * x.[61] // aaA | ligation: a + aA <-> aaA
            -0.001 * x.[3] * x.[13] // a + aA | ligation: a + aA <-> aaA
            0.0001 * x.[64] // aab | ligation: a + ab <-> aab
            -0.001 * x.[3] * x.[16] // a + ab | ligation: a + ab <-> aab
            0.0001 * x.[63] // aaa | ligation: a + aa <-> aaa
            -0.001 * x.[3] * x.[15] // a + aa | ligation: a + aa <-> aaa
            0.0001 * x.[14] // aB | ligation: a + B <-> aB
            -0.001 * x.[3] * x.[2] // a + B | ligation: a + B <-> aB
            0.0001 * x.[13] // aA | ligation: a + A <-> aA
            -0.001 * x.[3] * x.[1] // a + A | ligation: a + A <-> aA
            0.0001 * x.[7] // Aa | ligation: A + a <-> Aa
            -0.001 * x.[1] * x.[3] // A + a | ligation: A + a <-> Aa
            0.0001 * x.[16] // ab | ligation: a + b <-> ab
            -0.001 * x.[3] * x.[4] // a + b | ligation: a + b <-> ab
            0.0001 * x.[15] // aa | ligation: a + a <-> aa
            0.0001 * x.[15] // aa | ligation: a + a <-> aa
            -0.001 * x.[3] * x.[3] // a + a | ligation: a + a <-> aa
            -0.001 * x.[3] * x.[3] // a + a | ligation: a + a <-> aa
            -0.0001 * x.[3] // a | synthesis: Y <-> a
            0.001 * x.[0] // Y | synthesis: Y <-> a
        |]
        |> Array.sum


    // 4 - b
    let d4 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -kW * (2.0 * xSum - x.[4]) * x.[4]
            0.0001 * x.[74] // bBB | ligation: b + BB <-> bBB
            -0.001 * x.[4] * x.[10] // b + BB | ligation: b + BB <-> bBB
            0.0001 * x.[73] // bBA | ligation: b + BA <-> bBA
            -0.001 * x.[4] * x.[9] // b + BA | ligation: b + BA <-> bBA
            0.0001 * x.[76] // bBb | ligation: b + Bb <-> bBb
            -0.001 * x.[4] * x.[12] // b + Bb | ligation: b + Bb <-> bBb
            0.0001 * x.[75] // bBa | ligation: b + Ba <-> bBa
            -0.001 * x.[4] * x.[11] // b + Ba | ligation: b + Ba <-> bBa
            0.0001 * x.[70] // bAB | ligation: b + AB <-> bAB
            -0.001 * x.[4] * x.[6] // b + AB | ligation: b + AB <-> bAB
            0.0001 * x.[69] // bAA | ligation: b + AA <-> bAA
            -0.001 * x.[4] * x.[5] // b + AA | ligation: b + AA <-> bAA
            0.0001 * x.[72] // bAb | ligation: b + Ab <-> bAb
            -0.001 * x.[4] * x.[8] // b + Ab | ligation: b + Ab <-> bAb
            0.0001 * x.[71] // bAa | ligation: b + Aa <-> bAa
            -0.001 * x.[4] * x.[7] // b + Aa | ligation: b + Aa <-> bAa
            0.0001 * x.[82] // bbB | ligation: b + bB <-> bbB
            -0.001 * x.[4] * x.[18] // b + bB | ligation: b + bB <-> bbB
            0.0001 * x.[81] // bbA | ligation: b + bA <-> bbA
            -0.001 * x.[4] * x.[17] // b + bA | ligation: b + bA <-> bbA
            0.0001 * x.[84] // bbb | ligation: b + bb <-> bbb
            -0.001 * x.[4] * x.[20] // b + bb | ligation: b + bb <-> bbb
            0.0001 * x.[83] // bba | ligation: b + ba <-> bba
            -0.001 * x.[4] * x.[19] // b + ba | ligation: b + ba <-> bba
            0.0001 * x.[78] // baB | ligation: b + aB <-> baB
            -0.001 * x.[4] * x.[14] // b + aB | ligation: b + aB <-> baB
            0.0001 * x.[77] // baA | ligation: b + aA <-> baA
            -0.001 * x.[4] * x.[13] // b + aA | ligation: b + aA <-> baA
            0.0001 * x.[80] // bab | ligation: b + ab <-> bab
            -0.001 * x.[4] * x.[16] // b + ab | ligation: b + ab <-> bab
            0.0001 * x.[79] // baa | ligation: b + aa <-> baa
            -0.001 * x.[4] * x.[15] // b + aa | ligation: b + aa <-> baa
            0.0001 * x.[18] // bB | ligation: b + B <-> bB
            -0.001 * x.[4] * x.[2] // b + B | ligation: b + B <-> bB
            0.0001 * x.[12] // Bb | ligation: B + b <-> Bb
            -0.001 * x.[2] * x.[4] // B + b | ligation: B + b <-> Bb
            0.0001 * x.[17] // bA | ligation: b + A <-> bA
            -0.001 * x.[4] * x.[1] // b + A | ligation: b + A <-> bA
            0.0001 * x.[20] // bb | ligation: b + b <-> bb
            0.0001 * x.[20] // bb | ligation: b + b <-> bb
            -0.001 * x.[4] * x.[4] // b + b | ligation: b + b <-> bb
            -0.001 * x.[4] * x.[4] // b + b | ligation: b + b <-> bb
            0.0001 * x.[8] // Ab | ligation: A + b <-> Ab
            -0.001 * x.[1] * x.[4] // A + b | ligation: A + b <-> Ab
            0.0001 * x.[16] // ab | ligation: a + b <-> ab
            -0.001 * x.[3] * x.[4] // a + b | ligation: a + b <-> ab
            -0.0001 * x.[4] // b | synthesis: Y <-> b
            0.001 * x.[0] // Y | synthesis: Y <-> b
        |]
        |> Array.sum


    // 5 - AA
    let d5 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -kW * (2.0 * xSum - x.[5]) * x.[5]
            0.0001 * x.[69] // bAA | ligation: b + AA <-> bAA
            -0.001 * x.[4] * x.[5] // b + AA | ligation: b + AA <-> bAA
            0.0001 * x.[37] // BAA | ligation: B + AA <-> BAA
            -0.001 * x.[2] * x.[5] // B + AA | ligation: B + AA <-> BAA
            0.0001 * x.[53] // aAA | ligation: a + AA <-> aAA
            -0.001 * x.[3] * x.[5] // a + AA | ligation: a + AA <-> aAA
            0.0001 * x.[21] // AAA | ligation: A + AA <-> AAA
            -0.001 * x.[1] * x.[5] // A + AA | ligation: A + AA <-> AAA
            -0.0001 * x.[5] // AA | ligation: A + A <-> AA
            0.001 * x.[1] * x.[1] // A + A | ligation: A + A <-> AA
        |]
        |> Array.sum


    // 6 - AB
    let d6 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -kW * (2.0 * xSum - x.[6]) * x.[6]
            0.0001 * x.[70] // bAB | ligation: b + AB <-> bAB
            -0.001 * x.[4] * x.[6] // b + AB | ligation: b + AB <-> bAB
            0.0001 * x.[38] // BAB | ligation: B + AB <-> BAB
            -0.001 * x.[2] * x.[6] // B + AB | ligation: B + AB <-> BAB
            0.0001 * x.[54] // aAB | ligation: a + AB <-> aAB
            -0.001 * x.[3] * x.[6] // a + AB | ligation: a + AB <-> aAB
            0.0001 * x.[22] // AAB | ligation: A + AB <-> AAB
            -0.001 * x.[1] * x.[6] // A + AB | ligation: A + AB <-> AAB
            -0.0001 * x.[6] // AB | ligation: A + B <-> AB
            0.001 * x.[1] * x.[2] // A + B | ligation: A + B <-> AB
        |]
        |> Array.sum


    // 7 - Aa
    let d7 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -kW * (2.0 * xSum - x.[7]) * x.[7]
            0.0001 * x.[71] // bAa | ligation: b + Aa <-> bAa
            -0.001 * x.[4] * x.[7] // b + Aa | ligation: b + Aa <-> bAa
            0.0001 * x.[39] // BAa | ligation: B + Aa <-> BAa
            -0.001 * x.[2] * x.[7] // B + Aa | ligation: B + Aa <-> BAa
            0.0001 * x.[55] // aAa | ligation: a + Aa <-> aAa
            -0.001 * x.[3] * x.[7] // a + Aa | ligation: a + Aa <-> aAa
            0.0001 * x.[23] // AAa | ligation: A + Aa <-> AAa
            -0.001 * x.[1] * x.[7] // A + Aa | ligation: A + Aa <-> AAa
            -0.0001 * x.[7] // Aa | ligation: A + a <-> Aa
            0.001 * x.[1] * x.[3] // A + a | ligation: A + a <-> Aa
        |]
        |> Array.sum


    // 8 - Ab
    let d8 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -kW * (2.0 * xSum - x.[8]) * x.[8]
            0.0001 * x.[72] // bAb | ligation: b + Ab <-> bAb
            -0.001 * x.[4] * x.[8] // b + Ab | ligation: b + Ab <-> bAb
            0.0001 * x.[40] // BAb | ligation: B + Ab <-> BAb
            -0.001 * x.[2] * x.[8] // B + Ab | ligation: B + Ab <-> BAb
            0.0001 * x.[56] // aAb | ligation: a + Ab <-> aAb
            -0.001 * x.[3] * x.[8] // a + Ab | ligation: a + Ab <-> aAb
            0.0001 * x.[24] // AAb | ligation: A + Ab <-> AAb
            -0.001 * x.[1] * x.[8] // A + Ab | ligation: A + Ab <-> AAb
            -0.0001 * x.[8] // Ab | ligation: A + b <-> Ab
            0.001 * x.[1] * x.[4] // A + b | ligation: A + b <-> Ab
        |]
        |> Array.sum


    // 9 - BA
    let d9 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -kW * (2.0 * xSum - x.[9]) * x.[9]
            0.0001 * x.[73] // bBA | ligation: b + BA <-> bBA
            -0.001 * x.[4] * x.[9] // b + BA | ligation: b + BA <-> bBA
            0.0001 * x.[41] // BBA | ligation: B + BA <-> BBA
            -0.001 * x.[2] * x.[9] // B + BA | ligation: B + BA <-> BBA
            0.0001 * x.[57] // aBA | ligation: a + BA <-> aBA
            -0.001 * x.[3] * x.[9] // a + BA | ligation: a + BA <-> aBA
            0.0001 * x.[25] // ABA | ligation: A + BA <-> ABA
            -0.001 * x.[1] * x.[9] // A + BA | ligation: A + BA <-> ABA
        |]
        |> Array.sum


    // 10 - BB
    let d10 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -kW * (2.0 * xSum - x.[10]) * x.[10]
            0.0001 * x.[74] // bBB | ligation: b + BB <-> bBB
            -0.001 * x.[4] * x.[10] // b + BB | ligation: b + BB <-> bBB
            0.0001 * x.[42] // BBB | ligation: B + BB <-> BBB
            -0.001 * x.[2] * x.[10] // B + BB | ligation: B + BB <-> BBB
            -0.0001 * x.[10] // BB | ligation: B + B <-> BB
            0.001 * x.[2] * x.[2] // B + B | ligation: B + B <-> BB
            0.0001 * x.[58] // aBB | ligation: a + BB <-> aBB
            -0.001 * x.[3] * x.[10] // a + BB | ligation: a + BB <-> aBB
            0.0001 * x.[26] // ABB | ligation: A + BB <-> ABB
            -0.001 * x.[1] * x.[10] // A + BB | ligation: A + BB <-> ABB
        |]
        |> Array.sum


    // 11 - Ba
    let d11 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -kW * (2.0 * xSum - x.[11]) * x.[11]
            0.0001 * x.[75] // bBa | ligation: b + Ba <-> bBa
            -0.001 * x.[4] * x.[11] // b + Ba | ligation: b + Ba <-> bBa
            0.0001 * x.[43] // BBa | ligation: B + Ba <-> BBa
            -0.001 * x.[2] * x.[11] // B + Ba | ligation: B + Ba <-> BBa
            -0.0001 * x.[11] // Ba | ligation: B + a <-> Ba
            0.001 * x.[2] * x.[3] // B + a | ligation: B + a <-> Ba
            0.0001 * x.[59] // aBa | ligation: a + Ba <-> aBa
            -0.001 * x.[3] * x.[11] // a + Ba | ligation: a + Ba <-> aBa
            0.0001 * x.[27] // ABa | ligation: A + Ba <-> ABa
            -0.001 * x.[1] * x.[11] // A + Ba | ligation: A + Ba <-> ABa
        |]
        |> Array.sum


    // 12 - Bb
    let d12 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -kW * (2.0 * xSum - x.[12]) * x.[12]
            0.0961330595979871 * x.[60] * x.[11] // aBb + Ba | catalytic ligation: a + Bb + Ba <-> aBb + Ba
            -0.961330595979871 * x.[3] * x.[12] * x.[11] // a + Bb + Ba | catalytic ligation: a + Bb + Ba <-> aBb + Ba
            0.0001 * x.[76] // bBb | ligation: b + Bb <-> bBb
            -0.001 * x.[4] * x.[12] // b + Bb | ligation: b + Bb <-> bBb
            0.0001 * x.[44] // BBb | ligation: B + Bb <-> BBb
            -0.001 * x.[2] * x.[12] // B + Bb | ligation: B + Bb <-> BBb
            -0.0001 * x.[12] // Bb | ligation: B + b <-> Bb
            0.001 * x.[2] * x.[4] // B + b | ligation: B + b <-> Bb
            0.0001 * x.[60] // aBb | ligation: a + Bb <-> aBb
            -0.001 * x.[3] * x.[12] // a + Bb | ligation: a + Bb <-> aBb
            0.0001 * x.[28] // ABb | ligation: A + Bb <-> ABb
            -0.001 * x.[1] * x.[12] // A + Bb | ligation: A + Bb <-> ABb
        |]
        |> Array.sum


    // 13 - aA
    let d13 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -kW * (2.0 * xSum - x.[13]) * x.[13]
            0.0001 * x.[45] // BaA | ligation: B + aA <-> BaA
            -0.001 * x.[2] * x.[13] // B + aA | ligation: B + aA <-> BaA
            0.0001 * x.[77] // baA | ligation: b + aA <-> baA
            -0.001 * x.[4] * x.[13] // b + aA | ligation: b + aA <-> baA
            0.0001 * x.[29] // AaA | ligation: A + aA <-> AaA
            -0.001 * x.[1] * x.[13] // A + aA | ligation: A + aA <-> AaA
            0.0001 * x.[61] // aaA | ligation: a + aA <-> aaA
            -0.001 * x.[3] * x.[13] // a + aA | ligation: a + aA <-> aaA
            -0.0001 * x.[13] // aA | ligation: a + A <-> aA
            0.001 * x.[3] * x.[1] // a + A | ligation: a + A <-> aA
        |]
        |> Array.sum


    // 14 - aB
    let d14 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -kW * (2.0 * xSum - x.[14]) * x.[14]
            0.0001 * x.[46] // BaB | ligation: B + aB <-> BaB
            -0.001 * x.[2] * x.[14] // B + aB | ligation: B + aB <-> BaB
            0.0001 * x.[78] // baB | ligation: b + aB <-> baB
            -0.001 * x.[4] * x.[14] // b + aB | ligation: b + aB <-> baB
            0.0001 * x.[30] // AaB | ligation: A + aB <-> AaB
            -0.001 * x.[1] * x.[14] // A + aB | ligation: A + aB <-> AaB
            0.0001 * x.[62] // aaB | ligation: a + aB <-> aaB
            -0.001 * x.[3] * x.[14] // a + aB | ligation: a + aB <-> aaB
            -0.0001 * x.[14] // aB | ligation: a + B <-> aB
            0.001 * x.[3] * x.[2] // a + B | ligation: a + B <-> aB
        |]
        |> Array.sum


    // 15 - aa
    let d15 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -kW * (2.0 * xSum - x.[15]) * x.[15]
            0.0001 * x.[47] // Baa | ligation: B + aa <-> Baa
            -0.001 * x.[2] * x.[15] // B + aa | ligation: B + aa <-> Baa
            0.0001 * x.[79] // baa | ligation: b + aa <-> baa
            -0.001 * x.[4] * x.[15] // b + aa | ligation: b + aa <-> baa
            0.0001 * x.[31] // Aaa | ligation: A + aa <-> Aaa
            -0.001 * x.[1] * x.[15] // A + aa | ligation: A + aa <-> Aaa
            0.0001 * x.[63] // aaa | ligation: a + aa <-> aaa
            -0.001 * x.[3] * x.[15] // a + aa | ligation: a + aa <-> aaa
            -0.0001 * x.[15] // aa | ligation: a + a <-> aa
            0.001 * x.[3] * x.[3] // a + a | ligation: a + a <-> aa
        |]
        |> Array.sum


    // 16 - ab
    let d16 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -kW * (2.0 * xSum - x.[16]) * x.[16]
            0.0001 * x.[48] // Bab | ligation: B + ab <-> Bab
            -0.001 * x.[2] * x.[16] // B + ab | ligation: B + ab <-> Bab
            0.0001 * x.[80] // bab | ligation: b + ab <-> bab
            -0.001 * x.[4] * x.[16] // b + ab | ligation: b + ab <-> bab
            0.0001 * x.[32] // Aab | ligation: A + ab <-> Aab
            -0.001 * x.[1] * x.[16] // A + ab | ligation: A + ab <-> Aab
            0.0001 * x.[64] // aab | ligation: a + ab <-> aab
            -0.001 * x.[3] * x.[16] // a + ab | ligation: a + ab <-> aab
            -0.0001 * x.[16] // ab | ligation: a + b <-> ab
            0.001 * x.[3] * x.[4] // a + b | ligation: a + b <-> ab
        |]
        |> Array.sum


    // 17 - bA
    let d17 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -kW * (2.0 * xSum - x.[17]) * x.[17]
            0.0001 * x.[49] // BbA | ligation: B + bA <-> BbA
            -0.001 * x.[2] * x.[17] // B + bA | ligation: B + bA <-> BbA
            0.0001 * x.[81] // bbA | ligation: b + bA <-> bbA
            -0.001 * x.[4] * x.[17] // b + bA | ligation: b + bA <-> bbA
            -0.0001 * x.[17] // bA | ligation: b + A <-> bA
            0.001 * x.[4] * x.[1] // b + A | ligation: b + A <-> bA
            0.0001 * x.[33] // AbA | ligation: A + bA <-> AbA
            -0.001 * x.[1] * x.[17] // A + bA | ligation: A + bA <-> AbA
            0.0001 * x.[65] // abA | ligation: a + bA <-> abA
            -0.001 * x.[3] * x.[17] // a + bA | ligation: a + bA <-> abA
        |]
        |> Array.sum


    // 18 - bB
    let d18 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -kW * (2.0 * xSum - x.[18]) * x.[18]
            0.0961330595979871 * x.[34] * x.[17] // AbB + bA | catalytic ligation: A + bB + bA <-> AbB + bA
            -0.961330595979871 * x.[1] * x.[18] * x.[17] // A + bB + bA | catalytic ligation: A + bB + bA <-> AbB + bA
            0.0001 * x.[50] // BbB | ligation: B + bB <-> BbB
            -0.001 * x.[2] * x.[18] // B + bB | ligation: B + bB <-> BbB
            0.0001 * x.[82] // bbB | ligation: b + bB <-> bbB
            -0.001 * x.[4] * x.[18] // b + bB | ligation: b + bB <-> bbB
            -0.0001 * x.[18] // bB | ligation: b + B <-> bB
            0.001 * x.[4] * x.[2] // b + B | ligation: b + B <-> bB
            0.0001 * x.[34] // AbB | ligation: A + bB <-> AbB
            -0.001 * x.[1] * x.[18] // A + bB | ligation: A + bB <-> AbB
            0.0001 * x.[66] // abB | ligation: a + bB <-> abB
            -0.001 * x.[3] * x.[18] // a + bB | ligation: a + bB <-> abB
        |]
        |> Array.sum


    // 19 - ba
    let d19 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -kW * (2.0 * xSum - x.[19]) * x.[19]
            0.0001 * x.[51] // Bba | ligation: B + ba <-> Bba
            -0.001 * x.[2] * x.[19] // B + ba | ligation: B + ba <-> Bba
            0.0001 * x.[83] // bba | ligation: b + ba <-> bba
            -0.001 * x.[4] * x.[19] // b + ba | ligation: b + ba <-> bba
            0.0001 * x.[35] // Aba | ligation: A + ba <-> Aba
            -0.001 * x.[1] * x.[19] // A + ba | ligation: A + ba <-> Aba
            0.0001 * x.[67] // aba | ligation: a + ba <-> aba
            -0.001 * x.[3] * x.[19] // a + ba | ligation: a + ba <-> aba
        |]
        |> Array.sum


    // 20 - bb
    let d20 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -kW * (2.0 * xSum - x.[20]) * x.[20]
            0.0001 * x.[52] // Bbb | ligation: B + bb <-> Bbb
            -0.001 * x.[2] * x.[20] // B + bb | ligation: B + bb <-> Bbb
            0.0001 * x.[84] // bbb | ligation: b + bb <-> bbb
            -0.001 * x.[4] * x.[20] // b + bb | ligation: b + bb <-> bbb
            -0.0001 * x.[20] // bb | ligation: b + b <-> bb
            0.001 * x.[4] * x.[4] // b + b | ligation: b + b <-> bb
            0.0001 * x.[36] // Abb | ligation: A + bb <-> Abb
            -0.001 * x.[1] * x.[20] // A + bb | ligation: A + bb <-> Abb
            0.0001 * x.[68] // abb | ligation: a + bb <-> abb
            -0.001 * x.[3] * x.[20] // a + bb | ligation: a + bb <-> abb
        |]
        |> Array.sum


    // 21 - AAA
    let d21 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -kW * (2.0 * xSum - x.[21]) * x.[21]
            -0.0001 * x.[21] // AAA | ligation: A + AA <-> AAA
            0.001 * x.[1] * x.[5] // A + AA | ligation: A + AA <-> AAA
        |]
        |> Array.sum


    // 22 - AAB
    let d22 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -kW * (2.0 * xSum - x.[22]) * x.[22]
            -0.0001 * x.[22] // AAB | ligation: A + AB <-> AAB
            0.001 * x.[1] * x.[6] // A + AB | ligation: A + AB <-> AAB
        |]
        |> Array.sum


    // 23 - AAa
    let d23 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -kW * (2.0 * xSum - x.[23]) * x.[23]
            -0.0001 * x.[23] // AAa | ligation: A + Aa <-> AAa
            0.001 * x.[1] * x.[7] // A + Aa | ligation: A + Aa <-> AAa
        |]
        |> Array.sum


    // 24 - AAb
    let d24 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -kW * (2.0 * xSum - x.[24]) * x.[24]
            -0.0001 * x.[24] // AAb | ligation: A + Ab <-> AAb
            0.001 * x.[1] * x.[8] // A + Ab | ligation: A + Ab <-> AAb
        |]
        |> Array.sum


    // 25 - ABA
    let d25 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -kW * (2.0 * xSum - x.[25]) * x.[25]
            -0.0001 * x.[25] // ABA | ligation: A + BA <-> ABA
            0.001 * x.[1] * x.[9] // A + BA | ligation: A + BA <-> ABA
        |]
        |> Array.sum


    // 26 - ABB
    let d26 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -kW * (2.0 * xSum - x.[26]) * x.[26]
            -0.0001 * x.[26] // ABB | ligation: A + BB <-> ABB
            0.001 * x.[1] * x.[10] // A + BB | ligation: A + BB <-> ABB
        |]
        |> Array.sum


    // 27 - ABa
    let d27 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -kW * (2.0 * xSum - x.[27]) * x.[27]
            -0.0001 * x.[27] // ABa | ligation: A + Ba <-> ABa
            0.001 * x.[1] * x.[11] // A + Ba | ligation: A + Ba <-> ABa
        |]
        |> Array.sum


    // 28 - ABb
    let d28 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -kW * (2.0 * xSum - x.[28]) * x.[28]
            -0.0001 * x.[28] // ABb | ligation: A + Bb <-> ABb
            0.001 * x.[1] * x.[12] // A + Bb | ligation: A + Bb <-> ABb
        |]
        |> Array.sum


    // 29 - AaA
    let d29 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -kW * (2.0 * xSum - x.[29]) * x.[29]
            -0.0001 * x.[29] // AaA | ligation: A + aA <-> AaA
            0.001 * x.[1] * x.[13] // A + aA | ligation: A + aA <-> AaA
        |]
        |> Array.sum


    // 30 - AaB
    let d30 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -kW * (2.0 * xSum - x.[30]) * x.[30]
            -0.0001 * x.[30] // AaB | ligation: A + aB <-> AaB
            0.001 * x.[1] * x.[14] // A + aB | ligation: A + aB <-> AaB
        |]
        |> Array.sum


    // 31 - Aaa
    let d31 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -kW * (2.0 * xSum - x.[31]) * x.[31]
            -0.0001 * x.[31] // Aaa | ligation: A + aa <-> Aaa
            0.001 * x.[1] * x.[15] // A + aa | ligation: A + aa <-> Aaa
        |]
        |> Array.sum


    // 32 - Aab
    let d32 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -kW * (2.0 * xSum - x.[32]) * x.[32]
            -0.0001 * x.[32] // Aab | ligation: A + ab <-> Aab
            0.001 * x.[1] * x.[16] // A + ab | ligation: A + ab <-> Aab
        |]
        |> Array.sum


    // 33 - AbA
    let d33 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -kW * (2.0 * xSum - x.[33]) * x.[33]
            -0.0001 * x.[33] // AbA | ligation: A + bA <-> AbA
            0.001 * x.[1] * x.[17] // A + bA | ligation: A + bA <-> AbA
        |]
        |> Array.sum


    // 34 - AbB
    let d34 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -kW * (2.0 * xSum - x.[34]) * x.[34]
            -0.0961330595979871 * x.[34] * x.[17] // AbB + bA | catalytic ligation: A + bB + bA <-> AbB + bA
            0.961330595979871 * x.[1] * x.[18] * x.[17] // A + bB + bA | catalytic ligation: A + bB + bA <-> AbB + bA
            -0.0001 * x.[34] // AbB | ligation: A + bB <-> AbB
            0.001 * x.[1] * x.[18] // A + bB | ligation: A + bB <-> AbB
        |]
        |> Array.sum


    // 35 - Aba
    let d35 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -kW * (2.0 * xSum - x.[35]) * x.[35]
            -0.0001 * x.[35] // Aba | ligation: A + ba <-> Aba
            0.001 * x.[1] * x.[19] // A + ba | ligation: A + ba <-> Aba
        |]
        |> Array.sum


    // 36 - Abb
    let d36 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -kW * (2.0 * xSum - x.[36]) * x.[36]
            -0.0001 * x.[36] // Abb | ligation: A + bb <-> Abb
            0.001 * x.[1] * x.[20] // A + bb | ligation: A + bb <-> Abb
        |]
        |> Array.sum


    // 37 - BAA
    let d37 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -kW * (2.0 * xSum - x.[37]) * x.[37]
            -0.0001 * x.[37] // BAA | ligation: B + AA <-> BAA
            0.001 * x.[2] * x.[5] // B + AA | ligation: B + AA <-> BAA
        |]
        |> Array.sum


    // 38 - BAB
    let d38 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -kW * (2.0 * xSum - x.[38]) * x.[38]
            -0.0001 * x.[38] // BAB | ligation: B + AB <-> BAB
            0.001 * x.[2] * x.[6] // B + AB | ligation: B + AB <-> BAB
        |]
        |> Array.sum


    // 39 - BAa
    let d39 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -kW * (2.0 * xSum - x.[39]) * x.[39]
            -0.0001 * x.[39] // BAa | ligation: B + Aa <-> BAa
            0.001 * x.[2] * x.[7] // B + Aa | ligation: B + Aa <-> BAa
        |]
        |> Array.sum


    // 40 - BAb
    let d40 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -kW * (2.0 * xSum - x.[40]) * x.[40]
            -0.0001 * x.[40] // BAb | ligation: B + Ab <-> BAb
            0.001 * x.[2] * x.[8] // B + Ab | ligation: B + Ab <-> BAb
        |]
        |> Array.sum


    // 41 - BBA
    let d41 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -kW * (2.0 * xSum - x.[41]) * x.[41]
            -0.0001 * x.[41] // BBA | ligation: B + BA <-> BBA
            0.001 * x.[2] * x.[9] // B + BA | ligation: B + BA <-> BBA
        |]
        |> Array.sum


    // 42 - BBB
    let d42 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -kW * (2.0 * xSum - x.[42]) * x.[42]
            -0.0001 * x.[42] // BBB | ligation: B + BB <-> BBB
            0.001 * x.[2] * x.[10] // B + BB | ligation: B + BB <-> BBB
        |]
        |> Array.sum


    // 43 - BBa
    let d43 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -kW * (2.0 * xSum - x.[43]) * x.[43]
            -0.0001 * x.[43] // BBa | ligation: B + Ba <-> BBa
            0.001 * x.[2] * x.[11] // B + Ba | ligation: B + Ba <-> BBa
        |]
        |> Array.sum


    // 44 - BBb
    let d44 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -kW * (2.0 * xSum - x.[44]) * x.[44]
            -0.0001 * x.[44] // BBb | ligation: B + Bb <-> BBb
            0.001 * x.[2] * x.[12] // B + Bb | ligation: B + Bb <-> BBb
        |]
        |> Array.sum


    // 45 - BaA
    let d45 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -kW * (2.0 * xSum - x.[45]) * x.[45]
            -0.0001 * x.[45] // BaA | ligation: B + aA <-> BaA
            0.001 * x.[2] * x.[13] // B + aA | ligation: B + aA <-> BaA
        |]
        |> Array.sum


    // 46 - BaB
    let d46 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -kW * (2.0 * xSum - x.[46]) * x.[46]
            -0.0001 * x.[46] // BaB | ligation: B + aB <-> BaB
            0.001 * x.[2] * x.[14] // B + aB | ligation: B + aB <-> BaB
        |]
        |> Array.sum


    // 47 - Baa
    let d47 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -kW * (2.0 * xSum - x.[47]) * x.[47]
            -0.0001 * x.[47] // Baa | ligation: B + aa <-> Baa
            0.001 * x.[2] * x.[15] // B + aa | ligation: B + aa <-> Baa
        |]
        |> Array.sum


    // 48 - Bab
    let d48 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -kW * (2.0 * xSum - x.[48]) * x.[48]
            -0.0001 * x.[48] // Bab | ligation: B + ab <-> Bab
            0.001 * x.[2] * x.[16] // B + ab | ligation: B + ab <-> Bab
        |]
        |> Array.sum


    // 49 - BbA
    let d49 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -kW * (2.0 * xSum - x.[49]) * x.[49]
            -0.0001 * x.[49] // BbA | ligation: B + bA <-> BbA
            0.001 * x.[2] * x.[17] // B + bA | ligation: B + bA <-> BbA
        |]
        |> Array.sum


    // 50 - BbB
    let d50 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -kW * (2.0 * xSum - x.[50]) * x.[50]
            -0.0001 * x.[50] // BbB | ligation: B + bB <-> BbB
            0.001 * x.[2] * x.[18] // B + bB | ligation: B + bB <-> BbB
        |]
        |> Array.sum


    // 51 - Bba
    let d51 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -kW * (2.0 * xSum - x.[51]) * x.[51]
            -0.0001 * x.[51] // Bba | ligation: B + ba <-> Bba
            0.001 * x.[2] * x.[19] // B + ba | ligation: B + ba <-> Bba
        |]
        |> Array.sum


    // 52 - Bbb
    let d52 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -kW * (2.0 * xSum - x.[52]) * x.[52]
            -0.0001 * x.[52] // Bbb | ligation: B + bb <-> Bbb
            0.001 * x.[2] * x.[20] // B + bb | ligation: B + bb <-> Bbb
        |]
        |> Array.sum


    // 53 - aAA
    let d53 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -kW * (2.0 * xSum - x.[53]) * x.[53]
            -0.0001 * x.[53] // aAA | ligation: a + AA <-> aAA
            0.001 * x.[3] * x.[5] // a + AA | ligation: a + AA <-> aAA
        |]
        |> Array.sum


    // 54 - aAB
    let d54 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -kW * (2.0 * xSum - x.[54]) * x.[54]
            -0.0001 * x.[54] // aAB | ligation: a + AB <-> aAB
            0.001 * x.[3] * x.[6] // a + AB | ligation: a + AB <-> aAB
        |]
        |> Array.sum


    // 55 - aAa
    let d55 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -kW * (2.0 * xSum - x.[55]) * x.[55]
            -0.0001 * x.[55] // aAa | ligation: a + Aa <-> aAa
            0.001 * x.[3] * x.[7] // a + Aa | ligation: a + Aa <-> aAa
        |]
        |> Array.sum


    // 56 - aAb
    let d56 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -kW * (2.0 * xSum - x.[56]) * x.[56]
            -0.0001 * x.[56] // aAb | ligation: a + Ab <-> aAb
            0.001 * x.[3] * x.[8] // a + Ab | ligation: a + Ab <-> aAb
        |]
        |> Array.sum


    // 57 - aBA
    let d57 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -kW * (2.0 * xSum - x.[57]) * x.[57]
            -0.0001 * x.[57] // aBA | ligation: a + BA <-> aBA
            0.001 * x.[3] * x.[9] // a + BA | ligation: a + BA <-> aBA
        |]
        |> Array.sum


    // 58 - aBB
    let d58 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -kW * (2.0 * xSum - x.[58]) * x.[58]
            -0.0001 * x.[58] // aBB | ligation: a + BB <-> aBB
            0.001 * x.[3] * x.[10] // a + BB | ligation: a + BB <-> aBB
        |]
        |> Array.sum


    // 59 - aBa
    let d59 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -kW * (2.0 * xSum - x.[59]) * x.[59]
            -0.0001 * x.[59] // aBa | ligation: a + Ba <-> aBa
            0.001 * x.[3] * x.[11] // a + Ba | ligation: a + Ba <-> aBa
        |]
        |> Array.sum


    // 60 - aBb
    let d60 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -kW * (2.0 * xSum - x.[60]) * x.[60]
            -0.0961330595979871 * x.[60] * x.[11] // aBb + Ba | catalytic ligation: a + Bb + Ba <-> aBb + Ba
            0.961330595979871 * x.[3] * x.[12] * x.[11] // a + Bb + Ba | catalytic ligation: a + Bb + Ba <-> aBb + Ba
            -0.0001 * x.[60] // aBb | ligation: a + Bb <-> aBb
            0.001 * x.[3] * x.[12] // a + Bb | ligation: a + Bb <-> aBb
        |]
        |> Array.sum


    // 61 - aaA
    let d61 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -kW * (2.0 * xSum - x.[61]) * x.[61]
            -0.0001 * x.[61] // aaA | ligation: a + aA <-> aaA
            0.001 * x.[3] * x.[13] // a + aA | ligation: a + aA <-> aaA
        |]
        |> Array.sum


    // 62 - aaB
    let d62 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -kW * (2.0 * xSum - x.[62]) * x.[62]
            -0.0001 * x.[62] // aaB | ligation: a + aB <-> aaB
            0.001 * x.[3] * x.[14] // a + aB | ligation: a + aB <-> aaB
        |]
        |> Array.sum


    // 63 - aaa
    let d63 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -kW * (2.0 * xSum - x.[63]) * x.[63]
            -0.0001 * x.[63] // aaa | ligation: a + aa <-> aaa
            0.001 * x.[3] * x.[15] // a + aa | ligation: a + aa <-> aaa
        |]
        |> Array.sum


    // 64 - aab
    let d64 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -kW * (2.0 * xSum - x.[64]) * x.[64]
            -0.0001 * x.[64] // aab | ligation: a + ab <-> aab
            0.001 * x.[3] * x.[16] // a + ab | ligation: a + ab <-> aab
        |]
        |> Array.sum


    // 65 - abA
    let d65 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -kW * (2.0 * xSum - x.[65]) * x.[65]
            -0.0001 * x.[65] // abA | ligation: a + bA <-> abA
            0.001 * x.[3] * x.[17] // a + bA | ligation: a + bA <-> abA
        |]
        |> Array.sum


    // 66 - abB
    let d66 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -kW * (2.0 * xSum - x.[66]) * x.[66]
            -0.0001 * x.[66] // abB | ligation: a + bB <-> abB
            0.001 * x.[3] * x.[18] // a + bB | ligation: a + bB <-> abB
        |]
        |> Array.sum


    // 67 - aba
    let d67 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -kW * (2.0 * xSum - x.[67]) * x.[67]
            -0.0001 * x.[67] // aba | ligation: a + ba <-> aba
            0.001 * x.[3] * x.[19] // a + ba | ligation: a + ba <-> aba
        |]
        |> Array.sum


    // 68 - abb
    let d68 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -kW * (2.0 * xSum - x.[68]) * x.[68]
            -0.0001 * x.[68] // abb | ligation: a + bb <-> abb
            0.001 * x.[3] * x.[20] // a + bb | ligation: a + bb <-> abb
        |]
        |> Array.sum


    // 69 - bAA
    let d69 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -kW * (2.0 * xSum - x.[69]) * x.[69]
            -0.0001 * x.[69] // bAA | ligation: b + AA <-> bAA
            0.001 * x.[4] * x.[5] // b + AA | ligation: b + AA <-> bAA
        |]
        |> Array.sum


    // 70 - bAB
    let d70 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -kW * (2.0 * xSum - x.[70]) * x.[70]
            -0.0001 * x.[70] // bAB | ligation: b + AB <-> bAB
            0.001 * x.[4] * x.[6] // b + AB | ligation: b + AB <-> bAB
        |]
        |> Array.sum


    // 71 - bAa
    let d71 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -kW * (2.0 * xSum - x.[71]) * x.[71]
            -0.0001 * x.[71] // bAa | ligation: b + Aa <-> bAa
            0.001 * x.[4] * x.[7] // b + Aa | ligation: b + Aa <-> bAa
        |]
        |> Array.sum


    // 72 - bAb
    let d72 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -kW * (2.0 * xSum - x.[72]) * x.[72]
            -0.0001 * x.[72] // bAb | ligation: b + Ab <-> bAb
            0.001 * x.[4] * x.[8] // b + Ab | ligation: b + Ab <-> bAb
        |]
        |> Array.sum


    // 73 - bBA
    let d73 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -kW * (2.0 * xSum - x.[73]) * x.[73]
            -0.0001 * x.[73] // bBA | ligation: b + BA <-> bBA
            0.001 * x.[4] * x.[9] // b + BA | ligation: b + BA <-> bBA
        |]
        |> Array.sum


    // 74 - bBB
    let d74 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -kW * (2.0 * xSum - x.[74]) * x.[74]
            -0.0001 * x.[74] // bBB | ligation: b + BB <-> bBB
            0.001 * x.[4] * x.[10] // b + BB | ligation: b + BB <-> bBB
        |]
        |> Array.sum


    // 75 - bBa
    let d75 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -kW * (2.0 * xSum - x.[75]) * x.[75]
            -0.0001 * x.[75] // bBa | ligation: b + Ba <-> bBa
            0.001 * x.[4] * x.[11] // b + Ba | ligation: b + Ba <-> bBa
        |]
        |> Array.sum


    // 76 - bBb
    let d76 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -kW * (2.0 * xSum - x.[76]) * x.[76]
            -0.0001 * x.[76] // bBb | ligation: b + Bb <-> bBb
            0.001 * x.[4] * x.[12] // b + Bb | ligation: b + Bb <-> bBb
        |]
        |> Array.sum


    // 77 - baA
    let d77 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -kW * (2.0 * xSum - x.[77]) * x.[77]
            -0.0001 * x.[77] // baA | ligation: b + aA <-> baA
            0.001 * x.[4] * x.[13] // b + aA | ligation: b + aA <-> baA
        |]
        |> Array.sum


    // 78 - baB
    let d78 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -kW * (2.0 * xSum - x.[78]) * x.[78]
            -0.0001 * x.[78] // baB | ligation: b + aB <-> baB
            0.001 * x.[4] * x.[14] // b + aB | ligation: b + aB <-> baB
        |]
        |> Array.sum


    // 79 - baa
    let d79 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -kW * (2.0 * xSum - x.[79]) * x.[79]
            -0.0001 * x.[79] // baa | ligation: b + aa <-> baa
            0.001 * x.[4] * x.[15] // b + aa | ligation: b + aa <-> baa
        |]
        |> Array.sum


    // 80 - bab
    let d80 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -kW * (2.0 * xSum - x.[80]) * x.[80]
            -0.0001 * x.[80] // bab | ligation: b + ab <-> bab
            0.001 * x.[4] * x.[16] // b + ab | ligation: b + ab <-> bab
        |]
        |> Array.sum


    // 81 - bbA
    let d81 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -kW * (2.0 * xSum - x.[81]) * x.[81]
            -0.0001 * x.[81] // bbA | ligation: b + bA <-> bbA
            0.001 * x.[4] * x.[17] // b + bA | ligation: b + bA <-> bbA
        |]
        |> Array.sum


    // 82 - bbB
    let d82 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -kW * (2.0 * xSum - x.[82]) * x.[82]
            -0.0001 * x.[82] // bbB | ligation: b + bB <-> bbB
            0.001 * x.[4] * x.[18] // b + bB | ligation: b + bB <-> bbB
        |]
        |> Array.sum


    // 83 - bba
    let d83 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -kW * (2.0 * xSum - x.[83]) * x.[83]
            -0.0001 * x.[83] // bba | ligation: b + ba <-> bba
            0.001 * x.[4] * x.[19] // b + ba | ligation: b + ba <-> bba
        |]
        |> Array.sum


    // 84 - bbb
    let d84 (x : array<double>) xSum xSumN xSumSquaredN = 
        [|
            -kW * (2.0 * xSum - x.[84]) * x.[84]
            -0.0001 * x.[84] // bbb | ligation: b + bb <-> bbb
            0.001 * x.[4] * x.[20] // b + bb | ligation: b + bb <-> bbb
        |]
        |> Array.sum

    let update (x : array<double>) : array<double> = 

        // printfn "update::Starting..."

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
            d0 x xSum xSumN xSumSquaredN
            d1 x xSum xSumN xSumSquaredN
            d2 x xSum xSumN xSumSquaredN
            d3 x xSum xSumN xSumSquaredN
            d4 x xSum xSumN xSumSquaredN
            d5 x xSum xSumN xSumSquaredN
            d6 x xSum xSumN xSumSquaredN
            d7 x xSum xSumN xSumSquaredN
            d8 x xSum xSumN xSumSquaredN
            d9 x xSum xSumN xSumSquaredN
            d10 x xSum xSumN xSumSquaredN
            d11 x xSum xSumN xSumSquaredN
            d12 x xSum xSumN xSumSquaredN
            d13 x xSum xSumN xSumSquaredN
            d14 x xSum xSumN xSumSquaredN
            d15 x xSum xSumN xSumSquaredN
            d16 x xSum xSumN xSumSquaredN
            d17 x xSum xSumN xSumSquaredN
            d18 x xSum xSumN xSumSquaredN
            d19 x xSum xSumN xSumSquaredN
            d20 x xSum xSumN xSumSquaredN
            d21 x xSum xSumN xSumSquaredN
            d22 x xSum xSumN xSumSquaredN
            d23 x xSum xSumN xSumSquaredN
            d24 x xSum xSumN xSumSquaredN
            d25 x xSum xSumN xSumSquaredN
            d26 x xSum xSumN xSumSquaredN
            d27 x xSum xSumN xSumSquaredN
            d28 x xSum xSumN xSumSquaredN
            d29 x xSum xSumN xSumSquaredN
            d30 x xSum xSumN xSumSquaredN
            d31 x xSum xSumN xSumSquaredN
            d32 x xSum xSumN xSumSquaredN
            d33 x xSum xSumN xSumSquaredN
            d34 x xSum xSumN xSumSquaredN
            d35 x xSum xSumN xSumSquaredN
            d36 x xSum xSumN xSumSquaredN
            d37 x xSum xSumN xSumSquaredN
            d38 x xSum xSumN xSumSquaredN
            d39 x xSum xSumN xSumSquaredN
            d40 x xSum xSumN xSumSquaredN
            d41 x xSum xSumN xSumSquaredN
            d42 x xSum xSumN xSumSquaredN
            d43 x xSum xSumN xSumSquaredN
            d44 x xSum xSumN xSumSquaredN
            d45 x xSum xSumN xSumSquaredN
            d46 x xSum xSumN xSumSquaredN
            d47 x xSum xSumN xSumSquaredN
            d48 x xSum xSumN xSumSquaredN
            d49 x xSum xSumN xSumSquaredN
            d50 x xSum xSumN xSumSquaredN
            d51 x xSum xSumN xSumSquaredN
            d52 x xSum xSumN xSumSquaredN
            d53 x xSum xSumN xSumSquaredN
            d54 x xSum xSumN xSumSquaredN
            d55 x xSum xSumN xSumSquaredN
            d56 x xSum xSumN xSumSquaredN
            d57 x xSum xSumN xSumSquaredN
            d58 x xSum xSumN xSumSquaredN
            d59 x xSum xSumN xSumSquaredN
            d60 x xSum xSumN xSumSquaredN
            d61 x xSum xSumN xSumSquaredN
            d62 x xSum xSumN xSumSquaredN
            d63 x xSum xSumN xSumSquaredN
            d64 x xSum xSumN xSumSquaredN
            d65 x xSum xSumN xSumSquaredN
            d66 x xSum xSumN xSumSquaredN
            d67 x xSum xSumN xSumSquaredN
            d68 x xSum xSumN xSumSquaredN
            d69 x xSum xSumN xSumSquaredN
            d70 x xSum xSumN xSumSquaredN
            d71 x xSum xSumN xSumSquaredN
            d72 x xSum xSumN xSumSquaredN
            d73 x xSum xSumN xSumSquaredN
            d74 x xSum xSumN xSumSquaredN
            d75 x xSum xSumN xSumSquaredN
            d76 x xSum xSumN xSumSquaredN
            d77 x xSum xSumN xSumSquaredN
            d78 x xSum xSumN xSumSquaredN
            d79 x xSum xSumN xSumSquaredN
            d80 x xSum xSumN xSumSquaredN
            d81 x xSum xSumN xSumSquaredN
            d82 x xSum xSumN xSumSquaredN
            d83 x xSum xSumN xSumSquaredN
            d84 x xSum xSumN xSumSquaredN
        |]


    let modelDataParamsWithExtraData = 
        {
            modelDataParams = 
                {
                    modelInfo = 
                        {
                            versionNumber = "1.0.0.0"
                            seedValue = seedValue
                            modelName = "20181129_05"
                            numberOfSubstances = 85
                            numberOfAminoAcids = TwoAminoAcids
                            maxPeptideLength = ThreeMax
                        }

                    allParams = 
                        [
                            {
                                synthesisDistribution = DeltaDistribution(1251686301, { threshold = None }) |> Delta
                                forwardScale = Some 0.001
                                backwardScale = Some 0.0001
                            }
                            |> SynthesisRateParam

                            {
                                catSynthDistribution = UniformDistribution(880727927, { threshold = Some 0.0005 }) |> Uniform
                                multiplier = 1000.0
                                maxEe = 0.05
                            }
                            |> CatalyticSynthesisRateParam

                            {
                                ligationDistribution = DeltaDistribution(875999892, { threshold = None }) |> Delta
                                forwardScale = Some 0.001
                                backwardScale = Some 0.0001
                            }
                            |> LigationRateParam

                            {
                                catLigationDistribution = UniformDistribution(1389856627, { threshold = Some 0.0001 }) |> Uniform
                                multiplier = 1000.0
                                maxEe = 0.05
                            }
                            |> CatalyticLigationRateParam

                            {
                                sedimentationDirectDistribution = TriangularDistribution(666798682, { threshold = Some 0.0001 }) |> Triangular
                                forwardScale = Some 100.0
                            }
                            |> SedimentationDirectRateParam

                            {
                                sedimentationAllDistribution = UniformDistribution(1581478358, { threshold = None }) |> Uniform
                                forwardScale = Some 0.1
                            }
                            |> SedimentationAllRateParam

                        ]
                }

            getTotals = getTotals
            getTotalSubst = getTotalSubst
            allSubst = allSubst
            allInd = allInd

            allRawReactions = 
                [
                    (SynthesisName, 4)
                    (CatalyticSynthesisName, 320)
                    (LigationName, 39)
                    (CatalyticLigationName, 3120)
                    (SedimentationDirectName, 2331)
                    (SedimentationAllName, 4)
                ]

            allReactions = 
                [
                    (SynthesisName, 4)
                    (LigationName, 78)
                    (CatalyticLigationName, 2)
                ]
        }

