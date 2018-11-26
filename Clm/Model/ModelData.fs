namespace Model

open Clm.Substances
open Clm.Model

module ModelData = 
    let seedValue = 123456
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

    let kW = 0.055415258535843 / 84.0


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
                5.0 * 55.3443986861371 * x.[17] * x.[55] // bA + aAa | SedimentationDirectName: bA + aAa -> 5 Y
                5.0 * 55.3443986861371 * x.[11] * x.[29] // Ba + AaA | SedimentationDirectName: Ba + AaA -> 5 Y
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
                0.001 * x.[17] // bA | LigationName: b + A <-> bA
                -0.01 * x.[4] * x.[1] // b + A | LigationName: b + A <-> bA
                0.001 * x.[36] // Abb | LigationName: A + bb <-> Abb
                -0.01 * x.[1] * x.[20] // A + bb | LigationName: A + bb <-> Abb
                0.001 * x.[35] // Aba | LigationName: A + ba <-> Aba
                -0.01 * x.[1] * x.[19] // A + ba | LigationName: A + ba <-> Aba
                0.001 * x.[34] // AbB | LigationName: A + bB <-> AbB
                -0.01 * x.[1] * x.[18] // A + bB | LigationName: A + bB <-> AbB
                0.001 * x.[33] // AbA | LigationName: A + bA <-> AbA
                -0.01 * x.[1] * x.[17] // A + bA | LigationName: A + bA <-> AbA
                0.001 * x.[32] // Aab | LigationName: A + ab <-> Aab
                -0.01 * x.[1] * x.[16] // A + ab | LigationName: A + ab <-> Aab
                0.001 * x.[31] // Aaa | LigationName: A + aa <-> Aaa
                -0.01 * x.[1] * x.[15] // A + aa | LigationName: A + aa <-> Aaa
                0.001 * x.[30] // AaB | LigationName: A + aB <-> AaB
                -0.01 * x.[1] * x.[14] // A + aB | LigationName: A + aB <-> AaB
                0.001 * x.[29] // AaA | LigationName: A + aA <-> AaA
                -0.01 * x.[1] * x.[13] // A + aA | LigationName: A + aA <-> AaA
                0.001 * x.[28] // ABb | LigationName: A + Bb <-> ABb
                -0.01 * x.[1] * x.[12] // A + Bb | LigationName: A + Bb <-> ABb
                0.001 * x.[27] // ABa | LigationName: A + Ba <-> ABa
                -0.01 * x.[1] * x.[11] // A + Ba | LigationName: A + Ba <-> ABa
                0.001 * x.[26] // ABB | LigationName: A + BB <-> ABB
                -0.01 * x.[1] * x.[10] // A + BB | LigationName: A + BB <-> ABB
                0.001 * x.[25] // ABA | LigationName: A + BA <-> ABA
                -0.01 * x.[1] * x.[9] // A + BA | LigationName: A + BA <-> ABA
                0.001 * x.[24] // AAb | LigationName: A + Ab <-> AAb
                -0.01 * x.[1] * x.[8] // A + Ab | LigationName: A + Ab <-> AAb
                0.001 * x.[23] // AAa | LigationName: A + Aa <-> AAa
                -0.01 * x.[1] * x.[7] // A + Aa | LigationName: A + Aa <-> AAa
                0.001 * x.[22] // AAB | LigationName: A + AB <-> AAB
                -0.01 * x.[1] * x.[6] // A + AB | LigationName: A + AB <-> AAB
                0.001 * x.[21] // AAA | LigationName: A + AA <-> AAA
                -0.01 * x.[1] * x.[5] // A + AA | LigationName: A + AA <-> AAA
                0.001 * x.[8] // Ab | LigationName: A + b <-> Ab
                -0.01 * x.[1] * x.[4] // A + b | LigationName: A + b <-> Ab
                0.001 * x.[13] // aA | LigationName: a + A <-> aA
                -0.01 * x.[3] * x.[1] // a + A | LigationName: a + A <-> aA
                0.001 * x.[7] // Aa | LigationName: A + a <-> Aa
                -0.01 * x.[1] * x.[3] // A + a | LigationName: A + a <-> Aa
                0.001 * x.[6] // AB | LigationName: A + B <-> AB
                -0.01 * x.[1] * x.[2] // A + B | LigationName: A + B <-> AB
                0.001 * x.[5] // AA | LigationName: A + A <-> AA
                0.001 * x.[5] // AA | LigationName: A + A <-> AA
                -0.01 * x.[1] * x.[1] // A + A | LigationName: A + A <-> AA
                -0.01 * x.[1] * x.[1] // A + A | LigationName: A + A <-> AA
                -0.001 * x.[1] // A | SynthesisName: Y <-> A
                0.01 * x.[0] // Y | SynthesisName: Y <-> A
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 2 - B
            [|
                -kW * (2.0 * xSum - x.[2]) * x.[2]
                0.001 * x.[52] // Bbb | LigationName: B + bb <-> Bbb
                -0.01 * x.[2] * x.[20] // B + bb | LigationName: B + bb <-> Bbb
                0.001 * x.[51] // Bba | LigationName: B + ba <-> Bba
                -0.01 * x.[2] * x.[19] // B + ba | LigationName: B + ba <-> Bba
                0.001 * x.[50] // BbB | LigationName: B + bB <-> BbB
                -0.01 * x.[2] * x.[18] // B + bB | LigationName: B + bB <-> BbB
                0.001 * x.[49] // BbA | LigationName: B + bA <-> BbA
                -0.01 * x.[2] * x.[17] // B + bA | LigationName: B + bA <-> BbA
                0.001 * x.[48] // Bab | LigationName: B + ab <-> Bab
                -0.01 * x.[2] * x.[16] // B + ab | LigationName: B + ab <-> Bab
                0.001 * x.[47] // Baa | LigationName: B + aa <-> Baa
                -0.01 * x.[2] * x.[15] // B + aa | LigationName: B + aa <-> Baa
                0.001 * x.[46] // BaB | LigationName: B + aB <-> BaB
                -0.01 * x.[2] * x.[14] // B + aB | LigationName: B + aB <-> BaB
                0.001 * x.[45] // BaA | LigationName: B + aA <-> BaA
                -0.01 * x.[2] * x.[13] // B + aA | LigationName: B + aA <-> BaA
                0.001 * x.[44] // BBb | LigationName: B + Bb <-> BBb
                -0.01 * x.[2] * x.[12] // B + Bb | LigationName: B + Bb <-> BBb
                0.001 * x.[43] // BBa | LigationName: B + Ba <-> BBa
                -0.01 * x.[2] * x.[11] // B + Ba | LigationName: B + Ba <-> BBa
                0.001 * x.[42] // BBB | LigationName: B + BB <-> BBB
                -0.01 * x.[2] * x.[10] // B + BB | LigationName: B + BB <-> BBB
                0.001 * x.[41] // BBA | LigationName: B + BA <-> BBA
                -0.01 * x.[2] * x.[9] // B + BA | LigationName: B + BA <-> BBA
                0.001 * x.[40] // BAb | LigationName: B + Ab <-> BAb
                -0.01 * x.[2] * x.[8] // B + Ab | LigationName: B + Ab <-> BAb
                0.001 * x.[39] // BAa | LigationName: B + Aa <-> BAa
                -0.01 * x.[2] * x.[7] // B + Aa | LigationName: B + Aa <-> BAa
                0.001 * x.[38] // BAB | LigationName: B + AB <-> BAB
                -0.01 * x.[2] * x.[6] // B + AB | LigationName: B + AB <-> BAB
                0.001 * x.[37] // BAA | LigationName: B + AA <-> BAA
                -0.01 * x.[2] * x.[5] // B + AA | LigationName: B + AA <-> BAA
                0.001 * x.[18] // bB | LigationName: b + B <-> bB
                -0.01 * x.[4] * x.[2] // b + B | LigationName: b + B <-> bB
                0.001 * x.[12] // Bb | LigationName: B + b <-> Bb
                -0.01 * x.[2] * x.[4] // B + b | LigationName: B + b <-> Bb
                0.001 * x.[11] // Ba | LigationName: B + a <-> Ba
                -0.01 * x.[2] * x.[3] // B + a | LigationName: B + a <-> Ba
                0.001 * x.[10] // BB | LigationName: B + B <-> BB
                0.001 * x.[10] // BB | LigationName: B + B <-> BB
                -0.01 * x.[2] * x.[2] // B + B | LigationName: B + B <-> BB
                -0.01 * x.[2] * x.[2] // B + B | LigationName: B + B <-> BB
                0.001 * x.[14] // aB | LigationName: a + B <-> aB
                -0.01 * x.[3] * x.[2] // a + B | LigationName: a + B <-> aB
                0.001 * x.[6] // AB | LigationName: A + B <-> AB
                -0.01 * x.[1] * x.[2] // A + B | LigationName: A + B <-> AB
                -0.001 * x.[2] // B | SynthesisName: Y <-> B
                0.01 * x.[0] // Y | SynthesisName: Y <-> B
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 3 - a
            [|
                -kW * (2.0 * xSum - x.[3]) * x.[3]
                0.001 * x.[11] // Ba | LigationName: B + a <-> Ba
                -0.01 * x.[2] * x.[3] // B + a | LigationName: B + a <-> Ba
                0.001 * x.[58] // aBB | LigationName: a + BB <-> aBB
                -0.01 * x.[3] * x.[10] // a + BB | LigationName: a + BB <-> aBB
                0.001 * x.[57] // aBA | LigationName: a + BA <-> aBA
                -0.01 * x.[3] * x.[9] // a + BA | LigationName: a + BA <-> aBA
                0.001 * x.[60] // aBb | LigationName: a + Bb <-> aBb
                -0.01 * x.[3] * x.[12] // a + Bb | LigationName: a + Bb <-> aBb
                0.001 * x.[59] // aBa | LigationName: a + Ba <-> aBa
                -0.01 * x.[3] * x.[11] // a + Ba | LigationName: a + Ba <-> aBa
                0.001 * x.[54] // aAB | LigationName: a + AB <-> aAB
                -0.01 * x.[3] * x.[6] // a + AB | LigationName: a + AB <-> aAB
                0.001 * x.[53] // aAA | LigationName: a + AA <-> aAA
                -0.01 * x.[3] * x.[5] // a + AA | LigationName: a + AA <-> aAA
                0.001 * x.[56] // aAb | LigationName: a + Ab <-> aAb
                -0.01 * x.[3] * x.[8] // a + Ab | LigationName: a + Ab <-> aAb
                0.001 * x.[55] // aAa | LigationName: a + Aa <-> aAa
                -0.01 * x.[3] * x.[7] // a + Aa | LigationName: a + Aa <-> aAa
                0.001 * x.[66] // abB | LigationName: a + bB <-> abB
                -0.01 * x.[3] * x.[18] // a + bB | LigationName: a + bB <-> abB
                0.001 * x.[65] // abA | LigationName: a + bA <-> abA
                -0.01 * x.[3] * x.[17] // a + bA | LigationName: a + bA <-> abA
                0.001 * x.[68] // abb | LigationName: a + bb <-> abb
                -0.01 * x.[3] * x.[20] // a + bb | LigationName: a + bb <-> abb
                0.001 * x.[67] // aba | LigationName: a + ba <-> aba
                -0.01 * x.[3] * x.[19] // a + ba | LigationName: a + ba <-> aba
                0.001 * x.[62] // aaB | LigationName: a + aB <-> aaB
                -0.01 * x.[3] * x.[14] // a + aB | LigationName: a + aB <-> aaB
                0.001 * x.[61] // aaA | LigationName: a + aA <-> aaA
                -0.01 * x.[3] * x.[13] // a + aA | LigationName: a + aA <-> aaA
                0.001 * x.[64] // aab | LigationName: a + ab <-> aab
                -0.01 * x.[3] * x.[16] // a + ab | LigationName: a + ab <-> aab
                0.001 * x.[63] // aaa | LigationName: a + aa <-> aaa
                -0.01 * x.[3] * x.[15] // a + aa | LigationName: a + aa <-> aaa
                0.001 * x.[14] // aB | LigationName: a + B <-> aB
                -0.01 * x.[3] * x.[2] // a + B | LigationName: a + B <-> aB
                0.001 * x.[13] // aA | LigationName: a + A <-> aA
                -0.01 * x.[3] * x.[1] // a + A | LigationName: a + A <-> aA
                0.001 * x.[7] // Aa | LigationName: A + a <-> Aa
                -0.01 * x.[1] * x.[3] // A + a | LigationName: A + a <-> Aa
                0.001 * x.[16] // ab | LigationName: a + b <-> ab
                -0.01 * x.[3] * x.[4] // a + b | LigationName: a + b <-> ab
                0.001 * x.[15] // aa | LigationName: a + a <-> aa
                0.001 * x.[15] // aa | LigationName: a + a <-> aa
                -0.01 * x.[3] * x.[3] // a + a | LigationName: a + a <-> aa
                -0.01 * x.[3] * x.[3] // a + a | LigationName: a + a <-> aa
                -0.001 * x.[3] // a | SynthesisName: Y <-> a
                0.01 * x.[0] // Y | SynthesisName: Y <-> a
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 4 - b
            [|
                -kW * (2.0 * xSum - x.[4]) * x.[4]
                0.001 * x.[74] // bBB | LigationName: b + BB <-> bBB
                -0.01 * x.[4] * x.[10] // b + BB | LigationName: b + BB <-> bBB
                0.001 * x.[73] // bBA | LigationName: b + BA <-> bBA
                -0.01 * x.[4] * x.[9] // b + BA | LigationName: b + BA <-> bBA
                0.001 * x.[76] // bBb | LigationName: b + Bb <-> bBb
                -0.01 * x.[4] * x.[12] // b + Bb | LigationName: b + Bb <-> bBb
                0.001 * x.[75] // bBa | LigationName: b + Ba <-> bBa
                -0.01 * x.[4] * x.[11] // b + Ba | LigationName: b + Ba <-> bBa
                0.001 * x.[70] // bAB | LigationName: b + AB <-> bAB
                -0.01 * x.[4] * x.[6] // b + AB | LigationName: b + AB <-> bAB
                0.001 * x.[69] // bAA | LigationName: b + AA <-> bAA
                -0.01 * x.[4] * x.[5] // b + AA | LigationName: b + AA <-> bAA
                0.001 * x.[72] // bAb | LigationName: b + Ab <-> bAb
                -0.01 * x.[4] * x.[8] // b + Ab | LigationName: b + Ab <-> bAb
                0.001 * x.[71] // bAa | LigationName: b + Aa <-> bAa
                -0.01 * x.[4] * x.[7] // b + Aa | LigationName: b + Aa <-> bAa
                0.001 * x.[82] // bbB | LigationName: b + bB <-> bbB
                -0.01 * x.[4] * x.[18] // b + bB | LigationName: b + bB <-> bbB
                0.001 * x.[81] // bbA | LigationName: b + bA <-> bbA
                -0.01 * x.[4] * x.[17] // b + bA | LigationName: b + bA <-> bbA
                0.001 * x.[84] // bbb | LigationName: b + bb <-> bbb
                -0.01 * x.[4] * x.[20] // b + bb | LigationName: b + bb <-> bbb
                0.001 * x.[83] // bba | LigationName: b + ba <-> bba
                -0.01 * x.[4] * x.[19] // b + ba | LigationName: b + ba <-> bba
                0.001 * x.[78] // baB | LigationName: b + aB <-> baB
                -0.01 * x.[4] * x.[14] // b + aB | LigationName: b + aB <-> baB
                0.001 * x.[77] // baA | LigationName: b + aA <-> baA
                -0.01 * x.[4] * x.[13] // b + aA | LigationName: b + aA <-> baA
                0.001 * x.[80] // bab | LigationName: b + ab <-> bab
                -0.01 * x.[4] * x.[16] // b + ab | LigationName: b + ab <-> bab
                0.001 * x.[79] // baa | LigationName: b + aa <-> baa
                -0.01 * x.[4] * x.[15] // b + aa | LigationName: b + aa <-> baa
                0.001 * x.[18] // bB | LigationName: b + B <-> bB
                -0.01 * x.[4] * x.[2] // b + B | LigationName: b + B <-> bB
                0.001 * x.[12] // Bb | LigationName: B + b <-> Bb
                -0.01 * x.[2] * x.[4] // B + b | LigationName: B + b <-> Bb
                0.001 * x.[17] // bA | LigationName: b + A <-> bA
                -0.01 * x.[4] * x.[1] // b + A | LigationName: b + A <-> bA
                0.001 * x.[20] // bb | LigationName: b + b <-> bb
                0.001 * x.[20] // bb | LigationName: b + b <-> bb
                -0.01 * x.[4] * x.[4] // b + b | LigationName: b + b <-> bb
                -0.01 * x.[4] * x.[4] // b + b | LigationName: b + b <-> bb
                0.001 * x.[8] // Ab | LigationName: A + b <-> Ab
                -0.01 * x.[1] * x.[4] // A + b | LigationName: A + b <-> Ab
                0.001 * x.[16] // ab | LigationName: a + b <-> ab
                -0.01 * x.[3] * x.[4] // a + b | LigationName: a + b <-> ab
                -0.001 * x.[4] // b | SynthesisName: Y <-> b
                0.01 * x.[0] // Y | SynthesisName: Y <-> b
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 5 - AA
            [|
                -kW * (2.0 * xSum - x.[5]) * x.[5]
                0.001 * x.[69] // bAA | LigationName: b + AA <-> bAA
                -0.01 * x.[4] * x.[5] // b + AA | LigationName: b + AA <-> bAA
                0.001 * x.[37] // BAA | LigationName: B + AA <-> BAA
                -0.01 * x.[2] * x.[5] // B + AA | LigationName: B + AA <-> BAA
                0.001 * x.[53] // aAA | LigationName: a + AA <-> aAA
                -0.01 * x.[3] * x.[5] // a + AA | LigationName: a + AA <-> aAA
                0.001 * x.[21] // AAA | LigationName: A + AA <-> AAA
                -0.01 * x.[1] * x.[5] // A + AA | LigationName: A + AA <-> AAA
                -0.001 * x.[5] // AA | LigationName: A + A <-> AA
                0.01 * x.[1] * x.[1] // A + A | LigationName: A + A <-> AA
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 6 - AB
            [|
                -kW * (2.0 * xSum - x.[6]) * x.[6]
                0.001 * x.[70] // bAB | LigationName: b + AB <-> bAB
                -0.01 * x.[4] * x.[6] // b + AB | LigationName: b + AB <-> bAB
                0.001 * x.[38] // BAB | LigationName: B + AB <-> BAB
                -0.01 * x.[2] * x.[6] // B + AB | LigationName: B + AB <-> BAB
                0.001 * x.[54] // aAB | LigationName: a + AB <-> aAB
                -0.01 * x.[3] * x.[6] // a + AB | LigationName: a + AB <-> aAB
                0.001 * x.[22] // AAB | LigationName: A + AB <-> AAB
                -0.01 * x.[1] * x.[6] // A + AB | LigationName: A + AB <-> AAB
                -0.001 * x.[6] // AB | LigationName: A + B <-> AB
                0.01 * x.[1] * x.[2] // A + B | LigationName: A + B <-> AB
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 7 - Aa
            [|
                -kW * (2.0 * xSum - x.[7]) * x.[7]
                0.001 * x.[71] // bAa | LigationName: b + Aa <-> bAa
                -0.01 * x.[4] * x.[7] // b + Aa | LigationName: b + Aa <-> bAa
                0.001 * x.[39] // BAa | LigationName: B + Aa <-> BAa
                -0.01 * x.[2] * x.[7] // B + Aa | LigationName: B + Aa <-> BAa
                0.001 * x.[55] // aAa | LigationName: a + Aa <-> aAa
                -0.01 * x.[3] * x.[7] // a + Aa | LigationName: a + Aa <-> aAa
                0.001 * x.[23] // AAa | LigationName: A + Aa <-> AAa
                -0.01 * x.[1] * x.[7] // A + Aa | LigationName: A + Aa <-> AAa
                -0.001 * x.[7] // Aa | LigationName: A + a <-> Aa
                0.01 * x.[1] * x.[3] // A + a | LigationName: A + a <-> Aa
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 8 - Ab
            [|
                -kW * (2.0 * xSum - x.[8]) * x.[8]
                0.001 * x.[72] // bAb | LigationName: b + Ab <-> bAb
                -0.01 * x.[4] * x.[8] // b + Ab | LigationName: b + Ab <-> bAb
                0.001 * x.[40] // BAb | LigationName: B + Ab <-> BAb
                -0.01 * x.[2] * x.[8] // B + Ab | LigationName: B + Ab <-> BAb
                0.001 * x.[56] // aAb | LigationName: a + Ab <-> aAb
                -0.01 * x.[3] * x.[8] // a + Ab | LigationName: a + Ab <-> aAb
                0.001 * x.[24] // AAb | LigationName: A + Ab <-> AAb
                -0.01 * x.[1] * x.[8] // A + Ab | LigationName: A + Ab <-> AAb
                -0.001 * x.[8] // Ab | LigationName: A + b <-> Ab
                0.01 * x.[1] * x.[4] // A + b | LigationName: A + b <-> Ab
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 9 - BA
            [|
                -kW * (2.0 * xSum - x.[9]) * x.[9]
                0.001 * x.[73] // bBA | LigationName: b + BA <-> bBA
                -0.01 * x.[4] * x.[9] // b + BA | LigationName: b + BA <-> bBA
                0.001 * x.[41] // BBA | LigationName: B + BA <-> BBA
                -0.01 * x.[2] * x.[9] // B + BA | LigationName: B + BA <-> BBA
                0.001 * x.[57] // aBA | LigationName: a + BA <-> aBA
                -0.01 * x.[3] * x.[9] // a + BA | LigationName: a + BA <-> aBA
                0.001 * x.[25] // ABA | LigationName: A + BA <-> ABA
                -0.01 * x.[1] * x.[9] // A + BA | LigationName: A + BA <-> ABA
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 10 - BB
            [|
                -kW * (2.0 * xSum - x.[10]) * x.[10]
                0.001 * x.[74] // bBB | LigationName: b + BB <-> bBB
                -0.01 * x.[4] * x.[10] // b + BB | LigationName: b + BB <-> bBB
                0.001 * x.[42] // BBB | LigationName: B + BB <-> BBB
                -0.01 * x.[2] * x.[10] // B + BB | LigationName: B + BB <-> BBB
                -0.001 * x.[10] // BB | LigationName: B + B <-> BB
                0.01 * x.[2] * x.[2] // B + B | LigationName: B + B <-> BB
                0.001 * x.[58] // aBB | LigationName: a + BB <-> aBB
                -0.01 * x.[3] * x.[10] // a + BB | LigationName: a + BB <-> aBB
                0.001 * x.[26] // ABB | LigationName: A + BB <-> ABB
                -0.01 * x.[1] * x.[10] // A + BB | LigationName: A + BB <-> ABB
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 11 - Ba
            [|
                -kW * (2.0 * xSum - x.[11]) * x.[11]
                -55.3443986861371 * x.[11] * x.[29] // Ba + AaA | SedimentationDirectName: Ba + AaA -> 5 Y
                0.001 * x.[75] // bBa | LigationName: b + Ba <-> bBa
                -0.01 * x.[4] * x.[11] // b + Ba | LigationName: b + Ba <-> bBa
                0.001 * x.[43] // BBa | LigationName: B + Ba <-> BBa
                -0.01 * x.[2] * x.[11] // B + Ba | LigationName: B + Ba <-> BBa
                -0.001 * x.[11] // Ba | LigationName: B + a <-> Ba
                0.01 * x.[2] * x.[3] // B + a | LigationName: B + a <-> Ba
                0.001 * x.[59] // aBa | LigationName: a + Ba <-> aBa
                -0.01 * x.[3] * x.[11] // a + Ba | LigationName: a + Ba <-> aBa
                0.001 * x.[27] // ABa | LigationName: A + Ba <-> ABa
                -0.01 * x.[1] * x.[11] // A + Ba | LigationName: A + Ba <-> ABa
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 12 - Bb
            [|
                -kW * (2.0 * xSum - x.[12]) * x.[12]
                0.001 * x.[76] // bBb | LigationName: b + Bb <-> bBb
                -0.01 * x.[4] * x.[12] // b + Bb | LigationName: b + Bb <-> bBb
                0.001 * x.[44] // BBb | LigationName: B + Bb <-> BBb
                -0.01 * x.[2] * x.[12] // B + Bb | LigationName: B + Bb <-> BBb
                -0.001 * x.[12] // Bb | LigationName: B + b <-> Bb
                0.01 * x.[2] * x.[4] // B + b | LigationName: B + b <-> Bb
                0.001 * x.[60] // aBb | LigationName: a + Bb <-> aBb
                -0.01 * x.[3] * x.[12] // a + Bb | LigationName: a + Bb <-> aBb
                0.001 * x.[28] // ABb | LigationName: A + Bb <-> ABb
                -0.01 * x.[1] * x.[12] // A + Bb | LigationName: A + Bb <-> ABb
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 13 - aA
            [|
                -kW * (2.0 * xSum - x.[13]) * x.[13]
                0.001 * x.[45] // BaA | LigationName: B + aA <-> BaA
                -0.01 * x.[2] * x.[13] // B + aA | LigationName: B + aA <-> BaA
                0.001 * x.[77] // baA | LigationName: b + aA <-> baA
                -0.01 * x.[4] * x.[13] // b + aA | LigationName: b + aA <-> baA
                0.001 * x.[29] // AaA | LigationName: A + aA <-> AaA
                -0.01 * x.[1] * x.[13] // A + aA | LigationName: A + aA <-> AaA
                0.001 * x.[61] // aaA | LigationName: a + aA <-> aaA
                -0.01 * x.[3] * x.[13] // a + aA | LigationName: a + aA <-> aaA
                -0.001 * x.[13] // aA | LigationName: a + A <-> aA
                0.01 * x.[3] * x.[1] // a + A | LigationName: a + A <-> aA
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 14 - aB
            [|
                -kW * (2.0 * xSum - x.[14]) * x.[14]
                0.001 * x.[46] // BaB | LigationName: B + aB <-> BaB
                -0.01 * x.[2] * x.[14] // B + aB | LigationName: B + aB <-> BaB
                0.001 * x.[78] // baB | LigationName: b + aB <-> baB
                -0.01 * x.[4] * x.[14] // b + aB | LigationName: b + aB <-> baB
                0.001 * x.[30] // AaB | LigationName: A + aB <-> AaB
                -0.01 * x.[1] * x.[14] // A + aB | LigationName: A + aB <-> AaB
                0.001 * x.[62] // aaB | LigationName: a + aB <-> aaB
                -0.01 * x.[3] * x.[14] // a + aB | LigationName: a + aB <-> aaB
                -0.001 * x.[14] // aB | LigationName: a + B <-> aB
                0.01 * x.[3] * x.[2] // a + B | LigationName: a + B <-> aB
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 15 - aa
            [|
                -kW * (2.0 * xSum - x.[15]) * x.[15]
                0.001 * x.[47] // Baa | LigationName: B + aa <-> Baa
                -0.01 * x.[2] * x.[15] // B + aa | LigationName: B + aa <-> Baa
                0.001 * x.[79] // baa | LigationName: b + aa <-> baa
                -0.01 * x.[4] * x.[15] // b + aa | LigationName: b + aa <-> baa
                0.001 * x.[31] // Aaa | LigationName: A + aa <-> Aaa
                -0.01 * x.[1] * x.[15] // A + aa | LigationName: A + aa <-> Aaa
                0.001 * x.[63] // aaa | LigationName: a + aa <-> aaa
                -0.01 * x.[3] * x.[15] // a + aa | LigationName: a + aa <-> aaa
                -0.001 * x.[15] // aa | LigationName: a + a <-> aa
                0.01 * x.[3] * x.[3] // a + a | LigationName: a + a <-> aa
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 16 - ab
            [|
                -kW * (2.0 * xSum - x.[16]) * x.[16]
                0.001 * x.[48] // Bab | LigationName: B + ab <-> Bab
                -0.01 * x.[2] * x.[16] // B + ab | LigationName: B + ab <-> Bab
                0.001 * x.[80] // bab | LigationName: b + ab <-> bab
                -0.01 * x.[4] * x.[16] // b + ab | LigationName: b + ab <-> bab
                0.001 * x.[32] // Aab | LigationName: A + ab <-> Aab
                -0.01 * x.[1] * x.[16] // A + ab | LigationName: A + ab <-> Aab
                0.001 * x.[64] // aab | LigationName: a + ab <-> aab
                -0.01 * x.[3] * x.[16] // a + ab | LigationName: a + ab <-> aab
                -0.001 * x.[16] // ab | LigationName: a + b <-> ab
                0.01 * x.[3] * x.[4] // a + b | LigationName: a + b <-> ab
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 17 - bA
            [|
                -kW * (2.0 * xSum - x.[17]) * x.[17]
                -55.3443986861371 * x.[17] * x.[55] // bA + aAa | SedimentationDirectName: bA + aAa -> 5 Y
                0.001 * x.[49] // BbA | LigationName: B + bA <-> BbA
                -0.01 * x.[2] * x.[17] // B + bA | LigationName: B + bA <-> BbA
                0.001 * x.[81] // bbA | LigationName: b + bA <-> bbA
                -0.01 * x.[4] * x.[17] // b + bA | LigationName: b + bA <-> bbA
                -0.001 * x.[17] // bA | LigationName: b + A <-> bA
                0.01 * x.[4] * x.[1] // b + A | LigationName: b + A <-> bA
                0.001 * x.[33] // AbA | LigationName: A + bA <-> AbA
                -0.01 * x.[1] * x.[17] // A + bA | LigationName: A + bA <-> AbA
                0.001 * x.[65] // abA | LigationName: a + bA <-> abA
                -0.01 * x.[3] * x.[17] // a + bA | LigationName: a + bA <-> abA
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 18 - bB
            [|
                -kW * (2.0 * xSum - x.[18]) * x.[18]
                0.001 * x.[50] // BbB | LigationName: B + bB <-> BbB
                -0.01 * x.[2] * x.[18] // B + bB | LigationName: B + bB <-> BbB
                0.001 * x.[82] // bbB | LigationName: b + bB <-> bbB
                -0.01 * x.[4] * x.[18] // b + bB | LigationName: b + bB <-> bbB
                -0.001 * x.[18] // bB | LigationName: b + B <-> bB
                0.01 * x.[4] * x.[2] // b + B | LigationName: b + B <-> bB
                0.001 * x.[34] // AbB | LigationName: A + bB <-> AbB
                -0.01 * x.[1] * x.[18] // A + bB | LigationName: A + bB <-> AbB
                0.001 * x.[66] // abB | LigationName: a + bB <-> abB
                -0.01 * x.[3] * x.[18] // a + bB | LigationName: a + bB <-> abB
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 19 - ba
            [|
                -kW * (2.0 * xSum - x.[19]) * x.[19]
                0.001 * x.[51] // Bba | LigationName: B + ba <-> Bba
                -0.01 * x.[2] * x.[19] // B + ba | LigationName: B + ba <-> Bba
                0.001 * x.[83] // bba | LigationName: b + ba <-> bba
                -0.01 * x.[4] * x.[19] // b + ba | LigationName: b + ba <-> bba
                0.001 * x.[35] // Aba | LigationName: A + ba <-> Aba
                -0.01 * x.[1] * x.[19] // A + ba | LigationName: A + ba <-> Aba
                0.001 * x.[67] // aba | LigationName: a + ba <-> aba
                -0.01 * x.[3] * x.[19] // a + ba | LigationName: a + ba <-> aba
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 20 - bb
            [|
                -kW * (2.0 * xSum - x.[20]) * x.[20]
                0.001 * x.[52] // Bbb | LigationName: B + bb <-> Bbb
                -0.01 * x.[2] * x.[20] // B + bb | LigationName: B + bb <-> Bbb
                0.001 * x.[84] // bbb | LigationName: b + bb <-> bbb
                -0.01 * x.[4] * x.[20] // b + bb | LigationName: b + bb <-> bbb
                -0.001 * x.[20] // bb | LigationName: b + b <-> bb
                0.01 * x.[4] * x.[4] // b + b | LigationName: b + b <-> bb
                0.001 * x.[36] // Abb | LigationName: A + bb <-> Abb
                -0.01 * x.[1] * x.[20] // A + bb | LigationName: A + bb <-> Abb
                0.001 * x.[68] // abb | LigationName: a + bb <-> abb
                -0.01 * x.[3] * x.[20] // a + bb | LigationName: a + bb <-> abb
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 21 - AAA
            [|
                -kW * (2.0 * xSum - x.[21]) * x.[21]
                -0.001 * x.[21] // AAA | LigationName: A + AA <-> AAA
                0.01 * x.[1] * x.[5] // A + AA | LigationName: A + AA <-> AAA
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 22 - AAB
            [|
                -kW * (2.0 * xSum - x.[22]) * x.[22]
                -0.001 * x.[22] // AAB | LigationName: A + AB <-> AAB
                0.01 * x.[1] * x.[6] // A + AB | LigationName: A + AB <-> AAB
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 23 - AAa
            [|
                -kW * (2.0 * xSum - x.[23]) * x.[23]
                -0.001 * x.[23] // AAa | LigationName: A + Aa <-> AAa
                0.01 * x.[1] * x.[7] // A + Aa | LigationName: A + Aa <-> AAa
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 24 - AAb
            [|
                -kW * (2.0 * xSum - x.[24]) * x.[24]
                -0.001 * x.[24] // AAb | LigationName: A + Ab <-> AAb
                0.01 * x.[1] * x.[8] // A + Ab | LigationName: A + Ab <-> AAb
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 25 - ABA
            [|
                -kW * (2.0 * xSum - x.[25]) * x.[25]
                -0.001 * x.[25] // ABA | LigationName: A + BA <-> ABA
                0.01 * x.[1] * x.[9] // A + BA | LigationName: A + BA <-> ABA
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 26 - ABB
            [|
                -kW * (2.0 * xSum - x.[26]) * x.[26]
                -0.001 * x.[26] // ABB | LigationName: A + BB <-> ABB
                0.01 * x.[1] * x.[10] // A + BB | LigationName: A + BB <-> ABB
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 27 - ABa
            [|
                -kW * (2.0 * xSum - x.[27]) * x.[27]
                -0.001 * x.[27] // ABa | LigationName: A + Ba <-> ABa
                0.01 * x.[1] * x.[11] // A + Ba | LigationName: A + Ba <-> ABa
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 28 - ABb
            [|
                -kW * (2.0 * xSum - x.[28]) * x.[28]
                -0.001 * x.[28] // ABb | LigationName: A + Bb <-> ABb
                0.01 * x.[1] * x.[12] // A + Bb | LigationName: A + Bb <-> ABb
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 29 - AaA
            [|
                -kW * (2.0 * xSum - x.[29]) * x.[29]
                -55.3443986861371 * x.[11] * x.[29] // Ba + AaA | SedimentationDirectName: Ba + AaA -> 5 Y
                -0.001 * x.[29] // AaA | LigationName: A + aA <-> AaA
                0.01 * x.[1] * x.[13] // A + aA | LigationName: A + aA <-> AaA
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 30 - AaB
            [|
                -kW * (2.0 * xSum - x.[30]) * x.[30]
                -0.001 * x.[30] // AaB | LigationName: A + aB <-> AaB
                0.01 * x.[1] * x.[14] // A + aB | LigationName: A + aB <-> AaB
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 31 - Aaa
            [|
                -kW * (2.0 * xSum - x.[31]) * x.[31]
                -0.001 * x.[31] // Aaa | LigationName: A + aa <-> Aaa
                0.01 * x.[1] * x.[15] // A + aa | LigationName: A + aa <-> Aaa
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 32 - Aab
            [|
                -kW * (2.0 * xSum - x.[32]) * x.[32]
                -0.001 * x.[32] // Aab | LigationName: A + ab <-> Aab
                0.01 * x.[1] * x.[16] // A + ab | LigationName: A + ab <-> Aab
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 33 - AbA
            [|
                -kW * (2.0 * xSum - x.[33]) * x.[33]
                -0.001 * x.[33] // AbA | LigationName: A + bA <-> AbA
                0.01 * x.[1] * x.[17] // A + bA | LigationName: A + bA <-> AbA
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 34 - AbB
            [|
                -kW * (2.0 * xSum - x.[34]) * x.[34]
                -0.001 * x.[34] // AbB | LigationName: A + bB <-> AbB
                0.01 * x.[1] * x.[18] // A + bB | LigationName: A + bB <-> AbB
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 35 - Aba
            [|
                -kW * (2.0 * xSum - x.[35]) * x.[35]
                -0.001 * x.[35] // Aba | LigationName: A + ba <-> Aba
                0.01 * x.[1] * x.[19] // A + ba | LigationName: A + ba <-> Aba
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 36 - Abb
            [|
                -kW * (2.0 * xSum - x.[36]) * x.[36]
                -0.001 * x.[36] // Abb | LigationName: A + bb <-> Abb
                0.01 * x.[1] * x.[20] // A + bb | LigationName: A + bb <-> Abb
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 37 - BAA
            [|
                -kW * (2.0 * xSum - x.[37]) * x.[37]
                -0.001 * x.[37] // BAA | LigationName: B + AA <-> BAA
                0.01 * x.[2] * x.[5] // B + AA | LigationName: B + AA <-> BAA
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 38 - BAB
            [|
                -kW * (2.0 * xSum - x.[38]) * x.[38]
                -0.001 * x.[38] // BAB | LigationName: B + AB <-> BAB
                0.01 * x.[2] * x.[6] // B + AB | LigationName: B + AB <-> BAB
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 39 - BAa
            [|
                -kW * (2.0 * xSum - x.[39]) * x.[39]
                -0.001 * x.[39] // BAa | LigationName: B + Aa <-> BAa
                0.01 * x.[2] * x.[7] // B + Aa | LigationName: B + Aa <-> BAa
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 40 - BAb
            [|
                -kW * (2.0 * xSum - x.[40]) * x.[40]
                -0.001 * x.[40] // BAb | LigationName: B + Ab <-> BAb
                0.01 * x.[2] * x.[8] // B + Ab | LigationName: B + Ab <-> BAb
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 41 - BBA
            [|
                -kW * (2.0 * xSum - x.[41]) * x.[41]
                -0.001 * x.[41] // BBA | LigationName: B + BA <-> BBA
                0.01 * x.[2] * x.[9] // B + BA | LigationName: B + BA <-> BBA
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 42 - BBB
            [|
                -kW * (2.0 * xSum - x.[42]) * x.[42]
                -0.001 * x.[42] // BBB | LigationName: B + BB <-> BBB
                0.01 * x.[2] * x.[10] // B + BB | LigationName: B + BB <-> BBB
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 43 - BBa
            [|
                -kW * (2.0 * xSum - x.[43]) * x.[43]
                -0.001 * x.[43] // BBa | LigationName: B + Ba <-> BBa
                0.01 * x.[2] * x.[11] // B + Ba | LigationName: B + Ba <-> BBa
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 44 - BBb
            [|
                -kW * (2.0 * xSum - x.[44]) * x.[44]
                -0.001 * x.[44] // BBb | LigationName: B + Bb <-> BBb
                0.01 * x.[2] * x.[12] // B + Bb | LigationName: B + Bb <-> BBb
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 45 - BaA
            [|
                -kW * (2.0 * xSum - x.[45]) * x.[45]
                -0.001 * x.[45] // BaA | LigationName: B + aA <-> BaA
                0.01 * x.[2] * x.[13] // B + aA | LigationName: B + aA <-> BaA
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 46 - BaB
            [|
                -kW * (2.0 * xSum - x.[46]) * x.[46]
                -0.001 * x.[46] // BaB | LigationName: B + aB <-> BaB
                0.01 * x.[2] * x.[14] // B + aB | LigationName: B + aB <-> BaB
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 47 - Baa
            [|
                -kW * (2.0 * xSum - x.[47]) * x.[47]
                -0.001 * x.[47] // Baa | LigationName: B + aa <-> Baa
                0.01 * x.[2] * x.[15] // B + aa | LigationName: B + aa <-> Baa
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 48 - Bab
            [|
                -kW * (2.0 * xSum - x.[48]) * x.[48]
                -0.001 * x.[48] // Bab | LigationName: B + ab <-> Bab
                0.01 * x.[2] * x.[16] // B + ab | LigationName: B + ab <-> Bab
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 49 - BbA
            [|
                -kW * (2.0 * xSum - x.[49]) * x.[49]
                -0.001 * x.[49] // BbA | LigationName: B + bA <-> BbA
                0.01 * x.[2] * x.[17] // B + bA | LigationName: B + bA <-> BbA
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 50 - BbB
            [|
                -kW * (2.0 * xSum - x.[50]) * x.[50]
                -0.001 * x.[50] // BbB | LigationName: B + bB <-> BbB
                0.01 * x.[2] * x.[18] // B + bB | LigationName: B + bB <-> BbB
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 51 - Bba
            [|
                -kW * (2.0 * xSum - x.[51]) * x.[51]
                -0.001 * x.[51] // Bba | LigationName: B + ba <-> Bba
                0.01 * x.[2] * x.[19] // B + ba | LigationName: B + ba <-> Bba
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 52 - Bbb
            [|
                -kW * (2.0 * xSum - x.[52]) * x.[52]
                -0.001 * x.[52] // Bbb | LigationName: B + bb <-> Bbb
                0.01 * x.[2] * x.[20] // B + bb | LigationName: B + bb <-> Bbb
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 53 - aAA
            [|
                -kW * (2.0 * xSum - x.[53]) * x.[53]
                -0.001 * x.[53] // aAA | LigationName: a + AA <-> aAA
                0.01 * x.[3] * x.[5] // a + AA | LigationName: a + AA <-> aAA
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 54 - aAB
            [|
                -kW * (2.0 * xSum - x.[54]) * x.[54]
                -0.001 * x.[54] // aAB | LigationName: a + AB <-> aAB
                0.01 * x.[3] * x.[6] // a + AB | LigationName: a + AB <-> aAB
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 55 - aAa
            [|
                -kW * (2.0 * xSum - x.[55]) * x.[55]
                -55.3443986861371 * x.[17] * x.[55] // bA + aAa | SedimentationDirectName: bA + aAa -> 5 Y
                -0.001 * x.[55] // aAa | LigationName: a + Aa <-> aAa
                0.01 * x.[3] * x.[7] // a + Aa | LigationName: a + Aa <-> aAa
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 56 - aAb
            [|
                -kW * (2.0 * xSum - x.[56]) * x.[56]
                -0.001 * x.[56] // aAb | LigationName: a + Ab <-> aAb
                0.01 * x.[3] * x.[8] // a + Ab | LigationName: a + Ab <-> aAb
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 57 - aBA
            [|
                -kW * (2.0 * xSum - x.[57]) * x.[57]
                -0.001 * x.[57] // aBA | LigationName: a + BA <-> aBA
                0.01 * x.[3] * x.[9] // a + BA | LigationName: a + BA <-> aBA
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 58 - aBB
            [|
                -kW * (2.0 * xSum - x.[58]) * x.[58]
                -0.001 * x.[58] // aBB | LigationName: a + BB <-> aBB
                0.01 * x.[3] * x.[10] // a + BB | LigationName: a + BB <-> aBB
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 59 - aBa
            [|
                -kW * (2.0 * xSum - x.[59]) * x.[59]
                -0.001 * x.[59] // aBa | LigationName: a + Ba <-> aBa
                0.01 * x.[3] * x.[11] // a + Ba | LigationName: a + Ba <-> aBa
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 60 - aBb
            [|
                -kW * (2.0 * xSum - x.[60]) * x.[60]
                -0.001 * x.[60] // aBb | LigationName: a + Bb <-> aBb
                0.01 * x.[3] * x.[12] // a + Bb | LigationName: a + Bb <-> aBb
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 61 - aaA
            [|
                -kW * (2.0 * xSum - x.[61]) * x.[61]
                -0.001 * x.[61] // aaA | LigationName: a + aA <-> aaA
                0.01 * x.[3] * x.[13] // a + aA | LigationName: a + aA <-> aaA
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 62 - aaB
            [|
                -kW * (2.0 * xSum - x.[62]) * x.[62]
                -0.001 * x.[62] // aaB | LigationName: a + aB <-> aaB
                0.01 * x.[3] * x.[14] // a + aB | LigationName: a + aB <-> aaB
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 63 - aaa
            [|
                -kW * (2.0 * xSum - x.[63]) * x.[63]
                -0.001 * x.[63] // aaa | LigationName: a + aa <-> aaa
                0.01 * x.[3] * x.[15] // a + aa | LigationName: a + aa <-> aaa
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 64 - aab
            [|
                -kW * (2.0 * xSum - x.[64]) * x.[64]
                -0.001 * x.[64] // aab | LigationName: a + ab <-> aab
                0.01 * x.[3] * x.[16] // a + ab | LigationName: a + ab <-> aab
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 65 - abA
            [|
                -kW * (2.0 * xSum - x.[65]) * x.[65]
                -0.001 * x.[65] // abA | LigationName: a + bA <-> abA
                0.01 * x.[3] * x.[17] // a + bA | LigationName: a + bA <-> abA
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 66 - abB
            [|
                -kW * (2.0 * xSum - x.[66]) * x.[66]
                -0.001 * x.[66] // abB | LigationName: a + bB <-> abB
                0.01 * x.[3] * x.[18] // a + bB | LigationName: a + bB <-> abB
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 67 - aba
            [|
                -kW * (2.0 * xSum - x.[67]) * x.[67]
                -0.001 * x.[67] // aba | LigationName: a + ba <-> aba
                0.01 * x.[3] * x.[19] // a + ba | LigationName: a + ba <-> aba
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 68 - abb
            [|
                -kW * (2.0 * xSum - x.[68]) * x.[68]
                -0.001 * x.[68] // abb | LigationName: a + bb <-> abb
                0.01 * x.[3] * x.[20] // a + bb | LigationName: a + bb <-> abb
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 69 - bAA
            [|
                -kW * (2.0 * xSum - x.[69]) * x.[69]
                -0.001 * x.[69] // bAA | LigationName: b + AA <-> bAA
                0.01 * x.[4] * x.[5] // b + AA | LigationName: b + AA <-> bAA
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 70 - bAB
            [|
                -kW * (2.0 * xSum - x.[70]) * x.[70]
                -0.001 * x.[70] // bAB | LigationName: b + AB <-> bAB
                0.01 * x.[4] * x.[6] // b + AB | LigationName: b + AB <-> bAB
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 71 - bAa
            [|
                -kW * (2.0 * xSum - x.[71]) * x.[71]
                -0.001 * x.[71] // bAa | LigationName: b + Aa <-> bAa
                0.01 * x.[4] * x.[7] // b + Aa | LigationName: b + Aa <-> bAa
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 72 - bAb
            [|
                -kW * (2.0 * xSum - x.[72]) * x.[72]
                -0.001 * x.[72] // bAb | LigationName: b + Ab <-> bAb
                0.01 * x.[4] * x.[8] // b + Ab | LigationName: b + Ab <-> bAb
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 73 - bBA
            [|
                -kW * (2.0 * xSum - x.[73]) * x.[73]
                -0.001 * x.[73] // bBA | LigationName: b + BA <-> bBA
                0.01 * x.[4] * x.[9] // b + BA | LigationName: b + BA <-> bBA
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 74 - bBB
            [|
                -kW * (2.0 * xSum - x.[74]) * x.[74]
                -0.001 * x.[74] // bBB | LigationName: b + BB <-> bBB
                0.01 * x.[4] * x.[10] // b + BB | LigationName: b + BB <-> bBB
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 75 - bBa
            [|
                -kW * (2.0 * xSum - x.[75]) * x.[75]
                -0.001 * x.[75] // bBa | LigationName: b + Ba <-> bBa
                0.01 * x.[4] * x.[11] // b + Ba | LigationName: b + Ba <-> bBa
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 76 - bBb
            [|
                -kW * (2.0 * xSum - x.[76]) * x.[76]
                -0.001 * x.[76] // bBb | LigationName: b + Bb <-> bBb
                0.01 * x.[4] * x.[12] // b + Bb | LigationName: b + Bb <-> bBb
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 77 - baA
            [|
                -kW * (2.0 * xSum - x.[77]) * x.[77]
                -0.001 * x.[77] // baA | LigationName: b + aA <-> baA
                0.01 * x.[4] * x.[13] // b + aA | LigationName: b + aA <-> baA
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 78 - baB
            [|
                -kW * (2.0 * xSum - x.[78]) * x.[78]
                -0.001 * x.[78] // baB | LigationName: b + aB <-> baB
                0.01 * x.[4] * x.[14] // b + aB | LigationName: b + aB <-> baB
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 79 - baa
            [|
                -kW * (2.0 * xSum - x.[79]) * x.[79]
                -0.001 * x.[79] // baa | LigationName: b + aa <-> baa
                0.01 * x.[4] * x.[15] // b + aa | LigationName: b + aa <-> baa
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 80 - bab
            [|
                -kW * (2.0 * xSum - x.[80]) * x.[80]
                -0.001 * x.[80] // bab | LigationName: b + ab <-> bab
                0.01 * x.[4] * x.[16] // b + ab | LigationName: b + ab <-> bab
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 81 - bbA
            [|
                -kW * (2.0 * xSum - x.[81]) * x.[81]
                -0.001 * x.[81] // bbA | LigationName: b + bA <-> bbA
                0.01 * x.[4] * x.[17] // b + bA | LigationName: b + bA <-> bbA
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 82 - bbB
            [|
                -kW * (2.0 * xSum - x.[82]) * x.[82]
                -0.001 * x.[82] // bbB | LigationName: b + bB <-> bbB
                0.01 * x.[4] * x.[18] // b + bB | LigationName: b + bB <-> bbB
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 83 - bba
            [|
                -kW * (2.0 * xSum - x.[83]) * x.[83]
                -0.001 * x.[83] // bba | LigationName: b + ba <-> bba
                0.01 * x.[4] * x.[19] // b + ba | LigationName: b + ba <-> bba
            |]
            |> Array.fold (fun acc r -> acc + r) 0.0


            // 84 - bbb
            [|
                -kW * (2.0 * xSum - x.[84]) * x.[84]
                -0.001 * x.[84] // bbb | LigationName: b + bb <-> bbb
                0.01 * x.[4] * x.[20] // b + bb | LigationName: b + bb <-> bbb
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
            allSubst = allSubst
            allInd = allInd
        }

