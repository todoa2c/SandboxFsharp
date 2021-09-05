#load "CommentCsv.fsx"
open CommentCsv

/// マッチパターン * 曲名 のリスト
let songList =
    [ ("矛先", "愛情と矛先")
      ("アウフヘーベン", "アウフヘーベン")
      ("藍", "藍(あお)")
      ("青と夏", "青と夏")
      ("attitude", "Attitude")
      ("プリオリ", "ア・プリオリ")
      ("アボイド", "アボイドノート")
      ("アンゼンパイ", "アンゼンパイ")
      ("umbrella", "umbrella")
      ("morning", "In the Morning")
      ("インザ", "In the Morning")
      ("inspiration", "InsPirATioN")
      ("interlude", "InTerLuDe ～白い朝～")
      ("インフェルノ", "インフェルノ")
      ("flower", "WaLL FloWeR")
      ("wanted", "WanteD! WanteD!")
      ("嘘じゃないよ", "嘘じゃないよ")
      ("うブ", "うブ")
      ("えほん", "えほん")
      ("l.p", "L.P")
      ("lp", "L.P")
      ("oz", "Oz")
      ("おもちゃの兵隊", "おもちゃの兵隊")
      ("mind", "On My MiND")
      ("我逢人", "我逢人")
      ("キコリ時計", "キコリ時計")
      ("bee", "Ke-Mo Sah-Bee")
      ("キモサベ", "Ke-Mo Sah-Bee")
      ("鯨", "鯨の唄")
      ("クダリ", "クダリ")
      ("恋と吟", "恋と吟(うた)")
      ("coffee", "Coffee")
      ("conflict", "CONFLICT")
      ("circle", "Circle")
      ("サママ", "サママ・フェスティバル!")
      ("theater", "Theater")
      ("friend", "Just a Friend")
      ("journey", "JOURNEY")
      ("春愁", "春愁")
      ("庶幾", "庶幾の唄")
      ("simple", "SimPle")
      ("switch", "SwitCh")
      ("スターダム", "スターダム")
      ("start", "StaRt")
      ("speaking", "Speaking")
      ("soup", "Soup")
      ("splash", "SPLASH!!!")
      ("スマイロブドリーマ", "スマイロブドリーマ")
      ("they are", "They are")
      ("theyare", "They are")
      ("絶世生物", "絶世生物")
      ("ゼンマイ", "ゼンマイ")
      ("drink", "soFt-dRink")
      ("ソフトドリンク", "soFt-dRink")
      ("cheers", "CHEERS")
      ("月とアネモネ", "月とアネモネ")
      ("ツキマシテハ", "ツキマシテハ")
      ("点描", "点描の唄")
      ("道徳と皿", "道徳と皿")
      ("どこかで日は昇る", "どこかで日は昇る")
      ("灯火", "灯火")
      ("ナニヲナニヲ", "ナニヲナニヲ")
      ("no.7", "No.7")
      ("no7", "No.7")
      ("ノニサクウタ", "ノニサクウタ")
      ("viking", "Viking")
      ("how-to", "How-to")
      ("howto", "How-to")
      ("hug", "Hug")
      ("はじまり", "はじまり")
      ("party", "PARTY")
      ("パブリック", "パブリック")
      ("hello", "HeLLo")
      ("光のうた", "光のうた")
      ("vip", "VIP")
      ("日々と君", "日々と君")
      ("factory", "FACTORY")
      ("folktale", "Folktale")
      ("whoo whoo whoo", "WHOO WHOO WHOO")
      ("whoowhoowhoo", "WHOO WHOO WHOO")
      ("present", "PRESENT")
      ("propose", "ProPose")
      ("僕のこと", "僕のこと")
      ("ミスカサズ", "ミスカサズ")
      ("lion", "Lion")
      ("lovin", "lovin'")
      ("love me", "Love me); Love you")
      ("loveme", "Love me); Love you")
      ("リスキーゲーム", "リスキーゲーム")
      ("reverse", "REVERSE")
      ("log", "Log")
      ("ロマンチシズム", "ロマンチシズム")
      ("私", "私") ]

let songMap = Map.ofList songList

let extractSongs songMap (comment: string) =
    Map.filter (fun (key: string) _ -> comment.Contains(key)) songMap
    |> Map.toList
    |> List.map (fun (_, song) -> song)

/// 2次元のリストを1次元にフラット化する
let flatten lst = Seq.fold (@) List.empty lst

// let comments = stdin.ReadToEnd().Split('\n')
let comments =
    CommentCsv.CommentCsvType.ParseRows(stdin.ReadToEnd())

let counting collections =
    collections
    |> Seq.groupBy id
    |> Map.ofSeq
    |> Map.map (fun _ words -> words |> Seq.length)
    |> Map.toList
    |> List.sortBy (fun (_, count) -> -count)

let songRanking comments =
    comments
    |> Seq.map (
        (fun (comment: CommentCsvType.Row) -> comment.Comment)
        >> (fun (comment: string) -> comment.ToLower())
        >> (extractSongs songMap)
    )
    |> flatten
    |> counting

printfn "曲名 \t 投票者数"

for (song, order) in (songRanking comments) do
    printfn $"{song} \t {order}"
