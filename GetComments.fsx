#r "nuget: FSharp.Data, 4.2.2"
#load "CommentCsv.fsx"
#load "SampleResponse.fsx"

open FSharp.Data
open CommentCsv
open SampleResponse

let url =
    "https://www.googleapis.com/youtube/v3/commentThreads"

let apiKey =
    (System.Environment.GetEnvironmentVariable "YOUTUBE_API_KEY")

let videoID = "utbzRfPnHgM"

let providerURL =
    $"{url}?key={apiKey}&part=snippet&videoId={videoID}&order=relevance&textFormat=plaintext&maxResults=100"

type Comments = JsonProvider<SampleResponseJSON>

let rec getAllComments nextToken =
    let nextTokenParam =
        match nextToken with
        | Some (token) -> $"&pageToken={token}"
        | None -> ""
    // printfn "nextTokenParam: %s" nextTokenParam
    let comments =
        Comments.Load(providerURL + nextTokenParam)
    // printfn "next: '%s'" comments.NextPageToken
    // printfn "%A" comments
    if comments.NextPageToken = "" then
        Array.toList comments.Items
    else
        (Array.toList comments.Items)
        @ (getAllComments (Some comments.NextPageToken))

let comments = getAllComments None
// for comment in comments do
//     printfn "%s\t%d\t%s" comment.Snippet.TopLevelComment.Snippet.AuthorDisplayName comment.Snippet.TopLevelComment.Snippet.LikeCount (comment.Snippet.TopLevelComment.Snippet.TextDisplay.Replace('\n', ' '))

let csvComments =
    comments
    |> List.map
        (fun c ->
            CommentCsvType.Row(
                c.Snippet.TopLevelComment.Snippet.AuthorDisplayName,
                c.Snippet.TopLevelComment.Snippet.LikeCount,
                (c.Snippet.TopLevelComment.Snippet.TextDisplay.Replace('\n', ' '))
            ))
    // |> CommentCsvType  // こちらのほうがスッキリしているがWarningが出る
    |> (fun rows -> new CommentCsvType(rows))

printfn "%s" (csvComments.SaveToString())
