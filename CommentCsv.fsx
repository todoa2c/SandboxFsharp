#r "nuget: FSharp.Data, 4.2.2"
open FSharp.Data

type CommentCsvType = CsvProvider<Schema="Author (string), LikeCount (int), Comment (string)", HasHeaders=false>
