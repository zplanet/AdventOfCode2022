open System.IO

let input = 
    Path.Combine(Directory.GetCurrentDirectory(), "input.txt")
    |> File.ReadLines
    |> Seq.head

let isUnique list = list |> Set.ofList |> Seq.length |> (fun l -> l = list.Length)

let find size list =
    let rec loop idx size list = 
        if (List.length list) < size
        then
            -1
        else
            if isUnique (List.take size list)
            then idx
            else loop (idx + 1) size (List.tail list)
    loop size size list

input
|> Seq.toList
|> find 4
|> printfn "Part 1: %A"

input
|> Seq.toList
|> find 14
|> printfn "Part 2: %A"
