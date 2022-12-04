open System.IO

let input = 
    Path.Combine(Directory.GetCurrentDirectory(), "input.txt")
    |> File.ReadLines

let toPair (line: string) =
    let ps = line.Split(",")
    (Array.head ps, (Array.tail >> Array.head) ps)

let toRange (s: string) =
    let ns = s.Split("-")
    (int (Array.head ns), int ((Array.tail >> Array.head) ns))

let doesOneRangeFullyContainTheOther =
    function
    | ((r1Min, r1Max), (r2Min, r2Max)) when r1Min <= r2Min && r2Max <= r1Max -> true
    | ((r1Min, r1Max), (r2Min, r2Max)) when r2Min <= r1Min && r1Max <= r2Max -> true
    | _ -> false

let doTheRangesOverlap = 
    function
    | ((r1Min, r1Max), (r2Min, r2Max)) when r1Max < r2Min -> false
    | ((r1Min, r1Max), (r2Min, r2Max)) when r2Max < r1Min -> false
    | _ -> true

input
|> Seq.map toPair
|> Seq.map (fun (s1, s2) -> (toRange s1, toRange s2))
|> Seq.map doesOneRangeFullyContainTheOther
|> Seq.countBy id
|> Seq.filter fst
|> Seq.map snd
|> Seq.head
|> printfn "Part 1: %A"

input
|> Seq.map toPair
|> Seq.map (fun (s1, s2) -> (toRange s1, toRange s2))
|> Seq.map doTheRangesOverlap
|> Seq.countBy id
|> Seq.filter fst
|> Seq.map snd
|> Seq.head
|> printfn "Part 2: %A"