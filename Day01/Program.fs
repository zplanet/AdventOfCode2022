open System.IO

let input = 
    Path.Combine(Directory.GetCurrentDirectory(), "input.txt")
    |> File.ReadLines
    |> Seq.map (fun s -> if s.Length > 0 then Some((int)s) else None)

let calcCalories data =
    data
    |> Seq.fold 
        (fun (h::ts) v -> 
            match v with 
            | Some n -> (h + n)::ts 
            | None -> (0::h::ts)) 
        [0]

// part 1
input
|> calcCalories
|> List.max
|> printfn "part 1: %A"

// part 2
input
|> calcCalories
|> List.sortDescending
|> List.take 3
|> List.sum
|> printfn "part 2: %A"