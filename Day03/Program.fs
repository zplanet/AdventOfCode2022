open System.IO

let input = 
    Path.Combine(Directory.GetCurrentDirectory(), "input.txt")
    |> File.ReadLines

let toRucksack (s: string) =
    let len = s.Length / 2
    (s.Substring(0, len), s.Substring(len))

let increaseMap m c = 
    Map.change 
        c 
        (function 
            | Some n -> Some (n + 1) 
            | None -> Some 1)
        m

let findSharedItemType (firstCompartment, secondCompartment) =
    let map = 
        firstCompartment
        |> Seq.fold increaseMap Map.empty

    secondCompartment
    |> Seq.tryFind (fun c -> Map.containsKey c map)

let calcPriority (c: char)=
    let n = int c
    if n < 91
    then n - (int 'A') + 27
    else n - (int 'a') + 1

let toPriority =
    function
    | Some c -> calcPriority c
    | None -> 0

let initMap m c = 
    Map.change 
        c 
        (function 
            | Some n -> Some n
            | None -> Some 1)
        m

let updateMap s k v =
    Map.change
        k
        (function
        | Some n -> Some (n + v)
        | None -> Some v)
        s

let findSharedItemTypeInGroup (first::second::third::[]) =
    let m1 = Seq.fold initMap Map.empty first
    let m2 = Seq.fold initMap Map.empty second
    let m3 = Seq.fold initMap Map.empty third

    Map.fold 
        updateMap 
        m3 
        (Map.fold updateMap m2 m1)

input
|> Seq.map toRucksack
|> Seq.map findSharedItemType
|> Seq.map toPriority
|> Seq.sum
|> printfn "Part 1: %A"

input
|> Seq.indexed
|> Seq.groupBy (fun (idx, line) -> idx / 3)
|> Seq.map (snd >> (Seq.map snd) >> Seq.toList)
|> Seq.map findSharedItemTypeInGroup
|> Seq.map (fun grp -> Map.filter (fun k v -> v = 3) grp)
|> Seq.map (Map.toList >> List.head >> fst)
|> Seq.map calcPriority
|> Seq.sum
|> printfn "Part 2: %A"

