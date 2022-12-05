open System.IO

type Procedure = { Count: int; From: int; To: int }

let input = 
    Path.Combine(Directory.GetCurrentDirectory(), "input.txt")
    |> File.ReadLines

let crates =
    let indexes = [1..4..33] |> List.indexed

    let toMap m (id, c) =
        if c = ' '
        then m
        else
            Map.change 
                id 
                (function
                | Some stck -> Some (c::stck)
                | None -> Some (c::[])) 
                m

    input
    |> Seq.take 8
    |> Seq.toList
    |> List.map (fun line -> List.map (fun (id, idx) -> (id + 1, line[idx])) indexes)
    |> List.rev
    |> List.collect id
    |> List.fold toMap Map.empty

let procedures =
    input
    |> Seq.skip 10
    |> Seq.map (fun line -> line.Split(' '))
    |> Seq.map (fun arr -> { Count = (int arr.[1]); From = (int arr.[3]); To = (int arr.[5])})
    

let pop id map =
    let list = Map.find id map
    let updated = 
        Map.change 
            id
            (function 
                | Some (h::ts) -> Some ts
                | Some [] -> Some []
                | None -> None)
            map
    (List.head list, updated)

let push v id map =
    Map.change
        id
        (function
            | Some list -> Some (v::list)
            | None -> Some (v::[]))
        map

let folder cs (p: Procedure) =
    [1..p.Count]
    |> List.fold 
        (fun acc _ -> 
            let (pv, pmap) = pop p.From acc
            push pv p.To pmap)
        cs

let pop2 cnt id map =
    let list = Map.find id map
    let updated = 
        Map.change 
            id
            (function 
                | Some list -> Some (List.skip cnt list)
                | None -> None)
            map
    (List.take cnt list, updated)

let push2 vs id map =
    Map.change
        id
        (function
            | Some list -> Some (vs @ list)
            | None -> Some vs)
        map

let folder2 cs (p: Procedure) =
    let (pvs, pmap) = pop2 p.Count p.From cs
    push2 pvs p.To pmap

let run fldr = 
    procedures
    |> Seq.fold fldr crates
    |> Map.toList
    |> List.map (fun (_, list) -> string (List.head list))
    |> List.fold (fun acc s -> acc + s) ""

run folder
|> printfn "Part 1: %A"

run folder2
|> printfn "Part 2: %A"
