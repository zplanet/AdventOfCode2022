open System.IO
open System.Text.RegularExpressions

let input =
    Path.Combine(Directory.GetCurrentDirectory(), "input.txt")
    |> File.ReadLines

let parse str =
    Regex.Matches(str, "([0-9]+),([0-9]+)")
    |> Seq.map (fun m -> (int m.Groups[1].Value, int m.Groups[2].Value))
    |> Seq.toList

let pair list =
    let rec loop r l1 l2 =
        if List.isEmpty l2
        then List.rev r
        else loop ((List.head l1, List.head l2)::r) (List.tail l1) (List.tail l2)
    loop [] list (List.tail list)

let calcRock map (fromPos: int * int, (toX: int, toY: int)) =
    let rec loop r = function
        | Some (fromX, fromY) when fromX = toX && fromY < toY -> loop ((fromX, fromY)::r) (Some (fromX, fromY + 1))
        | Some (fromX, fromY) when fromX = toX && fromY > toY -> loop ((fromX, fromY)::r) (Some (fromX, fromY - 1))
        | Some (fromX, fromY) when fromX = toX && fromY = toY -> loop ((fromX, fromY)::r) None
        | Some (fromX, fromY) when fromY = toY && fromX < toX -> loop ((fromX, fromY)::r) (Some (fromX + 1, fromY))
        | Some (fromX, fromY) when fromY = toY && fromX > toX -> loop ((fromX, fromY)::r) (Some (fromX - 1, fromY))
        | Some (fromX, fromY) when fromY = toY && fromX = toX -> loop ((fromX, fromY)::r) None
        | None -> r
    loop [] (Some fromPos)
    |> List.fold
        (fun m pos -> Map.add pos '#' m)
        map

let rocks =
    input
    |> Seq.map (parse >> pair)
    |> Seq.fold
        (fun m list -> List.fold calcRock m list)
        Map.empty

let edges = 
    rocks
    |> Map.toList
    |> List.fold 
        (fun (left, right, top, bottom) ((x, y), _) -> 
            ((if x < left then x else left), (if right < x then x else right), (if y < top then y else top), (if bottom < y then y else bottom))) 
        (System.Int32.MaxValue, 0, System.Int32.MaxValue, 0)

let showMap (left, right, top, bottom) map =
    printfn ""; printfn "==="
    seq {
        for i in top .. bottom do
            yield! seq { for j in left .. right -> Some (j, i) }
            yield None
    }    
    |> Seq.iter
        (function
        | Some pos ->
            match Map.tryFind pos map with
            | Some c -> printf "%c" c
            | None -> printf "."
        | None -> printfn "")
    |> ignore

let drop1 startPos map =
    let isOutOfEdge (x, y) (left, right, _, bottom) = if x < left || right < x || bottom < y then true else false
    let isRockOrSand pos = Map.containsKey pos map
    let rec loop (x, y) =
        let immediate_down = (x, y + 1)
        let left_down = (x - 1, y + 1)
        let right_down = (x + 1, y + 1)

        if isOutOfEdge (x, y) edges
        then (true, (x, y))
        else
            if isRockOrSand immediate_down
            then
                if isRockOrSand left_down
                then
                    if isRockOrSand right_down
                    then (false, (x, y))
                    else loop right_down
                else
                    loop left_down
            else
                loop immediate_down
    loop startPos

let drop2 startPos map =
    let isFull pos = Map.containsKey pos map
    let isRockOrSand (x, y) =
        let (left, right, top, bottom) = edges
        (Map.containsKey (x, y) map) || y = bottom + 2
        
    let rec loop (x, y) =
        let immediate_down = (x, y + 1)
        let left_down = (x - 1, y + 1)
        let right_down = (x + 1, y + 1)

        if isFull (x, y)
        then (true, (x, y))
        else
            if isRockOrSand immediate_down
            then
                if isRockOrSand left_down
                then
                    if isRockOrSand right_down
                    then (false, (x, y))
                    else loop right_down
                else
                    loop left_down
            else
                loop immediate_down
    loop startPos

let simulateDrop dropFunc map =
    let rec loop cnt m = 
        // showMap edges m
        match dropFunc (500, 0) m with
        | isDone, pos when isDone -> cnt
        | isDone, pos when not isDone -> loop (cnt + 1) (Map.add pos 'o' m)
        | _ -> cnt
    loop 0 map

rocks
|> simulateDrop drop1
|> printfn "Part1: %A"

rocks
|> simulateDrop drop2
|> printfn "Part2: %A"
