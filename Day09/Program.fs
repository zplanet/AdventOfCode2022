open System.IO
open System.Text.RegularExpressions

let input =
    Path.Combine(Directory.GetCurrentDirectory(), "input.txt")
    |> File.ReadLines

type Direction = Left | Right | Up | Down
let initialPos = (0, 0)

let (|LEFT|_|) str =
   let m = Regex.Match(str, "L ([0-9]+)")
   if m.Success then Some (int m.Groups[1].Value) else None

let (|RIGHT|_|) str =
   let m = Regex.Match(str, "R ([0-9]+)")
   if m.Success then Some (int m.Groups[1].Value) else None

let (|UP|_|) str =
   let m = Regex.Match(str, "U ([0-9]+)")
   if m.Success then Some (int m.Groups[1].Value) else None

let (|DOWN|_|) str =
   let m = Regex.Match(str, "D ([0-9]+)")
   if m.Success then Some (int m.Groups[1].Value) else None

let updatePos pos map = Map.change pos (function | Some v -> Some (v + 1) | None -> Some 1) map

let simulate positions moves =
    let isAdjacent (hx, hy) (tx, ty) = (abs (tx - hx)) < 2 && (abs (ty - hy)) < 2
    let step (x, y) = function
        | Left -> (x - 1, y)
        | Right -> (x + 1, y)
        | Up -> (x, y + 1)
        | Down -> (x, y - 1)
    let getAdjacentPos = function
        | (x1, y1), (x2, y2) when x1 = x2 && y1 = y2 -> (x2, y2)

        | (x1, y1), (x2, y2) when x1 = x2 && y1 = (y2 + 1) -> (x2, y2)
        | (x1, y1), (x2, y2) when x1 = x2 && y1 = (y2 + 2) -> (x2, y1 - 1)
        | (x1, y1), (x2, y2) when x1 = x2 && y1 = (y2 - 1) -> (x2, y2)
        | (x1, y1), (x2, y2) when x1 = x2 && y1 = (y2 - 2) -> (x2, y1 + 1)

        | (x1, y1), (x2, y2) when y1 = y2 && x1 = (x2 + 1) -> (x2, y2)
        | (x1, y1), (x2, y2) when y1 = y2 && x1 = (x2 + 2) -> (x1 - 1, y2)
        | (x1, y1), (x2, y2) when y1 = y2 && x1 = (x2 - 1) -> (x2, y2)
        | (x1, y1), (x2, y2) when y1 = y2 && x1 = (x2 - 2) -> (x1 + 1, y2)        
        
        | (x1, y1), (x2, y2) when x1 = (x2 + 1) && y1 = (y2 + 1) -> (x2, y2)
        | (x1, y1), (x2, y2) when x1 = (x2 + 1) && y1 = (y2 - 1) -> (x2, y2)
        | (x1, y1), (x2, y2) when x1 = (x2 - 1) && y1 = (y2 + 1) -> (x2, y2)
        | (x1, y1), (x2, y2) when x1 = (x2 - 1) && y1 = (y2 - 1) -> (x2, y2)

        | (x1, y1), (x2, y2) when x1 = (x2 + 1) && y1 = (y2 + 2) -> (x1, y1 - 1)
        | (x1, y1), (x2, y2) when x1 = (x2 + 1) && y1 = (y2 - 2) -> (x1, y1 + 1)
        | (x1, y1), (x2, y2) when x1 = (x2 - 1) && y1 = (y2 + 2) -> (x1, y1 - 1)
        | (x1, y1), (x2, y2) when x1 = (x2 - 1) && y1 = (y2 - 2) -> (x1, y1 + 1)

        | (x1, y1), (x2, y2) when x1 = (x2 + 2) && y1 = (y2 + 1) -> (x1 - 1, y1)
        | (x1, y1), (x2, y2) when x1 = (x2 + 2) && y1 = (y2 - 1) -> (x1 - 1, y1)
        | (x1, y1), (x2, y2) when x1 = (x2 - 2) && y1 = (y2 + 1) -> (x1 + 1, y1)
        | (x1, y1), (x2, y2) when x1 = (x2 - 2) && y1 = (y2 - 1) -> (x1 + 1, y1)

        | (x1, y1), (x2, y2) when x1 = (x2 + 2) && y1 = (y2 + 2) -> (x2 + 1, y2 + 1)
        | (x1, y1), (x2, y2) when x1 = (x2 + 2) && y1 = (y2 - 2) -> (x2 + 1, y2 - 1)
        | (x1, y1), (x2, y2) when x1 = (x2 - 2) && y1 = (y2 + 2) -> (x2 - 1, y2 + 1)
        | (x1, y1), (x2, y2) when x1 = (x2 - 2) && y1 = (y2 - 2) -> (x2 - 1, y2 - 1)
    
    let rec apply r ps direction = 
        if List.isEmpty r
        then
            apply ((step (List.head ps) direction)::r) (List.tail ps) direction
        else
            if List.isEmpty ps
            then r
            else 
                let pos1 = List.head r
                let pos2 = List.head ps
                apply ((getAdjacentPos (pos1, pos2))::r) (List.tail ps) direction

    let rec move n direction positions m =
        if n = 0
        then (positions, m)
        else
            let npositions = 
                match direction with
                | Left -> apply [] positions Left
                | Right -> apply [] positions Right
                | Up -> apply [] positions Up
                | Down -> apply [] positions Down
            move (n - 1) direction (List.rev npositions) (updatePos (List.head npositions) m)

    moves
    |> Seq.fold
        (fun (positions, m) line ->
            match line with
            | LEFT n -> move n Left positions m
            | RIGHT n -> move n Right positions m
            | UP n -> move n Up positions m
            | DOWN n -> move n Down positions m)
        (positions, (updatePos initialPos Map.empty))
    |> snd

input
|> simulate [ for i in 1 .. 2 -> initialPos]
|> Map.toList
|> List.length
|> printfn "Part1: %A"

input
|> simulate [ for i in 1 .. 10 -> initialPos]
|> Map.toList
|> List.length
|> printfn "Part2: %A"
