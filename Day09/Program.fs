open System.IO
open System.Text.RegularExpressions

let input =
    Path.Combine(Directory.GetCurrentDirectory(), "input.txt")
    |> File.ReadLines

type Direction = Left | Right | Up | Down
let initialHeadPos = (0, 0)
let initialTailPos = (0, 0)

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

let simulate moves =
    let isAdjacent (hx, hy) (tx, ty) = (abs (tx - hx)) < 2 && (abs (ty - hy)) < 2
    let moveLeft (x, y) = (x - 1, y)
    let moveRight (x, y) = (x + 1, y)
    let moveUp (x, y) = (x, y + 1)
    let moveDown (x, y) = (x, y - 1)
    let getAdjacentPos (x, y) = function
        | Left -> (x + 1, y)
        | Right -> (x - 1, y)
        | Up -> (x, y - 1)
        | Down -> (x, y + 1)

    let rec move n direction currHeadPos currTailPos m =
        if n = 0
        then (currHeadPos, currTailPos, m)
        else
            match direction with
            | Left -> 
                let headPos = moveLeft currHeadPos
                let tailPos = if (isAdjacent headPos currTailPos) then currTailPos else getAdjacentPos headPos Left
                move (n - 1) direction headPos tailPos (updatePos tailPos m)
            | Right ->
                let headPos = moveRight currHeadPos
                let tailPos = if (isAdjacent headPos currTailPos) then currTailPos else getAdjacentPos headPos Right
                move (n - 1) direction headPos tailPos (updatePos tailPos m)
            | Up ->
                let headPos = moveUp currHeadPos
                let tailPos = if (isAdjacent headPos currTailPos) then currTailPos else getAdjacentPos headPos Up
                move (n - 1) direction headPos tailPos (updatePos tailPos m)
            | Down ->
                let headPos = moveDown currHeadPos
                let tailPos = if (isAdjacent headPos currTailPos) then currTailPos else getAdjacentPos headPos Down
                move (n - 1) direction headPos tailPos (updatePos tailPos m)

    moves
    |> Seq.fold
        (fun (currHeadPos, currTailPos, m) line ->
            match line with
            | LEFT n -> move n Left currHeadPos currTailPos m
            | RIGHT n -> move n Right currHeadPos currTailPos m
            | UP n -> move n Up currHeadPos currTailPos m
            | DOWN n -> move n Down currHeadPos currTailPos m)
        (initialHeadPos, initialTailPos, (updatePos initialTailPos Map.empty))
    |> (fun (_, _, m) -> m)

input
|> simulate
|> Map.toList
|> List.length
|> printfn "Part1: %A"
