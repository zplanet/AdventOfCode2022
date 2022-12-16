open System.IO
open System.Collections.Generic

let input =
    Path.Combine(Directory.GetCurrentDirectory(), "input.txt")
    |> File.ReadLines

let rowLen = input |> Seq.length
let colLen = input |> Seq.head |> Seq.length

type Path = { Elevation: char; Pos: int * int; Cost: int; }

let (startPos, endPos, map) =
    input
    |> Seq.indexed
    |> Seq.fold 
        (fun (s, e, m) (row, line) -> 
            line
            |> Seq.indexed
            |> Seq.fold
                (fun (s', e', m') (col, ch) -> 
                    let s''  = if ch = 'S' then (row, col) else s'
                    let e'' = if ch = 'E' then (row, col) else e'
                    (s'', e'', Map.add (row, col) ch m'))
                (s, e, m))
        ((0, 0), (0, 0), Map.empty)

let findShortestPath (start: int * int) target (nextElevation: char -> char) (comparer: int -> int -> bool) map =
    let toNumber = function
        | 'S' -> 96
        | 'E' -> 123
        | ch -> int ch
    let calcNexts (r, c) = 
        [(0, -1); (0, 1); (-1, 0); (1, 0)] 
        |> List.map (fun (r', c') -> (r + r', c + c')) 
        |> List.filter (fun (r', c') -> -1 < r' && r' < rowLen && -1 < c' && c' < colLen)
    
    let rec loop (queue: PriorityQueue<Path, int>) visited =
        let currPath = if (Set.count visited) = 0 then { Elevation = (Map.find start map); Pos = start; Cost = 0; } else queue.Dequeue()

        if currPath.Elevation = target
        then currPath.Cost
        else 
            let nextElevation' = nextElevation currPath.Elevation
            let nexts =
                currPath.Pos
                    |> calcNexts
                    |> List.filter (fun coord' -> not (Set.contains coord' visited))
                    |> List.map (fun coord' -> (coord', Map.find coord' map))
                    |> List.filter (fun (_, ch) -> comparer (toNumber ch) (toNumber nextElevation'))

            let visited' = nexts |> List.fold (fun acc (coord', _) -> Set.add coord' acc) visited

            nexts
                |> List.map (fun (coord', ch) -> { Elevation = ch; Pos = coord'; Cost = currPath.Cost + 1 })
                |> List.iter (fun path -> queue.Enqueue(path, path.Cost))
            
            loop queue visited'
    loop (PriorityQueue()) Set.empty

map
|> findShortestPath
    startPos
    'E'
    (function
        | 'S' -> 'a'
        | 'z' -> 'E'
        | ch -> char ((int ch) + 1))
    (fun a b -> a <= b)
|> printfn "Part1: %A"

map
|> findShortestPath
    endPos 
    'a' 
    (function
        | 'E' -> 'z'
        | ch -> char ((int ch) - 1))
    (fun a b -> a >= b)
|> printfn "Part2: %A"
