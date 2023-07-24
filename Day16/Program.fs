open System.IO
open System.Text.RegularExpressions
open System.Collections.Generic

let input =
    Path.Combine(Directory.GetCurrentDirectory(), "input.test.txt")
    |> File.ReadLines

let parse str =
    let m = Regex.Match(str, "Valve (?<valveName>[A-Z]+) has flow rate=(?<rate>[0-9]+); tunnel[s]? lead[s]? to valve[s]? (?<valves>[A-Z]+(, )?)+")
    if m.Success
    then Some (m.Groups["valveName"].Value, int m.Groups["rate"].Value, m.Groups["valves"].Captures |> Seq.toList |> List.map (fun c -> c.Value.Trim(' ', ',')))
    else None

let valveLayout =
    input
    |> Seq.map parse
    |> Seq.choose id
    |> Seq.fold 
        (fun acc (valveName, rate, tunnels) ->
            Map.add valveName (rate, tunnels) acc) 
        Map.empty

// let findShortestPath (start: int * int) target (nextElevation: char -> char) (comparer: int -> int -> bool) map =
//     let toNumber = function
//         | 'S' -> 96
//         | 'E' -> 123
//         | ch -> int ch
//     let calcNexts (r, c) =
//         [(0, -1); (0, 1); (-1, 0); (1, 0)]
//         |> List.map (fun (r', c') -> (r + r', c + c'))
//         |> List.filter (fun (r', c') -> -1 < r' && r' < rowLen && -1 < c' && c' < colLen)

//     let rec loop (queue: PriorityQueue<Path, int>) visited =
//         let currPath = if (Set.count visited) = 0 then { Elevation = (Map.find start map); Pos = start; Cost = 0; } else queue.Dequeue()

//         if currPath.Elevation = target
//         then currPath.Cost
//         else
//             let nextElevation' = nextElevation currPath.Elevation
//             let nexts =
//                 currPath.Pos
//                     |> calcNexts
//                     |> List.filter (fun coord' -> not (Set.contains coord' visited))
//                     |> List.map (fun coord' -> (coord', Map.find coord' map))
//                     |> List.filter (fun (_, ch) -> comparer (toNumber ch) (toNumber nextElevation'))

//             let visited' = nexts |> List.fold (fun acc (coord', _) -> Set.add coord' acc) visited

//             nexts
//                 |> List.map (fun (coord', ch) -> { Elevation = ch; Pos = coord'; Cost = currPath.Cost + 1 })
//                 |> List.iter (fun path -> queue.Enqueue(path, path.Cost))

//             loop queue visited'
//     loop (PriorityQueue()) Set.empty

// let calcPressure minLimit =
//     let rec loop minute currPressure currValve=
//         let operation = minute % 2
//         if operation = 0
//         then
//         else
//         // let pressure = currPressure + (getRate currValve)

//         // getConnections currValve
//         // |> List.map (loop (minute + 1) pressure)
//         // |> List.max
//     loop 0 0 "AA"

// pipeScan
// |> calcPressure 30


// let invertedValveLayout = 
//     valveLayout
//     |> Map.map (fun k (rate, connections) -> (rate * -1, connections))

// let initQueue =
//     let q = PriorityQueue()
//     q.Enqueue (["AA"], 0)
//     q

// let findMaxPath count map =
//     let getValve valveName = 
//         printfn "%A" valveName
//         map |> Map.find valveName
//     let getTunnels = getValve >> snd
//     let getRate = getValve >> fst

//     let rec loop cnt (queue: PriorityQueue<string list, int>) visits =
//         if cnt < count
//         then
//             let list =
//                 match queue.TryDequeue() with
//                 | true, path, priority ->
//                     path
//                     |> List.head
//                     |> getTunnels
//                     |> List.filter (fun valveName -> visits |> Set.contains valveName |> not)
//                     |> List.map
//                         (fun valveName -> (valveName::path, getRate valveName + priority))
//                 | _ -> []

//             list
//             |> List.iter
//                 (fun (path, priority) -> queue.Enqueue(path, priority))
            
//             loop (cnt + 1) queue visits
//         else
//             queue.Dequeue() |> List.rev
//     loop 0 initQueue Set.empty

// invertedValveLayout
// |> findMaxPath 10
// |> printfn "%A"



// let findMaxPath count startValve map =
//     let getValve valveName = Map.find valveName
//     let getTunnels valveName = (getValve valveName) >> (fun (_, _, tunnels) -> tunnels)
//     let getRate valveName = (getValve valveName) >> (fun (rate, isOpen, _) -> if isOpen then 0 else rate)
//     let isOpen valveName = (getValve valveName) >> (fun (_, isOpen, _) -> isOpen)
//     let setOpen valveName isOpen = Map.change valveName (function | Some (rate, _, tunnels) -> Some (rate, isOpen, tunnels) | None -> None)
//     let calcPressure m = m |> Map.filter (fun _ (_, isOpen, _) -> isOpen) |> Map.fold (fun acc _ (rate, _, _) -> acc + rate) 0

//     let rec loop cnt r m opens valveName =
//         let r' = opens |> Set.fold (fun acc (_, rate) -> acc + rate) 0
//         let opens' = 
//             if cnt % 2 = 1 
//             then opens 
//             else
//                 let rate = getRate valveName m
//                 if rate = 0 then opens else  (Set.add (valveName, rate) opens)

//         printfn "%A" (cnt, valveName)
//         if count <= cnt
//         then r + r'
//         else
//             m |> getTunnels valveName |> List.map (loop (cnt + 1) (r + r') m opens') |> List.max
//     loop 1 0 map Set.empty startValve

let findMaxPath (total: int) startValve map =
    let getValve valveName = Map.find valveName map
    let getTunnels = getValve >> snd
    let getRate = getValve >> fst

    let rec loop (minutes, valve) opens sum =
        if minutes = 0
        then sum
        else
            let nextValve = valve |> getTunnels |> List.maxBy (fun name -> getRate name)
            if (getRate valve) = 0
            then loop (minutes - 1, nextValve) opens sum
            else
                let tunnels = getTunnels valve

    loop (total, startValve) Set.empty 0

valveLayout
|> findMaxPath 30 "AA"
|> printfn "%A"