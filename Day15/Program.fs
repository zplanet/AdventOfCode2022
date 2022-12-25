open System.IO
open System.Text.RegularExpressions

let input =
    Path.Combine(Directory.GetCurrentDirectory(), "input.txt")
    |> File.ReadLines

let calcDistance (x1, y1) (x2, y2) = abs(x1 - x2) + abs (y1 - y2)

let sensorsWithBeacon = 
    let parseLine line =
        let ms = Regex.Matches(line, "at x=(-?[0-9]+), y=(-?[0-9]+)")
        if ms.Count = 2
        then Some ((int ms[0].Groups[1].Value, int ms[0].Groups[2].Value), (int ms[1].Groups[1].Value, int ms[1].Groups[2].Value))
        else
            printfn "wrong %A" line
            None

    input
    |> Seq.map parseLine
    |> Seq.choose id
    |> Seq.fold
        (fun acc (sensor, beacon) -> (sensor, beacon)::acc)
        []

let sensorsWithDistance = 
    sensorsWithBeacon
    |> List.map (fun (sensor, beacon) -> (sensor, calcDistance sensor beacon))

let beaconsInY targetY =
    sensorsWithBeacon
    |> List.map snd
    |> List.fold (fun acc pos -> Set.add pos acc) Set.empty
    |> Set.filter (fun (_, y) -> y = targetY)

let calcPositionsInRange targetY sensor =
    let isInRange (sensorPos, distance) targetPos = (calcDistance sensorPos targetPos) <= distance

    let targetPos = ((fst (fst sensor)), targetY)

    let rec loop inc pos r =
        if isInRange sensor pos
        then loop inc ((fst >> inc) pos, snd pos) (Set.add pos r)
        else r

    if isInRange sensor targetPos
    then
        Set.empty
        |> Set.add targetPos
        |> (loop (fun x -> x - 1) targetPos)
        |> (loop (fun x -> x + 1) targetPos)
    else Set.empty

let calcNumberOfTakenPositionsAt targetY =
    sensorsWithDistance
    |> List.map (calcPositionsInRange targetY)
    |> List.reduce (fun a b -> Set.union a b)
    |> (fun s -> Set.difference s (beaconsInY targetY))
    |> Set.count

// calcNumberOfTakenPositionsAt 2000000
// |> printfn "Part1: %A"

// let calcTuningFrequencyForDistressBeacon max =
//     let sensorsAndBeacons =
//         sensorsWithBeacon
//         |> List.fold
//             (fun acc (sensor, beacon) -> 
//                 acc |> Map.add sensor 'S' |> Map.add beacon 'B')
//             Map.empty
    
//     let notSensorOrBeacon pos = not (Map.containsKey pos sensorsAndBeacons)

//     let calcBoundary ((x, y), distance) = 
//         let rec loop compare increase ((lx, rx), y') r =
//             if compare y y' 
//             then loop compare increase ((lx - 1, rx + 1), increase y') ((lx, y')::(rx, y')::r)
//             else r
//         [(x, y + distance + 1); (x, y - distance - 1)]
//         |> loop (fun a b -> a <= b) (fun y' -> y' - 1) ((x - 1, x + 1), y + distance)
//         |> loop (fun a b -> a > b) (fun y' -> y' + 1) ((x - 1, x + 1), y - distance)
//         |> List.filter (fun (x, y) -> 0 <= x && x <= max && 0 <= y && y <= max && (notSensorOrBeacon (x, y)))

//     sensorsWithDistance
//     |> List.map calcBoundary
//     |> List.fold 
//         (fun acc positions -> 
//             positions
//             |> List.fold
//                 (fun m pos -> 
//                     Map.change
//                         pos
//                         (function
//                         | Some n -> Some (n + 1)
//                         | None -> Some 1)
//                         m)
//                 acc)
//         Map.empty
//     |> Map.toList
//     |> List.sortByDescending (fun (_, n) -> n)
//     |> List.head
//     |> (fun ((x, y), _) -> (uint x) * 4000000u + (uint y))

let calcTuningFrequencyForDistressBeacon max =
    let isCovered ((tx, ty), (bx, by)) (sensor, distance) =
        if tx = bx && bx = by
        then [(tx,ty)]
        else [(tx, ty); (bx, ty); (tx, by); (bx, by)]
        |> List.forall (fun pos -> calcDistance sensor pos <= distance)
        
    let divideArea ((tx, ty), (bx, by)) =
        if tx = bx && ty = by
        then []
        else
            let mx = (tx + bx) / 2
            let my = (ty + by) / 2
            if bx - tx > 2
            then [((tx, ty), (mx, my)); ((mx + 1, ty), (bx, my)); ((tx, my + 1), (mx, by)); ((mx + 1, my + 1), (bx, by));]
            else [ for i in tx .. bx do for j in ty .. by -> ((i, j), (i, j)) ] 

    let rec loop ls =
        let areas = 
            ls 
            |> List.map divideArea 
            |> List.collect id
            |> List.filter (fun area -> sensorsWithDistance |> List.exists (isCovered area) |> not)

        if List.isEmpty areas
        then List.head ls
        else loop areas

    [((0, 0), (max, max))]
    |> loop
    |> (fun ((x, y), _) -> (uint x) * 4000000u + (uint y))    

calcTuningFrequencyForDistressBeacon 4000000
// calcTuningFrequencyForDistressBeacon 20
|> printfn "Part2: %A"
