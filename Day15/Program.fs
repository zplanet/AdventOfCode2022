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

let beacons = 
    sensorsWithBeacon
    |> List.map snd
    |> List.fold (fun acc pos -> Set.add pos acc) Set.empty

let beaconsInY targetY =
    beacons
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

let calcNumberOfTakenPositions targetY =
    sensorsWithDistance
    |> List.toArray
    |> Array.Parallel.map (calcPositionsInRange targetY)
    |> Array.reduce (fun a b -> Set.union a b)
    |> (fun s -> Set.difference s (beaconsInY targetY))
    |> Set.count

calcNumberOfTakenPositions 2000000
|> printfn "Part1: %A"
