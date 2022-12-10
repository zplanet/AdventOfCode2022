open System.IO

let input =
    Path.Combine(Directory.GetCurrentDirectory(), "input.txt")
    |> File.ReadLines
    |> Seq.toList

let rowLength = input |> List.length
let colLength = input |> List.head |> Seq.length

let trees = 
    input
    |> List.indexed
    |> List.map 
        (fun (row, line: string) -> 
            line 
            |> Seq.toList 
            |> List.indexed 
            |> List.map (fun (col, c) -> (row + 1, col + 1, (int (string c)))))
    |> List.collect id

let getHeight coord = (Map.find coord) >> fst

let calcNumberOfVisibleTreesFromOutside list =
    let left2Right =
        [
            for row in 1 .. rowLength ->
                [for col in 1 .. colLength -> (row, col)]
        ]

    let right2Left =
        [
            for row in 1 .. rowLength ->
                [for col in colLength .. -1 .. 1 -> (row, col)]
        ]

    let top2Bottom =
        [
            for col in 1 .. colLength ->
                [for row in 1 .. rowLength -> (row, col)]
        ]

    let bottom2Top =
        [
            for col in 1 .. colLength ->
                [for row in rowLength .. -1 .. 1 -> (row, col)]
        ]

    let updateVisibility direction map =
        let setVisible coord isVisible =
            Map.change 
                coord 
                (function | Some (height, b) -> Some (height, isVisible) | None -> None)

        let getVisible coord = (Map.find coord) >> snd

        direction
        |> List.fold
            (fun m rows -> 
                rows
                |> List.fold
                    (fun (max, m') coord ->
                        if max = 9
                        then (max, m')
                        else
                            let height = getHeight coord m'
                            if (getVisible coord m')
                            then ((if max < height then height else max), m')
                            else
                                let (r, c) = coord
                                if r = 1 || c = 1 || r = rowLength || c = colLength
                                then ((if max < height then height else max), setVisible coord true m')
                                else
                                    if max < height
                                    then (height, setVisible coord true m')
                                    else
                                        (max, m'))
                    (0, m)
                |> snd)
            map

    list
    |> List.fold 
        (fun acc (r, c, v) -> Map.add (r, c) (v, false) acc) 
        Map.empty
    |> updateVisibility left2Right
    |> updateVisibility right2Left
    |> updateVisibility top2Bottom
    |> updateVisibility bottom2Top
    |> Map.filter (fun coord (v, isVisible) -> isVisible)
    |> Map.count

let findBestScenicScore list =
    let setScenicScore coord score =
        Map.change 
            coord 
            (function | Some (height, scores) -> Some (height, score * scores) | None -> None)

    let allCoords =
        [ for row in 2 .. (rowLength - 1) do for col in 2 .. (colLength - 1) -> (row, col)]

    let treeMap = 
        list
        |> List.fold 
            (fun acc (r, c, v) -> Map.add (r, c) (v, 1) acc) 
            Map.empty

    let calcScenicScore coord map =
        let height = getHeight coord map
        let (row, col) = coord
        let toRight = [for c in (col + 1) .. colLength -> (row, c)]
        let toLeft = [for c in (col - 1) .. -1 .. 1 -> (row, c)]
        let toTop = [for r in (row - 1) .. -1 .. 1 -> (r, col)]
        let toBottom = [for r in (row + 1).. rowLength -> (r, col)]

        let rec traverse score m ls =
            match ls with
            | (r,c)::ts ->
                let height' = getHeight (r,c) m
                if height' < height
                then traverse (score + 1) m ts
                else setScenicScore coord (score + 1) m
            | [] -> setScenicScore coord score m

        [toRight; toLeft; toTop; toBottom]
        |> List.fold
            (fun m direction ->
                traverse 0 m direction)
            map
    
    allCoords
    |> List.fold
        (fun m coord -> calcScenicScore coord m)
        treeMap
    |> Map.toList
    |> List.sortByDescending (fun (_, (_, score)) -> score)
    |> List.head
    |> (snd >> snd)

trees
|> calcNumberOfVisibleTreesFromOutside
|> printfn "Part1: %A"

trees
|> findBestScenicScore
|> printfn "Part2: %A"
