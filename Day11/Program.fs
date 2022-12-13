open System.IO
open System.Text.RegularExpressions

let input =
    Path.Combine(Directory.GetCurrentDirectory(), "input.txt")
    |> File.ReadAllText

type Monkey = {
    Id: int;
    Items: uint64 list;
    CalcWorryLevel: uint64 -> uint64;
    Divisor: uint64;
    Test: uint64 -> int;
    InspectionCount: uint64;
}

let monkeys =
    let pattern = "Monkey ([0-9]+):[^S]+Starting items: ([0-9 ,]+)[^O]+Operation: new = old ([*+]) (old|[0-9]+)[^T]+Test: divisible by ([0-9]+)[^I]+If true: throw to monkey ([0-9]+)[^I]+If false: throw to monkey ([0-9]+)"
    let matches = Regex.Matches (input, pattern, RegexOptions.Multiline)

    let makeTest divisor trueVal falseVal = 
        (fun (dividend: uint64) -> if dividend % divisor = 0UL then trueVal else falseVal)

    let makeCalcWorryLevel f fb = (fun a -> f a (fb a))

    let createMonkey (m: Match) =
        let monkeyId = int m.Groups[1].Value
        let items = m.Groups[2].Value.Split(", ") |> Array.map uint64 |> Array.toList
        let calcWorryLevel =
            match m.Groups[3].Value, m.Groups[4].Value with
            | operator, operand when operator = "+" && operand = "old" -> makeCalcWorryLevel (+) id
            | operator, operand when operator = "*" && operand = "old" -> makeCalcWorryLevel (*) id
            | operator, operand when operator = "+" -> makeCalcWorryLevel (+) (fun _ -> uint64 operand)
            | _, operand -> makeCalcWorryLevel (*) (fun _ -> uint64 operand)
        let divisor = uint64 m.Groups[5].Value
        let monkeyIdTrue = int m.Groups[6].Value
        let monkeyIdFalse = int m.Groups[7].Value
        { Id = monkeyId; Items = items; CalcWorryLevel = calcWorryLevel; Divisor = divisor; Test = (makeTest divisor monkeyIdTrue monkeyIdFalse); InspectionCount = 0UL }

    matches
    |> Seq.map createMonkey
    |> Seq.fold
        (fun map monkey ->
            Map.add monkey.Id monkey map)
        Map.empty

let inspect relieve (map: Map<int, Monkey>) index =
    let id = index % map.Count
    let monkey = Map.find id map
    let worryLevels =
        monkey.Items
        |> List.map monkey.CalcWorryLevel
        |> List.map relieve
        |> List.map (fun level -> (monkey.Test level, level))
    worryLevels
    |> List.fold
        (fun map' (monkeyId, worryLevel) ->
            Map.change
                monkeyId
                (function
                | Some m -> Some { m with Items = worryLevel::m.Items }
                | None -> None)
                map')
        map
    |> Map.change
        id
        (function
        | Some m -> Some { m with Items = []; InspectionCount = m.InspectionCount + ((uint64)(List.length worryLevels)) }
        | None -> None)

let processRound isRidiculous (monkeys: Map<int, Monkey>) _ = 
    let modulo = monkeys |> Map.toList |> List.map (fun (_, m) -> m.Divisor) |> List.fold (fun acc v -> acc * v) 1UL
    let relieve =
        if isRidiculous
        then (fun x -> x % modulo)
        else (fun x -> x / 3UL)

    [0 .. (monkeys.Count - 1)]
    |> List.fold (inspect relieve) monkeys 

[1..20]
|> List.fold (processRound false) monkeys
|> Map.toList
|> List.sortByDescending (fun (_, m) -> m.InspectionCount)
|> List.take 2
|> List.fold (fun acc (_, m) -> acc * m.InspectionCount) 1UL
|> printfn "Part1: %A"

[1..10000]
|> List.fold (processRound true) monkeys
|> Map.toList
|> List.sortByDescending (fun (_, m) -> m.InspectionCount)
|> List.take 2
|> List.fold (fun acc (_, m) -> acc * m.InspectionCount) 1UL
|> printfn "Part2: %A"
