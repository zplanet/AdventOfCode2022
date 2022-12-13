open System.IO
open System.Text.RegularExpressions

let input =
    Path.Combine(Directory.GetCurrentDirectory(), "input.txt")
    |> File.ReadAllText

type Monkey = {
    Id: int;
    Items: int list;
    CalcWorryLevel: int -> int;
    Test: int -> int;
    InspectionCount: int;
}

let parseMonkeys makeCalcWorryLevel =
    let pattern = "Monkey ([0-9]+):[^S]+Starting items: ([0-9 ,]+)[^O]+Operation: new = old ([*+]) (old|[0-9]+)[^T]+Test: divisible by ([0-9]+)[^I]+If true: throw to monkey ([0-9]+)[^I]+If false: throw to monkey ([0-9]+)"
    let matches = Regex.Matches (input, pattern, RegexOptions.Multiline)

    let test divisor trueVal falseVal dividend = if dividend % divisor = 0 then trueVal else falseVal
    let makeTest divisor trueVal falseVal = test divisor trueVal falseVal

    let createMonkey (m: Match) =
        let monkeyId = int m.Groups[1].Value
        let items = m.Groups[2].Value.Split(", ") |> Array.map int |> Array.toList
        let calcWorryLevel =
            match m.Groups[3].Value, m.Groups[4].Value with
            | operator, operand when operator = "+" && operand = "old" -> makeCalcWorryLevel (+) id
            | operator, operand when operator = "*" && operand = "old" -> makeCalcWorryLevel (*) id
            | operator, operand when operator = "+" -> makeCalcWorryLevel (+) (fun _ -> int operand)
            | _, operand -> makeCalcWorryLevel (*) (fun _ -> int operand)
        let divisor = int m.Groups[5].Value
        let monkeyIdTrue = int m.Groups[6].Value
        let monkeyIdFalse = int m.Groups[7].Value
        { Id = monkeyId; Items = items; CalcWorryLevel = calcWorryLevel; Test = (makeTest divisor monkeyIdTrue monkeyIdFalse); InspectionCount = 0 }

    matches
    |> Seq.map createMonkey
    |> Seq.fold
        (fun map monkey ->
            Map.add monkey.Id monkey map)
        Map.empty

let inspect (map: Map<int, Monkey>) index =
    let id = index % map.Count
    let monkey = Map.find id map
    let worryLevels =
        monkey.Items
        |> List.map monkey.CalcWorryLevel
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
        | Some m -> Some { m with Items = []; InspectionCount = m.InspectionCount + (List.length worryLevels) }
        | None -> None)

let makeCalcWorryLevel f fb = (fun a -> (f a (fb a)) / 3)
let makeCalcWorryLevel2 (f: uint64 -> uint64 -> uint64) (fb: uint64 -> uint64) = (fun a -> f a (fb a))

let processRound (monkeys: Map<int, Monkey>) _ = 
    [0 .. (monkeys.Count - 1)]
    |> List.fold inspect monkeys

[1..20]
|> List.fold processRound (parseMonkeys makeCalcWorryLevel)
|> Map.toList
|> List.sortByDescending (fun (_, m) -> m.InspectionCount)
|> List.take 2
|> List.fold (fun acc (_, m) -> acc * m.InspectionCount) 1
|> printfn "Part1: %A"
