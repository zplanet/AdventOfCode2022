open System.IO
open System.Text.RegularExpressions

let input =
    Path.Combine(Directory.GetCurrentDirectory(), "input.txt")
    |> File.ReadLines

type Instruction = | Addx of int | Noop

let (|ADDX|_|) str =
    let m = Regex.Match(str, "addx ([-]?[0-9]+)")
    if m.Success then Some (int m.Groups[1].Value) else None

let (|NOOP|_|) str =
    let m = Regex.Match(str, "noop")
    if m.Success then Some () else None

let rec toInstructions lines = 
    seq {
        if Seq.isEmpty lines
        then Seq.empty
        else
            match (Seq.head lines) with
            | ADDX n ->
                yield Noop
                yield Addx n
            | NOOP () ->
                yield Noop
            
            yield! (toInstructions (Seq.tail lines))
    }

let instructions = input |> toInstructions
let cycles = Seq.initInfinite (fun index -> index + 1)

let rec loop registerX cycles instructions =
    seq {
        if Seq.isEmpty instructions
        then Seq.empty
        else
            yield ((Seq.head cycles), registerX)
            let x = 
                match (Seq.head instructions) with
                | Addx n -> registerX + n
                | Noop -> registerX
            yield! loop x (Seq.tail cycles) (Seq.tail instructions)
    }

let registerXs = loop 1 cycles instructions

[20; 60; 100; 140; 180; 220;]
|> List.fold
    (fun (prev, r, xs) index ->
        let xs' = Seq.skip (index - prev - 1) xs
        let x = snd (Seq.head xs')
        (index, ((x * index)::r), (Seq.tail xs')))
    (0, [], registerXs)
|> (fun (_, ls, _) -> List.rev ls)
|> List.sum
|> printfn "Part1: %A"
