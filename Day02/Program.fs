open System.IO

type RPS = Rock | Paper | Scissor
type Result = Win | Lose | Draw

let input = 
    Path.Combine(Directory.GetCurrentDirectory(), "input.txt")
    |> File.ReadLines

let toRPS = function
    | "A" | "X" -> Rock
    | "B" | "Y" -> Paper
    | "C" | "Z" -> Scissor

let toResult = function
    | "X" -> Lose
    | "Y" -> Draw
    | "Z" -> Win

let parseRound opponentParser meParser (s: string) =
    let arr = s.Split(" ")
    (opponentParser arr.[0], meParser arr.[1])

let calcScore = function
    | (Rock, Rock) -> 3
    | (Rock, Paper) -> 6
    | (Rock, Scissor) -> 0
    | (Paper, Rock) -> 0
    | (Paper, Paper) -> 3
    | (Paper, Scissor) -> 6
    | (Scissor, Rock) -> 6
    | (Scissor, Paper) -> 0
    | (Scissor, Scissor) -> 3

let calcChoice = function
    | (_, Rock) -> 1
    | (_, Paper) -> 2
    | (_, Scissor) -> 3

let calcResult = function
    | (Rock, Lose) -> (Rock, Scissor)
    | (Rock, Draw) -> (Rock, Rock)
    | (Rock, Win) -> (Rock, Paper)
    | (Paper, Lose) -> (Paper, Rock)
    | (Paper, Draw) -> (Paper, Paper)
    | (Paper, Win) -> (Paper, Scissor)
    | (Scissor, Lose) -> (Scissor, Paper)
    | (Scissor, Draw) -> (Scissor, Scissor)
    | (Scissor, Win) -> (Scissor, Rock)

// Part 1
input
|> Seq.map (parseRound toRPS toRPS)
|> Seq.map (fun round -> calcScore(round) + calcChoice(round))
|> Seq.sum
|> printfn "Part 1: %A"

// Part 2
input
|> Seq.map (parseRound toRPS toResult)
|> Seq.map calcResult
|> Seq.map (fun round -> calcScore(round) + calcChoice(round))
|> Seq.sum
|> printfn "Part 2: %A"
