open System.IO
open System.Text.RegularExpressions

let input =
    Path.Combine(Directory.GetCurrentDirectory(), "input.txt")
    |> File.ReadLines

type PacketData =
    | Packet of int 
    | PacketList of PacketData list

let pairs lines = 
    let parse str =
        Regex.Matches(str, "(\[|]|[0-9]+)")
        |> Seq.map (fun m -> m.Value)
        |> Seq.toList

    let rec loop prev ls =
        seq {
            if Seq.isEmpty ls
            then Seq.empty
            else
                let curr = Seq.head ls
                let tail = Seq.tail ls
                match prev with
                | None -> yield! loop (Some curr) tail
                | Some a when a = "" -> yield! loop (Some curr) tail
                | Some a ->
                    yield (parse a, parse curr)
                    yield! loop None tail
        }
    loop None lines

let parseInt (str: string) = 
    match System.Int32.TryParse str with
    | true, n -> Some n
    | _ -> None

let toPacketData list =
    let rec loop r = function
        | h::ts when h = "[" -> 
            let (plist, remains) = loop [] ts
            loop ((PacketList plist)::r) remains
        | h::ts when h = "]" -> 
            (List.rev r, ts)
        | h::ts -> 
            match parseInt h with
            | Some n -> loop ((Packet n)::r) ts
            | None -> loop r ts
        | _ ->
            (List.rev r, [])
    loop [] list
    |> fst |> List.head

let isInRightOrder pair =
    let rec loop = function
        | PacketList ((Packet p1)::t1), PacketList ((Packet p2)::t2) ->
            if p1 = p2
            then loop (PacketList t1, PacketList t2)
            else (if p1 < p2 then true else false)
        | PacketList ((PacketList l1)::t1), PacketList ((PacketList l2)::t2) ->
            if (loop (PacketList l1, PacketList l2))
            then loop (PacketList t1, PacketList t2)
            else false
        | PacketList ((PacketList l1)::t1), PacketList ((Packet p2)::t2) ->
            if (loop (PacketList l1, PacketList [Packet p2]))
            then loop (PacketList t1, PacketList t2)
            else false
        | PacketList ((Packet p1)::t1), PacketList ((PacketList l2)::t2) ->
            if (loop (PacketList [Packet p1], PacketList l2))
            then loop (PacketList t1, PacketList t2)
            else false
        | PacketList (h1::t1), PacketList [] -> false
        // | PacketList [], _ -> true
        | _ -> true
    loop pair

input
|> pairs
|> Seq.map (fun (left, right) -> (toPacketData left, toPacketData right))
|> Seq.map isInRightOrder
|> Seq.indexed
|> Seq.filter snd
|> Seq.map (fun (index, _) -> index + 1)
|> Seq.sum
|> printfn "%A"