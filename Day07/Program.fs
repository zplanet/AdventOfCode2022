open System.IO
open System.Text.RegularExpressions

let input = 
    Path.Combine(Directory.GetCurrentDirectory(), "input.txt")
    |> File.ReadLines
    |> Seq.toList

type File = int * string
type Folder = | Folder of string * File list * Folder list * int

let (|CD|_|) str =
   let m = Regex.Match(str, "\$ cd ([/A-Za-z0-9.]+)")
   if m.Success then Some (m.Groups[1].Value) else None

let (|LS|_|) str =
   let m = Regex.Match(str, "\$ ls")
   if m.Success then Some ("ls") else None

let (|DIR|_|) str =
   let m = Regex.Match(str, "dir ([A-Za-z0-9]+)")
   if m.Success then Some (m.Groups[1].Value) else None

let (|FILE|_|) str =
   let m = Regex.Match(str, "([0-9]+) ([A-Za-z0-9.]+)")
   if m.Success then Some ((int m.Groups[1].Value), m.Groups[2].Value) else None

let buildTree lines =
   let rec cd folderName files (subFolders: Folder list) ls =
      if (List.isEmpty ls)
      then (Folder (folderName, files, subFolders, 0), [])
      else
         match (List.head ls) with
         | "" -> (Folder (folderName, files, subFolders, 0), [])
         | CD name when name = ".." -> (Folder (folderName, files, subFolders, 0), (List.tail ls))
         | CD name -> 
            let (subFolder, tail) = cd name [] [] (List.tail ls)
            cd folderName files (subFolder::subFolders) tail
         | FILE info -> cd folderName (info::files) subFolders (List.tail ls)
         | _ -> cd folderName files subFolders (List.tail ls)
   cd "/" [] [] (List.tail lines)

let calcFolderSize folder =
   let rec traverse (Folder (name, files, subFolders, _)) =
      let totalFileSize = List.sumBy (fun (size, _) -> size) files

      if (List.isEmpty subFolders)
      then Folder (name, files, subFolders, totalFileSize)
      else 
         let subs = List.map traverse subFolders
         let totalSubFolderSize = List.sumBy (fun (Folder (_, _, _, size)) -> size) subs
         Folder (name, files, subs, totalFileSize + totalSubFolderSize)
   traverse folder

let findFolders folder =
   let rec traverse r q =
      if List.isEmpty q
      then r
      else
         let (Folder (name, _, subFolders, size)) = List.head q
         if size < 100000
         then
            traverse ((name, size)::r) ((List.tail q) @ subFolders)
         else
            traverse r ((List.tail q) @ subFolders)
   traverse [] [folder]

let rootFolder = 
   input
   |> buildTree
   |> fst
   |> calcFolderSize

let totalDiskSpace = 70000000
let leastUnusedSpace = 30000000

let requiredSpace = 
   rootFolder
   |> (fun (Folder (_, _, _, size)) -> size)
   |> ((-) totalDiskSpace)
   |> ((-) leastUnusedSpace)

let findFolderToBeDeleted folder = 
   let rec traverse r q =
      if List.isEmpty q
      then r
      else
         let (Folder (name, _, subFolders, size)) = List.head q
         if requiredSpace <= size && size < r
         then
            traverse size ((List.tail q) @ subFolders)
         else
            traverse r ((List.tail q) @ subFolders)
   traverse totalDiskSpace [folder]

rootFolder
|> findFolders
|> List.sumBy snd
|> printfn "Part1: %A"

rootFolder
|> findFolderToBeDeleted
|> printfn "Part2: %A"
