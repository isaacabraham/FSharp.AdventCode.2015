open System.IO

let data = File.ReadAllText "1.txt" 
let move = function | '(' -> 1 | _  -> -1

// 1.1
let endPosition = data |> Seq.sumBy move

// 1.2
let firstBasement =
    (0, data)
    ||> Seq.scan(fun position movement -> position + move movement)
    |> Seq.mapi(fun index position -> index, position)
    |> Seq.filter(snd >> ((=) -1))
    |> Seq.head