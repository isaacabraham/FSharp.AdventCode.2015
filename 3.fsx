open System.IO

let data = File.ReadAllText "3.txt"

let move (x,y) direction =
    match direction with
    | '^' -> x, y + 1
    | 'v' -> x, y - 1
    | '<' -> x - 1, y
    | '>' -> x + 1, y
    | _ -> failwith "unknown direction!"

let mapRoute moves =
    ((0,0), moves)
    ||> Seq.scan move
    
let calculateUniqueItems = Seq.distinct >> Seq.length

// 3.1
let housesVisitedYear1 = mapRoute data |> calculateUniqueItems     

// 3.2
let splitWork =
    Seq.mapi(fun index item -> index, item)
    >> Seq.groupBy(fun (index, _) -> index % 2 = 0)
    >> Seq.map(fun (key, items) -> items |> Seq.map snd)

let calculateMultipleRoutes = 
    splitWork >> Seq.map mapRoute >> Seq.concat >> calculateUniqueItems

let housesVisitedYear2 = calculateMultipleRoutes data