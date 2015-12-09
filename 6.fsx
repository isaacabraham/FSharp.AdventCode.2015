open System.IO

type LightState = int
type LightMap = LightState [,]
type Coordinate = int * int
    
let parseCommandText (command:string) =
    let parseCoord (coord:string) = coord.Split ',' |> Array.map int |> fun c -> c.[0], c.[1]  
    match command.Split ' ' with
    | [| _; action; startCoord; _; endCoord |]
    | [| action; startCoord; _; endCoord |] ->
        let startCoord = startCoord |> parseCoord
        let endCoord = endCoord |> parseCoord
        action, startCoord, endCoord
    | _ -> failwith "Unknown format!"

let processCommand processor lightMap (action, (x1, y1), (x2, y2)) =    
    for x = x1 to x2 do
        for y = y1 to y2 do
            lightMap |> processor action (x,y)

module Array2D =
    let toSeq (lightMap:LightMap) = seq {
        for x in 0 .. (Array2D.length1 lightMap - 1) do
            for y in 0 .. (Array2D.length2 lightMap - 1) do
                yield lightMap.[x,y] } 

let calculateLightsLit processor commands lightMap =
    commands
    |> Seq.iter (processCommand processor lightMap)
    
    lightMap
    |> Array2D.toSeq
    |> Seq.sum
    
let createLightMap() = Array2D.init 1000 1000 (fun _ _ -> 0)

let commands =
    File.ReadLines "6.txt"
    |> Seq.map parseCommandText

// 6.1
let rec simpleProcessor action (x,y) (lightMap: LightMap) =
    match action with
    | "on" -> lightMap.[x,y] <- 1
    | "off" -> lightMap.[x,y] <- 0
    | "toggle" -> lightMap |> simpleProcessor (match lightMap.[x,y] with 0 -> "on" | _ -> "off") (x,y) 
    | _ -> failwith "unknown state!"

let part1Answer = createLightMap() |> calculateLightsLit simpleProcessor commands

// 6.2
let complexProcessor action (x,y) (lightMap: LightMap) =
    let currentLightLevel = lightMap.[x,y] 
    match action, currentLightLevel with
    | "off", 0 -> () // do nothing
    | "off", n -> lightMap.[x,y] <- n - 1
    | "on", n -> lightMap.[x,y] <- n + 1
    | "toggle", n -> lightMap.[x,y] <- n + 2 
    | _ -> failwith "unknown state!"    

let part2Answer = createLightMap() |> calculateLightsLit complexProcessor commands

