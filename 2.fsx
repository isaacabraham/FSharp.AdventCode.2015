open System.IO

let data = File.ReadAllLines "2.txt" 

let parseDayTwo (text:string) = text.Split('x') |> Array.map int |> fun [|l;w;h|] -> l,w,h 
let smallestSides (l,w,h) = 
    [ l; w; h ]
    |> List.sort
    |> List.take 2
// 2.1
let calculateMain (l,w,h) = (2 * l * w) + (2 * w * h) + (2 * h * l)
let calculateSmallestSideArea = smallestSides >> Seq.reduce (*)
//(*)  
let combinePaper dimensions = calculateMain dimensions + calculateSmallestSideArea dimensions
let calculatePaper = parseDayTwo >> combinePaper
let paperNeeded = data |> Seq.sumBy calculatePaper

// 2.2
let ribbonForWrapping =
    smallestSides
    >> List.map((*) 2) //(*)
    >> List.reduce (+) //(*)
let ribbonForBow (l,w,h) = [l;w;h] |> List.reduce (*) //(*)
let combineRibbon dimensions = ribbonForWrapping dimensions + ribbonForBow dimensions
let calculateRibbon = parseDayTwo >> combineRibbon

let ribbonNeeded = data |> Seq.sumBy calculateRibbon