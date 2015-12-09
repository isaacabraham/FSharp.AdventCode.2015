open System
open System.IO

type Validator = string -> bool
type LetterPair = char * char
let composeRules : Validator seq -> Validator = Seq.reduce(fun a b value -> a value && b value)   
let asPairs : string -> (int * LetterPair) seq  = Seq.pairwise >> Seq.mapi (fun index (a, b) -> index, (a, b))
let matchingPair : LetterPair -> bool = fun (a,b) -> a = b
// 5.1
//    It contains at least three vowels (aeiou only), like aei, xazegov, or aeiouaeiouaeiou.
let hasAtLeastThreeVowels : Validator =
    let vowels = ['a';'e';'i';'o';'u'] |> Set.ofSeq
    Seq.filter vowels.Contains
    >> Seq.length
    >> (<) 2

//    It contains at least one letter that appears twice in a row, like xx, abcdde (dd), or aabbccdd (aa, bb, cc, or dd).
let oneLetterTwiceInARow : Validator = asPairs >> Seq.map snd >> Seq.exists matchingPair

//    It does not contain the strings ab, cd, pq, or xy, even if they are part of one of the other requirements.
let doesNotContainSomeStrings : Validator =
    let badPairs = [ 'a','b'; 'c','d'; 'p','q'; 'x','y' ] |> Set.ofList
    Seq.pairwise >> Seq.exists (badPairs.Contains) >> not

let isValidWord =
    composeRules
      [ hasAtLeastThreeVowels
        oneLetterTwiceInARow
        doesNotContainSomeStrings ]       
   
let niceStrings =
    File.ReadLines "5.txt"
    |> Seq.filter isValidWord
    |> Seq.length
   
// 5.2

//    It contains a pair of any two letters that appears at least twice in the string without overlapping, like xyxy (xy) or aabcdefgaa (aa), but not like aaa (aa, but it overlaps).
let pairsAtLeastTwice : Validator =
  asPairs
  >> Seq.groupBy snd
  >> Seq.filter (snd >> Seq.length >> (<) 1)
  >> Seq.map(fun (pair, items) -> pair, items |> Seq.map fst)
  >> Seq.exists(fun (_, indexes) ->
     match indexes |> Seq.toList with
     | [ a; b ] -> b > a + 1
     | a :: b :: c :: _ -> b > a + 1 || c > a + 1
     | _ -> false)

//    It contains at least one letter which repeats with exactly one letter between them, like xyx, abcdefeghi (efe), or even aaa.
let repeatedLetterWithMiddleLetter : Validator =
  Seq.windowed 3
  >> Seq.exists(fun letters -> Seq.head letters = Seq.last letters)
  
let isValidComplexWord = composeRules [ pairsAtLeastTwice; repeatedLetterWithMiddleLetter ]

let niceComplexStrings =
    File.ReadLines "5.txt"
    |> Seq.filter isValidComplexWord
    |> Seq.length