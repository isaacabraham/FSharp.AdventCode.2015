open System.Security.Cryptography
open System.Text
open FSharp.Core.Printf

#time

let calculateMD5Hash getHash (input:string) =
    let hash:byte array = 
        input
        |> Encoding.ASCII.GetBytes
        |> getHash

    seq { for hashByte in hash.[0..2] -> hashByte.ToString "X2" }

let findNextToken key =
    let md5 = MD5.Create()
    seq {
        for number in 0 .. System.Int32.MaxValue do
            if number % 500000 = 0 then printfn "%d" number
            yield number, (key + (number.ToString())
                          |> calculateMD5Hash md5.ComputeHash
                          |> String.concat "")
    }
    |> Seq.filter(fun (_, token) -> token = "000000")
    |> Seq.head

findNextToken "yzbqklnj"


