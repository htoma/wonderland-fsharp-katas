// See the file wonderland-number.md for detailed information.

let haveSameDigits (n1:int,n2:int) =
    (string n1 |> Set.ofSeq) = (string n2 |> Set.ofSeq)

let hasUniqueDigits (v: int) =
    string v
    |> Seq.groupBy (fun v -> v)
    |> Seq.exists (fun (k,l) -> l |> Seq.length >1) |> not

let wonderlandNumber () = 
    [123456..(int (1000000/6))] // first digit has to be 1, otherwise it overflows the six digits
    |> List.filter hasUniqueDigits
    |> List.filter (fun v ->
                            haveSameDigits(v, v*2) &&
                            haveSameDigits(v, v*3) &&
                            haveSameDigits(v, v*4) &&
                            haveSameDigits(v, v*5) &&
                            haveSameDigits(v, v*6))
    |> List.head

#r @"../packages/Unquote/lib/net45/Unquote.dll"
open Swensen.Unquote

let tests () =

    let wonderNum = wonderlandNumber ()

    test <@ (string wonderNum).Length = 6 @>

    test <@ haveSameDigits (wonderNum, 2 * wonderNum) @>
    test <@ haveSameDigits (wonderNum, 3 * wonderNum) @>
    test <@ haveSameDigits (wonderNum, 4 * wonderNum) @>
    test <@ haveSameDigits (wonderNum, 5 * wonderNum) @>
    test <@ haveSameDigits (wonderNum, 6 * wonderNum) @>

// run the tests
tests ()
