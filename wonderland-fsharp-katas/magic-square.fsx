// See the file magic-square.md for detailed information.

let listWithout (pos: int) (values: 'a list) =
          values |> List.skip (pos+1)

let generateComb (values: 'a list) (n: int) =
    let rec gc (values: 'a list) (cur: int) =
        if cur=n then [[]]
        else
            values
            |> List.mapi (fun i x -> i,x)
            |> List.map (fun (i,v) -> 
                gc (values |> listWithout i) (cur+1)
                |> List.map (fun x -> v::x))
            |> List.concat
    gc values 0

let values = [| 1.0 .. 0.5 .. 5.0 |]

type Square = float[,]

let dim = 3

let magicSquare () =
    let sum = (values |> Array.sum) / (float)dim

    let lines = generateComb (values |> List.ofArray) dim
                |> List.filter (fun l -> l |> List.sum = sum)

    let hist = 
        lines
        |> List.concat
        |> List.groupBy (fun v -> v)
        |> List.map (fun (k,l) -> k,l.Length)

    let findEl (hist: ('a*int) list) (count: int) =
        let (v,_) = 
                hist |> List.find (fun (k,c) -> c=count) 
        v

    // fix this so that the tests pass!
    let square = Array2D.init dim dim (fun row col -> 0.)

    let center = findEl hist 4 // middle element has 4 lines passing through
    square.[dim/2,dim/2]<-center
    let centerLines = lines
                      |> List.filter (fun l -> l |> List.contains center)

    square


#r @"../packages/Unquote/lib/net45/Unquote.dll"
open Swensen.Unquote

let maxIndex = dim - 1
let indexes = [ 0 .. maxIndex ]

let row (sq:Square) i = [ for col in indexes -> sq.[i,col] ]
let col (sq:Square) i = [ for row in indexes -> sq.[row,i] ]

let sumRow (sq:Square) row =
    [ for col in indexes -> sq.[row,col] ] |> List.sum

let sumColumn (sq:Square) col =
    [ for row in indexes -> sq.[row,col] ] |> List.sum

let sumDownDiagonal (sq:Square) =
    [ for i in indexes -> sq.[i,i] ] |> List.sum

let sumUpDiagonal (sq:Square) =
    [ for i in indexes -> sq.[i, maxIndex - i] ] |> List.sum

let tests () =

    let magic = magicSquare ()

    // all the rows sum to the same number
    test <@ indexes |> List.map (sumRow magic) |> Set.ofList |> Set.count = 1 @>

    // all the columns sum to the same number
    test <@ indexes |> List.map (sumColumn magic) |> Set.ofList |> Set.count = 1 @>

    // all the diagonals sum to the same number
    test <@ sumDownDiagonal magic = sumUpDiagonal magic @>

// run the tests
tests ()
