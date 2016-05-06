// See the file magic-square.md for detailed information.

let values = [| 1.0 .. 0.5 .. 5.0 |]

type Square = float[,]

let dim = 3

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

let listAfter (pos: int) (values: 'a list) =
          values |> List.skip (pos+1)

let generateComb (values: 'a list) (n: int) =
    let rec gc (values: 'a list) (cur: int) =
        if cur=n then [[]]
        else
            values
            |> List.mapi (fun i x -> i,x)
            |> List.map (fun (i,v) -> 
                gc (values |> listAfter i) (cur+1)
                |> List.map (fun x -> v::x))
            |> List.concat
    gc values 0

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
        hist 
        |> List.filter (fun (k,c) -> c=count) 
        |> List.map (fun (v,_) -> v)
 
    // fix this so that the tests pass!
    let square = Array2D.init dim dim (fun row col -> 0.)

    let center = findEl hist 4 |> List.head // middle element has 4 lines passing through
    square.[dim/2,dim/2]<-center

    let elDiag = findEl hist 3
    let firstDiag = [|elDiag.Head;elDiag |> List.find (fun v -> v+center+elDiag.Head=sum)|]
    let secondDiag = elDiag |> List.except firstDiag |> Array.ofList
    for i in [0..(dim-1)] do
        if i<>(dim/2) then
            square.[i,i]<- firstDiag.[i-( if i>dim/2 then 1 else 0)]
            square.[i,2-i]<- secondDiag.[i-( if i>dim/2 then 1 else 0)]
    //the easy way :)
    for i in [0..(dim-1)] do
        for j in [0..(dim-1)] do
            if square.[i,j]=0. then
                square.[i,j]<-System.Math.Min(sum-(sumColumn square j), sum-(sumRow square i))

    square

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
