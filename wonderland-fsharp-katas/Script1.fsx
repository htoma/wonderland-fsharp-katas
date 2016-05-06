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
            square.[i,i]<- firstDiag.[i-(if i>dim/2 then 1 else 0)]
            square.[i,2-i]<- firstDiag.[i-(if i>dim/2 then 1 else 0)]
    square

magicSquare()

