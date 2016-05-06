open System

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

let values = [1.0 .. 0.5 .. 5.0]
let sum = (values |> List.sum) / 3
generateComb [1.0 .. 0.5 .. 5.0] 3
|> List.filter (fun l -> l |> List.sum = )

