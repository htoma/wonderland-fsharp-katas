// See the file tiny-maze.md for detailed information.

type Cell =
    | Start
    | Exit
    | Empty
    | Wall

type Maze = Cell [,]

type Path =
    | X
    | O

type Solution = Path [,]

let rows (maze:Maze) =
    (int) (System.Math.Sqrt((float)maze.Length))

let getNeighbors (maze:Maze) i j =
    let rows = rows maze
    [i-1,j
     i,j-1
     i,j+1
     i+1,j]
     |> List.filter (fun (i,j) -> i>=0 && i<rows && j>=0 && j<rows)

let findStart (maze:Maze) =
    let rows = rows maze
    [for i in 0..rows-1 do
        for j in 0..rows-1 -> i,j,maze.[i,j]]
    |> List.filter (fun (i,j,v) -> v=Start)
    |> List.exactlyOne

let solve (maze:Maze) : Solution =
    let i,j,start = findStart maze
    let mutable seen = [] |> Set.ofList
                    
    let rec advance (i,j) =
        seen<-seen.Add (i,j)
        match maze.[i,j] with
        | Exit -> [(i,j)]
        | Start | Empty ->
            let sol = getNeighbors maze i j
                        |> List.filter (fun (i,j) -> Set.contains (i,j) seen |> not)
                        |> List.map (fun v -> advance v)
                        |> List.tryFind (fun v -> v |> List.length>0)
            match sol with
            | Some acc -> (i,j)::acc
            | None -> []
        | _ -> []
        
    let track = advance (i,j)
    let rows = rows maze
    [for i in 0..rows-1 ->
        [for j in 0..rows-1 ->
            if (track |> List.contains (i,j)) then X else O]
    ] |> array2D


#r @"../packages/Unquote/lib/net45/Unquote.dll"
open Swensen.Unquote

let tests () =

    // sample 3x3 maze
    let maze3x3 =
        [ [Start; Empty; Wall]
          [Wall;  Empty; Wall]
          [Wall;  Empty; Exit]]
        |> array2D

    // sample 3x3 maze solution
    let solution3x3 =
        [ [X; X; O]
          [O; X; O]
          [O; X; X]]
        |> array2D

    test <@ solution3x3 = solve maze3x3 @>

    // sample 4x4 maze
    let maze4x4 =
         [[Start; Empty; Empty; Wall ]
          [Wall;  Wall;  Empty; Empty]
          [Wall;  Empty; Empty; Wall ]
          [Wall;  Wall;  Empty; Exit ]]
         |> array2D

    // sample 4x4 maze solution
    let solution4x4 =
        [[X; X; X; O]
         [O; O; X; O]
         [O; O; X; O]
         [O; O; X; X]]
        |> array2D

    test <@ solution4x4 = solve maze4x4 @>


// run the tests
tests ()
