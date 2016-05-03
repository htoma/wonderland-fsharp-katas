// See the file doublets.md for detailed information.

open System.IO

type Vertex<'a> =
    { Data: 'a
      mutable Edges: Edge<'a> list }
and Edge<'a> =
    { To: Vertex<'a> }

type Graph<'a> when 'a:equality () =
    let mutable vertexes: Vertex<'a> list = []
   
    member this.AddVertex (data: 'a) =
        let v = { Data = data; Edges = []}
        vertexes <- v::vertexes
        v
   
    member this.AddEdge (fromV: Vertex<'a>) (toV: Vertex<'a>) =
        fromV.Edges <- { To = toV }::fromV.Edges
   
    member this.TryFindVertex (data: 'a) =
        vertexes
        |> List.tryFind ( fun v -> v.Data=data)
   
    member this.TryFindEdge (fromV: Vertex<'a>, toV: Vertex<'a>) =
        fromV.Edges
        |> List.tryFind ( fun e -> e.To=toV)

let createGraph (edges: ('a*'a) list) =
    let graph = Graph<'a>()
    edges
    |> List.iter (fun (f,t) ->
                    match graph.TryFindVertex f, graph.TryFindVertex t with
                    | Some v, Some u ->
                        match graph.TryFindEdge(v, u) with
                        | None -> graph.AddEdge v u
                        | _ -> ignore()
                        ignore()
                    | Some v, None ->
                        let u = graph.AddVertex t
                        graph.AddEdge v u
                    | None, Some u ->
                        let v = graph.AddVertex f
                        graph.AddEdge v u
                    | _ ->
                        let v = graph.AddVertex f
                        let u = graph.AddVertex t
                        graph.AddEdge v u
                    )
    graph
   
let areDiffByOne (left: string ) (right: string) =
    if left.Length<>right.Length || left=right then false
    else
        let rec diff acc left right =
            if acc>1 then acc
            else
                match left, right with
                | l::restl, r::restr ->
                    diff ( if l<>r then acc+1 else acc) restl restr
                | _ -> acc
        let res = diff 0 (left|> List.ofSeq) (right|> List.ofSeq)
        res=1

let getGraph (length: int ) (dictionary: string seq) =
    let words = dictionary
                |> Seq.filter ( fun v -> length=v.Length)
                |> Seq.distinct
                |> List.ofSeq
    [for w in words do
        for v in words -> w,v ]
    |> List.filter (fun (w,v) -> areDiffByOne w v)
    |> createGraph
   
//starts searching from v until a node with value t is found
//provides the first solution as a list of node values
//None otherwise
let search (v: 'a Vertex ) (t: 'a) =
    let containsData (t: 'a) (vlist: Vertex<'a> list) =
        vlist
        |> List.map ( fun v -> v.Data)
        |> List.exists ( fun v -> v=t)
    let rec dfs_in (v: 'a Vertex) (t: 'a) acc found=
        if v.Data=t then true,acc
        else
            let res = v.Edges
                       |> Seq.filter ( fun e -> acc |> containsData e.To.Data |> not)
                       |> Seq.map ( fun e -> (dfs_in e.To t (e.To::acc) false))
                       |> Seq.tryFind ( fun (s,_) -> s)           
            match res with
            | Some( true, list) -> true,list
            | _ -> false,acc
       
    dfs_in v t [v] false


let wordsPath = Path.Combine (__SOURCE_DIRECTORY__,"resources","words.txt")
let words = File.ReadAllLines wordsPath

type Word = string
let doublets (w1:Word,w2:Word) = 
    let graph = getGraph w1.Length words
    match graph.TryFindVertex w1 with
    | Some v ->
                match (search v w2) with
                | true,list -> list |> List.rev |> List.map ( fun v -> v.Data)
                | _ -> []
    | _ -> []

#r @"../packages/Unquote/lib/net45/Unquote.dll"
open Swensen.Unquote

let tests () =

    test <@ doublets ("head", "tail") = ["head"; "heal"; "teal"; "tell"; "tall"; "tail"] @>
    test <@ doublets ("door", "lock") = ["door"; "boor"; "book"; "look"; "lock"] @>
    test <@ doublets ("bank", "loan") = ["bank"; "bonk"; "book"; "look"; "loon"; "loan"] @>
    test <@ doublets ("wheat", "bread") = ["wheat"; "cheat"; "cheap"; "cheep"; "creep"; "creed"; "breed"; "bread"] @>

    test <@ doublets ("ye", "freezer") = [] @>

// run the tests
tests ()
