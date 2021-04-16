// Learn more about F# at http://fsharp.org
open System
open Microsoft.FSharp.Collections

module Graph =
    open System.Collections.Generic

    type Graph<'Node when 'Node : equality>() =
        let mutable _costs = new Dictionary<'Node, Double>()
        let mutable _pred = new Dictionary<'Node, 'Node>()

        let rec path node currentPath =
            if _pred.ContainsKey(node) 
            then let pred = _pred.Item node
                 path pred (List.Cons(node, currentPath))
            else currentPath

        member this.CameFrom(node1, node2) =
            _pred.Add(node1, node2)

        member this.UpdateCost(node, w) =
            _costs.Add(node, w)

        member this.Path node =
            path node List.empty

        member this.SafeCost node =
            if _costs.ContainsKey node then Some (_costs.Item node)
            else None

        member this.Cost node =
            _costs.Item node

module AStar =
    open Graph
    let fst (x, _) = x
    let snd (_, x) = x
    let headTail ls = match ls with | head :: tail -> (head, tail)
                                    | [] -> failwith "Unexpected empty list in head-tail decomposition."

    let rec aStarR goal next cost h (graph : Graph<'Node>) (openNodes : 'Node list) =
        if List.isEmpty openNodes then None
        else let f (n : 'Node) = (graph.Cost n) + (h n)
             let (x, xs) = headTail (List.sortBy f openNodes)
             if goal x then Some (x, graph)
             else
                let mutable openNodes' = xs
                let g = graph.Cost x
                let path = graph.Path x
                for (n : 'Node) in (next x) do
                   let newG = g + cost x n
                   let mOldGP = graph.SafeCost n //Look for node in open + closed
                   match mOldGP with
                   | None -> do graph.UpdateCost(n, newG)
                             do graph.CameFrom(n, x)
                             openNodes' <- List.Cons (n, openNodes')
                   | Some oldG -> if oldG > newG then do graph.UpdateCost(n, newG)
                                                      do graph.CameFrom(n, x)
                                                      openNodes' <- List.Cons (n, openNodes')
                aStarR goal next cost h graph openNodes'

    let aStar goal next cost h start =
        let openNodes = [start] 
        let graph = new Graph<'Node>()
        do graph.UpdateCost(start, 0.0)
        aStarR goal next cost h graph openNodes                

[<EntryPoint>]
let main argv =
    let mutable x = ""
    do Console.WriteLine("Write something!")
    x <- Console.ReadLine()
    do Console.WriteLine("You wrote: " + x)
    do Console.ReadKey() |> ignore
    0 // return an integer exit code
