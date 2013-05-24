// * **********************************************************************************************
// * Copyright (c) Edmondo Pentangelo. 
// *
// * This source code is subject to terms and conditions of the Apache License, Version 2.0. A 
// * copy of the license can be found in the License.html file at the root of this distribution. 
// * By using this source code in any fashion, you are agreeing to be bound 
// * by the terms of the Apache License, Version 2.0.
// *
// * You must not remove this notice, or any other, from this software.
// * **********************************************************************************************

module Zippers

type 'a Tree = { rootLabel:'a
                 subForest:'a Forest }
and 'a Forest = 'a Tree list


type 'a TreeLoc = { tree : 'a Tree
                    lefts : 'a Forest
                    rights : 'a Forest
                    parents : ('a Forest * 'a * 'a Forest) list }

// create_node : 'a -> 'a Tree
let create_node x = { rootLabel = x
                      subForest = [] }

// maybe : 'a -> ('b -> 'a) -> 'b option -> 'a
let maybe n f m = match m with
                  | None -> n
                  | Some x -> f x

// combChildren : 'a list -> 'a -> 'a list -> 'a list
let private combChildren ls t rs = List.fold (fun xs x -> x::xs) (t::rs) ls

// downParents : 'a TreeLoc -> ('a Forest * 'a * 'a Forest) list
let private downParents loc = (loc.lefts, (loc.tree).rootLabel, loc.rights) :: loc.parents

// splitChildren : 'a list -> 'a list -> int -> ('a list * 'a list) option
let rec splitChildren acc xs n = 
            match (acc, xs, n) with
            | (acc, xs, 0) -> Some (acc, xs)
            | (acc, x::xs, n) -> splitChildren (x::acc) xs (n - 1)
            | _ -> None                        


// parent : 'a TreeLoc -> 'a TreeLoc option
let parent loc = match loc.parents with
                 | (pls, v, prs) :: ps -> 
                       Some { tree = { rootLabel = v
                                       subForest = (combChildren (loc.lefts) (loc.tree) (loc.rights)) }
                              lefts = pls
                              rights = prs
                              parents = ps }
                 | [] -> None

// root : 'a TreeLoc -> 'a TreeLoc                 
let rec root loc = maybe loc root (parent loc)

// left : 'a TreeLoc -> 'a TreeLoc option
let left loc = match loc.lefts with
               | t::ts -> Some  { loc with
                                    tree = t
                                    lefts = ts
                                    rights = loc.tree :: loc.rights }
               | [] -> None

// right : 'a TreeLoc -> 'a TreeLoc option
let right loc = match loc.rights with
                | t::ts -> Some { loc with
                                    tree = t
                                    lefts = loc.tree :: loc.lefts
                                    rights = ts }
                | [] -> None

// firstChild : 'a TreeLoc -> 'a TreeLoc option             
let firstChild loc = match (loc.tree).subForest with
                     | t::ts -> Some { tree = t
                                       lefts = []
                                       rights = ts
                                       parents = downParents loc }
                     | [] -> None

// lastChild : 'a TreeLoc -> 'a TreeLoc option
let lastChild loc = match (List.rev ((loc.tree).subForest)) with
                    | t::ts -> Some { tree = t
                                      lefts = ts
                                      rights = []
                                      parents = downParents loc }
                    | [] -> None

// getChild : int -> 'a TreeLoc -> 'a TreeLoc option                    
let getChild n loc = 
        splitChildren [] ((loc.tree).subForest) n
        |> Option.map (fun (t::ls, rs) -> { tree = t
                                            lefts = ls
                                            rights = rs
                                            parents = downParents loc })


// findChild : ('a Tree -> bool) -> 'a TreeLoc -> 'a TreeLoc option
let findChild p loc =
       let rec split acc xs = 
            match xs with
            | x::xs when p x -> Some(acc, x, xs)
            | x::xs -> split (x::acc) xs 
                          | []-> None 
       split [] ((loc.tree).subForest)
       |> Option.map (fun (ls, t, rs) -> { tree = t
                                           lefts = ls
                                           rights = rs
                                           parents = downParents loc })

// fromTree : 'a Tree -> 'a TreeLoc
let fromTree t = { tree = t
                   lefts = []
                   rights = []
                   parents = [] }

// fromForest : 'a Forest -> 'a TreeLoc option                   
let fromForest (ts : 'a Forest) = 
    match ts with
    | t::ts -> Some { tree = t
                      lefts = []
                      rights = ts
                      parents = [] }
    | [] -> None

// toTree : 'a TreeLoc -> 'a Tree
let toTree loc = (root loc).tree
      
// toForest : 'a TreeLoc -> 'a Forest                      
let toForest loc : 'a Forest = 
    let r = root loc in combChildren (r.lefts) (r.tree) (r.rights)     
    
// isRoot : 'a TreeLoc -> bool
let isRoot loc = List.isEmpty (loc.parents)                            

// isFirst : 'a TreeLoc -> bool
let isFirst loc = List.isEmpty (loc.lefts)

// isLast : 'a TreeLoc -> bool
let isLast loc = List.isEmpty (loc.rights)

// isLeaf : 'a TreeLoc -> bool
let isLeaf loc = List.isEmpty ((loc.tree).subForest)

// isChild : 'a TreeLoc -> bool
let isChild loc = not (isRoot loc)

// hasChildren : 'a TreeLoc -> bool
let hasChildren loc = not (isLeaf loc)

// setTree : 'a Tree -> 'a TreeLoc -> 'a TreeLoc
let setTree t loc = { loc with tree = t }

// modifyTree : ('a Tree -> 'a Tree) -> 'a TreeLoc -> 'a TreeLoc
let modifyTree f loc = setTree (f (loc.tree)) loc

// setLabel : 'a -> 'a TreeLoc -> 'a TreeLoc
let setLabel v loc = modifyTree (fun t -> { t with rootLabel = v }) loc

// getLabel : 'a TreeLoc -> 'a
let getLabel loc = (loc.tree).rootLabel

// modifyLabel : ('a -> 'a) -> 'a TreeLoc -> 'a TreeLoc
let modifyLabel f loc = setLabel (f (getLabel loc)) loc

// insertLeft : 'a Tree -> 'a TreeLoc -> 'a TreeLoc
let insertLeft t loc = { loc with tree = t
                                  rights = loc.tree :: loc.rights }

// insertRight : 'a Tree -> 'a TreeLoc -> 'a TreeLoc
let insertRight t loc = { loc with tree = t
                                   lefts = loc.tree :: loc.lefts }

// insertDownFirst : 'a Tree -> 'a TreeLoc -> 'a TreeLoc
let insertDownFirst t loc = { loc with tree = t
                                       lefts = []
                                       rights = (loc.tree).subForest
                                       parents = downParents loc }

// insertDownLast : 'a Tree -> 'a TreeLoc -> 'a TreeLoc                                       
let insertDownLast t loc = { loc with tree = t
                                      lefts = List.rev ((loc.tree).subForest)
                                      rights = []
                                      parents = downParents loc }

// insertDownAt : int -> 'a Tree -> 'a TreeLoc -> 'a TreeLoc option
let insertDownAt n t loc = 
        splitChildren [] ((loc.tree).subForest) n
        |> Option.map (fun (ls, rs) -> { loc with tree = t
                                                  lefts = ls
                                                  rights = rs
                                                  parents = downParents loc })



