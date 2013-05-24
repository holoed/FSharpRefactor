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

module SymbolTable

open Zippers
open Ast

type SymbolTable = Map<string, SrcLoc list> * int  

let empty : SymbolTable TreeLoc = 
        create_node (Map.empty, 0) |> fromTree

let insert s l (t : SymbolTable TreeLoc) = 
        t |> modifyLabel (fun (dict, n) -> (Map.add s [l] dict, n)) 

let rec lookUp s l (t:SymbolTable TreeLoc) = 
        let { rootLabel = (dict, n)
              subForest = children } = t |> toTree
        if ((Map.containsKey s dict) &&
            (List.exists (fun x -> x = l) dict.[s])) then dict.[s]
        else children |> List.map (fun x -> lookUp s l (fromTree x)) 
                      |> List.concat                        
                                                
let enter_scope (t:SymbolTable TreeLoc) = 
        let { tree = { subForest = xs } } = t
        t |> insertDownLast (create_node (Map.empty, List.length xs))

let exit_scope (t:SymbolTable TreeLoc) = 
        t |> parent

let findTable s (t:SymbolTable TreeLoc) : (SymbolTable TreeLoc * int list) option = 
        let rec findTable' tOption acc =
            if (Option.isSome tOption) then
                let t = Option.get tOption
                let (dict, n) = getLabel t
                if (Map.containsKey s dict) then Some (t, acc)
                else findTable' (parent t) (n::acc)
            else Option.None
        findTable' (Some t) []

let rec moveBackTo path (t:SymbolTable TreeLoc) = 
         match path with
         | [] -> t
         | n::ns -> 
            let childOption = findChild (fun { rootLabel = (_, n') } -> n = n') t
            moveBackTo ns (Option.get childOption)        

let addRef s l (t:SymbolTable TreeLoc) =   
    let (Some(t', path)) = findTable s t  
    t' |> modifyLabel (fun (dict, n) -> (Map.map (fun s' ls -> if (s = s') then l::ls else ls) dict), n) 
       |> moveBackTo path