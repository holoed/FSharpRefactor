module SymbolTable

open Ast

type SymbolTable = private SymbolTable of Map<string, SrcLoc List> * SymbolTable option               

let empty = SymbolTable(Map.empty, Option.None)

let create t = SymbolTable(Map.empty, Some t)

let insert s l (SymbolTable (dict, p)) = SymbolTable (Map.add s [l] dict, p)

let rec lookUp s l (SymbolTable (dict, p)) = 
    if (dict |> Map.containsKey s |> not && Option.isSome p) 
    then lookUp s l (Option.get p)
    else let xs = dict |> Map.find s
         if (List.exists (fun x -> x = l) xs) then xs else [] 

let rec private map f ((SymbolTable (dict, p)) as t) = 
    match p with
    | Option.None -> SymbolTable (f dict, Option.None)
    | Option.Some x -> SymbolTable (f dict, Some(map f x))
                                              
let addRef s l t =
    t |> map (fun dict -> if (Map.containsKey s dict) 
                          then Map.add s (l::dict.[s]) dict
                          else dict)
       
 




