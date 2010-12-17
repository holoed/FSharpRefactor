// * **********************************************************************************************
// * Copyright (c) Edmondo Pentangelo. 
// *
// * This source code is subject to terms and conditions of the Microsoft Public License. 
// * A copy of the license can be found in the License.html file at the root of this distribution. 
// * By using this source code in any fashion, you are agreeing to be bound by the terms of the 
// * Microsoft Public License.
// *
// * You must not remove this notice, or any other, from this software.
// * **********************************************************************************************

module ASTAnalysis

open Ast
open StateMonad
open AstCatamorphisms
open PurelyFunctionalDataStructures

type OpenScopes = OpenScopes of Map<string, Stack<SrcLoc list>>
type SymbolTable = SymbolTable of Map<string, SrcLoc list list>

let addUsage (s,l) = state { let! (OpenScopes(map), table) = getState 
                             if (map.ContainsKey s) then
                                 let scopes = map.[s]
                                 let (xs,xxs) = pop scopes
                                 let instances = push xxs (l::xs)
                                 let newMap = map.Remove(s).Add(s, instances)
                                 do! setState(OpenScopes(newMap), table)
                                 return () }

let enterScope (s,l) = state { let! (OpenScopes(map), table) = getState 
                               let newMap = if (not (map.ContainsKey s)) then 
                                                        map.Add (s, push empty ([l])) 
                                                  else        
                                                        let xs = map.[s]
                                                        map.Remove(s).Add(s, push xs [l])
                               do! setState(OpenScopes(newMap), table)
                               return () }

let exitScope (s,l) = state { let! (OpenScopes(map), SymbolTable(table)) = getState 
                              let scopes = map.[s]
                              let (x,xs) = pop scopes
                              let newMap = map.Remove(s).Add(s, xs)
                              let newTable = if (not (table.ContainsKey s)) then 
                                                        table.Add (s, [x]) 
                                                  else        
                                                        let ys = table.[s]
                                                        table.Remove(s).Add(s, x::ys)
                              do! setState(OpenScopes(newMap), SymbolTable(newTable))
                              return () }

let exitGlobalScope = state { let! (OpenScopes(map), SymbolTable(table)) = getState 
                              let scopesList = map |> Map.toList |> List.filter (fun (_,scopes) -> not (isEmpty scopes))
                              let! _ = mmap (fun (s,scopes) ->
                                                  state { let! (_, SymbolTable(table)) = getState 
                                                          let (x,xs) = pop scopes
                                                          let newTable = if (not (table.ContainsKey s)) then 
                                                                                  table.Add (s, [x]) 
                                                                         else        
                                                                                  let ys = table.[s]
                                                                                  table.Remove(s).Add(s, x::ys)
                                                          do! setState(OpenScopes(Map.empty), SymbolTable(newTable))
                                                          return () }) scopesList
                              return () }

let execute action p = foldPatState (fun (s:string,l:SrcLoc) -> state { do! action (s,l)
                                                                        return () }) 
                                    (fun l r -> state { let! l' = l
                                                        let! r' = r
                                                        return () }) 
                                    (fun x -> state { return () })
                                    (fun xs -> state { return () }) p

let flatPat p = foldPat (fun x -> [PVar x]) (fun l r -> l @ r) (fun x -> [PLit x]) (fun xs -> List.concat xs) p

//Exp<'a> -> State<(OpenScopes * SymbolTable), Exp<'a>>
let buildSymbolTable'' exp : State<(OpenScopes * SymbolTable), Decl<'a>> = 
    let foldPat p = foldPatState (fun x -> state { return PVar x }) 
                                 (fun l r -> state { let! l' = l
                                                     let! r' = r
                                                     return PApp (l', r') }) 
                                 (fun x -> state { return PLit x })
                                 (fun es ->  state { let! es' = mmap (fun e -> state { return! e }) es
                                                     return PTuple es' }) p
    foldExpState (fun x -> state { do! addUsage x
                                   return Var x })
                 (fun ps b -> state { let! ps' = mmap (fun p -> state { return! foldPat p }) ps
                                      let! b' = b
                                      return Lam (ps', b') })
                 (fun x y -> state { let! x' = x
                                     let! y' = y
                                     return App (x', y') })
                 (fun p e1 e2 -> state { let flatpat = flatPat p
                                         let boundName = List.head flatpat
                                         let args = List.tail flatpat
                                         match (boundName, args) with
                                         | PVar (s,l), [] ->                                                 
                                             let! p'  = foldPat p                                                 
                                             let! e1' = e1       
                                             do! enterScope (s,l)    // ----------------------------------------------------                                               
                                             let! e2' = e2           // let x = x in x Only "let x" and "in x" refer to the same identifier.
                                             if (e2' <> (Lit Unit)) then
                                                do! exitScope (s,l)  // ----------------------------------------------------  
                                             return Let (p', e1', e2') 
                                         | PVar (sf,lf), vars ->                                                 
                                             let! p'  = foldPat p
                                             let! _ = mmap (fun x -> execute enterScope x) vars // ----------------------------------------------------
                                             let! e1' = e1                                      // function variables scope (like the x in let f x = x)                                  
                                             let! _ = mmap (fun x -> execute exitScope x) vars  // ----------------------------------------------------
                                             do! enterScope (sf, lf)     // ----------------------------------------------------                                           
                                             let! e2' = e2               // function name scope (like the f in let f x = x)  
                                             if (e2' <> (Lit Unit)) then
                                                do! exitScope (sf, lf)      // ----------------------------------------------------                               
                                             return Let (p', e1', e2') })
                 (fun x -> state { return Lit x })
                 (fun es -> state { let! es' = mmap (fun e -> state { return! e }) es
                                    return Tuple es' })
                 (fun es -> state { let! es' = mmap (fun e -> state { return! e }) es
                                    return List es' })
                 (fun e -> state {  let! e' = e
                                    return Exp e' })
                 (fun xs -> state { let! xs' = mmap (fun x -> state { return! x }) xs
                                    return Types xs' })
                 (fun name cases -> state { let! _ = mmap (fun x -> enterScope x) cases
                                            return DisUnion(name, cases) })
        exp

// Exp<'a> list -> State<(OpenScopes * SymbolTable), Exp<'a> list>
let rec buildSymbolTable' (exps:Decl<'a> list) = 
    state { let! ret = mmap (fun exp -> buildSymbolTable'' exp) exps
            do! exitGlobalScope
            return ret }
               
// Exp<'a> list -> SymbolTable
let buildSymbolTable exps = 
    let openScopes = OpenScopes(Map.empty)
    let symbolTable = SymbolTable(Map.empty)
    let (scopes, table) = executeGetState (buildSymbolTable' exps) (openScopes, symbolTable)
    table

// SymbolTable -> Exp<string * SrcLoc>
let getAllReferences (SymbolTable(table)) (pos:SrcLoc) = 
        let data = Map.toList table
        [ for (s,xxs) in data do
            for xs in xxs do
                for x in xs do
                    if (x = pos) then
                        yield List.map (fun x -> Var(s, x)) xs ] |> List.concat
    
       
// Exp<'a> list -> SrcLoc -> Exp<string * SrcLoc> list                
let findAllReferences (pos:SrcLoc) (exps:Decl<'a> list) = 
        let symbolTable = buildSymbolTable exps
        getAllReferences symbolTable pos