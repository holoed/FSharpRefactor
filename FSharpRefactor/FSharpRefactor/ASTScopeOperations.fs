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

module ASTScopeOperations

open Ast
open StateMonad
open AstCatamorphisms
open PurelyFunctionalDataStructures

type OpenScopes = OpenScopes of Map<string, Stack<SrcLoc list>>
type SymbolTable = SymbolTable of Map<string, SrcLoc list list>

let SubMatch (map:Map<string,_>) s =  map |> Seq.filter (fun x -> x.Key.Contains "." && (x.Key.Split [|'.'|]).[1] = s)
                                          |> Seq.map (fun p -> (p.Key, p.Value))

let mostRecent (map:seq<string * Stack<SrcLoc list>>) = map |> Seq.map (fun (k, v) -> (k, (stackToList v) |> Seq.concat))
                                                            |> Seq.map (fun (k, vs) -> Seq.map (fun v -> (k, v)) vs)
                                                            |> Seq.concat
                                                            |> Seq.maxBy (fun (_, v) -> v.srcLine.startLine)
                                                            |> fun (k, _) -> k   

let addUsage (s,l) = state { let! (OpenScopes(map), table) = getState 
                             if (map.ContainsKey s) then
                                 let scopes = map.[s]
                                 if (not (isEmpty scopes)) then
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
                              if (map.ContainsKey s) then                                
                                  let scopes = map.[s]
                                  let (x,xs) = pop scopes
                                  let newMap = map.Remove(s).Add(s, xs)
                                  let newTable = if (not (table.ContainsKey s)) then 
                                                            table.Add (s, [x]) 
                                                      else        
                                                            let ys = table.[s]
                                                            table.Remove(s).Add(s, x::ys)
                                  do! setState(OpenScopes(newMap), SymbolTable(newTable))
                                  return ()
                              else
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


