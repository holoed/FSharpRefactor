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


module ASTAnalysis

open Ast
open AstAlgebras
open StateMonad
open AstCatamorphisms
open PurelyFunctionalDataStructures

type OpenScopes = OpenScopes of Map<string, Stack<SrcLoc list>>
type SymbolTable = SymbolTable of Map<string, SrcLoc list list>

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
                                    (fun xs -> state { return () }) 
                                    (fun () -> state { return () })
                                    (fun xs -> state { return () })
                                    (fun xs -> state { return () }) p

let flatPat p = foldPat (fun x -> [PVar x]) 
                        (fun l r -> l @ r) 
                        (fun x -> [PLit x]) 
                        (fun xs -> List.concat xs) 
                        (fun () -> [])
                        (fun xs -> List.concat xs) 
                        (fun xs -> List.concat xs) p

let astAnalysisAlgebra : ExpStateAlgebra<_, _, _, _, _, _, _, _, _, _, _, _, _>  = 
                let foldPat p = foldPatState (fun x -> state { return PVar x }) 
                                             (fun l r -> state { let! l' = l
                                                                 let! r' = r
                                                                 return PApp (l', r') }) 
                                             (fun x -> state { return PLit x })
                                             (fun es ->  state { let! es' = mmap (fun e -> state { return! e }) es
                                                                 return PTuple es' })
                                             (fun () -> state { return PWild } ) 
                                             (fun es ->  state { let! es' = mmap (fun e -> state { return! e }) es
                                                                 return PList es' })
                                             (fun xs -> state { let! xsAcc = mmap (fun x -> state { return! x }) xs 
                                                                return PLongVar xsAcc }) p
                {  varF = (fun x -> state { do! addUsage x
                                            return Var x })
                   longVarF = (fun xs -> state { let! xs' = mmap (fun x -> state { return! x }) xs
                                                 return LongVar xs' })
                   longVarSetF = (fun e1 e2 -> state { let! e1Acc = e1
                                                       let! e2Acc = e2
                                                       return LongVarSet (e1Acc, e2Acc) })
                   lamF = (fun ps e -> state { let! vars = mmap (fun p -> state { return! foldPat p }) ps
                                               let! _ = mmap (fun x -> execute enterScope x) vars
                                               let! e' = e
                                               let! _ = mmap (fun x -> execute exitScope x) vars 
                                               return Lam (vars, e') })
                   appF = (fun x y -> state { let! x' = x
                                              let! y' = y
                                              return App (x', y') })
                   letF = (fun isRec p e1 e2 -> state { let flatpat = flatPat p
                                                        match p with   
                                                        | PApp(_, _) ->
                                                             let (PVar (sf,lf)) = List.head flatpat
                                                             let vars = List.tail flatpat                                                
                                                             let! p'  = foldPat p
                                                             if isRec then 
                                                                do! enterScope (sf, lf) 
                                                             let! _ = mmap (fun x -> execute enterScope x) vars // ----------------------------------------------------
                                                             let! e1' = e1                                      // function variables scope (like the x in let f x = x)                                  
                                                             let! _ = mmap (fun x -> execute exitScope x) vars  // ----------------------------------------------------
                                                             if (not isRec) then
                                                                 do! enterScope (sf, lf)     // ----------------------------------------------------                                           
                                                             let! e2' = e2               // function name scope (like the f in let f x = x)  
                                                             if (e2' <> (Lit Unit)) then
                                                                do! exitScope (sf, lf)      // ----------------------------------------------------                               
                                                             return Let (isRec, p', e1', e2') 
                                                        | _ ->
                                                            let! p'  = foldPat p                                                 
                                                            let! e1' = e1       
                                                            let! _ = mmap (fun x -> execute enterScope x) flatpat                                            
                                                            let! e2' = e2           // let x = x in x Only "let x" and "in x" refer to the same identifier.
                                                            if (e2' <> (Lit Unit)) then
                                                                let! _ = mmap (fun x -> execute exitScope x) flatpat  // ---------------------------------------------------- 
                                                                do ()
                                                            return Let (isRec, p', e1', e2') })
                   litF = (fun x -> state { return Lit x })
                   tupleF = (fun es -> state { let! es' = mmap (fun e -> state { return! e }) es
                                               return Tuple es' })
                   listF = (fun es -> state { let! es' = mmap (fun e -> state { return! e }) es
                                              return List es' })
                   expF = (fun es -> state {  let! es' = mmap (fun e -> state { return! e }) es
                                              return Exp es' })
                   typesF = (fun xs -> state { let! xs' = mmap (fun x -> state { return! x }) xs
                                               return Types xs' })
                   unionF = (fun name cases -> state { let! _ = mmap (fun x -> enterScope x) cases
                                                       return DisUnion(name, cases) })
                   matchF = (fun e cs -> state { let! e' = e
                                                 let! cs' = mmap (fun c -> state { return! c }) cs
                                                 return Match(e', cs')})
                   clauseF = (fun p e -> state { let vars = flatPat p
                                                 let! _ = mmap (fun x -> execute enterScope x) vars 
                                                 let! e' = e
                                                 let! _ = mmap (fun x -> execute exitScope x) vars 
                                                 return Clause(p, e') })
                   forEachF = (fun p e1 e2 -> state{    let flatpat = flatPat p
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
                                                            return ForEach (p', e1', e2') 
                                                        | PVar (sf,lf), vars ->                                                 
                                                            let! p'  = foldPat p
                                                            let! _ = mmap (fun x -> execute enterScope x) vars // ----------------------------------------------------
                                                            let! e1' = e1                                      // function variables scope (like the x in let f x = x)                                  
                                                            let! _ = mmap (fun x -> execute exitScope x) vars  // ----------------------------------------------------
                                                            do! enterScope (sf, lf)     // ----------------------------------------------------                                           
                                                            let! e2' = e2               // function name scope (like the f in let f x = x)  
                                                            if (e2' <> (Lit Unit)) then
                                                                do! exitScope (sf, lf)      // ----------------------------------------------------                               
                                                            return ForEach (p', e1', e2') })
                   yieldOrRetF = (fun e -> state {  let! e' = e
                                                    return YieldOrReturn e' })
                   moduleF = (fun n ms -> state { let! ms' = mmap (fun m -> state { return! m }) ms
                                                  return NestedModule (n, ms') })
                   openF = (fun s -> state { return Open s })
                   ifThenElseF = (fun e1 e2 e3 -> state { let! e1' = e1
                                                          let! e2' = e2
                                                          let! e3' = match e3 with
                                                                     | Some x -> x
                                                          return IfThenElse(e1', e2', Some e3') })
                   dotIndexedSetF = (fun e1 es e3 -> state { let! e1' = e1
                                                             let! es' = mmap (fun e -> state { return! e }) es
                                                             let! e3' = e3
                                                             return DotIndexedSet (e1', es', e3') })
                   dotIndexedGetF = (fun e1 es -> state { let! e1' = e1
                                                          let! es' = mmap (fun e -> state { return! e }) es
                                                          return DotIndexedGet (e1', es') }) 
                   recordDefF = (fun name fields ms -> state { let! msAcc = mmap (fun m -> state { return! m }) ms
                                                               return Record(name, fields, msAcc) })
                   recordInstF = (fun fields -> state { let! fields' = mmap (fun e -> state { return! e }) fields
                                                        return Exp.Record fields'})
                   recordFieldInstF = (fun n e -> state { let! eAcc = e
                                                          return (n, eAcc) })
                   newF = (fun ss e -> state { let! eAcc = e
                                               return New (LongIdent (List.map (fun s -> Ident s) ss), eAcc) })                                                
                   noneF = (fun name -> state { return None name })
                   classF = (fun n ms -> state { let! ic = mmap (fun x -> state { let! xAcc = x
                                                                                  match xAcc with | ImplicitCtor ps -> return ps | _ -> return [] }) ms
                                                 let! _ = mmap (fun ps -> mmap (fun x -> execute enterScope x) ps) ic
                                                 let! msAcc = mmap (fun m -> state { return! m }) (if ic.IsEmpty then ms else ms.Tail)
                                                 let! _ = mmap (fun ps -> mmap (fun x -> execute exitScope x) ps) ic
                                                 return Class (n, msAcc) })
                   implicitConF = (fun ps -> state { return ImplicitCtor ps })
                   memberF = (fun p e -> state { let flatpat = flatPat p                                                                   
                                                 let boundName = List.head (List.tail flatpat)
                                                 let args = (List.head flatpat) :: List.tail (List.tail flatpat)
                                                 let! _ = mmap (fun x -> execute enterScope x) args
                                                 let! eAcc = e
                                                 let! _ = mmap (fun x -> execute exitScope x) args
                                                 return Member(p, eAcc) })
                   abstractSlotF = (fun n -> state { return AbstractSlot n })
                   objExprF = (fun ms -> state { let! msAcc = mmap (fun e -> state { return! e }) ms
                                                 return ObjExpr msAcc })
                   doF = (fun e -> state { let! eAcc = e
                                           return Do eAcc })
                   downcastF = (fun e t -> state { let! eAcc = e
                                                   return Downcast (eAcc, (LongIdent (List.map (fun s -> Ident s) t))) })
                   upcastF = (fun e t -> state { let! eAcc = e
                                                 return Upcast (eAcc, (LongIdent (List.map (fun s -> Ident s) t))) })
                   interfaceF = (fun t ms -> state { let! msAcc = mmap (fun e -> state { return! e }) ms
                                                     return Interface (LongIdent (List.map (fun s -> Ident s) t), msAcc) })
                   letBindingsF = (fun es -> state { let! esAcc = mmap (fun e -> state { return! e }) es
                                                     return LetBindings esAcc })
                   errorF = (fun () -> state { return ArbitraryAfterError }) }

// Exp<'a> list -> State<(OpenScopes * SymbolTable), Exp<'a> list>
let rec buildSymbolTable' progs = 
    state { let! ret = mmap (fun prog -> foldExpState astAnalysisAlgebra prog) progs
            do! exitGlobalScope
            return ret }
               
// Exp<'a> list -> SymbolTable
let buildSymbolTable progs = 
    let openScopes = OpenScopes(Map.empty)
    let symbolTable = SymbolTable(Map.empty)
    let (scopes, table) = executeGetState (buildSymbolTable' progs) (openScopes, symbolTable)
    table

// SymbolTable -> Exp<string * SrcLoc>
let getAllReferences (SymbolTable(table)) pos = 
        let data = Map.toList table
        [ for (s,xxs) in data do
            for xs in xxs do
                for x in xs do
                    if (x = pos) then
                        yield List.map (fun x -> Var(s, x)) xs ] |> List.concat
    
       
// Exp<'a> list -> SrcLoc -> Exp<string * SrcLoc> list                
let findAllReferences pos progs = 
        let symbolTable = buildSymbolTable progs
        getAllReferences symbolTable pos