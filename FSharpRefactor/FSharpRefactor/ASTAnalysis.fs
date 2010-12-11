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

let execute action p = foldPatState (fun (s:string,l:SrcLoc) -> state { do! action (s,l)
                                                                        return () }) 
                                    (fun l r -> state { //let! l' = l
                                                        let! r' = r
                                                        return () }) 
                                    (fun x -> state { return () }) p


//Exp<'a> -> State<(OpenScopes * SymbolTable), Exp<'a>>
let buildSymbolTable'' exp : State<(OpenScopes * SymbolTable), Exp<'a>> = 
        let foldPat p = foldPatState (fun x -> state { return PVar x }) 
                                     (fun l r -> state { let! l' = l
                                                         let! r' = r
                                                         return PApp (l', r') }) 
                                                   (fun x -> state { return PLit x }) p
        foldExpState (fun x -> state { do! addUsage x
                                       return Var x })
                     (fun ps b -> state { let! ps' = mmap (fun p -> state { return! foldPat p }) ps
                                          let! b' = b
                                          return Lam (ps', b') })
                     (fun x y -> state { let! x' = x
                                         let! y' = y
                                         return App (x', y') })
                     (fun p e1 e2 -> state { do! execute (enterScope) p
                                             let! p'  = foldPat p
                                             let! e1' = e1                                             
                                             do! execute (exitScope) p                                             
                                             let! e2' = e2                                             
                                             return Let (p', e1', e2') })
                     (fun x -> state { return Lit x })
                     (fun e t -> state { let! e' = e
                                         return WithTy (e', t) })
         exp

// Exp<'a> list -> State<(OpenScopes * SymbolTable), Exp<'a> list>
let rec buildSymbolTable' (exps:Exp<'a> list) = 
    state { return! mmap (fun exp -> buildSymbolTable'' exp) exps }
               
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
let findAllReferences (exps:Exp<'a> list) (pos:SrcLoc) = 
        let symbolTable = buildSymbolTable exps
        getAllReferences symbolTable pos