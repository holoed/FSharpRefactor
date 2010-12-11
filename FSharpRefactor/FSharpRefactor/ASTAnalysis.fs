module ASTAnalysis

open Ast
open StateMonad
open AstCatamorphisms
open PurelyFunctionalDataStructures

type OpenScopes = OpenScopes of Map<string, Stack<SrcLoc list>>
type SymbolTable = SymbolTable of Map<string, SrcLoc list list>

//Exp<'a> -> State<(OpenScopes * SymbolTable), Exp<'a>>
let buildSymbolTable'' exp : State<(OpenScopes * SymbolTable), Exp<'a>> = 
        let foldPat p = foldPatState (fun x -> state { return PVar x }) 
                                     (fun l r -> state { let! l' = l
                                                         let! r' = r
                                                         return PApp (l', r') }) 
                                                   (fun x -> state { return PLit x }) p
        foldExpState (fun x -> state { return Var x })
                     (fun ps b -> state { let! ps' = mmap (fun p -> state { return! foldPat p }) ps
                                          let! b' = b
                                          return Lam (ps', b') })
                     (fun x y -> state { let! x' = x
                                         let! y' = y
                                         return App (x', y') })
                     (fun p e1 e2 -> state { let! p'  = foldPat p
                                             let! e1' = e1
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
let getAllReferences (table:SymbolTable) = [Var ("x", {srcFilename = "test.fs"; srcLine = {startLine = 1; endLine = 1;}; srcColumn = {startColumn = 4; endColumn = 5;}})]
       
// Exp<'a> list -> SrcLoc -> Exp<string * SrcLoc> list                
let findAllReferences (exps:Exp<'a> list) (pos:SrcLoc) = 
        let symbolTable = buildSymbolTable exps
        getAllReferences symbolTable