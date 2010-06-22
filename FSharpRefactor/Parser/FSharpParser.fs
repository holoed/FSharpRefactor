module FSharpParser

open Utils
open System
open Ast
open MonadicParser
open MonadicParserCombinators

let addOp = parser { let! _ = symbol "+"
                     return fun x y -> InfixApp (x, "+", y) }
let subOp = parser { let! _ = symbol "-"
                     return fun x y -> InfixApp (x, "-", y) }
let mulOp = parser { let! _ = symbol "*"
                     return fun x y -> InfixApp (x, "*", y) }
let divOp = parser { let! _ = symbol "/"
                     return fun x y -> InfixApp (x, "/", y) }

let appOp = parser { return fun x y -> App (x, y) }

let integer = parser { let! x = integer
                       return Lit(Integer x) }

let rec atom = lam +++ local +++ var +++ integer +++ paren

and exp = chainl1 term (addOp +++ subOp)
and term = chainl1 factor (mulOp +++ divOp)
and factor = chainl1 atom (appOp)

and lam = parser { let! _ = symbol "fun"
                   let! x = variable
                   let! _ = symbol "->"
                   let! e = exp 
                   return Lam (seqtostring x, e) }

and local = parser { let! _ = symbol "let"
                     let! x = variable
                     let! _ = symbol "="
                     let! e = exp
                     let! _ = symbol "in"
                     let! e' = exp
                     return Let (seqtostring x, e, e') }

and var = parser { let! x = variable
                   return Var (seqtostring x) }

and paren = parser { let! _ = symbol "("
                     let! e = exp
                     let! _ = symbol ")" 
                     return e }

and variable = identifier ["let";"in"]

let parseExp s = match (parse exp (PString((0,0), s))) with
                 | [] -> None
                 | [(exp, _)] -> Some exp