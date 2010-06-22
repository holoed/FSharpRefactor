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