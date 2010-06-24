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

let chainl1Line p op =         
      let rec rest (x, l1) = parser { let! f = op
                                      let! ((l2,_), _) = fetch
                                      let! y = p                                       
                                      if (l1 = l2) then
                                            return! rest ((f x y), l2) } +++ parser { return x }
      parser { let! ((l,_), _) = fetch
               let! x = p               
               return! rest (x,l) }


let addOp = parser { let! _ = symbol "+"
                    return fun x y -> InfixApp (x, "+", y) }
let subOp = parser { let! _ = symbol "-"
                    return fun x y -> InfixApp (x, "-", y) }
let mulOp = parser { let! _ = symbol "*"
                    return fun x y -> InfixApp (x, "*", y) }
let divOp = parser { let! _ = symbol "/"
                    return fun x y -> InfixApp (x, "/", y) }
let appOp = parser { return fun x y -> App (x, y) }

let integerp = parser { let! x = integer
                      return Lit(Integer x) }

let rec atom = local +++ lam +++ var +++ integerp +++ paren

and exp = chainl1Line item (appOp)
and item = chainl1 term (addOp +++ subOp)
and term = chainl1 atom (mulOp +++ divOp)

and lam = parser { let! _ = symbol "fun"
                   let! x = variable
                   let! _ = symbol "->"
                   let! e = exp 
                   return Lam (seqtostring x, e) }

and local = parser { let! _ = symbol "let"
                     let! x = variable
                     let! _ = symbol "="
                     let! e = exp
                     let! e' = localExp +++ (off exp)
                     return Let (seqtostring x, e, e') }

and localExp = parser { let! _ = symbol "in"
                        let! e = exp
                        return e } 

and var = parser { let! x = variable
                  return Var (seqtostring x) }

and paren = parser { let! _ = symbol "("
                     let! e = exp
                     let! _ = symbol ")" 
                     return e }

and variable = identifier ["let";"in"]

let parseExp s = match (parse exp (0,1) (PString((0,-1), s))) with
                 | [] -> None
                 | [(exp, _)] -> Some exp         