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

let rec extractBindName x = match x with 
                            | PVar s -> Var s 
                            | PApp (x', _) -> extractBindName x'

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

let floatp = parser { let! x = float
                      return Lit(Float x) }

// expr::= expr+ term| expr–term| term
// term::= term* factor| term/ factor| factor
// factor::= int| ( expr)
// int::= 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7

let rec prog = decl +++ expr
and expr = chainl1 term (addOp +++ subOp)
and term = chainl1 factor (mulOp +++ divOp)
and factor =  chainl1Line atom appOp
and atom =  lam +++ var +++ floatp +++ integerp +++ paren

and lam = parser { let! _ = symbol "fun"
                   let! x = many1 pattern
                   let! _ = symbol "->"
                   let! e = expr 
                   return Lam (Seq.toList x, e) }

and decl = parser {  let! _ = symbol "let"
                     let! x = pattern
                     let! _ = symbol "="
                     let! e = prog
                     let! e' = localExp +++ (off prog) +++ parser { return extractBindName x } 
                     return Let (x, e, e') }

and localExp = parser { let! _ = symbol "in"
                        let! e = prog
                        return e } 

and var = parser { let! x = variable
                   return Var (seqtostring x) }

and paren = parser { let! _ = symbol "("
                     let! e = prog
                     let! _ = symbol ")" 
                     return e }

and variable = identifier ["let";"in"]

and pvar = parser { let! x = variable
                   return PVar (seqtostring x) }

and pvarAnnotated = parser { let! _ = symbol "("
                             let! x = pvar
                             let! _ = symbol ":"
                             let! t = ident
                             let! _ = symbol ")" 
                             return PWithTy (x, TyCon(seqtostring t, [])) }

and papp = chainl1 (pvarAnnotated +++ pvar) (parser { return fun x y -> PApp (x, y) })            

and pattern = papp +++ pvar +++ pvarAnnotated

let parseExp s = match (parse prog (0,1) (PString((0,-1), s))) with
                 | [] -> None
                 | [(exp, _)] -> Some exp         