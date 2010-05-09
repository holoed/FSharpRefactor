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

module CodeParser

open System
open StringUtils
open SharpMalib.Parser.ParserMonad
open Tokens 
open Ast                      

let chainl1' (p:Parser<Exp * SrcLoc, Token>) op : Parser<Exp*SrcLoc, Token>=         
        let rec rest (l:Exp, p1) = parser { let! f = op
                                            let! (r, p2) = p 
                                            if (p1.srcColumn < p2.srcColumn) then
                                               return! rest (f (l, p1) (r, p2)) } +++ parser { return (l, p1) }
        parser { let! (l, p1) = p
                 return! rest (l, p1) }


let keyword s = parser { let! Token(_,p) = sat (fun (Token (t, p)) -> match t with
                                                                      | Keyword(s') when s = s' -> true
                                                                      | _ -> false)
                         return  s, p }
                            
let literal = parser { let! x = item
                       return! match x with
                               | Token(IntegerLiteral v, p) -> result (Lit (Integer (System.Int32.Parse v)), p)
                               | _ -> zero }

let identifier = parser { let! x = item
                          return! match x with
                                  | Token(Identifier v, p) -> result (Ident v, p)
                                  | _ -> zero }

let variable = parser { let! (n, p) = identifier
                        return (Var n), p }

let symbol s = parser { let! Token(_, p) = sat (fun t -> match t with
                                                         | Token (Tokens.Symbol s', p) when s = s' -> true
                                                         | _ -> false)
                        return VarOp (Symbol s), p }

let addOp = parser { let! (x, p) = symbol "+"
                     return fun (l, _) (r, _) -> InfixApp (l,x,r), p }
let subOp = parser { let! (x, p) = symbol "-"
                     return fun (l, _) (r, _) -> InfixApp (l,x,r), p }
let mulOp = parser { let! (x, p) = symbol "*"
                     return fun (l, _) (r, _) -> InfixApp (l,x,r), p }
let divOp = parser { let! (x, p) = symbol "/"
                     return fun (l, _) (r, _) -> InfixApp (l,x,r), p }


let rec factor = valueBinding +++ lambda +++ lookUp +++ tuple +++ literal +++ variable +++ parens

and parens = parser { let! _ = symbol "("
                      let! n = expr
                      let! _ = symbol ")"
                      return n }

and expr = chainl1 term (addOp +++ subOp)
and term = chainl1 app (mulOp +++ divOp)
and app = chainl1' factor (parser {  return fun (e1, p1) (e2, p2) -> App(e1, e2), p1 })


and varPat = parser { let! (x, p) = identifier
                      return PVar x }

and funPat = parser { let! name, _ = identifier
                      let! args = many1 pattern
                      return PApp (name, Seq.toList args) }

and pattern = funPat +++ tuplePat +++ varPat

and tuplePat = parser { let! _ = symbol "("
                        let! exprs = sepBy1 pattern (symbol ",") 
                        let! _ = symbol ")"
                        let ret = Seq.toList exprs
                        if (ret.Length > 1) then 
                           return PTuple (ret) }

and valueBinding = parser { let! (_, p) = keyword "let"
                            let! pat = pattern                                 
                            let! _ = symbol "="
                            let! (value, _) = expr
                            return Let (pat, value), p }

and lookUp = parser { let! e, p = variable
                      let! _ = symbol "."
                      let! name, _ = identifier
                      return LookUp (e, name), p }

and tuple = parser { let! _, p = symbol "("
                     let! exprs = sepBy1 expr (symbol ",") 
                     let! _ = symbol ")"
                     let ret = exprs |> Seq.map (fun (x, _) -> x) |> Seq.toList
                     if (ret.Length > 1) then 
                        return Tuple ret, p }

and lambda = parser { let! (_, p) = keyword "fun"
                      let! pats = many1 (varPat +++ tuplePat)
                      let! _ = symbol "->"
                      let! (body, _) = expr
                      return Lambda (Seq.toList pats, body), p }

let exprs = parser { let! xs = many expr
                     if (Seq.length xs > 1) then
                        let pos = (Seq.head xs) |> snd
                        return Code (xs |> Seq.map (fun (x, _) -> x) 
                                        |> Seq.toList), pos }

let parseCode (xs: seq<Token>) : Exp option =
    match parseString (exprs +++ expr) xs with
    | Empty -> None
    | Cons((x, _), _) -> Some x