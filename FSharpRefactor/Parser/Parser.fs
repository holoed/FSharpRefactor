module Parser

open StringUtils
open SharpMalib.Parser.ParserMonad
open Tokens 
open Ast                      

let keyword s = parser { let! _ = sat (fun t -> t = Keyword s)
                         return s }
                            
let literal = parser { let! x = item
                       return! match x with
                               | IntegerLiteral v -> result (Lit (Integer v))
                               | _ -> zero }

let identifier = parser { let! x = item
                          return! match x with
                                  | Identifier v -> result (Ident v)
                                  | _ -> zero }

let variable = parser { let! x = identifier
                        return Var x }

let symbol s = parser { let! _ = sat (fun t -> t = Tokens.Symbol s)
                        return VarOp (Symbol s) }

let addOp = parser { let! x = symbol "+"
                     return fun l r -> InfixApp (l,x,r) }
let subOp = parser { let! x = symbol "-"
                     return fun l r -> InfixApp (l,x,r) }
let mulOp = parser { let! x = symbol "*"
                     return fun l r -> InfixApp (l,x,r) }
let divOp = parser { let! x = symbol "/"
                     return fun l r -> InfixApp (l,x,r) }


let rec factor = literal +++ variable +++ parser { let! _ = symbol "("
                                                   let! n = expr
                                                   let! _ = symbol ")"
                                                   return n }
and expr = chainl1 term (addOp +++ subOp)
and term = chainl1 factor (mulOp +++ divOp)

let varPat = parser { let! x = identifier
                      return PVar x }

let rec funPat = parser { let! name = identifier
                          let! args = many1 pattern
                          return PApp (name, Seq.toList args) }

and pattern = funPat +++ tuplePat +++ varPat

and tuplePat = parser { let! _ = symbol "("
                        let! exprs = sepBy pattern (symbol ",") 
                        let! _ = symbol ")"
                        return PTuple (Seq.toList exprs) }

let rec valueBinding = parser { let! _ = keyword "let"
                                let! pat = pattern                                 
                                let! _ = symbol "="
                                let! value = exp
                                return Let (pat, value) }

and exp = lambda +++ tuple +++ app +++ expr +++ variable +++ literal

and code = valueBinding +++ exp

and app = parser { let! e1 = variable
                   let! e2 = variable
                   return App (e1, e2) }

and tuple = parser { let! _ = symbol "("
                     let! exprs = sepBy exp (symbol ",") 
                     let! _ = symbol ")"
                     return Tuple (Seq.toList exprs) }

and lambda = parser { let! _ = keyword "fun"
                      let! pats = many1 pattern
                      let! _ = symbol "->"
                      let! body = exp
                      return Lambda (Seq.toList pats, body) }

let parseCode (xs: seq<Token>) : Exp option =
    match parseString code xs with
    | Empty -> None
    | Cons(x, _) -> Some x