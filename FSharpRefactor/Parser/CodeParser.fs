module CodeParser

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


let rec factor = valueBinding +++ lambda +++ lookUp +++ tuple +++ literal +++ variable +++ parens

and parens = parser { let! _ = symbol "("
                      let! n = expr
                      let! _ = symbol ")"
                      return n }

and expr = chainl1 term (addOp +++ subOp)
and term = chainl1 app (mulOp +++ divOp)
and app = chainl1 factor (parser { return fun e1 e2 -> App(e1, e2) })


and varPat = parser { let! x = identifier
                      return PVar x }

and funPat = parser { let! name = identifier
                      let! args = many1 pattern
                      return PApp (name, Seq.toList args) }

and pattern = funPat +++ tuplePat +++ varPat

and tuplePat = parser { let! _ = symbol "("
                        let! exprs = sepBy1 pattern (symbol ",") 
                        let! _ = symbol ")"
                        let ret = Seq.toList exprs
                        if (ret.Length > 1) then 
                           return PTuple (ret) }

and valueBinding = parser { let! _ = keyword "let"
                            let! pat = pattern                                 
                            let! _ = symbol "="
                            let! value = expr
                            return Let (pat, value) }

and lookUp = parser { let! e = variable
                      let! _ = symbol "."
                      let! name = identifier
                      return LookUp (e, name) }

and tuple = parser { let! _ = symbol "("
                     let! exprs = sepBy1 expr (symbol ",") 
                     let! _ = symbol ")"
                     let ret = Seq.toList exprs
                     if (ret.Length > 1) then 
                        return Tuple ret }

and lambda = parser { let! _ = keyword "fun"
                      let! pats = many1 (varPat +++ tuplePat)
                      let! _ = symbol "->"
                      let! body = expr
                      return Lambda (Seq.toList pats, body) }

let parseCode (xs: seq<Token>) : Exp option =
    match parseString expr xs with
    | Empty -> None
    | Cons(x, _) -> Some x