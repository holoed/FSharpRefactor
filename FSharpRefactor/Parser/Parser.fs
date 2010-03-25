module Parser

open StringUtils
open SharpMalib.Parser.ParserMonad
open Tokenizer

type Expression = | Literal of Token   
                  | Name of Token            
                  | Binding of Expression * Expression
                  | BinaryExpression of Token * Expression * Expression

let keyword s = parser { let! _ = sat (fun t -> t = Keyword s)
                         return s }
                            
let literal = parser { let! x = item
                       return! match x with
                               | IntegerLiteral _ -> result (Literal x)
                               | _ -> zero }

let identifier = parser { let! x = item
                          return! match x with
                                  | Identifier _ -> result (Name x)
                                  | _ -> zero }

let symbol s = parser { let! x = sat (fun t -> t = SymbolOp s)
                        return x }

let addOp = parser { let! x = symbol "+"
                     return fun l r -> BinaryExpression (x,l,r) }
let subOp = parser { let! x = symbol "-"
                     return fun l r -> BinaryExpression (x,l,r) }
let mulOp = parser { let! x = symbol "*"
                     return fun l r -> BinaryExpression (x,l,r) }
let divOp = parser { let! x = symbol "/"
                     return fun l r -> BinaryExpression (x,l,r) }


let rec factor = literal +++ parser { let! _ = symbol "("
                                      let! n = expr
                                      let! _ = symbol ")"
                                      return n }
and expr = chainl1 term (addOp +++ subOp)
and term = chainl1 factor (mulOp +++ divOp)



let rec binding = parser { let! _ = keyword "let"
                           let! name = identifier 
                           let! _ = symbol "="
                           let! value = exp
                           return Binding (name, value) }

and exp = expr +++ identifier +++ literal +++ binding

let parseCode (xs: seq<Token>) : Expression option =
    match parseString binding xs with
    | Empty -> None
    | Cons(x, _) -> Some x