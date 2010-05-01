module Parser

open StringUtils
open SharpMalib.Parser.ParserMonad
open Tokenizer      

type Name
    = Ident of string   
    | Symbol of string 

type Type =   
     | TyFun of Type * Type     // function type
     | TyTuple of Type list     // tuple type
     | TyList of  Type          // list syntax, e.g. [a], as opposed to [] a
     | TyApp of Type * Type     // application of a type constructor
     | TyVar of  Name           // type variable
     | TyCon of  Name           // named type or type constructor
     | TyParen of Type          // type surrounded by parentheses
     | TyInfix of Type * Name * Type  // infix type constructor

type Op
    = VarOp of Name  // variable operator (/qvarop/)
    | ConOp of Name  // constructor operator (/qconop/)

type Literal
    = Char  of  char          // character literal
    | String of string        // string literal
    | Integer of  int         // integer literal
    | Frac  of  float         // floating point literal    

type Pat
    = PVar of Name                // variable
    | PLit of Literal             // literal constant
    | PApp of Name * Pat list     // type constructor and argument patterns
    | PTuple of Pat list          // tuple pattern
    | PList of Pat list           // list pattern
    | PParen of Pat               // parenthesized pattern
    | PWildCard                   // wildcard pattern: @_@

type Exp
    = Var of Name                           // variable
    | Con of Name                           // type constructor
    | Lit of Literal                        // literal constant
    | InfixApp of Exp * Op * Exp            // infix application
    | App of Exp * Exp                      // ordinary application
    | Lambda of Pat list * Exp              // lambda expression
    | Let of Name * Exp                     // local declarations with @let@ ... @in@ ...
    | If of Exp * Exp * Exp                 // @if@ /exp/ @then@ /exp/ @else@ /exp/
    | Match of Exp * Alt list               // @case@ /exp/ @of@ /alts/
    | Tuple of Exp list                     // tuple expression
    | List of Exp list                      // list expression
    | Paren of Exp                          // parenthesised expression

and Alt
    = Alt of Pat * Exp                   

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

let symbol s = parser { let! _ = sat (fun t -> t = SymbolOp s)
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

let rec valueBinding = parser { let! _ = keyword "let"
                                let! ident = identifier                                 
                                let! _ = symbol "="
                                let! value = exp
                                return Let (ident, value) }

and exp = app +++ expr +++ variable +++ literal

and code = valueBinding +++ exp

and app = parser { let! e1 = variable
                   let! e2 = variable
                   return App (e1, e2) }

let parseCode (xs: seq<Token>) : Exp option =
    match parseString code xs with
    | Empty -> None
    | Cons(x, _) -> Some x