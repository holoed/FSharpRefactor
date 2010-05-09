module Ast

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
    | LookUp of Exp * Name                  // lookup List.map
    | InfixApp of Exp * Op * Exp            // infix application
    | App of Exp * Exp                      // ordinary application
    | Lambda of Pat list * Exp              // lambda expression
    | Let of Pat * Exp                     // local declarations with @let@ ... @in@ ...
    | If of Exp * Exp * Exp                 // @if@ /exp/ @then@ /exp/ @else@ /exp/
    | Match of Exp * Alt list               // @case@ /exp/ @of@ /alts/
    | Tuple of Exp list                     // tuple expression
    | List of Exp list                      // list expression
    | Paren of Exp                          // parenthesised expression
    | Code of Exp list

and Alt
    = Alt of Pat * Exp