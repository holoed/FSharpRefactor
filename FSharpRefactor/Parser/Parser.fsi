module Parser

open Tokens
open Ast
              
val parseCode : seq<Token> -> Exp option