module CodeParser

open Tokens
open Ast
              
val parseCode : seq<Token> -> Exp option