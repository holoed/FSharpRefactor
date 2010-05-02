module Parser

open Tokenizer
open Ast
              
val parseCode : seq<Token> -> Exp option