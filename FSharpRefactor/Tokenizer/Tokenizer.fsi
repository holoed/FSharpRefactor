module Tokenizer

type SrcLoc = { srcFilename : string; srcLine : int; srcColumn : int }   

type Token = | Identifier of string
             | IntegerLiteral of int
             | StringLiteral of string
             | SymbolOp of string
             | Keyword of string

val tokenize : seq<char> -> Token seq option