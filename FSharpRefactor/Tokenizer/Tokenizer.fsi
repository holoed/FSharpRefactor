module Tokenizer

type Token = | Identifier of string
             | IntegerLiteral of int
             | StringLiteral of string
             | SymbolOp of string
             | Keyword of string

val tokenize : seq<char> -> Token seq option