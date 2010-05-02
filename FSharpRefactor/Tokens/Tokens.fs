module Tokens

type SrcLoc = { srcFilename : string; srcLine : int; srcColumn : int }   

type Token = | Identifier of string
             | IntegerLiteral of int
             | StringLiteral of string
             | Symbol of string
             | Keyword of string