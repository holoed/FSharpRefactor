module Tokens

type SrcLoc = { srcLine : int; srcColumn : int }   

type Token = | Identifier of string * SrcLoc
             | IntegerLiteral of string * SrcLoc
             | StringLiteral of string * SrcLoc
             | Symbol of string * SrcLoc
             | Keyword of string * SrcLoc

let zeroLoc = { srcLine = 0; srcColumn = 0 }