module Tokens

type SrcLoc = { srcLine : int; srcColumn : int }   

type TokenKind = | Identifier of string
                 | IntegerLiteral of string
                 | StringLiteral of string
                 | Symbol of string
                 | Keyword of string

type Token = Token of TokenKind * SrcLoc

let zeroLoc = { srcLine = 0; srcColumn = 0 }