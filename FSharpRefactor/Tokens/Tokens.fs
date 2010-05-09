// * **********************************************************************************************
// * Copyright (c) Edmondo Pentangelo. 
// *
// * This source code is subject to terms and conditions of the Microsoft Public License. 
// * A copy of the license can be found in the License.html file at the root of this distribution. 
// * By using this source code in any fashion, you are agreeing to be bound by the terms of the 
// * Microsoft Public License.
// *
// * You must not remove this notice, or any other, from this software.
// * **********************************************************************************************

module Tokens

type SrcLoc = { srcLine : int; srcColumn : int }   

type TokenKind = | Identifier of string
                 | IntegerLiteral of string
                 | StringLiteral of string
                 | Symbol of string
                 | Keyword of string

type Token = Token of TokenKind * SrcLoc

let zeroLoc = { srcLine = 0; srcColumn = 0 }