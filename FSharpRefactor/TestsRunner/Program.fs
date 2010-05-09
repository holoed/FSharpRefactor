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

// Learn more about F# at http://fsharp.net

//open TokenizerTests
//open CodeParserTests

//let tokenizerTests = new TokenizerTests()
//let parserTests = new CodeParserTests()

//tokenizerTests.Symbols ()

//parserTests.FunctionApplicationIsLeftAssociative ()

//let ast = CodeParserTests.parse "let product = List.fold (fun x y -> x * y) 1"

//ast.ToString()

open Tokens
open Tokenizer

let ret = tokenize @"let x = 42"


let x = ret |> Option.get |> Seq.toArray

ret.ToString()