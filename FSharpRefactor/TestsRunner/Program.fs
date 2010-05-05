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