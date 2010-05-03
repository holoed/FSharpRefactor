// Learn more about F# at http://fsharp.net

open TokenizerTests
open ParserTests

//let tokenizerTests = new TokenizerTests()
let parserTests = new ParserTests()

//tokenizerTests.Symbols ()

parserTests.FunctionApplicationIsLeftAssociative ()

//let ast = ParserTests.parse "let product = List.fold (fun x y -> x * y) 1"

//ast.ToString()