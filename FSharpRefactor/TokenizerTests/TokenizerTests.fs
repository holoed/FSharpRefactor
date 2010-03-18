module TokenizerTests

open Tokenizer
open System.Collections
open NUnit.Framework

let assertTest expected input = 
  match tokenize input with
        | None -> Assert.Fail ()
        | Some x -> CollectionAssert.AreEqual (expected , x, sprintf "Actual: %A" x)

[<TestFixture>]
type TokenizerTests() =
    
    [<Test>]
    member this.Words() = 
        assertTest (seq [Identifier "Hello"; Identifier "World"]) "Hello World" 

    [<Test>]
    member this.Number() = 
        assertTest (seq [IntegerLiteral 42]) "42" 

    [<Test>]
    member this.String() = 
        assertTest (seq [StringLiteral "Hello World"]) "\"Hello World\""

    [<Test>]
    member this.Symbols() = 
        assertTest (seq [SymbolOp "="; SymbolOp "/"; SymbolOp "*"; SymbolOp "+"; SymbolOp ">"; SymbolOp "<"]) "= / * + > <"

    [<Test>]
    member this.Keyword() = 
        assertTest (seq [Keyword "let"; Keyword "in"]) "let in"

    [<Test>]
    member this.Binding() = 
        assertTest (seq [Keyword "let"; Identifier "x"; SymbolOp "="; IntegerLiteral 42]) "let x = 42"
