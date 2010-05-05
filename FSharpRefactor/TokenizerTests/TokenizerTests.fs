module TokenizerTests

open Tokens
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
    member this.Identifiers() = 
        assertTest (seq [Identifier ("x", zeroLoc)]) "x" 
        assertTest (seq [Identifier ("x'", zeroLoc)]) "x'" 
        assertTest (seq [Identifier ("_x", zeroLoc)]) "_x"        
        assertTest (seq [Identifier ("x42", zeroLoc)]) "x42" 
        assertTest (seq [Identifier ("_x42'", zeroLoc)]) "_x42'" 

    [<Test>]
    member this.Words() = 
        assertTest (seq [Identifier ("Hello", zeroLoc); Identifier ("World", { zeroLoc with srcColumn = 6})]) "Hello World" 

    [<Test>]
    member this.Number() = 
        assertTest (seq [IntegerLiteral ("42", zeroLoc)]) "42" 

    [<Test>]
    member this.String() = 
        assertTest (seq [StringLiteral ("Hello World", { zeroLoc with srcColumn = 1 })]) "\"Hello World\""

    [<Test>]
    member this.Symbols() = 
        assertTest (seq 
        [Symbol ("=", zeroLoc);
         Symbol ("/", { zeroLoc with srcColumn = 2 }); 
         Symbol ("*", { zeroLoc with srcColumn = 4 }); 
         Symbol ("+", { zeroLoc with srcColumn = 6 });
         Symbol (">", { zeroLoc with srcColumn = 8 }); 
         Symbol ("<", { zeroLoc with srcColumn = 10 }); 
         Symbol ("(", { zeroLoc with srcColumn = 12 }); 
         Symbol (")", { zeroLoc with srcColumn = 14 });
         Symbol (",", { zeroLoc with srcColumn = 16 });
         Symbol (".", { zeroLoc with srcColumn = 18 });
         Symbol ("->",{ zeroLoc with srcColumn = 20 })]) "= / * + > < ( ) , . ->"

    [<Test>]
    member this.Keyword() = 
        assertTest (seq [Keyword ("let", zeroLoc); 
                         Keyword ("in", { zeroLoc with srcColumn = 4 })]) "let in"

    [<Test>]
    member this.Binding() = 
        assertTest (seq [Keyword ("let", zeroLoc); 
                         Identifier ("x", { zeroLoc with srcColumn = 4 }); 
                         Symbol ("=", { zeroLoc with srcColumn = 6 }); 
                         IntegerLiteral ("42", { zeroLoc with srcColumn = 8 })]) "let x = 42"

    [<Test>]
    member this.FunctionBinding() = 
        assertTest (seq [Keyword ("let", zeroLoc); 
                         Identifier ("f", { zeroLoc with srcColumn = 4 }); 
                         Identifier ("x", { zeroLoc with srcColumn = 6 }); 
                         Symbol ("=", { zeroLoc with srcColumn = 8 }); 
                         Identifier ("x", { zeroLoc with srcColumn = 10 })]) "let f x = x"

    [<Test>]
    member this.FunctionBinding2() = 
        assertTest (seq [Keyword ("let", zeroLoc); 
                         Identifier ("square", { zeroLoc with srcColumn = 4 }); 
                         Identifier ("x", { zeroLoc with srcColumn = 11 }); 
                         Symbol ("=", { zeroLoc with srcColumn = 13 }); 
                         Identifier ("x", { zeroLoc with srcColumn = 15 }); 
                         Symbol ("*", { zeroLoc with srcColumn = 17 }); 
                         Identifier ("x", { zeroLoc with srcColumn = 19 })]) "let square x = x * x"
