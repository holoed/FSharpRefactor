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
        assertTest (seq [Token (Identifier "x", zeroLoc)]) "x" 
        assertTest (seq [Token (Identifier "x'", zeroLoc)]) "x'" 
        assertTest (seq [Token (Identifier "_x", zeroLoc)]) "_x"        
        assertTest (seq [Token (Identifier "x42", zeroLoc)]) "x42" 
        assertTest (seq [Token (Identifier "_x42'", zeroLoc)]) "_x42'" 

    [<Test>]
    member this.Words() = 
        assertTest (seq [Token (Identifier "Hello", zeroLoc); Token (Identifier "World", { zeroLoc with srcColumn = 6})]) "Hello World" 

    [<Test>]
    member this.Number() = 
        assertTest (seq [Token (IntegerLiteral "42", zeroLoc)]) "42" 

    [<Test>]
    member this.String() = 
        assertTest (seq [Token (StringLiteral "Hello World", { zeroLoc with srcColumn = 1 })]) "\"Hello World\""

    [<Test>]
    member this.Symbols() = 
        assertTest (seq 
        [Token (Symbol "=", zeroLoc);
         Token (Symbol "/", { zeroLoc with srcColumn = 2 }); 
         Token (Symbol "*", { zeroLoc with srcColumn = 4 }); 
         Token (Symbol "+", { zeroLoc with srcColumn = 6 });
         Token (Symbol ">", { zeroLoc with srcColumn = 8 }); 
         Token (Symbol "<", { zeroLoc with srcColumn = 10 }); 
         Token (Symbol "(", { zeroLoc with srcColumn = 12 }); 
         Token (Symbol ")", { zeroLoc with srcColumn = 14 });
         Token (Symbol ",", { zeroLoc with srcColumn = 16 });
         Token (Symbol ".", { zeroLoc with srcColumn = 18 });
         Token (Symbol "->",{ zeroLoc with srcColumn = 20 })]) "= / * + > < ( ) , . ->"

    [<Test>]
    member this.Keyword() = 
        assertTest (seq [Token (Keyword "let", zeroLoc); 
                         Token (Keyword "in", { zeroLoc with srcColumn = 4 })]) "let in"

    [<Test>]
    member this.Binding() = 
        assertTest (seq [Token (Keyword "let", zeroLoc); 
                         Token (Identifier "x", { zeroLoc with srcColumn = 4 }); 
                         Token (Symbol "=", { zeroLoc with srcColumn = 6 }); 
                         Token (IntegerLiteral "42", { zeroLoc with srcColumn = 8 })]) "let x = 42"

    [<Test>]
    member this.FunctionBinding() = 
        assertTest (seq [Token (Keyword "let", zeroLoc); 
                         Token (Identifier "f", { zeroLoc with srcColumn = 4 }); 
                         Token (Identifier "x", { zeroLoc with srcColumn = 6 }); 
                         Token (Symbol "=", { zeroLoc with srcColumn = 8 }); 
                         Token (Identifier "x", { zeroLoc with srcColumn = 10 })]) "let f x = x"

    [<Test>]
    member this.FunctionBinding2() = 
        assertTest (seq [Token (Keyword "let", zeroLoc); 
                         Token (Identifier "square", { zeroLoc with srcColumn = 4 }); 
                         Token (Identifier "x", { zeroLoc with srcColumn = 11 }); 
                         Token (Symbol "=", { zeroLoc with srcColumn = 13 }); 
                         Token (Identifier "x", { zeroLoc with srcColumn = 15 }); 
                         Token (Symbol "*", { zeroLoc with srcColumn = 17 }); 
                         Token (Identifier "x", { zeroLoc with srcColumn = 19 })]) "let square x = x * x"
