module TokenizerTests

open Tokenizer
open System.Collections
open NUnit.Framework

[<TestFixture>]
type TokenizerTests() =
    
    [<Test>]
    member this.Words() = 
        match tokenize "Hello World" with
        | None -> Assert.Fail ()
        | Some x -> CollectionAssert.AreEqual (seq [Token "Hello"; Token "World"] , x, sprintf "Actual: %A" x)