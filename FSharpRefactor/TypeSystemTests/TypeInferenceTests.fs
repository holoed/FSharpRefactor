﻿module TypeInferenceTests

open NUnit.Framework
open CodeParser
open Tokenizer
open TypeInference



//[<TestFixture>]
//type TypeInferenceTests() =
//    
//    [<Test>]
//    member this.Integer() = 
//        let x = Literal (IntegerLiteral 42)
//        let y = Literal (IntegerLiteral 42, Int)
//        let success = infer x = y
//        Assert.IsTrue success

