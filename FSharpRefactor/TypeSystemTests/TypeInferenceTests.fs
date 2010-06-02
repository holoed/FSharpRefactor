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

module TypeInferenceTests

open NUnit.Framework
open Tokenizer
open CodeParser
open Ast
open TypeInference
open Utils

[<TestFixture>]
type TypeInferenceTests() =

    let infer x = option { let! tok = tokenize x
                           let! ast = parseCode tok
                           let! typ = typeInfer ast
                           return typ } 
    
    [<Test>]
    [<Ignore("Continue from here...")>]
    member this.Infer1() = 
        Assert.IsTrue (match (infer "let x = 1") with
                       | None -> false
                       | Some x -> x = Type(Int))

    [<Test>]
    [<Ignore("Continue from here...")>]
    member this.Infer2() = 
        Assert.IsTrue (match (infer "let f x = x + 1") with
                       | None -> false
                       | Some x -> x = TyFun (Type Int, Type Int))

