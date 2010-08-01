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

module LambdaTests 

open NUnit.Framework
open Ast
open FSharpParser

[<TestFixture>]
type LambdaTests() =

    [<Test>]
    member this.Lambdas() =
        Assert.IsTrue(Some (Lam("x", Var "x")) = parseExp "fun x -> x")
        Assert.IsTrue(Some (Lam("x", Lam("y", InfixApp(Var "x", "+", Var "y")))) = parseExp "fun x -> fun y -> x + y")
        
    

  