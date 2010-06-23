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

module LocalDefinitions

open System
open NUnit.Framework
open Ast
open FSharpParser

[<TestFixture>]
type LocalDefinitionTests() =

    [<Test>]
    member this.LocalDefinitions() =
        Assert.IsTrue(Some (Let("x", [Lit(Integer 42)], Var "x")) = parseExp "let x = 42 in x")
        Assert.IsTrue(Some (Let("x", [Lit(Integer 42)], Var "x")) = parseExp "let x = 42 in x")

    [<Test>]
    member this.NestedLocalDefinitions() = 
        Assert.IsTrue(Some (Let("b", [Lit(Integer 4)], Let("b", [InfixApp(Var "b", "+", Lit(Integer 1))], Var "b")))  = 
             parseExp "let b = 4 in let b = b + 1 in  b")    
        Assert.IsTrue(Some (Let("z", [Let("x", [Lit(Integer 42)], Var "x")], Var "z")) = 
             parseExp "let z = let x = 42 in x in z")    

    [<Test>]
    member this.MultilineDef() = 
        Assert.IsTrue(Some (Let("z", [Let("x", [Lit(Integer 42)], Var "x")], Var "z")) = 
             parseExp ("let z =                           \n" +
                       "    let x = 42 in x in z          \n"))    
    

//    [<Test>]
//    member this.OffSideLocalDefinitions() =
//        let ret = parseExp "let z =                      " + Environment.NewLine +
//                           "    let x = 42               " + Environment.NewLine +
//                           "    let y = 12 in x + y      "
    

  