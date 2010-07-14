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
        Assert.IsTrue(Some (Let("x", Lit(Integer 42), Var "x")) = parseExp "let x = 42 in x")
        Assert.IsTrue(Some (Let("x", Lit(Integer 42), Var "x")) = parseExp "let x = 42 in x")

    [<Test>]
    member this.NestedLocalDefinitions() = 
        Assert.IsTrue(Some (Let("b", Lit(Integer 4), Let("b", InfixApp(Var "b", "+", Lit(Integer 1)), Var "b")))  = 
             parseExp "let b = 4 in let b = b + 1 in  b")    
        Assert.IsTrue(Some (Let("z", Let("x", Lit(Integer 42), Var "x"), Var "z")) = 
             parseExp "let z = let x = 42 in x in z")    

    [<Test>]
    member this.MultilineDef() = 
        Assert.IsTrue(Some (Let("z", Let("x", Lit(Integer 42), Var "x"), Var "z")) = 
             parseExp ("let z =                           \n" +
                       "    let x = 42 in x in z          \n"))    
    

    [<Test>]
    member this.OffSideLocalDefinitions() =
        let k = Some (Let("x", Lit(Integer 12), Let("y", Lit(Integer 32), InfixApp(Var "x", "+", Var "y"))))

        let x = parseExp "let x = 12 in let y = 32 in x + y"

        let y = parseExp ("let x = 12 in " + 
                          "let y = 32 in " +
                          "x + y         ");

        let z = parseExp ("let x = 12 \n " + 
                          "let y = 32 \n " +
                          "x + y         ");                    
        Assert.IsTrue (k = x && x = y && y = z)

    [<Test>]
    member this.OffSideLocalDefinitions2() =
        let k = Some (Let ("z", 
                                Let("x", 
                                    Lit(Integer 12), 
                                    Let("y", 
                                        Lit(Integer 32), 
                                        InfixApp (Var "x", "+", Var "y"))), 
                                Var "z"))

        let x = parseExp "let z = (let x = 12 in let y = 32 in x + y) in z"

        let y = parseExp ("let z =       \n " + 
                          "   let x = 12 \n " +
                          "   let y = 32 \n " +
                          "   x + y         ")     
        Assert.IsTrue ( (k = x) )                                       
        Assert.IsTrue ( (x = y) )

    

  