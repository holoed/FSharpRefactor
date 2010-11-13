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
        Assert.IsTrue(Some (Let(PVar "x", Lit(Integer 42), Var "x")) = parseExp "let x = 42 in x") 
        Assert.IsTrue(Some (Let(PVar "x", Lit(Integer 42), Var "x")) = parseExp "let x = 42 in x")
        Assert.IsTrue(Some (Let(PVar "x", Lit(Integer 42), InfixApp(Var "x", "*", Var "x"))) = parseExp "let x = 42 in x * x")

    [<Test>]
    member this.LocalFunctionDefinitions() =
        Assert.IsTrue(Some (Let(PApp(PVar "f", PVar "x"), Var "x", Var "f")) = parseExp "let f x = x")
        Assert.IsTrue(Some (Let(PApp(PVar "g", PVar "x"), InfixApp(Var "x", "*", Var "x"), Var "g")) = parseExp "let g x = x * x")

    [<Test>]
    member this.NestedLocalDefinitions() = 
        Assert.IsTrue(Some (Let(PVar "b", Lit(Integer 4), Let(PVar "b", InfixApp(Var "b", "+", Lit(Integer 1)), Var "b")))  = 
             parseExp "let b = 4 in let b = b + 1 in  b")    
        Assert.IsTrue(Some (Let(PVar "z", Let(PVar "x", Lit(Integer 42), Var "x"), Var "z")) = 
             parseExp "let z = let x = 42 in x in z")    

    [<Test>]
    member this.MultilineDef() = 
        Assert.IsTrue(Some (Let(PVar "z", Let(PVar "x", Lit(Integer 42), Var "x"), Var "z")) = 
             parseExp ("let z =                           \n" +
                       "    let x = 42 in x in z          \n"))    
    

    [<Test>]
    member this.OffSideLocalDefinitions() =
        let k = Some (Let(PVar "x", Lit(Integer 12), Let(PVar "y", Lit(Integer 32), InfixApp(Var "x", "+", Var "y"))))

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
        let k = Some (Let (PVar "z", 
                                Let(PVar "x", 
                                    Lit(Integer 12), 
                                    Let(PVar "y", 
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


    [<Test>]
    member this.OffSideLocalDefinition3() =
        
        let x = parseExp ("let computeDerivative f x = \n" +
                          "    let p1 = f (x - 0.05)   \n" +
                          "    let p2 = f (x + 0.05)   \n" +
                          "    (p2 - p1) / 0.1           ")
                           
        let y = Some(
                    Let(
                        PApp(PApp(PVar "computeDerivative", PVar "f"), PVar "x"), 
                            Let(PVar "p1", App(Var "f", InfixApp(Var "x", "-", Lit(Float(0.05)))), 
                                Let(PVar "p2", App(Var "f", InfixApp(Var "x", "+", Lit(Float(0.05)))), 
                                    InfixApp(InfixApp(Var "p2", "-", Var "p1"), "/", Lit(Float(0.1))))), Var "computeDerivative"))

        Assert.IsTrue ( (x = y) )

    

  