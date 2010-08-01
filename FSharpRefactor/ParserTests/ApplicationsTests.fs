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

module ApplicationsTests 

open NUnit.Framework
open Ast
open FSharpParser

[<TestFixture>]
type ApplicationsTests() =

    [<Test>]
    member this.ApplicationsAssociatesToTheLeft() =
        Assert.IsTrue(Some (App(Var "f", Var "g")) = parseExp "f g")
        Assert.IsTrue(Some (App(App(Var "f", Var "g"), Var "h")) = parseExp "f g h")
        Assert.IsTrue(Some (App(App(App(Var "f", Var "g"), Var "h"), Var "j")) = parseExp "f g h j")

    [<Test>]
    member this.ParensToModifyAssociation() =
        Assert.IsTrue(Some (App(Var "f", App(Var "g", Var "h"))) = parseExp "f (g h)")
        Assert.IsTrue(Some (App(Var "f", App(Var "g", App(Var "h", Var "j")))) = parseExp "f (g (h j))")

    [<Test>]
    member this.Currying() = 
        Assert.IsTrue (Some (Let (PVar "squares", App (App(Var "map", Var "square"), Var "numbers"), Var "squares")) = parseExp "let squares = map square numbers")

  