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

  