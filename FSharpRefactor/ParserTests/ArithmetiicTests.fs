module ArithmetiicTests

open NUnit.Framework
open Ast
open FSharpParser

[<TestFixture>]
type ArithmeticTests() =

    [<Test>]
    member this.Integer() =
        Assert.IsTrue(Some (Lit (Integer 42)) = parseExp "42")
        Assert.IsTrue(Some (Lit (Integer -42)) = parseExp "-42")

    [<Test>]
    member this.Sum() =
        Assert.IsTrue(Some (InfixApp (Lit (Integer 42),"+",Lit (Integer 24))) = parseExp "42 + 24")

    [<Test>]
    member this.Sub() =
        Assert.IsTrue(Some (InfixApp (Lit (Integer 42),"-",Lit (Integer 24))) = parseExp "42 - 24")

    [<Test>]
    member this.Mul() =
        Assert.IsTrue(Some (InfixApp (Lit (Integer 42),"*",Lit (Integer 24))) = parseExp "42 * 24")

    [<Test>]
    member this.Div() =
        Assert.IsTrue(Some (InfixApp (Lit (Integer 42),"/",Lit (Integer 24))) = parseExp "42 / 24")

    [<Test>]
    member this.AddMul() =
        Assert.IsTrue(Some (InfixApp (Lit (Integer 42),"+", InfixApp (Lit (Integer 24),"*",Lit (Integer 12)))) = parseExp "42 + 24 * 12")

    [<Test>]
    member this.Parens() =
        Assert.IsTrue(Some (InfixApp (InfixApp (Lit (Integer 42),"+",Lit (Integer 24)),"*", Lit (Integer 12))) = parseExp "(42 + 24) * 12")