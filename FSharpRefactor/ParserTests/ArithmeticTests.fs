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

module ArithmeticTests

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
    member this.Float() =
        Assert.IsTrue(Some (Lit (Float 0.05)) = parseExp "0.05")
        Assert.IsTrue(Some (Lit (Float -12.5)) = parseExp "-12.5")
        Assert.IsTrue(Some (Lit (Float 2.0015)) = parseExp "2.0015")

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