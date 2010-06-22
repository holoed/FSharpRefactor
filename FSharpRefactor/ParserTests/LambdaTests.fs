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
        
    

  