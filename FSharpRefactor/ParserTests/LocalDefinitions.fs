module LocalDefinitions

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
        Assert.IsTrue(Some (Let("b", Lit(Integer 4), Let("b", InfixApp(Var "b", "+", Lit(Integer 1)), Var "b")))  = parseExp "let b = 4 in let b = b + 1 in  b")    
    

  