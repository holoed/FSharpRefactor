module DeSugarFunctionsTests

open NUnit.Framework
open DeSugarFunctions
open FSharpParser

[<TestFixture>]
type DeSugarFunctionsTests() =
    [<Test>]
    member this.``Desugar function "let f x = x" to "let f = fun x -> x"``() =
        let (Some exp) = parseExp "let f x = x"
        let (Some exp') = parseExp "let f = fun x -> x"
        let exp'' = desugar exp
        Assert.IsTrue ((exp' = exp''))

    [<Test>]
    member this.``Desugar function "let f x y = x" to "let f = fun x -> fun y -> x"``() =
        let (Some exp) = parseExp "let f x y = x"
        let (Some exp') = parseExp "let f = fun x -> fun y -> x"
        let exp'' = desugar exp
        Assert.IsTrue ((exp' = exp''))

    [<Test>]
    member this.``Desugar function "let f x y z = z" to "let f = fun x -> fun y -> fun z -> z"``() =
        let (Some exp) = parseExp "let f x y z = z"
        let (Some exp') = parseExp "let f = fun x -> fun y -> fun z -> z"
        let exp'' = desugar exp
        Assert.IsTrue ((exp' = exp''))
        