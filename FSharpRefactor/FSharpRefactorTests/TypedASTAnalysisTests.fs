module TypedASTAnalysisTests

open Ast
open CompilerToAstTyped
open CompilerToAstTypedTests
open TypedASTAnalysis
open Utils
open NUnit.Framework



[<TestFixture>]
type TypedASTAnalysisTests() =

    [<Test>]
    member this.``Find usages of f in identity function`` () =
        AssertAreEqual [Var ("f", loc(4,5,1,1))] (findAllReferences (loc (4,5,1,1)) (parseWithPosDecl "let f x = x"))

    [<Test>]
    member this.``Find usages of x in identity function`` () =
        AssertAreEqual [Var ("x", loc(10,11,1,1)); Var ("x", loc(6,7,1,1))] (findAllReferences (loc (6,7,1,1)) (parseWithPosDecl "let f x = x"))
        AssertAreEqual [Var ("x", loc(6,7,1,1)); Var ("x", loc(10,11,1,1))] (findAllReferences (loc (10,11,1,1)) (parseWithPosDecl "let f x = x"))

    [<Test>]
    member this.``Find usages of x in lambda expression`` () =
        let ast = parseWithPosDecl ("let f = fun x -> x")
        AssertAreEqual [Var ("x", loc(17,18,1,1));Var ("x", loc(12,13,1,1))] (findAllReferences (loc (12,13,1,1)) ast)

    [<Test>]
    member this.``Find usages of x and y in lambda expression`` () =
        let ast = parseWithPosDecl ("let f = fun x -> fun y -> x + y")
        AssertAreEqual [Var ("x", loc(26,27,1,1));Var ("x", loc(12,13,1,1))] (findAllReferences (loc (12,13,1,1)) ast)
        AssertAreEqual [Var ("y", loc(30,31,1,1));Var ("y", loc(21,22,1,1))] (findAllReferences (loc (21,22,1,1)) ast)