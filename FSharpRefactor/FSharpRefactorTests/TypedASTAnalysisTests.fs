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
        AssertAreEqual [Var ("x", loc(6,7,1,1)); Var ("x", loc(10,11,1,1))] (findAllReferences (loc (6,7,1,1)) (parseWithPosDecl "let f x = x"))
        AssertAreEqual [Var ("x", loc(6,7,1,1)); Var ("x", loc(10,11,1,1))] (findAllReferences (loc (10,11,1,1)) (parseWithPosDecl "let f x = x"))