module ASTAnalysisTests

open Ast
open CompilerToAst
open CompilerToAstTests
open ASTAnalysis
open NUnit.Framework

[<TestFixture>]
type ASTAnalysisTests() =
                 //012345678
    let sample0 = "let x = x"                             
                 //0123456789012345678901
    let sample1 = "let f x = let g x = x  \n" +  
                  "          g x"

    [<Test>]
    [<Ignore("Work in progress..")>]
    member this.``Find usage of x given its definition in sample 0`` () =
        let ast = parseWithPos sample0
        AssertAreEqual [Var ("x", loc(4,5,1,1));Var ("x", loc(8,9,1,1))] (findAllReferences ast (loc(4,5,1,1)))

    [<Test>]
    [<Ignore("Work in progress..")>]
    member this.``Find definition of x given its usage in sample 0`` () =
        let ast = parseWithPos sample0
        AssertAreEqual [Var ("x", loc(4,5,1,1));Var ("x", loc(8,9,1,1))] (findAllReferences ast (loc(8,9,1,1)))

    [<Test>]
    [<Ignore("Work in progress..")>]
    member this.``Find usages of x given its definition bound in f in sample 1``() =
        let ast = parseWithPos sample1
        AssertAreEqual [Var ("x", loc(6,7,1,1));Var ("x", loc(2,3,2,2))] (findAllReferences ast (loc(6,7,1,1)))

    [<Test>]
    [<Ignore("Work in progress..")>]
    member this.``Find definition of x bound in f given its usage in sample 1``() =
        let ast = parseWithPos sample1
        AssertAreEqual [Var ("x", loc(6,7,1,1));Var ("x", loc(2,3,2,2))] (findAllReferences ast (loc(2,3,2,2)))

    [<Test>]
    [<Ignore("Work in progress..")>]
    member this.``Find usages of x given its definition bound in g in sample 1``() =
        let ast = parseWithPos sample1
        AssertAreEqual [Var ("x", loc(14,15,1,1));Var ("x", loc(20,21,1,1))] (findAllReferences ast (loc(14,15,1,1)))

    [<Test>]
    [<Ignore("Work in progress..")>]
    member this.``Find definition of x bound in g given its usage in sample 1``() =
        let ast = parseWithPos sample1
        AssertAreEqual [Var ("x", loc(14,15,1,1));Var ("x", loc(20,21,1,1))] (findAllReferences ast (loc(20,21,1,1)))
       


