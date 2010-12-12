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
                 //012345678901234
    let sample2 = "let f x y = x y"   
                 //012345678901234
    let sample3 = "let f x = x  \n" +
                  "let g x = f x  "

                 //0123456789012345678
    let sample4 = "let f x y = (x, y)"

    [<Test>]
    member this.``Find definition of x bound in f given its usage in sample 1``() =
        let ast = parseWithPos sample1
        AssertAreEqual [Var ("x", loc(12,13,2,2));Var ("x", loc(6,7,1,1))] (findAllReferences (loc(12,13,2,2)) ast)

    [<Test>]
    member this.``Find definition of x bound in g given its usage in sample 1``() =
        let ast = parseWithPos sample1
        AssertAreEqual [Var ("x", loc(20,21,1,1));Var ("x", loc(16,17,1,1))] (findAllReferences (loc(20,21,1,1)) ast)

    [<Test>]
    member this.``Find definition of x given its usage in sample 0`` () =
        let ast = parseWithPos sample0
        // The result is empty because the x in the body is not defined.
        AssertAreEqual [] (findAllReferences (loc(8,9,1,1)) ast)

    [<Test>]
    member this.``Find usage of x given its definition in sample 0`` () =
        let ast = parseWithPos sample0
        // The result is only the definition because the x in the body is not the same identifier.
        AssertAreEqual [Var ("x", loc(4,5,1,1))] (findAllReferences (loc(4,5,1,1)) ast)

    [<Test>]
    member this.``Find usages of x given its definition bound in f in sample 1``() =
        let ast = parseWithPos sample1
        AssertAreEqual [Var ("x", loc(12,13,2,2));Var ("x", loc(6,7,1,1))] (findAllReferences (loc(6,7,1,1)) ast) 

    [<Test>]
    member this.``Find usages of x given its definition bound in g in sample 1``() =
        let ast = parseWithPos sample1
        AssertAreEqual [Var ("x", loc(20,21,1,1));Var ("x", loc(16,17,1,1))] (findAllReferences (loc(16,17,1,1)) ast)
 
    [<Test>]
    member this.``Find usages of function g given its definition in sample 1``() =
        let ast = parseWithPos sample1
        AssertAreEqual [Var ("g", loc(10,11,2,2));Var ("g", loc(14,15,1,1))] (findAllReferences (loc(14,15,1,1)) ast)

    [<Test>]
    member this.``Find definition of function g given its usage in sample 1``() =
        let ast = parseWithPos sample1
        AssertAreEqual [Var ("g", loc(10,11,2,2));Var ("g", loc(14,15,1,1))] (findAllReferences (loc(10,11,2,2)) ast)

    [<Test>]
    member this.``Find usage of x given its definition in sample 2`` () =
        let ast = parseWithPos sample2
        AssertAreEqual [Var ("x", loc(12,13,1,1));Var ("x", loc(6,7,1,1))] (findAllReferences (loc(6,7,1,1)) ast)       

    [<Test>]
    member this.``Find usage of y given its definition in sample 2`` () =
        let ast = parseWithPos sample2
        AssertAreEqual [Var ("y", loc(14,15,1,1));Var ("y", loc(8,9,1,1))] (findAllReferences (loc(8,9,1,1)) ast)       

    [<Test>]
    member this.``Find usage of f given its definition in sample 3`` () =
        let ast = parseWithPos sample3
        AssertAreEqual [Var ("f", loc(10,11,2,2));Var ("f", loc(4,5,1,1))] (findAllReferences (loc(4,5,1,1)) ast)       

    [<Test>]
    member this.``Find definition of f given its usage in sample 3`` () =
        let ast = parseWithPos sample3
        AssertAreEqual [Var ("f", loc(10,11,2,2));Var ("f", loc(4,5,1,1))] (findAllReferences (loc(10,11,2,2)) ast)       

    [<Test>]
    member this.``Find usage of x given its definition in sample 4`` () =
        let ast = parseWithPos sample4
        AssertAreEqual [Var ("x", loc(13,14,1,1));Var ("x", loc(6,7,1,1))] (findAllReferences (loc(6,7,1,1)) ast)       

    [<Test>]
    member this.``Find definition of x given its usage in sample 4`` () =
        let ast = parseWithPos sample4
        AssertAreEqual [Var ("x", loc(13,14,1,1));Var ("x", loc(6,7,1,1))] (findAllReferences (loc(13,14,1,1)) ast)       

