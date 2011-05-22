﻿// * **********************************************************************************************
// * Copyright (c) Edmondo Pentangelo. 
// *
// * This source code is subject to terms and conditions of the Apache License, Version 2.0. A 
// * copy of the license can be found in the License.html file at the root of this distribution. 
// * By using this source code in any fashion, you are agreeing to be bound 
// * by the terms of the Apache License, Version 2.0.
// *
// * You must not remove this notice, or any other, from this software.
// * **********************************************************************************************


module TypedASTAnalysisTests

open Ast
open CompilerToAstTyped
open CompilerToAstTypedTests
open TypedASTAnalysis
open Utils
open NUnit.Framework



[<TestFixture>]
type TypedASTAnalysisTests() =
                          
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

    let sample5 = "let f x y = [x; y]"

    let sample6 = "let f x = Some x"

    let sample7 = "let x = 42\n" + 
                  "let y = x   " 

    let sample8 = "type Exp = Var of string\n" + 
                  "let exp = Var(\"x\")   " 

                 //0123456789012345678
    let sample9 = "let f (x, y) = x"

    let sample10 = "let f (x, _) = x"

    let sample11 = "let f p = match p with (x,y) -> x"

    let sample12 = "let xs = seq { for i in 1..5 do yield i }"

    let sample13 = "let f n x y = if n = 0 then x else y"
  
    [<Test>]
    member this.``Find definition of x bound in f given its usage in sample 1``() =
        let ast = parseWithPosDecl sample1
        AssertAreEqual [Var ("x", loc(12,13,2,2));Var ("x", loc(6,7,1,1))] (findAllReferences (loc(12,13,2,2)) ast)

    [<Test>]
    member this.``Find definition of x bound in g given its usage in sample 1``() =
        let ast = parseWithPosDecl sample1
        AssertAreEqual [Var ("x", loc(20,21,1,1));Var ("x", loc(16,17,1,1))] (findAllReferences (loc(20,21,1,1)) ast)   

    [<Test>]
    member this.``Find usages of x given its definition bound in f in sample 1``() =
        let ast = parseWithPosDecl sample1
        AssertAreEqual [Var ("x", loc(12,13,2,2));Var ("x", loc(6,7,1,1))] (findAllReferences (loc(6,7,1,1)) ast) 

    [<Test>]
    member this.``Find usages of x given its definition bound in g in sample 1``() =
        let ast = parseWithPosDecl sample1
        AssertAreEqual [Var ("x", loc(20,21,1,1));Var ("x", loc(16,17,1,1))] (findAllReferences (loc(16,17,1,1)) ast)
 
    [<Test>]
    member this.``Find usages of function g given its definition in sample 1``() =
        let ast = parseWithPosDecl sample1
        AssertAreEqual [Var ("g", loc(10,11,2,2));Var ("g", loc(14,15,1,1))] (findAllReferences (loc(14,15,1,1)) ast)

    [<Test>]
    member this.``Find definition of function g given its usage in sample 1``() =
        let ast = parseWithPosDecl sample1
        AssertAreEqual [Var ("g", loc(10,11,2,2));Var ("g", loc(14,15,1,1))] (findAllReferences (loc(10,11,2,2)) ast)

    [<Test>]
    member this.``Find usage of x given its definition in sample 2`` () =
        let ast = parseWithPosDecl sample2
        AssertAreEqual [Var ("x", loc(12,13,1,1));Var ("x", loc(6,7,1,1))] (findAllReferences (loc(6,7,1,1)) ast)       

    [<Test>]
    member this.``Find usage of y given its definition in sample 2`` () =
        let ast = parseWithPosDecl sample2
        AssertAreEqual [Var ("y", loc(14,15,1,1));Var ("y", loc(8,9,1,1))] (findAllReferences (loc(8,9,1,1)) ast)       

    [<Test>]
    member this.``Find usage of f given its definition in sample 3`` () =
        let ast = parseWithPosDecl sample3
        AssertAreEqual [Var ("f", loc(10,11,2,2));Var ("f", loc(4,5,1,1))] (findAllReferences (loc(4,5,1,1)) ast)        

    [<Test>]
    member this.``Find definition of f given its usage in sample 3`` () =
        let ast = parseWithPosDecl sample3
        AssertAreEqual [Var ("f", loc(10,11,2,2));Var ("f", loc(4,5,1,1))] (findAllReferences (loc(10,11,2,2)) ast)   
        
    [<Test>]
    member this.``Find usage of x given its definition in sample 4`` () =
        let ast = parseWithPosDecl sample4
        AssertAreEqual [Var ("x", loc(13,14,1,1));Var ("x", loc(6,7,1,1))] (findAllReferences (loc(6,7,1,1)) ast)       

    [<Test>]
    member this.``Find definition of x given its usage in sample 4`` () =
        let ast = parseWithPosDecl sample4
        AssertAreEqual [Var ("x", loc(13,14,1,1));Var ("x", loc(6,7,1,1))] (findAllReferences (loc(13,14,1,1)) ast)                                      

    [<Test>]
    member this.``Find usage of x given its definition in sample 5`` () =
        let ast = parseWithPosDecl sample5
        AssertAreEqual [Var ("x", loc(13,14,1,1));Var ("x", loc(6,7,1,1))] (findAllReferences (loc(6,7,1,1)) ast) 

    [<Test>]
    member this.``Find definition of x given its usage in sample 5`` () =
        let ast = parseWithPosDecl sample5
        AssertAreEqual [Var ("x", loc(13,14,1,1));Var ("x", loc(6,7,1,1))] (findAllReferences (loc(13,14,1,1)) ast) 

    [<Test>]
    member this.``Find usage of x given its definition in sample 6`` () =
        let ast = parseWithPosDecl sample6
        AssertAreEqual [Var ("x", loc(15,16,1,1));Var ("x", loc(6,7,1,1))] (findAllReferences (loc(6,7,1,1)) ast) 

    [<Test>]
    member this.``Find definition of x given its usage in sample 6`` () =
        let ast = parseWithPosDecl sample6
        AssertAreEqual [Var ("x", loc(15,16,1,1));Var ("x", loc(6,7,1,1))] (findAllReferences (loc(15,16,1,1)) ast) 

    [<Test>]
    member this.``Find usage of x given its definition in sample 7`` () =
        let ast = parseWithPosDecl sample7
        AssertAreEqual [Var ("x", loc(8,9,2,2));Var ("x", loc(4,5,1,1))] (findAllReferences (loc(4,5,1,1)) ast) 

    [<Test>]
    member this.``Find definition of x given its usage in sample 7`` () =
        let ast = parseWithPosDecl sample7
        AssertAreEqual [Var ("x", loc(8,9,2,2));Var ("x", loc(4,5,1,1))] (findAllReferences (loc(8,9,2,2)) ast) 

    [<Test>]
    member this.``Find usage of Var given its definition in sample 8`` () =
        let ast = parseWithPosDecl sample8
        AssertAreEqual [Var ("Var", loc(10,13,2,2));Var ("Var", loc(11,14,1,1))] (findAllReferences (loc(11,14,1,1)) ast) 

    [<Test>]
    member this.``Find definition of Var given its usage in sample 8`` () =
        let ast = parseWithPosDecl sample8
        AssertAreEqual [Var ("Var", loc(10,13,2,2));Var ("Var", loc(11,14,1,1))] (findAllReferences (loc(10,13,2,2)) ast) 

    [<Test>]
    member this.``Find usage of x given its definition in sample 9`` () =
        let ast = parseWithPosDecl sample9
        AssertAreEqual [Var ("x", loc(15,16,1,1));Var ("x", loc(7,8,1,1))] (findAllReferences (loc(7,8,1,1)) ast) 

    [<Test>]
    member this.``Find definition of x given its usage in sample 9`` () =
        let ast = parseWithPosDecl sample9
        AssertAreEqual [Var ("x", loc(15,16,1,1));Var ("x", loc(7,8,1,1))] (findAllReferences (loc(15,16,1,1)) ast) 

    [<Test>]
    member this.``Find usage of x given its definition in sample 10`` () =
        let ast = parseWithPosDecl sample10
        AssertAreEqual [Var ("x", loc(15,16,1,1));Var ("x", loc(7,8,1,1))] (findAllReferences (loc(7,8,1,1)) ast) 

    [<Test>]
    member this.``Find definition of x given its usage in sample 10`` () =
        let ast = parseWithPosDecl sample10
        AssertAreEqual [Var ("x", loc(15,16,1,1));Var ("x", loc(7,8,1,1))] (findAllReferences (loc(15,16,1,1)) ast) 

    [<Test>]
    member this.``Find usage of x given its definition in sample 11`` () =
        let ast = parseWithPosDecl sample11
        AssertAreEqual [Var ("x", loc(32,33,1,1));Var ("x", loc(24,25,1,1))] (findAllReferences (loc(24,25,1,1)) ast) 

    [<Test>]
    member this.``Find usage of x given its definition in sample 12`` () =
        let ast = parseWithPosDecl sample12
        AssertAreEqual [Var ("i", loc(38,39,1,1));Var ("i", loc(19,20,1,1))] (findAllReferences (loc(19,20,1,1)) ast) 

    [<Test>]
    member this.``Find definition of x given its usage in sample 12`` () =
        let ast = parseWithPosDecl sample12
        AssertAreEqual [Var ("i", loc(38,39,1,1));Var ("i", loc(19,20,1,1))] (findAllReferences (loc(38,39,1,1)) ast)    

    [<Test>]
    member this.``Find usage of x given its definition in sample 13`` () =
        let ast = parseWithPosDecl sample13
        AssertAreEqual [Var ("x", loc(28,29,1,1));Var ("x", loc(8,9,1,1))] (findAllReferences (loc(8,9,1,1)) ast) 

    [<Test>]
    member this.``Find definition of x given its usage in sample 13`` () =
        let ast = parseWithPosDecl sample13
        AssertAreEqual [Var ("x", loc(28,29,1,1));Var ("x", loc(8,9,1,1))] (findAllReferences (loc(28,29,1,1)) ast) 

    [<Test>]
    member this.``Find usage of y given its definition in sample 13`` () =
        let ast = parseWithPosDecl sample13
        AssertAreEqual [Var ("y", loc(35,36,1,1));Var ("y", loc(10,11,1,1))] (findAllReferences (loc(10,11,1,1)) ast) 

    [<Test>]
    member this.``Find definition of y given its usage in sample 13`` () =
        let ast = parseWithPosDecl sample13
        AssertAreEqual [Var ("y", loc(35,36,1,1));Var ("y", loc(10,11,1,1))] (findAllReferences (loc(35,36,1,1)) ast) 

    [<Test>]
    member this.``Find usages of f given its definition when LetRec and when is only Let`` () =
        let astRec = parseWithPosDecl "let rec f x = f x"
        AssertAreEqual [Var ("f", loc(14,15,1,1));Var ("f", loc(8,9,1,1))] (findAllReferences (loc(8,9,1,1)) astRec)

    [<Test>]
    member this.``Find usages of x in lambda expression`` () =
        let ast = parseWithPosDecl ("let f = fun x -> x")
        AssertAreEqual [Var ("x", loc(17,18,1,1));Var ("x", loc(12,13,1,1))] (findAllReferences (loc (12,13,1,1)) ast)

    [<Test>]
    member this.``Find usages of x and y in lambda expression`` () =
        let ast = parseWithPosDecl ("let f = fun x -> fun y -> x + y")
        AssertAreEqual [Var ("x", loc(26,27,1,1));Var ("x", loc(12,13,1,1))] (findAllReferences (loc (12,13,1,1)) ast)
        AssertAreEqual [Var ("y", loc(30,31,1,1));Var ("y", loc(21,22,1,1))] (findAllReferences (loc (21,22,1,1)) ast)
           
    [<Test>]
    member this.``Find usages in an inferred downcast``() =
        let ast = parseWithPosDecl ("let x = 42 :> System.Object \n" +
                                    "let y:string = downcast x")
        AssertAreEqual [Var ("x", loc(24,25,2,2));Var ("x", loc(4,5,1,1))] (findAllReferences (loc (4,5,1,1)) ast)

    [<Test>]
    member this.``Quoted identifier``() =        
        let ast = parseWithPosDecl ("let x' = 42 \n" +
                                    "let y = x'")
        AssertAreEqual [Var ("x'", loc(8,10,2,2));Var ("x'", loc(4,6,1,1))] (findAllReferences (loc (4,6,1,1)) ast)
                        
        