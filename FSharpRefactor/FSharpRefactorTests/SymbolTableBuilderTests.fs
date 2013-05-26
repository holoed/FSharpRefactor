// * **********************************************************************************************
// * Copyright (c) Edmondo Pentangelo. 
// *
// * This source code is subject to terms and conditions of the Apache License, Version 2.0. A 
// * copy of the license can be found in the License.html file at the root of this distribution. 
// * By using this source code in any fashion, you are agreeing to be bound 
// * by the terms of the Apache License, Version 2.0.
// *
// * You must not remove this notice, or any other, from this software.
// * **********************************************************************************************

module SymbolTableBuilderTests

open NUnit.Framework
open Ast
open CompilerToAst
open CompilerToAstTests
open SymbolTableBuilder
open Utils

[<TestFixture>]
type SymbolTableBuilderTests() =

    [<Test>]
    member this.``Should find identifier x in simple binding``() = 
        let ast = parseWithPosDecl "let x = 42"
        AssertAreEqual [loc(4,5,1,1)] (SymbolTableBuilder.findAllReferences "x" (loc(4,5,1,1)) ast)
             
    [<Test>]
    member this.``Should find all occurences of identifier x in function binding``() = 
        let ast = parseWithPosDecl "let f x = x"
        AssertAreEqual [loc(10,11,1,1); loc(6,7,1,1)] (SymbolTableBuilder.findAllReferences "x" (loc(6,7,1,1)) ast)

    [<Test>]
    member this.``Find definition of x given its usage in let x = x`` () =
        let ast = parseWithPosDecl "let x = x" 
        // The result is empty because the x in the body is not defined.
        AssertAreEqual [] (findAllReferences "x" (loc(8,9,1,1)) ast)

    [<Test>]
    member this.``Find usage of x given its definition in let x = x`` () =
        let ast = parseWithPosDecl "let x = x" 
        // The result is only the definition because the x in the body is not the same identifier.
        AssertAreEqual [loc(4,5,1,1)] (findAllReferences "x" (loc(4,5,1,1)) ast)                   
                             
    [<Test>]
    member this.``Find definition of x bound in f given its usage in sample 1``() =
        let ast = parseWithPosDecl ("let f x = let g x = x  \n" +  
                                    "          g x")
        AssertAreEqual [loc(12,13,2,2); loc(6,7,1,1)] (findAllReferences "x" (loc(12,13,2,2)) ast)                             

    [<Test>]
    member this.``Find definition of x bound in g given its usage in sample 1``() =
        let ast = parseWithPosDecl ("let f x = let g x = x  \n" +  
                                    "          g x")
        AssertAreEqual [loc(20,21,1,1); loc(16,17,1,1)] (findAllReferences "x" (loc(20,21,1,1)) ast)   

    [<Test>]
    member this.``Find usages of x given its definition bound in f in sample 1``() =
        let ast = parseWithPosDecl ("let f x = let g x = x  \n" +  
                                    "          g x")
        AssertAreEqual [loc(12,13,2,2); loc(6,7,1,1)] (findAllReferences "x" (loc(6,7,1,1)) ast) 

    [<Test>]
    member this.``Find usages of x given its definition bound in g in sample 1``() =
        let ast = parseWithPosDecl ("let f x = let g x = x  \n" +  
                                    "          g x")
        AssertAreEqual [loc(20,21,1,1); loc(16,17,1,1)] (findAllReferences "x" (loc(16,17,1,1)) ast)

    [<Test>]
    member this.``Find usages of function g given its definition in sample 1``() =
        let ast = parseWithPosDecl ("let f x = let g x = x  \n" +  
                                    "          g x")
        AssertAreEqual [loc(10,11,2,2); loc(14,15,1,1)] (findAllReferences "g" (loc(14,15,1,1)) ast)

    [<Test>]
    member this.``Find definition of function g given its usage in sample 1``() =
        let ast = parseWithPosDecl ("let f x = let g x = x  \n" +  
                                    "          g x")
        AssertAreEqual [loc(10,11,2,2); loc(14,15,1,1)] (findAllReferences "g" (loc(10,11,2,2)) ast)

    [<Test>]
    member this.``Find usage of x given its definition in sample 2`` () =
        let ast = parseWithPosDecl "let f x y = x y"  
        AssertAreEqual [loc(12,13,1,1); loc(6,7,1,1)] (findAllReferences "x" (loc(6,7,1,1)) ast) 
        
    [<Test>]
    member this.``Find usage of y given its definition in sample 2`` () =
        let ast = parseWithPosDecl "let f x y = x y"
        AssertAreEqual [loc(14,15,1,1); loc(8,9,1,1)] (findAllReferences "y" (loc(8,9,1,1)) ast)
        
    [<Test>]
    member this.``Find usage of f given its definition in sample 3`` () =
        let ast = parseWithPosDecl ("let f x = x  \n" +
                                    "let g x = f x  ")
        AssertAreEqual [loc(10,11,2,2); loc(4,5,1,1)] (findAllReferences "f" (loc(4,5,1,1)) ast)       
               
    [<Test>]
    member this.``Find definition of f given its usage in sample 3`` () =
        let ast = parseWithPosDecl ("let f x = x  \n" +
                                    "let g x = f x  ")
        AssertAreEqual [loc(10,11,2,2); loc(4,5,1,1)] (findAllReferences "f" (loc(10,11,2,2)) ast)       
           
    [<Test>]
    member this.``Find usage of x given its definition in sample 4`` () =
        let ast = parseWithPosDecl "let f x y = (x, y)"
        AssertAreEqual [loc(13,14,1,1); loc(6,7,1,1)] (findAllReferences "x" (loc(6,7,1,1)) ast)                                          

    [<Test>]
    member this.``Find definition of x given its usage in sample 4`` () =
        let ast = parseWithPosDecl "let f x y = (x, y)"
        AssertAreEqual [loc(13,14,1,1); loc(6,7,1,1)] (findAllReferences "x" (loc(13,14,1,1)) ast)     

    [<Test>]
    member this.``Find usage of x given its definition in sample 5`` () =
        let ast = parseWithPosDecl "let f x y = [x; y]"
        AssertAreEqual [loc(13,14,1,1); loc(6,7,1,1)] (findAllReferences "x" (loc(6,7,1,1)) ast) 

    [<Test>]
    member this.``Find definition of x given its usage in sample 5`` () =
        let ast = parseWithPosDecl "let f x y = [x; y]"
        AssertAreEqual [loc(13,14,1,1); loc(6,7,1,1)] (findAllReferences "x" (loc(13,14,1,1)) ast) 

    [<Test>]
    member this.``Find usage of x given its definition in sample 6`` () =
        let ast = parseWithPosDecl "let f x = Some x"
        AssertAreEqual [loc(15,16,1,1); loc(6,7,1,1)] (findAllReferences "x" (loc(6,7,1,1)) ast) 

    [<Test>]
    member this.``Find definition of x given its usage in sample 6`` () =
        let ast = parseWithPosDecl "let f x = Some x"
        AssertAreEqual [loc(15,16,1,1); loc(6,7,1,1)] (findAllReferences "x" (loc(15,16,1,1)) ast) 