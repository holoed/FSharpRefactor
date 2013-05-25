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

[<TestFixture>]
type SymbolTableBuilderTests() =

    [<Test>]
    member this.``Should find identifier x in simple binding``() = 
        let ast = parseWithPosDecl "let x = 42"
        let m = SymbolTableBuilder.buildSymbolTable ast
        let state = SymbolTable.empty |> StateMonad.executeGetState m
        let loc = Utils.loc(4,5,1,1)
        let ret = SymbolTable.lookUp "x" loc  state   
        Assert.AreEqual(1, List.length ret)                              
        Assert.AreEqual(loc, ret.[0])   
        
    [<Test>]
    member this.``Should find all occurences of identifier x in function binding``() = 
        let ast = parseWithPosDecl "let f x = x"
        let m = SymbolTableBuilder.buildSymbolTable ast
        let state = SymbolTable.empty |> StateMonad.executeGetState m
        let loc = Utils.loc(6,7,1,1)
        let ret = SymbolTable.lookUp "x" loc  state   
        Assert.AreEqual(2, List.length ret)                              
        Assert.AreEqual(Utils.loc(10,11,1,1), ret.[0]) 
        Assert.AreEqual(loc, ret.[1])                                      
                                     