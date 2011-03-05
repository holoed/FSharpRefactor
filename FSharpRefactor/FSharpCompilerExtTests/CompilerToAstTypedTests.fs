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

module CompilerToAstTypedTests

open NUnit.Framework
open Ast
open CompilerToTypedAst
open System.IO
open AstCatamorphisms
open Utils

let parseWithPosDecl s = 
        File.WriteAllText("test.fs", s)        
        let [xs:_] = parseToTypedAst [path]
        [xs]

let parse = parse parseWithPosDecl

let parseWithPos = parseWithPos parseWithPosDecl

let parseTypes = parseTypes parseWithPosDecl

let parseModule = parseModule parseWithPosDecl

[<TestFixture>]
type CompilerToAstTypedTests() =
     
    [<Test>]
    member this.Const() =        
        Assert.IsTrue ([Lit(Integer 42)] = parse "42")

    [<Test>]
    member this.SimpleDecls() =        
        AssertAreEqual [Let(false,[PVar "x", Lit(Integer 42)], Lit(Unit))] (parse "let x = 42")
        AssertAreEqual [Let(false,[PVar "x", Lit(Integer 42)], Lit(Unit)); Let(false,[PVar "y", Lit(Integer 24)], Lit(Unit))] (parse "let x = 42\nlet y = 24")

    [<Test>]
    member this.FunctionsDecls() =        
        AssertAreEqual [Let (false,[(PVar "f", Lam ([PVar "x"],Var "x"))],Lit Unit)]  (parse "let f x = x")
        AssertAreEqual [Let (false,[(PVar "f", Lam ([PVar "x"],Lam ([PVar "y"],Var "y")))], Lit(Unit))]  (parse "let f x y = y")        
        AssertAreEqual [Let (false,[(PVar "f", Lam ([PVar "x"],Lam ([PVar "y"], Lam ([PVar "z"],Var "z"))))], Lit(Unit))] (parse "let f x y z = z")

    [<Test>]
    member this.Lambdas() =        
        AssertAreEqual [Let (false,[(PVar "f", Lam ([PVar "x"],Var "x"))],Lit Unit)]  (parse "let f = fun x -> x")
        AssertAreEqual [Let (false,[(PVar "f", Lam ([PVar "x"],Lam ([PVar "y"],Var "y")))], Lit(Unit))]  (parse "let f = fun x -> fun y -> y")        
        AssertAreEqual [Let (false,[(PVar "f", Lam ([PVar "x"],Lam ([PVar "y"], Lam ([PVar "z"],Var "z"))))], Lit(Unit))] (parse "let f = fun x -> fun y -> fun z -> z")