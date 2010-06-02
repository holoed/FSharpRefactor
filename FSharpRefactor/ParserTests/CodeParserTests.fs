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

module CodeParserTests

open System;
open Tokenizer
open Ast
open CodeParser
open NUnit.Framework
open Utils

let parse s = option  { let! x = tokenize s 
                        let! y = parseCode x
                        return y }

[<TestFixture>]
type CodeParserTests() =

    [<Test>]
    member this.Variable() =        
        Assert.IsTrue (Some (Var (Ident "x")) = parse "x")
        Assert.IsTrue (Some (Var (Ident "x")) = parse "(x)")
    
    [<Test>]
    member this.ArithmeticExpression() =        
        Assert.IsTrue (Some(InfixApp (Var (Ident "x"), VarOp (Symbol "*"), Lit (Integer 42))) = 
        parse "x * 42")

    [<Test>]
    member this.LookUp() = 
        Assert.IsTrue (Some(LookUp ( Var (Ident "List"), (Ident "map"))) = 
        parse "List.map")

    [<Test>]
    member this.FunctionApplication() = 
        Assert.IsTrue (Some(App ( Var (Ident "f"), Var (Ident "x"))) = 
        parse "f x")

    [<Test>]
    member this.FunctionApplicationIsLeftAssociative() = 
        Assert.IsTrue (Some(App(App ( Var (Ident "f"), Var (Ident "x")), Var (Ident "y"))) = 
        parse "f x y")

    [<Test>]
    member this.TupleParse() = 
        Assert.IsTrue (Some(Tuple [Lit (Integer 42); Var(Ident "y")]) = 
        parse "(42, y)")

    [<Test>]
    member this.Tuple3Parse() = 
        Assert.IsTrue (Some(Tuple [Var(Ident "x"); Var(Ident "y");Var(Ident "z")]) = 
        parse "(x, y, z)")

    [<Test>]
    member this.Lambda() = 
        Assert.IsTrue (Some (Lambda ([PVar (Ident "x")], Var (Ident "x"))) = 
        parse "fun x -> x")

    [<Test>]
    member this.BindingToValue() =
        Assert.IsTrue (Some(Let (PVar (Ident "x"), Lit (Integer 42))) = 
        parse "let x = 42")

    [<Test>]
    member this.TupleValueBinding() =
        Assert.IsTrue (Some(Let (PTuple [PVar (Ident "x"); PVar (Ident "y")], Tuple [Lit (Integer 2); Lit (Integer 3)])) = 
        parse "let (x,y) = (2,3)")

    [<Test>]
    member this.BindingToArithmeticExpression() =
        Assert.IsTrue (Some(Let (PVar (Ident "x"), InfixApp ( Lit (Integer 2), VarOp (Symbol "*"), Lit (Integer 3)))) = 
        parse "let x = 2 * 3")
                                                            
    [<Test>]
    member this.BindingToFunctionApplication() = 
        Assert.IsTrue (Some(Let (PVar (Ident "x"), App ( Var (Ident "f"), Var (Ident "x")))) = 
        parse "let x = f x")

    [<Test>]
    member this.FunctionBinding() = 
        Assert.IsTrue (Some(Let (PApp (Ident "f", [PVar (Ident "x")]), Var (Ident "x"))) = 
        parse "let f x = x")

    [<Test>]
    member this.FunctionBindingWithArithmeticExpressionBody() = 
        Assert.IsTrue (Some(Let (PApp (Ident "f", [PVar (Ident "x")]),  InfixApp (Var (Ident "x"), VarOp (Symbol "+"), Lit (Integer 1)))) = 
        parse "let f x = x + 1")

    [<Test>]
    member this.Example1() = 
        Assert.IsTrue (Some (Let (
                                  PVar (Ident "product"), 
                                  App (
                                    App (
                                         LookUp (Var (Ident "List"), Ident "fold"), 
                                         Lambda ([PVar (Ident "x"); PVar (Ident "y")],  InfixApp(Var (Ident "x"), VarOp (Symbol "*"), Var (Ident "y"))) ), 
                                   Lit(Integer 1)))) = 
        parse "let product = List.fold (fun x y -> x * y) 1")

    [<Test>]
    member this.Bindings() =
        Assert.IsTrue (Some(Code([Let (PVar (Ident "x"), Lit (Integer 42));
                                  Let (PVar (Ident "y"), Lit (Integer 24))])) = 
        parse ("let x = 42" + Environment.NewLine + 
               "let y = 24"))

    [<Test>]
    member this.Bindings2() =
        Assert.IsTrue (Some(Let (PVar (Ident "x"), Lit (Integer 42))) = 
        parse ("let x = " + Environment.NewLine + 
               "        42"))

    [<Test>]
    member this.BindingsOffSide() =
        let ret = parse (" let x = " + Environment.NewLine + 
                         "42")
        Assert.IsTrue (ret.IsNone)
        