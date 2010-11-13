+// * **********************************************************************************************
// * Copyright (c) Edmondo Pentangelo. 
// *
// * This source code is subject to terms and conditions of the Microsoft Public License. 
// * A copy of the license can be found in the License.html file at the root of this distribution. 
// * By using this source code in any fashion, you are agreeing to be bound by the terms of the 
// * Microsoft Public License.
// *
// * You must not remove this notice, or any other, from this software.
// * **********************************************************************************************

module TypeAnnotationTests

open NUnit.Framework
open FSharpParser
open Ast

[<TestFixture>]
type TypeAnnotationTests() =
    
        [<Test>]
        member this.``Simple type annotation of function argument``() =
            Assert.IsTrue(Some (Let(PApp(PVar "f", PWithTy(PVar "x", tyInteger)), Var "x", Var "f")) = parseExp "let f (x:int) = x") 

        [<Test>]
        member this.``Simple type annotation of lambda argument``() =
            Assert.IsTrue(Some (Let(PVar "f", Lam([PWithTy(PVar "x", tyInteger)], Var "x"), Var "f")) = parseExp "let f = fun (x:int) -> x") 


