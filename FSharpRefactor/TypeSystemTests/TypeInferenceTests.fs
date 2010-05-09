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

module TypeInferenceTests

open NUnit.Framework
open Tokenizer
open TypeInference



//[<TestFixture>]
//type TypeInferenceTests() =
//    
//    [<Test>]
//    member this.Integer() = 
//        let x = Literal (IntegerLiteral 42)
//        let y = Literal (IntegerLiteral 42, Int)
//        let success = infer x = y
//        Assert.IsTrue success

