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

namespace TypeInferenceTests

open NUnit.Framework
open FSharpParser
open TypeInf

[<TestFixture>]
type TypeInferenceTests () =
    
    let parseExp s = 
        let (Some exp) = parseExp s
        exp

    [<Test>]
    member x.SimpleBinding () =
        let exp = parseExp "let x = 42"
        let typeOfExp = typeOf exp
        Assert.AreEqual ("int", typeOfExp.ToString())

    [<Test>]
    member x.SimpleBinding2 () =
        let exp = parseExp "let x = 42.5"
        let typeOfExp = typeOf exp
        Assert.AreEqual ("float", typeOfExp.ToString())   

    [<Test>]
    member x.IdentityFunction () =
        let exp = parseExp "let f = fun x -> x"
        let typeOfExp = typeOf exp
        Assert.AreEqual ("('a -> 'a)", typeOfExp.ToString())

    [<Test>]
    member x.TakesATakesBReturnsB () =
        let exp = parseExp "let f = fun x -> fun y -> y"
        let typeOfExp = typeOf exp
        Assert.AreEqual ("('a -> ('b -> 'b))", typeOfExp.ToString())

    [<Test>]
    member x.TakesATakesBReturnsA () =
        let exp = parseExp "let f = fun x -> fun y -> x"
        let typeOfExp = typeOf exp
        Assert.AreEqual ("('a -> ('b -> 'a))", typeOfExp.ToString())

    [<Test>]
    member x.TakesATakesBReturnsAPlusB () =
        let exp = parseExp "let f = fun (x:int) -> fun y -> x + y"
        let typeOfExp = typeOf exp
        Assert.AreEqual ("(int -> (int -> int))", typeOfExp.ToString())

    [<Test>]
    member x.Increment () =
        let exp = parseExp "let f = fun x -> x + 1"
        let typeOfExp = typeOf exp
        Assert.AreEqual ("(int -> int)", typeOfExp.ToString())

    [<Test>]
    member x.Compose () =
        let exp = parseExp "let compose = fun f -> fun g -> fun x -> g (f x) "
        let typeOfExp = typeOf exp
        Assert.AreEqual ("(('a -> 'b) -> (('b -> 'c) -> ('a -> 'c)))", typeOfExp.ToString())

    [<Test>]
    member x.``Identity function with type annotation`` () =
        let exp = parseExp "let f = fun (x:int) -> x"
        let typeOfExp = typeOf exp
        Assert.AreEqual ("(int -> int)", typeOfExp.ToString())

    [<Test>]
    member x.``Plus function should be polymorphic`` () =
        let typeOfExp = "let f = fun (x:float) -> fun y -> x + y" |> parseExp |> typeOf
        Assert.AreEqual ("(float -> (float -> float))", typeOfExp.ToString())

        let typeOfExp = "let f = fun (x:int) -> fun y -> x + y" |> parseExp |> typeOf
        Assert.AreEqual ("(int -> (int -> int))", typeOfExp.ToString())

        let typeOfExp = "let f = fun x -> fun (y:int) -> x + y" |> parseExp |> typeOf
        Assert.AreEqual ("(int -> (int -> int))", typeOfExp.ToString())

        let typeOfExp = "let f = fun x -> fun (y:float) -> x + y" |> parseExp |> typeOf
        Assert.AreEqual ("(float -> (float -> float))", typeOfExp.ToString())
        