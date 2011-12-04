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


module ASTAnalysisTests

open Ast
open CompilerToAst
open CompilerToAstTests
open Utils
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

    let sample5 = "let f x y = [x; y]"

    let sample6 = "let f x = Some x"

    let sample7 = "let x = 42\n" + 
                  "let y = x   " 

    let sample8 = "type Exp = Var of string\n" + 
                  "let exp = Exp.Var(\"x\")   " 

                 //0123456789012345678
    let sample9 = "let f (x, y) = x"

    let sample10 = "let f (x, _) = x"

    let sample11 = "let f p = match p with (x,y) -> x"

    let sample12 = "let xs = seq { for i in 1..5 do yield i }"

    let sample13 = "let f n x y = if n = 0 then x else y"


    [<Test>]
    member this.``Find definition of x given its usage in sample 0`` () =
        let ast = parseWithPosDecl sample0
        // The result is empty because the x in the body is not defined.
        AssertAreEqual [] (findAllReferences (loc(8,9,1,1)) ast)

    [<Test>]
    member this.``Find usage of x given its definition in sample 0`` () =
        let ast = parseWithPosDecl sample0
        // The result is only the definition because the x in the body is not the same identifier.
        AssertAreEqual [Var ("x", loc(4,5,1,1))] (findAllReferences (loc(4,5,1,1)) ast)

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
        AssertAreEqual [Var ("Exp.Var", loc(14,17,2,2));Var ("Exp.Var", loc(11,14,1,1))] (findAllReferences (loc(11,14,1,1)) ast) 

    [<Test>]
    member this.``Find definition of Var given its usage in sample 8`` () =
        let ast = parseWithPosDecl sample8
        AssertAreEqual [Var ("Exp.Var", loc(14,17,2,2));Var ("Exp.Var", loc(11,14,1,1))] (findAllReferences (loc(14,17,2,2)) ast) 

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
        let ast    = parseWithPosDecl "let f x = f x"
        let astRec = parseWithPosDecl "let rec f x = f x"
        AssertAreEqual [Var ("f", loc(4,5,1,1))] (findAllReferences (loc(4,5,1,1)) ast)
        AssertAreEqual [Var ("f", loc(14,15,1,1));Var ("f", loc(8,9,1,1))] (findAllReferences (loc(8,9,1,1)) astRec)

    [<Test>]
    member this.``Find usages of x and y given their definition in List pattern`` () =
        let ast = parseWithPosDecl "let f [x;y] = [x;y]"
        AssertAreEqual [Var ("x", loc(15,16,1,1));Var ("x", loc(7,8,1,1))] (findAllReferences (loc(7,8,1,1)) ast)
        AssertAreEqual [Var ("y", loc(17,18,1,1));Var ("y", loc(9,10,1,1))] (findAllReferences (loc(9,10,1,1)) ast)

    [<Test>]
    member this.``Find usages of x in array setter given its definition`` () =
        let ast = parseWithPosDecl ("let x = 42              \n" + 
                                    "myArray.[0, 1] <- x")
        AssertAreEqual [Var ("x", loc(18,19,2,2));Var ("x", loc(4,5,1,1))] (findAllReferences (loc(4,5,1,1)) ast)

    [<Test>]
    member this.``Find usages of x in array getter given its definition`` () =
        let ast = parseWithPosDecl ("let x = 42              \n" + 
                                    "let z = myArray.[0, x]")
        AssertAreEqual [Var ("x", loc(20,21,2,2));Var ("x", loc(4,5,1,1))] (findAllReferences (loc(4,5,1,1)) ast)

    [<Test>]
    member this.``Find usages of x and y given their definitions`` () = 
        let ast = parseWithPosDecl ("let (x,y) = (12,42)    \n" +
                                    "let p = (x, y) ")
        AssertAreEqual [Var ("y", loc(12,13,2,2));Var ("y", loc(7,8,1,1))] (findAllReferences (loc (7,8,1,1)) ast)
        AssertAreEqual [Var ("x", loc(9,10,2,2));Var ("x", loc(5,6,1,1))] (findAllReferences (loc (5,6,1,1)) ast)

    [<Test>]
    member this.``Find usages of x and y in record given their definitions`` () =
        let ast = parseWithPosDecl ("let (x,y) = (12,42)    \n" +
                                    "let p = { X = x; Y = y }")
        AssertAreEqual [Var ("y", loc(21,22,2,2));Var ("y", loc(7,8,1,1))] (findAllReferences (loc (7,8,1,1)) ast)
        AssertAreEqual [Var ("x", loc(14,15,2,2));Var ("x", loc(5,6,1,1))] (findAllReferences (loc (5,6,1,1)) ast)

    [<Test>]
    member this.``Find usages of x and y in class members given their definition in the implicit constructor.`` () = 
        let ast = parseWithPosDecl  ("type Point (x:int,y:int) = \n" +
                                     "    member this.X = x      \n" +
                                     "    member this.Y = y")
        AssertAreEqual [Var ("x", loc(20,21,2,2));Var ("x", loc(12,13,1,1))] (findAllReferences (loc (12,13,1,1)) ast)

    [<Test>]
    member this.``Find usages of x and y in object instance construction given their definitions`` () =
        let ast = parseWithPosDecl ("let (x,y) = (12,42)    \n" +
                                    "let p = new Point(x, y)")
        AssertAreEqual [Var ("y", loc(21,22,2,2));Var ("y", loc(7,8,1,1))] (findAllReferences (loc (7,8,1,1)) ast)
        AssertAreEqual [Var ("x", loc(18,19,2,2));Var ("x", loc(5,6,1,1))] (findAllReferences (loc (5,6,1,1)) ast)

    [<Test>]
    member this.``Find usages of x and y in object expr construction given their definitions`` () =
        let ast = parseWithPosDecl ("let (x,y) = (12,42)    \n" +
                                    "let p = { new IPoint with            \n" +
                                    "               member this.X = x      \n" +
                                    "               member this.Y = y }")
        AssertAreEqual [Var ("y", loc(31,32,4,4));Var ("y", loc(7,8,1,1))] (findAllReferences (loc (7,8,1,1)) ast)
        AssertAreEqual [Var ("x", loc(31,32,3,3));Var ("x", loc(5,6,1,1))] (findAllReferences (loc (5,6,1,1)) ast)

    [<Test>]
    member this.``Find usages of x in do expression given its definition or usage`` () =
        let ast = parseWithPosDecl ("let x = 42   \n" +
                                    "do write x ")
        AssertAreEqual [Var ("x", loc(9,10,2,2));Var ("x", loc(4,5,1,1))] (findAllReferences (loc (4,5,1,1)) ast)
        AssertAreEqual [Var ("x", loc(9,10,2,2));Var ("x", loc(4,5,1,1))] (findAllReferences (loc (9,10,2,2)) ast)

    [<Test>]
    member this.``Find usages of x in interface implementation given its definition or usage`` () =
        let ast = parseWithPosDecl ("let x = 42 \n" +
                                    "type MyClass = \n" +
                                    "     interface IFoo with \n" +
                                    "          member this.Bar () = x")
        AssertAreEqual [Var ("x", loc(31,32,4,4));Var ("x", loc(4,5,1,1))] (findAllReferences (loc (4,5,1,1)) ast)
        AssertAreEqual [Var ("x", loc(31,32,4,4));Var ("x", loc(4,5,1,1))] (findAllReferences (loc (31,32,4,4)) ast)

    [<Test>]
    member this.``Find usages of x in assignment given its definition or usages`` () =
        let ast = parseWithPosDecl ("let x = 42 \n" +
                                    "x <- x + 1")
        AssertAreEqual [Var ("x", loc(5,6,2,2));Var ("x", loc(0,1,2,2));Var ("x", loc(4,5,1,1))] (findAllReferences (loc (4,5,1,1)) ast)
        AssertAreEqual [Var ("x", loc(5,6,2,2));Var ("x", loc(0,1,2,2));Var ("x", loc(4,5,1,1))] (findAllReferences (loc (0,1,2,2)) ast)
        AssertAreEqual [Var ("x", loc(5,6,2,2));Var ("x", loc(0,1,2,2));Var ("x", loc(4,5,1,1))] (findAllReferences (loc (5,6,2,2)) ast)

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
    member this.``Find usages of x and y in member body`` () =
        let ast = parseWithPosDecl  ("type Point = \n" +
                                     "    member this.Swap (x, y) = (y, x)")        
        AssertAreEqual [Var ("x", loc(34,35,2,2));Var ("x", loc(22,23,2,2))] (findAllReferences (loc (22,23,2,2)) ast)
        AssertAreEqual [Var ("y", loc(31,32,2,2));Var ("y", loc(25,26,2,2))] (findAllReferences (loc (25,26,2,2)) ast)

    [<Test>]
    member this.``Find usages of this object identifier of instance member`` () =
        let ast = parseWithPosDecl  ("type Point = \n" +
                                     "    member p.Swap = p")        
        AssertAreEqual [Var ("p", loc(20,21,2,2));Var ("p", loc(11,12,2,2))] (findAllReferences (loc (11,12,2,2)) ast)

    [<Test>]
    member this.``Identifiers defined within the scope of a method should be unavailable outside of the method`` () =
        let ast = parseWithPosDecl  ("let y = 12 \n" +
                                     "type Point = \n" +
                                     "    member p.Add x =  \n" +
                                     "        let y = 42    \n" +
                                     "        x + y         \n" +
                                     "let foo = y")        
        AssertAreEqual [Var ("y", loc(10,11,6,6));Var ("y", loc(4,5,1,1))] (findAllReferences (loc (10,11,6,6)) ast)
        AssertAreEqual [Var ("y", loc(12,13,5,5));Var ("y", loc(12,13,4,4))] (findAllReferences (loc (12,13,5,5)) ast)

    [<Test>]
    member this.``Find usages of x in computation expression return!`` () =
        let ast = parseWithPosDecl ("let x = 42 \n" +
                                    "let y = identity { return! x }")
        AssertAreEqual [Var ("x", loc(27,28,2,2));Var ("x", loc(4,5,1,1))] (findAllReferences (loc (4,5,1,1)) ast)

    [<Test>]
    member this.``Find usages in body of try with expression`` () =
        let ast = parseWithPosDecl(  "let divide1 x y =      \n" +
                                     "    try               \n" +
                                     "      Some (x / y)    \n" +
                                     "    with              \n" +
                                     "    | :? System.DivideByZeroException -> None ")
        AssertAreEqual [Var ("x", loc(12,13,3,3));Var ("x", loc(12,13,1,1))] (findAllReferences (loc (12,13,1,1)) ast)
        AssertAreEqual [Var ("y", loc(16,17,3,3));Var ("y", loc(14,15,1,1))] (findAllReferences (loc (14,15,1,1)) ast)

    [<Test>]
    member this.``Find usages of identifiers bound with let! in computation expression``  () =
        let ast = parseWithPosDecl("let y = state { let! x = f   \n" +
                                   "                return x }")
        AssertAreEqual [Var ("x", loc(23,24,2,2));Var ("x", loc(21,22,1,1))] (findAllReferences (loc (21,22,1,1)) ast)

    [<Test>]
    member this.``Find usages in mutually recursive functions``() =
        let ast = parseWithPosDecl ( "let loop e =              \n" +
                                     "    let rec foo x =       \n" +
                                     "      bar x               \n" +
                                     "    and bar y =           \n" +
                                     "        foo y             \n" +
                                     "    foo e")
        AssertAreEqual [Var ("x", loc(10,11,3,3));Var ("x", loc(16,17,2,2))] (findAllReferences (loc (16,17,2,2)) ast)
        AssertAreEqual [Var ("y", loc(12,13,5,5));Var ("y", loc(12,13,4,4))] (findAllReferences (loc (12,13,4,4)) ast)
        

    [<Test>]
    member this.``Find usages of identifiers in do!``  () =
        let ast = parseWithPosDecl("let transfer accA accB amount = stm { do! withdraw accA amount \n" +
                                   "                                      do! deposit accB amount }")
        AssertAreEqual [Var ("accA", loc(51,55,1,1));Var ("accA", loc(13,17,1,1))] (findAllReferences (loc (13,17,1,1)) ast)
        AssertAreEqual [Var ("accB", loc(50,54,2,2));Var ("accB", loc(18,22,1,1))] (findAllReferences (loc (18,22,1,1)) ast)
        AssertAreEqual [Var ("amount", loc(55,61,2,2));Var ("amount", loc(56,62,1,1));Var ("amount", loc(23,29,1,1))] (findAllReferences (loc (23,29,1,1)) ast)

    [<Test>]
    member this.``Find usages of identifiers in dotGet``  () =
        let ast = parseWithPosDecl("let getName xs = (List.head xs).Name")
        AssertAreEqual [Var ("xs", loc(28,30,1,1));Var ("xs", loc(12,14,1,1))] (findAllReferences (loc (12,14,1,1)) ast)

    [<Test>]
    member this.``Find usages of identifiers in dotSet``  () =
        let ast = parseWithPosDecl("let x = 42 \n" +
                                   "(List.head xs).Value <- x")
        AssertAreEqual [Var ("x", loc(24,25,2,2));Var ("x", loc(4,5,1,1))] (findAllReferences (loc (4,5,1,1)) ast)

    [<Test>]
    member this.``Find usages of identifiers in typed function`` () =
        let ast = parseWithPosDecl ("let f x : int = x")
        AssertAreEqual [Var ("x", loc(16,17,1,1));Var ("x", loc(6,7,1,1))] (findAllReferences (loc (6,7,1,1)) ast)

    [<Test>]
    member this.``Find usages in static members of exception definition`` () =
        let ast = parseWithPosDecl ("let x = 42 \n" +
                                    "exception Foo with static member Bar = x")
        AssertAreEqual [Var ("x", loc(39,40,2,2));Var ("x", loc(4,5,1,1))] (findAllReferences (loc (4,5,1,1)) ast)

    [<Test>]
    member this.``Find usages in instance members of exception definition`` () =
        let ast = parseWithPosDecl ("let x = 42 \n" +
                                    "exception Foo with member this.Bar = x")
        AssertAreEqual [Var ("x", loc(37,38,2,2));Var ("x", loc(4,5,1,1))] (findAllReferences (loc (4,5,1,1)) ast)

    [<Test>]
    member this.``Find usages in IfThen expression without else`` () =
        let ast = parseWithPosDecl ("let f x = state { if (x > 0) then return! f x }")
        AssertAreEqual [Var ("x", loc(44,45,1,1));Var ("x", loc(22,23,1,1));Var ("x", loc(6,7,1,1))] (findAllReferences (loc (6,7,1,1)) ast)

    [<Test>]
    member this.``Find usages in Record pattern`` () =
        let ast = parseWithPosDecl ("let { FirstName = x; LastName = y } = ret  \n" +
                                    "let fullName = x + y                         ")
        AssertAreEqual [Var ("x", loc(15,16,2,2));Var ("x", loc(18,19,1,1))] (findAllReferences (loc (18,19,1,1)) ast)
        AssertAreEqual [Var ("y", loc(19,20,2,2));Var ("y", loc(32,33,1,1))] (findAllReferences (loc (32,33,1,1)) ast)

    [<Test>]
    member this.``Find usages in expression containing anonymous type constraints`` () =
        let ast = parseWithPosDecl ("let x : IDictionary<_, _> = dict \n" +
                                    "let y = x ")
        AssertAreEqual [Var ("x", loc(8,9,2,2));Var ("x", loc(4,5,1,1))] (findAllReferences (loc (4,5,1,1)) ast)

    [<Test>]
    member this.``Find usages in expression containing an array type constraint`` () =
        let ast = parseWithPosDecl ("let x : int[] = xs \n" +
                                    "let y = x ")
        AssertAreEqual [Var ("x", loc(8,9,2,2));Var ("x", loc(4,5,1,1))] (findAllReferences (loc (4,5,1,1)) ast)

    [<Test>]
    member this.``Find usages in the presence of a type application`` () =
        let ast = parseWithPosDecl ("let x = Foo<int> \n" +
                                    "let y = x")
        AssertAreEqual [Var ("x", loc(8,9,2,2));Var ("x", loc(4,5,1,1))] (findAllReferences (loc (4,5,1,1)) ast)

    [<Test>]
    member this.``Find usages in the presence of addressOf &`` () =
        let ast = parseWithPosDecl ("let x = 42 \n" +
                                    "let y = &x")
        AssertAreEqual [Var ("x", loc(9,10,2,2));Var ("x", loc(4,5,1,1))] (findAllReferences (loc (4,5,1,1)) ast)

    [<Test>]
    member this.``Find usages in the presence of a null pattern`` () =
        let ast = parseWithPosDecl ("let x = null \n" +
                                    "let null = x")
        AssertAreEqual [Var ("x", loc(11,12,2,2));Var ("x", loc(4,5,1,1))] (findAllReferences (loc (4,5,1,1)) ast)

    [<Test>]
    member this.``Find usages of iterator variable in for loop`` () =
        let ast = parseWithPosDecl ("for i = 0 to 5 do \n" +
                                    "   printf \"%i\" i ")
        AssertAreEqual [Var ("i", loc(15,16,2,2));Var ("i", loc(4,5,1,1))] (findAllReferences (loc (4,5,1,1)) ast)

    [<Test>]
    member this.``Iterator variable should be available only within the body of the loop`` () =
        let ast = parseWithPosDecl ("for i = 0 to 5 do \n" +
                                    "   printf \"%i\" i \n" +
                                    "let x = i ")
        AssertAreEqual [Var ("i", loc(15,16,2,2));Var ("i", loc(4,5,1,1))] (findAllReferences (loc (4,5,1,1)) ast)
        AssertAreEqual [] (findAllReferences (loc (8,9,1,1)) ast)

    [<Test>]
    member this.``Iterator variable should not be available in the expression used to define the start value of the loop`` () =
        let ast = parseWithPosDecl ("for i = 0 + i to 5 do \n" +
                                    "   printf \"%i\" i ")
        AssertAreEqual [Var ("i", loc(15,16,2,2));Var ("i", loc(4,5,1,1))] (findAllReferences (loc (4,5,1,1)) ast)
        AssertAreEqual [] (findAllReferences (loc (12,13,1,1)) ast)

    [<Test>]
    member this.``Iterator variable should not be available in the expression used to define the end value of the loop`` () =
        let ast = parseWithPosDecl ("for i = 0 to 5 + i do \n" +
                                    "   printf \"%i\" i ")
        AssertAreEqual [Var ("i", loc(15,16,2,2));Var ("i", loc(4,5,1,1))] (findAllReferences (loc (4,5,1,1)) ast)
        AssertAreEqual [] (findAllReferences (loc (17,18,1,1)) ast)

    [<Test>]
    member this.``Find usages in the presence of or on pattern matching ``() =
        let ast = parseWithPosDecl ("let x = 42  \n"+
                                    "try Foo() with                     \n" +
                                    "| :? System.ArgumentException      \n" +
                                    "| :? System.ArgumentNullException -> x")
        AssertAreEqual [Var ("x", loc(37,38,4,4));Var ("x", loc(4,5,1,1))] (findAllReferences (loc (4,5,1,1)) ast)

    [<Test>]
    member this.``Find usages in the presence of an assert``() = 
        let ast = parseWithPosDecl ("let x = 42 \n" +
                                    "let _ = assert (x <= 32)")
        AssertAreEqual [Var ("x", loc(16,17,2,2));Var ("x", loc(4,5,1,1))] (findAllReferences (loc (4,5,1,1)) ast)

    [<Test>]
    member this.``Find usages in a while loop``() = 
        let ast = parseWithPosDecl ("let x = 42 \n" +
                                    "let y = 32 \n" +
                                    "while x > 0 do  \n" +
                                    "   foo y ")
        AssertAreEqual [Var ("x", loc(6,7,3,3));Var ("x", loc(4,5,1,1))] (findAllReferences (loc (4,5,1,1)) ast)
        AssertAreEqual [Var ("y", loc(7,8,4,4));Var ("y", loc(4,5,2,2))] (findAllReferences (loc (4,5,2,2)) ast)

    [<Test>]
    member this.``Find usages in a lazy value``() =
        let ast = parseWithPosDecl ("let x = 42 \n" +
                                    "let g = lazy x")
        AssertAreEqual [Var ("x", loc(13,14,2,2));Var ("x", loc(4,5,1,1))] (findAllReferences (loc (4,5,1,1)) ast)

    [<Test>]
    member this.``Find usages of enum cases``() =
        let ast = parseWithPosDecl ("type Choice = \n" +
                                    "   | Yes = 0    \n" +
                                    "   | No  = 1    \n" +
                                    "let x = Yes")
        AssertAreEqual [Var ("Yes", loc(8,11,4,4));Var ("Yes", loc(5,8,2,2))] (findAllReferences (loc (5,8,2,2)) ast)
    
    [<Test>]
    member this.``Find usages in an inferred downcast``() =
        let ast = parseWithPosDecl ("let x = 42 :> System.Object \n" +
                                    "let y:string = downcast x")
        AssertAreEqual [Var ("x", loc(24,25,2,2));Var ("x", loc(4,5,1,1))] (findAllReferences (loc (4,5,1,1)) ast)

    [<Test>]
    member this.``Find usages in an inferred upcast``() =
        let ast = parseWithPosDecl ("let x = 42 :> System.Object \n" +
                                    "let y:string = upcast x")
        AssertAreEqual [Var ("x", loc(22,23,2,2));Var ("x", loc(4,5,1,1))] (findAllReferences (loc (4,5,1,1)) ast)


    [<Test>]
    member this.``Quoted identifier``() =        
        let ast = parseWithPosDecl ("let x' = 42 \n" +
                                    "let y = x'")
        AssertAreEqual [Var ("x'", loc(8,10,2,2));Var ("x'", loc(4,6,1,1))] (findAllReferences (loc (4,6,1,1)) ast)

    [<Test>]
    member this.``Find usages in the presence of inheriting a base type`` () =
        let ast = parseWithPosDecl  ("type MyClassDerived1 =   \n" +
                                     "  inherit MyClassBase1   \n" +
                                     "  override u.function1(a: int) = a + 1")        
        AssertAreEqual [Var ("a", loc(33,34,3,3));Var ("a", loc(23,24,3,3))] (findAllReferences (loc (23,24,3,3)) ast)

    [<Test>]
    member this.``Find usages in the presence of implicit inheriting a base type`` () =
        let ast = parseWithPosDecl  ("type MyClassDerived1() =   \n" +
                                     "  inherit MyClassBase1()   \n" +
                                     "  override u.function1(a: int) = a + 1")        
        AssertAreEqual [Var ("a", loc(33,34,3,3));Var ("a", loc(23,24,3,3))] (findAllReferences (loc (23,24,3,3)) ast)

    [<Test>]
    member this.``Find usages in a Try finally``() =
        let ast = parseWithPosDecl ("let divide x y =   \n" +
                                    "      try          \n" +  
                                    "          x / y    \n" +
                                    "      finally      \n" +
                                    "        printfn \"%A %A\" x y ")
        AssertAreEqual [Var ("x", loc(24,25,5,5));Var ("x", loc(10,11,3,3));Var ("x", loc(11,12,1,1))] (findAllReferences (loc (11,12,1,1)) ast)
        AssertAreEqual [Var ("y", loc(26,27,5,5));Var ("y", loc(14,15,3,3));Var ("y", loc(13,14,1,1))] (findAllReferences (loc (13,14,1,1)) ast)

    [<Test>]
    member this.``Find usages in a for with a PWild as a bound variable``() = 
        let ast = parseWithPosDecl ("let Padded initialAlignment (v:byte[]) = \n" +
                                    "      [| yield! v                        \n" +
                                    "         for _ in 1..(4 - (initialAlignment + v.Length) % 4) % 4 do  \n" +
                                    "             yield 0x0uy |]")
        AssertAreEqual [Var ("v", loc(46,47,3,3));Var ("v", loc(16,17,2,2));Var ("v", loc(29,30,1,1))] (findAllReferences (loc (29,30,1,1)) ast)

    [<Test>]
    member this.``Find usages in typed quotation``() =
        let ast = parseWithPosDecl ("let x = 42 \n" +
                                    "let y = <@ x + 1 @>")
        AssertAreEqual [Var ("x", loc(11,12,2,2));Var ("x", loc(4,5,1,1))] (findAllReferences (loc (4,5,1,1)) ast)
        
    [<Test>]
    member this.``Find usages in un-typed quotation``() =
        let ast = parseWithPosDecl ("let x = 42 \n" +
                                    "let y = <@@ x + 1 @@>")
        AssertAreEqual [Var ("x", loc(12,13,2,2));Var ("x", loc(4,5,1,1))] (findAllReferences (loc (4,5,1,1)) ast)

    [<Test>]
    member this.``Find usages in the presence of unit of measures``() =   
        let ast = parseWithPosDecl ("[<Measure>] type kg          \n" +
                                    "let x = 42.5<kg> \n" +
                                    "let y = x + 12.5<kg>");
        AssertAreEqual [Var ("x", loc(8,9,3,3));Var ("x", loc(4,5,2,2))] (findAllReferences (loc (4,5,2,2)) ast)

    [<Test>]
    member this.``Find usages in the presence of unit of measures power``() =   
        let ast = parseWithPosDecl ("[<Measure>] type cm \n" +
                                    "[<Measure>] type ml = cm^3  \n" +
                                    "let x = 42.5<ml> \n" +
                                    "let y = x + 12.5<cm^3>");
        AssertAreEqual [Var ("x", loc(8,9,4,4));Var ("x", loc(4,5,3,3))] (findAllReferences (loc (4,5,3,3)) ast)
    
    [<Test>]
    member this.``Find usages in the presence of unit of measure divide``() =
        let ast = parseWithPosDecl ("[<Measure>] type foo \n" +
                                    "[<Measure>] type bar = foo/3  \n" +
                                    "let x = 42.5<bar> \n" +
                                    "let y = x + 12.5<foo/3>");
        AssertAreEqual [Var ("x", loc(8,9,4,4));Var ("x", loc(4,5,3,3))] (findAllReferences (loc (4,5,3,3)) ast)

    [<Test>]
    member this.``Find usages in the presence of a type test``() =
        let ast = parseWithPosDecl ("let x = 2 :? double \n" +
                                    "let y = x :? int ")
        AssertAreEqual [Var ("x", loc(8,9,2,2));Var ("x", loc(4,5,1,1))] (findAllReferences (loc (4,5,1,1)) ast)

    [<Test>]
    member this.``Find usages in the presence of an anoymous measure``() =
        let ast = parseWithPosDecl ("[<Measure>]                \n" +
                                    "type m                     \n" +
                                    "                           \n" +
                                    "let x = 0.0<_>             \n" +
                                    "let y = x + 4.0<m>           ")
        AssertAreEqual [Var ("x", loc(8,9,5,5));Var ("x", loc(4,5,4,4))] (findAllReferences (loc (4,5,4,4)) ast)

    [<Test>]
    member this.``Find usages in ands patterns``() = 
        let ast = parseWithPosDecl ("let detectZeroAND point =               \n" +
                                    "       match point with                 \n" +
                                    "       | (0, 0) -> 0                    \n" +
                                    "       | (var1, var2) & (0, _) -> var1     \n" +
                                    "       | (var1, var2)  & (_, 0) -> var2    \n" +
                                    "       | _ -> 3")
        AssertAreEqual [Var ("var1", loc(34,38,4,4));Var ("var1", loc(10,14,4,4))] (findAllReferences (loc (10,14,4,4)) ast)
        AssertAreEqual [Var ("var2", loc(35,39,5,5));Var ("var2", loc(16,20,5,5))] (findAllReferences (loc (16,20,5,5)) ast)
        
    [<Test>]
    member this.``Find usages in the presence of statically resolved typed parameters``() =    
        let ast = parseWithPosDecl ("let inline joinM b m =          \n" +
                                    "   let (>>=) m f = (^x: (member Bind: ^m -> (^n -> ^n) -> ^n) b, m, f) \n" +
                                    "   m >>= id")   
        AssertAreEqual [Var ("m", loc(3,4,3,3));Var ("m", loc(19,20,1,1))] (findAllReferences (loc (19,20,1,1)) ast)
        AssertAreEqual [Var ("m", loc(65,66,2,2));Var ("m", loc(13,14,2,2))] (findAllReferences (loc (13,14,2,2)) ast)
        
    [<Test>]
    member this.``Find usages in a type extension``() =
        let ast = parseWithPosDecl ("type A () = do () with member x.A b c d = b + c * d")
        AssertAreEqual [Var ("b", loc(42,43,1,1));Var ("b", loc(34,35,1,1))] (findAllReferences (loc (34,35,1,1)) ast)
        AssertAreEqual [Var ("c", loc(46,47,1,1));Var ("c", loc(36,37,1,1))] (findAllReferences (loc (36,37,1,1)) ast)
        AssertAreEqual [Var ("d", loc(50,51,1,1));Var ("d", loc(38,39,1,1))] (findAllReferences (loc (50,51,1,1)) ast)

    [<Test>]
    member this.``Find usages when using record aliases`` () =
        let ast = parseWithPosDecl("type AParameters = { a : int }\n" +
                                   "type X = | A of AParameters | B\n" +
                                   "let f (r : X) =\n" +
                                   " match r with\n" +
                                   " | X.A ( { a = aValue } as t )-> aValue\n" +
                                   " | X.B -> 0\n")
        AssertAreEqual [Var ("aValue", loc(33,39,5,5));Var ("aValue", loc(15,21,5,5))] (findAllReferences (loc (15,21,5,5)) ast)

    [<Test>]
    member this.``Find usages in the presence of optional arguments in class members``() =
        let ast = parseWithPosDecl ("type Foo() = member this.Foo ?x = defaultArg x 42")
        AssertAreEqual [Var ("x", loc(45,46,1,1));Var ("x", loc(30,31,1,1))] (findAllReferences (loc (30,31,1,1)) ast)

    [<Test>]
    member this.``Find usages should distinguish between two different discriminated unions with the same constructor name``() =
        let ast = parseWithPosDecl ("type Foo = Var of string  \n" +
                                    "type Bar = Var of string  \n" +
                                    "let x = Foo.Var \"Hello\" \n" +
                                    "let y = Bar.Var \"World\" ")
        AssertAreEqual [Var ("Foo.Var", loc(12,15,3,3));Var ("Foo.Var", loc(11,14,1,1))] (findAllReferences (loc (12,15,3,3)) ast)
        AssertAreEqual [Var ("Bar.Var", loc(12,15,4,4));Var ("Bar.Var", loc(11,14,2,2))] (findAllReferences (loc (12,15,4,4)) ast)        
        AssertAreEqual [Var ("Foo.Var", loc(12,15,3,3));Var ("Foo.Var", loc(11,14,1,1))] (findAllReferences (loc (11,14,1,1)) ast)
        AssertAreEqual [Var ("Bar.Var", loc(12,15,4,4));Var ("Bar.Var", loc(11,14,2,2))] (findAllReferences (loc (11,14,2,2)) ast)

    [<Test>]
    member this.``Find usages should distinguish between two different discriminated unions in a match expression``() =
        let ast = parseWithPosDecl ("type Foo = Yes | No  \n" +
                                    "type Bar = Yes | No  \n" +
                                    "let f x = match x with \n" +
                                    "          | Bar.Yes -> Foo.Yes \n" +
                                    "          | Bar.No -> Foo.No ")
        AssertAreEqual [Var ("Foo.Yes", loc(27,30,4,4));Var ("Foo.Yes", loc(11,14,1,1))] (findAllReferences (loc (11,14,1,1)) ast)
        AssertAreEqual [Var ("Bar.Yes", loc(16,19,4,4));Var ("Bar.Yes", loc(11,14,2,2))] (findAllReferences (loc (11,14,2,2)) ast)
        AssertAreEqual [Var ("Foo.No", loc(26,28,5,5));Var ("Foo.No", loc(17,19,1,1))] (findAllReferences (loc (17,19,1,1)) ast)
        AssertAreEqual [Var ("Bar.No", loc(16,18,5,5));Var ("Bar.No", loc(17,19,2,2))] (findAllReferences (loc (17,19,2,2)) ast)

    [<Test>]
    member this.``Find usages of long name identifiers``() = 
        let ast = parseWithPosDecl ("let ``my function`` ``this value`` = ``this value``")
        AssertAreEqual [Var ("this value", loc(37,51,1,1));Var ("this value", loc(20,34,1,1))] (findAllReferences (loc (20,34,1,1)) ast)

    [<Test>]
    member this.``Find usages in the presence of a Params array attribute`` () =
        let ast = parseWithPosDecl (@"  [<AttributeUsage(AttributeTargets.Method, AllowMultiple = true)>]
                                        type TestAttribute([<ParamArray>] parameters: obj[])  =
                                            inherit Attribute()
                                            member this.Parameters = parameters")         
        AssertAreEqual [Var ("parameters", loc(69,79,4,4));Var ("parameters", loc(74,84,2,2))] (findAllReferences (loc (69,79,4,4)) ast)
        AssertAreEqual [Var ("parameters", loc(69,79,4,4));Var ("parameters", loc(74,84,2,2))] (findAllReferences (loc (74,84,2,2)) ast)

    [<Test>]
    member this.``Find usages in the presence of static methods with parameters`` () = 
        let ast = parseWithPosDecl (" type Sample (code : string) =           \n" +
                                    "     member this.Encoding = code         \n" +
                                    "     static member Decode code = code      ")
        AssertAreEqual [Var ("code", loc(33,37,3,3));Var ("code", loc(26,30,3,3))] (findAllReferences (loc (26,30,3,3)) ast)

    [<Test>]
    member this.``Find usages in the presence of static methods with no parameters`` () = 
        let ast = parseWithPosDecl (" type Sample (code : string, value: string) =           \n" +
                                    "     member this.Encoding = code         \n" +
                                    "     static member Decode = value      ")
        AssertAreEqual [Var ("value", loc(28,33,3,3));Var ("value", loc(29,34,1,1))] (findAllReferences (loc (28,33,3,3)) ast)

    [<Test>]
    member this.``Find usages in the presence of static methods with multiple parameters`` () = 
        let ast = parseWithPosDecl (" type Sample (code : string) =           \n" +
                                    "     member this.Encoding = code         \n" +
                                    "     static member Decode (code, other) = (code, other)   ")
        AssertAreEqual [Var ("code", loc(43,47,3,3));Var ("code", loc(27,31,3,3))] (findAllReferences (loc (27,31,3,3)) ast)
        AssertAreEqual [Var ("other", loc(49,54,3,3));Var ("other", loc(33,38,3,3))] (findAllReferences (loc (33,38,3,3)) ast)

    [<Test>]
    member this.``Find usages in the presence of a Hash constraint`` () =
        let ast = parseWithPosDecl ("let something: #IEnumerable option = None \n" +
                                    "let foo = something ")
        AssertAreEqual [Var ("something", loc(10,19,2,2));Var ("something", loc(4,13,1,1))] (findAllReferences (loc (4,13,1,1)) ast)

    [<Test>]
    member this.``Find usages of Value constructor when fully qualified``() =
        let ast = parseWithPosDecl ("type Foo = Yes | No     \n" + 
                                    "type Bar = Yes | No     \n" + 
                                    "let x = function        \n" + 
                                    "        | Foo.Yes -> 0  \n" + 
                                    "        | Foo.No -> 1   \n" +
                                    "let y = function        \n" + 
                                    "        | Bar.Yes -> 0  \n" + 
                                    "        | Bar.No -> 1   ")
        AssertAreEqual [Var ("Foo.Yes", loc(14,17,4,4));Var ("Foo.Yes", loc(11,14,1,1))] (findAllReferences (loc (11,14,1,1)) ast)
        AssertAreEqual [Var ("Foo.No", loc(14,16,5,5));Var ("Foo.No", loc(17,19,1,1))] (findAllReferences (loc (17,19,1,1)) ast)
        AssertAreEqual [Var ("Bar.Yes", loc(14,17,7,7));Var ("Bar.Yes", loc(11,14,2,2))] (findAllReferences (loc (11,14,2,2)) ast)
        AssertAreEqual [Var ("Bar.No", loc(14,16,8,8));Var ("Bar.No", loc(17,19,2,2))] (findAllReferences (loc (17,19,2,2)) ast)

    [<Test>]
    member this.``Find usages of Value constructor when not fully qualified``() =
        let ast = parseWithPosDecl ("type Foo = Yes | No     \n" + 
                                    "type Bar = Yes | No     \n" + 
                                    "let x = function        \n" + 
                                    "        | Yes -> 0      \n" + 
                                    "        | No -> 1         ")
        let xs = (findAllReferences (loc (11,14,2,2)) ast)
        AssertAreEqual [Var ("Bar.Yes", loc(10,13,4,4));Var ("Bar.Yes", loc(11,14,2,2))] xs
        AssertAreEqual [Var ("Bar.No", loc(10,12,5,5));Var ("Bar.No", loc(17,19,2,2))] (findAllReferences (loc (17,19,2,2)) ast)

    [<Test>]
    member this.``Find usages in the presence of record pattern matching in for loop``() =
        let ast = parseWithPosDecl (" for {Button=b} as sq in squares do\n" +
                                    "    b.Click.Add(fun _ ->  onClick sq ) ")
        AssertAreEqual [Var ("b", loc(4, 5, 2, 2));Var ("b", loc(13, 14, 1, 1))] (findAllReferences (loc(13, 14, 1, 1)) ast)

    [<Test>]
    member this.``Find usages of discriminated unions value constructors in function application``() =
        let ast = parseWithPosDecl ("type Foo = Yes | No  \n" +
                                    "let f = function      \n" +
                                    "        | Yes -> 0    \n" +
                                    "        | No -> 1     \n" +
                                    "(f No) |> printf \"%A\" \n")
        AssertAreEqual [Var ("Foo.No", loc(3,5,5,5));Var ("Foo.No", loc(10,12,4,4));Var ("Foo.No", loc(17,19,1,1))] (findAllReferences (loc (17,19,1,1)) ast)
       
    [<Test>]
    member this.``Find usages of identifiers bound in left side pattern match of Algebraic Data Type``() =
         let ast = parseWithPosDecl ("type Foo = Foo of int * int  \n" +
                                     "let (Foo(x,y)) = Foo(5,7)    \n" +
                                     "let z = x")
         AssertAreEqual [Var ("x", loc(8,9,3,3));Var ("x", loc(9,10,2,2))] (findAllReferences (loc (9,10,2,2)) ast)

      