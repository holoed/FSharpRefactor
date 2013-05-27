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

    [<Test>]
    member this.``Find usage of x given its definition in sample 7`` () =
        let ast = parseWithPosDecl ("let x = 42\n" + 
                                    "let y = x   " )
        AssertAreEqual [loc(8,9,2,2); loc(4,5,1,1)] (findAllReferences "x" (loc(4,5,1,1)) ast) 

    [<Test>]
    member this.``Find definition of x given its usage in sample 7`` () =
        let ast = parseWithPosDecl ("let x = 42\n" + 
                                    "let y = x   " )
        AssertAreEqual [loc(8,9,2,2); loc(4,5,1,1)] (findAllReferences "x" (loc(8,9,2,2)) ast) 

    [<Test>]
    member this.``Find usage of Var given its definition in sample 8`` () =
        let ast = parseWithPosDecl ("type Exp = Var of string\n" + 
                                    "let exp = Exp.Var(\"x\")   ")
        AssertAreEqual [loc(10,17,2,2); loc(11,14,1,1)] (findAllReferences "Exp.Var" (loc(11,14,1,1)) ast) 

    [<Test>]
    member this.``Find definition of Var given its usage in sample 8`` () =
        let ast = parseWithPosDecl ("type Exp = Var of string\n" + 
                                    "let exp = Exp.Var(\"x\")   ")
        AssertAreEqual [loc(10,17,2,2); loc(11,14,1,1)] (findAllReferences "Exp.Var" (loc(10,17,2,2)) ast) 

    [<Test>]
    member this.``Find usage of x given its definition in sample 9`` () =
        let ast = parseWithPosDecl "let f (x, y) = x"
        AssertAreEqual [loc(15,16,1,1); loc(7,8,1,1)] (findAllReferences "x" (loc(7,8,1,1)) ast) 

    [<Test>]
    member this.``Find definition of x given its usage in sample 9`` () =
        let ast = parseWithPosDecl "let f (x, y) = x"
        AssertAreEqual [loc(15,16,1,1); loc(7,8,1,1)] (findAllReferences "x" (loc(15,16,1,1)) ast) 

    [<Test>]
    member this.``Find usage of x given its definition in sample 10`` () =
        let ast = parseWithPosDecl "let f (x, _) = x"
        AssertAreEqual [loc(15,16,1,1); loc(7,8,1,1)] (findAllReferences "x" (loc(7,8,1,1)) ast) 

    [<Test>]
    member this.``Find definition of x given its usage in sample 10`` () =
        let ast = parseWithPosDecl "let f (x, _) = x"
        AssertAreEqual [loc(15,16,1,1); loc(7,8,1,1)] (findAllReferences "x" (loc(15,16,1,1)) ast) 

    [<Test>]
    member this.``Find usage of x given its definition in sample 11`` () =
        let ast = parseWithPosDecl "let f p = match p with (x,y) -> x"
        AssertAreEqual [loc(32,33,1,1); loc(24,25,1,1)] (findAllReferences "x" (loc(24,25,1,1)) ast) 

    [<Test>]
    member this.``Find usage of x given its usage in sample 11`` () =
        let ast = parseWithPosDecl "let f p = match p with (x,y) -> x"
        AssertAreEqual [loc(32,33,1,1); loc(24,25,1,1)] (findAllReferences "x" (loc(32,33,1,1)) ast) 

    [<Test>]
    member this.``Find usage of x given its definition in sample 12`` () =
        let ast = parseWithPosDecl "let xs = seq { for i in 1..5 do yield i }"
        AssertAreEqual [loc(38,39,1,1); loc(19,20,1,1)] (findAllReferences "i" (loc(19,20,1,1)) ast) 

    [<Test>]
    member this.``Find definition of x given its usage in sample 12`` () =
        let ast = parseWithPosDecl "let xs = seq { for i in 1..5 do yield i }"
        AssertAreEqual [loc(38,39,1,1); loc(19,20,1,1)] (findAllReferences "i" (loc(38,39,1,1)) ast) 

    [<Test>]
    member this.``Find usage of x given its definition in sample 13`` () =
        let ast = parseWithPosDecl "let f n x y = if n = 0 then x else y"
        AssertAreEqual [loc(28,29,1,1); loc(8,9,1,1)] (findAllReferences "x" (loc(8,9,1,1)) ast) 

    [<Test>]
    member this.``Find definition of x given its usage in sample 13`` () =
        let ast = parseWithPosDecl "let f n x y = if n = 0 then x else y"
        AssertAreEqual [loc(28,29,1,1); loc(8,9,1,1)] (findAllReferences "x" (loc(28,29,1,1)) ast) 

    [<Test>]
    member this.``Find usage of y given its definition in sample 13`` () =
        let ast = parseWithPosDecl "let f n x y = if n = 0 then x else y"
        AssertAreEqual [loc(35,36,1,1); loc(10,11,1,1)] (findAllReferences "y" (loc(10,11,1,1)) ast) 

    [<Test>]
    member this.``Find definition of y given its usage in sample 13`` () =
        let ast = parseWithPosDecl "let f n x y = if n = 0 then x else y"
        AssertAreEqual [loc(35,36,1,1); loc(10,11,1,1)] (findAllReferences "y" (loc(35,36,1,1)) ast) 

    [<Test>]
    member this.``Find usages of f given its definition when LetRec and when is only Let`` () =
        let ast    = parseWithPosDecl "let f x = f x"
        let astRec = parseWithPosDecl "let rec f x = f x"
        AssertAreEqual [loc(4,5,1,1)] (findAllReferences "f" (loc(4,5,1,1)) ast)
        AssertAreEqual [loc(14,15,1,1); loc(8,9,1,1)] (findAllReferences "f" (loc(8,9,1,1)) astRec)

    [<Test>]
    member this.``Find usages of x and y given their definition in List pattern`` () =
        let ast = parseWithPosDecl "let f [x;y] = [x;y]"
        AssertAreEqual [loc(15,16,1,1); loc(7,8,1,1)] (findAllReferences "x" (loc(7,8,1,1)) ast)
        AssertAreEqual [loc(17,18,1,1); loc(9,10,1,1)] (findAllReferences "y" (loc(9,10,1,1)) ast)

    [<Test>]
    member this.``Find usages of x in array setter given its definition`` () =
        let ast = parseWithPosDecl ("let x = 42              \n" + 
                                    "myArray.[0, 1] <- x")
        AssertAreEqual [loc(18,19,2,2); loc(4,5,1,1)] (findAllReferences "x" (loc(4,5,1,1)) ast)

    [<Test>]
    member this.``Find usages of x in array setter given its usage`` () =
        let ast = parseWithPosDecl ("let x = 42              \n" + 
                                    "myArray.[0, 1] <- x")
        AssertAreEqual [loc(18,19,2,2); loc(4,5,1,1)] (findAllReferences "x" (loc(18,19,2,2)) ast)

    [<Test>]
    member this.``Find usages of x in array getter given its definition`` () =
        let ast = parseWithPosDecl ("let x = 42              \n" + 
                                    "let z = myArray.[0, x]")
        AssertAreEqual [loc(20,21,2,2); loc(4,5,1,1)] (findAllReferences "x" (loc(4,5,1,1)) ast)

    [<Test>]
    member this.``Find usages of x in array getter given its usage`` () =
        let ast = parseWithPosDecl ("let x = 42              \n" + 
                                    "let z = myArray.[0, x]")
        AssertAreEqual [loc(20,21,2,2); loc(4,5,1,1)] (findAllReferences "x" (loc(20,21,2,2)) ast)

    [<Test>]
    member this.``Find usages of x and y given their definitions`` () = 
        let ast = parseWithPosDecl ("let (x,y) = (12,42)    \n" +
                                    "let p = (x, y) ")
        AssertAreEqual [loc(12,13,2,2); loc(7,8,1,1)] (findAllReferences "y" (loc (7,8,1,1)) ast)
        AssertAreEqual [loc(9,10,2,2); loc(5,6,1,1)] (findAllReferences "x" (loc (5,6,1,1)) ast)

    [<Test>]
    member this.``Find usages of x and y given their usages`` () = 
        let ast = parseWithPosDecl ("let (x,y) = (12,42)    \n" +
                                    "let p = (x, y) ")
        AssertAreEqual [loc(12,13,2,2); loc(7,8,1,1)] (findAllReferences "y" (loc (12,13,2,2)) ast)
        AssertAreEqual [loc(9,10,2,2); loc(5,6,1,1)] (findAllReferences "x" (loc (9,10,2,2)) ast)

    [<Test>]
    member this.``Find usages of x and y in class members given their definition in the implicit constructor.`` () = 
        let ast = parseWithPosDecl  ("type Point (x:int,y:int) = \n" +
                                     "    member this.X = x      \n" +
                                     "    member this.Y = y")
        AssertAreEqual [loc(20,21,2,2); loc(12,13,1,1)] (findAllReferences "x" (loc (12,13,1,1)) ast)
        AssertAreEqual [loc(20,21,3,3); loc(18,19,1,1)] (findAllReferences "y" (loc (18,19,1,1)) ast)

    [<Test>]
    member this.``Find usages of x and y defined in constructor given their usages in the members of the class.`` () = 
        let ast = parseWithPosDecl  ("type Point (x:int,y:int) = \n" +
                                     "    member this.X = x      \n" +
                                     "    member this.Y = y")
        AssertAreEqual [loc(20,21,2,2); loc(12,13,1,1)] (findAllReferences "x" (loc (20,21,2,2)) ast)
        AssertAreEqual [loc(20,21,3,3); loc(18,19,1,1)] (findAllReferences "y" (loc(20,21,3,3)) ast)

    [<Test>]
    member this.``Find usages of x and y in object instance construction given their definitions`` () =
        let ast = parseWithPosDecl ("let (x,y) = (12,42)    \n" +
                                    "let p = new Point(x, y)")
        AssertAreEqual [loc(21,22,2,2); loc(7,8,1,1)] (findAllReferences "y" (loc (7,8,1,1)) ast)
        AssertAreEqual [loc(18,19,2,2); loc(5,6,1,1)] (findAllReferences "x" (loc (5,6,1,1)) ast)

    [<Test>]
    member this.``Find definition of x and y given their usages in object instance construction`` () =
        let ast = parseWithPosDecl ("let (x,y) = (12,42)    \n" +
                                    "let p = new Point(x, y)")
        AssertAreEqual [loc(21,22,2,2); loc(7,8,1,1)] (findAllReferences "y" (loc (21,22,2,2)) ast)
        AssertAreEqual [loc(18,19,2,2); loc(5,6,1,1)] (findAllReferences "x" (loc (18,19,2,2)) ast)

    [<Test>]
    member this.``Find usages of x and y in object expr construction given their definitions`` () =
        let ast = parseWithPosDecl ("let (x,y) = (12,42)    \n" +
                                    "let p = { new IPoint with            \n" +
                                    "               member this.X = x      \n" +
                                    "               member this.Y = y }")
        AssertAreEqual [loc(31,32,4,4); loc(7,8,1,1)] (findAllReferences "y" (loc (7,8,1,1)) ast)
        AssertAreEqual [loc(31,32,3,3); loc(5,6,1,1)] (findAllReferences "x" (loc (5,6,1,1)) ast)

    [<Test>]
    member this.``Find definition of x and y given their usages in members of Object Expression`` () =
        let ast = parseWithPosDecl ("let (x,y) = (12,42)    \n" +
                                    "let p = { new IPoint with            \n" +
                                    "               member this.X = x      \n" +
                                    "               member this.Y = y }")
        AssertAreEqual [loc(31,32,4,4); loc(7,8,1,1)] (findAllReferences "y" (loc (31,32,4,4)) ast)
        AssertAreEqual [loc(31,32,3,3); loc(5,6,1,1)] (findAllReferences "x" (loc (31,32,3,3)) ast)

    [<Test>]
    member this.``Find usages of x in do expression given its definition or usage`` () =
        let ast = parseWithPosDecl ("let x = 42   \n" +
                                    "do write x ")
        AssertAreEqual [loc(9,10,2,2); loc(4,5,1,1)] (findAllReferences "x" (loc (4,5,1,1)) ast)
        AssertAreEqual [loc(9,10,2,2); loc(4,5,1,1)] (findAllReferences "x" (loc (9,10,2,2)) ast)

    [<Test>]
    member this.``Find usages of x in interface implementation given its definition or usage`` () =
        let ast = parseWithPosDecl ("let x = 42 \n" +
                                    "type MyClass = \n" +
                                    "     interface IFoo with \n" +
                                    "          member this.Bar () = x")
        AssertAreEqual [loc(31,32,4,4); loc(4,5,1,1)] (findAllReferences "x" (loc (4,5,1,1)) ast)
        AssertAreEqual [loc(31,32,4,4); loc(4,5,1,1)] (findAllReferences "x" (loc (31,32,4,4)) ast)

    [<Test>]
    member this.``Find usages of x in assignment given its definition or usages`` () =
        let ast = parseWithPosDecl ("let x = 42 \n" +
                                    "x <- x + 1")
        AssertAreEqual [loc(5,6,2,2); loc(0,1,2,2); loc(4,5,1,1)] (findAllReferences "x" (loc (4,5,1,1)) ast)
        AssertAreEqual [loc(5,6,2,2); loc(0,1,2,2); loc(4,5,1,1)] (findAllReferences "x" (loc (0,1,2,2)) ast)
        AssertAreEqual [loc(5,6,2,2); loc(0,1,2,2); loc(4,5,1,1)] (findAllReferences "x" (loc (5,6,2,2)) ast)

    [<Test>]
    member this.``Find usages of x in lambda expression`` () =
        let ast = parseWithPosDecl ("let f = fun x -> x")
        AssertAreEqual [loc(17,18,1,1); loc(12,13,1,1)] (findAllReferences "x" (loc (12,13,1,1)) ast)
        AssertAreEqual [loc(17,18,1,1); loc(12,13,1,1)] (findAllReferences "x" (loc (17,18,1,1)) ast)

    [<Test>]
    member this.``Find usages of x and y in lambda expression`` () =
        let ast = parseWithPosDecl ("let f = fun x -> fun y -> x + y")
        AssertAreEqual [loc(26,27,1,1); loc(12,13,1,1)] (findAllReferences "x" (loc (12,13,1,1)) ast)
        AssertAreEqual [loc(30,31,1,1); loc(21,22,1,1)] (findAllReferences "y" (loc (21,22,1,1)) ast)
