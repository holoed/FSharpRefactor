module ASTAnalysisTests

open Ast
open CompilerToAst
open CompilerToAstTests
open ASTAnalysis
open NUnit.Framework

[<TestFixture>]
type ASTAnalysisTests() =

    [<Test>]
    member this.FindIdentifierByPos() =    
                              //123456789
        let ast = parseWithPos "let x = s"        
        AssertAreEqual [Var ("x", loc(4,5,1,1))] (findAllReferences ast (loc(4,5,1,1)))
        AssertAreEqual [Var ("s", loc(8,9,1,1))] (findAllReferences ast (loc(8,9,1,1)))

    [<Test>]
    member this.FindIdentifiersPosDescending() =    
        AssertAreEqual [Var ("x", loc(4,5,1,1));Var ("x", loc(8,9,1,1))] (findAllReferences (parseWithPos "let x = x") (loc(4,5,1,1)))
        AssertAreEqual [Var ("x", loc(4,5,1,1));Var ("x", loc(8,9,1,1));Var ("x", loc(12,13,1,1))] (findAllReferences (parseWithPos "let x = x + x") (loc(4,5,1,1)))
        AssertAreEqual [Var ("x", loc(6,7,1,1));Var ("x", loc(10,11,1,1));Var ("x", loc(14,15,1,1))] (findAllReferences (parseWithPos "let f x = x * x") (loc(6,7,1,1)))
        AssertAreEqual [Var ("x", loc(6,7,1,1));Var ("x", loc(14,15,1,1))] (findAllReferences (parseWithPos "let f x = 1 + x") (loc(6,7,1,1)))
       


