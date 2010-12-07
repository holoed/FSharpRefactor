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
    [<Ignore("Working on this...")>]
    member this.FindIdentifiersPosDescending() =    
        let ast = parseWithPos "let x = x"        
        AssertAreEqual [Var ("x", loc(4,5,1,1));Var ("x", loc(8,9,1,1))] (findAllReferences ast (loc(4,5,1,1)))
       


