// Learn more about F# at http://fsharp.net

module CompilerToAstTests

open NUnit.Framework
open Ast
open CompilerToAst
open System.IO

let parse s = 
        File.WriteAllText("test.fs", s)
        let path = sprintf "%s\\%s" (Directory.GetCurrentDirectory()) "test.fs" 
        let [[x:_]:_] = parseToAst [path]
        x

[<TestFixture>]
type CompilerToAstTests() =

    [<Test>]
    member this.Const() =        
        Assert.IsTrue (Lit(Integer 42) = parse "42")

    [<Test>]
    member this.LocalDefinitions() =        
        Assert.IsTrue (Let(PVar "x", Lit(Integer 42), Lit(Unit)) = parse "let x = 42")
        
