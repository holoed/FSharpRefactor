// Learn more about F# at http://fsharp.net

module CompilerToAstTests

open NUnit.Framework
open Ast
open CompilerToAst
open System.IO

let parse s = 
        File.WriteAllText("test.fs", s)
        let path = sprintf "%s\\%s" (Directory.GetCurrentDirectory()) "test.fs" 
        let [xs:_] = parseToAst [path]
        xs

[<TestFixture>]
type CompilerToAstTests() =

    [<Test>]
    member this.Const() =        
        Assert.IsTrue ([Lit(Integer 42)] = parse "42")

    [<Test>]
    member this.SimpleDecls() =        
        Assert.IsTrue ([Let(PVar "x", Lit(Integer 42), Lit(Unit))] = parse "let x = 42")
        Assert.IsTrue ([Let(PVar "x", Lit(Integer 42), Lit(Unit)); Let(PVar "x", Lit(Integer 24), Lit(Unit))] = parse "let x = 42\nlet x = 24")

    [<Test>]
    member this.FunctionsDecls() =        
        Assert.IsTrue ([Let(PApp(PVar "f", PVar "x"), Var "x", Lit(Unit)) ] = parse "let f x = x")
        Assert.IsTrue ([Let(PApp(PApp(PVar "f", PVar "x"), PVar "y"), Var "y", Lit(Unit)) ] = parse "let f x y = y")        
        Assert.IsTrue ([Let(PApp(PApp(PApp(PVar "f", PVar "x"), PVar "y"), PVar "z"), Var "z", Lit(Unit)) ] = parse "let f x y z = z")
   
        
