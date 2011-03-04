module CompilerToAstTypedTests

open NUnit.Framework
open Ast
open CompilerToTypedAst
open System.IO
open AstCatamorphisms

let path = sprintf "%s\\%s" (Directory.GetCurrentDirectory()) "test.fs" 

let parseWithPosDecl s = 
        File.WriteAllText("test.fs", s)        
        let [xs:_] = parseToTypedAst [path]
        xs 

let parse s = s |> parseWithPosDecl |> (fun (Exp xs) -> xs)

let toS x = sprintf "%A" x

let AssertAreEqual x y = Assert.AreEqual(toS x, toS y)


[<TestFixture>]
type CompilerToAstTypedTests() =

    [<Test>]
    member this.Const() =        
        Assert.IsTrue ([Lit(Integer 42)] = parse "42")

    [<Test>]
    member this.SimpleDecls() =        
        AssertAreEqual [Let(false,[PVar "x", Lit(Integer 42)], Lit(Unit))] (parse "let x = 42")