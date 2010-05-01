module ParserTests

open Tokenizer
open Parser
open NUnit.Framework

type MaybeBuilder() =
    member this.Bind (m, f) = Option.bind f m
    member this.Return x = Some x

let maybe = MaybeBuilder()

let parse s = maybe  { let! x = tokenize s 
                       let! y = parseCode x
                       return y }

[<TestFixture>]
type ParserTests() =
    
    [<Test>]
    member this.ArithmeticExpression() =        
        Assert.IsTrue (Some(InfixApp (Var (Ident "x"), VarOp (Symbol "*"), Lit (Integer 42))) = 
        parse "x * 42")

    [<Test>]
    member this.FunctionApplication() = 
        Assert.IsTrue (Some(App ( Var (Ident "f"), Var (Ident "x"))) = 
        parse "f x")

    [<Test>]
    member this.BindingToValue() =
        Assert.IsTrue (Some(Let (Ident "x", Lit (Integer 42))) = 
        parse "let x = 42")

    [<Test>]
    member this.BindingToArithmeticExpression() =
        Assert.IsTrue (Some(Let (Ident "x", InfixApp ( Lit (Integer 2), VarOp (Symbol "*"), Lit (Integer 3)))) = 
        parse "let x = 2 * 3")
                                                            
    [<Test>]
    member this.BindingToFunctionApplication() = 
        Assert.IsTrue (Some(Let (Ident "x", App ( Var (Ident "f"), Var (Ident "x")))) = 
        parse "let x = f x")
//
//    [<Test>]
//    member this.FunctionBinding2() =         
//        Assert.IsTrue (Some(FunctionBinding (Identifier "square", [Name (Identifier "x")], BinaryExpression(SymbolOp "*", Name (Identifier "x"), Name (Identifier "x"))))  = 
//        parse "let square x = x * x")