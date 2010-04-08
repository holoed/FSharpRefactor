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
        Assert.IsTrue (Some(BinaryExpression (SymbolOp "*", Name (Identifier "x"), Literal (IntegerLiteral 42))) = 
        parse "x * 42")

    [<Test>]
    member this.BindingToValue() =
        Assert.IsTrue (Some(ValueBinding (Name (Identifier "x"), Literal (IntegerLiteral 42))) = 
        parse "let x = 42")

    [<Test>]
    member this.BindingToArithmeticExpression() =
        Assert.IsTrue (Some(ValueBinding (Name (Identifier "x"), BinaryExpression (SymbolOp "*", Literal (IntegerLiteral 2), Literal (IntegerLiteral 3)))) = 
        parse "let x = 2 * 3")
                                                            
    [<Test>]
    member this.FunctionBinding() = 
        Assert.IsTrue (Some(FunctionBinding (Identifier "f", [Name (Identifier "x")], Name (Identifier "x")))  = 
        parse "let f x = x")

    [<Test>]
    member this.FunctionBinding2() =         
        Assert.IsTrue (Some(FunctionBinding (Identifier "square", [Name (Identifier "x")], BinaryExpression(SymbolOp "*", Name (Identifier "x"), Name (Identifier "x"))))  = 
        parse "let square x = x * x")