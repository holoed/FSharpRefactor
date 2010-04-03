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
    member this.BindingToValue() =
        Assert.IsTrue (Some(Binding (Name (Identifier "x"), Literal (IntegerLiteral 42))) = parse "let x = 42")

    [<Test>]
    member this.BindingToArithmeticExpression() =
        Assert.IsTrue (Some(Binding (Name (Identifier "x"), BinaryExpression (SymbolOp "*", Literal (IntegerLiteral 2), Literal (IntegerLiteral 3)))) = parse "let x = 2 * 3")
                                                            
