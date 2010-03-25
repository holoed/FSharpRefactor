module ParserTests

open Tokenizer
open Parser
open NUnit.Framework

[<TestFixture>]
type ParserTests() =
    
    [<Test>]
    member this.BindingToValue() =
        Assert.IsTrue (Some(Binding (Name (Identifier "x"), Literal (IntegerLiteral 42))) = 
                            let (Some x) = tokenize "let x = 42" in 
                            let ret = parseCode x in ret)

    [<Test>]
    member this.BindingToArithmeticExpression() =
        Assert.IsTrue (Some(Binding (Name (Identifier "x"), BinaryExpression (SymbolOp "*", Literal (IntegerLiteral 2), Literal (IntegerLiteral 3)))) = 
                            let (Some x) = tokenize "let x = 2 * 3" in 
                            let ret = parseCode x in ret)
                                                            
