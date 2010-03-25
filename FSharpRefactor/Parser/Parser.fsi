module Parser

open Tokenizer

type Expression = | Literal of Token   
                  | Name of Token            
                  | Binding of Expression * Expression
                  | BinaryExpression of Token * Expression * Expression 

val parseCode : seq<Token> -> Expression option