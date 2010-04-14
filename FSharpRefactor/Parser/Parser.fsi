module Parser

open Tokenizer

type Expression<'a> = | Literal of 'a
                      | TypedLiteral of 'a
                      | Name of 'a       
                      | TypedName of 'a
                      | BinaryExpression of 'a * Expression<'a> * Expression<'a>
                      | ValueBinding of Expression<'a> * Expression<'a>
                      | FunctionBinding of 'a * Expression<'a> list * Expression<'a>
              
val parseCode : seq<Token> -> Expression<Token> option