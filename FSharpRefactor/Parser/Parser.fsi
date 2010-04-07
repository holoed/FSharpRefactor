module Parser

open Tokenizer

type Expression = | Literal of Token   
                  | Name of Token            
                  | BinaryExpression of Token * Expression * Expression 
                  | ValueBinding of Expression * Expression
                  | FunctionBinding of Token * Expression list * Expression
              
val parseCode : seq<Token> -> Expression option