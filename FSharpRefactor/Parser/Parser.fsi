module Parser

open Tokenizer

type Type = Unknown | Int | Double | Float | String | Char | Tuple of Type list | Function of Type list * Type

type Expression = | Literal of Token
                  | TypedLiteral of Token * Type  
                  | Name of Token       
                  | TypedName of Token * Type
                  | BinaryExpression of Token * Expression * Expression 
                  | ValueBinding of Expression * Expression
                  | FunctionBinding of Token * Expression list * Expression
              
val parseCode : seq<Token> -> Expression option