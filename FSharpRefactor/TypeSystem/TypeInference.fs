module TypeInference

open Parser
open Tokenizer

type Type = Int | String


let integer exp = match exp with
                  | Literal (IntegerLiteral x) -> Literal (IntegerLiteral x, Int)

let infer exp = integer exp