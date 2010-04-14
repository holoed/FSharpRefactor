module TypeInference

open Parser
open Tokenizer

type Type = Int | String

val infer : Expression<Token> -> Expression<Token * Type>
