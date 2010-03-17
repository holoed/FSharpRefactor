module Tokenizer

type Token = Token of string

val tokenize : seq<char> -> Token seq option