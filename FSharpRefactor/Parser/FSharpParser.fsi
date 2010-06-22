module FSharpParser

open Ast

val parseExp : seq<char> -> Exp option
