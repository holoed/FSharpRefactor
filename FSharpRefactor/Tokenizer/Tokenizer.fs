module Tokenizer

open SharpMalib.Parser.ParserMonad

type Token = Token of string

let word  = parser { let! x = word
                     return Token (new System.String (Seq.toArray x)) }

let whitespace  = parser { let! x = whitespaces
                           return Token (new System.String (Seq.toArray x)) }



let tokenize s = 
    let ret = parseString (sepBy1 word whitespace) s
    if (Seq.empty = ret) then
        None
    else
        Some (Seq.head ret)
                 