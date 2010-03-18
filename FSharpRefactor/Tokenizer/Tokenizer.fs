module Tokenizer

open StringUtils
open SharpMalib.Parser.ParserMonad

type Token = | Identifier of string
             | IntegerLiteral of int
             | StringLiteral of string
             | SymbolOp of string
             | Keyword of string

let SeqToString xs = new System.String (Seq.toArray xs)

let keyword  = parser { let! x = stringp "let" +++ stringp "in"
                        return Keyword (SeqToString x) }

let identifier  = parser { let! x = word
                           return Identifier (SeqToString x) }

let symbolOp  = parser { let! x = symb "=" +++  symb "/" +++ symb "*" +++ symb "+" +++ symb ">" +++ symb "<"
                         return SymbolOp (SeqToString x) }

let integerLiteral  = parser { let! x = integer
                               return IntegerLiteral x }

let stringLiteral  = parser { let! _ = symb "\""
                              let! s = many1 (letter +++ digit +++ whitespace)
                              let! _ = symb "\""
                              return StringLiteral (SeqToString s)  }

let whitespace  = parser { let! x = whitespaces
                           return Identifier (SeqToString x) }

let tokenize s = 
    match parseString (sepBy1 (keyword +++ identifier +++ integerLiteral +++ stringLiteral +++ symbolOp) whitespace) s with
    | Empty -> None
    | Cons(x, _) -> Some x
    
                 