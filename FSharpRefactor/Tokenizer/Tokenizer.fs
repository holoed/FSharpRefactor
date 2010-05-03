module Tokenizer

open Tokens
open StringUtils
open SharpMalib.Parser.ParserMonad
            
let (|+) p q = parser { let! x = p
                        let! y = q
                        return Seq.append x y } +++ p

let (+|) p q = parser { let! x = p
                        let! y = q
                        return Seq.append x y } +++ q

let SeqToString xs = new System.String (Seq.toArray xs)

let keyword  = parser { let! x = stringp "let" +++ 
                                 stringp "in"  +++
                                 stringp "fun"
                        return Keyword (SeqToString x) }

let identifier  = parser { let! x = (stringp "_") +| (word |+ number) |+ (stringp "\'")
                           return Identifier (SeqToString x) }

let symbolOp  = parser { let! x = symb "->" +++
                                  symb "=" +++ 
                                  symb "/" +++ 
                                  symb "*" +++ 
                                  symb "+" +++ 
                                  symb ">" +++ 
                                  symb "<" +++
                                  symb "," +++
                                  symb "." +++
                                  symb "(" +++
                                  symb ")"                                  
                         return Symbol (SeqToString x) }

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
    
                 