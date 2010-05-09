module Tokenizer

open System
open Tokens
open StringUtils
open StringToIndexedString
open SharpMalib.Parser.ParserMonad

let digit = sat (fun (_, _, x) -> Char.IsDigit x)

let lower = sat (fun (_, _, x) -> Char.IsLower x)

let upper = sat (fun (_, _, x) -> Char.IsUpper x)

let letter = lower +++ upper

let word = many1 letter

let number = many1 digit

let pwhitespace = sat (fun (_, _, x) -> Char.IsWhiteSpace x)

let whitespaces = many pwhitespace

let char ch = sat (fun (_,_,x) -> ch = x)

let token p = parser { let! x = p
                       let! _ = whitespaces
                       return x }

let rec stringp s = parser { match s with
                              | Empty -> return Seq.empty
                              | Cons (x, xs) -> let! x' = char x
                                                let! xs' = stringp xs
                                                return cons x' xs' }

let symb cs = token (stringp cs)
            
let (|+) p q = parser { let! x = p
                        let! y = q
                        return Seq.append x y } +++ p

let (+|) p q = parser { let! x = p
                        let! y = q
                        return Seq.append x y } +++ q

let SeqToString xs f = 
   if (Seq.isEmpty xs) then 
        Token (f "", { srcLine = 0; srcColumn = 0 })
   else
       let (r, c, _) = Seq.head xs
       let xs' = new System.String (Seq.toArray (Seq.map (fun (_, _, x) -> x) xs))
       Token (f xs', { srcLine = r; srcColumn = c })

let keyword  = parser { let! x = stringp "let" +++ 
                                 stringp "in"  +++
                                 stringp "fun"
                        return SeqToString x Keyword }

let identifier  = parser { let! x = (stringp "_") +| (word |+ number) |+ (stringp "\'")
                           return SeqToString x Identifier }

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
                         return SeqToString x Symbol }

let integerLiteral  = parser { let! x = number
                               return SeqToString x IntegerLiteral }

let stringLiteral  = parser { let! _ = symb "\""
                              let! s = many1 (letter +++ digit +++ pwhitespace)
                              let! _ = symb "\""
                              return SeqToString s StringLiteral }

let whitespace  = parser { let! x = whitespaces
                           return SeqToString x Identifier }

let tokenize s = 
    let is = indexStr s
    match parseString (sepBy1 (keyword +++ identifier +++ integerLiteral +++ stringLiteral +++ symbolOp) whitespace) is with
    | Empty -> None
    | Cons(x, _) -> Some x
    
                 