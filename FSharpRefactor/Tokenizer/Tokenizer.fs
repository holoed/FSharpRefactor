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

let SeqToString xs = 
   if (Seq.isEmpty xs) then 
        (String.Empty, { srcLine = 0; srcColumn = 0 })
   else
       let (r, c, _) = Seq.head xs
       let xs' = new System.String (Seq.toArray (Seq.map (fun (_, _, x) -> x) xs))
       (xs', { srcLine = r; srcColumn = c })

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

let integerLiteral  = parser { let! x = number
                               return IntegerLiteral (SeqToString x) }

let stringLiteral  = parser { let! _ = symb "\""
                              let! s = many1 (letter +++ digit +++ pwhitespace)
                              let! _ = symb "\""
                              return StringLiteral (SeqToString s)  }

let whitespace  = parser { let! x = whitespaces
                           return Identifier (SeqToString x) }

let tokenize s = 
    let is = indexStr s
    match parseString (sepBy1 (keyword +++ identifier +++ integerLiteral +++ stringLiteral +++ symbolOp) whitespace) is with
    | Empty -> None
    | Cons(x, _) -> Some x
    
                 