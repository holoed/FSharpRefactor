// * **********************************************************************************************
// * Copyright (c) Edmondo Pentangelo. 
// *
// * This source code is subject to terms and conditions of the Microsoft Public License. 
// * A copy of the license can be found in the License.html file at the root of this distribution. 
// * By using this source code in any fashion, you are agreeing to be bound by the terms of the 
// * Microsoft Public License.
// *
// * You must not remove this notice, or any other, from this software.
// * **********************************************************************************************

module MonadicParserCombinators

open System
open Utils
open MonadicParser

let newstate (PString((l, c), Cons(x,xs))) = 
    let newpos = match x with
                 | '\n' -> (l+1, 0)
                 | '\t' -> (l, ((c / 8) + 1) * 8)
                 | _    -> (l, c + 1)                 
    PString(newpos, xs)                 
    
let onside (l, c) (dl, dc) = (c > dc) || (l = dl)    

// Deterministic choice operator
let (+++) p q = Parser (fun s -> match parse p s with
                                 | []-> parse q s
                                 | result -> result)

let item = Parser(fun ps -> match (string ps) with
                            | Empty -> []
                            | Cons(x, _) ->
                                let defp = pos ps       
                                let (PString(p, xs)) = newstate ps 
                                if (onside p defp) then 
                                    [(x, PString(p, xs))]
                                else [])

let sat p = parser { let! x = item
                     if (p x) then
                        return x }

let char x = sat (fun y -> x = y)

let lower = sat (Char.IsLower)

let upper = sat (Char.IsUpper)

let digit = sat (Char.IsDigit)

let letter = lower +++ upper

let alphanum = letter +++ digit

let rec stringp s = parser { match s with
                             | Empty -> return seq[]
                             | Cons (x, xs) -> let! _ = char x
                                               let! _ = stringp xs
                                               return cons x xs }

let chainl1 p op =         
       let rec rest l = parser { let! f = op
                                 let! r = p 
                                 return! rest (f l r) } +++ parser { return l }
       parser { let! l = p
                return! rest l }

let chainl p op l = (chainl1 p op) +++ parser { return l }


let rec many1 p = parser { let! y = p
                           let! ys = many p
                           return cons y ys }
and many p = many1 p +++ parser { return Seq.empty }

let ident = parser { let! x = lower
                     let! xs = many alphanum
                     return cons x xs }

let spaces = parser { let! _ = many1 (sat (Char.IsWhiteSpace))
                      return () }

let comment = parser { let! _ = stringp "//"
                       let! _ = many1 (sat (fun ch -> ch <> '\n'))
                       return () }

let junk = parser { let! _ = many (spaces +++ comment)
                    return () }

let token p = parser { let! v = p
                       do! junk
                       return v }
 
let symbol xs = token (stringp xs)

let identifier (ks:string list) = token (parser { let! x = ident
                                    if (not (List.exists(fun xi -> xi = seqtostring x) ks)) then
                                             return x })
let number = many1 digit

let natural = 
    let toDigit x = (int x) - (int '0')
    let op m n = 10 * m + n    
    let eval xs = xs |> Seq.map toDigit
                     |> Seq.reduce op 
    parser { let! xs = token number
             return eval xs }

let integer = 
   let negate x = -x
   let op = parser { let! _ = char '-'
                     return negate } +++ parser { return id }
   parser { let! f = op
            let! n = natural
            return f n }

