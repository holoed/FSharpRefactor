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

let setEnv (pos:Pos) p = Parser(fun _ -> fun ps -> parse p pos ps)

let env = Parser(fun pos -> fun ps -> [(pos, ps)])

let fetch = Parser(fun _ -> fun (PString(pos, s)) -> [((pos, s), PString(pos, s))])

let newstate (PString((l, c), Cons(x,xs))) = 
   let newpos = match x with
                | '\n' -> (l + 1, 0)
                | '\t' -> (l, ((c / 8) + 1) * 8)
                | _    -> (l, c + 1)                 
   PString(newpos, xs)                 
   
let onside (l, c) (dl, dc) = (c > dc) || (l = dl)    

// Deterministic choice operator
let (+++) p q = Parser (fun pos -> fun s -> match parse p pos s with
                                            | []-> parse q pos s
                                            | result -> result)

let item = Parser(fun defpos -> fun ps -> match (string ps) with
                                          | Empty -> []
                                          | Cons(x, _) ->      
                                               let (PString(pos, xs)) = newstate ps 
                                               if (onside pos defpos) then 
                                                   [(x, PString(pos, xs))]
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


let off p = parser { let! (dl, dc) = env
                     let! ((l,c), ps) = fetch
                     let debug = Seq.toList ps
                     if (c >= dc) then
                       return! setEnv (l, dc) p }

let rec many1_offside p = parser { let! (pos, _) = fetch
                                   let! vs = setEnv pos (many1 (off p)) 
                                   return vs }
and many_offside p = many1_offside p +++ parser { return Seq.empty }


let ident = parser { let! x = lower
                     let! xs = many alphanum
                     return cons x xs }

let spaces = parser { let! _ = many1 (sat (fun ch -> Char.IsWhiteSpace ch || ch = '\n' || ch = '\r' || ch = '\t'))
                      return () }

let comment = parser { let! _ = stringp "//"
                       let! _ = many1 (sat (fun ch -> ch <> '\n'))
                       return () }

let junk = parser { let! _ = setEnv (0,-1) (many (spaces +++ comment))
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

let decimals =
   let toDigit x = float ((int x) - (int '0'))
   let eval xs = xs |> Seq.mapi (fun i x -> (toDigit x) * (Math.Pow(10.0, -(float i))))
                    |> Seq.reduce (+)
   parser { let! xs = token number
            return (eval xs) / 10.0 }

let integer = 
  let negate x = -x
  let op = parser { let! _ = char '-'
                    return negate } +++ parser { return id }
  parser { let! f = op
           let! n = natural
           return f n }

let float = parser { let! x = integer
                     let! _ = char '.'
                     let! y = decimals
                     let x = float x
                     return if x >= 0.0 then x + y else x - y}

