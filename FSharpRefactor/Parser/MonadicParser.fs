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

module MonadicParser

open System
open Utils

type Pos = int * int

type PString = PString of Pos * seq<char>

let pos (PString(pos, _)) = pos

let string (PString(_, s)) = s

type Parser<'a> = Parser of (PString -> ('a * PString) list)

let parse (Parser f) x = f x
                            
type ParserMonad() =
    //a -> m a
    member this.Return x = Parser(fun ps -> [(x, ps)])  
    
    //m a -> (a -> m b) -> m b
    member this.Bind (m, f) = Parser(fun ps -> match parse m ps with
                                               | [] -> []
                                               | [(x, ps)] -> parse (f x) ps)

    // m a -> m a
    member this.ReturnFrom (x:Parser<'a>) = x

    // () -> m a
    member this.Zero () = Parser(fun ps -> [])
                                               
let parser = ParserMonad()   

