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

open System
open Ast
open FSharpParser
open TypeInf


let (Some x) = parseExp ("let f = fun x -> fun y -> y")


printfn "%A" x

//let inf = Inferencer()

//let t = inf.TypeOf(x)

let t = typeOf x

printfn "%A"(t.ToString())


//let ret = parseExp ("let z = let x = 42 in x in z")

//let ret = parseExp ("let z =                      \n" +
//                    "    let x = 42 in x in z          \n")


//let ret = parseExp ("let z =                      " + (Environment.NewLine) +
//                    "    let x = 42               " + (Environment.NewLine) +
//                    "    let y = 12 in x + y      ")

//let x = parseExp "let x = 12 in let y = 32 in x + y"
//
//let y = parseExp ("let x = 12 in " + 
//                  "let y = 32 in " +
//                  "x + y         ");

let z = parseExp ("let x = 12 \n " + 
                  "let y = 32 \n " +
                  "x + y         ");                    


let ret = parseExp "f  g"

printf "%A" ret