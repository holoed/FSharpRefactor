// Learn more about F# at http://fsharp.net

open System
open FSharpParser


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