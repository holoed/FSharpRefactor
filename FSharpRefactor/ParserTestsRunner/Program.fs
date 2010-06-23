// Learn more about F# at http://fsharp.net

open System
open FSharpParser

let ret = parseExp ("let z =                      " + (Environment.NewLine) +
                    "    let x = 42               " + (Environment.NewLine) +
                    "    let y = 12 in x + y      ")

printf "%A" ret