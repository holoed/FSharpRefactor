// Learn more about F# at http://fsharp.net

module Utils

type OptionBuilder() =
    member this.Bind (m, f) = Option.bind f m
    member this.Return x = Some x

let option = OptionBuilder()