module PrettyPrinter

open Ast
open System.Dynamic
open System.Text

type System.String with
  member x.AsLiteral()=
    let sb = new StringBuilder(x)
    sb.Replace("\"", "\\\"") |> ignore
    sb.Replace("\t", "\\\t") |> ignore
    sb.Replace("\\", "\\\\") |> ignore
    sb.Insert(0, "\"").Append("\"").ToString()

type PrettyPrinter() =
  let builder = new StringBuilder()
  
  member x.prettyPrint(any:Type)=
    any.ToString()

  member x.prettyPrint(any:Literal)=
    match any with
    | Char c -> c.ToString()
    | String s -> s.AsLiteral()
    | Double d -> d.ToString()
    | Integer i -> i.ToString()
    | Float f -> f.ToString()
