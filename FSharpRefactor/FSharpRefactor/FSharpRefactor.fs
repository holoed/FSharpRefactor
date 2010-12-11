module FSharpRefactor

open CompilerToAst
open ASTAnalysis
open Ast
open System.IO

let path = sprintf "%s\\%s" (Directory.GetCurrentDirectory()) "test.fs" 

let parseWithPos s =         
        File.WriteAllText("test.fs", s)        
        let [xs:_] = parseToAst [path]
        xs 

let findAllReferences (text,(x1,x2,y1,y2)) =
        let pos = { srcFilename = path; srcLine = { startLine = y1; endLine = y2 }; srcColumn = { startColumn = x1; endColumn = x2 } }
        try 
            text 
            |> parseWithPos 
            |> findAllReferences pos
            |> List.map (fun (Var (s, { srcFilename = _; srcLine = { startLine = y1; endLine = y2 }; srcColumn = { startColumn = x1; endColumn = x2 } })) -> (x1, x2, y1, y2))
            |> List.toSeq
        with
        | _ -> Seq.empty
             