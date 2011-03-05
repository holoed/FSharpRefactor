module Utils

open Ast
open Microsoft.FSharp.Compiler.Ast

let internal mkSrcLoc (r: Microsoft.FSharp.Compiler.Range.range)  = 
    { srcFilename = r.FileName; srcLine = { startLine = r.StartLine; endLine = r.EndLine }; 
                                srcColumn = { startColumn = r.StartColumn; endColumn = r.EndColumn } }