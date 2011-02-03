// * **********************************************************************************************
// * Copyright (c) Edmondo Pentangelo. 
// *
// * This source code is subject to terms and conditions of the Apache License, Version 2.0. A 
// * copy of the license can be found in the License.html file at the root of this distribution. 
// * By using this source code in any fashion, you are agreeing to be bound 
// * by the terms of the Apache License, Version 2.0.
// *
// * You must not remove this notice, or any other, from this software.
// * **********************************************************************************************

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

let findAllReferences (ast, (x1,x2,y1,y2)) =
        let pos = { srcFilename = path; srcLine = { startLine = y1; endLine = y2 }; srcColumn = { startColumn = x1; endColumn = x2 } }
        try 
            ast
            |> findAllReferences pos
            |> List.map (fun (Var (s, { srcFilename = _; srcLine = { startLine = y1; endLine = y2 }; srcColumn = { startColumn = x1; endColumn = x2 } })) -> (x1, x2, y1, y2))
            |> List.toSeq
        with
        | _ -> Seq.empty

let findAllReferencesInSymbolTable (symbolTable, (x1,x2,y1,y2)) =
        let pos = { srcFilename = path; srcLine = { startLine = y1; endLine = y2 }; srcColumn = { startColumn = x1; endColumn = x2 } }
        try 
            pos
            |> ASTAnalysis.getAllReferences symbolTable
            |> List.map (fun (Var (s, { srcFilename = _; srcLine = { startLine = y1; endLine = y2 }; srcColumn = { startColumn = x1; endColumn = x2 } })) -> (x1, x2, y1, y2))
            |> List.toSeq
        with
        | _ -> Seq.empty      