﻿// * **********************************************************************************************
// * Copyright (c) Edmondo Pentangelo. 
// *
// * This source code is subject to terms and conditions of the Apache License, Version 2.0. A 
// * copy of the license can be found in the License.html file at the root of this distribution. 
// * By using this source code in any fashion, you are agreeing to be bound 
// * by the terms of the Apache License, Version 2.0.
// *
// * You must not remove this notice, or any other, from this software.
// * **********************************************************************************************

module Utils

open Ast
open Microsoft.FSharp.Compiler.Ast

let internal mkSrcLoc (r: Microsoft.FSharp.Compiler.Range.range)  = 
    { srcFilename = r.FileName; srcLine = { startLine = r.StartLine; endLine = r.EndLine }; 
                                srcColumn = { startColumn = r.StartColumn; endColumn = r.EndColumn } }

let internal joinSrcLoc { srcFilename = fileName1; srcLine = { startLine = startLine1; endLine = _ }; 
                                srcColumn = { startColumn = startColumn1; endColumn = _ } }
                        { srcFilename = _; srcLine = { startLine = _; endLine = endLine2 }; 
                                srcColumn = { startColumn = _; endColumn = endColumn2 } } =
                        { srcFilename = fileName1; srcLine = { startLine = startLine1; endLine = endLine2 }; 
                                srcColumn = { startColumn = startColumn1; endColumn = endColumn2 } }