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

module SymbolTableTests
 
open Ast
open SymbolTable
open NUnit.Framework
 
[<TestFixture>]
type SymbolTableTests() =
 
    [<Test>]
    member this.``Should be able to add an identifier to the symbol table and look it up``() =
        let loc = { srcFilename = "foo.fs"
                    srcLine = { startLine = 1; endLine = 1 }
                    srcColumn = { startColumn = 4; endColumn = 5 } }
        let table = empty |> insert "x" loc
        let occurrences = table |> lookUp "x" loc
        Assert.AreEqual (1, List.length occurrences)
 
    [<Test>]
    member this.``Should be able to add more than one identifier and look it up``() =
        let loc = { srcFilename = "foo.fs"
                    srcLine = { startLine = 1; endLine = 1 }
                    srcColumn = { startColumn = 4; endColumn = 5 } }
        let loc2 = { srcFilename = "foo.fs"
                     srcLine = { startLine = 1; endLine = 1 }
                     srcColumn = { startColumn = 2; endColumn = 3 } }
        let table = empty |> insert "x" loc
                          |> insert "y" loc2
        let occurrences = table |> lookUp "x" loc
        let occurrences2 = table |> lookUp "y" loc2
        Assert.AreEqual (loc, occurrences.[0])
        Assert.AreEqual (loc2, occurrences2.[0])
 
    [<Test>]
    member this.``Should be able to add a reference to an existing identifier``() =
        let loc = { srcFilename = "foo.fs"
                    srcLine = { startLine = 1; endLine = 1 }
                    srcColumn = { startColumn = 4; endColumn = 5 } }
        let loc2 = { srcFilename = "foo.fs"
                     srcLine = { startLine = 1; endLine = 1 }
                     srcColumn = { startColumn = 8; endColumn = 9 } }
        let table = empty |> insert "x" loc
                          |> addRef "x" loc2
        let occurrences = table |> lookUp "x" loc
        Assert.AreEqual (loc2, occurrences.[0])
        Assert.AreEqual (loc, occurrences.[1])
 
    [<Test>]
    member this.``Should be able to handle scoping by creating a new symbol table and looking up``() =
        let loc = { srcFilename = "foo.fs"
                    srcLine = { startLine = 1; endLine = 1 }
                    srcColumn = { startColumn = 3; endColumn = 4 } }
        let occurences = empty |> insert "x" loc
                               |> enter_scope
                               |> lookUp "x" loc
        Assert.AreEqual (1, List.length occurences)
        Assert.AreEqual (loc, occurences.[0])
 
    [<Test>]
    member this.``Should be able to shadow variables of parent scope``() =
        let loc = { srcFilename = "foo.fs"
                    srcLine = { startLine = 1; endLine = 1 }
                    srcColumn = { startColumn = 3; endColumn = 4 } }
        let loc2 = { srcFilename = "foo.fs"
                     srcLine = { startLine = 1; endLine = 1 }
                     srcColumn = { startColumn = 2; endColumn = 3 } }
        let occurences = empty |> insert "x" loc
                               |> enter_scope
                               |> insert "x" loc2
                               |> lookUp "x" loc2
        Assert.AreEqual (1, List.length occurences)
        Assert.AreEqual (loc2, occurences.[0])
 
    [<Test>]
    member this.``Should be able to addref of an identifier defined in a parent scope``() = 
        let loc = { srcFilename = "foo.fs"
                    srcLine = { startLine = 1; endLine = 1 }
                    srcColumn = { startColumn = 3; endColumn = 4 } }
        let loc2 = { srcFilename = "foo.fs"
                     srcLine = { startLine = 4; endLine = 4 }
                     srcColumn = { startColumn = 5; endColumn = 6 } }
        let occurences = empty |> insert "x" loc
                               |> enter_scope
                               |> addRef "x" loc2
                               |> lookUp "x" loc
        Assert.AreEqual (2, List.length occurences)
        Assert.AreEqual (loc2, occurences.[0])
        Assert.AreEqual (loc, occurences.[1])
 
    [<Test>]
    member this.``Should be able to lookUp an identifier defined in a child scope``() =
        let loc = { srcFilename = "foo.fs"
                    srcLine = { startLine = 1; endLine = 1 }
                    srcColumn = { startColumn = 3; endColumn = 4 } }
        let loc2 = { srcFilename = "foo.fs"
                     srcLine = { startLine = 4; endLine = 4 }
                     srcColumn = { startColumn = 1; endColumn = 2 } }
        let loc3 = { srcFilename = "foo.fs"
                     srcLine = { startLine = 4; endLine = 4 }
                     srcColumn = { startColumn = 5; endColumn = 6 } }
        let occurences = empty |> insert "x" loc
                               |> enter_scope
                               |> insert "y" loc2
                               |> addRef "x" loc3
                               |> lookUp "y" loc2
        Assert.AreEqual (1, List.length occurences)
        Assert.AreEqual (loc2, occurences.[0])

    [<Test>]
    member this.``Should be able to create two child scopes at the same level``() =
        let loc = { srcFilename = "foo.fs"
                    srcLine = { startLine = 1; endLine = 1 }
                    srcColumn = { startColumn = 3; endColumn = 4 } }
        let loc2 = { srcFilename = "foo.fs"
                     srcLine = { startLine = 4; endLine = 4 }
                     srcColumn = { startColumn = 1; endColumn = 2 } }
        let loc3 = { srcFilename = "foo.fs"
                     srcLine = { startLine = 4; endLine = 4 }
                     srcColumn = { startColumn = 5; endColumn = 6 } }
        let loc4 = { srcFilename = "foo.fs"
                     srcLine = { startLine = 5; endLine = 5 }
                     srcColumn = { startColumn = 8; endColumn = 9 } }
        let t = empty |> insert "x" loc
                      |> enter_scope
                      |> insert "y" loc2
                      |> addRef "x" loc3
                      |> exit_scope
                      |> Option.get
                      |> enter_scope
                      |> insert "y" loc4
                      |> exit_scope
                      |> Option.get
        let occurences = t |> lookUp "y" loc2
        let occurences2 = t |> lookUp "y" loc4
        let occurences3 = t |> lookUp "x" loc3
        Assert.AreEqual (1, List.length occurences)
        Assert.AreEqual (loc2, occurences.[0])
        Assert.AreEqual (1, List.length occurences2)
        Assert.AreEqual (loc4, occurences2.[0])
        Assert.AreEqual (2, List.length occurences3)
        Assert.AreEqual (loc3, occurences3.[0])
        Assert.AreEqual (loc, occurences3.[1])