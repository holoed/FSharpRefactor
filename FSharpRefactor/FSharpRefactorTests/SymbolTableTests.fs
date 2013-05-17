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
                               |> create
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
                               |> create
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
                               |> create
                               |> addRef "x" loc2
                               |> lookUp "x" loc
        Assert.AreEqual (2, List.length occurences)
        Assert.AreEqual (loc2, occurences.[0])
        Assert.AreEqual (loc, occurences.[1])

    [<Test>]
    member this.``Should be able to addRef of an identifier defined in a parent of a parent scope``() =
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
                               |> create
                               |> insert "y" loc2
                               |> addRef "x" loc3
                               |> lookUp "y" loc2
        Assert.AreEqual (1, List.length occurences)
        Assert.AreEqual (loc2, occurences.[0])

                                  