module internal FSharpCompiler.Tests

open Microsoft.FSharp.Compiler.Build
open Microsoft.FSharp.Compiler.Lexhelp
open Microsoft.FSharp.Compiler.ErrorLogger
open Microsoft.FSharp.Compiler.Driver
open Microsoft.FSharp.Compiler.Ast
open System.IO
open Ast

let sourceFiles = [Directory.GetCurrentDirectory() + "..\\..\\..\\test.fs"]

let parseFiles sourceFiles = 
    let tcConfigBuilder = TcConfigBuilder.CreateNew (Directory.GetCurrentDirectory(), false, Directory.GetCurrentDirectory())
    let tcConfig = TcConfig.Create (tcConfigBuilder, false)
    let lexResourceManager = new LexResourceManager()
    let errorLogger = DiscardErrorsLogger
    let input = 
        try  
            sourceFiles 
            |> tcConfig.ComputeCanContainEntryPoint 
            |> List.zip sourceFiles
            |> List.choose (fun (input,isLastCompiland) -> 
                    ParseOneInputFile(tcConfig,lexResourceManager,["COMPILED"],input,isLastCompiland,errorLogger,(*retryLocked*)false)) 
        with e -> 
           errorRecoveryNoRange e; exiter.Exit 1
    input

let getDecls input =
    let implFile = match input with
                   | ImplFileInput implFile -> implFile

    let synModuleOrNamspace = let (ImplFile(_,_,_,_,_,x,_)) = implFile in x

    let (ModuleOrNamespace(_,_,x,_,_,_,_)) = (List.head synModuleOrNamspace) in x


let printPat ast = match ast with
                   | SynPat.Named (_, x, _, _, _) -> x.idText

let printConst ast = match ast with
                     | SynConst.Int32(x) -> sprintf "%i" x
                     | SynConst.Unit -> sprintf "()"

let rec printExpr ast = match ast with
                        | SynExpr.Const(x,_) -> printConst x
                        | SynExpr.LetOrUse (_,_,xs,x,_) -> sprintf "%s in %s" (List.fold (fun (a:string) b -> a + (printBinding b)) "let " xs)  (printExpr x)

and printBinding ast = match ast with
                       | SynBinding.Binding(_,_,_,_,_,_,_,name,_,expr,_,_) -> sprintf "%s = %s" (printPat name) (printExpr expr)

let printDecl ast = match ast with
                    | SynModuleDecl.Let (_,x,_) -> List.fold (fun (a:string) b -> a + (printBinding b)) "let " x
                    | SynModuleDecl.DoExpr (_,x,_) -> printExpr x

let printDecls ast = List.fold (fun x y -> sprintf "%s\n%s" x (printDecl y)) "" ast


printfn "%A" (sourceFiles |> parseFiles |> List.head |> getDecls |> printDecls)


                                                  