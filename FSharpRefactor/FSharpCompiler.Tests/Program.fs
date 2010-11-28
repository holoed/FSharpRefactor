module internal FSharpCompiler.Tests

open Microsoft.FSharp.Compiler.Build
open Microsoft.FSharp.Compiler.Lexhelp
open Microsoft.FSharp.Compiler.ErrorLogger
open Microsoft.FSharp.Compiler.Driver
open Microsoft.FSharp.Compiler.Ast
open System.IO

let sourceFiles = [Directory.GetCurrentDirectory() + "..\\..\\..\\test.fs"]

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

let ret = List.head input

let implFile = match ret with
               | ImplFileInput implFile -> implFile

let synModuleOrNamspace = let (ImplFile(_,_,_,_,_,x,_)) = implFile in x

let decls = let (ModuleOrNamespace(_,_,x,_,_,_,_)) = (List.head synModuleOrNamspace) in x

let rec printPat ast = match ast with
                       | SynPat.Named (_, x, _, _, _) -> x.idText

let rec printConst ast = match ast with
                         | SynConst.Int32(x) -> sprintf "%i" x

let rec printExpr ast = match ast with
                        | SynExpr.Const(x,_) -> printConst x

let rec printBinding ast = match ast with
                           | SynBinding.Binding(_,_,_,_,_,_,_,name,_,expr,_,_) -> sprintf "%s = %s" (printPat name) (printExpr expr)

let rec printDecl ast = match ast with
                        | SynModuleDecl.Let (_,x,_) -> List.fold (fun (a:string) b -> a + (printBinding b)) "let " x

let rec printDecls ast = List.fold (fun x y -> sprintf "%s\n%s" x (printDecl y)) "" ast


printfn "%A" (printDecls decls)