module CompilerToAst

open Microsoft.FSharp.Compiler.Build
open Microsoft.FSharp.Compiler.Lexhelp
open Microsoft.FSharp.Compiler.ErrorLogger
open Microsoft.FSharp.Compiler.Driver
open Microsoft.FSharp.Compiler.Ast
open System.IO
open Ast

let internal parseFiles sourceFiles = 
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

let internal getDecls input =
    let implFile = match input with
                   | ImplFileInput implFile -> implFile

    let synModuleOrNamspace = let (ImplFile(_,_,_,_,_,x,_)) = implFile in x

    let (ModuleOrNamespace(_,_,x,_,_,_,_)) = (List.head synModuleOrNamspace) in x


let internal patToAst x = match x with
                          | SynPat.Named (_, x, _, _, _) -> Ast.PVar (x.idText)

let internal constToAst x = match x with
                            | SynConst.Int32 x -> Ast.Lit(Ast.Literal.Integer x)
                            | SynConst.Unit -> Ast.Lit(Ast.Literal.Unit)

let rec internal exprToAst x = match x with 
                               | SynExpr.Const(x, _) -> constToAst x
                               | SynExpr.LetOrUse (_,_,xs,x,_) -> let (Let(j, k, _)) = bindingToAst (xs.Head)
                                                                  Let (j, k, x |> exprToAst)

and internal bindingToAst x = match x with
                     | SynBinding.Binding(_,_,_,_,_,_,_,name,_,expr,_,_) -> Ast.Let((patToAst name), (exprToAst expr), Ast.Lit(Ast.Literal.Unit))

let internal declToAst x = match x with
                  | SynModuleDecl.Let (_,xs,_) -> xs |> List.head |> bindingToAst
                  | SynModuleDecl.DoExpr (_,x,_) -> exprToAst x

let rec internal declsToAst xs = List.map (fun x -> declToAst x) xs

let parseToAst sourceFiles = sourceFiles |> parseFiles |> List.map (fun x -> x |> getDecls |> declsToAst)


                                                  

