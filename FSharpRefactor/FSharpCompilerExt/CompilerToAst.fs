// * **********************************************************************************************
// * Copyright (c) Edmondo Pentangelo. 
// *
// * This source code is subject to terms and conditions of the Microsoft Public License. 
// * A copy of the license can be found in the License.html file at the root of this distribution. 
// * By using this source code in any fashion, you are agreeing to be bound by the terms of the 
// * Microsoft Public License.
// *
// * You must not remove this notice, or any other, from this software.
// * **********************************************************************************************

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

let internal mkSrcLoc (r: Microsoft.FSharp.Compiler.Range.range)  = 
    { srcFilename = r.FileName; srcLine = { startLine = r.StartLine; endLine = r.EndLine }; srcColumn = { startColumn = r.StartColumn; endColumn = r.EndColumn } }

let rec internal buildPApp f xs = match xs with
                                  | x::[] -> Ast.PApp (f, patToAst x)
                                  | x::xs' -> Ast.PApp(buildPApp f xs', patToAst x)
                               
and internal patToAst x = match x with
                              | SynPat.Named (_, x, _, _, _) -> Ast.PVar (x.idText, mkSrcLoc x.idRange)
                              | SynPat.LongIdent (x::_, _, _, ys, _, _) -> match ys with
                                                                           | [] when x.idText = "True" -> PLit(Literal.Bool(true))
                                                                           | _ -> buildPApp (Ast.PVar (x.idText, mkSrcLoc x.idRange)) (List.rev ys)
                              | SynPat.Paren(x, _) -> patToAst x
                              | SynPat.Tuple(xs, _) ->  PTuple(List.map patToAst xs)
                              | SynPat.Wild _ -> Pat.PWild

let internal constToAst x = match x with
                            | SynConst.Int32 x -> Ast.Lit(Ast.Literal.Integer x)
                            | SynConst.Double x -> Ast.Lit(Ast.Literal.Float x)
                            | SynConst.Unit -> Ast.Lit(Ast.Literal.Unit)
                            | SynConst.String (x, _) -> Ast.Lit(Ast.Literal.String x)

let internal spatToAst x = match x with
                           | SynSimplePat.Id(ident, _, _, _, _) -> PVar (ident.idText, mkSrcLoc (ident.idRange))

let internal spatsToAst x = match x with
                            | SynSimplePats.SimplePats(xs, _) -> List.map (fun x -> spatToAst x) xs
                   
let rec internal clauseToAst c = match c with
                                 | SynMatchClause.Clause(pat,_,expr,_,_) -> Ast.Clause(patToAst pat, exprToAst expr)

and internal exprToAst x = match x with 
                               | SynExpr.Match(_,e,cs,_,_) -> Ast.Match(exprToAst e, List.map (fun c -> clauseToAst c) cs)
                               | SynExpr.Seq(_, _, e1, e2, _) -> Ast.List [exprToAst e1; exprToAst e2]
                               | SynExpr.CompExpr(_, _, expr, _) -> exprToAst expr
                               | SynExpr.ArrayOrListOfSeqExpr(_, expr, _) -> exprToAst expr
                               | SynExpr.Tuple(exprs, _) -> Ast.Tuple(List.map exprToAst exprs)
                               | SynExpr.Const(x, _) -> constToAst x
                               | SynExpr.Ident(id) -> Ast.Var (id.idText, mkSrcLoc id.idRange)
                               | SynExpr.App(_, x, y, _) -> Ast.App(exprToAst x, exprToAst y)
                               | SynExpr.Paren(x, _) -> exprToAst x
                               | SynExpr.Lambda(_,_,x,y,_) -> Ast.Lam(spatsToAst x, exprToAst y)
                               | SynExpr.LetOrUse (_,_,xs,x,_) -> let (Let(j, k, _)) = bindingToAst (xs.Head)
                                                                  Let (j, k, x |> exprToAst)

and internal bindingToAst x = match x with
                              | SynBinding.Binding(_,_,_,_,_,_,_,name,_,expr,_,_) -> Ast.Let((patToAst name), (exprToAst expr), Ast.Lit(Ast.Literal.Unit))

and internal unionCasesToAst x = match x with
                                 | SynUnionCase.UnionCase(_,x,_,_,_,_) -> (x.idText, mkSrcLoc x.idRange)

and internal simpleTypeRepToAst x = match x with
                                    | SynTypeDefnSimpleRepr.Union(_, xs, _) -> fun name -> TypeDef.DisUnion (name, List.map (fun x -> unionCasesToAst x) xs)

and internal typeRepToAst x = match x with
                              | SynTypeDefnRepr.Simple(x, _) -> fun name -> simpleTypeRepToAst x name

and internal typeToAst x = match x with
                           | SynTypeDefn.TypeDefn(SynComponentInfo.ComponentInfo(_,_,_,ident,_,_,_,_) , x, _,_) -> typeRepToAst x ((List.head ident).idText)


let internal declToAst x = match x with
                  | SynModuleDecl.Let (_,xs,_) -> xs |> List.head |> bindingToAst |> Prog.Exp
                  | SynModuleDecl.DoExpr (_,x,_) -> x |> exprToAst |> Prog.Exp
                  | SynModuleDecl.Types (xs, _) -> xs |> List.map(fun x -> typeToAst x) |> Prog.Types

let rec internal declsToAst xs = List.map (fun x -> declToAst x) xs

let parseToAst sourceFiles = sourceFiles |> parseFiles |> List.map (fun x -> x |> getDecls |> declsToAst)


                                                  

