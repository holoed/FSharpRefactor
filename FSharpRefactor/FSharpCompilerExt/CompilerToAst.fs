﻿// * **********************************************************************************************
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
    { srcFilename = r.FileName; srcLine = { startLine = r.StartLine; endLine = r.EndLine }; 
                                srcColumn = { startColumn = r.StartColumn; endColumn = r.EndColumn } }

let rec internal buildPApp f xs = 
                    match xs with
                    | x::[] -> Ast.PApp (f, patToAst x)
                    | x::xs' -> Ast.PApp(buildPApp f xs', patToAst x)
                               
and internal patToAst x = 
                    match x with
                    | SynPat.Named (_, x, _, _, _) -> Ast.PVar (x.idText, mkSrcLoc x.idRange)
                    | SynPat.LongIdent (x::_, _, _, ys, _, _) -> match ys with
                                                                 | [] when x.idText = "True" -> PLit(Literal.Bool(true))
                                                                 | _ -> buildPApp (Ast.PVar (x.idText, mkSrcLoc x.idRange)) (List.rev ys)
                    | SynPat.Paren(x, _) -> patToAst x
                    | SynPat.Tuple(xs, _) ->  PTuple(List.map patToAst xs)
                    | SynPat.Wild _ -> Pat.PWild
                    | SynPat.ArrayOrList (_,xs,_) -> Ast.PList (List.map (fun x -> patToAst x) xs)

let internal constToAst x =
                    match x with
                    | SynConst.Int32 x -> Ast.Lit(Ast.Literal.Integer x)
                    | SynConst.Double x -> Ast.Lit(Ast.Literal.Float x)
                    | SynConst.Unit -> Ast.Lit(Ast.Literal.Unit)
                    | SynConst.String (x, _) -> Ast.Lit(Ast.Literal.String x)
                    | SynConst.Char ch -> Ast.Lit(Ast.Literal.Char ch)
                    | SynConst.Bool b -> Ast.Lit (Ast.Literal.Bool b)

let internal spatToAst x = 
                    match x with
                    | SynSimplePat.Id(ident, _, _, _, _) -> PVar (ident.idText, mkSrcLoc (ident.idRange))

let internal spatsToAst x = 
                    match x with
                    | SynSimplePats.SimplePats(xs, _) -> List.map (fun x -> spatToAst x) xs
                   
let rec internal clauseToAst c = 
                    match c with
                    | SynMatchClause.Clause(pat,_,expr,_,_) -> Ast.Clause(patToAst pat, exprToAst expr)

and internal exprToAst x = 
                    match x with 
                    | SynExpr.Match(_,e,cs,_,_) -> Ast.Match(exprToAst e, List.map (fun c -> clauseToAst c) cs)
                    | SynExpr.Seq(_, _, e1, e2, _) -> Ast.List [exprToAst e1; exprToAst e2]
                    | SynExpr.ArrayOrList(_, xs, _) -> Ast.List (List.map (fun x -> exprToAst x) xs)
                    | SynExpr.CompExpr(_, _, expr, _) -> exprToAst expr
                    | SynExpr.ArrayOrListOfSeqExpr(_, expr, _) -> exprToAst expr
                    | SynExpr.Tuple(exprs, _) -> Ast.Tuple(List.map exprToAst exprs)
                    | SynExpr.Const(x, _) -> constToAst x
                    | SynExpr.Ident(id) -> Ast.Var (id.idText, mkSrcLoc id.idRange)
                    | SynExpr.LongIdent(_, ids, _) -> Ast.LongVar (List.map (fun (id:Ident) -> Ast.Var (id.idText, mkSrcLoc id.idRange)) ids)
                    | SynExpr.App(_, x, y, _) -> Ast.App(exprToAst x, exprToAst y)
                    | SynExpr.Paren(x, _) -> exprToAst x
                    | SynExpr.Lambda(_,_,x,y,_) -> Ast.Lam(spatsToAst x, exprToAst y)
                    | SynExpr.LetOrUse (isRec,_,xs,x,_) -> let (Let(r, j, k, _)) = bindingToAst isRec (xs.Head)
                                                           Let (r, j, k, x |> exprToAst)
                    | SynExpr.ForEach (_,_,pat,e1,e2,_) -> Ast.ForEach (patToAst pat, exprToAst e1, exprToAst e2)
                    | SynExpr.YieldOrReturn (_, e, _) -> Ast.YieldOrReturn (exprToAst e)
                    | SynExpr.IfThenElse (e1,e2,e3,_,_,_) -> Ast.IfThenElse (exprToAst e1, exprToAst e2, if (e3.IsSome) then Some (exprToAst (e3.Value)) else None)
                    | SynExpr.DotIndexedSet (e1, es, e2, _, _) -> Ast.DotIndexedSet (exprToAst e1, List.map exprToAst es, exprToAst e2)
                    | SynExpr.DotIndexedGet (e1, es, _, _) -> Ast.DotIndexedGet (exprToAst e1, List.map exprToAst es)
                    | SynExpr.ArbitraryAfterError _ -> Ast.ArbitraryAfterError

and internal bindingToAst isRec x = 
                    match x with
                    | SynBinding.Binding(_,_,_,_,_,_,_,name,_,expr,_,_) -> Ast.Let(isRec, (patToAst name), (exprToAst expr), Ast.Lit(Ast.Literal.Unit))

and internal unionCasesToAst x = 
                    match x with
                    | SynUnionCase.UnionCase(_,x,_,_,_,_) -> (x.idText, mkSrcLoc x.idRange)

and internal simpleTypeRepToAst x = 
                    match x with
                    | SynTypeDefnSimpleRepr.Union(_, xs, _) -> fun name -> TypeDef.DisUnion (name, List.map (fun x -> unionCasesToAst x) xs)

and internal typeRepToAst x = 
                    match x with
                    | SynTypeDefnRepr.Simple(x, _) -> fun name -> simpleTypeRepToAst x name

and internal typeToAst x = 
                    match x with
                    | SynTypeDefn.TypeDefn(SynComponentInfo.ComponentInfo(_,_,_,ident,_,_,_,_) , x, _,_) -> typeRepToAst x ((List.head ident).idText)


let rec internal declToAst x = 
                  match x with
                  | SynModuleDecl.Let (isRec,xs,_) ->  xs |> List.map (fun x -> x |> (bindingToAst isRec)) |> Ast.Module.Exp
                  | SynModuleDecl.DoExpr (_,x,_) -> Ast.Module.Exp [x |> exprToAst] 
                  | SynModuleDecl.Types (xs, _) -> xs |> List.map(fun x -> typeToAst x) |> Ast.Module.Types
                  | SynModuleDecl.NestedModule (SynComponentInfo.ComponentInfo(_,_,_,longId,_,_,_,_), xs, _, _) -> Ast.NestedModule (List.map (fun (x:Ident) -> x.idText) longId, declsToAst (Seq.toList xs))
                  | SynModuleDecl.Open (xs, _) -> Ast.Open (List.map (fun (x:Ident) -> x.idText) xs)

and internal declsToAst xs = List.map (fun x -> declToAst x) xs

let parseToAst sourceFiles = sourceFiles |> parseFiles |> List.map (fun x -> x |> getDecls |> declsToAst)


                                                  

