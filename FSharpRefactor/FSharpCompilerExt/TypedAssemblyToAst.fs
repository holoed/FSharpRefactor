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

module TypedAssemblyToAst

open Ast
open ContinuationMonad
open Microsoft.FSharp.Compiler.Tast
open Utils

let internal foldDecls (decls:TypedAssembly) =
    let rec LoopDecl x =
        cont { match x with
               | TypedAssembly.TAssembly ts ->
                    return! mmap LoopTypedImplFile ts }
    and LoopTypedImplFile x =
        cont { match x with 
               | TypedImplFile.TImplFile(_, _, m, _, _) ->
                    return! LoopModuleOrNamespace m }
    and LoopModuleOrNamespace x =
        cont { match x with
               | ModuleOrNamespaceExprWithSig.ModuleOrNamespaceExprWithSig(t, e, _) ->
                    let! eAcc = LoopModuleOrNamespaceExpr e
                    return [Ast.Module.Exp eAcc] }
                    //let tAcc = LoopModuleOrNamespaceType t
                    //return [Ast.Module.Exp eAcc; Ast.Module.Types tAcc] }
//    and LoopModuleOrNamespaceType t =
//                         t.ModuleAndNamespaceDefinitions
//                         |> Seq.map (fun x -> x.ModuleOrNamespaceType)    
//                         |> Seq.map (fun x -> x.AllEntities)
//                         |> Seq.concat
//                         |> Seq.map (fun e -> if (e.IsUnionTycon) then
//                                                TypeDef.DisUnion(e.DisplayName, e.UnionCasesAsList 
//                                                                                |> List.map (fun x -> (x.Id.idText, mkSrcLoc (x.Id.idRange), (int64)e.Stamp, true)))
//                                              else
//                                                TypeDef.None "Foo") 
//                         |> Seq.toList

    and LoopModuleOrNamespaceExpr x =
        cont { match x with
               | ModuleOrNamespaceExpr.TMDefs es ->
                    let! esAcc = mmap LoopModuleOrNamespaceExpr es
                    return List.concat esAcc
               | ModuleOrNamespaceExpr.TMDefLet (b, _) ->
                    let! bAcc = LoopLetDef b
                    return [bAcc]
               | ModuleOrNamespaceExpr.TMDefDo (b, _) ->
                    let! bAcc = LoopExpr b
                    return [bAcc]
               | ModuleOrNamespaceExpr.TMDefRec (_, bs,nbs,_) ->
                    let! bsAcc = mmap LoopLetDef bs
                    let! nbsAcc = mmap LoopNamespaceOrModuleBinding nbs
                    return List.concat [bsAcc ; List.concat nbsAcc] }
    and LoopNamespaceOrModuleBinding x =
        cont { match x with
               | ModuleOrNamespaceBinding.ModuleOrNamespaceBinding(_, mne) ->
                    let! mneAcc = LoopModuleOrNamespaceExpr mne
                    return mneAcc }
    and LoopLetDef x =
        cont { match x with 
               | Binding.TBind (v,e,_) ->
                    let! eAcc = LoopExpr e
                    if (v.IsCompilerGenerated) then
                        return eAcc
                    else
                        return Ast.Let(false, [Pat.PVar(v.DisplayName, mkSrcLoc v.Id.idRange, (int64)v.Stamp, true), eAcc], Lit(Unit)) }

    and LoopExpr x =
        cont { match x with  
               | Expr.Link e ->
                    return! LoopExpr (e.Value)
               | Expr.Match(_,_,_,xs,_,_) ->                   
                    let! xsAcc = mmap (fun (DecisionTreeTarget.TTarget(x,y,z)) -> cont { return! LoopExpr y }) (Array.toList xs)
                    return Exp.Match(Exp.LongVar(xsAcc) ,[])
               | Expr.Op (f,_,es,range) -> 
                    let! fAcc = LoopTOp f range
                    return! buildApp fAcc es
               | Expr.App (e,_,_,es,_) ->
                    let! eAcc = LoopExpr e
                    return! buildApp eAcc es
               | Expr.Let (Binding.TBind(v, e1, _), e2, _, _) ->
                    let! e1Acc = LoopExpr e1
                    let! e2Acc = LoopExpr e2
                    if (v.IsCompilerGenerated) then
                        return Ast.List [e1Acc; e2Acc]
                    else
                        return Exp.Let(false, [PVar (v.DisplayName, mkSrcLoc (v.Id.idRange), (int64)v.Stamp, true), e1Acc], e2Acc)
               | Expr.Const (c, _, _) ->
                    let! cAcc = LoopConst c
                    return Lit(cAcc)
               | Expr.TyLambda (_,_,e,_,_) ->
                    let! eAcc = LoopExpr e
                    return eAcc
               | Expr.Lambda (_,sv,_,svs,e,_,_) ->
                    let! eAcc = LoopExpr e
                    let svs' = svs |> List.filter (fun (v:Val) -> not v.IsCompilerGenerated)
                                   |> List.map (fun (v:Val) -> PVar (v.DisplayName, mkSrcLoc (v.Id.idRange), (int64)v.Stamp, true))  
                    return Ast.Lam(svs', eAcc)                    
               | Expr.Val (v,_,range) ->     
                    let vx = v.binding.Range
                    let local = v.DefinitionRange.FileName.Contains("Test")
                    if (not v.IsCompilerGenerated && local) then 
                        return Ast.Var(v.DisplayName, mkSrcLoc range, (int64)v.Stamp, false) 
                    else 
                        return Ast.Null
                        
               | Expr.Seq (e1,e2,_,_,_) -> 
                    let! e1Acc = LoopExpr e1
                    let! e2Acc = LoopExpr e2
                    return Ast.List [e1Acc; e2Acc] }   
    and LoopTOp x range =
        cont { match x with
               | TOp.UnionCase(y) ->
                    let (UCRef(t, s)) = y
                    return LongVar([Var (s, mkSrcLoc (y.Range), t.Stamp, true) ;  Var (s, mkSrcLoc range, t.Stamp, false)])    
               | TOp.UnionCaseProof (uref) ->
                    return Ast.Null
               | TOp.UnionCaseFieldGet (uref, _) ->
                    return Ast.Null
               | TOp.Coerce -> return Ast.Null 
               | TOp.Tuple -> return Ast.Null
               | TOp.TupleFieldGet x -> return Ast.Null
               | TOp.ILAsm (_,_) -> return Ast.Null
               | TOp.LValueOp (_, _) -> return Ast.Null 
               | TOp.ExnConstr _ -> return Ast.Null }                    
                                       
    and LoopConst x =
        cont { match x with
               | Const.Int32 x -> return Literal.Integer x 
               | Const.String s -> return Literal.String s
               | Const.Bool x -> return Literal.Bool x }

    and buildApp f xs = 
               cont { match List.rev xs with
                      | x::[] -> 
                        let! xAcc = LoopExpr x
                        return Ast.App (f, xAcc)
                      | x::xs' -> 
                        let! xAcc = LoopExpr x
                        let! pAcc = buildApp f xs'
                        return Ast.App(pAcc, xAcc)
                       | [] -> return f }

    LoopDecl decls id
