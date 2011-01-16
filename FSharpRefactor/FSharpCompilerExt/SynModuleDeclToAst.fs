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

module SynModuleDeclToAst

open Ast
open ContinuationMonad
open Microsoft.FSharp.Compiler.Ast

let internal mkSrcLoc (r: Microsoft.FSharp.Compiler.Range.range)  = 
    { srcFilename = r.FileName; srcLine = { startLine = r.StartLine; endLine = r.EndLine }; 
                                srcColumn = { startColumn = r.StartColumn; endColumn = r.EndColumn } }
let internal foldDecls decls =
    let rec LoopDecl x =
        cont { match x with
               | SynModuleDecl.Let (isRec,xs,_) -> 
                    let! xsAcc = mmap (LoopBinding isRec) xs
                    return Ast.Module.Exp xsAcc
               | SynModuleDecl.DoExpr (_,x,_) -> 
                    let! xAcc = LoopExpr x
                    return Ast.Module.Exp [xAcc] 
               | SynModuleDecl.Types (xs, _) -> 
                    let! xsAcc = mmap LoopTypeDef xs
                    return Ast.Module.Types xsAcc
               | SynModuleDecl.NestedModule (SynComponentInfo.ComponentInfo(_,_,_,longId,_,_,_,_), xs, _, _) -> 
                    let! xsAcc = mmap LoopDecl xs
                    return Ast.NestedModule (List.map (fun (x:Ident) -> x.idText) longId, xsAcc)
               | SynModuleDecl.Open (xs, _) -> 
                    return Ast.Open (List.map (fun (x:Ident) -> x.idText) xs) }
    and LoopBinding isRec x = 
        cont { match x with
               | SynBinding.Binding(_,_,_,_,_,_,_,name,_,expr,_,_) -> 
                    let! nAcc = LoopPat name
                    let! eAcc = LoopExpr expr
                    return Ast.Let(isRec, nAcc, eAcc, Ast.Lit(Ast.Literal.Unit)) }
    and LoopMemberBinding x = 
        cont { match x with
               | SynBinding.Binding(_,_,_,_,_,_,_,name,_,expr,_,_) -> 
                    let! nAcc = LoopPat name
                    let! eAcc = LoopExpr expr
                    return Ast.Member(nAcc, eAcc) }
    and LoopExpr x =
         cont { match x with
                | SynExpr.Match(_,e,cs,_,_) -> 
                    let! eAcc = LoopExpr e
                    let! csAcc = mmap LoopClause cs
                    return Ast.Match(eAcc, csAcc)
                 | SynExpr.Seq(_, _, e1, e2, _) -> 
                    let! e1Acc = LoopExpr e1
                    let! e2Acc = LoopExpr e2
                    return Ast.List [e1Acc; e2Acc]
                | SynExpr.ArrayOrList(_, xs, _) -> 
                    let! xsAcc = mmap LoopExpr xs
                    return Ast.List xsAcc
                | SynExpr.CompExpr(_, _, expr, _) -> 
                    return! LoopExpr expr
                | SynExpr.ArrayOrListOfSeqExpr(_, expr, _) -> 
                    return! LoopExpr expr
                | SynExpr.Tuple(exprs, _) -> 
                    let! esAcc = mmap LoopExpr exprs
                    return Ast.Tuple esAcc
                | SynExpr.Const(x, _) -> return! LoopConst x
                | SynExpr.Ident(id) -> 
                    return Ast.Var (id.idText, mkSrcLoc id.idRange) 
                | SynExpr.LongIdent(_, ids, _) -> 
                    return Ast.LongVar (List.map (fun (id:Ident) -> Ast.Var (id.idText, mkSrcLoc id.idRange)) ids)
                | SynExpr.App(_, x, y, _) -> 
                    let! xAcc = LoopExpr x
                    let! yAcc = LoopExpr y
                    return Ast.App (xAcc, yAcc)
                | SynExpr.Paren(x, _) -> return! LoopExpr x
                | SynExpr.Lambda(_,_,x,y,_) -> 
                    let! xAcc = LoopSimplePats x
                    let! yAcc = LoopExpr y
                    return Ast.Lam (xAcc, yAcc)
                | SynExpr.LetOrUse (isRec,_,xs,x,_) -> 
                    let! (Let(r, j, k, _)) = LoopBinding isRec (xs.Head)
                    let! xAcc = LoopExpr x
                    return Let (r, j, k, xAcc)
                | SynExpr.ForEach (_,_,pat,e1,e2,_) -> 
                    let! pAcc = LoopPat pat
                    let! e1Acc = LoopExpr e1
                    let! e2Acc = LoopExpr e2
                    return Ast.ForEach (pAcc, e1Acc, e2Acc)
                | SynExpr.YieldOrReturn (_, e, _) ->
                    let! eAcc = LoopExpr e 
                    return Ast.YieldOrReturn eAcc
                | SynExpr.DotIndexedSet (e1, es, e2, _, _) -> 
                    let! e1Acc = LoopExpr e1
                    let! esAcc = mmap LoopExpr es
                    let! e2Acc = LoopExpr e2
                    return Ast.DotIndexedSet (e1Acc, esAcc, e2Acc)
                | SynExpr.DotIndexedGet (e1, es, _, _) -> 
                    let! e1Acc = LoopExpr e1
                    let! esAcc = mmap LoopExpr es
                    return Ast.DotIndexedGet (e1Acc, esAcc)
                | SynExpr.IfThenElse (e1,e2,e3,_,_,_) -> 
                    let! e1Acc = LoopExpr e1
                    let! e2Acc = LoopExpr e2
                    if (e3.IsSome) then 
                        let! e3Acc = LoopExpr e3.Value
                        return Ast.IfThenElse (e1Acc, e2Acc, Some e3Acc)
                    else
                        return Ast.IfThenElse (e1Acc, e2Acc, Option.None)
                | SynExpr.Record (_,_,xs,_) -> 
                    let! xsAcc = mmap LoopRecordFieldInst xs
                    return Ast.Record xsAcc
                | SynExpr.New (_,t,e,_) -> 
                        let! tAcc = LoopType t
                        let! eAcc = LoopExpr e
                        return Ast.New (tAcc, eAcc)
                | SynExpr.ObjExpr (t, x, bd, ims, _) ->
                        let! bdAcc = mmap LoopMemberBinding bd
                        return Ast.ObjExpr bdAcc
                | SynExpr.Do (e, _) ->
                        let! eAcc = LoopExpr e
                        return Ast.Do eAcc
                | SynExpr.Downcast (e, t, _) ->
                        let! eAcc = LoopExpr e
                        let! tAcc = LoopType t
                        return Ast.Downcast (eAcc, tAcc)
                | SynExpr.LongIdentSet (li, e, _) ->
                    let liAcc = Ast.LongVar (List.map (fun (id:Ident) -> Ast.Var (id.idText, mkSrcLoc id.idRange)) li)
                    let! eAcc = LoopExpr e
                    return Ast.LongVarSet (liAcc, eAcc)
                | SynExpr.ArbitraryAfterError _ -> 
                    return Ast.ArbitraryAfterError }

    and LoopRecordFieldInst ((_,(x:Ident)), e) =
        cont {  let! eAcc = LoopExpr e
                return ((x.idText, mkSrcLoc x.idRange), eAcc) }

    and LoopSimplePats x =
        cont { match x with 
               | SynSimplePats.SimplePats (xs, _) -> 
                    return! mmap LoopSimplePat xs }

    and LoopSimplePat x =
        cont { match x with
               | SynSimplePat.Id(ident, _, _, _, _) -> 
                    return PVar (ident.idText, mkSrcLoc (ident.idRange))
               | SynSimplePat.Typed(p,_,_) ->
                    return! LoopSimplePat p }
    
    and LoopConst x =
        cont { match x with
               | SynConst.Int32 x -> return Ast.Lit(Ast.Literal.Integer x)
               | SynConst.Double x -> return Ast.Lit(Ast.Literal.Float x)
               | SynConst.Unit -> return Ast.Lit(Ast.Literal.Unit)
               | SynConst.String (x, _) -> return Ast.Lit(Ast.Literal.String x)
               | SynConst.Char ch -> return Ast.Lit(Ast.Literal.Char ch)
               | SynConst.Bool b -> return Ast.Lit (Ast.Literal.Bool b) }
    and LoopTypeDef x =
         cont { match x with
                | SynTypeDefn.TypeDefn(SynComponentInfo.ComponentInfo(_,_,_,ident,_,_,_,_) , x, ms,_) ->
                    return! LoopRep ((List.head ident).idText) ms x }
    and LoopType x =
         cont { match x with
                | SynType.LongIdent (ident, _) ->
                    return Ast.LongIdent (List.map (fun (id:Ident) -> Ast.Ident (id.idText, mkSrcLoc id.idRange)) ident) }
    and LoopRep name ms x =
         cont { match x with
                | SynTypeDefnRepr.Simple(x, _) -> 
                        return! LoopSimpleTypeRep name ms x
                | SynTypeDefnRepr.ObjectModel(_,ms,_) ->
                        let! msAcc = mmap LoopClassMember ms
                        return Ast.Class (name, msAcc) }
    and LoopClassMember x = 
        cont { match x with 
               | SynMemberDefn.ImplicitCtor (_,_,ps,_,_) -> 
                    let! psAcc = mmap LoopSimplePat ps
                    return ClassMember.ImplicitCtor psAcc
               | SynMemberDefn.Member (b, _) -> 
                   return! LoopMemberBinding b
               | SynMemberDefn.LetBindings (es,_,_,_) ->
                   let! esAcc = mmap (LoopBinding false) es
                   return ClassMember.LetBindings esAcc 
               | SynMemberDefn.AbstractSlot(SynValSig.ValSpfn(_, ident, _, _, _, _, _, _, _, _, _),_,_) ->
                   return ClassMember.AbstractSlot (ident.idText)
               | SynMemberDefn.Interface(t, Some ms, _) ->
                   let! tAcc = LoopType t 
                   let! msAcc = mmap LoopClassMember ms
                   return ClassMember.Interface (tAcc, msAcc) }
    and LoopSimpleTypeRep name ms x =
         cont { match x with
                | SynTypeDefnSimpleRepr.Union (_, xs, _) -> 
                    let! xsAcc = mmap LoopUnionCases xs
                    return TypeDef.DisUnion (name, xsAcc)
                | SynTypeDefnSimpleRepr.Record (_, fields, _) -> 
                    let! fieldsAcc = mmap LoopRecordFields fields
                    let! msAcc = mmap LoopClassMember ms
                    return TypeDef.Record (name, fieldsAcc, msAcc)
                | SynTypeDefnSimpleRepr.None _ -> 
                    return TypeDef.None name }
    and LoopUnionCases x =
        cont { match x with
               | SynUnionCase.UnionCase(_,x,_,_,_,_) ->
                    return (x.idText, mkSrcLoc x.idRange) }
    and LoopRecordFields x =
        cont { match x with 
               | SynField.Field(_, _, identOption, _, _, _, _, _) -> 
                    return Option.map (fun (x:Ident) -> (x.idText, mkSrcLoc x.idRange)) identOption }
    and LoopClause x = 
        cont { match x with
               | SynMatchClause.Clause(p,_,e,_,_) -> 
                    let! pAcc = LoopPat p
                    let! eAcc = LoopExpr e
                    return Ast.Clause(pAcc, eAcc) }
    and LoopPat x =
        cont { match x with
               | SynPat.Typed (pat, _, _) -> 
                    return! LoopPat pat //TODO: Add support for Typed patterns in AST.
               | SynPat.Named (_, x, _, _, _) -> 
                    return Ast.PVar (x.idText, mkSrcLoc x.idRange)
               | SynPat.LongIdent (xs, _, _, ys, _, _) -> 
                    let x = xs |> Seq.map (fun (x:Ident) -> PVar (x.idText, mkSrcLoc x.idRange)) 
                               |> Seq.toList                               
                               |> fun xs' -> match xs' with
                                             | x'::[] -> match x' with
                                                         | PVar ("True",_) -> PLit(Bool(true))
                                                         | _ -> x'
                                             | xs -> PLongVar xs
                    return! if List.isEmpty ys then
                                cont { return x }
                            else
                                buildPApp x (List.rev ys)
               | SynPat.Paren(x, _) -> return! LoopPat x
               | SynPat.Tuple(xs, _) ->
                    let! xsAcc = mmap LoopPat xs  
                    return PTuple xsAcc 
               | SynPat.Wild _ -> 
                    return Pat.PWild
               | SynPat.Const (c, _) -> 
                    let! (Ast.Lit lit) = LoopConst c
                    return Pat.PLit lit
               | SynPat.ArrayOrList (_,xs,_) -> 
                    let! xsAcc = mmap LoopPat xs
                    return Ast.PList xsAcc }

    and buildPApp f xs = 
               cont { match xs with
                      | x::[] -> 
                        let! xAcc = LoopPat x
                        return Ast.PApp (f, xAcc)
                      | x::xs' -> 
                        let! xAcc = LoopPat x
                        let! pAcc = buildPApp f xs'
                        return Ast.PApp(pAcc, xAcc) }
       
    List.map (fun decl -> LoopDecl decl id) decls