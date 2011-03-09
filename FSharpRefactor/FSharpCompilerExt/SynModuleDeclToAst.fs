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
open Utils

let internal foldDecls decls =
    let rec LoopDecl x =
        cont { match x with
               | SynModuleDecl.HashDirective (ParsedHashDirective(s, ss, _), _) ->
                    return Ast.Module.HashDirective (s, ss)
               | SynModuleDecl.Let (isRec,xs,_) -> 
                    let! xsAcc = mmap LoopBinding xs
                    let xsAcc' = List.map (fun (nAcc, eAcc) -> Let(isRec, [nAcc, eAcc], Lit(Unit))) xsAcc
                    return Ast.Module.Exp xsAcc'
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
                    return Ast.Open (List.map (fun (x:Ident) -> x.idText) xs) 
               | SynModuleDecl.Exception (ed, _) ->
                    let! edAcc = LoopExceptionDef ed
                    return Ast.Exception edAcc }
    and LoopExceptionDef x =
        cont { match x with
               | SynExceptionDefn.ExceptionDefn (SynExceptionRepr.ExceptionDefnRepr(_,uc,Option.None,_,_,_), ms, _) ->
                    let! (name, _) = LoopUnionCases uc
                    let! msAcc = mmap LoopClassMember ms                    
                    return Ast.ExceptionDef (name, msAcc) }
    and LoopBinding x = 
        cont { match x with
               | SynBinding.Binding(_,_,_,_,_,_,_,name,_,expr,_,_) -> 
                    let! nAcc = LoopPat name
                    let! eAcc = LoopExpr expr
                    return (nAcc, eAcc) }
    and LoopMemberBinding x = 
        cont { match x with
               | SynBinding.Binding(_,_,_,_,_,_,_,name,_,expr,_,_) -> 
                    let! nAcc = LoopPat name
                    let! eAcc = LoopExpr expr
                    return Ast.Member(nAcc, eAcc) }
    and LoopExpr x =
         cont { match x with
                | SynExpr.Lazy (e, _) ->
                    let! eAcc = LoopExpr e
                    return Ast.Lazy eAcc
                | SynExpr.While (_, e1, e2, _) ->
                    let! e1Acc = LoopExpr e1
                    let! e2Acc = LoopExpr e2
                    return Ast.While(e1Acc, e2Acc)
                | SynExpr.Assert(e, _) ->
                    let! eAcc = LoopExpr e
                    return Ast.Assert eAcc
                | SynExpr.For(_, id, e1, _, e2, e3, _) ->
                    let! e1Acc = LoopExpr e1
                    let! e2Acc = LoopExpr e2
                    let! e3Acc = LoopExpr e3
                    return Ast.For(Ast.PVar(id.idText, mkSrcLoc id.idRange), e1Acc, e2Acc, e3Acc)
                | SynExpr.Null _ ->
                    return Ast.Null
                | SynExpr.AddressOf (_, e, _, _) ->
                    let! eAcc = LoopExpr e
                    return Ast.AddressOf eAcc
                | SynExpr.TypeApp (e, ts, _) ->
                    let! eAcc = LoopExpr e
                    let! tsAcc = mmap LoopType ts
                    return Ast.TypeApp (eAcc, tsAcc)
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
                    if (List.isEmpty xs) then
                        return Ast.ArbitraryAfterError
                    else
                        let! bsAcc = mmap LoopBinding xs
                        let! xAcc = LoopExpr x
                        return Let (isRec, bsAcc, xAcc)
                | SynExpr.LetOrUseBang (_,_,p,e1,e2,_) ->
                    let! pAcc = LoopPat p
                    let! e1Acc = LoopExpr e1
                    let! e2Acc = LoopExpr e2
                    return LetBang (pAcc, e1Acc, e2Acc)                        
                | SynExpr.ForEach (_,_,pat,e1,e2,_) -> 
                    let! pAcc = LoopPat pat
                    let! e1Acc = LoopExpr e1
                    let! e2Acc = LoopExpr e2
                    return Ast.ForEach (pAcc, e1Acc, e2Acc)
                | SynExpr.YieldOrReturn (_, e, _) ->
                    let! eAcc = LoopExpr e 
                    return Ast.YieldOrReturn eAcc
                | SynExpr.YieldOrReturnFrom (_, e, _) ->
                    let! eAcc = LoopExpr e 
                    return Ast.YieldOrReturnFrom eAcc
                | SynExpr.DotIndexedSet (e1, es, e2, _, _) -> 
                    let! e1Acc = LoopExpr e1
                    let! esAcc = mmap LoopExpr es
                    let! e2Acc = LoopExpr e2
                    return Ast.DotIndexedSet (e1Acc, esAcc, e2Acc)
                | SynExpr.DotIndexedGet (e1, es, _, _) -> 
                    let! e1Acc = LoopExpr e1
                    let! esAcc = mmap LoopExpr es
                    return Ast.DotIndexedGet (e1Acc, esAcc)
                | SynExpr.DotGet (e, li, _) -> 
                    let! eAcc = LoopExpr e
                    let liAcc = Ast.LongVar (List.map (fun (id:Ident) -> Ast.Var (id.idText, mkSrcLoc id.idRange)) li)
                    return Ast.DotGet (eAcc, liAcc)
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
                | SynExpr.DoBang (e, _) ->
                        let! eAcc = LoopExpr e
                        return Ast.DoBang eAcc
                | SynExpr.Downcast (e, t, _) ->
                        let! eAcc = LoopExpr e
                        let! tAcc = LoopType t
                        return Ast.Downcast (eAcc, tAcc)
                | SynExpr.Upcast (e, t, _) ->
                        let! eAcc = LoopExpr e
                        let! tAcc = LoopType t
                        return Ast.Upcast (eAcc, tAcc)
                | SynExpr.LongIdentSet (li, e, _) ->
                        let liAcc = Ast.LongVar (List.map (fun (id:Ident) -> Ast.Var (id.idText, mkSrcLoc id.idRange)) li)
                        let! eAcc = LoopExpr e
                        return Ast.LongVarSet (liAcc, eAcc)
                | SynExpr.TryWith (e,_,cl,_,_,_,_) ->
                        let! eAcc = LoopExpr e
                        let! clAcc = mmap LoopClause cl
                        return Ast.TryWith (eAcc, clAcc)
                | SynExpr.Typed (e, t, _) ->
                        let! eAcc = LoopExpr e
                        let! tAcc = LoopType t
                        return Ast.Typed (eAcc, tAcc)
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
               | SynConst.UInt32 x -> return Ast.Lit(Ast.Literal.UnsignedInteger x)
               | SynConst.Int32 x -> return Ast.Lit(Ast.Literal.Integer x)
               | SynConst.Int64 x -> return Ast.Lit(Ast.Literal.Int64 x)
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
                | SynType.Array (n,t,_) ->
                    let! tAcc = LoopType t
                    return Ast.TArray (n, tAcc)
                | SynType.Anon(_) ->
                    return Ast.TAnon
                | SynType.LongIdent (ident, _) ->
                    return Ast.LongIdent (List.map (fun (id:Ident) -> Ast.Ident (id.idText, mkSrcLoc id.idRange)) ident) 
                | SynType.Var (SynTypar.Typar(id, _, _), _) ->
                    return Ast.Type.TVar(Ast.Ident (id.idText, mkSrcLoc id.idRange))
                | SynType.Fun (ty1, ty2, _) ->
                    let! ty1Acc = LoopType ty1
                    let! ty2Acc = LoopType ty2
                    return Ast.TFun (ty1Acc, ty2Acc)
                | SynType.App (t, ts, _, _) ->
                    let! tAcc = LoopType t
                    let! tsAcc = mmap LoopType ts
                    return Ast.TApp (tAcc, tsAcc) 
                | SynType.Tuple (ts, _) ->
                    let! tsAcc = mmap (fun (_, x) -> LoopType x) ts
                    return Ast.TTuple tsAcc}
    and LoopRep name ms x =
         cont { match x with
                | SynTypeDefnRepr.Simple(x, _) -> 
                        return! LoopSimpleTypeRep name ms x
                | SynTypeDefnRepr.ObjectModel(_,ms,_) ->
                        let! msAcc = mmap LoopClassMember ms
                        return Ast.Class (name, msAcc) }
    and LoopClassMember x = 
        cont { match x with 
               | SynMemberDefn.ValField(SynField.Field(_,_,id,t,_,_,_,_), _) ->
                    let idAcc = Option.map(fun (x:Ident) -> Ast.Ident(x.idText, mkSrcLoc x.idRange)) id
                    let! tAcc = LoopType t
                    return ClassMember.ValField (idAcc, tAcc)
               | SynMemberDefn.ImplicitCtor (_,_,ps,_,_) -> 
                    let! psAcc = mmap LoopSimplePat ps
                    return ClassMember.ImplicitCtor psAcc
               | SynMemberDefn.Member (b, _) -> 
                   return! LoopMemberBinding b
               | SynMemberDefn.LetBindings (es,_,_,_) ->
                   let! esAcc = mmap LoopBinding es
                   let esAcc' = List.map (fun (pAcc, eAcc) -> Let(false, [pAcc, eAcc], Lit(Unit))) esAcc
                   return ClassMember.LetBindings esAcc'
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
                    return TypeDef.None name 
                | SynTypeDefnSimpleRepr.TypeAbbrev (ty, _) ->
                    let! tAcc = LoopType ty
                    return TypeDef.Abbrev (name, tAcc) }
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
               | SynPat.Attrib (p, attrs, _) ->
                    let! pAcc = LoopPat p
                    let! attrsAcc = mmap LoopAttribute attrs
                    return Ast.PAttribute(pAcc, attrsAcc)
               | SynPat.Or (p1, p2, _) ->
                    let! p1Acc = LoopPat p1
                    let! p2Acc = LoopPat p2
                    return Ast.POr(p1Acc, p2Acc)
               | SynPat.Null _ ->
                    return Ast.PNull
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
               | SynPat.Record (xs, _) ->
                    let! xsAcc = mmap (fun ((_, i : Ident), p) -> cont { let! pAcc = LoopPat p
                                                                         return (i.idText, pAcc) }) xs
                    return Pat.PRecord xsAcc
               | SynPat.Wild _ -> 
                    return Pat.PWild
               | SynPat.Const (c, _) -> 
                    let! (Ast.Lit lit) = LoopConst c
                    return Pat.PLit lit
               | SynPat.ArrayOrList (_,xs,_) -> 
                    let! xsAcc = mmap LoopPat xs
                    return Ast.PList xsAcc
               | SynPat.IsInst (t, _) ->
                    let! tAcc = LoopType t
                    return Ast.PIsInst tAcc }

    and LoopAttribute (x:SynAttribute) =
        cont { let! argExprAcc = LoopExpr (x.ArgExpr)
               return Ast.Attribute(argExprAcc) }

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