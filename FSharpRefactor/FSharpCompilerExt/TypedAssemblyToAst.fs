module TypedAssemblyToAst

open Ast
open ContinuationMonad
open Microsoft.FSharp.Compiler.Tast

let internal mkSrcLoc (r: Microsoft.FSharp.Compiler.Range.range)  = 
    { srcFilename = r.FileName; srcLine = { startLine = r.StartLine; endLine = r.EndLine }; 
                                srcColumn = { startColumn = r.StartColumn; endColumn = r.EndColumn } }
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
               | ModuleOrNamespaceExprWithSig.ModuleOrNamespaceExprWithSig(_, m, _) ->
                    let! mAcc = LoopModuleOrNamespaceExpr m
                    return Ast.Module.Exp mAcc }
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
               | ModuleOrNamespaceExpr.TMDefRec (_, _,nbs,_) ->
                    let! nbsAcc = mmap LoopNamespaceOrModuleBinding nbs
                    return List.concat nbsAcc }
    and LoopNamespaceOrModuleBinding x =
        cont { match x with
               | ModuleOrNamespaceBinding.ModuleOrNamespaceBinding(_, mne) ->
                    let! mneAcc = LoopModuleOrNamespaceExpr mne
                    return mneAcc }
    and LoopLetDef x =
        cont { match x with 
               | Binding.TBind (v,e,_) ->
                    let! eAcc = LoopExpr e
                    return Ast.Let(false, [Pat.PVar(v.DisplayName), eAcc], Lit(Unit)) }

    and LoopExpr x =
        cont { match x with
               | Expr.Let (b, e, _, _) ->
                    let! eAcc = LoopExpr e
                    return eAcc
               | Expr.Const (c, _, _) ->
                    let! cAcc = LoopConst c
                    return Lit(cAcc) }                      
    and LoopConst x =
        cont { match x with
               | Const.Int32 x -> return Literal.Integer x }

    LoopDecl decls id
