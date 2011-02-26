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

module AstCatamorphisms

open Ast
open ContinuationMonad
open Algebras

let foldExpAlgebra (algebra: AstAlgebra<_,_,_,_,_,_,_,_,_,_>) decl =
  let rec LoopExp e =
          cont {  match e with
                  | Var x -> return algebra.varF x 
                  | LongVar xs -> let! xsAcc = mmap LoopExp xs
                                  return algebra.longVarF xsAcc 
                  | LongVarSet (e1, e2) -> let! e1Acc = LoopExp e1
                                           let! e2Acc = LoopExp e2
                                           return algebra.longVarSetF e1Acc e2Acc 
                  | Lam (ps, body) -> let! psAcc = mmap LoopPat ps
                                      let! bodyAcc = LoopExp body
                                      return algebra. lamF psAcc bodyAcc
                  | App (l, r) -> let! lAcc = LoopExp l
                                  let! rAcc = LoopExp r
                                  return algebra.appF lAcc rAcc
                  | Let (isRec, bs, e) -> let! bsAcc = mmap LoopBinding bs
                                          let! eAcc = LoopExp e
                                          return algebra.letF isRec bsAcc eAcc
                  | LetBang (p, e1, e2) -> let! pAcc = LoopPat p
                                           let! e1Acc = LoopExp e1
                                           let! e2Acc = LoopExp e2
                                           return algebra.letBangF pAcc e1Acc e2Acc
                  | Lit x -> return algebra.litF x
                
                  | Tuple es -> let! esAcc = mmap LoopExp es
                                return algebra.tupleF esAcc
                  | List es -> let! esAcc = mmap LoopExp es
                               return algebra.listF esAcc
                  | Match (e, cs) -> let! eAcc = LoopExp e
                                     let! csAcc = mmap  LoopClauses cs
                                     return algebra.matchF eAcc csAcc
                  | ForEach (p, e1, e2) -> let! pAcc = LoopPat p
                                           let! e1Acc = LoopExp e1
                                           let! e2Acc = LoopExp e2
                                           return algebra.forEachF pAcc e1Acc e2Acc
                  | YieldOrReturn e -> let! eAcc = LoopExp e
                                       return algebra.yieldOrRetF eAcc
                  | YieldOrReturnFrom e -> let! eAcc = LoopExp e
                                           return algebra.yieldOrRetFromF eAcc
                  | IfThenElse (e1, e2, e3) -> let! e1Acc = LoopExp e1
                                               let! e2Acc = LoopExp e2
                                               let! e3Acc = match e3 with 
                                                            | Some e3' -> cont { let! e3Acc' = LoopExp e3'   
                                                                                 return Option.Some e3Acc' }
                                                            | Option.None -> cont { return Option.None }
                                               return algebra.ifThenElseF e1Acc e2Acc e3Acc
                  | DotGet (e, li) -> let! eAcc = LoopExp e
                                      let! liAcc = LoopExp li
                                      return algebra.dotGetF eAcc liAcc
                  | DotIndexedSet (e1, es, e3) -> let! e1Acc = LoopExp e1
                                                  let! esAcc = mmap LoopExp es
                                                  let! e2Acc = LoopExp e3
                                                  return algebra.dotIndexedSetF e1Acc esAcc e2Acc
                  | DotIndexedGet (e1, es) -> let! e1Acc = LoopExp e1
                                              let! esAcc = mmap LoopExp es
                                              return algebra.dotIndexedGetF e1Acc esAcc 
                  | Exp.Record xs -> let! xsAcc = mmap (fun x -> LoopRecordInst x) xs
                                     return algebra.recordInstF xsAcc 
                  | Exp.New (s, e) -> let! sAcc = LoopTypeInst s
                                      let! eAcc = LoopExp e
                                      return algebra.newF sAcc eAcc 
                  | Exp.ObjExpr ms -> let! msAcc = mmap LoopClassMember ms
                                      return algebra.objExprF msAcc
                  | Exp.Do e -> let! eAcc = LoopExp e
                                return algebra.doF eAcc
                  | Exp.DoBang e -> let! eAcc = LoopExp e
                                    return algebra.doBangF eAcc
                  | Exp.Downcast (e, t) -> let! eAcc = LoopExp e
                                           let! tAcc = LoopTypeInst t
                                           return algebra.downcastF eAcc tAcc
                  | Exp.Upcast (e, t) -> let! eAcc = LoopExp e
                                         let! tAcc = LoopTypeInst t
                                         return algebra.upcastF eAcc tAcc 
                  | Exp.TryWith (e, cl) -> let! eAcc = LoopExp e
                                           let! clAcc = mmap LoopClauses cl
                                           return algebra.tryWithF eAcc clAcc
                  | ArbitraryAfterError -> return algebra.errorF () }

      and LoopBinding (p, e) =
            cont { let! pAcc = LoopPat p
                   let! eAcc = LoopExp e
                   return (pAcc, eAcc) }

      and LoopTypeInst t = 
                cont { match t with   
                       | LongIdent ts -> let! tsAcc = mmap LoopTypeInst ts
                                         return algebra.tLongIdentF tsAcc
                       | Ident s -> return algebra.tIdentF s
                       | Type.TVar v -> let! vAcc = LoopTypeInst v
                                        return algebra.tvarF vAcc 
                       | Type.TFun (t1, t2) -> let! t1Acc = LoopTypeInst t1
                                               let! t2Acc = LoopTypeInst t2
                                               return algebra.tfunF t1Acc t2Acc } 

      and LoopRecordInst (n, e) = 
                cont { let! eAcc = LoopExp e 
                       return algebra.recordFieldInstF n eAcc }
                       
      and LoopClauses c =
                cont { match c with
                       | Clause(p, e) ->let! pAcc = LoopPat p
                                        let! eAcc = LoopExp e
                                        return algebra.clauseF pAcc eAcc }

      and LoopTypes t = 
               cont { match t with
                      | DisUnion (name, cases) -> return algebra.unionF name cases 
                      | Record (name, fields, ms) -> 
                            let! msAcc = mmap LoopClassMember ms
                            return algebra.recordDefF name fields msAcc  
                      | None name -> return algebra.noneF name
                      | Class (name, members) -> 
                            let! msAcc = mmap LoopClassMember members
                            return algebra.classF name msAcc
                      | Abbrev (name, ty) -> let! tyAcc = LoopTypeInst ty
                                             return algebra.abbrevF name tyAcc }

      and LoopClassMember ms =
                cont { match ms with
                       | ImplicitCtor ps -> let! psAcc = mmap LoopPat ps
                                            return algebra.implicitConF psAcc
                       | Member (p, e) -> let! pAcc = LoopPat p
                                          let! eAcc = LoopExp e
                                          return algebra.memberF pAcc eAcc
                       | Interface(t, ms) -> let! tAcc = LoopTypeInst t
                                             let! msAcc = mmap LoopClassMember ms
                                             return algebra.interfaceF tAcc msAcc
                       | AbstractSlot n -> return algebra.abstractSlotF n
                       | LetBindings es -> let! esAcc = mmap LoopExp es
                                           return algebra.letBindingsF esAcc }
       and LoopPat pat =    
                cont {    match pat with
                          | PVar x -> return algebra.pVarF x
                          | PApp (l, r) -> let! lAcc = LoopPat l
                                           let! rAcc = LoopPat r
                                           return algebra.pAppF lAcc rAcc  
                          | PLit x -> return algebra.pLitF x
                          | PTuple es -> let! esAcc = mmap LoopPat es
                                         return algebra.pTupleF esAcc
                          | PWild -> return algebra.pWildF () 
                          | PList es -> let! esAcc = mmap LoopPat es
                                        return algebra.pArrayOrListF esAcc
                          | PLongVar xs -> let! xsAcc = mmap LoopPat xs
                                           return algebra.pLongVarF xsAcc
                          | PIsInst t -> let! tAcc = LoopTypeInst t
                                         return algebra.pIsInstF tAcc }

  let rec LoopDecl e = 
           cont { match e with
                  | Exp xs -> let! xsAcc = mmap LoopExp xs
                              return algebra.expF xsAcc
                  | Types xs -> let! xsAcc = mmap LoopTypes xs
                                return algebra.typesF xsAcc 
                  | NestedModule (n, xs) ->  let! xsAcc = mmap LoopDecl xs
                                             return algebra.moduleF n xsAcc 
                  | Open s -> return algebra.openF s }
  LoopDecl decl id    


