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

module AstCatamorphisms

open Ast
open ContinuationMonad
     
let foldExp      varF 
                 longVarF
                 longVarSetF
                 lamF 
                 appF 
                 letF 
                 litF 
                 tupleF 
                 listF 
                 expF 
                 typesF 
                 unionF 
                 matchF
                 clauseF 
                 forEachF 
                 yieldOrRetF 
                 yieldOrRetFromF
                 moduleF 
                 openF 
                 ifThenElseF
                 dotIndexedSetF 
                 dotIndexedGetF 
                 recordDefF 
                 recordInstF 
                 recordFieldInstF 
                 newF 
                 noneF 
                 classF 
                 implicitConF 
                 memberF 
                 abstractSlotF 
                 objExprF 
                 doF 
                 downcastF 
                 upcastF 
                 interfaceF 
                 letBindingsF 
                 abbrevF 
                 tfunF 
                 tIdentF
                 tLongIdentF
                 tvarF
                 errorF 
                 decl =
  let rec LoopExp e =
          cont {  match e with
                  | Var x -> return varF x 
                  | LongVar xs -> let! xsAcc = mmap LoopExp xs
                                  return longVarF xsAcc 
                  | LongVarSet (e1, e2) -> let! e1Acc = LoopExp e1
                                           let! e2Acc = LoopExp e2
                                           return longVarSetF e1Acc e2Acc 
                  | Lam (x, body) -> let! bodyAcc = LoopExp body
                                     return  lamF x bodyAcc
                  | App (l, r) -> let! lAcc = LoopExp l
                                  let! rAcc = LoopExp r
                                  return appF lAcc rAcc
                  | Let (isRec, p, e1, e2) -> let! e1Acc = LoopExp e1
                                              let! e2Acc = LoopExp e2
                                              return letF isRec p e1Acc e2Acc
                  | Lit x -> return litF x
                
                  | Tuple es -> let! esAcc = mmap LoopExp es
                                return tupleF esAcc
                  | List es -> let! esAcc = mmap LoopExp es
                               return listF esAcc
                  | Match (e, cs) -> let! eAcc = LoopExp e
                                     let! csAcc = mmap  LoopClauses cs
                                     return matchF eAcc csAcc
                  | ForEach (p, e1, e2) -> let! e1Acc = LoopExp e1
                                           let! e2Acc = LoopExp e2
                                           return forEachF p e1Acc e2Acc
                  | YieldOrReturn e -> let! eAcc = LoopExp e
                                       return yieldOrRetF eAcc
                  | YieldOrReturnFrom e -> let! eAcc = LoopExp e
                                           return yieldOrRetFromF eAcc
                  | IfThenElse (e1, e2, e3) -> let! e1Acc = LoopExp e1
                                               let! e2Acc = LoopExp e2
                                               let! e3Acc = match e3 with 
                                                            | Some e3' -> LoopExp e3'                                                            
                                               return ifThenElseF e1Acc e2Acc (Some e3Acc)
                  | DotIndexedSet (e1, es, e3) -> let! e1Acc = LoopExp e1
                                                  let! esAcc = mmap LoopExp es
                                                  let! e2Acc = LoopExp e3
                                                  return dotIndexedSetF e1Acc esAcc e2Acc
                  | DotIndexedGet (e1, es) -> let! e1Acc = LoopExp e1
                                              let! esAcc = mmap LoopExp es
                                              return dotIndexedGetF e1Acc esAcc 
                  | Exp.Record xs -> let! xsAcc = mmap (fun x -> LoopRecordInst x) xs
                                     return recordInstF xsAcc 
                  | Exp.New (s, e) -> let! sAcc = LoopTypeInst s
                                      let! eAcc = LoopExp e
                                      return newF sAcc eAcc 
                  | Exp.ObjExpr ms -> let! msAcc = mmap LoopClassMember ms
                                      return objExprF msAcc
                  | Exp.Do e -> let! eAcc = LoopExp e
                                return doF eAcc
                  | Exp.Downcast (e, t) -> let! eAcc = LoopExp e
                                           let! tAcc = LoopTypeInst t
                                           return downcastF eAcc tAcc
                  | Exp.Upcast (e, t) -> let! eAcc = LoopExp e
                                         let! tAcc = LoopTypeInst t
                                         return upcastF eAcc tAcc 
                  | ArbitraryAfterError -> return errorF () }

      and LoopTypeInst t = 
                cont { match t with   
                       | LongIdent ts -> let! tsAcc = mmap LoopTypeInst ts
                                         return tLongIdentF tsAcc
                       | Ident s -> return tIdentF s
                       | Type.TVar v -> let! vAcc = LoopTypeInst v
                                        return tvarF vAcc 
                       | Type.TFun (t1, t2) -> let! t1Acc = LoopTypeInst t1
                                               let! t2Acc = LoopTypeInst t2
                                               return tfunF t1Acc t2Acc } 

      and LoopRecordInst (n, e) = 
                cont { let! eAcc = LoopExp e 
                       return recordFieldInstF n eAcc }
                       
      and LoopClauses c =
                cont { match c with
                       | Clause(p, e) ->let! eAcc = LoopExp e
                                        return clauseF p eAcc }

      and LoopTypes t = 
               cont { match t with
                      | DisUnion (name, cases) -> return unionF name cases 
                      | Record (name, fields, ms) -> 
                            let! msAcc = mmap LoopClassMember ms
                            return recordDefF name fields msAcc  
                      | None name -> return noneF name
                      | Class (name, members) -> 
                            let! msAcc = mmap LoopClassMember members
                            return classF name msAcc
                      | Abbrev (name, ty) -> let! tyAcc = LoopTypeInst ty
                                             return abbrevF name tyAcc }

      and LoopClassMember ms =
                cont { match ms with
                       | ImplicitCtor ps -> return implicitConF ps
                       | Member (p, e) -> let! eAcc = LoopExp e
                                          return memberF p eAcc
                       | Interface(t, ms) -> let! tAcc = LoopTypeInst t
                                             let! msAcc = mmap LoopClassMember ms
                                             return interfaceF tAcc msAcc
                       | AbstractSlot n -> return abstractSlotF n
                       | LetBindings es -> let! esAcc = mmap LoopExp es
                                           return letBindingsF esAcc }

  let rec LoopDecl e = 
           cont { match e with
                  | Exp xs -> let! xsAcc = mmap LoopExp xs
                              return expF xsAcc
                  | Types xs -> let! xsAcc = mmap LoopTypes xs
                                return typesF xsAcc 
                  | NestedModule (n, xs) ->  let! xsAcc = mmap LoopDecl xs
                                             return moduleF n xsAcc 
                  | Open s -> return openF s }
  LoopDecl decl id

let foldPat varF appF litF tupleF wildF arrayOrListF longVarF pat = 
  let rec Loop e =
      cont {  match e with
              | PVar x -> return varF x
              | PApp (l, r) -> let! lAcc = Loop l
                               let! rAcc = Loop r
                               return appF lAcc rAcc  
              | PLit x -> return litF x
              | PTuple es -> let! esAcc = mmap Loop es
                             return tupleF esAcc
              | PWild -> return wildF () 
              | PList es -> let! esAcc = mmap Loop es
                            return arrayOrListF esAcc
              | PLongVar xs -> let! xsAcc = mmap Loop xs
                               return longVarF xsAcc }
  Loop pat id


