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
open AstAlgebras
open StateMonad
open ContinuationMonad
     
let foldExpState (algebra:ExpStateAlgebra<_, _, _, _, _, _, _, _, _, _, _, _, _>)  decl =
  let rec LoopExp e =
          cont {  match e with
                  | Var x -> return state { return! algebra.varF x }
                  | LongVar xs -> let! xsAcc = mmap LoopExp xs
                                  return state { return! algebra.longVarF xsAcc } 
                  | LongVarSet (e1, e2) -> let! e1Acc = LoopExp e1
                                           let! e2Acc = LoopExp e2
                                           return state { return! algebra.longVarSetF e1Acc e2Acc } 
                  | Lam (x, body) -> let! bodyAcc = LoopExp body
                                     return  state { return! (algebra.lamF x bodyAcc) }
                  | App (l, r) -> let! lAcc = LoopExp l
                                  let! rAcc = LoopExp r
                                  return state { return! (algebra.appF lAcc rAcc) }     
                  | Let (isRec, p, e1, e2) -> let! e1Acc = LoopExp e1
                                              let! e2Acc = LoopExp e2
                                              return state { return! (algebra.letF isRec p e1Acc e2Acc)}
                  | Lit x -> return state { return! (algebra.litF x)}
                
                  | Tuple es -> let! esAcc = mmap LoopExp es
                                return state { return! (algebra.tupleF esAcc) } 
                  | List es -> let! esAcc = mmap LoopExp es
                               return state { return! (algebra.listF esAcc) }  
                  | Match (e, cs) -> let! eAcc = LoopExp e
                                     let! csAcc = mmap  LoopClauses cs
                                     return state { return! (algebra.matchF eAcc csAcc)  } 
                  | ForEach (p, e1, e2) -> let! e1Acc = LoopExp e1
                                           let! e2Acc = LoopExp e2
                                           return state { return! (algebra.forEachF p e1Acc e2Acc)} 
                  | YieldOrReturn e -> let! eAcc = LoopExp e
                                       return state { return! (algebra.yieldOrRetF eAcc)} 
                  | IfThenElse (e1, e2, e3) -> let! e1Acc = LoopExp e1
                                               let! e2Acc = LoopExp e2
                                               let! e3Acc = match e3 with 
                                                            | Some e3' -> LoopExp e3'                                                            
                                               return state { return! algebra.ifThenElseF e1Acc e2Acc (Some e3Acc) } 
                  | DotIndexedSet (e1, es, e3) -> let! e1Acc = LoopExp e1
                                                  let! esAcc = mmap LoopExp es
                                                  let! e2Acc = LoopExp e3
                                                  return state { return! algebra.dotIndexedSetF e1Acc esAcc e2Acc }
                  | DotIndexedGet (e1, es) -> let! e1Acc = LoopExp e1
                                              let! esAcc = mmap LoopExp es
                                              return state { return! algebra.dotIndexedGetF e1Acc esAcc }
                  | Exp.Record xs -> let! xsAcc = mmap (fun x -> LoopRecordInst x) xs
                                     return state { return! algebra.recordInstF xsAcc }
                  | Exp.New (s, e) -> let! sAcc = LoopTypeInst s
                                      let! eAcc = LoopExp e
                                      return state { return! algebra.newF sAcc eAcc }
                  | Exp.ObjExpr ms -> let! msAcc = mmap LoopClassMember ms
                                      return state { return! (algebra.objExprF msAcc) }
                  | Exp.Do e -> let! eAcc = LoopExp e
                                return state { return! (algebra.doF eAcc) }
                  | Exp.Downcast (e, t) -> let! eAcc = LoopExp e
                                           let! tAcc = LoopTypeInst t
                                           return state { return! algebra.downcastF eAcc tAcc }
                  | Exp.Upcast (e, t) -> let! eAcc = LoopExp e
                                         let! tAcc = LoopTypeInst t
                                         return state { return! algebra.upcastF eAcc tAcc }
                  | ArbitraryAfterError -> return state { return! (algebra.errorF ()) } }

      and LoopTypeInst t = 
                cont { match t with
                       | LongIdent id -> let! idAcc = mmap LoopTypeInst id
                                         return idAcc |> List.concat
                       | Ident s -> return [s] }

      and LoopRecordInst (n, e) = 
                cont { let! eAcc = LoopExp e 
                       return state { return! algebra.recordFieldInstF n eAcc } }
                       
      and LoopClauses c =
                cont { match c with
                       | Clause(p, e) ->let! eAcc = LoopExp e
                                        return state { return! algebra.clauseF p eAcc } }

      and LoopTypes t = 
               cont { match t with
                      | DisUnion (name, cases) -> return state { return! (algebra.unionF name cases) } 
                      | Record (name, fields, ms) -> 
                            let! msAcc = mmap LoopClassMember ms
                            return state { return! (algebra.recordDefF name fields msAcc) } 
                      | None name -> return state { return! (algebra.noneF name) } 
                      | Class (name, members) -> 
                            let! msAcc = mmap LoopClassMember members
                            return state { return! (algebra.classF name msAcc) } }

      and LoopClassMember ms =
                cont { match ms with
                       | ImplicitCtor ps -> return state { return! (algebra.implicitConF ps) }
                       | Member (p, e) -> let! eAcc = LoopExp e
                                          return state { return! (algebra.memberF p eAcc) }
                       | Interface(t, ms) -> let! tAcc = LoopTypeInst t
                                             let! msAcc = mmap LoopClassMember ms
                                             return state { return! (algebra.interfaceF tAcc msAcc) }
                       | AbstractSlot n -> return state { return! (algebra.abstractSlotF n) }
                       | LetBindings es -> let! esAcc = mmap LoopExp es
                                           return state { return! algebra.letBindingsF esAcc } }

  let rec LoopDecl e = 
           cont { match e with
                  | Exp xs -> let! xsAcc = mmap LoopExp xs
                              return state { return! (algebra.expF xsAcc) } 
                  | Types xs -> let! xsAcc = mmap LoopTypes xs
                                return state { return! algebra.typesF xsAcc } 
                  | NestedModule (n, xs) ->  let! xsAcc = mmap LoopDecl xs
                                             return state { return! algebra.moduleF n xsAcc } 
                  | Open s -> return state { return! algebra.openF s } }
  LoopDecl decl id

let foldPatState varF appF litF tupleF wildF arrayOrListF longVarF pat = 
  let rec Loop e =
      cont {  match e with
              | PVar x -> return state { return! (varF x) }
              | PApp (l, r) -> let! lAcc = Loop l
                               let! rAcc = Loop r
                               return state { return! (appF lAcc rAcc) }     
              | PLit x -> return state { return! (litF x) }
              | PTuple es -> let! esAcc = mmap Loop es
                             return state { return! (tupleF esAcc) } 
              | PWild -> return state { return! wildF () } 
              | PList es -> let! esAcc = mmap Loop es
                            return state { return! (arrayOrListF esAcc) }
              | PLongVar xs -> let! xsAcc = mmap Loop xs
                               return state { return! (longVarF xsAcc) } }
  Loop pat id

let foldPat varF appF litF tupleF wildF arrayOrListF longVarF pat = 
    StateMonad.execute (foldPatState (fun x -> state { return varF x })
                                     (fun x y -> state { let! x' = x
                                                         let! y' = y
                                                         return appF x' y' })
                                     (fun x -> state { return litF x })
                                     (fun es -> state { let! es' = mmapId es
                                                        return tupleF es' })
                                     (fun () -> state { return wildF () })
                                     (fun es -> state { let! es' = mmapId es
                                                        return arrayOrListF es' })
                                     (fun xs -> state { let! xsAcc = mmapId xs
                                                        return longVarF xsAcc}) pat) ()
                                      

let foldExp algebra decl =       
     StateMonad.execute (foldExpState (toStateMonad algebra)  decl) ()
                              
                                

                   
