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
open StateMonad
open ContinuationMonad
     
let foldExpState varF 
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
                 errorF 
                 decl =
  let rec LoopExp e =
          cont {  match e with
                  | Var x -> return state { return! varF x }
                  | LongVar xs -> let! xsAcc = mmap LoopExp xs
                                  return state { return! longVarF xsAcc } 
                  | LongVarSet (e1, e2) -> let! e1Acc = LoopExp e1
                                           let! e2Acc = LoopExp e2
                                           return state { return! longVarSetF e1Acc e2Acc } 
                  | Lam (x, body) -> let! bodyAcc = LoopExp body
                                     return  state { return! (lamF x bodyAcc) }
                  | App (l, r) -> let! lAcc = LoopExp l
                                  let! rAcc = LoopExp r
                                  return state { return! (appF lAcc rAcc) }     
                  | Let (isRec, p, e1, e2) -> let! e1Acc = LoopExp e1
                                              let! e2Acc = LoopExp e2
                                              return state { return! (letF isRec p e1Acc e2Acc)}
                  | Lit x -> return state { return! (litF x)}
                
                  | Tuple es -> let! esAcc = mmap LoopExp es
                                return state { return! (tupleF esAcc) } 
                  | List es -> let! esAcc = mmap LoopExp es
                               return state { return! (listF esAcc) }  
                  | Match (e, cs) -> let! eAcc = LoopExp e
                                     let! csAcc = mmap  LoopClauses cs
                                     return state { return! (matchF eAcc csAcc)  } 
                  | ForEach (p, e1, e2) -> let! e1Acc = LoopExp e1
                                           let! e2Acc = LoopExp e2
                                           return state { return! (forEachF p e1Acc e2Acc)} 
                  | YieldOrReturn e -> let! eAcc = LoopExp e
                                       return state { return! (yieldOrRetF eAcc)} 
                  | IfThenElse (e1, e2, e3) -> let! e1Acc = LoopExp e1
                                               let! e2Acc = LoopExp e2
                                               let! e3Acc = match e3 with 
                                                            | Some e3' -> LoopExp e3'                                                            
                                               return state { return! ifThenElseF e1Acc e2Acc (Some e3Acc) } 
                  | DotIndexedSet (e1, es, e3) -> let! e1Acc = LoopExp e1
                                                  let! esAcc = mmap LoopExp es
                                                  let! e2Acc = LoopExp e3
                                                  return state { return! dotIndexedSetF e1Acc esAcc e2Acc }
                  | DotIndexedGet (e1, es) -> let! e1Acc = LoopExp e1
                                              let! esAcc = mmap LoopExp es
                                              return state { return! dotIndexedGetF e1Acc esAcc }
                  | Exp.Record xs -> let! xsAcc = mmap (fun x -> LoopRecordInst x) xs
                                     return state { return! recordInstF xsAcc }
                  | Exp.New (s, e) -> let! sAcc = LoopTypeInst s
                                      let! eAcc = LoopExp e
                                      return state { return! newF sAcc eAcc }
                  | Exp.ObjExpr ms -> let! msAcc = mmap LoopClassMember ms
                                      return state { return! (objExprF msAcc) }
                  | Exp.Do e -> let! eAcc = LoopExp e
                                return state { return! (doF eAcc) }
                  | Exp.Downcast (e, t) -> let! eAcc = LoopExp e
                                           let! tAcc = LoopTypeInst t
                                           return state { return! downcastF eAcc tAcc }
                  | Exp.Upcast (e, t) -> let! eAcc = LoopExp e
                                         let! tAcc = LoopTypeInst t
                                         return state { return! upcastF eAcc tAcc }
                  | ArbitraryAfterError -> return state { return! (errorF ()) } }

      and LoopTypeInst t = 
                cont { match t with
                       | LongIdent id -> let! idAcc = mmap LoopTypeInst id
                                         return idAcc |> List.concat
                       | Ident s -> return [s] }

      and LoopRecordInst (n, e) = 
                cont { let! eAcc = LoopExp e 
                       return state { return! recordFieldInstF n eAcc } }
                       
      and LoopClauses c =
                cont { match c with
                       | Clause(p, e) ->let! eAcc = LoopExp e
                                        return state { return! clauseF p eAcc } }

      and LoopTypes t = 
               cont { match t with
                      | DisUnion (name, cases) -> return state { return! (unionF name cases) } 
                      | Record (name, fields, ms) -> 
                            let! msAcc = mmap LoopClassMember ms
                            return state { return! (recordDefF name fields msAcc) } 
                      | None name -> return state { return! (noneF name) } 
                      | Class (name, members) -> 
                            let! msAcc = mmap LoopClassMember members
                            return state { return! (classF name msAcc) } }

      and LoopClassMember ms =
                cont { match ms with
                       | ImplicitCtor ps -> return state { return! (implicitConF ps) }
                       | Member (p, e) -> let! eAcc = LoopExp e
                                          return state { return! (memberF p eAcc) }
                       | Interface(t, ms) -> let! tAcc = LoopTypeInst t
                                             let! msAcc = mmap LoopClassMember ms
                                             return state { return! (interfaceF tAcc msAcc) }
                       | AbstractSlot n -> return state { return! (abstractSlotF n) } }

  let rec LoopDecl e = 
           cont { match e with
                  | Exp xs -> let! xsAcc = mmap LoopExp xs
                              return state { return! (expF xsAcc) } 
                  | Types xs -> let! xsAcc = mmap LoopTypes xs
                                return state { return! typesF xsAcc } 
                  | NestedModule (n, xs) ->  let! xsAcc = mmap LoopDecl xs
                                             return state { return! moduleF n xsAcc } 
                  | Open s -> return state { return! openF s } }
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
                                      

let foldExp varF 
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
            errorF 
            decl =       
     StateMonad.execute (foldExpState  (fun x -> state { return varF x })
                                       (fun xs -> state { let! xs' = mmapId xs
                                                          return longVarF xs' })
                                       (fun e1 e2 -> state { let! e1Acc = e1
                                                             let! e2Acc = e2
                                                             return longVarSetF e1Acc e2Acc })
                                       (fun ps b -> state { let! b' = b
                                                            return lamF ps b' })
                                       (fun x y -> state { let! x' = x
                                                           let! y' = y
                                                           return appF x' y' })
                                       (fun isRec p e1 e2 -> state { let! e1' = e1
                                                                     let! e2' = e2
                                                                     return letF isRec p e1' e2' })
                                       (fun x -> state { return litF x })
                                       (fun es -> state { let! es' = mmapId es
                                                          return tupleF es' })
                                       (fun es -> state { let! es' = mmapId es
                                                          return listF es' })
                                       (fun es -> state {  let! es' = mmapId es
                                                           return expF es' })
                                       (fun xs -> state { let! xs' = mmapId xs  
                                                          return typesF xs' })
                                       (fun name cases -> state { return unionF name cases })
                                       (fun e cs -> state { let! e' = e
                                                            let! cs' = mmapId cs
                                                            return matchF e' cs'})
                                       (fun p e -> state { let! e' = e  
                                                           return clauseF p e' })
                                       (fun p e1 e2 -> state { let! e1' = e1
                                                               let! e2' = e2
                                                               return forEachF p e1' e2' })
                                       (fun e -> state { let! e' = e  
                                                         return yieldOrRetF e' })
                                       (fun n es -> state { let! es' = mmapId es
                                                            return moduleF n es' })
                                       (fun s -> state { return openF s })
                                       (fun e1 e2 e3 -> state { let! e1' = e1
                                                                let! e2' = e2
                                                                let! e3' = match e3 with
                                                                           | Some x -> x
                                                                return ifThenElseF e1' e2' (Some e3') }) 
                                       (fun e1 es e3 -> state { let! e1' = e1
                                                                let! es' = mmapId es
                                                                let! e3' = e3
                                                                return dotIndexedSetF e1' es' e3' })
                                       (fun e1 es -> state { let! e1' = e1
                                                             let! es' = mmapId es
                                                             return dotIndexedGetF e1' es' })
                                       (fun name fields ms -> state { let! msAcc = mmapId ms
                                                                      return recordDefF name fields msAcc })
                                       (fun fields -> state { let! fields' = mmapId fields
                                                              return recordInstF fields' })
                                       (fun n e -> state {  let! eAcc = e
                                                            return recordFieldInstF n eAcc })
                                       (fun s e -> state { let! eAcc = e
                                                           return newF s eAcc })
                                       (fun name -> state { return noneF name })
                                       (fun n ms -> state { let! msAcc = mmapId ms
                                                            return classF n msAcc })
                                       (fun ps -> state { return (implicitConF ps) })
                                       (fun n e -> state { let! eAcc = e
                                                           return memberF n eAcc })
                                       (fun n -> state { return abstractSlotF n })
                                       (fun ms -> state { let! msAcc = mmapId ms
                                                          return objExprF msAcc })
                                       (fun e -> state { let! eAcc = e
                                                         return doF eAcc })
                                       (fun e t -> state { let! eAcc = e
                                                           return downcastF eAcc t })
                                       (fun e t -> state { let! eAcc = e
                                                           return upcastF eAcc t })
                                       (fun t ms -> state { let! msAcc = mmapId ms
                                                            return interfaceF t msAcc })
                                       (fun () -> state { return (errorF ()) })  decl) ()
                              
                                

                   
