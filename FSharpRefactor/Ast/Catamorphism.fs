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

module AstCatamorphisms

open Ast
open StateMonad
open ContinuationMonad
     
let foldExpState varF lamF appF letF litF tupleF listF expF typesF unionF matchF clauseF decl =
  let rec LoopExp e =
          cont {  match e with
                  | Var x -> return state { return! varF x }
                  | Lam (x, body) -> let! bodyAcc = LoopExp body
                                     return  state { return! (lamF x bodyAcc) }
                  | App (l, r) -> let! lAcc = LoopExp l
                                  let! rAcc = LoopExp r
                                  return state { return! (appF lAcc rAcc) }     
                  | Let (p, e1, e2) -> let! e1Acc = LoopExp e1
                                       let! e2Acc = LoopExp e2
                                       return state { return! (letF p e1Acc e2Acc)}
                  | Lit x -> return state { return! (litF x)}
                
                  | Tuple es -> let! esAcc = mmap (fun x -> LoopExp x) es
                                return state { return! (tupleF esAcc) } 
                  | List es -> let! esAcc = mmap (fun x -> LoopExp x) es
                               return state { return! (listF esAcc) }  
                  | Match (e, cs) -> let! eAcc = LoopExp e
                                     let! csAcc = mmap (fun x -> LoopClauses x) cs
                                     return state { return! (matchF eAcc csAcc)  } }
      and LoopClauses c =
                cont { match c with
                       | Clause(p, e) ->let! eAcc = LoopExp e
                                        return state { return! clauseF p eAcc } }

  let rec LoopTypes t = 
           cont { match t with
                  | DisUnion (name, cases) -> return state { return! (unionF name cases) }  }

  let rec LoopDecl e = 
           cont { match e with
                  | Exp x -> let! x' = LoopExp x
                             return state { return! (expF x') } 
                  | Types xs -> let! xsAcc = mmap (fun x -> LoopTypes x) xs
                                return state { return! typesF xsAcc } }
  LoopDecl decl (fun x -> x)

let foldPatState varF appF litF tupleF wildF pat = 
  let rec Loop e =
      cont {  match e with
              | PVar x -> return state { return! (varF x) }
              | PApp (l, r) -> let! lAcc = Loop l
                               let! rAcc = Loop r
                               return state { return! (appF lAcc rAcc) }     
              | PLit x -> return state { return! (litF x) }
              | PTuple es -> let! esAcc = mmap (fun x -> Loop x) es
                             return state { return! (tupleF esAcc) } 
              | PWild -> return state { return! wildF () } }
  Loop pat (fun x -> x)

let foldPat varF appF litF tupleF wildF pat = 
    StateMonad.execute (foldPatState (fun x -> state { return varF x })
                                     (fun x y -> state { let! x' = x
                                                         let! y' = y
                                                         return appF x' y' })
                                     (fun x -> state { return litF x })
                                     (fun es -> state { let! es' = StateMonad.mmap (fun e -> state { return! e }) es
                                                        return tupleF es' })
                                     (fun () -> state { return wildF () }) pat) ()
                                      

let foldExp varF lamF appF letF litF tupleF listF expF typesF unionF matchF clauseF decl =       

     StateMonad.execute (foldExpState  (fun x -> state { return varF x })
                                       (fun ps b -> state { let! b' = b
                                                            return lamF ps b' })
                                       (fun x y -> state { let! x' = x
                                                           let! y' = y
                                                           return appF x' y' })
                                       (fun p e1 e2 -> state { let! e1' = e1
                                                               let! e2' = e2
                                                               return letF p e1' e2' })
                                       (fun x -> state { return litF x })
                                       (fun es -> state { let! es' = StateMonad.mmap (fun e -> state { return! e }) es
                                                          return tupleF es' })
                                       (fun es -> state { let! es' = StateMonad.mmap (fun e -> state { return! e }) es
                                                          return listF es' })
                                       (fun e -> state {  let! e' = e
                                                          return expF e' })
                                       (fun xs -> state { let! xs' = StateMonad.mmap (fun x -> state { return! x }) xs  
                                                          return typesF xs' })
                                       (fun name cases -> state { return unionF name cases })
                                       (fun e cs -> state { let! e' = e
                                                            let! cs' = StateMonad.mmap (fun c -> state { return! c }) cs
                                                            return matchF e' cs'})
                                       (fun p e -> state { let! e' = e  
                                                           return clauseF p e' })  decl) ()
                              
                                

                   
