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

let foldExp varF lamF appF letF litF tupleF listF expF typesF unionF decl = 
      let rec LoopExp e = 
           cont { match e with
                  | Var x -> return (varF x)                  
                  | Lam (p, body) -> let! bodyAcc = LoopExp body
                                     return lamF p bodyAcc
                  | App (l, r) -> let! lAcc = LoopExp l
                                  let! rAcc = LoopExp r
                                  return appF lAcc rAcc                  
                  | Let (p, e1, e2) -> let! e1Acc = LoopExp e1
                                       let! e2Acc = LoopExp e2
                                       return letF p e1Acc e2Acc
                  | Lit x -> return litF x
                 
                  | Tuple es -> let! esAcc = mmap (fun x -> LoopExp x) es
                                return tupleF esAcc
                  | List es -> let! esAcc = mmap (fun x -> LoopExp x) es
                               return listF esAcc  }
      let rec LoopTypes t = 
           cont { match t with
                  | DisUnion (name, cases) -> return (unionF name cases)  }

      let rec LoopDecl e = 
           cont { match e with
                  | Exp x -> let! x' = LoopExp x
                             return expF x' 
                  | Types xs -> let! xsAcc = mmap (fun x -> LoopTypes x) xs
                                return typesF xsAcc }
      LoopDecl decl (fun x -> x)
      

let foldPat varF appF litF tupleF pat = 
  let rec Loop e =
      cont { match e with
             | PVar x -> return varF x      
             | PApp (l, r) -> let! lAcc = Loop l
                              let! rAcc = Loop r
                              return appF lAcc rAcc
             | PLit x -> return litF x 
             | PTuple es -> let! esAcc = mmap (fun x -> Loop x) es
                            return tupleF esAcc }
  Loop pat (fun x -> x)

let foldExpState varF lamF appF letF litF tupleF listF expF typesF unionF  decl =
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
                               return state { return! (listF esAcc) }  }

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

let foldPatState varF appF litF tupleF pat = 
  let rec Loop e =
      cont {  match e with
              | PVar x -> return state { return! (varF x) }
              | PApp (l, r) -> let! lAcc = Loop l
                               let! rAcc = Loop r
                               return state { return! (appF lAcc rAcc) }     
              | PLit x -> return state { return! (litF x) }
              | PTuple es -> let! esAcc = mmap (fun x -> Loop x) es
                             return state { return! (tupleF esAcc) } }
  Loop pat (fun x -> x)
