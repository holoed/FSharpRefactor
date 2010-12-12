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

let foldExp varF lamF appF letF litF withTyF tupleF exp = 
      let rec Loop e = 
           cont { match e with
                  | Var x -> return (varF x)                  
                  | Lam (p, body) -> let! bodyAcc = Loop body
                                     return lamF p bodyAcc
                  | App (l, r) -> let! lAcc = Loop l
                                  let! rAcc = Loop r
                                  return appF lAcc rAcc                  
                  | Let (p, e1, e2) -> let! e1Acc = Loop e1
                                       let! e2Acc = Loop e2
                                       return letF p e1Acc e2Acc
                  | Lit x -> return litF x
                  | WithTy (e, t) -> let! eAcc = Loop e
                                     return withTyF eAcc t
                  | Tuple es -> let! es' = mmap (fun x -> Loop x) es
                                return tupleF es' }
      Loop exp (fun x -> x)

let foldPat varF appF litF pat = 
  let rec Loop e =
      cont { match e with
             | PVar x -> return varF x      
             | PApp (l, r) -> let! lAcc = Loop l
                              let! rAcc = Loop r
                              return appF lAcc rAcc
             | PLit x -> return litF x }
  Loop pat (fun x -> x)

let foldExpState varF lamF appF letF litF withTyF exp =
  let rec Loop e cont =
      match e with
      | Var x -> state { return! cont (varF x) }
      | Lam (x, body) -> Loop body (fun bodyAcc -> state { return! cont (lamF x bodyAcc) })
      | App (l, r) -> Loop l (fun lAcc ->
                      Loop r (fun rAcc ->
                      state { return! cont (appF lAcc rAcc) }))     
      | Let (p, e1, e2) -> Loop e1 (fun e1Acc ->
                           Loop e2 (fun e2Acc ->
                           state { return! cont (letF p e1Acc e2Acc)}))
      | Lit x -> state { return! cont (litF x)}
      | WithTy (e, t) -> Loop e (fun eAcc -> state { return! cont (withTyF eAcc t)})
  Loop exp (fun x -> x)

let foldPatState varF appF litF pat = 
  let rec Loop e cont =
      match e with
      | PVar x -> state { return! cont (varF x) }
      | PApp (l, r) -> Loop l (fun lAcc ->
                       Loop r (fun rAcc ->
                       state { return! cont (appF lAcc rAcc) }))
      | PLit x -> state { return! cont (litF x) }
  Loop pat (fun x -> x)
