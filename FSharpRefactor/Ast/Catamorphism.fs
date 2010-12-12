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
                  | Tuple es -> let! esAcc = mmap (fun x -> Loop x) es
                                return tupleF esAcc }
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

let foldExpState varF lamF appF letF litF withTyF tupleF exp =
  let rec Loop e =
          cont {  match e with
                  | Var x -> return state { return! varF x }
                  | Lam (x, body) -> let! bodyAcc = Loop body
                                     return  state { return! (lamF x bodyAcc) }
                  | App (l, r) -> let! lAcc = Loop l
                                  let! rAcc = Loop r
                                  return state { return! (appF lAcc rAcc) }     
                  | Let (p, e1, e2) -> let! e1Acc = Loop e1
                                       let! e2Acc = Loop e2
                                       return state { return! (letF p e1Acc e2Acc)}
                  | Lit x -> return state { return! (litF x)}
                  | WithTy (e, t) -> let! eAcc = Loop e
                                     return state { return! (withTyF eAcc t)}  
                  | Tuple es -> let! esAcc = mmap (fun x -> Loop x) es
                                return state { return! (tupleF esAcc) }  }
  Loop exp (fun x -> x)

let foldPatState varF appF litF pat = 
  let rec Loop e =
      cont {  match e with
              | PVar x -> return state { return! (varF x) }
              | PApp (l, r) -> let! lAcc = Loop l
                               let! rAcc = Loop r
                               return state { return! (appF lAcc rAcc) }     
              | PLit x -> return state { return! (litF x)}    }
  Loop pat (fun x -> x)
