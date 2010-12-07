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

let foldExp varF lamF appF letF litF withTyF exp =
  let rec Loop e cont =
      match e with
      | Var x -> cont (varF x)
      | Lam (x, body) -> Loop body (fun bodyAcc -> cont (lamF x bodyAcc))
      | App (l, r) -> Loop l (fun lAcc ->
                      Loop r (fun rAcc ->
                      cont (appF lAcc rAcc)))     
      | Let (p, e1, e2) -> Loop e1 (fun e1Acc ->
                           Loop e2 (fun e2Acc ->
                           cont (letF p e1Acc e2Acc)))
      | Lit x -> cont (litF x)
      | WithTy (e, t) -> Loop e (fun eAcc -> cont (withTyF eAcc t))
  Loop exp (fun x -> x)

let foldPat varF appF litF pat = 
  let rec Loop e cont =
      match e with
      | PVar x -> cont (varF x)      
      | PApp (l, r) -> Loop l (fun lAcc ->
                       Loop r (fun rAcc ->
                       cont (appF lAcc rAcc)))
      | PLit x -> cont (litF x)      
  Loop pat (fun x -> x)
