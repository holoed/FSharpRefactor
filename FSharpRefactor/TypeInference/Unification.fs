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

module Unification

open TypeTree
open Substitutions
open Environments

exception UnificationError of Type * Type

// Calculate the most general unifier of two types, 
// raising a UnificationError if there isn't one 
// mgu: Type -> Type -> Subst -> Subst
let rec mgu a b s =
    match (subs a s, subs b s) with
    | TyVar ta, TyVar tb when ta = tb -> s
    | TyVar ta, _ when not <| Set.contains ta (getTVarsOfType b) -> extend ta b s
    | _, TyVar _ -> mgu b a s
    | TyLam (a1, b1), TyLam (a2, b2) -> mgu a1 a2 (mgu b1 b2 s)
    | TyCon(name1, args1), TyCon(name2, args2) when name1 = name2 ->
            (s, args1, args2) |||> List.fold2 (fun subst t1 t2 -> mgu t1 t2 subst)
    | x,y -> raise <| UnificationError (x,y) 

