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

module Substitutions

open TypeTree

type Subst = Subst of Map<string, Type>

// extend: string -> Type -> Subst -> Subst
let extend v t (Subst s) = Subst (Map.add v t s)

// lookup: string -> Subst -> Type
let lookup v (Subst s) = 
    if (Map.containsKey v s) then 
        Map.find v s 
    else 
        TyVar v

// Apply a type substitution to a type 
// subs: Type -> Subst -> Type
let rec subs t s = 
    match t with
    | TyVar n -> 
          let t' = lookup n s
          if t = t' then t'
          else subs t' s    
    | TyLam(a, r) -> 
          TyLam(subs a s, subs r s)
    | TyCon(name, tyArgs) ->
            TyCon(name, tyArgs 
            |> List.map (fun x -> subs x s))