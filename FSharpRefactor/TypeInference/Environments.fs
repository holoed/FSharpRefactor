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

module Environments

open TypeTree

type TyScheme = TyScheme of Type * Set<string>

type Env = Env of Map<string, TyScheme>

// Calculate the list of type variables occurring in a type,
// without repeats
// getTVarsOfType: Type -> Set<string>
let rec getTVarsOfType t = 
    match t with
    | TyVar n  -> Set.singleton n
    | TyLam(t1, t2) -> getTVarsOfType(t1) + getTVarsOfType(t2)
    | TyCon(_, args) -> 
        List.fold (fun acc t -> acc + getTVarsOfType t) Set.empty args

// getTVarsOfScheme: TyScheme -> Set<string>
let getTVarsOfScheme (TyScheme (t, tvars)) = 
    (getTVarsOfType t) - tvars

// getTVarsOfEnv: Env -> Set<string>
let getTVarsOfEnv (Env e) = 
    let schemes = e |> Map.toSeq |> Seq.map snd
    Seq.fold (fun acc s -> acc + (getTVarsOfScheme s)) Set.empty schemes