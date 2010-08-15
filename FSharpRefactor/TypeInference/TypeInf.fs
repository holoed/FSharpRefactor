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

module TypeInf

open Ast
open TypeTree
open StateMonad

type TyScheme = Type * Set<string>

type Env = Map<string, TyScheme>

type Subst = Map<string, Type>

let extend v t (s:Subst) : Subst = Map.add v t s

let lookup v (s:Subst) = 
    if (Map.containsKey v s) then 
        Map.find v s 
    else 
        TyVar v

let newTyVar = 
    state { let! x = getState
            do! setState(x + 1)
            return TyVar(sprintf "T%d" x) }

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

let rec typeVarsOfType = function
    | TyVar(tv)  -> Set.singleton tv
    | TyLam(a, r) -> typeVarsOfType(a) + typeVarsOfType(r)
    | TyCon(_, tyArgs) -> 
        (Set.empty, tyArgs) 
        ||> List.fold (fun acc ty -> acc + typeVarsOfType ty)

let typeVarsOfScheme((t, tyvars) : TyScheme) = 
    (typeVarsOfType t) - tyvars

let typeVarsOfEnv (env : Env) = 
    let schemes = env |> Map.toSeq |> Seq.map snd
    (Set.empty, schemes)
    ||> Seq.fold (fun acc s -> acc + (typeVarsOfScheme s))

exception UnificationException of Type * Type

let rec unify (a:Type) (b:Type) (s:Subst) =
    match (subs a s, subs b s) with
    | TyVar ta, TyVar tb when ta = tb -> s
    | TyVar ta, _ when not <| typeVarsOfType(b).Contains(ta) -> extend ta b s
    | _, TyVar _ -> unify b a s
    | TyLam (a1, b1), TyLam (a2, b2) -> unify a1 a2 (unify b1 b2 s)
    | TyCon(name1, args1), TyCon(name2, args2) when name1 = name2 ->
            (s, args1, args2) |||> List.fold2 (fun subst t1 t2 -> unify t1 t2 subst)
    | x,y -> raise <| UnificationException(x,y) 

let integer = TyCon("int", [])

let rec analyze (env: Env) (exp:Exp) (bt : Type) (s : Subst) = 
    state {
            match exp with
            | Lit v -> return unify integer bt s
            | Var n ->  if not (Map.containsKey n env) 
                        then failwith "Name %s no found" n 
                        let (t, _) = Map.find n env
                        return unify (subs t s) bt s
            | Lam (x, e) ->  
                        let! a = newTyVar
                        let! b = newTyVar
                        let s1 = unify bt (TyLam (a, b)) s
                        let newEnv = Map.add x (a, Set.empty) env
                        return! analyze newEnv e b s1
            | App(e1, e2) ->
                        let! a = newTyVar
                        let! s1 = analyze env e1 (TyLam(a, bt)) s
                        return! analyze env e2 a s1
            | InfixApp(e1, op, e2) ->
                        let exp1 = App(App(Var op, e1), e2)
                        return! analyze env exp1 bt s
            | Let(PVar name, inV, body) ->
                        let! a = newTyVar
                        let! s1 = analyze env inV a s
                        let t = subs a s1
                        let newScheme = t, ((typeVarsOfType t) - (typeVarsOfEnv env))
                        return! analyze (Map.add name newScheme env) body bt s1 }

let newTypeScheme t : TyScheme = (t, Set.empty)
 
let builtins : Env = 
    [
    "+", newTypeScheme(TyLam(integer, TyLam(integer, integer)))
    "*", newTypeScheme(TyLam(integer, TyLam(integer, integer)))
    "-", newTypeScheme(TyLam(integer, TyLam(integer, integer)))
    ] |> Map.ofList


let containsKey k = state { let! (map, id) = getState
                            return Map.containsKey k map }

let addName k = state { let! (map, id) = getState
                        let newid = char (int id + 1)
                        do! setState(Map.add k id map, newid) 
                        return () }

let getName k = state { let! success = containsKey k
                        if (not success) then 
                            do! addName k
                        let! (map, id) = getState
                        return Map.find k map }

let alpha t =    
    let rec run x = 
        state {
                match x with
                | TyVar(name) ->
                    let! newName = getName name             
                    return TyVar(sprintf "'%c" newName)
                | TyLam(arg, res) -> 
                    let! t1 = run arg
                    let! t2 = run res
                    return TyLam(t1, t2)
                | TyCon(name, typeArgs) -> 
                    let! list = mmap (fun x -> run x) typeArgs
                    return TyCon(name, list) }
    execute (run t) (Map.empty, 'a')

let typeOf (exp: Exp) : Type =
   let typeOf' exp = 
    state { let! (a:Type) = newTyVar
            let emptySubst : Subst = Map.empty
            let! s1 = analyze builtins exp a emptySubst
            return subs a s1 }
   in execute (typeOf' exp) 0 |> alpha