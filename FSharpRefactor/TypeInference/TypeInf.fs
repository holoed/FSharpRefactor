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
open Ast
open Substitutions
open Environments
open Unification
open StateMonad
open AlphaConverter

let newTyVar = 
    state { let! x = getState
            do! setState(x + 1)
            return TyVar(sprintf "T%d" x) }

let litToTy lit = 
    match lit with
    | Integer _ -> tyInteger
    | Float _ -> tyFloat
    | Char _ -> tyChar
    | String  _ -> tyString

// Calculate the principal type scheme for an expression in a given 
// typing environment 
// tp: Env -> Exp -> Type -> Subst -> State<int, Subst>
let rec tp env exp bt s = 
    let findSc n (Env e) = Map.find n e
    let containsSc n (Env e) = Map.containsKey n e
    let addSc n sc (Env e) = Env (Map.add n sc e)
    state {
            match exp with
            | Lit v ->  return mgu (litToTy v) bt s
            | Var n ->  if not (containsSc n env) 
                        then failwith "Name %s no found" n 
                        let (TyScheme (t, _)) = findSc n env
                        return mgu (subs t s) bt s
            | Lam ([PVar x], e) ->  
                        let! a = newTyVar
                        let! b = newTyVar
                        let s1 = mgu bt (TyLam (a, b)) s
                        let newEnv = addSc x (TyScheme (a, Set.empty)) env
                        return! tp newEnv e b s1
            | Lam ([PWithTy(PVar x, t)], e) ->  
                        let! b = newTyVar
                        let s1 = mgu bt (TyLam (t, b)) s
                        let newEnv = addSc x (TyScheme (t, Set.empty)) env
                        return! tp newEnv e b s1
            | App(e1, e2) ->
                        let! a = newTyVar
                        let! s1 = tp env e1 (TyLam(a, bt)) s
                        return! tp env e2 a s1
            | InfixApp(e1, op, e2) ->
                        let exp1 = App(App(Var op, e1), e2)
                        return! tp env exp1 bt s
            | Let(PVar name, inV, body) ->
                        let! a = newTyVar
                        let! s1 = tp env inV a s
                        let t = subs a s1
                        let newScheme = TyScheme (t, ((getTVarsOfType t) - (getTVarsOfEnv env)))
                        return! tp (addSc name newScheme env) body bt s1 }
 
let predefinedEnv = 
    Env(["+", TyScheme (TyLam(tyNum, TyLam(tyNum, tyNum)), Set.empty)
         "*", TyScheme (TyLam(tyInteger, TyLam(tyInteger, tyInteger)), Set.empty)
         "-", TyScheme (TyLam(tyInteger, TyLam(tyInteger, tyInteger)), Set.empty)
           ] |> Map.ofList)

// typeOf: Exp -> Type
let typeOf exp =
   let typeOf' exp = 
    state { let! (a:Type) = newTyVar
            let emptySubst = Subst (Map.empty)
            let! s1 = tp predefinedEnv exp a emptySubst
            return subs a s1 }
   in execute (typeOf' exp) 0 |> renameTVarsToLetters