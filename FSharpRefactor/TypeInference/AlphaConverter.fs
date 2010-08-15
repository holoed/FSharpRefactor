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

module AlphaConverter

open StateMonad
open TypeTree

let getName k =
    let containsKey k = state { let! (map, id) = getState
                                return Map.containsKey k map }

    let addName k = state { let! (map, id) = getState
                            let newid = char (int id + 1)
                            do! setState(Map.add k id map, newid) 
                            return () }

    state { let! success = containsKey k
            if (not success) then 
                do! addName k
            let! (map, id) = getState
            return Map.find k map }

// renameTVarsToLetters: Type -> Type
let renameTVarsToLetters t =    
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

