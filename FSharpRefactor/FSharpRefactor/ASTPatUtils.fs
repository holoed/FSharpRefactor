module ASTPatUtils

open Ast
open StateMonad
open ASTScopeOperations

let flatPat p =
    let rec LoopPat pat =    
                ContinuationMonad.cont {  match pat with   
                                          | PAttribute(p, attrs) -> let! pAcc = LoopPat p
                                                                    return pAcc
                                          | POr (p1, p2) -> let! p1Acc = LoopPat p1
                                                            let! p2Acc = LoopPat p2
                                                            return p1Acc @ p2Acc  
                                          | PAnds ps -> let! psAcc = ContinuationMonad.mmap LoopPat ps
                                                        return List.concat psAcc                                                                                                 
                                          | PRecord x -> return [PRecord x]
                                          | PVar x -> return [PVar x]
                                          | PApp (l, r) -> let! lAcc = LoopPat l
                                                           let! rAcc = LoopPat r
                                                           return lAcc @ rAcc  
                                          | PLit x -> return [PLit x]
                                          | PTuple es -> let! esAcc = ContinuationMonad.mmap LoopPat es
                                                         return List.concat esAcc
                                          | PWild -> return []
                                          | PNull -> return [] 
                                          | PList es -> let! esAcc = ContinuationMonad.mmap LoopPat es
                                                        return List.concat esAcc
                                          | PLongVar xs -> let! xsAcc = ContinuationMonad.mmap LoopPat xs
                                                           return List.concat xsAcc
                                          | PIsInst t -> return []
                                          | PNamed (p1, p2) -> let! p1Acc = LoopPat p1
                                                               let! p2Acc = LoopPat p2
                                                               return p1Acc @ p2Acc }
    LoopPat p id

let execute action p =
    let rec LoopPat pat =    
                ContinuationMonad.cont {  match pat with
                                          | PVar (s:string,l) -> return state { do! action (s,l)
                                                                                if (not (s.Contains ".")) then
                                                                                   let! (OpenScopes(map), _) = getState
                                                                                   let subMatches = SubMatch map s
                                                                                   if (subMatches |> Seq.isEmpty |> not) then  
                                                                                        do! addUsage (mostRecent subMatches, l) 
                                                                                   return()
                                                                                else                                                                           
                                                                                    return () }
                                          | PApp (l, r) -> let! lAcc = LoopPat l
                                                           let! rAcc = LoopPat r
                                                           return state { let! l' = lAcc
                                                                          let! r' = rAcc
                                                                          return () }
                                          | PLit x -> return state { return () }
                                          | PTuple es -> let! esAcc = ContinuationMonad.mmap LoopPat es
                                                         return state { return () }
                                          | PRecord es -> let! esAcc = ContinuationMonad.mmap (fun (i, p) -> ContinuationMonad.cont { let! pAcc = LoopPat p
                                                                                                                                      return (i, pAcc) }) es
                                                          return state { let! _ = StateMonad.mmap (fun (i,e') -> state { let! eAcc' = e'
                                                                                                                         return (i, eAcc') }) esAcc 
                                                                         return () }
                                          | PWild -> return state { return () } 
                                          | PList es -> let! esAcc = ContinuationMonad.mmap LoopPat es
                                                        return state { return () }
                                          | PLongVar xs -> let! xsAcc = ContinuationMonad.mmap LoopPat xs
                                                           return state { return () }
                                          | PIsInst t -> return state { return () } }
    LoopPat p id