﻿// * **********************************************************************************************
// * Copyright (c) Edmondo Pentangelo. 
// *
// * This source code is subject to terms and conditions of the Apache License, Version 2.0. A 
// * copy of the license can be found in the License.html file at the root of this distribution. 
// * By using this source code in any fashion, you are agreeing to be bound 
// * by the terms of the Apache License, Version 2.0.
// *
// * You must not remove this notice, or any other, from this software.
// * **********************************************************************************************

// TODO: Implement Symbol Table Builder using the Symbol Table API in the following clean traversal of the syntax tree.
module SymbolTableBuilder

open Ast
open StateMonad
open AstCatamorphisms
open SymbolTableState
open Currying

let noOp = liftM

let noOp2 v = liftM2 (curry2 v)

let noOp3 v = liftM3 (curry3 v)

let flatPat p =
    let rec LoopPat pat =    
                ContinuationMonad.cont {  match pat with   
                                          | PParen p -> return! LoopPat p
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
                                          | PLongVar xs -> return []
                                          | PIsInst t -> return []
                                          | PNamed (p1, p2) -> let! p1Acc = LoopPat p1
                                                               let! p2Acc = LoopPat p2
                                                               return p1Acc @ p2Acc }
    LoopPat p id


let rec processPVar p = 
                state { 
                        match p with
                        | PVar (s:string,l) -> 
                                if (System.Char.IsUpper s.[0]) then
                                        do! addRef s l
                                else
                                        do! insert s l 
                        | PRecord xs -> do! processPVars (List.map (fun (_,x) -> x) xs) 
                        | _ -> return ()  }
and processPVars vars = 
        state {  let! _ = mmap processPVar vars 
                 return () }

let fromLoc loc n = { loc with srcColumn = { startColumn = loc.srcColumn.startColumn
                                             endColumn = loc.srcColumn.startColumn  + n } }

let varF x = state {  let (s : string, l) = x
                      let! state = getState
                      if ((s.Contains ".") && (System.Char.IsLower s.[0])) 
                      then do! s.Split '.' |> fun xs -> xs.[0]
                                           |> fun s -> addRef s (fromLoc l s.Length)
                           if (SymbolTable.containsDef s state)
                           then do! addRef s l
                                let last = s.Split '.' |> Seq.last
                                do! addRef s { l with srcColumn = { startColumn = l.srcColumn.startColumn + (s.Length - last.Length)
                                                                    endColumn = l.srcColumn.endColumn } }
                      else do! addRef s l
                      return Ast.Var x }

let lamF ps e = state { let! vars = mmap (fun p -> state { return! p }) ps
                        do! enter_scope
                        do! processPVars vars
                        let! e' = e
                        do! exit_scope 
                        return Lam (vars, e') }

let processBinding isRec (p, e) = 
        state { let! pAcc = p
                let flatPat = flatPat pAcc
                match pAcc with   
                | PApp(_, _) ->                                                                         
                    if ((List.length flatPat) > 1 && isRec) 
                    then let (PVar (s, l)) = flatPat.[0]
                         do! insert s l                                                                                                                                                    
                    do! enter_scope  
                    do! processPVars flatPat.Tail                                     
                    let! e' = e                                                           
                    do! exit_scope
                    if ((List.length flatPat) = 1) 
                    then let (PVar (s, l)) = flatPat.[0]
                         do! insert s l 
                    if ((List.length flatPat) > 1 && not isRec) 
                    then let (PVar (s, l)) = flatPat.[0]
                         do! insert s l   
                    return pAcc, e'
                | _ ->
                    let! pAcc  = p                                                   
                    do! enter_scope
                    let! e' = e   
                    do! exit_scope
                    do! processPVars flatPat  
                    return pAcc, e'                                                                                                                                                                                                                                                                               
                 }

let letF isRec bs e2 = state {  let! bsAcc = mmap (processBinding isRec) bs 
                                let! e2' = e2
                                return Let(isRec, bsAcc, Lit(Unit))   }

let letBangF p e1 e2 = state {  let! p' = p
                                let! e1' = e1
                                let! _ = processBinding false (p, e1)
                                let! e2' = e2
                                return LetBang (p', e1', e2') }

let unionF name cases = state { let! _ = mmap (fun (s,l) -> state { do! insert (sprintf "%s.%s" name s) l }) cases
                                return DisUnion(name, cases) }

let enumF name cases = state { let! _ = mmap (fun ((s,l), _) -> state { do! insert s l }) cases
                               return Enum(name, cases) }

let matchF e cs = state { let! e' = e
                          let! cs' = mmap (fun c -> state { return! c }) cs
                          return Match(e', cs')}

let clauseF p e = state { do! enter_scope
                          let! p' = p
                          let vars = flatPat p'                          
                          do! processPVars vars    
                          let! e' = e
                          do! exit_scope
                          return Clause(p', e') }

let forEachF p e1 e2 = state { let! p' = p
                               let vars = flatPat p'
                               let! e1' = e1
                               do! enter_scope
                               do! processPVars vars    
                               let! e2' = e2
                               do! exit_scope
                               return ForEach (p', e1', e2') }

let forF var startExp endExp bodyExp = 
                        state{ let! var' = var                               
                               let! startExp' = startExp
                               let! endExp' = endExp
                               do! enter_scope
                               do! processPVar var'
                               let! bodyExp' = bodyExp
                               do! exit_scope                               
                               return For(var', startExp', endExp', bodyExp') }

let moduleF n ms = state { let! ms' = mmap (fun m -> state { return! m }) ms
                           return NestedModule (n, ms') }

let hashdirectiveF s ss = state { return HashDirective (s, ss) }

let moduleAbbrevF s ss = state { return ModuleAbbrev (s, ss) }

let exceptionDefF n ms = state { let! msAcc = mmap (fun m -> state { return! m }) ms
                                 return ExceptionDef (n, msAcc) }

let optionMap f m = state { match m with
                            | Some x -> let! x' = f x
                                        return Some x'
                            | Option.None -> return Option.None }

let ifThenElseF e1 e2 e3 = state { let! e1' = e1
                                   let! e2' = e2
                                   let! e3' = optionMap id e3
                                   return IfThenElse(e1', e2', e3')}

let recordDefF name fields ms = state { let! msAcc = mmap (fun m -> state { return! m }) ms
                                        return Record(name, fields, msAcc) }

let classF n ms = state {  let! ic = mmap (fun x -> state { let! xAcc = x
                                                            match xAcc with | ImplicitCtor ps -> return ps | _ -> return [] }) ms
                           do! enter_scope
                           let! _ = mmap (fun ps -> processPVars ps) ic
                           let! msAcc = mmap (fun m -> state { return! m }) (if ic.IsEmpty then ms else ms.Tail)
                           do! exit_scope
                           return Class (n, msAcc) }

let valfieldF t1 t2 = state { let! t1' = optionMap id t1
                              let! t2' = t2
                              return ValField (t1', t2') }

let inheritF t1 t2 = state { let! t1' = t1
                             let! t2' = optionMap id t2
                             return Inherit (t1', t2') }

let implicitInheritF t e idOption = state { let! t' = t
                                            let! e' = e
                                            let! idOption' = optionMap id idOption
                                            return ImplicitInherit (t', e', idOption') }

let memberF isInstance p e = state { let! p' = p
                                     let vars = flatPat p'
                                     do! enter_scope
                                     do! processPVars vars
                                     let! e' = e    
                                     do! exit_scope                        
                                     return Member(isInstance, p', e') }

let interfaceF t msOption = state { let! t' = t
                                    let! ms' = optionMap mmapId msOption
                                    return Interface (t', ms') }

let pRecordF es = state { let! esAcc = mmap (fun (i, e) -> state { let! eAcc = e
                                                                   return i, eAcc }) es
                          return PRecord esAcc }

let pLongVarF xs = state {  let! xs' = mmapId xs   
                            let s = xs' |> List.map (fun (PVar (s,_)) -> s)
                                        |> fun xs -> System.String.Join(".", xs)
                            let ls = xs' |> List.map (fun (PVar (_, l)) -> l)
                            let first = List.head ls
                            let last = Seq.last ls
                            let l = { first with srcColumn = { startColumn = first.srcColumn.startColumn
                                                               endColumn = last.srcColumn.endColumn } }
                            if ((s.Contains ".") && (System.Char.IsLower s.[0])) 
                            then let ident = Seq.head (s.Split '.')
                                 do! insert ident (fromLoc l (ident.Length))
                            else  do! addRef s l
                            return PLongVar xs' }

let private buildSymbolTable' exp : State<'t, Ast.Module<'a>> = 
        foldExpAlgebra { memberSigF = noOp Ast.MemberSig
                         traitCallF = fun ss -> noOp3 Ast.TraitCall (state.Return ss)
                         typetestF = noOp2 Ast.TypeTest
                         measureVarF = fun s -> state.Return (Ast.MVar s)
                         measureOneF = fun _ -> state.Return Ast.One 
                         measureAnonF =  fun _ -> state.Return Ast.Anon 
                         measureProductF = noOp2 Ast.Product
                         measureDivideF = noOp2 Ast.Divide
                         powerF = fun m n -> noOp2 Ast.Power m (state.Return n)
                         measureF = noOp2 Ast.Measure
                         measureSeqF = fun ms -> noOp Ast.Seq (mmapId ms) 
                         measureNamedF = noOp Ast.Named                        
                         quoteF = noOp2 Ast.Quote
                         inferredDowncastF = noOp Ast.InferredDowncast                            
                         inferredUpcastF   = noOp Ast.InferredUpcast 
                         lazyF = noOp Ast.Lazy 
                         parenF = noOp Ast.Paren
                         whileF = noOp2 Ast.While
                         assertF = noOp Ast.Assert
                         nullF = fun () -> state.Return Null 
                         varF = varF
                         longVarSetF = noOp2 Ast.LongVarSet
                         lamF = lamF
                         appF = noOp2 Ast.App
                         letF = letF
                         letBangF = letBangF
                         litF = fun x -> state.Return (Lit x) 
                         tupleF = fun es -> noOp Ast.Tuple (mmapId es)
                         listF = fun es -> noOp Ast.List (mmapId es)
                         expF = fun es -> noOp Ast.Exp (mmapId es)
                         typesF = fun xs -> noOp Ast.Types (mmapId xs)
                         unionF = unionF     
                         enumF = enumF      
                         matchF = matchF
                         clauseF = clauseF
                         forEachF = forEachF
                         forF = forF
                         yieldOrRetF = noOp Ast.YieldOrReturn                
                         yieldOrRetFromF = noOp Ast.YieldOrReturnFrom
                         moduleF = moduleF
                         openF = fun s -> state.Return (Open s)
                         exceptionF = noOp Ast.Exception
                         hashdirectiveF = hashdirectiveF
                         moduleAbbrevF = moduleAbbrevF
                         attributesF = fun xs -> noOp Ast.Attributes (mmapId xs)                        
                         exceptionDefF = exceptionDefF
                         ifThenElseF = ifThenElseF
                         dotGetF = noOp2 Ast.DotGet
                         dotSetF = noOp3 Ast.DotSet
                         dotIndexedSetF = fun e1 es e3 -> (noOp3 Ast.DotIndexedSet) e1 (mmapId es) e3
                         dotIndexedGetF =  fun e1 es -> noOp2 Ast.DotIndexedGet e1 (mmapId es)
                         recordDefF = recordDefF
                         recordInstF = fun fields -> noOp Exp.Record (mmapId fields)                         
                         recordFieldInstF = fun n -> noOp (fun eAcc -> (n, eAcc))                             
                         newF = noOp2 Ast.New
                         typeappF = fun e ts -> noOp2 Ast.TypeApp e (mmapId ts)
                         noneF = fun name -> state.Return (None name)
                         classF = classF
                         implicitConF = fun ps -> noOp Ast.ImplicitCtor (mmapId ps)                        
                         valfieldF = valfieldF
                         inheritF  = inheritF
                         implicitInheritF = implicitInheritF
                         memberF = memberF
                         abstractSlotF = fun n -> state.Return (AbstractSlot n)
                         objExprF = fun ms -> noOp Ast.ObjExpr (mmapId ms)                    
                         doF = noOp Ast.Do 
                         addressofF = noOp Ast.AddressOf
                         doBangF = noOp Ast.DoBang
                         downcastF = noOp2 Ast.Downcast
                         upcastF = noOp2 Ast.Upcast
                         typedF = noOp2 Ast.Typed
                         interfaceF = interfaceF
                         letBindingsF =  fun es -> noOp Ast.LetBindings (mmapId es)
                         abbrevF =  fun n -> noOp2 Ast.Abbrev (state.Return n)
                         tfunF = noOp2 Ast.TFun
                         tIdentF = fun s -> state.Return (Ident s)
                         tLongIdentF = fun ts -> noOp Ast.LongIdent (mmapId ts)                     
                         tvarF =  noOp Ast.TVar
                         tappF = fun t ts -> noOp2 Ast.TApp t (mmapId ts)
                         ttupleF = fun ts -> noOp Ast.TTuple (mmapId ts)
                         tarrayF = fun n -> noOp2 Ast.TArray (state.Return n)
                         tmeasurePowerF = fun t n -> noOp2 Ast.TMeasurePower t (state.Return n)
                         tanonF = fun () -> state.Return TAnon 
                         tmeasureOneF = fun () -> state.Return TMeasureOne 
                         tryWithF = fun e cs -> noOp2 Ast.TryWith e (mmapId cs)
                         tryFinallyF = noOp2 Ast.TryFinally
                         errorF = fun () -> state.Return ArbitraryAfterError                          
                         pVarF = fun x -> state.Return (PVar x)
                         pParenF = noOp Ast.PParen
                         pAppF = noOp2 Ast.PApp
                         porF = noOp2 Ast.POr
                         pandsF = fun ps -> noOp Ast.PAnds (mmapId ps)                                                        
                         pLitF = fun x -> state.Return (PLit x)
                         pTupleF = fun es -> noOp Ast.PTuple (mmapId es)                    
                         pRecordF = pRecordF
                         pWildF = fun () -> state.Return PWild 
                         pArrayOrListF = fun es -> noOp Ast.PList (mmapId es)
                         pLongVarF = pLongVarF
                         pIsInstF  = noOp Ast.PIsInst              
                         pnullF = fun () -> state.Return PNull 
                         pattributeF = fun p attrs -> noOp2 Ast.PAttribute p (mmapId attrs)
                         attributeF = noOp Ast.Attribute
                         pnamedF = noOp2 Ast.PNamed }   exp

let private buildSymbolTable exp = mmap buildSymbolTable' exp

let findAllReferences s l progs =
    let m = buildSymbolTable progs
    let state = SymbolTable.empty |> StateMonad.executeGetState m
    SymbolTable.lookUp s l state |> Seq.distinct
                                 |> Seq.toList