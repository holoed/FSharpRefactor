// * **********************************************************************************************
// * Copyright (c) Edmondo Pentangelo. 
// *
// * This source code is subject to terms and conditions of the Apache License, Version 2.0. A 
// * copy of the license can be found in the License.html file at the root of this distribution. 
// * By using this source code in any fashion, you are agreeing to be bound 
// * by the terms of the Apache License, Version 2.0.
// *
// * You must not remove this notice, or any other, from this software.
// * **********************************************************************************************

module ASTAnalysis

open Ast
open StateMonad
open AstCatamorphisms
open Currying
open PurelyFunctionalDataStructures
open ASTScopeOperations
open ASTPatUtils

let noOp = liftM

let noOp2 v = liftM2 (curry2 v)

let noOp3 v = liftM3 (curry3 v)

let varF x = state {    let (s : string,l) = x                                                          
                        let { srcFilename = fileName
                                            srcLine = { startLine = startLine; endLine = endLine }
                                            srcColumn = { startColumn = startColumn; endColumn = endColumn } } = l                                                                                                                    

                        if (not (s.Contains ".")) then
                            let! (OpenScopes(map), _) = getState
                            let subMatches = SubMatch map s
                            if (subMatches |> Seq.isEmpty |> not) then  
                                do! addUsage (mostRecent subMatches, l) 
                            else
                                do! addUsage x                                                            
                        else
                            let v = s.Split '.' |> fun xs -> xs.[0]
                            let! (OpenScopes(map), _) = getState
                            if (map.ContainsKey v) then                             
                                do! addUsage (v, { srcFilename = fileName
                                                   srcLine = { startLine = startLine; endLine = startLine } 
                                                   srcColumn = { startColumn = startColumn; endColumn = startColumn  + v.Length } })                                                            
                            else
                                let l' = { srcFilename = fileName
                                           srcLine = { startLine = endLine; endLine = endLine } 
                                           srcColumn = { startColumn = endColumn - (s.Length - v.Length - 1); endColumn = endColumn  } }
                                do! addUsage (s, l')
                                                                                                                                                                              
                        return Var x }

let lamF ps e = state { let! vars = mmap (fun p -> state { return! p }) ps
                        let! _ = mmap (fun x -> execute enterScope x) vars
                        let! e' = e
                        let! _ = mmap (fun x -> execute exitScope x) vars 
                        return Lam (vars, e') }

               
let letF isRec bs e2 = state {  let! bsAcc = mmap (fun (p, e1) -> 
                                                state {
                                                        let! pAcc = p
                                                        let flatpat = flatPat pAcc
                                                        match pAcc with   
                                                        | PApp(_, _) ->
                                                                let (PVar (sf,lf)) = List.head flatpat
                                                                let vars = List.tail flatpat                                                                                                                                
                                                                if isRec then 
                                                                    do! enterScope (sf, lf) 
                                                                let! _ = mmap (fun x -> execute enterScope x) vars // ----------------------------------------------------
                                                                let! e1' = e1                                      // function variables scope (like the x in let f x = x)                                  
                                                                let! _ = mmap (fun x -> execute exitScope x) vars  // ----------------------------------------------------
                                                                if (not isRec) then
                                                                    do! enterScope (sf, lf)     // ----------------------------------------------------                                           
                                                                let! e2' = e2               // function name scope (like the f in let f x = x)  
                                                                if (e2' <> (Lit Unit)) then
                                                                    do! exitScope (sf, lf)      // ----------------------------------------------------                               
                                                                return pAcc, e1' 
                                                        | _ ->
                                                            let! p'  = p                                                 
                                                            let! e1' = e1       
                                                            let! _ = mmap (fun x -> execute enterScope x) flatpat                               
                                                            let! e2' = e2               // function name scope (like the f in let f x = x)                                                                                                               
                                                            if (e2' <> (Lit Unit)) then
                                                                let! _ = mmap (fun x -> execute exitScope x) flatpat  // ---------------------------------------------------- 
                                                                do ()
                                                            return p', e1' }) bs 
                                return Let(isRec, bsAcc, Lit(Unit))   }

let letBangF p e1 e2 = state {  let! pAcc = p
                                let flatpat = flatPat pAcc
                                match pAcc with   
                                | PApp(_, _) ->
                                        let (PVar (sf,lf)) = List.head flatpat
                                        let vars = List.tail flatpat                                                                                                                                
                                        let! _ = mmap (fun x -> execute enterScope x) vars // ----------------------------------------------------
                                        let! e1' = e1                                      // function variables scope (like the x in let f x = x)                                  
                                        let! _ = mmap (fun x -> execute exitScope x) vars  // ----------------------------------------------------
                                        do! enterScope (sf, lf)     // ----------------------------------------------------                                           
                                        let! e2' = e2               // function name scope (like the f in let f x = x)  
                                        if (e2' <> (Lit Unit)) then
                                            do! exitScope (sf, lf)      // ----------------------------------------------------                               
                                        return LetBang (pAcc, e1', e2') 
                                | _ ->
                                    let! p'  = p                                                 
                                    let! e1' = e1       
                                    let! _ = mmap (fun x -> execute enterScope x) flatpat                                            
                                    let! e2' = e2           // let x = x in x Only "let x" and "in x" refer to the same identifier.
                                    if (e2' <> (Lit Unit)) then
                                        let! _ = mmap (fun x -> execute exitScope x) flatpat  // ---------------------------------------------------- 
                                        do ()
                                    return LetBang (p', e1', e2') }


let unionF name cases = state { let! _ = mmap (fun (s,l) -> enterScope ((sprintf "%s.%s" name s), l)) cases
                                return DisUnion(name, cases) }

let enumF name cases = state { let! _ = mmap (fun (x, c) -> enterScope x) cases
                               return Enum(name, cases) }

let matchF e cs = state { let! e' = e
                          let! cs' = mmap (fun c -> state { return! c }) cs
                          return Match(e', cs')}

let clauseF p e = state { let! pAcc = p
                          let vars = flatPat pAcc
                          let! _ = mmap (fun x -> execute enterScope x) vars 
                          let! e' = e
                          let! _ = mmap (fun x -> execute exitScope x) vars 
                          return Clause(pAcc, e') }

let forEachF p e1 e2 = state{   let! pAcc = p
                                let flatpat = flatPat pAcc
                                let boundName, args = if (List.isEmpty flatpat) then pAcc, [] else List.head flatpat, List.tail flatpat            
                                match (boundName, args) with
                                | PLit(x), _ ->
                                    let! e1' = e1       
                                    let! e2' = e2
                                    return ForEach (PLit(x), e1', e2') 
                                | PWild, _ ->
                                    let! e1' = e1       
                                    let! e2' = e2
                                    return ForEach (PWild, e1', e2') 
                                | PRecord xs, vars ->
                                    let! p'  = p
                                    let! _ = mmap (fun x -> execute enterScope x) vars // ----------------------------------------------------
                                    let! e1' = e1                                      // function variables scope (like the x in let f x = x)                                  
                                    let! _ = mmap (fun x -> execute exitScope x) vars  // ----------------------------------------------------
                                    let! _ = mmap (fun (_,p) -> execute enterScope p) xs
                                    let! e2' = e2               // function name scope (like the f in let f x = x)  
                                    if (e2' <> (Lit Unit)) then
                                            let! _ = mmap (fun (_,p) -> execute exitScope p) xs
                                            do ()
                                    return ForEach (p', e1', e2')                                                                     
                                | PVar (s,l), [] ->                                                 
                                    let! p'  = p                                                 
                                    let! e1' = e1       
                                    do! enterScope (s,l)    // ----------------------------------------------------                                               
                                    let! e2' = e2           // let x = x in x Only "let x" and "in x" refer to the same identifier.
                                    if (e2' <> (Lit Unit)) then
                                        do! exitScope (s,l)  // ----------------------------------------------------  
                                    return ForEach (p', e1', e2') 
                                | PVar (sf,lf), vars ->                                                 
                                    let! p'  = p
                                    let! _ = mmap (fun x -> execute enterScope x) vars // ----------------------------------------------------
                                    let! e1' = e1                                      // function variables scope (like the x in let f x = x)                                  
                                    let! _ = mmap (fun x -> execute exitScope x) vars  // ----------------------------------------------------
                                    do! enterScope (sf, lf)     // ----------------------------------------------------                                           
                                    let! e2' = e2               // function name scope (like the f in let f x = x)  
                                    if (e2' <> (Lit Unit)) then
                                        do! exitScope (sf, lf)      // ----------------------------------------------------                               
                                    return ForEach (p', e1', e2') }

let forF var startExp endExp bodyExp = 
                        state{ let! varAcc = var
                               let! startExpAcc = startExp
                               let! endExpAcc = endExp
                               let! _ = execute enterScope varAcc                                                             
                               let! bodyExpAcc = bodyExp
                               let! _ = execute exitScope varAcc 
                               return For(varAcc, startExpAcc, endExpAcc, bodyExpAcc) }

let moduleF n ms = state { let! ms' = mmap (fun m -> state { return! m }) ms
                           return NestedModule (n, ms') }

let hashdirectiveF s ss = state { return HashDirective (s, ss) }

let moduleAbbrevF s ss = state { return ModuleAbbrev (s, ss) }

let exceptionDefF n ms = state { let! msAcc = mmap (fun m -> state { return! m }) ms
                                 return ExceptionDef (n, msAcc) }

let ifThenElseF e1 e2 e3 = state { let! e1' = e1
                                   let! e2' = e2
                                   match e3 with
                                   | Some x ->
                                       let! x' = x
                                       return IfThenElse(e1', e2', Some x')
                                   | Option.None ->
                                       return IfThenElse(e1', e2', Option.None) }

let recordDefF name fields ms = state { let! msAcc = mmap (fun m -> state { return! m }) ms
                                        return Record(name, fields, msAcc) }

let classF n ms = state {  let! ic = mmap (fun x -> state { let! xAcc = x
                                                            match xAcc with | ImplicitCtor ps -> return ps | _ -> return [] }) ms
                           let! _ = mmap (fun ps -> mmap (fun x -> execute enterScope x) ps) ic
                           let! msAcc = mmap (fun m -> state { return! m }) (if ic.IsEmpty then ms else ms.Tail)
                           let! _ = mmap (fun ps -> mmap (fun x -> execute exitScope x) ps) ic
                           return Class (n, msAcc) }

let valfieldF t1 t2 = state { let! t1Acc = match t1 with
                                           | Some t -> state { let! x = t
                                                               return Some x }
                                           | Option.None -> state { return Option.None }
                              let! t2Acc = t2
                              return ValField (t1Acc, t2Acc) }

let inheritF t1 t2 = state { let! t1Acc = t1
                             let! t2Acc = match t2 with
                                          | Some t -> state { let! x = t
                                                              return Some x }
                                          | Option.None -> state { return Option.None }
                             return Inherit (t1Acc, t2Acc) }

let implicitInheritF t e id = state { let! tAcc = t
                                      let! eAcc = e
                                      let! idAcc = match id with
                                                   | Some t -> state { let! x = t
                                                                       return Some x }
                                                   | Option.None -> state { return Option.None }
                                      return ImplicitInherit (tAcc, eAcc, idAcc) }

let memberF isInstance p e = 
                    state { let! pAcc = p
                            let flatpat = flatPat pAcc                                                                   
                            let boundName = if (isInstance) then 
                                                List.head (let xs = List.tail flatpat
                                                           if (List.isEmpty xs) then flatpat else xs)
                                            else
                                                List.head flatpat
                                                                                    

                            let args = if (isInstance) then
                                        (List.head flatpat) :: (let xs = List.tail flatpat
                                                                if (List.isEmpty xs) then xs else List.tail xs)
                                        else
                                            List.tail flatpat               

                            let! _ = mmap (fun x -> execute enterScope x) args
                            let! eAcc = e
                            let! _ = mmap (fun x -> execute exitScope x) args
                            return Member(isInstance, pAcc, eAcc) }


let interfaceF t msOption = state { let! (tAcc:Type<_>) = t
                                    let! msAcc = match msOption with
                                                 | Some ms -> state { let! msAcc = mmapId ms
                                                                      return Some msAcc }
                                                 | Option.None -> state { return Option.None }
                                    return Interface (tAcc, msAcc) }


let pRecordF es = state { let! esAcc = mmap (fun (i, e) -> state { let! eAcc = e
                                                                   return i, eAcc }) es
                          return PRecord esAcc }


let pLongVarF xs = state {  let! xsAcc = mmapId xs                                                                 
                            let ls = xsAcc |> List.map (fun (PVar(_,l')) -> l')
                                        |> List.rev
                                        |> List.toSeq
                                        |> Seq.take 1
                                        |> Seq.toList
                            let s = xsAcc  |> List.map (fun (PVar(s,_)) -> s)
                                        |> fun xs -> System.String.Join(".", xs)   
                            let! _ = mmap (fun l -> state { do! addUsage (s,l) }) ls    
                            return PLongVar xsAcc }

let parenF e = state { let! eAcc = e
                       return eAcc }

                    
let buildSymbolTable'' exp : State<(OpenScopes * SymbolTable), Ast.Module<'a>> = 
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
                         parenF = parenF
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
                     
                                                       

// Exp<'a> list -> State<(OpenScopes * SymbolTable), Exp<'a> list>
let rec buildSymbolTable' progs = 
    state { let! ret = mmap (fun prog -> buildSymbolTable'' prog) progs
            do! exitGlobalScope
            return ret }
               
// Exp<'a> list -> SymbolTable
let buildSymbolTable progs = 
    let openScopes = OpenScopes(Map.empty)
    let symbolTable = SymbolTable(Map.empty)
    let (scopes, table) = executeGetState (buildSymbolTable' progs) (openScopes, symbolTable)
    table

// SymbolTable -> Exp<string * SrcLoc>
let getAllReferences (SymbolTable(table)) pos = 
        let data = Map.toList table
        seq [ for (s,xxs) in data do
              for xs in xxs do
              for x in xs do
                 if (x = pos) then
                    yield List.map (fun x -> Var(s, x)) xs ] |> Seq.concat 
                                                             |> Seq.distinct 
                                                             |> fun xs -> let xs' = Seq.filter (fun (Var (s : string, x)) -> s.Contains(".")) xs
                                                                          if (Seq.isEmpty xs') then xs else xs'
                                                             |> Seq.toList
    
       
// Exp<'a> list -> SrcLoc -> Exp<string * SrcLoc> list                
let findAllReferences pos progs = 
        let symbolTable = buildSymbolTable progs
        getAllReferences symbolTable pos 