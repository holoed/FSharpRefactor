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

let memberSigF = liftM Ast.MemberSig

let traitCallF ss = liftM2 ((curry3 Ast.TraitCall) ss) 
                           
let typeTestF = liftM2 (curry2 Ast.TypeTest)

let measureVarF s = state.Return (Ast.MVar s)

let measureOneF _ = state.Return Ast.One 

let measureAnonF _ = state.Return Ast.Anon 

let measureDivideF = liftM2 (curry2 Ast.Divide)
                           
let powerF m n = liftM2 (curry2 Ast.Power) m (state.Return n)
              
let measureF = liftM2 (curry2 Ast.Measure)
             
let measureSeqF ms = liftM Ast.Seq (mmapId ms) 
                    
let measureNamedF = liftM Ast.Named                        

let quoteF = liftM2 (curry2 Ast.Quote)
                    
let inferredDowncastF = liftM Ast.InferredDowncast                            

let inferredUpcastF = liftM Ast.InferredUpcast 
                                
let lazyF = liftM Ast.Lazy 
                
let whileF = liftM2 (curry2 Ast.While)

let assertF = liftM Ast.Assert

let nullF () = state.Return Null 

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

let longVarSetF = liftM2 (curry2 Ast.LongVarSet)


let lamF ps e = state { let! vars = mmap (fun p -> state { return! p }) ps
                        let! _ = mmap (fun x -> execute enterScope x) vars
                        let! e' = e
                        let! _ = mmap (fun x -> execute exitScope x) vars 
                        return Lam (vars, e') }

let appF = liftM2 (curry2 Ast.App)
                

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

let litF x = state.Return (Lit x) 

let tupleF es = liftM Ast.Tuple (mmapId es)
                
let listF es = liftM Ast.List (mmapId es)
                
let expF es = liftM Ast.Exp (mmapId es)

let typesF xs = liftM Ast.Types (mmapId xs)
                
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

let yieldOrRetF = liftM Ast.YieldOrReturn                

let yieldOrRetFromF = liftM Ast.YieldOrReturnFrom

let moduleF n ms = state { let! ms' = mmap (fun m -> state { return! m }) ms
                           return NestedModule (n, ms') }

let openF s = state.Return (Open s)

let exceptionF = liftM Ast.Exception
                    
let hashdirectiveF s ss = state { return HashDirective (s, ss) }

let moduleAbbrevF s ss = state { return ModuleAbbrev (s, ss) }

let attributesF xs = liftM Ast.Attributes (mmapId xs)                        

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

let dotGetF = liftM2 (curry2 Ast.DotGet)

let dotSetF = liftM3 (curry3 Ast.DotSet)                        

let dotIndexedSetF e1 es e3 = liftM3 (curry3 Ast.DotIndexedSet) e1 (mmapId es) e3
                              
let dotIndexedGetF e1 es = liftM2 (curry2 Ast.DotIndexedGet) e1 (mmapId es)

let recordDefF name fields ms = state { let! msAcc = mmap (fun m -> state { return! m }) ms
                                        return Record(name, fields, msAcc) }

let recordInstF fields = liftM Exp.Record (mmapId fields)                         

let recordFieldInstF n = liftM (fun eAcc -> (n, eAcc))                             

let newF = liftM2 (curry2 Ast.New)
                
let typeappF e ts = liftM2 (curry2 Ast.TypeApp) e (mmapId ts)
                    
let noneF name = state.Return (None name)

let classF n ms = state {  let! ic = mmap (fun x -> state { let! xAcc = x
                                                            match xAcc with | ImplicitCtor ps -> return ps | _ -> return [] }) ms
                           let! _ = mmap (fun ps -> mmap (fun x -> execute enterScope x) ps) ic
                           let! msAcc = mmap (fun m -> state { return! m }) (if ic.IsEmpty then ms else ms.Tail)
                           let! _ = mmap (fun ps -> mmap (fun x -> execute exitScope x) ps) ic
                           return Class (n, msAcc) }

let implicitConF ps = liftM Ast.ImplicitCtor (mmapId ps)                        

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

let abstractSlotF n = state.Return (AbstractSlot n)

let objExprF ms = liftM Ast.ObjExpr (mmapId ms)                    

let doF = liftM Ast.Do 
            
let addressofF = liftM Ast.AddressOf

let doBangF = liftM Ast.DoBang

let downcastF = liftM2 (curry2 Ast.Downcast)
                    
let upcastF = liftM2 (curry2 Ast.Upcast)
                  
let typedF = liftM2 (curry2 Ast.Typed)


let interfaceF t msOption = state { let! (tAcc:Type<_>) = t
                                    let! msAcc = match msOption with
                                                 | Some ms -> state { let! msAcc = mmapId ms
                                                                      return Some msAcc }
                                                 | Option.None -> state { return Option.None }
                                    return Interface (tAcc, msAcc) }

let letBindingsF es = liftM Ast.LetBindings (mmapId es)

let abbrevF n = liftM2 (curry2 Ast.Abbrev) (state.Return n)

let tfunF = liftM2 (curry2 Ast.TFun)

let tIdentF s = state.Return (Ident s)

let tLongIdentF ts = liftM Ast.LongIdent (mmapId ts)                     

let tvarF = liftM Ast.TVar
                
let tappF t ts = liftM2 (curry2 Ast.TApp) t (mmapId ts)
                 
let ttupleF ts = liftM Ast.TTuple (mmapId ts)
                    
let tarrayF n = liftM2 (curry2 Ast.TArray) (state.Return n)

let tmeasurePowerF t n = liftM2 (curry2 Ast.TMeasurePower) t (state.Return n)

let tanonF () = state.Return TAnon 

let tmeasureOneF () = state.Return TMeasureOne 

let tryWithF e cs = liftM2 (curry2 Ast.TryWith) e (mmapId cs)

let tryFinallyF = liftM2 (curry2 Ast.TryFinally)                            

let errorF () = state.Return ArbitraryAfterError 

let pVarF x = state.Return (PVar x)

let pAppF = liftM2 (curry2 Ast.PApp)
                
let porF = liftM2 (curry2 Ast.POr)
                 
let pandsF ps = liftM Ast.PAnds (mmapId ps)                

let pLitF x = state.Return (PLit x)

let pTupleF es = liftM Ast.PTuple (mmapId es)                    

let pRecordF es = state { let! esAcc = mmap (fun (i, e) -> state { let! eAcc = e
                                                                   return i, eAcc }) es
                          return PRecord esAcc }

let pWildF () = state.Return PWild 

let pArrayOrListF es = liftM Ast.PList (mmapId es)

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

let pIsInstF = liftM Ast.PIsInst              

let pnullF () = state.Return PNull 

let pattributeF p attrs = liftM2 (curry2 Ast.PAttribute) p (mmapId attrs)

let attributeF = liftM Ast.Attribute
                    
let pnamedF = liftM2 (curry2 Ast.PNamed) 
                    
let buildSymbolTable'' exp : State<(OpenScopes * SymbolTable), Ast.Module<'a>> = 
        foldExpAlgebra { memberSigF = memberSigF
                         traitCallF = traitCallF
                         typetestF = typeTestF
                         measureVarF = measureVarF
                         measureOneF = measureOneF
                         measureAnonF =   measureAnonF
                         measureDivideF = measureDivideF
                         powerF = powerF
                         measureF = measureF
                         measureSeqF = measureSeqF
                         measureNamedF = measureNamedF
                         quoteF = quoteF
                         inferredDowncastF = inferredDowncastF
                         inferredUpcastF   = inferredUpcastF
                         lazyF = lazyF
                         whileF = whileF
                         assertF = assertF
                         nullF = nullF
                         varF = varF
                         longVarSetF = longVarSetF
                         lamF = lamF
                         appF = appF
                         letF = letF
                         letBangF = letBangF
                         litF = litF
                         tupleF = tupleF
                         listF = listF
                         expF = expF
                         typesF = typesF
                         unionF = unionF     
                         enumF = enumF      
                         matchF = matchF
                         clauseF = clauseF
                         forEachF = forEachF
                         forF = forF
                         yieldOrRetF = yieldOrRetF
                         yieldOrRetFromF = yieldOrRetFromF
                         moduleF = moduleF
                         openF = openF
                         exceptionF = exceptionF
                         hashdirectiveF = hashdirectiveF
                         moduleAbbrevF = moduleAbbrevF
                         attributesF = attributesF
                         exceptionDefF = exceptionDefF
                         ifThenElseF = ifThenElseF
                         dotGetF = dotGetF
                         dotSetF = dotSetF
                         dotIndexedSetF = dotIndexedSetF
                         dotIndexedGetF =  dotIndexedGetF
                         recordDefF = recordDefF
                         recordInstF = recordInstF
                         recordFieldInstF = recordFieldInstF
                         newF = newF
                         typeappF = typeappF
                         noneF = noneF
                         classF = classF
                         implicitConF = implicitConF
                         valfieldF = valfieldF
                         inheritF  = inheritF
                         implicitInheritF = implicitInheritF
                         memberF = memberF
                         abstractSlotF = abstractSlotF
                         objExprF = objExprF
                         doF = doF
                         addressofF = addressofF
                         doBangF = doBangF
                         downcastF = downcastF
                         upcastF = upcastF
                         typedF = typedF
                         interfaceF = interfaceF
                         letBindingsF =  letBindingsF
                         abbrevF =  abbrevF
                         tfunF = tfunF
                         tIdentF = tIdentF
                         tLongIdentF = tLongIdentF
                         tvarF =  tvarF
                         tappF = tappF
                         ttupleF = ttupleF
                         tarrayF = tarrayF
                         tmeasurePowerF = tmeasurePowerF
                         tanonF = tanonF
                         tmeasureOneF = tmeasureOneF
                         tryWithF = tryWithF
                         tryFinallyF = tryFinallyF
                         errorF = errorF                         
                         pVarF = pVarF
                         pAppF = pAppF
                         porF = porF
                         pandsF = pandsF                                        
                         pLitF = pLitF
                         pTupleF = pTupleF
                         pRecordF = pRecordF
                         pWildF = pWildF
                         pArrayOrListF = pArrayOrListF
                         pLongVarF = pLongVarF
                         pIsInstF  = pIsInstF
                         pnullF = pnullF
                         pattributeF = pattributeF
                         attributeF = attributeF
                         pnamedF = pnamedF }   exp
                     
                                                       

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