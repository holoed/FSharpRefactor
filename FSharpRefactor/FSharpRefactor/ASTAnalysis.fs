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
open PurelyFunctionalDataStructures
open ASTScopeOperations
open ASTPatUtils

let memberSigF t = liftM Ast.MemberSig t

let traitCallF ss msig e = liftM2 (fun msigAcc eAcc -> Ast.TraitCall(ss, msigAcc, eAcc)) msig e
                           
let typeTestF e t = state { let! eAcc = e
                            let! tAcc = t
                            return Ast.TypeTest (eAcc, tAcc) }

let measureVarF s = state { return Ast.MVar s }

let measureOneF _ = state { return Ast.One }

let measureAnonF _ = state { return Ast.Anon }

let measureDivideF m1 m2 = state { let! m1Acc = m1
                                   let! m2Acc = m2
                                   return Ast.Divide (m1Acc, m2Acc) }

let powerF m n = state { let! mAcc = m
                        return Ast.Power (mAcc, n) }

let measureF e m = state { let! eAcc = e
                           let! mAcc = m
                           return Ast.Measure(eAcc, mAcc) }

let measureSeqF ms = state { let! msAcc = mmapId ms
                             return Ast.Seq msAcc }

let measureNamedF e = state { let! eAcc = e
                              return Ast.Named eAcc }

let quoteF e1 e2 = state { let! e1Acc = e1
                           let! e2Acc = e2
                           return Quote (e1Acc, e2Acc) }

let inferredDowncastF e = state { let! eAcc = e
                                  return InferredDowncast eAcc }

let inferredUpcastF e = state { let! eAcc = e
                                return InferredUpcast eAcc }  
                                
let lazyF e = state { let! eAcc = e
                      return Lazy eAcc }

let whileF e1 e2 = state { let! e1Acc = e1
                           let! e2Acc = e2
                           return While (e1Acc, e2Acc) }

let assertF e = state { let! eAcc = e
                        return Assert eAcc }

let nullF () = state { return Null }

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

let longVarSetF e1 e2 = state { let! e1Acc = e1
                                let! e2Acc = e2
                                return LongVarSet (e1Acc, e2Acc) }

let lamF ps e = state { let! vars = mmap (fun p -> state { return! p }) ps
                        let! _ = mmap (fun x -> execute enterScope x) vars
                        let! e' = e
                        let! _ = mmap (fun x -> execute exitScope x) vars 
                        return Lam (vars, e') }

let appF x y = state { let! x' = x
                       let! y' = y
                       return App (x', y') }

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

let litF x = state { return Lit x }

let tupleF es = state { let! es' = mmapId es
                        return Tuple es' }

let listF es = state { let! es' = mmapId es
                       return List es' }

let expF es = state { let! es' = mmapId es
                      return Exp es' }

let typesF xs = state { let! xs' = mmapId xs
                        return Types xs' }

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

let yieldOrRetF e = state {  let! e' = e
                             return YieldOrReturn e' }

let yieldOrRetFromF e = state {  let! e' = e
                                 return YieldOrReturnFrom e' }

let moduleF n ms = state { let! ms' = mmap (fun m -> state { return! m }) ms
                           return NestedModule (n, ms') }

let openF s = state { return Open s }

let exceptionF ex = state { let! ex' = ex
                            return Exception ex' }

let hashdirectiveF s ss = state { return HashDirective (s, ss) }

let moduleAbbrevF s ss = state { return ModuleAbbrev (s, ss) }

let attributesF xs = state { let! xs' = mmapId xs
                             return Attributes xs' }

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

let dotGetF e li = state { let! e' = e
                           let! li' = li
                           return DotGet (e', li') }

let dotSetF e1 li e2 = state { let! e1Acc = e1
                               let! liAcc = li
                               let! e2Acc = e2
                               return DotSet(e1Acc, liAcc, e2Acc) }

let dotIndexedSetF e1 es e3 = state { let! e1' = e1
                                      let! es' = mmapId es
                                      let! e3' = e3
                                      return DotIndexedSet (e1', es', e3') }

let dotIndexedGetF e1 es = state { let! e1' = e1
                                   let! es' = mmapId es
                                   return DotIndexedGet (e1', es') }

let recordDefF name fields ms = state { let! msAcc = mmap (fun m -> state { return! m }) ms
                                        return Record(name, fields, msAcc) }

let recordInstF fields = state { let! fields' = mmapId fields
                                 return Exp.Record fields'}

let recordFieldInstF n e = state { let! eAcc = e
                                   return (n, eAcc) }

let newF ss e = state { let! ssAcc = ss
                        let! eAcc = e
                        return New (ssAcc, eAcc) }

let typeappF e ts = state { let! eAcc= e
                            let! tsAcc = mmapId ts
                            return TypeApp (eAcc, tsAcc) }

let noneF name = state { return None name }

let classF n ms = state {  let! ic = mmap (fun x -> state { let! xAcc = x
                                                            match xAcc with | ImplicitCtor ps -> return ps | _ -> return [] }) ms
                           let! _ = mmap (fun ps -> mmap (fun x -> execute enterScope x) ps) ic
                           let! msAcc = mmap (fun m -> state { return! m }) (if ic.IsEmpty then ms else ms.Tail)
                           let! _ = mmap (fun ps -> mmap (fun x -> execute exitScope x) ps) ic
                           return Class (n, msAcc) }

let implicitConF ps = state { let! psAcc = mmapId ps
                              return ImplicitCtor psAcc }

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

let abstractSlotF n = state { return AbstractSlot n }

let objExprF ms = state { let! msAcc = mmapId ms
                          return ObjExpr msAcc }

let doF e = state { let! eAcc = e
                    return Do eAcc }

let addressofF e = state { let! eAcc = e
                           return AddressOf eAcc }

let doBangF e = state { let! eAcc = e
                        return DoBang eAcc }

let downcastF e t = state { let! eAcc = e
                            let! (tAcc:Type<_>) = t
                            return Downcast (eAcc, tAcc) }

let upcastF e t = state { let! eAcc = e
                          let! (tAcc:Type<_>) = t
                          return Upcast (eAcc, tAcc) }

let typedF e t = state { let! eAcc = e
                         let! tAcc = t
                         return Typed (eAcc, tAcc) }

let interfaceF t msOption = state { let! (tAcc:Type<_>) = t
                                    let! msAcc = match msOption with
                                                 | Some ms -> state { let! msAcc = mmapId ms
                                                                      return Some msAcc }
                                                 | Option.None -> state { return Option.None }
                                    return Interface (tAcc, msAcc) }

let letBindingsF es = state { let! esAcc = mmapId es
                              return LetBindings esAcc }

let abbrevF n t = state { let! (tAcc:Type<_>) = t
                          return Abbrev (n, tAcc) }

let tfunF t1 t2 = state { let! (t1Acc:Type<_>) = t1
                          let! (t2Acc:Type<_>) = t2
                          return TFun (t1Acc, t2Acc) }

let tIdentF s = state { return Ident s }

let tLongIdentF ts = state { let! tsAcc = mmapId ts
                             return LongIdent tsAcc } 

let tvarF t = state { let! tAcc = t
                      return TVar tAcc }

let tappF t ts = state { let! tAcc = t
                         let! tsAcc = mmapId ts
                         return TApp (tAcc, tsAcc) }

let ttupleF ts = state { let! tsAcc = mmapId ts
                         return TTuple tsAcc }

let tarrayF n t = state { let! tAcc = t
                          return TArray (n, tAcc) }

let tmeasurePowerF t n = state { let! tAcc = t
                                 return TMeasurePower (tAcc, n) }

let tanonF () = state { return TAnon }

let tmeasureOneF () = state { return TMeasureOne }

let tryWithF e cs = state { let! e' = e
                            let! cs' = mmapId cs
                            return TryWith(e', cs')}

let tryFinallyF e1 e2 = state { let! e1Acc = e1
                                let! e2Acc = e2
                                return TryFinally (e1Acc, e2Acc) }

let errorF () = state { return ArbitraryAfterError }

let pVarF x = state { return PVar x }

let pAppF l r = state { let! l' = l
                        let! r' = r
                        return PApp (l', r') } 

let porF p1 p2 = state { let! p1Acc = p1
                         let! p2Acc = p2
                         return POr (p1Acc, p2Acc) }

let pandsF ps = state { let! psAcc = mmapId ps
                        return Ast.PAnds psAcc }

let pLitF x = state { return PLit x }

let pTupleF es = state { let! es' = mmapId es
                         return PTuple es' }

let pRecordF es = state { let! esAcc = mmap (fun (i, e) -> state { let! eAcc = e
                                                                   return i, eAcc }) es
                          return PRecord esAcc }

let pWildF () = state { return PWild } 

let pArrayOrListF es = state { let! es' = mmapId es
                               return PList es' }

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

let pIsInstF t = state { let! tAcc= t
                         return PIsInst tAcc }

let pnullF () = state { return PNull }

let pattributeF p attrs = state { let! pAcc = p
                                  let! attrsAcc = mmapId attrs
                                  return PAttribute(pAcc, attrsAcc) }

let attributeF e = state { let! eAcc = e
                           return Attribute eAcc }

let pnamedF p1 p2 = state { let! p1Acc = p1
                            let! p2Acc = p2
                            return PNamed(p1Acc, p2Acc) }

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