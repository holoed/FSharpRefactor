module AstAlgebras

open Ast
open StateMonad

type ExpStateAlgebra<'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k, 'l, 'm> =
    {
        varF : 'a -> State<'b, 'c>
        longVarF : State<'b, 'c> list -> State<'b, 'c>
        longVarSetF : State<'b, 'c> -> State<'b, 'c> -> State<'b, 'c>
        lamF : Pat<'a> list -> State<'b, 'c> -> State<'b, 'c>
        appF : State<'b, 'c> -> State<'b, 'c> -> State<'b, 'c>
        letF : IsLetRec -> Pat<'a> -> State<'b, 'c> -> State<'b, 'c> -> State<'b, 'c>
        litF : Literal -> State<'b, 'c>
        tupleF : State<'b, 'c> list -> State<'b, 'c>
        listF : State<'b, 'c> list -> State<'b, 'c>
        expF : State<'b, 'c> list -> State<'d, 'e>
        typesF : State<'f, 'g> list -> State<'d, 'e>
        unionF : string -> 'a list -> State<'f, 'g>
        matchF: State<'b, 'c> -> State<'h, 'i> list -> State<'b, 'c>
        clauseF : Pat<'a> -> State<'b, 'c> -> State<'h, 'i>
        forEachF : Pat<'a> -> State<'b, 'c> -> State<'b, 'c> -> State<'b, 'c>
        yieldOrRetF : State<'b, 'c> -> State<'b, 'c>
        moduleF : string list -> State<'d, 'e> list -> State<'d, 'e>
        openF : string list -> State<'d, 'e>
        ifThenElseF: State<'b, 'c> -> State<'b, 'c> -> State<'b, 'c> option -> State<'b, 'c>
        dotIndexedSetF : State<'b, 'c> -> State<'b, 'c> list -> State<'b, 'c> -> State<'b, 'c>
        dotIndexedGetF : State<'b, 'c> -> State<'b, 'c> list -> State<'b, 'c>
        recordDefF : string -> 'a option list -> State<'j, 'k> list -> State<'f, 'g>
        recordInstF : State<'l, 'm> list -> State<'b, 'c>
        recordFieldInstF : 'a -> State<'b, 'c> -> State<'l, 'm>
        newF : 'a list -> State<'b, 'c> -> State<'b, 'c>
        noneF : string -> State<'f, 'g>
        classF : string -> State<'j, 'k> list -> State<'f, 'g>
        implicitConF : Pat<'a> list -> State<'j, 'k>
        memberF : Pat<'a> -> State<'b, 'c> -> State<'j, 'k>
        abstractSlotF : string -> State<'j, 'k>
        objExprF : State<'j, 'k> list -> State<'b, 'c>
        doF : State<'b, 'c> -> State<'b, 'c>
        downcastF : State<'b, 'c> -> 'a list -> State<'b, 'c>
        upcastF : State<'b, 'c> -> 'a list -> State<'b, 'c>
        interfaceF : 'a list -> State<'j, 'k> list -> State<'j, 'k>
        letBindingsF : State<'b, 'c> list -> State<'j, 'k>
        errorF : unit -> State<'b, 'c>
    }

type ExpAlgebra<'a, 'b, 'c, 'd, 'e, 'f, 'g> =
    {
        varF : 'a -> 'b
        longVarF : 'b list -> 'b
        longVarSetF : 'b -> 'b -> 'b
        lamF : Pat<'a> list -> 'b -> 'b
        appF : 'b -> 'b -> 'b
        letF : IsLetRec -> Pat<'a> -> 'b -> 'b -> 'b
        litF : Literal -> 'b
        tupleF : 'b list -> 'b
        listF : 'b list -> 'b
        expF : 'b list -> 'c
        typesF : 'd list -> 'c
        unionF : string -> 'a list -> 'd
        matchF: 'b -> 'e list -> 'b
        clauseF : Pat<'a> -> 'b -> 'e
        forEachF : Pat<'a> -> 'b -> 'b -> 'b
        yieldOrRetF : 'b -> 'b
        moduleF : string list -> 'c list -> 'c
        openF : string list -> 'c
        ifThenElseF: 'b -> 'b -> 'b option -> 'b
        dotIndexedSetF : 'b -> 'b list -> 'b -> 'b
        dotIndexedGetF : 'b -> 'b list -> 'b
        recordDefF : string -> 'a option list -> 'f list -> 'd
        recordInstF : 'g list -> 'b
        recordFieldInstF : 'a -> 'b -> 'g
        newF : 'a list -> 'b -> 'b
        noneF : string -> 'd
        classF : string -> 'f list -> 'd
        implicitConF : Pat<'a> list -> 'f
        memberF : Pat<'a> -> 'b -> 'f
        abstractSlotF : string -> 'f
        objExprF : 'f list -> 'b
        doF : 'b -> 'b
        downcastF : 'b -> 'a list -> 'b
        upcastF : 'b -> 'a list -> 'b
        interfaceF : 'a list -> 'f list -> 'f
        letBindingsF : 'b list -> 'f
        errorF : unit -> 'b
    }

let toStateMonad (algebra: ExpAlgebra<_, _, _, _, _, _, _>) :ExpStateAlgebra<_, _, _, _, _, _, _, _, _, _, _, _, _> =
        {
            varF = (fun x -> state { return algebra.varF x })
            longVarF = (fun xs -> state { let! xs' = mmapId xs
                                          return algebra.longVarF xs' })
            longVarSetF =  (fun e1 e2 -> state { let! e1Acc = e1
                                                 let! e2Acc = e2
                                                 return algebra.longVarSetF e1Acc e2Acc })
            lamF = (fun ps b -> state { let! b' = b
                                        return algebra.lamF ps b' })
            appF =  (fun x y -> state { let! x' = x
                                        let! y' = y
                                        return algebra.appF x' y' })
            letF = (fun isRec p e1 e2 -> state { let! e1' = e1
                                                 let! e2' = e2
                                                 return algebra.letF isRec p e1' e2' })
            litF = (fun x -> state { return algebra.litF x })
            tupleF = (fun es -> state { let! es' = mmapId es
                                        return algebra.tupleF es' })
            listF = (fun es -> state { let! es' = mmapId es
                                       return algebra.listF es' })
            expF = (fun es -> state {  let! es' = mmapId es
                                       return algebra.expF es' }) 
            typesF = (fun xs -> state { let! xs' = mmapId xs  
                                        return algebra.typesF xs' })
            unionF = (fun name cases -> state { return algebra.unionF name cases })
            matchF = (fun e cs -> state { let! e' = e
                                          let! cs' = mmapId cs
                                          return algebra.matchF e' cs'})
            clauseF = (fun p e -> state { let! e' = e  
                                          return algebra.clauseF p e' }) 
            forEachF = (fun p e1 e2 -> state { let! e1' = e1
                                               let! e2' = e2
                                               return algebra.forEachF p e1' e2' })
            yieldOrRetF = (fun e -> state { let! e' = e  
                                            return algebra.yieldOrRetF e' })
            moduleF = (fun n es -> state { let! es' = mmapId es
                                           return algebra.moduleF n es' })
            openF = (fun s -> state { return algebra.openF s })
            ifThenElseF = (fun e1 e2 e3 -> state { let! e1' = e1
                                                   let! e2' = e2
                                                   let! e3' = match e3 with
                                                              | Some x -> x
                                                   return algebra.ifThenElseF e1' e2' (Some e3') }) 
            dotIndexedSetF = (fun e1 es e3 -> state { let! e1' = e1
                                                      let! es' = mmapId es
                                                      let! e3' = e3
                                                      return algebra.dotIndexedSetF e1' es' e3' })
            dotIndexedGetF = (fun e1 es -> state { let! e1' = e1
                                                   let! es' = mmapId es
                                                   return algebra.dotIndexedGetF e1' es' })
            recordDefF = (fun name fields ms -> state { let! msAcc = mmapId ms
                                                        return algebra.recordDefF name fields msAcc })
            recordInstF = (fun fields -> state { let! fields' = mmapId fields
                                                 return algebra.recordInstF fields' })
            recordFieldInstF = (fun n e -> state {  let! eAcc = e
                                                    return algebra.recordFieldInstF n eAcc })
            newF = (fun s e -> state { let! eAcc = e
                                       return algebra.newF s eAcc })
            noneF = (fun name -> state { return algebra.noneF name })
            classF = (fun n ms -> state { let! msAcc = mmapId ms
                                          return algebra.classF n msAcc })
            implicitConF = (fun ps -> state { return (algebra.implicitConF ps) })
            memberF = (fun n e -> state { let! eAcc = e
                                          return algebra.memberF n eAcc })
            abstractSlotF = (fun n -> state { return algebra.abstractSlotF n })
            objExprF = (fun ms -> state { let! msAcc = mmapId ms
                                          return algebra.objExprF msAcc })
            doF = (fun e -> state { let! eAcc = e
                                    return algebra.doF eAcc })
            downcastF = (fun e t -> state { let! eAcc = e
                                            return algebra.downcastF eAcc t })
            upcastF = (fun e t -> state { let! eAcc = e
                                          return algebra.upcastF eAcc t })
            interfaceF = (fun t ms -> state { let! msAcc = mmapId ms
                                              return algebra.interfaceF t msAcc })
            letBindingsF =  (fun es -> state { let! esAcc = mmapId es
                                               return algebra.letBindingsF esAcc })
            errorF = (fun () -> state { return (algebra.errorF ()) })
        }


