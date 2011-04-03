module Algebras

open Ast
open StateMonad

type AstAlgebra<'a,'b,'c,'d,'e,'f,'g,'h,'i,'j,'k, 'l> =
    {
        assertF : 'b -> 'b
        lazyF : 'b -> 'b
        inferredDowncastF : 'b -> 'b
        whileF : 'b -> 'b -> 'b
        nullF : Unit -> 'b
        varF : 'a -> 'b
        longVarF : 'b list -> 'b
        longVarSetF : 'b -> 'b -> 'b
        lamF : 'j list -> 'b -> 'b
        appF : 'b -> 'b -> 'b
        letF : IsLetRec -> ('j * 'b) list -> 'b -> 'b
        letBangF : 'j -> 'b -> 'b -> 'b
        litF : Literal -> 'b
        tupleF : 'b list -> 'b
        listF : 'b list -> 'b
        expF : 'b list -> 'c
        typesF : 'd list -> 'e
        unionF : string -> 'a list -> 'd
        enumF : string -> ('a * Literal) list -> 'd
        matchF : 'b -> 'f list -> 'b
        addressofF : 'b -> 'b
        clauseF : 'j -> 'b -> 'f
        forEachF : 'j -> 'b -> 'b -> 'b
        forF : 'j -> 'b -> 'b -> 'b -> 'b
        yieldOrRetF : 'b -> 'b
        yieldOrRetFromF : 'b -> 'b
        moduleF : string list -> 'e list -> 'e
        openF : string list -> 'e
        exceptionF : 'k -> 'e
        hashdirectiveF : string -> string list -> 'e
        moduleAbbrevF : string -> string list -> 'e
        attributesF : 'l list -> 'e
        exceptionDefF : string -> 'g list -> 'k
        ifThenElseF : 'b -> 'b -> 'b option -> 'b
        dotGetF : 'b -> 'b -> 'b
        dotIndexedSetF : 'b -> 'b list -> 'b -> 'b
        dotIndexedGetF : 'b -> 'b list -> 'b
        recordDefF : string -> 'a option list -> 'g list -> 'd
        recordInstF : 'h list -> 'b
        recordFieldInstF : 'a -> 'b -> 'h
        newF : 'i -> 'b -> 'b
        typeappF : 'b -> 'i list -> 'b
        noneF : string -> 'd
        classF : string -> 'g list -> 'd
        valfieldF: 'i option -> 'i -> 'g
        inheritF: 'i -> 'i option -> 'g
        implicitInheritF :'i -> 'b -> 'i option -> 'g
        implicitConF : 'j list -> 'g
        memberF : 'j -> 'b -> 'g
        abstractSlotF : string -> 'g
        objExprF : 'g list -> 'b
        doF : 'b -> 'b
        doBangF : 'b -> 'b
        downcastF : 'b -> 'i -> 'b
        upcastF : 'b -> 'i -> 'b
        typedF : 'b -> 'i -> 'b
        interfaceF : 'i -> 'g list -> 'g
        letBindingsF : 'b list -> 'g
        abbrevF : string -> 'i -> 'd
        tfunF : 'i -> 'i -> 'i
        tIdentF : 'a -> 'i
        tLongIdentF : 'i list -> 'i
        tvarF : 'i -> 'i
        tappF : 'i -> 'i list -> 'i
        ttupleF : 'i list -> 'i
        tarrayF : int -> 'i -> 'i
        tanonF : Unit -> 'i
        tryWithF : 'b -> 'f list -> 'b
        errorF : unit -> 'b 
        pVarF : 'a -> 'j
        pAppF : 'j -> 'j -> 'j
        porF : 'j -> 'j -> 'j
        pLitF : Literal -> 'j
        pTupleF : 'j list -> 'j
        pRecordF : (string * 'j) list -> 'j
        pWildF : unit -> 'j
        pArrayOrListF : 'j list -> 'j
        pLongVarF : 'j list -> 'j
        pIsInstF : 'i -> 'j     
        pnullF : Unit -> 'j 
        attributeF : 'b -> 'l
        pattributeF : 'j -> 'l list -> 'j  
    }