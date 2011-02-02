module Algebras

open Ast
open StateMonad

type AstAlgebra<'a,'b,'c,'d,'e,'f,'g,'h,'i> =
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
        typesF : 'd list -> 'e
        unionF : string -> 'a list -> 'd
        matchF : 'b -> 'f list -> 'b
        clauseF : Pat<'a> -> 'b -> 'f
        forEachF : Pat<'a> -> 'b -> 'b -> 'b
        yieldOrRetF : 'b -> 'b
        yieldOrRetFromF : 'b -> 'b
        moduleF : string list -> 'e list -> 'e
        openF : string list -> 'e
        ifThenElseF : 'b -> 'b -> 'b option -> 'b
        dotIndexedSetF : 'b -> 'b list -> 'b -> 'b
        dotIndexedGetF : 'b -> 'b list -> 'b
        recordDefF : string -> 'a option list -> 'g list -> 'd
        recordInstF : 'h list -> 'b
        recordFieldInstF : 'a -> 'b -> 'h
        newF : 'i -> 'b -> 'b
        noneF : string -> 'd
        classF : string -> 'g list -> 'd
        implicitConF : Pat<'a> list -> 'g
        memberF : Pat<'a> -> 'b -> 'g
        abstractSlotF : string -> 'g
        objExprF : 'g list -> 'b
        doF : 'b -> 'b
        downcastF : 'b -> 'i -> 'b
        upcastF : 'b -> 'i -> 'b
        interfaceF : 'i -> 'g list -> 'g
        letBindingsF : 'b list -> 'g
        abbrevF : string -> 'i -> 'd
        tfunF : 'i -> 'i -> 'i
        tIdentF : 'a -> 'i
        tLongIdentF : 'i list -> 'i
        tvarF : 'i -> 'i
        errorF : unit -> 'b 
    }