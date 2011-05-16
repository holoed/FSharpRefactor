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

module TypedASTAnalysis

open Ast
open StateMonad
open AstCatamorphisms
open PurelyFunctionalDataStructures
open System.IO
open CompilerToAstTyped

let buildTypedSymbolTable (decl:Module<string * SrcLoc * int64>) : (string * SrcLoc * int64) list =             
            foldExpAlgebra {  memberSigF           =     (fun t -> [])
                              traitCallF           =     (fun ss msig e -> e)
                              typetestF            =     (fun e t -> e)
                              measureVarF          =     (fun s -> [])
                              measureOneF          =     (fun unit -> []) 
                              measureAnonF         =     (fun unit -> [])
                              measureDivideF       =     (fun m1 m2 -> [])
                              powerF               =     (fun m n -> [])
                              measureNamedF        =     (fun t -> [])
                              measureSeqF          =     (fun ms -> [])
                              measureF             =     (fun e m -> e)
                              quoteF               =     (fun e1 e2 -> List.concat [e1;e2])
                              inferredDowncastF    =     (fun e -> e)
                              inferredUpcastF      =     (fun e -> e)
                              lazyF                =     (fun e -> e)
                              whileF               =     (fun e1 e2 -> List.concat [e1;e2])
                              assertF              =     (fun e -> e)
                              nullF                =     (fun () -> [])
                              varF                 =     (fun (s, l, t) -> [(s,l,t)]) 
                              longVarF             =     (fun xs -> List.concat xs)
                              longVarSetF          =     (fun e1 e2 -> List.concat [e1;e2])
                              lamF                 =     (fun ps b -> List.concat [List.concat ps ; b]) 
                              appF                 =     (fun x y -> List.concat [x;y])
                              letF                 =     (fun isRec bs e2 -> List.concat [(List.concat (List.map (fun (x,y) -> List.concat [x; y]) bs));  e2])
                              letBangF             =     (fun p e1 e2 -> List.concat [e1;e2])
                              litF                 =     (fun x -> [])
                              tupleF               =     (fun xs -> List.concat xs)
                              listF                =     (fun xs -> List.concat xs)
                              expF                 =     (fun xs -> List.concat xs)
                              typesF               =     (fun xs -> List.concat xs)
                              unionF               =     (fun name cases -> cases)
                              enumF               =      (fun name cases -> List.map (fun ((s,l,t), c) -> (s,l,t)) cases)
                              addressofF           =     (fun e -> e)
                              matchF               =     (fun e cs -> e)
                              clauseF              =     (fun p e -> e)
                              forEachF             =     (fun p e1 e2 -> List.concat [e1;e2])
                              forF                 =     (fun i e1 e2 e3 -> [])
                              yieldOrRetF          =     (fun e -> [])
                              yieldOrRetFromF      =     (fun e -> [])
                              moduleF              =     (fun n ms -> List.concat ms)
                              openF                =     (fun s -> [])
                              exceptionF           =     (fun ex -> [])
                              hashdirectiveF       =     (fun s ss -> [])
                              moduleAbbrevF        =     (fun s ss -> [])
                              attributesF          =     (fun xs -> [])
                              exceptionDefF        =     (fun n ms -> [])
                              ifThenElseF          =     (fun e1 e2 e3 -> List.concat [e1;e2])
                              dotGetF              =     (fun e li -> e)
                              dotSetF              =     (fun e1 li e2 -> [])
                              dotIndexedSetF       =     (fun e1 es e3 -> [])
                              dotIndexedGetF       =     (fun e1 es -> [])
                              recordDefF           =     (fun name fields ms -> [])
                              recordInstF          =     (fun fields -> [])
                              recordFieldInstF     =     (fun n e -> []) 
                              newF                 =     (fun ss e -> [])
                              typeappF             =     (fun e ts -> [])
                              noneF                =     (fun name -> [])
                              classF               =     (fun n ms -> [])
                              valfieldF            =     (fun t1 t2 -> [])
                              inheritF             =     (fun t1 t2 -> [])
                              implicitInheritF     =     (fun t e id -> [])
                              implicitConF         =     (fun ps -> [])
                              memberF              =     (fun n e -> [])
                              abstractSlotF        =     (fun n -> [])
                              objExprF             =     (fun ms -> [])
                              doF                  =     (fun e -> [])
                              doBangF              =     (fun e -> [])
                              downcastF            =     (fun e t -> [])
                              upcastF              =     (fun e t -> [])
                              typedF               =     (fun e t -> [])
                              interfaceF           =     (fun t ms -> [])
                              letBindingsF         =     (fun es -> [])
                              abbrevF              =     (fun n t -> [])
                              tfunF                =     (fun t1 t2 -> [])
                              tIdentF              =     (fun (s,l,t) -> [])
                              tLongIdentF          =     (fun ts -> [])
                              tvarF                =     (fun t -> [])  
                              tappF                =     (fun t ts -> [])
                              ttupleF              =     (fun ts -> [])
                              tarrayF              =     (fun n t -> [])
                              tmeasurePowerF       =     (fun t n -> [])
                              tmeasureOneF         =     (fun () -> [])
                              tanonF               =     (fun () -> [])
                              tryWithF             =     (fun e cl -> [])   
                              tryFinallyF          =     (fun e1 e2 -> [])                        
                              errorF               =     (fun () -> []) 
                              pVarF                =     (fun (s,l,t) -> [(s,l,t)]) 
                              pAppF                =     (fun l r -> List.concat [l;r]) 
                              porF                 =     (fun p1 p2 -> [])
                              pandsF               =     (fun ps -> [])
                              pLitF                =     (fun x -> []) 
                              pTupleF              =     (fun xs -> []) 
                              pRecordF             =     (fun xs -> [])
                              pWildF               =     (fun () -> [])
                              pArrayOrListF        =     (fun xs -> [])
                              pLongVarF            =     (fun xs -> [])
                              pIsInstF             =     (fun t -> [])
                              pnullF               =     (fun () -> []) 
                              pattributeF          =     (fun p attrs -> [])
                              attributeF           =     (fun e -> [])
                              pnamedF              =     (fun p1 p2 -> [])} decl

let buildTypeSymbolTable' exps = List.map (fun exp -> buildTypedSymbolTable exp) exps |> List.concat

let findAllReferences pos progs =
    let table = buildTypeSymbolTable' progs
    table |> List.toSeq 
          |> Seq.filter (fun (s,l,t) -> l = pos)
          |> Seq.map (fun (s,l,t) -> t) 
          |> Seq.distinct
          |> Seq.map (fun t' -> List.filter (fun (s,l,t) -> t = t') table)
          |> Seq.concat
          |> Seq.map (fun (s,l,t) -> Var(s, l))
          |> Seq.toList


