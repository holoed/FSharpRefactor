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

module CompilerToAstTypedTests

open NUnit.Framework
open Ast
open CompilerToTypedAst
open System.IO
open AstCatamorphisms
open Utils

let parseWithPosDecl s = 
        File.WriteAllText("test.fs", s)        
        let [xs:_] = parseToTypedAst [path]
        [xs]

let stripPosTyped (decl:Module<string * SrcLoc * int64>) : Module<string> =             
            foldExpAlgebra {  memberSigF           =     (fun t -> Ast.MemberSig t)
                              traitCallF           =     (fun ss msig e -> Ast.TraitCall (ss, msig, e))
                              typetestF            =     (fun e t -> Ast.TypeTest (e, t))
                              measureVarF          =     (fun s -> Measure.MVar s)
                              measureOneF          =     (fun unit -> Measure.One) 
                              measureAnonF         =     (fun unit -> Measure.Anon)
                              measureDivideF       =     (fun m1 m2 -> Measure.Divide(m1, m2))
                              powerF               =     (fun m n -> Measure.Power (m, n))
                              measureNamedF        =     (fun t -> Measure.Named t)
                              measureSeqF          =     (fun ms -> Measure.Seq ms)
                              measureF             =     (fun e m -> Measure(e, m))
                              quoteF               =     (fun e1 e2 -> Quote (e1, e2))
                              inferredDowncastF    =     (fun e -> InferredDowncast e)
                              inferredUpcastF      =     (fun e -> InferredUpcast e)
                              lazyF                =     (fun e -> Lazy e)
                              whileF               =     (fun e1 e2 -> While(e1, e2))
                              assertF              =     (fun e -> Assert e)
                              nullF                =     (fun () -> Null)
                              varF                 =     (fun (s, l, t) -> Var s) 
                              longVarF             =     (fun xs -> LongVar xs)
                              longVarSetF          =     (fun e1 e2 -> LongVarSet (e1, e2))
                              lamF                 =     (fun ps b -> Lam(ps, b)) 
                              appF                 =     (fun x y -> App (x, y))
                              letF                 =     (fun isRec bs e2 -> Let(isRec, bs, e2))
                              letBangF             =     (fun p e1 e2 -> LetBang (p, e1, e2))
                              litF                 =     (fun x -> Lit x)
                              tupleF               =     (fun xs -> Tuple xs)
                              listF                =     (fun xs -> List xs)
                              expF                 =     (fun xs -> Exp xs)
                              typesF               =     (fun xs -> Types xs)
                              unionF               =     (fun name cases -> DisUnion (name, List.map (fun (s,l,t) -> s) cases))
                              enumF               =      (fun name cases -> Enum (name, List.map (fun ((s,l,t), c) -> (s, c)) cases))
                              addressofF           =     (fun e -> AddressOf e)
                              matchF               =     (fun e cs -> Match(e, cs))
                              clauseF              =     (fun p e -> Clause(p, e))
                              forEachF             =     (fun p e1 e2 -> ForEach (p, e1, e2))
                              forF                 =     (fun i e1 e2 e3 -> For(i, e1, e2, e3))
                              yieldOrRetF          =     (fun e -> YieldOrReturn e)
                              yieldOrRetFromF      =     (fun e -> YieldOrReturnFrom e)
                              moduleF              =     (fun n ms -> NestedModule (n,ms))
                              openF                =     (fun s -> Open s)
                              exceptionF           =     (fun ex -> Exception ex)
                              hashdirectiveF       =     (fun s ss -> HashDirective (s, ss))
                              moduleAbbrevF        =     (fun s ss -> ModuleAbbrev (s, ss))
                              attributesF          =     (fun xs -> Attributes xs)
                              exceptionDefF        =     (fun n ms -> ExceptionDef (n, ms))
                              ifThenElseF          =     (fun e1 e2 e3 -> IfThenElse (e1, e2, e3))
                              dotGetF              =     (fun e li -> DotGet (e, li))
                              dotSetF              =     (fun e1 li e2 -> DotSet(e1, li, e2))
                              dotIndexedSetF       =     (fun e1 es e3 -> DotIndexedSet (e1, es, e3))
                              dotIndexedGetF       =     (fun e1 es -> DotIndexedGet (e1, es))
                              recordDefF           =     (fun name fields ms -> Record (name, List.map (fun x -> Option.map (fun (s,l,t) -> s) x) fields, ms))
                              recordInstF          =     (fun fields -> Exp.Record (List.map (fun ((s,l,t), e) -> (s,e)) fields))
                              recordFieldInstF     =     (fun n e -> (n, e)) 
                              newF                 =     (fun ss e -> Exp.New (ss, e))
                              typeappF             =     (fun e ts -> Exp.TypeApp (e, ts))
                              noneF                =     (fun name -> TypeDef.None name)
                              classF               =     (fun n ms -> Class (n, ms))
                              valfieldF            =     (fun t1 t2 -> ValField (t1, t2))
                              inheritF             =     (fun t1 t2 -> Inherit(t1, t2))
                              implicitInheritF     =     (fun t e id -> ImplicitInherit (t, e, id))
                              implicitConF         =     (fun ps -> Ast.ImplicitCtor ps)
                              memberF              =     (fun n e -> Member (n, e))
                              abstractSlotF        =     (fun n -> AbstractSlot n)
                              objExprF             =     (fun ms -> ObjExpr ms)
                              doF                  =     (fun e -> Do e)
                              doBangF              =     (fun e -> DoBang e)
                              downcastF            =     (fun e t -> Downcast (e, t))
                              upcastF              =     (fun e t -> Upcast (e, t))
                              typedF               =     (fun e t -> Typed (e, t))
                              interfaceF           =     (fun t ms -> Interface (t, ms))
                              letBindingsF         =     (fun es -> LetBindings es)
                              abbrevF              =     (fun n t -> Abbrev(n, t))
                              tfunF                =     (fun t1 t2 -> TFun(t1, t2))
                              tIdentF              =     (fun (s,l,t) -> Ident s)
                              tLongIdentF          =     (fun ts -> LongIdent ts)
                              tvarF                =     (fun t -> TVar t)  
                              tappF                =     (fun t ts -> TApp (t, ts))
                              ttupleF              =     (fun ts -> TTuple ts)
                              tarrayF              =     (fun n t -> TArray (n, t))
                              tmeasurePowerF       =     (fun t n -> TMeasurePower (t, n))
                              tmeasureOneF         =     (fun () -> TMeasureOne)
                              tanonF               =     (fun () -> TAnon)
                              tryWithF             =     (fun e cl -> TryWith(e, cl))   
                              tryFinallyF          =     (fun e1 e2 -> TryFinally(e1, e2))                        
                              errorF               =     (fun () -> Ast.ArbitraryAfterError) 
                              pVarF                =     (fun (s,l,t) -> PVar s) 
                              pAppF                =     (fun l r -> PApp(l,r)) 
                              porF                 =     (fun p1 p2 -> POr(p1, p2))
                              pandsF               =     (fun ps -> PAnds ps)
                              pLitF                =     (fun x -> PLit x) 
                              pTupleF              =     (fun xs -> PTuple xs) 
                              pRecordF             =     (fun xs -> PRecord xs)
                              pWildF               =     (fun () -> PWild)
                              pArrayOrListF        =     (fun xs -> PList xs)
                              pLongVarF            =     (fun xs -> PLongVar xs)
                              pIsInstF             =     (fun t -> PIsInst t)
                              pnullF               =     (fun () -> PNull) 
                              pattributeF          =     (fun p attrs -> PAttribute(p, attrs))
                              attributeF           =     (fun e -> Attribute e)
                              pnamedF              =     (fun p1 p2 -> PNamed (p1, p2))} decl

let stripAllTypedPos exps = List.map (fun exp -> stripPosTyped exp) exps

let parseTyped f s = s |> f |> stripAllTypedPos |> List.map (fun (Exp xs) -> xs) |> List.concat

let parseTypedWithPos f s = s |> f |> List.map (fun (Exp xs) -> xs) |> List.concat

let parseTypedTypes f s = s |> f |> stripAllTypedPos |> List.map (fun (Types xs) -> xs)

let parseTypedModule f s = s |> f |> stripAllTypedPos

let parse = parseTyped parseWithPosDecl

let parseWithPos = parseTypedWithPos parseWithPosDecl

let parseTypes = parseTypedTypes parseWithPosDecl

let parseModule = parseTypedModule parseWithPosDecl

[<TestFixture>]
type CompilerToAstTypedTests() =
     
    [<Test>]
    member this.Const() =                
        AssertAreEqual [Lit (Integer 42)] (parse "42")

    [<Test>]
    member this.SimpleDecls() =        
        let ast = parse "let f x y = x + y"
        AssertAreEqual [Let  (false,  [(PVar "f",   
                                            Lam ([PVar "x"],  Lam ([PVar "y"], App (App (Var "( + )",Var "x"),Var "y"))))],
                                        Lit Unit)] ast

