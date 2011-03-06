module Utils

open NUnit.Framework
open Ast
open CompilerToAst
open System.IO
open AstCatamorphisms

let path = sprintf "%s\\%s" (Directory.GetCurrentDirectory()) "test.fs" 

let stripPos (decl:Module<'a*'b>) :Module<'a> =             
            foldExpAlgebra {  varF                 =     (fun (s, l) -> Var s) 
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
                              unionF               =     (fun name cases -> DisUnion (name, List.map (fun (s,l) -> s) cases))
                              matchF               =     (fun e cs -> Match(e, cs))
                              clauseF              =     (fun p e -> Clause(p, e))
                              forEachF             =     (fun p e1 e2 -> ForEach (p, e1, e2))
                              yieldOrRetF          =     (fun e -> YieldOrReturn e)
                              yieldOrRetFromF      =     (fun e -> YieldOrReturnFrom e)
                              moduleF              =     (fun n ms -> NestedModule (n,ms))
                              openF                =     (fun s -> Open s)
                              exceptionF           =     (fun ex -> Exception ex)
                              exceptionDefF        =     (fun n ms -> ExceptionDef (n, ms))
                              ifThenElseF          =     (fun e1 e2 e3 -> IfThenElse (e1, e2, e3))
                              dotGetF              =     (fun e li -> DotGet (e, li))
                              dotIndexedSetF       =     (fun e1 es e3 -> DotIndexedSet (e1, es, e3))
                              dotIndexedGetF       =     (fun e1 es -> DotIndexedGet (e1, es))
                              recordDefF           =     (fun name fields ms -> Record (name, List.map (fun x -> Option.map (fun (s,l) -> s) x) fields, ms))
                              recordInstF          =     (fun fields -> Exp.Record (List.map (fun ((s,l), e) -> (s,e)) fields))
                              recordFieldInstF     =     (fun n e -> (n, e)) 
                              newF                 =     (fun ss e -> Exp.New (ss, e))
                              typeappF             =     (fun e ts -> Exp.TypeApp (e, ts))
                              noneF                =     (fun name -> TypeDef.None name)
                              classF               =     (fun n ms -> Class (n, ms))
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
                              tIdentF              =     (fun (s,l) -> Ident s)
                              tLongIdentF          =     (fun ts -> LongIdent ts)
                              tvarF                =     (fun t -> TVar t)  
                              tappF                =     (fun t ts -> TApp (t, ts))
                              ttupleF              =     (fun ts -> TTuple ts)
                              tarrayF              =     (fun n t -> TArray (n, t))
                              tanonF               =     (fun () -> TAnon)
                              tryWithF             =     (fun e cl -> TryWith(e, cl))                           
                              errorF               =     (fun () -> Ast.ArbitraryAfterError) 
                              pVarF                =     (fun (s,l) -> PVar s) 
                              pAppF                =     (fun l r -> PApp(l,r)) 
                              pLitF                =     (fun x -> PLit x) 
                              pTupleF              =     (fun xs -> PTuple xs) 
                              pRecordF             =     (fun xs -> PRecord xs)
                              pWildF               =     (fun () -> PWild)
                              pArrayOrListF        =     (fun xs -> PList xs)
                              pLongVarF            =     (fun xs -> PLongVar xs)
                              pIsInstF             =     (fun t -> PIsInst t) } decl
let stripAllPos exps = List.map (fun exp -> stripPos exp) exps

let parse f s = s |> f |> stripAllPos |> List.map (fun (Exp xs) -> xs) |> List.concat

let parseWithPos f s = s |> f |> List.map (fun (Exp xs) -> xs) |> List.concat

let parseTypes f s = s |> f |> stripAllPos |> List.map (fun (Types xs) -> xs)

let parseModule f s = s |> f |> stripAllPos

let loc (cs,ce,ls,le) = { srcFilename = path; srcLine = { startLine = ls; endLine = le }; srcColumn = { startColumn = cs; endColumn = ce } }

let toS x = sprintf "%A" x

let AssertAreEqual x y = Assert.AreEqual(toS x, toS y)