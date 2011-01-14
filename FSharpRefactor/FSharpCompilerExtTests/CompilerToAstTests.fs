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


module CompilerToAstTests

open NUnit.Framework
open Ast
open CompilerToAst
open System.IO
open AstCatamorphisms

let path = sprintf "%s\\%s" (Directory.GetCurrentDirectory()) "test.fs" 

let stripPos (decl:Module<'a*'b>) :Module<'a> = 
                     let foldPat p = foldPat (fun (s,l) -> PVar s) 
                                             (fun l r -> PApp(l,r)) 
                                             (fun x -> PLit x) 
                                             (fun xs -> PTuple xs) 
                                             (fun () -> PWild)
                                             (fun xs -> PList xs)
                                             (fun xs -> PLongVar xs) p
                     foldExp (fun (s, l) -> Var s) 
                             (fun xs -> LongVar xs)
                             (fun ps b -> Lam(List.map (fun p -> foldPat p) ps, b)) 
                             (fun x y -> App (x, y))
                             (fun isRec p e1 e2 -> Let(isRec, foldPat p, e1, e2))
                             (fun x -> Lit x)
                             (fun xs -> Tuple xs)
                             (fun xs -> List xs)
                             (fun xs -> Exp xs)
                             (fun xs -> Types xs)
                             (fun name cases -> DisUnion (name, List.map (fun (s,l) -> s) cases))
                             (fun e cs -> Match(e, cs))
                             (fun p e -> Clause(foldPat p, e))
                             (fun p e1 e2 -> ForEach (foldPat p, e1, e2))
                             (fun e -> YieldOrReturn e)
                             (fun n ms -> NestedModule (n,ms))
                             (fun s -> Open s)
                             (fun e1 e2 e3 -> IfThenElse (e1, e2, e3))
                             (fun e1 es e3 -> DotIndexedSet (e1, es, e3))
                             (fun e1 es -> DotIndexedGet (e1, es))
                             (fun name fields -> Record (name, List.map (fun x -> Option.map (fun (s,l) -> s) x) fields))
                             (fun fields -> Exp.Record (List.map (fun ((s,l), e) -> (s,e)) fields))
                             (fun n e -> (n, e)) 
                             (fun ss e -> Exp.New (LongIdent (List.map (fun (s,l) -> Ident s) ss), e))
                             (fun name -> TypeDef.None name)
                             (fun n ms -> Class (n, ms))
                             (fun ps -> Ast.ImplicitCtor (List.map foldPat ps))
                             (fun n e -> Member (foldPat n, e))
                             (fun n -> AbstractSlot n)
                             (fun ms -> ObjExpr ms)
                             (fun () -> Ast.ArbitraryAfterError) decl
let stripAllPos exps = List.map (fun exp -> stripPos exp) exps


let parseWithPosDecl s = 
        File.WriteAllText("test.fs", s)        
        let [xs:_] = parseToAst [path]
        xs 

let parse s = s |> parseWithPosDecl |> stripAllPos |> List.map (fun (Exp xs) -> xs) |> List.concat

let parseWithPos s = s |> parseWithPosDecl |> List.map (fun (Exp xs) -> xs) |> List.concat

let parseTypes s = s |> parseWithPosDecl |> stripAllPos |> List.map (fun (Types xs) -> xs)

let parseModule s = s |> parseWithPosDecl |> stripAllPos

let loc (cs,ce,ls,le) = { srcFilename = path; srcLine = { startLine = ls; endLine = le }; srcColumn = { startColumn = cs; endColumn = ce } }

let toS x = sprintf "%A" x

let AssertAreEqual x y = Assert.AreEqual(toS x, toS y)


[<TestFixture>]
type CompilerToAstTests() =

    [<Test>]
    member this.Const() =        
        Assert.IsTrue ([Lit(Integer 42)] = parse "42")

    [<Test>]
    member this.SimpleDecls() =        
        AssertAreEqual [Let(false,PVar "x", Lit(Integer 42), Lit(Unit))] (parse "let x = 42")
        AssertAreEqual [Let(false,PVar "x", Lit(Integer 42), Lit(Unit)); Let(false,PVar "x", Lit(Integer 24), Lit(Unit))] (parse "let x = 42\nlet x = 24")

    [<Test>]
    member this.FunctionsDecls() =        
        AssertAreEqual [Let(false,PApp(PVar "f", PVar "x"), Var "x", Lit(Unit)) ]  (parse "let f x = x")
        AssertAreEqual [Let(false,PApp(PApp(PVar "f", PVar "x"), PVar "y"), Var "y", Lit(Unit)) ]  (parse "let f x y = y")        
        AssertAreEqual [Let(false,PApp(PApp(PApp(PVar "f", PVar "x"), PVar "y"), PVar "z"), Var "z", Lit(Unit)) ] (parse "let f x y z = z")
   
    [<Test>]
    member this.NestedDecls() =
     
       let ret = parse ("let x = let y = 12  \n" +
                        "        let z = 21  \n" +       
                        "        z" )
       Assert.IsTrue ([Let(false,PVar "x", Let(false,PVar "y",Lit (Integer 12),Let(false,PVar "z",Lit (Integer 21),Var "z")),  Lit Unit)] = ret)
        
    [<Test>]
    member this.NestedDeclsVsIn() =
     
       let ret = parse ("let x = let y = 12  \n" +
                        "        let z = 21  \n" +       
                        "        z" )
       let ret2 = parse ("let x = let y = 12 in let z = 21 in z")
       Assert.IsTrue ((ret = ret2))

    [<Test>]
    member this.InfixApp() =
        Assert.IsTrue ([App(App (Var "op_Addition", Var "x"), Var "y")] = parse "x + y")
        Assert.IsTrue ([App(App (Var "op_Addition", App(App (Var "op_Addition", Var "x"), Var "y")), Var "z")] = parse "x + y + z")

    [<Test>]
    member this.InfixAppVsPrefixApp() =
        Assert.IsTrue (parse "(+) x y" = parse "x + y")
        Assert.IsTrue (parse "(+) ((+) x y) z" = parse "x + y + z")
        Assert.IsTrue (parse "(+) ((+) ((+) x y) z) k" = parse "x + y + z + k")

    [<Test>]
    member this.ApplicationsAssociatesToTheLeft() =
        Assert.IsTrue([(App(Var "f", Var "g"))] = parse "f g") 
        Assert.IsTrue([(App(App(Var "f", Var "g"), Var "h"))] = parse "f g h")
        Assert.IsTrue([(App(App(App(Var "f", Var "g"), Var "h"), Var "j"))] = parse "f g h j")

    [<Test>]
    member this.ParensToModifyAssociation() =
        Assert.IsTrue([(App(Var "f", App(Var "g", Var "h")))] = parse "f (g h)")
        Assert.IsTrue([(App(Var "f", App(Var "g", App(Var "h", Var "j"))))] = parse "f (g (h j))")

    [<Test>]
    member this.Currying() = 
        let ret = parse "let squares = map square numbers"
        Assert.IsTrue ([(Let(false,PVar "squares", App (App(Var "map", Var "square"), Var "numbers"),  Lit Unit))] = ret)

    [<Test>]
    member this.Integer() =
        Assert.IsTrue([(Lit (Integer 42))] = parse "42") 
        Assert.IsTrue([(Lit (Integer -42))] = parse "-42")

    [<Test>]
    member this.Float() =
        Assert.IsTrue([(Lit (Float 0.05))] = parse "0.05")
        Assert.IsTrue([(Lit (Float -12.5))] = parse "-12.5")
        Assert.IsTrue([(Lit (Float 2.0015))] = parse "2.0015")

    [<Test>]
    member this.Char() =
        Assert.IsTrue([(Lit (Char 'f'))] = parse "'f'")
        Assert.IsTrue([(Lit (Char 'g'))] = parse "'g'")

    [<Test>]
    member this.Bool() =
        Assert.IsTrue([(Lit (Bool true))] = parse "true")
        Assert.IsTrue([(Lit (Bool false))] = parse "false")

    [<Test>]
    member this.Lambdas() =
        Assert.IsTrue([Lam([PVar "x"], Var "x")] = parse "fun x -> x") 
        Assert.IsTrue([Lam([PVar "x"], Lam([PVar "y"], Var "y"))] = parse "fun x -> fun y -> y")
        Assert.IsTrue([Lam([PVar "x"], Lam([PVar "y"], Lam([PVar "z"], Var "z")))] = parse "fun x -> fun y -> fun z -> z")

    [<Test>]
    member this.LambdasSugar() =        
        Assert.IsTrue(parse "fun x y -> y" = parse "fun x -> fun y -> y")
        Assert.IsTrue(parse "fun x y z -> z" = parse "fun x -> fun y -> fun z -> z")

    [<Test>]
    member this.OffSideLocalDefinitions2() =
        let k = [Let(false,PVar "z", 
                                Let(false,PVar "x", 
                                    Lit(Integer 12), 
                                    Let(false,PVar "y", 
                                        Lit(Integer 32), 
                                        App(App (Var "op_Addition", Var "x"), Var "y"))), 
                                Lit(Unit))]

        let x = parse "let z = (let x = 12 in let y = 32 in x + y)"

        let y = parse ("let z =       \n " + 
                       "   let x = 12 \n " +
                       "   let y = 32 \n " +
                       "   x + y         ")             

        Assert.IsTrue ( (k = x) )                                       
        Assert.IsTrue ( (x = y) )


    [<Test>]
    member this.OffSideLocalDefinition3() =
        
        let x = parse ("let computeDerivative f x = \n" +
                       "    let p1 = f (x - 0.05)   \n" +
                       "    let p2 = f (x + 0.05)   \n" +
                       "    (p2 - p1) / 0.1           ")
                           
        let y = [Let
                   (false,PApp (PApp (PVar "computeDerivative",PVar "f"),PVar "x"),
                    Let
                      (false, PVar "p1",
                       App (Var "f",App (App (Var "op_Subtraction",Var "x"),Lit (Float 0.05))),
                       Let
                         (false, PVar "p2",
                          App (Var "f",App (App (Var "op_Addition",Var "x"),Lit (Float 0.05))),
                          App
                            (App
                               (Var "op_Division",
                                App (App (Var "op_Subtraction",Var "p2"),Var "p1")),
                             Lit (Float 0.1)))),Lit Unit)]

        Assert.IsTrue ( (x = y) )

    [<Test>]
    member this.SimpleDeclsWithPos() =        
        AssertAreEqual [Let(false,PVar ("x", loc (4,5,1,1)), Lit(Integer 42), Lit(Unit))]  (parseWithPos "let x = 42")
        AssertAreEqual [Let(false,PVar ("x", loc (4,5,1,1)), Lit(Integer 42), Lit(Unit)); Let(false,PVar ("x", loc (4,5,2,2)), Lit(Integer 24), Lit(Unit))] (parseWithPos "let x = 42\nlet x = 24")

       
    [<Test>]
    member this.FunctionsDeclsWithPos() =        
        AssertAreEqual [Let(false,PApp(PVar ("f", loc (4,5,1,1)), PVar ("x", loc (6,7,1,1))), Var ("x", loc (10,11,1,1)), Lit(Unit)) ]  (parseWithPos "let f x = x")
        AssertAreEqual [Let(false,PApp(PApp(PVar ("f", loc (4,5,1,1)), PVar ("x", loc (6,7,1,1))), PVar ("y", loc (8,9,1,1))), Var ("y", loc (12,13,1,1)), Lit(Unit)) ] (parseWithPos "let f x y = y")        
        AssertAreEqual [Let(false,PApp(PApp(PApp(PVar ("f", loc (4,5,1,1)), PVar ("x", loc (6,7,1,1))), PVar ("y", loc (8,9,1,1))), PVar ("z", loc (10,11,1,1))), Var ("z", loc (14,15,1,1)), Lit(Unit)) ] (parseWithPos "let f x y z = z")
        
    [<Test>]
    member this.OffSideLocalDefinitionWithPos() =
        
        let x = parseWithPos ("let computeDerivative f x = \n" +
                              "    let p1 = f (x - 0.05)   \n" +
                              "    let p2 = f (x + 0.05)   \n" +
                              "    (p2 - p1) / 0.1           ")
                           
        let y = [Let
                   (false, PApp (PApp (PVar ("computeDerivative", loc (4,21,1,1)),PVar ("f", loc (22,23,1,1))),PVar ("x", loc (24,25,1,1))),
                    Let
                      (false, PVar ("p1", loc (8,10,2,2)),
                       App (Var ("f", loc (13,14,2,2)),App (App (Var ("op_Subtraction", loc (18,19,2,2)),Var ("x", loc (16,17,2,2))),Lit (Float 0.05))),
                       Let
                         (false, PVar ("p2", loc (8,10,3,3)),
                          App (Var ("f", loc (13,14,3,3)),App (App (Var ("op_Addition", loc (18,19,3,3)),Var ("x", loc (16,17,3,3))),Lit (Float 0.05))),
                          App
                            (App
                               (Var ("op_Division", loc (14,15,4,4)),
                                App (App (Var ("op_Subtraction", loc (8,9,4,4)),Var ("p2", loc (5,7,4,4))),Var ("p1", loc (10,12,4,4)))),
                             Lit (Float 0.1)))),Lit Unit)]

        AssertAreEqual y x

    [<Test>]
    member this.Tuples() = 
       AssertAreEqual [Let(false,PVar "x", Tuple [Lit(Integer 42);Lit(Integer 24)], Lit(Unit))]  (parse "let x = (42, 24)")
       AssertAreEqual [Let(false,PVar "x", Tuple [Lit(Integer 42);Tuple [Lit(String "Hello"); Var "y"]], Lit(Unit))]  (parse "let x = (42, (\"Hello\", y))")

    [<Test>]
    member this.Lists() = 
       AssertAreEqual [Let(false,PVar "x", List [Lit(Integer 42);Lit(Integer 24)], Lit(Unit))]  (parse "let x = [42; 24]")
       AssertAreEqual [Let(false,PVar "x", List [Lit(Integer 42); Var "y"], Lit(Unit))]  (parse "let x = [42; y]")
       AssertAreEqual [Let(false,PVar "x", List [Lit(Integer 42); List[Var "y"; Var "z"]], Lit(Unit))]  (parse "let x = [42; y; z]")

    [<Test>]
    member this.OptionType() =
        AssertAreEqual [Let(false,PVar "x", App (Var "Some", Lit(Integer 42)), Lit(Unit))]  (parse "let x = Some 42")
        AssertAreEqual [Let(false,PVar "x", Var "None", Lit(Unit))]  (parse "let x = None")
         
    [<Test>]
    member this.DiscriminatedUnion() =
        let ast = parseTypes "type Exp = Var of string" |> List.concat
        AssertAreEqual [DisUnion("Exp", ["Var"])]  ast

    [<Test>]
    member this.TuplePattern() =
        AssertAreEqual [Let(false,PApp(PVar "f", PTuple [PVar "x"; PVar "y"]), Var "x", Lit(Unit))]  (parse "let f (x,y) = x")

    [<Test>]
    member this.WildPattern() =
        AssertAreEqual [Let(false,PWild, Var "x", Lit(Unit))]  (parse "let _ = x")

    [<Test>]
    member this.SimplePatternMatching() =
        AssertAreEqual [Let(false,PApp(PVar "f", PVar "x"), Match(Var "x", [Clause(PLit(Bool(true)), Lit(Integer 42))]), Lit(Unit))]  (parse "let f x = match x with True -> 42")

    [<Test>]
    member this.SimplePatternMatchingWithTuplePattern() =
        AssertAreEqual [Let(false,PApp(PVar "f", PVar "p"), Match(Var "p", [Clause(PTuple [PVar "x"; PVar "y"], Var "x")]), Lit(Unit))]  (parse "let f p = match p with (x,y) -> x")

    [<Test>]
    member this.SimpleSeqComprehension() =        
        AssertAreEqual [Let(false,PVar "xs", App (Var "seq",App (App (Var "op_Range",Lit (Integer 1)),Lit (Integer 10))), Lit Unit)] (parse "let xs = seq { 1..10 }")

    [<Test>]
    member this.SimpleSeqComprehensionWithForIn() =
        AssertAreEqual [Let(false,PVar "xs", App (Var "seq",ForEach(PVar "i", App (App (Var "op_Range",Lit (Integer 1)), Lit (Integer 5)), YieldOrReturn (Var "i"))), Lit Unit)] (parse "let xs = seq { for i in 1..5 do yield i }")

    [<Test>]
    member this.OpenModule() = 
        AssertAreEqual [Open ["System"]] (parseModule "open System")

    [<Test>]
    member this.OpenModules() = 
        AssertAreEqual [Open ["System"]; Open ["System";"IO"]] (parseModule "open System\nopen System.IO")

    [<Test>]
    member this.ModuleQualifiedIdentifier() =
        AssertAreEqual [Let(false,PVar "xs", App (LongVar [Var "List"; Var "head"],App (App (Var "op_Range",Lit (Integer 1)),Lit (Integer 5))), Lit Unit)] (parse "let xs = List.head [1..5]")

    [<Test>]
    member this.NestedModule() =
        AssertAreEqual [NestedModule (["MyModule"], [Exp [Let(false,PVar "x",Lit (Integer 42),Lit Unit)]])]  (parseModule "module MyModule = let x = 42")

    [<Test>]
    member this.IfThenElse() =
        AssertAreEqual 
            [Let(true,PApp(PVar "fac", PVar "n"), IfThenElse(App(App (Var "op_Equality", Var "n"), Lit(Integer 0)), Lit(Integer 1), Some (App(App( Var "op_Multiply", Var "n"), App (Var "fac", App (App (Var "op_Subtraction",Var "n"),Lit (Integer 1)))))), Lit(Unit))]  
            (parse "let rec fac n = if n = 0 then 1 else n * fac (n - 1)")

    [<Test>]
    member this.LetRec() =
        let ast = parse "let rec f x = f x"
        AssertAreEqual [Let(true, PApp(PVar "f", PVar "x"), App(Var "f", Var "x"),Lit Unit)] (parse "let rec f x = f x")
        AssertAreEqual [Let(false, PApp(PVar "f", PVar "x"), App(Var "f", Var "x"),Lit Unit)] (parse "let f x = f x")

    [<Test>]
    member this.ListInMatchPattern() =
        AssertAreEqual [Let(false,PApp (PVar "f",PVar "p"),Match(Var "p",[Clause (PList [PVar "x"; PVar "y"],List [Var "x"; Var "y"])]), Lit Unit)] 
                       (parse "let f p = match p with [x;y] -> [x;y]")

    [<Test>]
     member this.ListPatternInFunction() =
        AssertAreEqual [Let(false,PApp (PVar "f", PList [PVar "x"; PVar "y"]), List [Var "x"; Var "y"], Lit Unit)] 
                       (parse "let f [x;y] = [x;y]")

    [<Test>]
    member this.ErrorRecovery() =
        AssertAreEqual [Let(false, PApp(PVar "f", PVar "x"), ArbitraryAfterError, Lit Unit)] (parse "let f x = x +")

    [<Test>]
    member this.DotIndexedSet() =
        AssertAreEqual [DotIndexedSet (Var "twoDimensionalArray", [Tuple [Lit (Integer 0); Lit (Integer 1)]], Lit (Float 1.0))] 
                       (parse "twoDimensionalArray.[0, 1] <- 1.0")

    [<Test>]
    member this.DotIndexedGet() =
        AssertAreEqual [Let (false, PVar "x", DotIndexedGet (Var "twoDimensionalArray", [Tuple [Lit (Integer 0); Lit (Integer 1)]]), Lit Unit)] 
                       (parse "let x = twoDimensionalArray.[0,1]")

    [<Test>]
    member this.RecordTypeDef() =
        let ast = parseTypes "type Point = { X : int; Y : int }" |> List.concat
        AssertAreEqual [Record("Point", [Some "X"; Some "Y"])]  ast

    [<Test>]
    member this.RecordUsage() =
        AssertAreEqual [Let (false, PVar "p", Exp.Record ["X", Lit(Integer 2); "Y", Lit(Integer 32)], Lit Unit)] 
                       (parse "let p = { X = 2; Y = 32 }")

    [<Test>]
    member this.ClassDefinition() =
        let ast = parseTypes ("type Point (x:int,y:int) = \n" +
                              "    member this.X = x      \n" +
                              "    member this.Y = y") |> List.concat
        AssertAreEqual [Class("Point", [ImplicitCtor [PVar "x"; PVar "y"]; 
                                            Member (PLongVar [PVar "this"; PVar "X"], Var "x"); 
                                            Member (PLongVar [PVar "this"; PVar "Y"], Var "y")])] ast

    [<Test>]
    member this.NewObject() =
        let ast = parse "let p = new Point(12, 31)"
        AssertAreEqual [Let (false, PVar "p", Exp.New (LongIdent [Ident "Point"], Tuple [Lit(Integer 12); Lit(Integer 31)]), Lit Unit)] ast

    [<Test>]
    member this.InterfaceDefinition() =
        let ast = parseTypes ("type IPeekPoke =  \n" +
                              "     abstract Peek: unit -> int  \n" +
                              "     abstract Poke: int -> unit    ") |> List.concat
        AssertAreEqual [Class("IPeekPoke", [AbstractSlot "Peek"; 
                                            AbstractSlot "Poke"])] ast

    [<Test>]
    member this.ObjectExpression() =
        let ast = parse "let disposable = { new IDisposable with member this.Dispose () = () }"        
        AssertAreEqual [Let (false, PVar "disposable", Exp.ObjExpr [Member (PApp (PLongVar [PVar "this"; PVar "Dispose"], PLit(Unit)), Lit Unit)], Lit Unit)] ast