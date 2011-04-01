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


module CompilerToAstTests

open NUnit.Framework
open Ast
open CompilerToAst
open System.IO
open AstCatamorphisms
open Utils

let parseWithPosDecl s = 
        File.WriteAllText("test.fs", s)        
        let [xs:_] = parseToAst [path]
        xs 

let parse = parse parseWithPosDecl

let parseWithPos = parseWithPos parseWithPosDecl

let parseTypes = parseTypes parseWithPosDecl

let parseModule = parseModule parseWithPosDecl

[<TestFixture>]
type CompilerToAstTests() =

    [<Test>]
    member this.Const() =        
        Assert.IsTrue ([Lit(Integer 42)] = parse "42")

    [<Test>]
    member this.SimpleDecls() =        
        AssertAreEqual [Let(false,[PVar "x", Lit(Integer 42)], Lit(Unit))] (parse "let x = 42")
        AssertAreEqual [Let(false,[PVar "x", Lit(Integer 42)], Lit(Unit)); Let(false,[PVar "y", Lit(Integer 24)], Lit(Unit))] (parse "let x = 42\nlet y = 24")

    [<Test>]
    member this.FunctionsDecls() =        
        AssertAreEqual [Let(false,[PApp(PVar "f", PVar "x"), Var "x"], Lit(Unit)) ]  (parse "let f x = x")
        AssertAreEqual [Let(false,[PApp(PApp(PVar "f", PVar "x"), PVar "y"), Var "y"], Lit(Unit)) ]  (parse "let f x y = y")        
        AssertAreEqual [Let(false,[PApp(PApp(PApp(PVar "f", PVar "x"), PVar "y"), PVar "z"), Var "z"], Lit(Unit)) ] (parse "let f x y z = z")
   
    [<Test>]
    member this.NestedDecls() =
     
       let ret = parse ("let x = let y = 12  \n" +
                        "        let z = 21  \n" +       
                        "        z" )
       Assert.IsTrue ([Let(false,
                            [PVar "x", 
                                    Let(false,
                                            [PVar "y",Lit (Integer 12)],
                                                Let(false,
                                                    [PVar "z",Lit (Integer 21)],
                                                        Var "z"))],  Lit Unit)] = ret)
        
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
        Assert.IsTrue ([(Let(false,[PVar "squares", App (App(Var "map", Var "square"), Var "numbers")],  Lit Unit))] = ret)

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
        let k = [Let(false,[PVar "z", 
                                Let(false,[PVar "x", 
                                        Lit(Integer 12)], 
                                            Let(false,[PVar "y", 
                                                    Lit(Integer 32)], 
                                                    App(App (Var "op_Addition", Var "x"), Var "y")))], 
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
                   (false,[PApp (PApp (PVar "computeDerivative",PVar "f"),PVar "x"),
                    Let
                      (false, [PVar "p1",
                       App (Var "f",App (App (Var "op_Subtraction",Var "x"),Lit (Float 0.05)))],
                       Let
                         (false, [PVar "p2",
                          App (Var "f",App (App (Var "op_Addition",Var "x"),Lit (Float 0.05)))],
                          App
                            (App
                               (Var "op_Division",
                                App (App (Var "op_Subtraction",Var "p2"),Var "p1")),
                             Lit (Float 0.1))))],Lit Unit)]

        Assert.IsTrue ( (x = y) )

    [<Test>]
    member this.SimpleDeclsWithPos() =        
        AssertAreEqual [Let(false,[PVar ("x", loc (4,5,1,1)), Lit(Integer 42)], Lit(Unit))]  (parseWithPos "let x = 42")
        AssertAreEqual [Let(false,[PVar ("x", loc (4,5,1,1)), Lit(Integer 42)], Lit(Unit)); Let(false,[PVar ("x", loc (4,5,2,2)), Lit(Integer 24)], Lit(Unit))] (parseWithPos "let x = 42\nlet x = 24")

       
    [<Test>]
    member this.FunctionsDeclsWithPos() =        
        AssertAreEqual [Let(false,[PApp(PVar ("f", loc (4,5,1,1)), PVar ("x", loc (6,7,1,1))), Var ("x", loc (10,11,1,1))], Lit(Unit)) ]  (parseWithPos "let f x = x")
        AssertAreEqual [Let(false,[PApp(PApp(PVar ("f", loc (4,5,1,1)), PVar ("x", loc (6,7,1,1))), PVar ("y", loc (8,9,1,1))), Var ("y", loc (12,13,1,1))], Lit(Unit)) ] (parseWithPos "let f x y = y")        
        AssertAreEqual [Let(false,[PApp(PApp(PApp(PVar ("f", loc (4,5,1,1)), PVar ("x", loc (6,7,1,1))), PVar ("y", loc (8,9,1,1))), PVar ("z", loc (10,11,1,1))), Var ("z", loc (14,15,1,1))], Lit(Unit)) ] (parseWithPos "let f x y z = z")
        
    [<Test>]
    member this.OffSideLocalDefinitionWithPos() =
        
        let x = parseWithPos ("let computeDerivative f x = \n" +
                              "    let p1 = f (x - 0.05)   \n" +
                              "    let p2 = f (x + 0.05)   \n" +
                              "    (p2 - p1) / 0.1           ")
                           
        let y = [Let
                   (false, [PApp (PApp (PVar ("computeDerivative", loc (4,21,1,1)),PVar ("f", loc (22,23,1,1))),PVar ("x", loc (24,25,1,1))),
                        Let
                            (false, [PVar ("p1", loc (8,10,2,2)),
                                App (Var ("f", loc (13,14,2,2)),App (App (Var ("op_Subtraction", loc (18,19,2,2)),Var ("x", loc (16,17,2,2))),Lit (Float 0.05)))],
                                    Let
                                        (false, [PVar ("p2", loc (8,10,3,3)),
                                            App (Var ("f", loc (13,14,3,3)),App (App (Var ("op_Addition", loc (18,19,3,3)),Var ("x", loc (16,17,3,3))),Lit (Float 0.05)))],
                                            App(App
                                               (Var ("op_Division", loc (14,15,4,4)),
                                                App (App (Var ("op_Subtraction", loc (8,9,4,4)),Var ("p2", loc (5,7,4,4))),Var ("p1", loc (10,12,4,4)))),
                                             Lit (Float 0.1))))],Lit Unit)]

        AssertAreEqual y x

    [<Test>]
    member this.Tuples() = 
       AssertAreEqual [Let(false,[PVar "x", Tuple [Lit(Integer 42);Lit(Integer 24)]], Lit(Unit))]  (parse "let x = (42, 24)")
       AssertAreEqual [Let(false,[PVar "x", Tuple [Lit(Integer 42);Tuple [Lit(String "Hello"); Var "y"]]], Lit(Unit))]  (parse "let x = (42, (\"Hello\", y))")

    [<Test>]
    member this.Lists() = 
       AssertAreEqual [Let(false,[PVar "x", List [Lit(Integer 42);Lit(Integer 24)]], Lit(Unit))]  (parse "let x = [42; 24]")
       AssertAreEqual [Let(false,[PVar "x", List [Lit(Integer 42); Var "y"]], Lit(Unit))]  (parse "let x = [42; y]")
       AssertAreEqual [Let(false,[PVar "x", List [Lit(Integer 42); List[Var "y"; Var "z"]]], Lit(Unit))]  (parse "let x = [42; y; z]")

    [<Test>]
    member this.OptionType() =
        AssertAreEqual [Let(false,[PVar "x", App (Var "Some", Lit(Integer 42))], Lit(Unit))]  (parse "let x = Some 42")
        AssertAreEqual [Let(false,[PVar "x", Var "None"], Lit(Unit))]  (parse "let x = None")
         
    [<Test>]
    member this.DiscriminatedUnion() =
        let ast = parseTypes "type Exp = Var of string" |> List.concat
        AssertAreEqual [DisUnion("Exp", ["Var"])]  ast

    [<Test>]
    member this.TuplePattern() =
        AssertAreEqual [Let(false,[PApp(PVar "f", PTuple [PVar "x"; PVar "y"]), Var "x"], Lit(Unit))]  (parse "let f (x,y) = x")

    [<Test>]
    member this.WildPattern() =
        AssertAreEqual [Let(false,[PWild, Var "x"], Lit(Unit))]  (parse "let _ = x")

    [<Test>]
    member this.SimplePatternMatching() =
        AssertAreEqual [Let(false,[PApp(PVar "f", PVar "x"), Match(Var "x", [Clause(PLit(Bool(true)), Lit(Integer 42))])], Lit(Unit))]  (parse "let f x = match x with True -> 42")

    [<Test>]
    member this.SimplePatternMatchingWithTuplePattern() =
        AssertAreEqual [Let(false,[PApp(PVar "f", PVar "p"), Match(Var "p", [Clause(PTuple [PVar "x"; PVar "y"], Var "x")])], Lit(Unit))]  (parse "let f p = match p with (x,y) -> x")

    [<Test>]
    member this.SimpleSeqComprehension() =        
        AssertAreEqual [Let(false,[PVar "xs", App (Var "seq",App (App (Var "op_Range",Lit (Integer 1)),Lit (Integer 10)))], Lit Unit)] (parse "let xs = seq { 1..10 }")

    [<Test>]
    member this.SimpleSeqComprehensionWithForIn() =
        AssertAreEqual [Let(false,[PVar "xs", App (Var "seq",ForEach(PVar "i", App (App (Var "op_Range",Lit (Integer 1)), Lit (Integer 5)), YieldOrReturn (Var "i")))], Lit Unit)] (parse "let xs = seq { for i in 1..5 do yield i }")

    [<Test>]
    member this.OpenModule() = 
        AssertAreEqual [Open ["System"]] (parseModule "open System")

    [<Test>]
    member this.OpenModules() = 
        AssertAreEqual [Open ["System"]; Open ["System";"IO"]] (parseModule "open System\nopen System.IO")

    [<Test>]
    member this.ModuleQualifiedIdentifier() =
        AssertAreEqual [Let(false,[PVar "xs", App (LongVar [Var "List"; Var "head"],App (App (Var "op_Range",Lit (Integer 1)),Lit (Integer 5)))], Lit Unit)] (parse "let xs = List.head [1..5]")

    [<Test>]
    member this.NestedModule() =
        AssertAreEqual [NestedModule (["MyModule"], [Exp [Let(false,[PVar "x",Lit (Integer 42)],Lit Unit)]])]  (parseModule "module MyModule = let x = 42")

    [<Test>]
    member this.IfThenElse() =
        AssertAreEqual 
            [Let(true,[PApp(PVar "fac", PVar "n"), IfThenElse(App(App (Var "op_Equality", Var "n"), Lit(Integer 0)), Lit(Integer 1), Some (App(App( Var "op_Multiply", Var "n"), App (Var "fac", App (App (Var "op_Subtraction",Var "n"),Lit (Integer 1))))))], Lit(Unit))]  
            (parse "let rec fac n = if n = 0 then 1 else n * fac (n - 1)")

    [<Test>]
    member this.LetRec() =
        let ast = parse "let rec f x = f x"
        AssertAreEqual [Let(true, [PApp(PVar "f", PVar "x"), App(Var "f", Var "x")],Lit Unit)] (parse "let rec f x = f x")
        AssertAreEqual [Let(false,[PApp(PVar "f", PVar "x"), App(Var "f", Var "x")],Lit Unit)] (parse "let f x = f x")

    [<Test>]
    member this.ListInMatchPattern() =
        AssertAreEqual [Let(false,[PApp (PVar "f",PVar "p"),Match(Var "p",[Clause (PList [PVar "x"; PVar "y"],List [Var "x"; Var "y"])])], Lit Unit)] 
                       (parse "let f p = match p with [x;y] -> [x;y]")

    [<Test>]
     member this.ListPatternInFunction() =
        AssertAreEqual [Let(false,[PApp (PVar "f", PList [PVar "x"; PVar "y"]), List [Var "x"; Var "y"]], Lit Unit)] 
                       (parse "let f [x;y] = [x;y]")

    [<Test>]
    member this.ErrorRecovery() =
        AssertAreEqual [Let(false,[PApp(PVar "f", PVar "x"), ArbitraryAfterError], Lit Unit)] (parse "let f x = x +")

    [<Test>]
    member this.DotIndexedSet() =
        AssertAreEqual [DotIndexedSet (Var "twoDimensionalArray", [Tuple [Lit (Integer 0); Lit (Integer 1)]], Lit (Float 1.0))] 
                       (parse "twoDimensionalArray.[0, 1] <- 1.0")

    [<Test>]
    member this.DotIndexedGet() =
        AssertAreEqual [Let (false,[PVar "x", DotIndexedGet (Var "twoDimensionalArray", [Tuple [Lit (Integer 0); Lit (Integer 1)]])], Lit Unit)] 
                       (parse "let x = twoDimensionalArray.[0,1]")

    [<Test>]
    member this.RecordTypeDef() =
        let ast = parseTypes "type Point = { X : int; Y : int }" |> List.concat
        AssertAreEqual [Record("Point", [Some "X"; Some "Y"], [])]  ast

    [<Test>]
    member this.RecordUsage() =
        AssertAreEqual [Let (false,[PVar "p", Exp.Record ["X", Lit(Integer 2); "Y", Lit(Integer 32)]], Lit Unit)] 
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
        AssertAreEqual [Let (false, [PVar "p", Exp.New (LongIdent [Ident "Point"], Tuple [Lit(Integer 12); Lit(Integer 31)])], Lit Unit)] ast

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
        AssertAreEqual [Let (false, [PVar "disposable", Exp.ObjExpr [Member (PApp (PLongVar [PVar "this"; PVar "Dispose"], PLit(Unit)), Lit Unit)]], Lit Unit)] ast

    [<Test>]
    member this.DoExpression() =
        let ast = parse "do Hello ()"
        AssertAreEqual [Do(App(Var "Hello", Lit(Unit)))] ast

    [<Test>]
    member this.Downcast() =
        let ast = parse "let x = 2 :?> double"
        AssertAreEqual [Let (false, [PVar "x", Downcast(Lit(Integer 2), LongIdent [Ident "double"])], Lit Unit)] ast

    [<Test>]
    member this.RecordMembers() =
        let ast = parseTypes "type Point = { X : int; Y: int} with member this.Sum = this.X + this.Y" |> List.concat
        AssertAreEqual [Record("Point", [Some "X"; Some "Y"], 
            [Member (PLongVar [PVar "this"; PVar "Sum"], App(App (Var "op_Addition", LongVar [Var "this"; Var "X"]), LongVar [Var "this"; Var "Y"]))])]  ast

    [<Test>]
    member this.InterfaceImplementation() =
        let ast = parseTypes "type Foo = interface IDisposable with member this.Dispose () = ()" |> List.concat
        AssertAreEqual [Class("Foo", [Interface(LongIdent [Ident "IDisposable"], [Member (PApp(PLongVar [PVar "this"; PVar "Dispose"], PLit(Unit)), Lit Unit)])])] ast
   
    [<Test>]
    member this.``Assignment of a mutable variable``() =
         let ast = parse ("x <- x + 1");
         AssertAreEqual [LongVarSet (LongVar [Var "x"], App(App (Var "op_Addition", Var "x"), Lit(Integer 1)))] ast
                                        
    [<Test>]
    member this.Upcast() =
        let ast = parse "let x = foo :> IBar"
        AssertAreEqual [Let (false, [PVar "x", Upcast(Var "foo", LongIdent [Ident "IBar"])], Lit Unit)] ast

    [<Test>]
    member this.``Lets defined in the context of a class`` () =
        let ast = parseTypes ("type Point = \n" +
                              "    let x = 42") |> List.concat
        AssertAreEqual [Class("Point", [LetBindings [Let(false,[PVar "x", Lit(Integer 42)], Lit(Unit))]])] ast

    [<Test>]
    member this.``A type abbreviation is an alias or alternate name for a type.`` () =
        let ast = parseTypes ("type Size = int") |> List.concat
        AssertAreEqual [TypeDef.Abbrev("Size", LongIdent [Ident "int"])] ast

    [<Test>]
    member this.``Function type constructor`` () =
        let ast = parseTypes ("type transform<'a> = 'a -> 'a") |> List.concat
        AssertAreEqual [TypeDef.Abbrev("transform", Type.TFun(TVar (Ident "a"), TVar (Ident "a")))] ast

    [<Test>]
    member this.``Error in Empty computation expression`` () =
        let ast = parse "let x = state { let"
        AssertAreEqual [Let (false, [PVar "x", App(Var "state", ArbitraryAfterError)], Lit Unit)] ast

    [<Test>]
    member this.``Return in computation expression`` () =
        let ast = parse "let x = state { return 5 }"
        AssertAreEqual [Let (false, [PVar "x", App (Var "state",YieldOrReturn (Lit (Integer 5)))],Lit Unit)] ast

    [<Test>]
    member this.``Return! in computation expression`` () =
        let ast = parse "let x = state { return! 5 }"
        AssertAreEqual [Let (false, [PVar "x", App (Var "state",YieldOrReturnFrom (Lit (Integer 5)))],Lit Unit)] ast

    [<Test>]
    member this.``Conditional with no else branch`` () =
        let ast = parse "let DoSome() = if (x > 0) then do Hello()"
        AssertAreEqual [Let (false, [PApp (PVar "DoSome",PLit Unit),  IfThenElse(App(App (Var "op_GreaterThan", Var "x"), Lit(Integer 0)), Do (App (Var "Hello",Lit Unit)), Option.None)],Lit Unit)] ast
        
    [<Test>]
    member this.``try with expression`` () =
        let ast = parse("let divide1 x y =      \n" +
                         "    try               \n" +
                         "      Some (x / y)    \n" +
                         "    with              \n" +
                         "    | :? System.DivideByZeroException -> None ")
        AssertAreEqual [Let
                           (false,[PApp (PApp (PVar "divide1",PVar "x"),PVar "y"),
                                TryWith
                                  (App (Var "Some",App (App (Var "op_Division",Var "x"),Var "y")),
                                   [Clause
                                      (PIsInst (LongIdent [Ident "System"; Ident "DivideByZeroException"]),
                                       Var "None")])],Lit Unit)] ast

    [<Test>]
    member this.``Let! in computation expression``  () =
        let ast = parse("let y = state { let! x = f   \n" +
                         "               return x }")
        AssertAreEqual [Let (false,[PVar "y",  App (Var "state",LetBang (PVar "x",Var "f",YieldOrReturn (Var "x")))], Lit Unit)] ast

    [<Test>]
    member this.``Mutually recursive nested functions`` () =
        let ast = parse ("let loop e =              \n" +
                         "    let rec foo x =       \n" +
                         "      bar x               \n" +
                         "    and bar y =           \n" +
                         "        foo y             \n" +
                         "    foo e")
        AssertAreEqual [Let(false,
                            [(PApp (PVar "loop",PVar "e"),
                              Let
                                (true,
                                 [(PApp (PVar "foo",PVar "x"), App (Var "bar",Var "x"));
                                  (PApp (PVar "bar",PVar "y"), App (Var "foo",Var "y"))],
                                 App (Var "foo",Var "e")))],Lit Unit)] ast

    [<Test>]
    member this.``Support for tuples fst and snd``() =
        AssertAreEqual [Let (false,[(PApp (PVar "f",PVar "x"), App (Var "fst",Var "x"))],Lit Unit)] (parse "let f x = fst x")
        AssertAreEqual [Let (false,[(PApp (PVar "f",PVar "x"), App (Var "snd",Var "x"))],Lit Unit)] (parse "let f x = snd x")

    [<Test>]
    member this.``do! in computation expression``  () =
        AssertAreEqual [Let (false, [(PApp (PVar "f",PVar "x"), App (Var "state",DoBang (App (Var "f",Var "x"))))], Lit Unit)] (parse("let f x = state { do! f x }"))

    [<Test>]
    member this.``dot get for object instances`` () =
        AssertAreEqual [Let (false, [(PApp (PVar "getName",PVar "xs"), DotGet (App (LongVar [Var "List"; Var "head"],Var "xs"),LongVar [Var "Name"]))], Lit Unit)] 
                       (parse "let getName xs = (List.head xs).Name")

    [<Test>]
    member this.``Typed function`` () =
        AssertAreEqual [Let (false,[(PApp (PVar "f",PVar "x"), Typed (Var "x",LongIdent [Ident "int"]))], Lit Unit)] (parse "let f x : int = x")

    [<Test>]
    member this.``Exception declaration`` () =
        AssertAreEqual [Exception (ExceptionDef ("Empty",[]))] (parseModule "exception Empty")

    [<Test>]
    member this.``Exception declaration with static members`` () =
        AssertAreEqual [Exception (ExceptionDef ("Empty",[Member (PVar "Foo",Lit (Integer 42))]))] 
                       (parseModule "exception Empty with static member Foo = 42")

    [<Test>]
    member this.``Exception declaration with instance members`` () =
        AssertAreEqual [Exception (ExceptionDef ("Empty",[Member (PLongVar [PVar "this"; PVar "Foo"],Lit (Integer 42))]))] 
                       (parseModule "exception Empty with member this.Foo = 42")

    [<Test>]
    member this.``Type application in type constraint`` () =
        AssertAreEqual [Let (false, [(PVar "xs", Typed (Var "ys",TApp (LongIdent [Ident "Seq"],[TVar (Ident "a")])))], Lit Unit)]
                       (parse "let xs : Seq<'a> = ys")

    [<Test>]
    member this.``Tuple type in type constraint`` () =
        AssertAreEqual [Let (false, [(PVar "xs", Typed (Var "ys",TTuple [TVar (Ident "a"); TVar (Ident "b")]))], Lit Unit)] (parse "let xs : 'a * 'b = ys")

    [<Test>]
    member this.``Record pattern`` () =
        AssertAreEqual [Let (false, [(PRecord [("FirstName", PVar "x"); ("LastName", PVar "y")], Var "ret")], Lit Unit)] 
                       (parse "let { FirstName = x; LastName = y } = ret")

    [<Test>]
    member this.``Anonymous type constraint`` () =
        AssertAreEqual [Let (false, [(PVar "x", Typed (Var "dict",TApp (LongIdent [Ident "IDictionary"],[TAnon; TAnon])))], Lit Unit)]
                       (parse "let x : IDictionary<_, _> = dict")

    [<Test>]
    member this.``Array type constraint`` () =        
        AssertAreEqual [Let (false, [(PVar "x", Typed (Var "xs",TArray (1, LongIdent[Ident "int"])))], Lit Unit)]
                       (parse "let x : int[] = xs")

    [<Test>]
    member this.``Type Application in instantiation.`` () =
        AssertAreEqual [Let (false,[(PVar "x", TypeApp (Var "Foo",[LongIdent [Ident "int"]]))],Lit Unit)]
                       (parse "let x = Foo<int>")

    [<Test>]
    member this.``AddressOf in expression`` () =
        AssertAreEqual [Let (false,[(PVar "x", AddressOf (Var "y"))],Lit Unit)]
                       (parse "let x = &y")
    
    [<Test>]
    member this.``Hash directive`` () =
        AssertAreEqual [HashDirective ("nowarn",["51"])]
                       (parseModule " #nowarn \"51\"")

    [<Test>]
    member this.``Null pattern`` () =        
        AssertAreEqual [Let (false,[(PNull, Var "x")],Lit Unit)]
                       (parse "let null = x")

    [<Test>]
    member this.``Null value`` () =
        AssertAreEqual [Let (false,[PVar "x", Null],Lit Unit)]
                       (parse "let x = null")

    [<Test>]
    member this.``Int64 literal value`` () =
        AssertAreEqual [Let (false,[PVar "x", Lit(Int64(0L))],Lit Unit)]
                       (parse "let x = 0L")

    [<Test>]
    member this.``Or on exception matching`` () =
        let ast = parse ("try Foo() with                     \n" +
                         "| :? System.ArgumentException      \n" +
                         "| :? System.ArgumentNullException -> 42")
        AssertAreEqual [TryWith (App (Var "Foo",Lit Unit), 
                            [Clause (POr (PIsInst (LongIdent [Ident "System"; Ident "ArgumentException"]), 
                                          PIsInst (LongIdent [Ident "System"; Ident "ArgumentNullException"])), Lit (Integer 42))])] ast

    [<Test>]
    member this.``For loop``() =
        let ast = parse ("for i = 0 to 5 do \n" +
                         "   printf \"%i\" i ")
        AssertAreEqual [For (PVar "i",Lit (Integer 0),Lit (Integer 5), App (App (Var "printf",Lit (String "%i")),Var "i"))] ast


    [<Test>]
    member this.``Extern method arguments attributes``() =
        let ast = parseModule (@"extern bool private HeapSetInformation( 
                                      UIntPtr _HeapHandle, 
                                      UInt32  _HeapInformationClass, 
                                      UIntPtr _HeapInformation, 
                                      UIntPtr _HeapInformationLength)")
        AssertAreEqual [Exp [Let (false,
                                   [(PApp
                                       (PVar "HeapSetInformation",
                                        PTuple
                                          [PAttribute (PVar "_HeapHandle",[]);
                                           PAttribute (PVar "_HeapInformationClass",[]);
                                           PAttribute (PVar "_HeapInformation",[]);
                                           PAttribute (PVar "_HeapInformationLength",[])]),
                                     Typed
                                       (App
                                          (Var "failwith",
                                           Lit (String "extern was not given a DllImport attribute")),
                                        TApp (LongIdent [Ident "bool"],[])))],Lit Unit)]] ast

    [<Test>]
    member this.``Unsigned Integer 32``() =
        AssertAreEqual [Let (false,[(PVar "x", Lit (UnsignedInteger 0u))],Lit Unit)] (parse "let x = 0u")

    [<Test>]
    member this.``Pattern type switching``() =
        let ast = parse ("let f x =  match (x:System.Object) with \n" +
                         "           | :? System.Boolean -> 42")
        AssertAreEqual [Let (false,
                             [(PApp (PVar "f",PVar "x"),
                               Match
                                 (Typed (Var "x",LongIdent [Ident "System"; Ident "Object"]),
                                  [Clause
                                     (PIsInst (LongIdent [Ident "System"; Ident "Boolean"]),
                                      Lit (Integer 42))]))],Lit Unit)] ast

    [<Test>]
    member this.``assert keyword``() =
        AssertAreEqual [Let (false, [(PWild,  Assert (App (App (Var "op_LessThanOrEqual",Var "pos_nbits"),Lit (Integer 32))))], Lit Unit)]
                       (parse "let _ = assert (pos_nbits <= 32)")

    [<Test>]
    member this.``Val keyword``() =
        let ast = parseTypes ("type MyType() =    \n" +
                              "    [<DefaultValue>] val mutable myInt2 : int")
        AssertAreEqual [[Class ("MyType",[ImplicitCtor []; ValField (Some (Ident "myInt2"), LongIdent [Ident "int"])])]] ast

    [<Test>]
    member this.``While loop``() =
        let ast = parse ("while x > 0 do  \n" +
                         "   foo x ")
        AssertAreEqual [While (App (App (Var "op_GreaterThan",Var "x"),Lit (Integer 0)), App (Var "foo",Var "x"))] ast

    [<Test>]
    member this.``Lazy keyword`` () =
        AssertAreEqual [Let (false,[(PVar "f", Lazy (Lit (Integer 42)))],Lit Unit)] (parse "let f = lazy 42")

    [<Test>]
    member this.``Support for Int16 UInt16 Byte``() =
        AssertAreEqual [Let (false,[(PVar "x", Lit (UnsignedInteger16 0us))],Lit Unit)] (parse "let x = 0us")
        AssertAreEqual [Let (false,[(PVar "x", Lit (Integer16  0s))],Lit Unit)] (parse "let x = 0s")
        AssertAreEqual [Let (false,[(PVar "x", Lit (Byte 0uy))],Lit Unit)] (parse "let x = 0uy")

    [<Test>]
    member this.``Enum support``() =
        let ast = parseTypes ("type Choice = \n" +
                              "   | Yes = 0    \n" +
                              "   | No  = 1      ")
        AssertAreEqual [[Enum ("Choice",[("Yes", Integer 0); ("No", Integer 1)])]] ast

    [<Test>]
    member this.``inferred downcast`` () =
        let ast = parse "let x:string = downcast foo()"
        AssertAreEqual [Let  (false,  [(PVar "x",  Typed (InferredDowncast (App (Var "foo",Lit Unit)),LongIdent [Ident "string"]))], Lit Unit)] ast
        
    [<Test>]
    member this.``Quoted identifier``() =        
        AssertAreEqual [Let(false,[PVar "x'", Lit(Integer 42)], Lit(Unit))] (parse "let x' = 42")

    [<Test>]
    member this.``Inheriting a type``() =
        let ast = parseTypes ("type IPartialEqualityComparer<'T> = inherit IEqualityComparer<'T>")
        AssertAreEqual [[Class ("IPartialEqualityComparer", [Inherit (TApp (LongIdent [Ident "IEqualityComparer"],[TVar (Ident "T")]), Option.None)])]] ast
                       


