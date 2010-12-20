// * **********************************************************************************************
// * Copyright (c) Edmondo Pentangelo. 
// *
// * This source code is subject to terms and conditions of the Microsoft Public License. 
// * A copy of the license can be found in the License.html file at the root of this distribution. 
// * By using this source code in any fashion, you are agreeing to be bound by the terms of the 
// * Microsoft Public License.
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

let stripPos decl =  let foldPat p = foldPat (fun (s,l) -> PVar s) (fun l r -> PApp(l,r)) (fun x -> PLit x) (fun xs -> PTuple xs) (fun () -> PWild) p
                     foldExp (fun (s, l) -> Var s) 
                             (fun ps b -> Lam(List.map (fun p -> foldPat p) ps, b)) 
                             (fun x y -> App (x, y))
                             (fun p e1 e2 -> Let (foldPat p, e1, e2))
                             (fun x -> Lit x)
                             (fun xs -> Tuple xs)
                             (fun xs -> List xs)
                             (fun x -> Exp x)
                             (fun xs -> Types xs)
                             (fun name cases -> DisUnion (name, List.map (fun (s,l) -> s) cases))
                             (fun e cs -> Match(e, cs))
                             (fun p e -> Clause(foldPat p, e))
                             decl
let stripAllPos exps = List.map (fun exp -> stripPos exp) exps


let parseWithPosDecl s = 
        File.WriteAllText("test.fs", s)        
        let [xs:_] = parseToAst [path]
        xs 

let parse s = s |> parseWithPosDecl |> stripAllPos |> List.map (fun (Exp x) -> x)

let parseWithPos s = s |> parseWithPosDecl |> List.map (fun (Exp x) -> x)

let parseTypes s = s |> parseWithPosDecl |> stripAllPos |> List.map (fun (Types xs) -> xs)

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
        Assert.IsTrue ([Let(PVar "x", Lit(Integer 42), Lit(Unit))] = parse "let x = 42")
        Assert.IsTrue ([Let(PVar "x", Lit(Integer 42), Lit(Unit)); Let(PVar "x", Lit(Integer 24), Lit(Unit))] = parse "let x = 42\nlet x = 24")

    [<Test>]
    member this.FunctionsDecls() =        
        Assert.IsTrue ([Let(PApp(PVar "f", PVar "x"), Var "x", Lit(Unit)) ] = parse "let f x = x")
        Assert.IsTrue ([Let(PApp(PApp(PVar "f", PVar "x"), PVar "y"), Var "y", Lit(Unit)) ] = parse "let f x y = y")        
        Assert.IsTrue ([Let(PApp(PApp(PApp(PVar "f", PVar "x"), PVar "y"), PVar "z"), Var "z", Lit(Unit)) ] = parse "let f x y z = z")
   
    [<Test>]
    member this.NestedDecls() =
     
       let ret = parse ("let x = let y = 12  \n" +
                        "        let z = 21  \n" +       
                        "        z" )
       Assert.IsTrue ([Let(PVar "x", Let (PVar "y",Lit (Integer 12),Let (PVar "z",Lit (Integer 21),Var "z")),  Lit Unit)] = ret)
        
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
        Assert.IsTrue ([(Let (PVar "squares", App (App(Var "map", Var "square"), Var "numbers"),  Lit Unit))] = ret)

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
        let k = [Let (PVar "z", 
                                Let(PVar "x", 
                                    Lit(Integer 12), 
                                    Let(PVar "y", 
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
                   (PApp (PApp (PVar "computeDerivative",PVar "f"),PVar "x"),
                    Let
                      (PVar "p1",
                       App (Var "f",App (App (Var "op_Subtraction",Var "x"),Lit (Float 0.05))),
                       Let
                         (PVar "p2",
                          App (Var "f",App (App (Var "op_Addition",Var "x"),Lit (Float 0.05))),
                          App
                            (App
                               (Var "op_Division",
                                App (App (Var "op_Subtraction",Var "p2"),Var "p1")),
                             Lit (Float 0.1)))),Lit Unit)]

        Assert.IsTrue ( (x = y) )

    [<Test>]
    member this.SimpleDeclsWithPos() =        
        AssertAreEqual [Let(PVar ("x", loc (4,5,1,1)), Lit(Integer 42), Lit(Unit))]  (parseWithPos "let x = 42")
        AssertAreEqual [Let(PVar ("x", loc (4,5,1,1)), Lit(Integer 42), Lit(Unit)); Let(PVar ("x", loc (4,5,2,2)), Lit(Integer 24), Lit(Unit))] (parseWithPos "let x = 42\nlet x = 24")

       
    [<Test>]
    member this.FunctionsDeclsWithPos() =        
        AssertAreEqual [Let(PApp(PVar ("f", loc (4,5,1,1)), PVar ("x", loc (6,7,1,1))), Var ("x", loc (10,11,1,1)), Lit(Unit)) ]  (parseWithPos "let f x = x")
        AssertAreEqual [Let(PApp(PApp(PVar ("f", loc (4,5,1,1)), PVar ("x", loc (6,7,1,1))), PVar ("y", loc (8,9,1,1))), Var ("y", loc (12,13,1,1)), Lit(Unit)) ] (parseWithPos "let f x y = y")        
        AssertAreEqual [Let(PApp(PApp(PApp(PVar ("f", loc (4,5,1,1)), PVar ("x", loc (6,7,1,1))), PVar ("y", loc (8,9,1,1))), PVar ("z", loc (10,11,1,1))), Var ("z", loc (14,15,1,1)), Lit(Unit)) ] (parseWithPos "let f x y z = z")
        
    [<Test>]
    member this.OffSideLocalDefinitionWithPos() =
        
        let x = parseWithPos ("let computeDerivative f x = \n" +
                              "    let p1 = f (x - 0.05)   \n" +
                              "    let p2 = f (x + 0.05)   \n" +
                              "    (p2 - p1) / 0.1           ")
                           
        let y = [Let
                   (PApp (PApp (PVar ("computeDerivative", loc (4,21,1,1)),PVar ("f", loc (22,23,1,1))),PVar ("x", loc (24,25,1,1))),
                    Let
                      (PVar ("p1", loc (8,10,2,2)),
                       App (Var ("f", loc (13,14,2,2)),App (App (Var ("op_Subtraction", loc (18,19,2,2)),Var ("x", loc (16,17,2,2))),Lit (Float 0.05))),
                       Let
                         (PVar ("p2", loc (8,10,3,3)),
                          App (Var ("f", loc (13,14,3,3)),App (App (Var ("op_Addition", loc (18,19,3,3)),Var ("x", loc (16,17,3,3))),Lit (Float 0.05))),
                          App
                            (App
                               (Var ("op_Division", loc (14,15,4,4)),
                                App (App (Var ("op_Subtraction", loc (8,9,4,4)),Var ("p2", loc (5,7,4,4))),Var ("p1", loc (10,12,4,4)))),
                             Lit (Float 0.1)))),Lit Unit)]

        AssertAreEqual y x

    [<Test>]
    member this.Tuples() = 
       AssertAreEqual [Let(PVar "x", Tuple [Lit(Integer 42);Lit(Integer 24)], Lit(Unit))]  (parse "let x = (42, 24)")
       AssertAreEqual [Let(PVar "x", Tuple [Lit(Integer 42);Tuple [Lit(String "Hello"); Var "y"]], Lit(Unit))]  (parse "let x = (42, (\"Hello\", y))")

    [<Test>]
    member this.Lists() = 
       AssertAreEqual [Let(PVar "x", List [Lit(Integer 42);Lit(Integer 24)], Lit(Unit))]  (parse "let x = [42; 24]")
       AssertAreEqual [Let(PVar "x", List [Lit(Integer 42); Var "y"], Lit(Unit))]  (parse "let x = [42; y]")
       AssertAreEqual [Let(PVar "x", List [Lit(Integer 42); List[Var "y"; Var "z"]], Lit(Unit))]  (parse "let x = [42; y; z]")

    [<Test>]
    member this.OptionType() =
        AssertAreEqual [Let(PVar "x", App (Var "Some", Lit(Integer 42)), Lit(Unit))]  (parse "let x = Some 42")
        AssertAreEqual [Let(PVar "x", Var "None", Lit(Unit))]  (parse "let x = None")
         
    [<Test>]
    member this.DiscriminatedUnion() =
        let ast = parseTypes "type Exp = Var of string" |> List.concat
        AssertAreEqual [DisUnion("Exp", ["Var"])]  ast

    [<Test>]
    member this.TuplePattern() =
        AssertAreEqual [Let(PApp(PVar "f", PTuple [PVar "x"; PVar "y"]), Var "x", Lit(Unit))]  (parse "let f (x,y) = x")

    [<Test>]
    member this.WildPattern() =
        AssertAreEqual [Let(PWild, Var "x", Lit(Unit))]  (parse "let _ = x")

    [<Test>]
    member this.SimplePatternMatching() =
        AssertAreEqual [Let(PApp(PVar "f", PVar "x"), Match(Var "x", [Clause(PLit(Bool(true)), Lit(Integer 42))]), Lit(Unit))]  (parse "let f x = match x with True -> 42")

    [<Test>]
    member this.SimplePatternMatchingWithTuplePattern() =
        AssertAreEqual [Let(PApp(PVar "f", PVar "p"), Match(Var "p", [Clause(PTuple [PVar "x"; PVar "y"], Var "x")]), Lit(Unit))]  (parse "let f p = match p with (x,y) -> x")
