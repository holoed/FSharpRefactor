// Learn more about F# at http://fsharp.net

module CompilerToAstTests

open NUnit.Framework
open Ast
open CompilerToAst
open System.IO
open AstCatamorphisms

let path = sprintf "%s\\%s" (Directory.GetCurrentDirectory()) "test.fs" 

let stripPos exp =  let foldPat p = foldPat (fun (s,l) -> PVar s) (fun l r -> PApp(l,r)) (fun x -> PLit x) p
                    foldExp (fun (s, l) -> Var s) 
                            (fun ps b -> Lam(List.map (fun p -> foldPat p) ps, b)) 
                            (fun x y -> App (x, y))
                            (fun p e1 e2 -> Let (foldPat p, e1, e2))
                            (fun x -> Lit x)
                            (fun e t -> WithTy (e,t))
                     exp
let stripAllPos exps = List.map (fun exp -> stripPos exp) exps


let parseWithPos s = 
        File.WriteAllText("test.fs", s)        
        let [xs:_] = parseToAst [path]
        xs 

let parse s = s |> parseWithPos |> stripAllPos

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
