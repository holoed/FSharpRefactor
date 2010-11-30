// Learn more about F# at http://fsharp.net

module CompilerToAstTests

open NUnit.Framework
open Ast
open CompilerToAst
open System.IO

let parse s = 
        File.WriteAllText("test.fs", s)
        let path = sprintf "%s\\%s" (Directory.GetCurrentDirectory()) "test.fs" 
        let [xs:_] = parseToAst [path]
        xs

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

       
        
