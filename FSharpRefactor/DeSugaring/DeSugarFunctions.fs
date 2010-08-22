module DeSugarFunctions

open Ast

let rec desugarPat pat = 
    match pat with
    | PApp(PVar f, PVar x) -> (PVar f, fun body -> Lam(x, body))
    | PApp(e1, PVar x) -> let (f, g) = desugarPat e1
                          (f, fun body -> g (Lam(x, body)))

let rec desugar exp = 
    match exp with
    | Let(pat, body, x) ->  let (f, g) = desugarPat pat
                            Let(f, g body, x)
    | _ -> exp
