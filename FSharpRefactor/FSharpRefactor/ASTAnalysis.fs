module ASTAnalysis

open Ast
open AstCatamorphisms

let findPos exp pos =  
        let foldPat p = foldPat (fun (s,l) -> if (l = pos) then [Var (s,l)] else []) (fun l r -> l @ r) (fun x -> []) p
        foldExp (fun (s, l) -> if (l = pos) then [Var (s,l)] else [])
                (fun ps b -> (List.concat (List.map (fun p -> foldPat p) ps)) @ b)
                (fun x y -> x @ y)
                (fun op x y -> x @ y)
                (fun p e1 e2 -> (foldPat p) @ e1 @ e2)
                (fun x -> [])
                (fun e t -> e)
         exp


let findAllReferences exps pos = 
        List.fold (fun acc exp -> acc @ (findPos exp pos)) [] exps