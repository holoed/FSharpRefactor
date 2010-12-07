module ASTAnalysis

open Ast
open AstCatamorphisms

//let findPos exp pos =  
//        let foldPat p = foldPat (fun (s,l) -> if (l = pos) then [Var (s,l)] else []) (fun l r -> l @ r) (fun x -> []) p
//        foldExp (fun (s, l) -> if (l = pos) then [Var (s,l)] else [])
//                (fun ps b -> (List.concat (List.map (fun p -> foldPat p) ps)) @ b)
//                (fun x y -> x @ y)
//                (fun p e1 e2 -> (foldPat p) @ e1 @ e2)
//                (fun x -> [])
//                (fun e t -> e)
//         exp

let rec findIdents exp w =
    let foldPat p = foldPat (fun (s,l) -> if (s = w) then [Var (s,l)] else []) (fun l r -> l @ r) (fun x -> []) p
    match exp with
    | Var (s,l)  -> if (s = w) then [exp] else []
    | Lam (ps,b) -> (List.concat (List.map (fun p -> foldPat p) ps)) @ (findIdents b w)
    | App (l,r)  -> (findIdents l w) @ (findIdents r w)
    | Let (p, e1, e2) -> if (List.isEmpty (foldPat p)) then 
                             (findIdents e1 w) @ (findIdents e2 w) 
                            else []   
    | Lit _ -> []
    | WithTy (e, t) -> (findIdents e w)

let rec findPos exp pos =
    let foldPat p = foldPat (fun (s,l) -> if (l = pos) then [Var(s,l)] else []) (fun l r -> l @ r) (fun x -> []) p
    match exp with
    | Var (s,l)  -> if (l = pos) then [exp] else []
    | Lam (ps,b) -> (List.concat (List.map (fun p -> foldPat p) ps)) @ (findPos b pos)
    | App (l,r)  -> (findPos l pos) @ (findPos r pos)
    | Let (p, e1, e2) -> let xs = foldPat p
                         if (List.isEmpty xs) then (findPos e1 pos) @ (findPos e2 pos)
                         else
                            xs @ List.concat (List.map (fun (Var (s,l)) -> (findIdents e1 s) @ (findIdents e2 s)) xs)
    | Lit _ -> []
    | WithTy (e, t) -> (findPos e pos)
    

let findAllReferences exps pos = 
        List.fold (fun acc exp -> acc @ (findPos exp pos)) [] exps