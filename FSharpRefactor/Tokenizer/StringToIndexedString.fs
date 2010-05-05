module StringToIndexedString

open System.Linq

let repeat x = Seq.initInfinite (fun _ -> x)

let scan = Seq.scan

let iterate f x = scan (fun l _ -> f l) x (repeat x)

let any = Enumerable.Any

let takeWhile p = fun xs -> Enumerable.TakeWhile (xs, new System.Func<'a, bool>(p))

let skipWhile p = fun xs -> Enumerable.SkipWhile (xs, new System.Func<'a, bool>(p))

let skip n = fun xs -> Enumerable.Skip (xs, n)

let unfold h p t =  iterate t >> Seq.takeWhile p >> Seq.map h 

let splitBy n = unfold (takeWhile n) any (skipWhile n >> skip 1)

let naturals = Seq.unfold (fun x -> Some(x, x + 1)) 0

let lines s = (splitBy (fun x -> x <> '\n') s)  
              |> Seq.map (fun xs -> xs 
                                       |> Seq.zip naturals)
let indexStr s = (lines s) |> Seq.zip naturals 
                           |> Seq.map (fun (r, xs) -> xs |> Seq.map (fun (c, x) -> (r, c, x)))
                           |> Seq.concat
    