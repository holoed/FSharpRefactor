module ContinuationMonad

type ContinuationBuilder() = 
    member this.Return(x) = (fun k -> k x) 
    member this.ReturnFrom(x) = x 
    member this.Bind(m,f) = (fun k -> m (fun a -> f a k)) 
    member this.Delay(f) = f() 
let cont = new ContinuationBuilder()

let mmap f xs = 
           let rec MMap' (f, xs', out) = 
               cont {
                       match xs' with
                       | h :: t -> let! h' = f(h)
                                   return! MMap'(f, t, List.append out [h'])
                       | [] -> return out
                    }
           MMap' (f, xs, [])