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


let mfold f acc xs =
          let rec Mfold' (f, xs', acc') = 
               cont {
                       match xs' with
                       | h :: t -> let! h' = f h acc'
                                   return! Mfold'(f, t, h')
                       | [] -> return acc'
                    }
          Mfold' (f, xs, acc)