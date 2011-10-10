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

module StateMonad

open System.Diagnostics

//F# State Monad

type State<'state, 'a> = State of ('state ->'a * 'state)

let run (State f) s = f s

type StateMonad() = 
       [<DebuggerHidden>]
       member b.Bind(State m, f) = State (fun s -> let (v,s') = m s in let (State n) = f v in n s')                                                    
       [<DebuggerHidden>]
       member b.Return x = State (fun s -> x, s)
       [<DebuggerHidden>]
       member b.ReturnFrom x = x
       [<DebuggerHidden>]
       member b.Zero () = State (fun s -> (), s)
       [<DebuggerHidden>]
       member b.Combine(r1, r2) = b.Bind(r1, fun () -> r2)
       [<DebuggerHidden>]
       member b.Delay f = State (fun s -> run (f()) s)

let state = StateMonad()

let getState = State (fun s -> s, s)
let setState s = State (fun _ -> (), s)  

let execute (State f) s = let (x,_) = f s in x

let executeGetState (State f) s = let (_, s) = f s in s

let mmap f xs = 
           let rec MMap' (f, xs', out) = 
               state {
                       match xs' with
                       | h :: t -> let! h' = f(h)
                                   return! MMap'(f, t, List.append out [h'])
                       | [] -> return out
                     }
           MMap' (f, xs, [])

let stateId x = state { return! x }

let mmapId xs = mmap stateId xs

// (a -> b) -> m a -> m b
let inline liftM f m = state.Bind (m, (fun a -> state.Return (f a)))

// (a -> b -> c) -> m a -> m b -> m c
let inline liftM2 f ma mb = state.Bind (ma, (fun a -> state.Bind(mb, (fun b -> state.Return (f a b)))))

// (a -> b -> c -> d) -> m a -> m b -> m c -> md
let inline liftM3 f ma mb mc = state.Bind (ma, (fun a -> state.Bind(mb, (fun b -> state.Bind(mc, fun c -> state.Return (f a b c))))))

