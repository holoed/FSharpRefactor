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

module StateMonad

//F# State Monad

type State<'state, 'a> = State of ('state ->'a * 'state)

type StateMonad() = 
        member b.Bind(State m, f) = State (fun s -> let (v,s') = m s in
                                                    let (State n) = f v in n s')                                                    
        member b.Return(x) = State (fun s -> x, s)

let state = StateMonad()

let getState = State (fun s -> s, s)
let setState s = State (fun _ -> (), s)  

let execute m s = match m with
                  | State f -> let r = f s
                               match r with
                               |(x,_) -> x