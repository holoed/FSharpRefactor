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


module PurelyFunctionalDataStructures

exception Empty

type Stack<'a> = Stack of 'a list

let empty = Stack []

let push (Stack xs) x = Stack(x::xs)

let pop (Stack xs) = match xs with
                     | x::xs -> (x, Stack(xs))
                     | [] -> raise Empty

let isEmpty (Stack xs) = List.isEmpty xs

let stackToList (Stack xs) = xs

