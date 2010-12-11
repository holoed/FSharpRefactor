module PurelyFunctionalDataStructures

exception Empty
exception Subscript

type 'a Stack = Nil | Cons of 'a * 'a Stack

let empty = Nil

let isEmpty = function
    | Nil -> true
    | _ -> false

let head = function
    | Nil -> raise Empty
    | Cons(x, s) -> x
    
let tail = function
    | Nil -> raise Empty
    | Cons(x, s) -> s

let rec (-||-) xs ys =
    if isEmpty xs then
        ys
    else
        Cons(head xs, tail xs -||- ys)

let rec update = function
    | Nil, i, y -> raise Subscript
    | xs, 0, y -> Cons(y, tail(xs))
    | xs, i, y -> Cons(head(xs), update(tail(xs), i-1, y))