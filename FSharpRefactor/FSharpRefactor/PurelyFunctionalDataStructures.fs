module PurelyFunctionalDataStructures

exception Empty

type Stack<'a> = Stack of 'a list

let empty = Stack []

let push (Stack xs) x = Stack(x::xs)

let pop (Stack xs) = match xs with
                     | x::xs -> (x, Stack(xs))
                     | [] -> raise Empty

