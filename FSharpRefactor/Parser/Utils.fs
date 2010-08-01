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

module Utils

open System

let (|Empty|Cons|) xs : Choice<Unit, 'a * seq<'a>> =
     if (Seq.isEmpty xs) then 
        Empty 
      else 
        Cons(Seq.head xs, Seq.skip 1 xs)

let cons x xs = seq { yield x
                      yield! xs }

let seqtostring xs = new System.String(Seq.toArray xs) 