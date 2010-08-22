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

module Ast

open System

type Type
    = TyLam of Type * Type
    | TyVar of string
    | TyCon of string * Type list
    with override this.ToString() = 
            match this with
            | TyLam (t1, t2) -> sprintf "(%s -> %s)" (t1.ToString()) (t2.ToString())
            | TyVar a -> a
            | TyCon (s, ts) -> s

let tyInteger = TyCon("int", [])
let tyFloat = TyCon("float", [])
let tyChar = TyCon("char", [])
let tyString = TyCon("string", [])

type Literal
   = Char  of  char          // character literal
   | String of string        // string literal
   | Integer of  int         // integer literal
   | Float  of  float        // floating point literal    

type Pat =
    | PVar of string
    | PApp of Pat * Pat
    | PLit of Literal    
    | PWithTy of Pat * Type

type Exp 
    = Var      of String               // variable    
    | Lam      of String * Exp         // lambda abstraction
    | App      of Exp * Exp            // application    
    | InfixApp of Exp * String * Exp   // infix application
    | Let      of Pat * Exp * Exp      // local definition    
    | Lit      of Literal              // literal 
    | WithTy   of Exp * Type


