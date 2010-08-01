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

type Literal
   = Char  of  char          // character literal
   | String of string        // string literal
   | Integer of  int         // integer literal
   | Float  of  float        // floating point literal    

type Pat =
    | PVar of string
    | PLit of Literal
    | PApp of Pat * Pat

type Exp = InfixApp of Exp * String * Exp        // infix application
            | App of Exp * Exp                   // application
            | Lam of String * Exp                // lambda abstraction
            | Let of Pat * Exp * Exp             // local definition
            | Var of String                      // variable
            | Lit of Literal                     // literal 