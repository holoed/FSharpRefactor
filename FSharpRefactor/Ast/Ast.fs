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

type PrimitiveType
    = Integer
    | String
    | Bool

type Type
    = TyInfixApp of Type * string * Type
    | TyApp of Type * Type
    | TyLam of Type * Type
    | TyLet of Type * Type * Type
    | TyVar  of string
    | TyLit  of PrimitiveType

type Literal
   = Char  of  char          // character literal
   | String of string        // string literal
   | Integer of  int         // integer literal
   | Float  of  float        // floating point literal    

type Pat =
    | PVar of string
    | PApp of Pat * Pat
    | PLit of Literal    

type Exp 
    = Var      of String               // variable    
    | Lam      of String * Exp         // lambda abstraction
    | App      of Exp * Exp            // application    
    | InfixApp of Exp * String * Exp   // infix application
    | Let      of Pat * Exp * Exp      // local definition
    
    | Lit      of Literal              // literal 
    | WithTy   of Exp * Type


