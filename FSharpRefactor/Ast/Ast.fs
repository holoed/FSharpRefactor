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
let tyDouble = TyCon("double", [])
let tyFloat = TyCon("float", [])
let tyChar = TyCon("char", [])
let tyString = TyCon("string", [])
let tyNum = TyVar "num"

type Literal
   = Char  of  char          // character literal
   | String of string        // string literal
   | Integer of  int         // integer literal
   | Float  of  float        // floating point literal   
   | Double of double 
   | Unit

type SrcCol = { startColumn : int; endColumn : int }

type SrcLine = { startLine : int; endLine : int }

type SrcLoc = { srcFilename : string; srcLine : SrcLine; srcColumn : SrcCol }     

type Pat<'a> =
    | PVar of 'a
    | PApp of Pat<'a> * Pat<'a>
    | PLit of Literal    
    | PWithTy of Pat<'a> * Type

type Exp<'a> 
    = Var      of 'a                           // variable    
    | Lam      of Pat<'a> list * Exp<'a>       // lambda abstraction
    | App      of Exp<'a> * Exp<'a>            // application    
    | InfixApp of Exp<'a> * String * Exp<'a>   // infix application
    | Let      of Pat<'a> * Exp<'a> * Exp<'a>  // local definition    
    | Lit      of Literal                      // literal 
    | WithTy   of Exp<'a> * Type


