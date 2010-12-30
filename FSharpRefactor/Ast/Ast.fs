﻿// * **********************************************************************************************
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
   | Double of double 
   | Bool of bool
   | Unit

type SrcCol = { startColumn : int; endColumn : int }

type SrcLine = { startLine : int; endLine : int }

type SrcLoc = { srcFilename : string; srcLine : SrcLine; srcColumn : SrcCol }     

type Pat<'a> =
    | PVar of 'a
    | PApp of Pat<'a> * Pat<'a>
    | PLit of Literal    
    | PTuple of Pat<'a> list
    | PWild
    | PList of Pat<'a> list
    | PThis
    | PLongVar of Pat<'a> list

type IsLetRec = bool

type Exp<'a> 
    = Var      of 'a                           // variable    
    | LongVar of Exp<'a> list
    | Lam      of Pat<'a> list * Exp<'a>       // lambda abstraction
    | App      of Exp<'a> * Exp<'a>            // application    
    | Let      of IsLetRec * Pat<'a> * Exp<'a> * Exp<'a>  // local definition    
    | Lit      of Literal                      // literal 
    | Tuple    of Exp<'a> list
    | List     of Exp<'a> list
    | Match    of Exp<'a> * Clause<'a> list
    | ForEach  of Pat<'a> * Exp<'a> * Exp<'a>
    | YieldOrReturn of Exp<'a>
    | IfThenElse of Exp<'a> * Exp<'a> * Exp<'a> option
    | DotIndexedSet of Exp<'a> * Exp<'a> list * Exp<'a>
    | DotIndexedGet of Exp<'a> * Exp<'a> list
    | Record of ('a * Exp<'a>) list
    | ArbitraryAfterError

and Clause<'a> = Clause of Pat<'a> * Exp<'a>

type TypeDef<'a> 
    = DisUnion of string * 'a list
    | Record of string * 'a option list
    | None of string
    | Class of string * ClassMember<'a> list

and ClassMember<'a>
    = ImplicitCtor of Pat<'a> list
    | Member of Pat<'a> * Exp<'a>
    | LetBindings of Exp<'a> list

type Module<'a>
    = Exp of Exp<'a> list
    | Types of TypeDef<'a> list
    | NestedModule of string list * Module<'a> list
    | Open of string list
    

