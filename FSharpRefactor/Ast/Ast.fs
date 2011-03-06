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

type IsLetRec = bool

type Pat<'a> =
    | PVar of 'a
    | PApp of Pat<'a> * Pat<'a>
    | PLit of Literal    
    | PTuple of Pat<'a> list
    | PRecord of (string * Pat<'a>) list
    | PWild
    | PList of Pat<'a> list
    | PThis
    | PLongVar of Pat<'a> list
    | PIsInst of Type<'a>

and Exp<'a> 
    = Var      of 'a                           // variable    
    | LongVar of Exp<'a> list
    | LongVarSet of Exp<'a> * Exp<'a>
    | Lam      of Pat<'a> list * Exp<'a>       // lambda abstraction
    | App      of Exp<'a> * Exp<'a>            // application    
    | Let      of IsLetRec * (Pat<'a> * Exp<'a>) list * Exp<'a>  // local definition    
    | LetBang  of Pat<'a> * Exp<'a> * Exp<'a>
    | Lit      of Literal                      // literal 
    | Tuple    of Exp<'a> list
    | List     of Exp<'a> list
    | Match    of Exp<'a> * Clause<'a> list
    | ForEach  of Pat<'a> * Exp<'a> * Exp<'a>
    | TypeApp  of Exp<'a> * Type<'a> list
    | YieldOrReturn of Exp<'a>
    | YieldOrReturnFrom of Exp<'a>
    | IfThenElse of Exp<'a> * Exp<'a> * Exp<'a> option
    | DotGet of Exp<'a> * Exp<'a>
    | DotIndexedSet of Exp<'a> * Exp<'a> list * Exp<'a>
    | DotIndexedGet of Exp<'a> * Exp<'a> list
    | Record of ('a * Exp<'a>) list
    | New of Type<'a> * Exp<'a>
    | ObjExpr of ClassMember<'a> list
    | Do of Exp<'a>
    | DoBang of Exp<'a>
    | Downcast of Exp<'a> * Type<'a>
    | Upcast of Exp<'a> * Type<'a>
    | TryWith of Exp<'a> * Clause<'a> list
    | Typed of Exp<'a> * Type<'a>
    | ArbitraryAfterError

and Clause<'a> = Clause of Pat<'a> * Exp<'a>

and ExceptionDef<'a> = ExceptionDef of string * ClassMember<'a> list    

and TypeDef<'a> 
    = DisUnion of string * 'a list
    | Record of string * 'a option list * ClassMember<'a> list
    | None of string
    | Class of string * ClassMember<'a> list
    | Abbrev of string * Type<'a>

and Type<'a> 
    = Ident of 'a
    | LongIdent of Type<'a> list
    | TFun of Type<'a> * Type<'a>
    | TVar of Type<'a>
    | TApp of Type<'a> * Type<'a> list
    | TTuple of Type<'a> list
    | TArray of int * Type<'a>
    | TAnon

and ClassMember<'a>
    = ImplicitCtor of Pat<'a> list
    | Member of Pat<'a> * Exp<'a>
    | LetBindings of Exp<'a> list
    | AbstractSlot of string
    | Interface of Type<'a> * ClassMember<'a> list

type Module<'a>
    = Exp of Exp<'a> list
    | Types of TypeDef<'a> list
    | NestedModule of string list * Module<'a> list
    | Open of string list
    | Exception of ExceptionDef<'a>
    

