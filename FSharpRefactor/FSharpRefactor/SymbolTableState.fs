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

module SymbolTableState

open StateMonad

let insert s l = state { let! t = getState
                         do! setState (SymbolTable.insert s l t) }

let addRef s l = state { let! t = getState
                         do! setState (SymbolTable.addRef s l t) }

let enter_scope = state { let! t = getState
                          do! setState (SymbolTable.enter_scope t) }

let exit_scope = state { let! t = getState
                         do! setState (Option.get (SymbolTable.exit_scope t)) }
