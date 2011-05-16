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

module CompilerToAstTyped

open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.Build
open Microsoft.FSharp.Compiler.Lexhelp
open Microsoft.FSharp.Compiler.ErrorLogger
open Microsoft.FSharp.Compiler.Driver
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.Range
open System.IO

let internal parseToTypedAstInternal sourceFiles =
   
    let tcConfigB = Build.TcConfigBuilder.CreateNew(defaultFSharpBinariesDir, false,Directory.GetCurrentDirectory())  

    // Now install a delayed logger to hold all errors from flags until after all flags have been parsed (for example, --vserrors)
    let delayForFlagsLogger = DelayAndForwardErrorLogger()
    let _unwindEL_1 = PushErrorLoggerPhaseUntilUnwind (fun _ -> delayForFlagsLogger)          
    
    // Share intern'd strings across all lexing/parsing
    let lexResourceManager = new Lexhelp.LexResourceManager() 
      
    tcConfigB.conditionalCompilationDefines <- "COMPILED" :: tcConfigB.conditionalCompilationDefines 

    // Create tcGlobals and frameworkTcImports
    let outfile,pdbfile,assemblyName = 
        try 
            tcConfigB.DecideNames sourceFiles 
        with e ->
            errorRecovery e rangeStartup
            delayForFlagsLogger.ForwardDelayedErrorsAndWarnings(tcConfigB)
            exiter.Exit 1 
                    
    // DecideNames may give "no inputs" error. Abort on error at this point. bug://3911
    if delayForFlagsLogger.ErrorCount > 0 then 
        delayForFlagsLogger.ForwardDelayedErrorsAndWarnings(tcConfigB)
        exiter.Exit 1
    
    // If there's a problem building TcConfig, abort    
    let tcConfig = 
        try
            TcConfig.Create(tcConfigB,validate=false)
        with e ->
            delayForFlagsLogger.ForwardDelayedErrorsAndWarnings(tcConfigB)
            exiter.Exit 1
    
    let errorLogger = ErrorLoggerThatQuitsAfterMaxErrors tcConfigB

    // Install the global error logger and never remove it. This logger does have all command-line flags considered.
    let _unwindEL_2 = PushErrorLoggerPhaseUntilUnwind (fun _ -> errorLogger)
    
    // Forward all errors from flags
    delayForFlagsLogger.ForwardDelayedErrorsAndWarnings(errorLogger)

    (* step - decideNames *)  
    abortOnError errorLogger;

    // Nice name generator    
    let niceNameGen = NiceNameGenerator()
                         
    let foundationalTcConfigP = TcConfigProvider.Constant(tcConfig)
    let sysRes,otherRes,_ = TcAssemblyResolutions.SplitNonFoundationalResolutions tcConfig
    let tcGlobals,frameworkTcImports = TcImports.BuildFrameworkTcImports (foundationalTcConfigP,sysRes,otherRes)

    (* step - parse sourceFiles *)
    use unwindParsePhase = PushThreadBuildPhaseUntilUnwind (BuildPhase.Parse)            
    let inputs =
        try  
            sourceFiles 
            |> tcConfig.ComputeCanContainEntryPoint 
            |> List.zip sourceFiles
            |> List.choose (fun (input,isLastCompiland) -> 
                    ParseOneInputFile(tcConfig,lexResourceManager,["COMPILED"],input,isLastCompiland,errorLogger,(*retryLocked*)false)) 
        with e -> 
            errorRecoveryNoRange e; exiter.Exit 1
    if tcConfig.parseOnly then exiter.Exit 0 else ();
    abortOnError errorLogger;

    if tcConfig.printAst then                
        inputs |> List.iter (fun input -> printf "AST:\n"; printfn "%+A" input; printf "\n") 

    let tcConfig = (tcConfig,inputs) ||> List.fold ApplyMetaCommandsFromInputToTcConfig 
    let tcConfigP = TcConfigProvider.Constant(tcConfig)

    let tcGlobals,tcImports =  
        let tcImports = TcImports.BuildNonFrameworkTcImports(tcConfigP,tcGlobals,frameworkTcImports,otherRes)
        tcGlobals,tcImports
    abortOnError errorLogger;

    use unwindParsePhase = PushThreadBuildPhaseUntilUnwind (BuildPhase.TypeCheck)            
    let tcEnv0 = GetInitialTypecheckerEnv (Some assemblyName) rangeStartup tcConfig tcImports tcGlobals

    // typecheck 
    let tcState,topAttrs,typedAssembly,_tcEnvAtEnd = 
        TypeCheck(tcConfig,tcImports,tcGlobals,errorLogger,assemblyName,niceNameGen,tcEnv0,inputs)

    let generatedCcu = tcState.Ccu
    abortOnError errorLogger;

    typedAssembly


let parseToTypedAst sourceFiles =     
    sourceFiles 
    |> parseToTypedAstInternal 
    |> TypedAssemblyToAst.foldDecls

