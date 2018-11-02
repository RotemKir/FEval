﻿namespace FEval

module Inspectors =
    open Microsoft.FSharp.Quotations
    open Microsoft.FSharp.Quotations.Patterns
    open Microsoft.FSharp.Reflection
    open System
    open System.IO
    open System.Reflection

    type InpectionStage = Pre | Post

    type PerformanceInspectorConfig =
        {
            HandleMessage : string -> unit
            PreMessageFormatter : DateTime -> Expr -> EvaluationState -> string
            PostMessageFormatter : DateTime -> Expr -> EvaluationState -> TimeSpan -> string
        }

    // Private functions

    let private getExprName expr =
        match expr with
        | Application         _ -> "Application"
        | Call                _ -> "Call"
        | Coerce              _ -> "Coerce"
        | DefaultValue        _ -> "DefaultValue"
        | FieldGet            _ -> "FieldGet"
        | FieldSet            _ -> "FieldSet"
        | ForIntegerRangeLoop _ -> "ForIntegerRangeLoop"
        | IfThenElse          _ -> "IfThenElse"
        | Lambda              _ -> "Lambda"
        | Let                 _ -> "Let"
        | LetRecursive        _ -> "LetRecursive"
        | NewArray            _ -> "NewArray"
        | NewObject           _ -> "NewObject"
        | NewRecord           _ -> "NewRecord"
        | NewTuple            _ -> "NewTuple"
        | NewUnionCase        _ -> "NewUnionCase"
        | PropertyGet         _ -> "PropertyGet"
        | PropertySet         _ -> "PropertySet"
        | QuoteRaw            _ -> "QuoteRaw"
        | QuoteTyped          _ -> "QuoteTyped"
        | Sequential          _ -> "Sequential"
        | TryFinally          _ -> "TryFinally"
        | TryWith             _ -> "TryWith"
        | TupleGet            _ -> "TupleGet"
        | TypeTest            _ -> "TypeTest"
        | UnionCaseTest       _ -> "UnionCaseTest"
        | Value               _ -> "Value"
        | VarSet              _ -> "VarSet"
        | Var                 _ -> "Var"
        | WhileLoop           _ -> "WhileLoop"
        |                     _ -> failwithf "Expression %O is not supported" expr
    
    let private getValueDispalyValue (value, valueType : Type) =
        sprintf "Get value: %O (%s)" value valueType.Name

    let private getMethodDisplayName instanceExpr (methodInfo : MethodInfo) =
        match instanceExpr with
        | Some instance -> ""
        | None          -> methodInfo.Name
    
    let private (|IsNoneOption|_|) (valueType : Type) value =
        if value = null && valueType.Name = "FSharpOption`1"
        then Some value
        else None

    let private (|IsTuple|_|) (valueType : Type) value =
        if FSharpType.IsTuple valueType
        then Some value
        else None

    let private getTupleTypeDisplayValue (tupleType : Type) =
        sprintf "(%s)"
            (Array.map (fun (t : Type) -> t.Name) tupleType.GenericTypeArguments
            |> String.concat ", ")

    let private formatStateLastValue state (valueType : Type) =
        match Evaluator.getLastValue state with
        | IsNoneOption valueType _ -> sprintf "None (%s)" valueType.Name
        | IsTuple valueType      v -> sprintf "%O %s" v <| getTupleTypeDisplayValue valueType
        | null                     -> sprintf "null (%s)" valueType.Name
        | v                        -> sprintf "%O (%s)" v <| valueType.Name

    let private formatVariable (variable : Var) =
        sprintf "%s (%s)" variable.Name variable.Type.Name

    let private getCallDispalyValue stage (instanceExpr, methodInfo, _) state =
        match stage with
        | Pre  -> 
            sprintf "Calling %s" 
            <| getMethodDisplayName instanceExpr methodInfo
        | Post -> 
            sprintf "Called %s, Returned: %s" 
            <| getMethodDisplayName instanceExpr methodInfo
            <| formatStateLastValue state methodInfo.ReturnType

    let private getNewUnionDisplayValue stage (unionCaseInfo : UnionCaseInfo, _) state =
        match stage with
        | Pre  -> 
            sprintf "Creating %s (%s)" unionCaseInfo.Name unionCaseInfo.DeclaringType.Name
        | Post -> 
            sprintf "Created %s" <| formatStateLastValue state unionCaseInfo.DeclaringType

    let private getNewRecordDisplayValue stage (recordType : Type, _) state =
        match stage with
        | Pre  -> 
            sprintf "Creating new %s" recordType.Name
        | Post -> 
            sprintf "Created %s" <| formatStateLastValue state recordType
                
    let private getNewTupleDisplayValue stage (tupleType : Type) state =
        match stage with
        | Pre  -> 
            sprintf "Creating new Tuple %s" <| getTupleTypeDisplayValue tupleType 
        | Post -> 
            sprintf "Created Tuple %s" <| formatStateLastValue state tupleType

    let private getLetDisplayValue stage (variable : Var, _, body : Expr) state =
        match stage with
        | Pre  -> 
            sprintf "Let %s" <| formatVariable variable
        | Post -> 
            sprintf "Let %s returned %s" variable.Name <| formatStateLastValue state body.Type
    
    let private getVarDisplayValue stage variable state =
        match stage with
        | Pre  -> 
            sprintf "Get variable %s" <| formatVariable variable
        | Post -> 
            sprintf "Get variable %s, Returned %s" 
            <| variable.Name
            <| formatStateLastValue state variable.Type
    

    let private getExprDispalyValue stage expr state =
        match expr with
        //| Application         _ -> "Application"
        | Call          callState -> getCallDispalyValue stage callState state
        //| Coerce              _ -> "Coerce"
        //| DefaultValue        _ -> "DefaultValue"
        //| FieldGet            _ -> "FieldGet"
        //| FieldSet            _ -> "FieldSet"
        //| ForIntegerRangeLoop _ -> "ForIntegerRangeLoop"
        //| IfThenElse          _ -> "IfThenElse"
        //| Lambda              _ -> "Lambda"
        | Let letState -> getLetDisplayValue stage letState state
        //| LetRecursive        _ -> "LetRecursive"
        //| NewArray            _ -> "NewArray"
        //| NewObject           _ -> "NewObject"
        | NewRecord  newRecordState -> getNewRecordDisplayValue stage newRecordState state
        | NewTuple  _ -> getNewTupleDisplayValue stage expr.Type state
        | NewUnionCase newUnionCaseState -> getNewUnionDisplayValue stage newUnionCaseState state
        //| PropertyGet         _ -> "PropertyGet"
        //| PropertySet         _ -> "PropertySet"
        //| QuoteRaw            _ -> "QuoteRaw"
        //| QuoteTyped          _ -> "QuoteTyped"
        //| Sequential          _ -> "Sequential"
        //| TryFinally          _ -> "TryFinally"
        //| TryWith             _ -> "TryWith"
        //| TupleGet            _ -> "TupleGet"
        //| TypeTest            _ -> "TypeTest"
        //| UnionCaseTest       _ -> "UnionCaseTest"
        | Value         valueState -> getValueDispalyValue valueState
        //| VarSet              _ -> "VarSet"
        | Var           variable -> getVarDisplayValue stage variable state
        //| WhileLoop           _ -> "WhileLoop"
        |                     _ -> failwithf "Expression %O is not supported" expr

    let private postPerformanceInspector config (startTime : DateTime) expr state =
        let endTime = DateTime.Now
        let elapsedTime = endTime.Subtract(startTime)
        config.HandleMessage <| config.PostMessageFormatter endTime expr state elapsedTime
    
    // Public functions

    let defaultPerformancePreMessageFormatter time expr state =
        sprintf "%O - Start - %s" 
            time
            <| getExprDispalyValue Pre expr state
        
    let defaultPostformancePreMessageFormatter time expr state (elapsed : TimeSpan) =
        sprintf "%O - End - %s, Elapsed - %.3f ms" 
            time 
            <| getExprDispalyValue Post expr state
            <| elapsed.TotalMilliseconds

    let saveToFile fileName message =
        File.AppendAllText (fileName, message)

    let performanceInspector config expr state =
        let startTime = DateTime.Now
        config.HandleMessage <| config.PreMessageFormatter startTime expr state
        Some <| postPerformanceInspector config startTime

    let filePerformanceInspector fileName =
        performanceInspector 
            {
                HandleMessage = saveToFile fileName
                PreMessageFormatter = defaultPerformancePreMessageFormatter
                PostMessageFormatter = defaultPostformancePreMessageFormatter
            }
