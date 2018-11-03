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
    
    let private (|IsOption|_|) (valueType : Type) =
        if valueType.Name = "FSharpOption`1"
        then Some valueType
        else None

    let private (|IsTuple|_|) (valueType : Type) =
        if FSharpType.IsTuple valueType
        then Some valueType
        else None
        
    let private (|IsFunction|_|) (valueType : Type) =
        if FSharpType.IsFunction valueType
        then Some valueType
        else None

    let private formatGenericTypeArguments typeFormatter (declaringType : Type) separator =
        Array.map typeFormatter declaringType.GenericTypeArguments
        |> String.concat separator

    let private formatTupleType typeFormatter tupleType =
        sprintf "(%s)" <| formatGenericTypeArguments typeFormatter tupleType ", "

    let private formatFunctionType typeFormatter functionType =
        sprintf "(%s)" <| formatGenericTypeArguments typeFormatter functionType " -> "

    let rec private formatType (valueType : Type) =
        match valueType with
        | IsFunction t -> formatFunctionType formatType t
        | IsTuple t    -> formatTupleType formatType t
        | IsOption _   -> "Option"
        | t            -> t.Name

    let private formatStateLastValue state valueType =
        let value = Evaluator.getLastValue state

        match valueType with
        | IsFunction t                 -> formatType t
        | IsOption t when value = null -> sprintf "None : %s" <| formatType t
        | t                            -> sprintf "%O : %s" value <| formatType t

    let private formatVariable (variable : Var) =
        sprintf "%s : %s" variable.Name <| formatType variable.Type
        
    let private formatValueExpr (value, valueType : Type) =
        sprintf "Get value %O : %s" value <| formatType valueType
        
    let private formatMethodDisplayName (instanceExpr : Expr option) (methodInfo : MethodInfo) =
        match instanceExpr with
        | Some instance -> sprintf "%s.%s" (formatType instance.Type) methodInfo.Name
        | None          -> methodInfo.Name

    let private formatCallExpr stage (instanceExpr, methodInfo, _) state =
        match stage with
        | Pre  -> 
            sprintf "Calling %s" 
            <| formatMethodDisplayName instanceExpr methodInfo
        | Post -> 
            sprintf "Called %s, Returned %s" 
            <| formatMethodDisplayName instanceExpr methodInfo
            <| formatStateLastValue state methodInfo.ReturnType

    let private formatNewUnionCaseExpr stage (unionCaseInfo : UnionCaseInfo, _) state =
        match stage with
        | Pre  -> 
            sprintf "Creating %s : %s" unionCaseInfo.Name <| formatType unionCaseInfo.DeclaringType
        | Post -> 
            sprintf "Created %s" <| formatStateLastValue state unionCaseInfo.DeclaringType

    let private formatNewRecordExpr stage (recordType : Type, _) state =
        match stage with
        | Pre  -> 
            sprintf "Creating new %s" recordType.Name
        | Post -> 
            sprintf "Created %s" <| formatStateLastValue state recordType
                
    let private formatNewTupleExpr stage (tupleType : Type) state =
        match stage with
        | Pre  -> 
            sprintf "Creating new Tuple %s" <| formatType tupleType 
        | Post -> 
            sprintf "Created Tuple %s" <| formatStateLastValue state tupleType

    let private formatLetExpr stage (variable : Var, _, body : Expr) state =
        match stage with
        | Pre  -> 
            sprintf "Let %s" <| formatVariable variable
        | Post -> 
            sprintf "Let %s returned %s" variable.Name <| formatStateLastValue state body.Type
    
    let private formatVariableExpr stage variable state =
        match stage with
        | Pre  -> 
            sprintf "Get variable %s" <| formatVariable variable
        | Post -> 
            sprintf "Get variable %s, Returned %s" 
            <| variable.Name
            <| formatStateLastValue state variable.Type
    
    let private formatlambdaExpr stage functionType =
        match stage with
        | Pre  -> 
            sprintf "Creating lambda %s" <| formatType functionType
        | Post -> 
            sprintf "Created lambda %s" <| formatType functionType

    let private formatApplicationExpr stage (funcExpr : Expr, _) state =
        match stage with
        | Pre  -> 
            sprintf "Applying function %s" <| formatType funcExpr.Type
        | Post -> 
            sprintf "Applyied function %s, Returned %s" 
            <| formatType funcExpr.Type
            <| (formatStateLastValue state <| state.LastValue.GetType())
    
    let private formatCoerceExpr stage (expr : Expr, coerceType) =
        match stage with
        | Pre  -> 
            sprintf "Coercing %s to %s" <| formatType expr.Type <| formatType coerceType
        | Post -> 
            sprintf "Coerced %s to %s" <| formatType expr.Type <| formatType coerceType
    
    let private getExprDispalyValue stage expr state =
        match expr with
        | Application applicationState -> formatApplicationExpr stage applicationState state
        | Call          callState -> formatCallExpr stage callState state
        | Coerce            coerceState -> formatCoerceExpr stage coerceState
        //| DefaultValue        _ -> "DefaultValue"
        //| FieldGet            _ -> "FieldGet"
        //| FieldSet            _ -> "FieldSet"
        //| ForIntegerRangeLoop _ -> "ForIntegerRangeLoop"
        //| IfThenElse          _ -> "IfThenElse"
        | Lambda _ -> formatlambdaExpr stage expr.Type
        | Let letState -> formatLetExpr stage letState state
        //| LetRecursive        _ -> "LetRecursive"
        //| NewArray            _ -> "NewArray"
        //| NewObject           _ -> "NewObject"
        | NewRecord  newRecordState -> formatNewRecordExpr stage newRecordState state
        | NewTuple  _ -> formatNewTupleExpr stage expr.Type state
        | NewUnionCase newUnionCaseState -> formatNewUnionCaseExpr stage newUnionCaseState state
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
        | Value valueState -> formatValueExpr valueState
        //| VarSet              _ -> "VarSet"
        | Var variable -> formatVariableExpr stage variable state
        //| WhileLoop           _ -> "WhileLoop"
        | _ -> failwithf "Expression %O is not supported" expr

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
