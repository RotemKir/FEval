namespace FEval

[<RequireQualifiedAccess>]
module PerformanceInspector =
    open FEval.CommonInspections
    open FEval.TypeFormatters
    open Microsoft.FSharp.Quotations
    open Microsoft.FSharp.Quotations.Patterns
    open Microsoft.FSharp.Reflection
    open System
    open System.IO

    type Config =
        {
            HandleMessage : string -> unit
            PreMessageFormatter : DateTime -> Expr -> EvaluationState -> string
            PostMessageFormatter : DateTime -> Expr -> EvaluationState -> TimeSpan -> string
        }

    // Private functions

    let private formatStateLastValue state =
        formatValue <| Evaluator.getLastValue state

    let private formatValueExpr stage (value, valueType : Type) =
        match stage with
        | Pre  -> 
            sprintf "Getting value %s" <| formatValue value valueType 
        | Post -> 
            sprintf "Got value %s" <| formatValue value valueType 
            
    let private formatCallExpr stage (instanceExpr, methodInfo, _) state =
        match stage with
        | Pre  -> 
            sprintf "Calling %s" 
            <| formatMethod methodInfo instanceExpr
        | Post -> 
            sprintf "Called %s, Returned %s" 
            <| formatMethod methodInfo instanceExpr
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
            sprintf "Getting variable %s" 
            <| formatVariable variable
        | Post -> 
            sprintf "Got variable %s, Returned %s" 
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
    
    let private formatNewObject stage (constructorInfo, _) state =
        match stage with
        | Pre  -> 
            sprintf "Creating new object %s" <| formatCtor constructorInfo
        | Post -> 
            sprintf "Created new object %s" 
            <| formatType constructorInfo.DeclaringType

    let private formatPropertyGet stage (instanceExpr, propertyInfo, _) state =
        match stage with
        | Pre  -> 
            sprintf "Getting property %s" 
            <| formatProperty propertyInfo instanceExpr
        | Post -> 
            sprintf "Got property %s, Returned %s"
            <| formatProperty propertyInfo instanceExpr
            <| formatStateLastValue state propertyInfo.PropertyType
    
    let private formatPropertySet stage (instanceExpr, propertyInfo, _, _) =
        match stage with
        | Pre  -> 
            sprintf "Setting property %s" 
            <| formatProperty propertyInfo instanceExpr
        | Post -> 
            sprintf "Set property %s"
            <| formatProperty propertyInfo instanceExpr

    let private formatSequential stage (firstExpr, secondExpr) =
        match stage with
        | Pre  -> 
            sprintf "Performing %s and then %s" 
            <| getExprName firstExpr
            <| getExprName secondExpr
        | Post -> 
            sprintf "Performed %s and then %s" 
            <| getExprName firstExpr
            <| getExprName secondExpr

    let private formatDefaultValue stage defaultValueType =
        match stage with
        | Pre  -> 
            sprintf "Creating default value for %s" 
            <| formatType defaultValueType
        | Post -> 
            sprintf "Created default value for %s" 
            <| formatType defaultValueType
        
    let private formatExpr stage expr state =
        match expr with
        | Application applicationState -> formatApplicationExpr stage applicationState state
        | Call          callState -> formatCallExpr stage callState state
        | Coerce            coerceState -> formatCoerceExpr stage coerceState
        | DefaultValue defaultValueState -> formatDefaultValue stage defaultValueState
        //| FieldGet            _ -> "FieldGet"
        //| FieldSet            _ -> "FieldSet"
        //| ForIntegerRangeLoop _ -> "ForIntegerRangeLoop"
        //| IfThenElse          _ -> "IfThenElse"
        | Lambda _ -> formatlambdaExpr stage expr.Type
        | Let letState -> formatLetExpr stage letState state
        //| LetRecursive        _ -> "LetRecursive"
        //| NewArray            _ -> "NewArray"
        | NewObject  newObjectState -> formatNewObject stage newObjectState state
        | NewRecord  newRecordState -> formatNewRecordExpr stage newRecordState state
        | NewTuple  _ -> formatNewTupleExpr stage expr.Type state
        | NewUnionCase newUnionCaseState -> formatNewUnionCaseExpr stage newUnionCaseState state
        | PropertyGet propertyGetState -> formatPropertyGet stage propertyGetState state
        | PropertySet propertySetState -> formatPropertySet stage propertySetState
        //| QuoteRaw            _ -> "QuoteRaw"
        //| QuoteTyped          _ -> "QuoteTyped"
        | Sequential sequentialState -> formatSequential stage sequentialState
        //| TryFinally          _ -> "TryFinally"
        //| TryWith             _ -> "TryWith"
        //| TupleGet            _ -> "TupleGet"
        //| TypeTest            _ -> "TypeTest"
        //| UnionCaseTest       _ -> "UnionCaseTest"
        | Value valueState -> formatValueExpr stage valueState
        //| VarSet              _ -> "VarSet"
        | Var variable -> formatVariableExpr stage variable state
        //| WhileLoop           _ -> "WhileLoop"
        | _ -> failwithf "Expression %O is not supported" expr

    let private postPerformanceInspector config (startTime : DateTime) expr state =
        let endTime = DateTime.Now
        let elapsedTime = endTime.Subtract(startTime)
        config.HandleMessage <| config.PostMessageFormatter endTime expr state elapsedTime
    
    // Public functions

    let defaultPreMessageFormatter time expr state =
        sprintf "%O - Start - %s" 
            time
            <| formatExpr Pre expr state
        
    let defaultPostMessageFormatter time expr state (elapsed : TimeSpan) =
        sprintf "%O - End   - %s, Elapsed - %.3f ms" 
            time 
            <| formatExpr Post expr state
            <| elapsed.TotalMilliseconds

    let saveToFile fileName message =
        File.AppendAllText (fileName, message)

    let createNew config expr state =
        let startTime = DateTime.Now
        config.HandleMessage <| config.PreMessageFormatter startTime expr state
        Some <| postPerformanceInspector config startTime

    let filePerformanceInspector fileName =
        createNew 
            {
                HandleMessage = saveToFile fileName
                PreMessageFormatter = defaultPreMessageFormatter
                PostMessageFormatter = defaultPostMessageFormatter
            }
