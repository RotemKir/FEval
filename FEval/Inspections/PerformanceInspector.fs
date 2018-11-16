namespace FEval.Inspections

[<RequireQualifiedAccess>]
module PerformanceInspector =
    open FEval
    open FEval.EvaluationTypes
    open FEval.Inspections.Persistance
    open FEval.Inspections.CommonInspections
    open FEval.Inspections.TypeFormatters
    open Microsoft.FSharp.Quotations
    open Microsoft.FSharp.Quotations.Patterns
    open System

    type InspectionResult =
        | PreResult of time : DateTime * message : string
        | PostResult of time : DateTime * message : string * elapsed : TimeSpan
            
    type Config =
        {
            HandleInspectionResult : InspectionResult -> unit
        }

    // Private functions
    
    let private formatStateLastValue inspectionContext =
        formatValue <| Evaluator.getLastValue inspectionContext.EvaluationState

    let private formatValueExpr inspectionContext (value, valueType : Type) =
        match inspectionContext.InspectionStage with
        | Pre  -> 
            sprintf "Getting value %s" <| formatValue value valueType 
        | Post -> 
            sprintf "Got value %s" <| formatValue value valueType 
            
    let private formatCallExpr inspectionContext (instanceExpr, methodInfo, _)  =
        match inspectionContext.InspectionStage with
        | Pre  -> 
            sprintf "Calling %s" 
            <| formatMethod methodInfo instanceExpr
        | Post -> 
            sprintf "Called %s, Returned %s" 
            <| formatMethod methodInfo instanceExpr
            <| formatStateLastValue inspectionContext methodInfo.ReturnType

    let private formatNewUnionCaseExpr inspectionContext (unionCaseInfo, _)  =
        match inspectionContext.InspectionStage with
        | Pre  -> 
            sprintf "Creating %s" 
            <| formatUnionCaseInfo unionCaseInfo
        | Post -> 
            sprintf "Created %s" 
            <| formatStateLastValue inspectionContext unionCaseInfo.DeclaringType

    let private formatNewRecordExpr inspectionContext (recordType : Type, _)  =
        match inspectionContext.InspectionStage with
        | Pre  -> 
            sprintf "Creating new %s" recordType.Name
        | Post -> 
            sprintf "Created %s" 
            <| formatStateLastValue inspectionContext recordType
                
    let private formatNewTupleExpr inspectionContext (tupleType : Type)  =
        match inspectionContext.InspectionStage with
        | Pre  -> 
            sprintf "Creating new tuple %s" 
            <| formatType tupleType 
        | Post -> 
            sprintf "Created tuple %s" 
            <| formatStateLastValue inspectionContext tupleType

    let private formatNewArray inspectionContext (arrayType, _)  =
        match inspectionContext.InspectionStage with
        | Pre  -> 
            sprintf "Creating new array %s" 
            <| formatType arrayType 
        | Post -> 
            sprintf "Created array %s"
            <| formatStateLastValue inspectionContext arrayType

    let private formatLetExpr inspectionContext (variable : Var, _, body : Expr)  =
        match inspectionContext.InspectionStage with
        | Pre  -> 
            sprintf "Let %s" 
            <| formatVariable variable
        | Post -> 
            sprintf "Let %s returned %s" variable.Name 
            <| formatStateLastValue inspectionContext body.Type
    
    let private formatVariableExpr inspectionContext variable  =
        match inspectionContext.InspectionStage with
        | Pre  -> 
            sprintf "Getting variable %s" 
            <| formatVariable variable
        | Post -> 
            sprintf "Got variable %s, Returned %s" 
            <| variable.Name
            <| formatStateLastValue inspectionContext variable.Type
    
    let private formatlambdaExpr inspectionContext functionType =
        match inspectionContext.InspectionStage with
        | Pre  -> 
            sprintf "Creating lambda %s" 
            <| formatType functionType
        | Post -> 
            sprintf "Created lambda %s" 
            <| formatType functionType

    let private formatApplicationExpr inspectionContext (funcExpr : Expr, _)  =
        match inspectionContext.InspectionStage with
        | Pre  -> 
            sprintf "Applying function %s" 
            <| formatType funcExpr.Type
        | Post -> 
            sprintf "Applied function %s, Returned %s" 
            <| formatType funcExpr.Type
            <| (formatStateLastValue inspectionContext <| getFunctionReturnType funcExpr.Type)
    
    let private formatCoerceExpr inspectionContext (expr : Expr, coerceType) =
        match inspectionContext.InspectionStage with
        | Pre  -> 
            sprintf "Coercing %s to %s" 
            <| formatType expr.Type 
            <| formatType coerceType
        | Post -> 
            sprintf "Coerced %s to %s" 
            <| formatType expr.Type 
            <| formatType coerceType
    
    let private formatNewObject inspectionContext (constructorInfo, _) =
        match inspectionContext.InspectionStage with
        | Pre  -> 
            sprintf "Creating new object %s" 
            <| formatCtor constructorInfo
        | Post -> 
            sprintf "Created new object %s" 
            <| formatType constructorInfo.DeclaringType

    let private formatPropertyGet inspectionContext (instanceExpr, propertyInfo, _)  =
        match inspectionContext.InspectionStage with
        | Pre  -> 
            sprintf "Getting property %s" 
            <| formatProperty propertyInfo instanceExpr
        | Post -> 
            sprintf "Got property %s, Returned %s"
            <| formatProperty propertyInfo instanceExpr
            <| formatStateLastValue inspectionContext propertyInfo.PropertyType
    
    let private formatPropertySet inspectionContext (instanceExpr, propertyInfo, _, _) =
        match inspectionContext.InspectionStage with
        | Pre  -> 
            sprintf "Setting property %s" 
            <| formatProperty propertyInfo instanceExpr
        | Post -> 
            sprintf "Set property %s"
            <| formatProperty propertyInfo instanceExpr

    let private formatSequential inspectionContext (firstExpr, secondExpr) =
        match inspectionContext.InspectionStage with
        | Pre  -> 
            sprintf "Performing %s and then %s" 
            <| getExprName firstExpr
            <| getExprName secondExpr
        | Post -> 
            sprintf "Performed %s and then %s" 
            <| getExprName firstExpr
            <| getExprName secondExpr

    let private formatDefaultValue inspectionContext defaultValueType =
        match inspectionContext.InspectionStage with
        | Pre  -> 
            sprintf "Creating default value for %s" 
            <| formatType defaultValueType
        | Post -> 
            sprintf "Created default value for %s" 
            <| formatType defaultValueType
        
    let private formatFieldGet inspectionContext (instanceExpr, fieldInfo)  =
        match inspectionContext.InspectionStage with
        | Pre  -> 
            sprintf "Getting field %s" 
            <| formatField fieldInfo instanceExpr
        | Post -> 
            sprintf "Got field %s, Returned %s"
            <| formatField fieldInfo instanceExpr
            <| formatStateLastValue inspectionContext fieldInfo.FieldType
    
    let private formatFieldSet inspectionContext (instanceExpr, fieldInfo, _) =
        match inspectionContext.InspectionStage with
        | Pre  -> 
            sprintf "Setting field %s" 
            <| formatField fieldInfo instanceExpr
        | Post -> 
            sprintf "Set field %s"
            <| formatField fieldInfo instanceExpr
    
    let private formatVarSet inspectionContext (variable, _) =
        match inspectionContext.InspectionStage with
        | Pre  -> 
            sprintf "Setting variable %s" 
            <| formatVariable variable
        | Post -> 
            sprintf "Set variable %s"
            <| formatVariable variable

    let private formatFor inspectionContext (variable, _, _, _) =
        match inspectionContext.InspectionStage with
        | Pre  -> 
            sprintf "Running for loop on %s" 
            <| formatVariable variable
        | Post -> 
            sprintf "Ran for loop on %s" 
            <| formatVariable variable

    let private formatIf inspectionContext (_, thenExpr : Expr, _)  =
        match inspectionContext.InspectionStage with
        | Pre  -> 
            "Evaluating if"
        | Post -> 
            sprintf "Evaluated if, Returned %s" 
            <| formatStateLastValue inspectionContext thenExpr.Type

    let private formatTupleGet inspectionContext (tupleExpr : Expr, index)  =
        match inspectionContext.InspectionStage with
        | Pre  -> 
            sprintf "Getting value from tuple %s at index %i"
            <| formatType tupleExpr.Type
            <| index
        | Post -> 
            sprintf "Got value from tuple %s at index %i, Retuned %s"
            <| formatType tupleExpr.Type
            <| index
            <| (formatStateLastValue inspectionContext <| getTupleItemType tupleExpr.Type index)

    let private formatUnionCaseTest inspectionContext (_, unionCaseInfo)  =
        match inspectionContext.InspectionStage with
        | Pre  -> 
            sprintf "Checking if union matches %s"
            <| formatUnionCaseInfo unionCaseInfo
        | Post -> 
            sprintf "Checked if union matches %s, Returned %s"
            <| formatUnionCaseInfo unionCaseInfo
            <| formatStateLastValue inspectionContext typeof<bool>

    let private formatTypeTest inspectionContext (expr : Expr, expectedType)  =
        match inspectionContext.InspectionStage with
        | Pre  -> 
            sprintf "Testing if %s is %s"
            <| formatType expr.Type
            <| formatType expectedType
        | Post -> 
            sprintf "Tested if %s is %s, Returned %s"
            <| formatType expr.Type
            <| formatType expectedType
            <| formatStateLastValue inspectionContext typeof<bool>

    let private formatTryWith inspectionContext (bodyExpr : Expr, _, _, _, _)  =
        match inspectionContext.InspectionStage with
        | Pre  -> 
            "Handling with try with"
        | Post -> 
            sprintf "Handled with try with, Returned %s"
            <| formatStateLastValue inspectionContext bodyExpr.Type

    let private formatTryFinally inspectionContext (bodyExpr : Expr, _)  =
        match inspectionContext.InspectionStage with
        | Pre  -> 
            "Handling with try finally"
        | Post -> 
            sprintf "Handled with try finally, Returned %s"
            <| formatStateLastValue inspectionContext bodyExpr.Type

    let private formatWhileLoop inspectionContext =
        match inspectionContext.InspectionStage with
        | Pre  -> "Running while loop"
        | Post -> "Ran while loop"

    let private formatLetRecursive inspectionContext (variableBindings, expr : Expr)  =
        let variables = List.map fst variableBindings
        
        match inspectionContext.InspectionStage with
        | Pre  -> 
            sprintf "Recursive let %s" 
            <| formatVariables variables 
        | Post -> 
            sprintf "Recursive let %s returned %s" 
            <| formatVariables variables 
            <| formatStateLastValue inspectionContext expr.Type
    
    let private formatQuote inspectionContext (quoteExpr : Expr) =
        match inspectionContext.InspectionStage with
        | Pre  -> 
            sprintf "Getting quote (%s) : %s" 
            <| getExprName quoteExpr
            <| formatType quoteExpr.Type
        | Post -> 
            sprintf "Got quote (%s) : %s" 
            <| getExprName quoteExpr
            <| formatType quoteExpr.Type

    let private formatExpr inspectionContext expr =
        match expr with
        | Application applicationState   -> formatApplicationExpr inspectionContext applicationState 
        | Call callState                 -> formatCallExpr inspectionContext callState 
        | Coerce coerceState             -> formatCoerceExpr inspectionContext coerceState
        | DefaultValue defaultValueState -> formatDefaultValue inspectionContext defaultValueState
        | FieldGet fieldGetState         -> formatFieldGet inspectionContext fieldGetState 
        | FieldSet fieldSetState         -> formatFieldSet inspectionContext fieldSetState
        | ForIntegerRangeLoop forState   -> formatFor inspectionContext forState
        | IfThenElse ifState             -> formatIf inspectionContext ifState 
        | Lambda _                       -> formatlambdaExpr inspectionContext expr.Type
        | Let letState                   -> formatLetExpr inspectionContext letState 
        | LetRecursive letRecursiveState -> formatLetRecursive inspectionContext letRecursiveState 
        | NewArray newArrayState         -> formatNewArray inspectionContext newArrayState 
        | NewObject newObjectState       -> formatNewObject inspectionContext newObjectState
        | NewRecord newRecordState       -> formatNewRecordExpr inspectionContext newRecordState 
        | NewTuple  _                    -> formatNewTupleExpr inspectionContext expr.Type 
        | NewUnionCase newUnionCaseState -> formatNewUnionCaseExpr inspectionContext newUnionCaseState 
        | PropertyGet propertyGetState   -> formatPropertyGet inspectionContext propertyGetState 
        | PropertySet propertySetState   -> formatPropertySet inspectionContext propertySetState
        | QuoteRaw quoteExpr             -> formatQuote inspectionContext quoteExpr
        | QuoteTyped quoteExpr           -> formatQuote inspectionContext quoteExpr
        | Sequential sequentialState     -> formatSequential inspectionContext sequentialState
        | TryFinally tryFinallyState     -> formatTryFinally inspectionContext tryFinallyState 
        | TryWith tryWithState           -> formatTryWith inspectionContext tryWithState 
        | TupleGet tupleGetState         -> formatTupleGet inspectionContext tupleGetState 
        | TypeTest typeTestState         -> formatTypeTest inspectionContext typeTestState 
        | UnionCaseTest unionCaseState   -> formatUnionCaseTest inspectionContext unionCaseState 
        | Value valueState               -> formatValueExpr inspectionContext valueState
        | VarSet varSetState             -> formatVarSet inspectionContext varSetState
        | Var variable                   -> formatVariableExpr inspectionContext variable 
        | WhileLoop _                    -> formatWhileLoop inspectionContext
        | _                              -> failwithf "Expression %O is not supported" expr

    let private createPreResult inspectionContext expr =
        PreResult (inspectionContext.Time, formatExpr inspectionContext expr)

    let private createPostResult inspectionContext elapsedTime expr =
        PostResult (inspectionContext.Time, formatExpr inspectionContext expr, elapsedTime)

    let private preInspector inspectionContext =
        inspectExprEvent 
            <| inspectionContext.InspectionEvent 
            <| createPreResult inspectionContext

    let private postInspector config (startTime : DateTime) inspectionContext =
        let elapsedTime = inspectionContext.Time.Subtract(startTime)
        inspectExprEvent 
            <| inspectionContext.InspectionEvent 
            <| createPostResult inspectionContext elapsedTime
        |> Option.get
        |> config.HandleInspectionResult

    let private handlePreInspectionResult config startTime result =
        config.HandleInspectionResult result 
        Some <| postInspector config startTime

    // Public functions

    let createNew config inspectionContext =
        Option.bind
            <| handlePreInspectionResult config inspectionContext.Time
            <| preInspector inspectionContext

    let stringInspectionResultFormatter inspectionResult =
        match inspectionResult with
        | PreResult (time, message) ->
            sprintf "%s - Start - %s" 
            <| formatTimeForLog time <| message
        | PostResult (time, message, elapsed) ->
            sprintf "%s - End   - %s, Elapsed - %.3f ms"
            <| formatTimeForLog time <| message <| elapsed.TotalMilliseconds
            
    let csvInspectionResultFormatter inspectionResult =
        match inspectionResult with
        | PreResult (time, message) ->
            sprintf "%s,Start,\"%s\"" 
            <| formatTimeForLog time <| formatCsvLine message
        | PostResult (time, message, elapsed) ->
            sprintf "%s,End,\"%s\",%.3f" 
            <| formatTimeForLog time <| formatCsvLine message <| elapsed.TotalMilliseconds

    let createFileLogConfig formatter fileName =
        {
            HandleInspectionResult = appendLineToFile fileName formatter
        }

    let createDefaultFileLogConfig = 
        createFileLogConfig stringInspectionResultFormatter
        
    let createCsvLogConfig fileName =
        setFileHeader fileName "Time,Stage,Message,Elapsed (ms)"
        createFileLogConfig csvInspectionResultFormatter fileName