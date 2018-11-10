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
            sprintf "Creating %s : %s" unionCaseInfo.Name 
            <| formatType unionCaseInfo.DeclaringType
        | Post -> 
            sprintf "Created %s" 
            <| formatStateLastValue state unionCaseInfo.DeclaringType

    let private formatNewRecordExpr stage (recordType : Type, _) state =
        match stage with
        | Pre  -> 
            sprintf "Creating new %s" recordType.Name
        | Post -> 
            sprintf "Created %s" 
            <| formatStateLastValue state recordType
                
    let private formatNewTupleExpr stage (tupleType : Type) state =
        match stage with
        | Pre  -> 
            sprintf "Creating new tuple %s" 
            <| formatType tupleType 
        | Post -> 
            sprintf "Created tuple %s" 
            <| formatStateLastValue state tupleType

    let private formatNewArray stage (arrayType, _) state =
        match stage with
        | Pre  -> 
            sprintf "Creating new array %s" 
            <| formatType arrayType 
        | Post -> 
            sprintf "Created array %s"
            <| formatStateLastValue state arrayType

    let private formatLetExpr stage (variable : Var, _, body : Expr) state =
        match stage with
        | Pre  -> 
            sprintf "Let %s" 
            <| formatVariable variable
        | Post -> 
            sprintf "Let %s returned %s" variable.Name 
            <| formatStateLastValue state body.Type
    
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
            sprintf "Creating lambda %s" 
            <| formatType functionType
        | Post -> 
            sprintf "Created lambda %s" 
            <| formatType functionType

    let private formatApplicationExpr stage (funcExpr : Expr, _) state =
        match stage with
        | Pre  -> 
            sprintf "Applying function %s" 
            <| formatType funcExpr.Type
        | Post -> 
            sprintf "Applied function %s, Returned %s" 
            <| formatType funcExpr.Type
            <| (formatStateLastValue state <| getFunctionReturnType funcExpr.Type)
    
    let private formatCoerceExpr stage (expr : Expr, coerceType) =
        match stage with
        | Pre  -> 
            sprintf "Coercing %s to %s" 
            <| formatType expr.Type 
            <| formatType coerceType
        | Post -> 
            sprintf "Coerced %s to %s" 
            <| formatType expr.Type 
            <| formatType coerceType
    
    let private formatNewObject stage (constructorInfo, _) state =
        match stage with
        | Pre  -> 
            sprintf "Creating new object %s" 
            <| formatCtor constructorInfo
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
        
    let private formatFieldGet stage (instanceExpr, fieldInfo) state =
        match stage with
        | Pre  -> 
            sprintf "Getting field %s" 
            <| formatField fieldInfo instanceExpr
        | Post -> 
            sprintf "Got field %s, Returned %s"
            <| formatField fieldInfo instanceExpr
            <| formatStateLastValue state fieldInfo.FieldType
    
    let private formatFieldSet stage (instanceExpr, fieldInfo, _) =
        match stage with
        | Pre  -> 
            sprintf "Setting field %s" 
            <| formatField fieldInfo instanceExpr
        | Post -> 
            sprintf "Set field %s"
            <| formatField fieldInfo instanceExpr
    
    let private formatVarSet stage (variable, _) =
        match stage with
        | Pre  -> 
            sprintf "Setting variable %s" 
            <| formatVariable variable
        | Post -> 
            sprintf "Set variable %s"
            <| formatVariable variable

    let private formatFor stage (variable, _, _, _) =
        match stage with
        | Pre  -> 
            sprintf "Running for loop on %s" 
            <| formatVariable variable
        | Post -> 
            sprintf "Ran for loop on %s" 
            <| formatVariable variable

    let private formatIf stage (_, thenExpr : Expr, _) state =
        match stage with
        | Pre  -> 
            "Evaluating if"
        | Post -> 
            sprintf "Evaluated if, Returned %s" 
            <| formatStateLastValue state thenExpr.Type

    let private formatTupleGet stage (tupleExpr : Expr, index) state =
        match stage with
        | Pre  -> 
            sprintf "Getting value from tuple %s at index %i"
            <| formatType tupleExpr.Type
            <| index
        | Post -> 
            sprintf "Got value from tuple %s at index %i, Retuned %s"
            <| formatType tupleExpr.Type
            <| index
            <| (formatStateLastValue state <| getTupleItemType tupleExpr.Type index)

    let private formatUnionCaseTest stage (_, unionCaseInfo) state =
        match stage with
        | Pre  -> 
            sprintf "Checking if union matches %s"
            <| formatUnionCaseInfo unionCaseInfo
        | Post -> 
            sprintf "Checked if union matches %s, Returned %s"
            <| formatUnionCaseInfo unionCaseInfo
            <| formatStateLastValue state typeof<bool>

    let private formatTypeTest stage (expr : Expr, expectedType) state =
        match stage with
        | Pre  -> 
            sprintf "Testing if %s is %s"
            <| formatType expr.Type
            <| formatType expectedType
        | Post -> 
            sprintf "Tested if %s is %s, Returned %s"
            <| formatType expr.Type
            <| formatType expectedType
            <| formatStateLastValue state typeof<bool>

    let private formatTryWith stage (bodyExpr : Expr, _, _, _, _) state =
        match stage with
        | Pre  -> 
            "Handling with try with"
        | Post -> 
            sprintf "Handled with try with, Returned %s"
            <| formatStateLastValue state bodyExpr.Type

    let private formatTryFinally stage (bodyExpr : Expr, _) state =
        match stage with
        | Pre  -> 
            "Handling with try finally"
        | Post -> 
            sprintf "Handled with try finally, Returned %s"
            <| formatStateLastValue state bodyExpr.Type

    let private formatWhileLoop stage =
        match stage with
        | Pre  -> "Running while loop"
        | Post -> "Ran while loop"

    let private formatLetRecursive stage (variableBindings, expr : Expr) state =
        let variables = List.map fst variableBindings
        
        match stage with
        | Pre  -> 
            sprintf "Recursive let %s" 
            <| formatVariables variables 
        | Post -> 
            sprintf "Recursive let %s returned %s" 
            <| formatVariables variables 
            <| formatStateLastValue state expr.Type
    
    let private formatQuote stage (quoteExpr : Expr) =
        match stage with
        | Pre  -> 
            sprintf "Getting quote (%s) : %s" 
            <| getExprName quoteExpr
            <| formatType quoteExpr.Type
        | Post -> 
            sprintf "Got quote (%s) : %s" 
            <| getExprName quoteExpr
            <| formatType quoteExpr.Type

    let private formatExpr stage expr state =
        match expr with
        | Application applicationState   -> formatApplicationExpr stage applicationState state
        | Call callState                 -> formatCallExpr stage callState state
        | Coerce coerceState             -> formatCoerceExpr stage coerceState
        | DefaultValue defaultValueState -> formatDefaultValue stage defaultValueState
        | FieldGet fieldGetState         -> formatFieldGet stage fieldGetState state
        | FieldSet fieldSetState         -> formatFieldSet stage fieldSetState
        | ForIntegerRangeLoop forState   -> formatFor stage forState
        | IfThenElse ifState             -> formatIf stage ifState state
        | Lambda _                       -> formatlambdaExpr stage expr.Type
        | Let letState                   -> formatLetExpr stage letState state
        | LetRecursive letRecursiveState -> formatLetRecursive stage letRecursiveState state
        | NewArray newArrayState         -> formatNewArray stage newArrayState state
        | NewObject  newObjectState      -> formatNewObject stage newObjectState state
        | NewRecord  newRecordState      -> formatNewRecordExpr stage newRecordState state
        | NewTuple  _                    -> formatNewTupleExpr stage expr.Type state
        | NewUnionCase newUnionCaseState -> formatNewUnionCaseExpr stage newUnionCaseState state
        | PropertyGet propertyGetState   -> formatPropertyGet stage propertyGetState state
        | PropertySet propertySetState   -> formatPropertySet stage propertySetState
        | QuoteRaw quoteExpr             -> formatQuote stage quoteExpr
        | QuoteTyped quoteExpr           -> formatQuote stage quoteExpr
        | Sequential sequentialState     -> formatSequential stage sequentialState
        | TryFinally tryFinallyState     -> formatTryFinally stage tryFinallyState state
        | TryWith tryWithState           -> formatTryWith stage tryWithState state
        | TupleGet tupleGetState         -> formatTupleGet stage tupleGetState state
        | TypeTest typeTestState         -> formatTypeTest stage typeTestState state
        | UnionCaseTest unionCaseState   -> formatUnionCaseTest stage unionCaseState state
        | Value valueState               -> formatValueExpr stage valueState
        | VarSet varSetState             -> formatVarSet stage varSetState
        | Var variable                   -> formatVariableExpr stage variable state
        | WhileLoop _                    -> formatWhileLoop stage
        | _                              -> failwithf "Expression %O is not supported" expr

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
