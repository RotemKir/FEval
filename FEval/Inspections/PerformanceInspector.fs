namespace FEval

[<RequireQualifiedAccess>]
module PerformanceInspector =
    open FEval.CommonInspections
    open FEval.TypeFormatters
    open Microsoft.FSharp.Quotations
    open Microsoft.FSharp.Quotations.Patterns
    open System

    type InspectionResult =
        | PreResult of time : DateTime * message : string
        | PostResult of time : DateTime * message : string * elapsed : TimeSpan

    type Inspector = DateTime -> Expr -> EvaluationState -> InspectionResult
    
    type Config =
        {
            HandleInspectionResult : InspectionResult -> unit
            PreInspector : Inspector
            PostInspector : TimeSpan -> Inspector
        }

    // Private functions

    let private formatStateLastValue evalState =
        formatValue <| Evaluator.getLastValue evalState

    let private formatValueExpr stage (value, valueType : Type) =
        match stage with
        | Pre  -> 
            sprintf "Getting value %s" <| formatValue value valueType 
        | Post -> 
            sprintf "Got value %s" <| formatValue value valueType 
            
    let private formatCallExpr stage (instanceExpr, methodInfo, _) evalState =
        match stage with
        | Pre  -> 
            sprintf "Calling %s" 
            <| formatMethod methodInfo instanceExpr
        | Post -> 
            sprintf "Called %s, Returned %s" 
            <| formatMethod methodInfo instanceExpr
            <| formatStateLastValue evalState methodInfo.ReturnType

    let private formatNewUnionCaseExpr stage (unionCaseInfo, _) evalState =
        match stage with
        | Pre  -> 
            sprintf "Creating %s" 
            <| formatUnionCaseInfo unionCaseInfo
        | Post -> 
            sprintf "Created %s" 
            <| formatStateLastValue evalState unionCaseInfo.DeclaringType

    let private formatNewRecordExpr stage (recordType : Type, _) evalState =
        match stage with
        | Pre  -> 
            sprintf "Creating new %s" recordType.Name
        | Post -> 
            sprintf "Created %s" 
            <| formatStateLastValue evalState recordType
                
    let private formatNewTupleExpr stage (tupleType : Type) evalState =
        match stage with
        | Pre  -> 
            sprintf "Creating new tuple %s" 
            <| formatType tupleType 
        | Post -> 
            sprintf "Created tuple %s" 
            <| formatStateLastValue evalState tupleType

    let private formatNewArray stage (arrayType, _) evalState =
        match stage with
        | Pre  -> 
            sprintf "Creating new array %s" 
            <| formatType arrayType 
        | Post -> 
            sprintf "Created array %s"
            <| formatStateLastValue evalState arrayType

    let private formatLetExpr stage (variable : Var, _, body : Expr) evalState =
        match stage with
        | Pre  -> 
            sprintf "Let %s" 
            <| formatVariable variable
        | Post -> 
            sprintf "Let %s returned %s" variable.Name 
            <| formatStateLastValue evalState body.Type
    
    let private formatVariableExpr stage variable evalState =
        match stage with
        | Pre  -> 
            sprintf "Getting variable %s" 
            <| formatVariable variable
        | Post -> 
            sprintf "Got variable %s, Returned %s" 
            <| variable.Name
            <| formatStateLastValue evalState variable.Type
    
    let private formatlambdaExpr stage functionType =
        match stage with
        | Pre  -> 
            sprintf "Creating lambda %s" 
            <| formatType functionType
        | Post -> 
            sprintf "Created lambda %s" 
            <| formatType functionType

    let private formatApplicationExpr stage (funcExpr : Expr, _) evalState =
        match stage with
        | Pre  -> 
            sprintf "Applying function %s" 
            <| formatType funcExpr.Type
        | Post -> 
            sprintf "Applied function %s, Returned %s" 
            <| formatType funcExpr.Type
            <| (formatStateLastValue evalState <| getFunctionReturnType funcExpr.Type)
    
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
    
    let private formatNewObject stage (constructorInfo, _) evalState =
        match stage with
        | Pre  -> 
            sprintf "Creating new object %s" 
            <| formatCtor constructorInfo
        | Post -> 
            sprintf "Created new object %s" 
            <| formatType constructorInfo.DeclaringType

    let private formatPropertyGet stage (instanceExpr, propertyInfo, _) evalState =
        match stage with
        | Pre  -> 
            sprintf "Getting property %s" 
            <| formatProperty propertyInfo instanceExpr
        | Post -> 
            sprintf "Got property %s, Returned %s"
            <| formatProperty propertyInfo instanceExpr
            <| formatStateLastValue evalState propertyInfo.PropertyType
    
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
        
    let private formatFieldGet stage (instanceExpr, fieldInfo) evalState =
        match stage with
        | Pre  -> 
            sprintf "Getting field %s" 
            <| formatField fieldInfo instanceExpr
        | Post -> 
            sprintf "Got field %s, Returned %s"
            <| formatField fieldInfo instanceExpr
            <| formatStateLastValue evalState fieldInfo.FieldType
    
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

    let private formatIf stage (_, thenExpr : Expr, _) evalState =
        match stage with
        | Pre  -> 
            "Evaluating if"
        | Post -> 
            sprintf "Evaluated if, Returned %s" 
            <| formatStateLastValue evalState thenExpr.Type

    let private formatTupleGet stage (tupleExpr : Expr, index) evalState =
        match stage with
        | Pre  -> 
            sprintf "Getting value from tuple %s at index %i"
            <| formatType tupleExpr.Type
            <| index
        | Post -> 
            sprintf "Got value from tuple %s at index %i, Retuned %s"
            <| formatType tupleExpr.Type
            <| index
            <| (formatStateLastValue evalState <| getTupleItemType tupleExpr.Type index)

    let private formatUnionCaseTest stage (_, unionCaseInfo) evalState =
        match stage with
        | Pre  -> 
            sprintf "Checking if union matches %s"
            <| formatUnionCaseInfo unionCaseInfo
        | Post -> 
            sprintf "Checked if union matches %s, Returned %s"
            <| formatUnionCaseInfo unionCaseInfo
            <| formatStateLastValue evalState typeof<bool>

    let private formatTypeTest stage (expr : Expr, expectedType) evalState =
        match stage with
        | Pre  -> 
            sprintf "Testing if %s is %s"
            <| formatType expr.Type
            <| formatType expectedType
        | Post -> 
            sprintf "Tested if %s is %s, Returned %s"
            <| formatType expr.Type
            <| formatType expectedType
            <| formatStateLastValue evalState typeof<bool>

    let private formatTryWith stage (bodyExpr : Expr, _, _, _, _) evalState =
        match stage with
        | Pre  -> 
            "Handling with try with"
        | Post -> 
            sprintf "Handled with try with, Returned %s"
            <| formatStateLastValue evalState bodyExpr.Type

    let private formatTryFinally stage (bodyExpr : Expr, _) evalState =
        match stage with
        | Pre  -> 
            "Handling with try finally"
        | Post -> 
            sprintf "Handled with try finally, Returned %s"
            <| formatStateLastValue evalState bodyExpr.Type

    let private formatWhileLoop stage =
        match stage with
        | Pre  -> "Running while loop"
        | Post -> "Ran while loop"

    let private formatLetRecursive stage (variableBindings, expr : Expr) evalState =
        let variables = List.map fst variableBindings
        
        match stage with
        | Pre  -> 
            sprintf "Recursive let %s" 
            <| formatVariables variables 
        | Post -> 
            sprintf "Recursive let %s returned %s" 
            <| formatVariables variables 
            <| formatStateLastValue evalState expr.Type
    
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

    let private formatExpr stage expr evalState =
        match expr with
        | Application applicationState   -> formatApplicationExpr stage applicationState evalState
        | Call callState                 -> formatCallExpr stage callState evalState
        | Coerce coerceState             -> formatCoerceExpr stage coerceState
        | DefaultValue defaultValueState -> formatDefaultValue stage defaultValueState
        | FieldGet fieldGetState         -> formatFieldGet stage fieldGetState evalState
        | FieldSet fieldSetState         -> formatFieldSet stage fieldSetState
        | ForIntegerRangeLoop forState   -> formatFor stage forState
        | IfThenElse ifState             -> formatIf stage ifState evalState
        | Lambda _                       -> formatlambdaExpr stage expr.Type
        | Let letState                   -> formatLetExpr stage letState evalState
        | LetRecursive letRecursiveState -> formatLetRecursive stage letRecursiveState evalState
        | NewArray newArrayState         -> formatNewArray stage newArrayState evalState
        | NewObject  newObjectState      -> formatNewObject stage newObjectState evalState
        | NewRecord  newRecordState      -> formatNewRecordExpr stage newRecordState evalState
        | NewTuple  _                    -> formatNewTupleExpr stage expr.Type evalState
        | NewUnionCase newUnionCaseState -> formatNewUnionCaseExpr stage newUnionCaseState evalState
        | PropertyGet propertyGetState   -> formatPropertyGet stage propertyGetState evalState
        | PropertySet propertySetState   -> formatPropertySet stage propertySetState
        | QuoteRaw quoteExpr             -> formatQuote stage quoteExpr
        | QuoteTyped quoteExpr           -> formatQuote stage quoteExpr
        | Sequential sequentialState     -> formatSequential stage sequentialState
        | TryFinally tryFinallyState     -> formatTryFinally stage tryFinallyState evalState
        | TryWith tryWithState           -> formatTryWith stage tryWithState evalState
        | TupleGet tupleGetState         -> formatTupleGet stage tupleGetState evalState
        | TypeTest typeTestState         -> formatTypeTest stage typeTestState evalState
        | UnionCaseTest unionCaseState   -> formatUnionCaseTest stage unionCaseState evalState
        | Value valueState               -> formatValueExpr stage valueState
        | VarSet varSetState             -> formatVarSet stage varSetState
        | Var variable                   -> formatVariableExpr stage variable evalState
        | WhileLoop _                    -> formatWhileLoop stage
        | _                              -> failwithf "Expression %O is not supported" expr

    let private postPerformanceInspector config (startTime : DateTime) expr evalState =
        let endTime = DateTime.Now
        let elapsedTime = endTime.Subtract(startTime)
        config.HandleInspectionResult <| config.PostInspector elapsedTime endTime expr evalState 
    
    // Public functions

    let defaultPreInspector time expr evalState =
        PreResult (time, formatExpr Pre expr evalState)
        
    let defaultPostInspector elapsed time expr evalState  =
        PostResult (time, formatExpr Post expr evalState, elapsed)

    let createNew config expr evalState =
        let startTime = DateTime.Now
        config.HandleInspectionResult <| config.PreInspector startTime expr evalState
        Some <| postPerformanceInspector config startTime