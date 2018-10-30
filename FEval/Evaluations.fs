namespace FEval

module Evaluations =
    open Microsoft.FSharp.Quotations
    open Microsoft.FSharp.Quotations.Patterns
    open FEval.ExceptionHandling
    open FEval.Loops
    open FEval.Reflection
    open System
    open System.Reflection
    
    let private objType = typeof<obj>
    let private convertType = typeof<Convert>
    let private operatorsType = typeof<Operators>

    // Private functions

    let private methodOverrides = 
        new Map<string, MethodInfo>
            [|
                (Methods.byte,        convertType.GetMethod("ToByte",    [|objType|]))
                (Methods.char,        convertType.GetMethod("ToChar",    [|objType|]))
                (Methods.decimal,     convertType.GetMethod("ToDecimal", [|objType|]))
                (Methods.float,       convertType.GetMethod("ToDouble",  [|objType|]))
                (Methods.float32,     convertType.GetMethod("ToSingle",  [|objType|]))
                (Methods.int,         convertType.GetMethod("ToInt32",   [|objType|]))
                (Methods.int16,       convertType.GetMethod("ToInt16",   [|objType|]))
                (Methods.int32,       convertType.GetMethod("ToInt32",   [|objType|]))
                (Methods.int64,       convertType.GetMethod("ToInt64",   [|objType|]))
                (Methods.sByte,       convertType.GetMethod("ToSByte",   [|objType|]))
                (Methods.uInt16,      convertType.GetMethod("ToUInt16",  [|objType|]))
                (Methods.uInt32,      convertType.GetMethod("ToUInt32",  [|objType|]))
                (Methods.uInt64,      convertType.GetMethod("ToUInt64",  [|objType|]))
                (Methods.subtraction, operatorsType.GetMethod("subtract"))
                (Methods.unaryNegate, operatorsType.GetMethod("unaryNegate"))
                (Methods.unaryPlus,   operatorsType.GetMethod("unaryPlus"))
                (Methods.division,    operatorsType.GetMethod("division"))
                (Methods.modulus,     operatorsType.GetMethod("modulus"))
                (Methods.bitwiseAnd,  operatorsType.GetMethod("bitwiseAnd"))
                (Methods.bitwiseOr,   operatorsType.GetMethod("bitwiseOr"))
                (Methods.exclusiveOr, operatorsType.GetMethod("exclusiveOr"))
                (Methods.logicalNot,  operatorsType.GetMethod("logicalNot"))
                (Methods.leftShift,   operatorsType.GetMethod("leftShift"))
                (Methods.rightShift,  operatorsType.GetMethod("rightShift"))
            |]

    let private evalValue state (value, _) =
        Evaluator.setLastValue state value

    let private evalInstanceExpr state instanceExpr =
        match instanceExpr with
        | None -> 
            (null, state)
        | Some expr -> 
            Evaluator.evalExpr expr state 
            |> Evaluator.getLastValueAndState

    let private getMethodOverride methodInfo =
        match getMethodFullName methodInfo |> Map.tryFind <| methodOverrides with
        | Some methodOverride -> methodOverride
        | _                   -> methodInfo

    let private evalMethodCall state (instanceExpr, methodInfo, parameterExprs) =
        let (instance, newState) = evalInstanceExpr state instanceExpr
        Evaluator.evalExprs parameterExprs newState
        |> invokeMethod instance (getMethodOverride methodInfo)
        |> Evaluator.setLastValue newState

    let private evalNewUnionCase state (unionCaseInfo, exprs) =
        Evaluator.evalExprs exprs state 
        |> makeUnion unionCaseInfo
        |> Evaluator.setLastValue state

    let private evalNewRecord state (recordType, exprs) = 
        Evaluator.evalExprs exprs state 
        |> makeRecord recordType
        |> Evaluator.setLastValue state

    let private evalNewTuple state exprs tupleType =
        Evaluator.evalExprs exprs state
        |> makeTuple tupleType
        |> Evaluator.setLastValue state
    
    let private evalNewArray state (arrayType, exprs) =
        Evaluator.evalExprs exprs state
        |> makeArray arrayType
        |> Evaluator.setLastValue state

    let private evalLet state (letVariable, letExpr, body) =
        Evaluator.evalExpr letExpr state
        |> Evaluator.setLastValueAsVar letVariable
        |> Evaluator.evalExpr body

    let private evalVarGet state variable = 
        Evaluator.getVar variable state
        |> Evaluator.setLastValue state

    let private evalVarSet state (variable, valueExpr) = 
        Evaluator.evalExpr valueExpr state
        |> Evaluator.setLastValueAsVar variable

    let private createLambdaBody state variable expr value =
        Evaluator.setLastValue state value
        |> Evaluator.setLastValueAsVar variable
        |> Evaluator.evalExprAndGetLastValue expr

    let private evalLambda state (variable : Var, expr : Expr) =
        createLambdaBody state variable expr
        |> makeFunction variable.Type expr.Type 
        |> Evaluator.setLastValue state

    let private evalApplication state (funcExpr, valueExpr) =
        let func = Evaluator.evalExprAndGetLastValue funcExpr state
        let value = Evaluator.evalExprAndGetLastValue valueExpr state
        let method = getMethodInfo func "Invoke"
        invokeMethod func method [|value|]
        |> Evaluator.setLastValue state 

    let private evalCoerce state (expr, _) =
        Evaluator.evalExpr expr state

    let private evalNewObject state (constructorInfo, parameterExprs) =
        Evaluator.evalExprs parameterExprs state
        |> invokeCtor constructorInfo
        |> Evaluator.setLastValue state

    let private evalSequential state (firstExpr, secondExpr) = 
        Evaluator.evalExpr firstExpr state
        |> Evaluator.evalExpr secondExpr

    let private evalPropertyGet state (instanceExpr, propertyInfo, parameterExprs) =
        let (instance, newState) = evalInstanceExpr state instanceExpr
        Evaluator.evalExprs parameterExprs newState
        |> invokeGetProperty instance propertyInfo
        |> Evaluator.setLastValue newState

    let private evalPropertySet state (instanceExpr, propertyInfo, indexerExprs, valueExpr) =
        let (instance, newState1) = evalInstanceExpr state instanceExpr
        let (value, newState2) = 
            Evaluator.evalExpr valueExpr newState1
            |> Evaluator.getLastValueAndState
        Evaluator.evalExprs indexerExprs newState2
        |> invokeSetProperty instance propertyInfo value
        |> Evaluator.setLastValue newState2

    let private evalDefaultValue state defaultType =
        createNewInstance defaultType
        |> Evaluator.setLastValue state

    let private evalFieldGet state (instanceExpr, fieldInfo) =
        let (instance, newState1) = evalInstanceExpr state instanceExpr
        invokeGetField instance fieldInfo
        |> Evaluator.setLastValue newState1
        
    let private evalFieldSet state (instanceExpr, fieldInfo, valueExpr) =
        let (instance, newState1) = evalInstanceExpr state instanceExpr
        let (value, newState2) = 
            Evaluator.evalExpr valueExpr newState1
            |> Evaluator.getLastValueAndState
        invokeSetField instance fieldInfo value
        |> Evaluator.setLastValue newState2

    let private evalFor state (loopVar, startExpr, endExpr, bodyExpr) =
        let startIndex = Evaluator.evalExprAndGetLastValue startExpr state :?> int
        let endIndex =  Evaluator.evalExprAndGetLastValue endExpr state :?> int
        
        runLoop 
        <| createForLoopConfiguration loopVar startIndex endIndex bodyExpr
        <| Evaluator.setVar loopVar startIndex state

    let private evalWhile state (conditionExpr, bodyExpr) =
        runLoop 
        <| createWhileLoopConfiguration conditionExpr bodyExpr
        <| state

    let private evalIf state (conditionExpr, thenExpr, elseExpr) =
        let condition = Evaluator.evalExprAndGetLastValue conditionExpr state :?> bool

        match condition with
        | true -> Evaluator.evalExpr thenExpr state
        | false -> Evaluator.evalExpr elseExpr state

    let private evalTupleGet state (tupleExpr, index) =
        Evaluator.evalExprAndGetLastValue tupleExpr state
        |> getTupleField index
        |> Evaluator.setLastValue state

    let private evalUnionCaseTest state (unionExpr, unionCaseInfo) =
        Evaluator.evalExprAndGetLastValue unionExpr state
        |> getUnionCaseInfo = unionCaseInfo
        |> Evaluator.setLastValue state

    let private evalTypeTest state (expr, expectedType : Type) =
        Evaluator.evalExprAndGetLastValue expr state |> getType 
        |> expectedType.IsAssignableFrom    
        |> Evaluator.setLastValue state

    let private evalTryFinally state (tryExpr, finallyExpr) =
        let mutable tempState = state

        try
            tempState <- Evaluator.evalExpr tryExpr tempState 
        with
            | EvaluationException (ex, exState)->            
                raise (EvaluationException (ex, Evaluator.evalExpr finallyExpr exState))
            
        Evaluator.evalExpr finallyExpr tempState
        |> Evaluator.updateVariables tempState

    let private evalTryWith state (tryExpr, _, _, catchVar, catchExpr) =
        let mutable tempState = state
        
        try
            tempState <- Evaluator.evalExpr tryExpr tempState 
        with 
            | EvaluationException (ex, exState)->            
                tempState <- 
                    Evaluator.setVar catchVar ex exState
                    |> Evaluator.evalExpr catchExpr  
            
        tempState

    let private handleTargetInvocationException (ex : TargetInvocationException) state =
        let innerException = ex.InnerException
        // If the inner exception is EvaluationException then we bubble the exception.
        // Otherwise we raise a new evaluation exception for the current state.
        match innerException with
        | :? EvaluationException -> raise innerException 
        | _                      -> raise (EvaluationException (innerException, state))

    let private evalLetRecursive state (variables, bodyExpr) =
        try
            List.fold 
                (fun s (variable, varExpr) -> 
                    Evaluator.declareRecVariable variable s
                    |> Evaluator.evalExpr varExpr
                    |> Evaluator.setLastValueAsVar variable) 
                state 
                variables
            |> Evaluator.evalExpr bodyExpr
        finally
            List.iter 
                (fun (variable, _) -> Evaluator.clearRecVariable variable state) 
                variables

    let private evalQuote  =
        Evaluator.setLastValue

    let rec private evalRec = 
        withExceptionHandling 
            (fun expr state -> 
            match expr with
                | Application applicationState   -> evalApplication state applicationState
                | Call callState                 -> evalMethodCall state callState
                | Coerce coerceState             -> evalCoerce state coerceState
                | DefaultValue defaultType       -> evalDefaultValue state defaultType
                | FieldGet fieldGetState         -> evalFieldGet state fieldGetState
                | FieldSet fieldSetState         -> evalFieldSet state fieldSetState
                | ForIntegerRangeLoop forState   -> evalFor state forState
                | IfThenElse ifState             -> evalIf state ifState
                | Lambda lambdaState             -> evalLambda state lambdaState
                | Let letState                   -> evalLet state letState
                | LetRecursive letRecursiveState -> evalLetRecursive state letRecursiveState
                | NewArray newArrayState         -> evalNewArray state newArrayState 
                | NewObject newObjectState       -> evalNewObject state newObjectState
                | NewRecord newRecordState       -> evalNewRecord state newRecordState
                | NewTuple exprs                 -> evalNewTuple state exprs expr.Type
                | NewUnionCase newUnionCaseState -> evalNewUnionCase state newUnionCaseState
                | PropertyGet propertyGetState   -> evalPropertyGet state propertyGetState
                | PropertySet propertySetState   -> evalPropertySet state propertySetState
                | QuoteRaw quoteExpr             -> evalQuote state quoteExpr
                | QuoteTyped quoteExpr           -> evalQuote state quoteExpr
                | Sequential sequentialState     -> evalSequential state sequentialState
                | TryFinally tryFinallyState     -> evalTryFinally state tryFinallyState
                | TryWith tryWithState           -> evalTryWith state tryWithState
                | TupleGet tupleGetState         -> evalTupleGet state tupleGetState
                | TypeTest typeTestState         -> evalTypeTest state typeTestState
                | UnionCaseTest unionCaseState   -> evalUnionCaseTest state unionCaseState
                | Value valueState               -> evalValue state valueState
                | VarSet varSetState             -> evalVarSet state varSetState
                | Var variable                   -> evalVarGet state variable
                | WhileLoop whileState           -> evalWhile state whileState
                | _                              -> failwithf "Expression %O is not supported" expr
            )

    // Public functions

    let eval<'a> (expr : Expr<'a>) : 'a =
        Evaluator.eval evalRec expr 
        |> Evaluator.getLastValue 
        :?> 'a