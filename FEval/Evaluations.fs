namespace FEval

module Evaluations =
    open Microsoft.FSharp.Quotations
    open Microsoft.FSharp.Quotations.Patterns
    open FEval.ExceptionHandling
    open FEval.EvaluationEvents
    open FEval.EvaluationTypes
    open FEval.Loops
    open System
    open System.Reflection
    
    let private objType = typeof<obj>
    let private convertType = typeof<Convert>
    let private operatorsType = typeof<Operators>
    let private nonPublicStaticBindingFlags = BindingFlags.Static ||| BindingFlags.NonPublic

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
                (Methods.subtraction, operatorsType.GetMethod("subtract",    nonPublicStaticBindingFlags))
                (Methods.unaryNegate, operatorsType.GetMethod("unaryNegate", nonPublicStaticBindingFlags))
                (Methods.division,    operatorsType.GetMethod("division",    nonPublicStaticBindingFlags))
                (Methods.modulus,     operatorsType.GetMethod("modulus",     nonPublicStaticBindingFlags))
                (Methods.bitwiseOr,   operatorsType.GetMethod("bitwiseOr",   nonPublicStaticBindingFlags))
                (Methods.unaryPlus,   operatorsType.GetMethod("unaryPlus",   nonPublicStaticBindingFlags))
                (Methods.bitwiseAnd,  operatorsType.GetMethod("bitwiseAnd",  nonPublicStaticBindingFlags))
                (Methods.exclusiveOr, operatorsType.GetMethod("exclusiveOr", nonPublicStaticBindingFlags))
                (Methods.logicalNot,  operatorsType.GetMethod("logicalNot",  nonPublicStaticBindingFlags))
                (Methods.leftShift,   operatorsType.GetMethod("leftShift",   nonPublicStaticBindingFlags))
                (Methods.rightShift,  operatorsType.GetMethod("rightShift",  nonPublicStaticBindingFlags))
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
        match Reflection.getMethodFullName methodInfo |> Map.tryFind <| methodOverrides with
        | Some methodOverride -> methodOverride
        | _                   -> methodInfo

    let private evalMethodCall state (instanceExpr, methodInfo, parameterExprs) =
        let (instance, newState) = evalInstanceExpr state instanceExpr
        Evaluator.invokeMethod 
            <| instance 
            <| getMethodOverride methodInfo 
            <| Evaluator.evalExprs parameterExprs newState 
            <| newState
        |> Evaluator.setLastValue newState

    let private evalNewUnionCase state (unionCaseInfo, exprs) =
        Evaluator.evalExprs exprs state 
        |> Reflection.makeUnion unionCaseInfo
        |> Evaluator.setLastValue state

    let private evalNewRecord state (recordType, exprs) = 
        Evaluator.evalExprs exprs state 
        |> Reflection.makeRecord recordType
        |> Evaluator.setLastValue state

    let private evalNewTuple state exprs tupleType =
        Evaluator.evalExprs exprs state
        |> Reflection.makeTuple tupleType
        |> Evaluator.setLastValue state
    
    let private evalNewArray state (arrayType, exprs) =
        Evaluator.evalExprs exprs state
        |> Reflection.makeArray arrayType
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
        |> Evaluator.setLastValueAsUnit

    let private createLambdaBody state variable expr value =
        Evaluator.setLastValue state value
        |> Evaluator.setLastValueAsVar variable
        |> Evaluator.evalExprAndGetLastValue expr

    let private evalLambda state (variable : Var, expr : Expr) =
        createLambdaBody state variable expr
        |> Reflection.makeFunction variable.Type expr.Type 
        |> Evaluator.setLastValue state

    let private evalApplication state (funcExpr, valueExpr) =
        let func = Evaluator.evalExprAndGetLastValue funcExpr state
        let value = Evaluator.evalExprAndGetLastValue valueExpr state
        let method = Reflection.getMethodInfo func "Invoke"
        Reflection.invokeMethod func method [|value|]
        |> Evaluator.setLastValue state 

    let private evalCoerce state (expr, _) =
        Evaluator.evalExpr expr state

    let private evalNewObject state (constructorInfo, parameterExprs) =
        Evaluator.evalExprs parameterExprs state
        |> Reflection.invokeCtor constructorInfo
        |> Evaluator.setLastValue state

    let private evalSequential state (firstExpr, secondExpr) = 
        Evaluator.evalExpr firstExpr state
        |> Evaluator.evalExpr secondExpr

    let private evalPropertyGet state (instanceExpr, propertyInfo, parameterExprs) =
        let (instance, newState) = evalInstanceExpr state instanceExpr
        Evaluator.evalExprs parameterExprs newState
        |> Reflection.invokeGetProperty instance propertyInfo
        |> Evaluator.setLastValue newState

    let private evalPropertySet state (instanceExpr, propertyInfo, indexerExprs, valueExpr) =
        let (instance, newState1) = evalInstanceExpr state instanceExpr
        let (value, newState2) = 
            Evaluator.evalExpr valueExpr newState1
            |> Evaluator.getLastValueAndState
        let indexerParameters = Evaluator.evalExprs indexerExprs newState2
        Evaluator.invokeSetProperty
            <| instance 
            <| propertyInfo
            <| value
            <| indexerParameters
            <| newState2
        |> Evaluator.setLastValue newState2

    let private evalDefaultValue state defaultType =
        Reflection.createNewInstance defaultType
        |> Evaluator.setLastValue state

    let private evalFieldGet state (instanceExpr, fieldInfo) =
        let (instance, newState1) = evalInstanceExpr state instanceExpr
        Reflection.invokeGetField instance fieldInfo
        |> Evaluator.setLastValue newState1
        
    let private evalFieldSet state (instanceExpr, fieldInfo, valueExpr) =
        let (instance, newState1) = evalInstanceExpr state instanceExpr
        let (value, newState2) = 
            Evaluator.evalExpr valueExpr newState1
            |> Evaluator.getLastValueAndState
        Evaluator.invokeSetField instance fieldInfo value newState2
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
        |> Reflection.getTupleField index
        |> Evaluator.setLastValue state

    let private evalUnionCaseTest state (unionExpr, unionCaseInfo) =
        Evaluator.evalExprAndGetLastValue unionExpr state
        |> Reflection.getUnionCaseInfo = unionCaseInfo
        |> Evaluator.setLastValue state

    let private evalTypeTest state (expr, expectedType : Type) =
        Evaluator.evalExprAndGetLastValue expr state |> Reflection.getType 
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

    let private evalQuoteRaw =
        Evaluator.setLastValue

    let private evalQuoteTyped state quoteExpr =
        Reflection.convertExprToTyped quoteExpr quoteExpr.Type
        |> Evaluator.setLastValue state

    let private evalExpr = 
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
                | QuoteRaw quoteExpr             -> evalQuoteRaw state quoteExpr
                | QuoteTyped quoteExpr           -> evalQuoteTyped state quoteExpr
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

    let evalWith<'a> (expr : Expr<'a>) inspectors : 'a =
        try 
            Evaluator.createNewState evalExpr inspectors
            |> Evaluator.evalExpr expr 
            |> Evaluator.getLastValue 
            :?> 'a
        finally
            disposeInspectors inspectors

    let eval<'a> (expr : Expr<'a>) : 'a = 
        evalWith expr Seq.empty