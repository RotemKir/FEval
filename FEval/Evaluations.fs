﻿namespace FEval

module Evaluations =
    open Microsoft.FSharp.Quotations
    open Microsoft.FSharp.Quotations.Patterns
    open FEval.Loops
    open FEval.Reflection
    open System
    open System.Reflection
    
    // Private functions

    let private evalValue =
        Evaluator.setLastValue

    let private evalInstanceExpr state instanceExpr =
        match instanceExpr with
        | None -> 
            (null, state)
        | Some expr -> 
            Evaluator.evalExpr expr state 
            |> Evaluator.getLastValueAndState

    let private evalRegularMethodCall state instanceExpr methodInfo parameterExprs =
        let (instance, newState) = evalInstanceExpr state instanceExpr
        Evaluator.evalExprs parameterExprs newState
        |> invokeMethod instance methodInfo
        |> Evaluator.setLastValue newState

    let private evalSingleExprMethod state exprs method =
        Evaluator.evalSingleExpr exprs state
        |> method
        |> Evaluator.setLastValue state

    let private evalMethodCall state (instanceExpr, methodInfo, parameterExprs) =
        match methodInfo with
        | MethodFullName Methods.Byte    -> evalSingleExprMethod state parameterExprs Convert.ToByte
        | MethodFullName Methods.Char    -> evalSingleExprMethod state parameterExprs Convert.ToChar
        | MethodFullName Methods.Decimal -> evalSingleExprMethod state parameterExprs Convert.ToDecimal
        | MethodFullName Methods.Float   -> evalSingleExprMethod state parameterExprs Convert.ToDouble
        | MethodFullName Methods.Float32 -> evalSingleExprMethod state parameterExprs Convert.ToSingle
        | MethodFullName Methods.Int     -> evalSingleExprMethod state parameterExprs Convert.ToInt32
        | MethodFullName Methods.Int16   -> evalSingleExprMethod state parameterExprs Convert.ToInt16
        | MethodFullName Methods.Int32   -> evalSingleExprMethod state parameterExprs Convert.ToInt32
        | MethodFullName Methods.Int64   -> evalSingleExprMethod state parameterExprs Convert.ToInt64
        | MethodFullName Methods.SByte   -> evalSingleExprMethod state parameterExprs Convert.ToSByte
        | MethodFullName Methods.UInt16  -> evalSingleExprMethod state parameterExprs Convert.ToUInt16
        | MethodFullName Methods.UInt32  -> evalSingleExprMethod state parameterExprs Convert.ToUInt32
        | MethodFullName Methods.UInt64  -> evalSingleExprMethod state parameterExprs Convert.ToUInt64
        | _ -> evalRegularMethodCall state instanceExpr methodInfo parameterExprs

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

    let private createLambdaBody state variable expr (value : obj) =
        Evaluator.setLastValue state value
        |> Evaluator.setLastValueAsVar variable
        |> Evaluator.evalExpr expr
        |> Evaluator.getLastValue

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

    let private evalTypeTest state (expr, expectedType) =
        Evaluator.evalExprAndGetLastValue expr state
        |> getType = expectedType
        |> Evaluator.setLastValue state

    let private evalTryFinally state (tryExpr, finallyExpr) =
        let mutable tempState = state
        let mutable evalFinally = false

        try
            tempState <- Evaluator.evalExpr tryExpr tempState 
        with
            | EvaluationException (ex, exState)->            
                raise (EvaluationException (ex, Evaluator.evalExpr finallyExpr exState))
            
        Evaluator.evalExpr finallyExpr tempState

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

    let rec private evalRec expr state =
        try match expr with
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
            | NewArray newArrayState         -> evalNewArray state newArrayState 
            | NewObject newObjectState       -> evalNewObject state newObjectState
            | NewRecord newRecordState       -> evalNewRecord state newRecordState
            | NewTuple exprs                 -> evalNewTuple state exprs expr.Type
            | NewUnionCase newUnionCaseState -> evalNewUnionCase state newUnionCaseState
            | PropertyGet propertyGetState   -> evalPropertyGet state propertyGetState
            | PropertySet propertySetState   -> evalPropertySet state propertySetState
            | Sequential sequentialState     -> evalSequential state sequentialState
            | TryFinally tryFinallyState     -> evalTryFinally state tryFinallyState
            | TryWith tryWithState           -> evalTryWith state tryWithState
            | TupleGet tupleGetState         -> evalTupleGet state tupleGetState
            | TypeTest typeTestState         -> evalTypeTest state typeTestState
            | UnionCaseTest unionCaseState   -> evalUnionCaseTest state unionCaseState
            | Value (value, _)               -> evalValue state value
            | VarSet varSetState             -> evalVarSet state varSetState
            | Var variable                   -> evalVarGet state variable
            | WhileLoop whileState           -> evalWhile state whileState
            | _                              -> failwithf "Expression %O is not supported" expr
        with
        | :? TargetInvocationException as ex -> handleTargetInvocationException ex state

    // Public functions

    let eval<'a> (expr : Expr<'a>) : 'a =
        Evaluator.eval evalRec expr 
        |> Evaluator.getLastValue 
        :?> 'a