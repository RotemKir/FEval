namespace FEval

module Evaluations =
    open Microsoft.FSharp.Quotations
    open Microsoft.FSharp.Quotations.Patterns
    open FEval.Reflection
    open System
    
    // Private functions

    let private evalValue =
        Evaluator.setLastValue

    let private evalMethodCallInstance state instanceExpr =
        match instanceExpr with
        | None -> 
            (null, state)
        | Some expr -> 
            Evaluator.evalExpr expr state 
            |> Evaluator.getLastValueAndState

    let private evalRegularMethodCall state instanceExpr methodInfo parameterExprs =
        let (instance, newState) = evalMethodCallInstance state instanceExpr
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

    let private evalVar state variable = 
        Evaluator.getVar variable state
        |> Evaluator.setLastValue state

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
        let funcAndValue = Evaluator.evalExprs [funcExpr ; valueExpr] state
        let func = funcAndValue.[0]
        let value = funcAndValue.[1]
        let method = getMethodInfo func "Invoke"
        invokeMethod func method [|value|]
        |> Evaluator.setLastValue state 

    let rec private evalRec expr state =
        match expr with
        | Value (value, _) -> 
            evalValue state value
        | Var variable ->
            evalVar state variable
        | NewUnionCase newUnionCaseState -> 
            evalNewUnionCase state newUnionCaseState
        | NewRecord newRecordState ->
            evalNewRecord state newRecordState
        | NewTuple exprs ->
            evalNewTuple state exprs expr.Type
        | NewArray newArrayState ->
            evalNewArray state newArrayState 
        | Call callState -> 
            evalMethodCall state callState
        | Let letState -> 
            evalLet state letState
        | Lambda lambdaState ->
            evalLambda state lambdaState
        | Application applicationState ->
            evalApplication state applicationState
        | _ -> failwithf "Expression %O is not supported" expr
        
    // Public functions

    let eval<'a> (expr : Expr<'a>) : 'a =
        Evaluator.eval evalRec expr 
        |> Evaluator.getLastValue 
        :?> 'a