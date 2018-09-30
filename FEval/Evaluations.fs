namespace FEval

module Evaluations =
    open Microsoft.FSharp.Quotations
    open Microsoft.FSharp.Quotations.Patterns
    open Microsoft.FSharp.Reflection
    open System.Reflection
    open FEval.Reflection
    
    // Private functions

    let private evalMethodCall state (instance, methodInfo : MethodInfo, parameterExprs) =
        Evaluator.evalExprs parameterExprs state
        |> invokeMethod methodInfo
        |> Evaluator.setLastValue state

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

    let rec private evalRec expr state =
        match expr with
        | Value (value, _) -> 
            Evaluator.setLastValue state value
        | Var variable ->
            evalVar state variable
        | NewUnionCase newUnionCaseState -> 
            evalNewUnionCase state newUnionCaseState
        | NewRecord newRecordState ->
            evalNewRecord state newRecordState
        | NewTuple exprs ->
            evalNewTuple state exprs expr.Type
        | Call callState -> 
            evalMethodCall state callState
        | Let letState -> 
            evalLet state letState
        | Lambda lambdaState ->
            evalLambda state lambdaState 
        | _ -> failwithf "Expression %O is not supported" expr
        
    // Public functions

    let eval<'a> (expr : Expr<'a>) : 'a =
        Evaluator.eval evalRec expr 
        |> Evaluator.getLastValue 
        :?> 'a