namespace FEval

module Evaluations =
    open Microsoft.FSharp.Quotations
    open Microsoft.FSharp.Quotations.Patterns
    open Microsoft.FSharp.Reflection
    open System.Reflection
    
    // Private functions

    let private evalMethodCall state (instance, methodInfo : MethodInfo, parameterExprs) =
        let parameters = Evaluator.evalExprs parameterExprs state
        methodInfo.Invoke(null, parameters)
        |> Evaluator.setLastValue state

    let private evalNewUnionCase state (unionCaseInfo, exprs) =
        let parameters = Evaluator.evalExprs exprs state 
        FSharpValue.MakeUnion (unionCaseInfo, parameters)
        |> Evaluator.setLastValue state

    let private evalNewRecord state (recordType, exprs) = 
        let parameters = Evaluator.evalExprs exprs state 
        FSharpValue.MakeRecord (recordType, parameters)
        |> Evaluator.setLastValue state

    let private evalNewTuple state exprs tupleType =
        let parameters = Evaluator.evalExprs exprs state
        FSharpValue.MakeTuple (parameters, tupleType)
        |> Evaluator.setLastValue state

    let private evalLet state (letVariable, letExpr, body) =
        state
        |> Evaluator.evalExpr letExpr 
        |> Evaluator.setLastValueAsVar letVariable
        |> Evaluator.evalExpr body

    let private evalVar state variable = 
        state
        |> Evaluator.getVar variable 
        |> Evaluator.setLastValue state

    let private createLambdBody state variable expr (value : obj) =
        Evaluator.setLastValue state value
        |> Evaluator.setLastValueAsVar variable
        |> Evaluator.evalExpr expr
        |> Evaluator.getLastValue

    let private evalLambda state (variable : Var, expr : Expr) =
        let funcType = FSharpType.MakeFunctionType (variable.Type, expr.Type)
        let funcBody = createLambdBody state variable expr
        FSharpValue.MakeFunction (funcType, funcBody)   
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
        Evaluator.eval evalRec expr |> Evaluator.getLastValue :?> 'a