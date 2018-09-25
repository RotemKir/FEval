namespace FEval

module Evaluations =
    open Microsoft.FSharp.Quotations
    open Microsoft.FSharp.Quotations.Patterns
    open Microsoft.FSharp.Reflection
    open System.Reflection
    
    // Private functions

    let private evalMethodCall state (instance, methodInfo : MethodInfo, parameterExprs) =
        let parameters = Evaluator.evalExprs state parameterExprs 
        methodInfo.Invoke(null, parameters)

    let private evalNewUnionCase state (unionCaseInfo, exprs) =
        let parameters = Evaluator.evalExprs state exprs  
        FSharpValue.MakeUnion (unionCaseInfo, parameters)

    let private evalNewRecord state (recordType, exprs) = 
        let parameters = Evaluator.evalExprs state exprs  
        FSharpValue.MakeRecord (recordType, parameters)

    let private evalNewTuple state exprs tupleType =
        let parameters = Evaluator.evalExprs state exprs  
        FSharpValue.MakeTuple (parameters, tupleType)

    let private evalLet state (letVariable, letExpr, body) =
        Evaluator.evalExpr state letExpr
        |> Evaluator.setVar state letVariable 
        Evaluator.evalExpr state body

    let private evalVar = Evaluator.getVar 

    let rec private evalRec state expr =
        let pushResult = Evaluator.push state

        match expr with
        | Var variable ->
            evalVar state variable |> pushResult
        | Value (value, _) -> 
            value |> pushResult
        | NewUnionCase newUnionCaseState -> 
            evalNewUnionCase state newUnionCaseState |> pushResult
        | NewRecord newRecordState ->
            evalNewRecord state newRecordState |> pushResult
        | NewTuple exprs ->
            evalNewTuple state exprs expr.Type |> pushResult
        | Call callState -> 
            evalMethodCall state callState |> pushResult
        | Let letState -> 
            evalLet state letState |> pushResult
        | _ -> failwithf "Expression %O is not supported" expr

    // Public functions

    let eval<'a> (expr : Expr<'a>) : 'a =
        Evaluator.eval evalRec expr :?> 'a