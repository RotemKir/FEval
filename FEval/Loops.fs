namespace FEval

module internal Loops =
    open FEval.EvaluationTypes
    open Microsoft.FSharp.Quotations

    type LoopConfiguration =
        {
            BodyExpr : Expr
            LoopAction : EvaluationState -> EvaluationState
            IsTerminated : EvaluationState -> bool
        }

    // Private functions

    let private createForLoopAction loopVar increment =
        Evaluator.updateVar loopVar (fun v -> v :?> int + increment) 

    let private createForLoopTermination loopVar increment endIndex state =
        Evaluator.getVar loopVar state :?> int = endIndex + increment

    let private createWhileLoopTermination conditionExpr state = 
        Evaluator.evalExprAndGetLastValue conditionExpr state :?> bool = false

    // Public functions

    let rec runLoop loopConfiguration state =
        match loopConfiguration.IsTerminated state with
        | true -> 
            Evaluator.setLastValueAsUnit state
        | false -> 
            Evaluator.evalExpr loopConfiguration.BodyExpr state
            |> loopConfiguration.LoopAction
            |> runLoop loopConfiguration

    let createForLoopConfiguration loopVar startIndex endIndex bodyExpr =
        let increment = if startIndex <= endIndex then 1 else -1
        
        {
            BodyExpr = bodyExpr
            LoopAction = createForLoopAction loopVar increment
            IsTerminated = createForLoopTermination loopVar increment endIndex
        }

    let createWhileLoopConfiguration conditionExpr bodyExpr =
        {
            BodyExpr = bodyExpr
            LoopAction = id
            IsTerminated = createWhileLoopTermination conditionExpr
        }