namespace FEval

open Microsoft.FSharp.Quotations

type EvaluationState =
    {
        LastValue : obj
        Variables : Map<string, obj>
        EvalFunc :  Expr -> EvaluationState -> EvaluationState
    }

[<RequireQualifiedAccess>]
module Evaluator =
    
    let getLastValue state =
        state.LastValue
        
    let getLastValueAndState state =
        (getLastValue state, state)

    let setLastValue state value =
        {
            state with LastValue = value
        }

    let setLastValueAsVar (variable : Var) state =
        {
            state with Variables = Map.add variable.Name state.LastValue state.Variables
        }

    let getVar (variable : Var) state =
        state.Variables.Item variable.Name

    let evalExpr expr state =
        state.EvalFunc expr state

    let evalExprs exprs state =
        exprs 
        |> Seq.toArray 
        |> Array.map (fun expr -> evalExpr expr state |> getLastValue) 
        
    let evalSingleExpr exprs state =
        evalExpr <| Seq.head exprs <| state 
        |> getLastValue

    let createNew evalFunc =
        {
            LastValue = ()
            Variables = Map.empty<string, obj>
            EvalFunc = evalFunc
        }

    let eval evalFunc expr =
        evalExpr expr <| createNew evalFunc
