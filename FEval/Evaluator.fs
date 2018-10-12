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

    let setVar (variable : Var) value state =
        {
            state with Variables = Map.add variable.Name value state.Variables
        }

    let getVar (variable : Var) state =
        state.Variables.Item variable.Name

    let setLastValueAsVar (variable : Var) state =
        setVar variable state.LastValue state
    
    let updateVar (variable : Var) newValueFunc state =
        let newValue = getVar variable state |> newValueFunc
        setVar variable newValue state

    let evalExpr expr state =
        state.EvalFunc expr state

    let evalExprAndGetLastValue expr state =
        evalExpr expr state
        |> getLastValue 
        
    let evalExprs exprs state =
        exprs 
        |> Seq.toArray 
        |> Array.map (fun expr -> evalExprAndGetLastValue expr state) 
    
    let evalSingleExpr exprs state =
        evalExprAndGetLastValue <| Seq.head exprs <| state 

    let createNew evalFunc =
        {
            LastValue = ()
            Variables = Map.empty<string, obj>
            EvalFunc = evalFunc
        }

    let eval evalFunc expr =
        evalExpr expr <| createNew evalFunc
