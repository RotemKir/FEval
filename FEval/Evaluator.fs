namespace FEval

open Microsoft.FSharp.Quotations
open System.Collections.Generic

type EvaluationState =
    {
        LastValue      : obj
        Variables      : Map<string, obj>
        RecVariables   : Dictionary<string, obj>
        EvalFunc       : EvaluationFunc
        Inspectors     : PreInspcetor seq
    }

and EvaluationFunc = Expr -> EvaluationState -> EvaluationState

and PreInspcetor = Expr -> EvaluationState -> PostInspector option

and PostInspector = Expr -> EvaluationState -> unit
        
[<RequireQualifiedAccess>]
module Evaluator =
    
    // Private Functions

    let private mergeVariables oldVariables newVariables =
        Map.map 
            (fun name value -> 
                match Map.tryFind name newVariables with
                | Some newValue -> newValue
                | None          -> value
            ) 
            oldVariables
            
    let private setRecVariable (variable : Var) value state =
        state.RecVariables.[variable.Name] <- value
        state

    let private actOnVariable (variable : Var) recVarAction varAction state =
        if state.RecVariables.ContainsKey(variable.Name)
        then
            recVarAction variable
        else
            varAction variable
    
    let private runPreInspectors expr state =
        Seq.map (fun i -> i expr state) state.Inspectors
        |> Seq.filter Option.isSome
        |> Seq.map Option.get
        |> Seq.toArray
        
    let private runPostInspectors inspectors expr state =
        Seq.iter (fun i -> i expr state) inspectors

    // Public Functions

    let getLastValue state =
        state.LastValue
        
    let getLastValueAndState state =
        (getLastValue state, state)

    let setLastValue state value =
        { state with LastValue = value }

    let setVar (variable : Var) value state =
        actOnVariable variable
            (fun v -> setRecVariable v value state)
            (fun v -> { state with Variables = Map.add variable.Name value state.Variables })
            state

    let getVar (variable : Var) state =
        actOnVariable variable
            (fun v -> state.RecVariables.Item v.Name)
            (fun v -> state.Variables.Item v.Name)
            state

    let setLastValueAsVar (variable : Var) state =
        setVar variable state.LastValue state
    
    let setLastValueAsUnit state =
        setLastValue state ()

    let updateVar (variable : Var) newValueFunc state =
        let newValue = getVar variable state |> newValueFunc
        setVar variable newValue state

    let updateVariables state newState =
        { state with Variables = mergeVariables state.Variables newState.Variables }

    let declareRecVariable (variable : Var) =
        setRecVariable variable <| new obj() 

    let clearRecVariable (variable : Var) state =
        state.RecVariables.Remove(variable.Name) |> ignore

    let evalExpr expr state =
        let postInspectors = runPreInspectors expr state
        let newState = state.EvalFunc expr state
        runPostInspectors postInspectors expr newState
        newState

    let evalExprAndGetLastValue expr state =
        evalExpr expr state
        |> getLastValue 
        
    let evalExprs exprs state =
        exprs 
        |> Seq.toArray 
        |> Array.map (fun expr -> evalExprAndGetLastValue expr state) 
    
    let evalSingleExpr exprs state =
        evalExprAndGetLastValue <| Seq.head exprs <| state 

    let createNewState evalFunc inspectors =
        {
            LastValue = ()
            Variables = Map.empty<string, obj>
            RecVariables = new Dictionary<string, obj>()
            EvalFunc = evalFunc
            Inspectors = inspectors
        }