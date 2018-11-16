namespace FEval

open Microsoft.FSharp.Quotations
open FEval.EvaluationTypes
open System.Collections.Generic
open System
        
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
        then recVarAction variable
        else varAction variable
    
    let private runPreInspectors inspectionContext =
        Seq.map (fun i -> i inspectionContext) inspectionContext.EvaluationState.Inspectors
        |> Seq.filter Option.isSome
        |> Seq.map Option.get
        |> Seq.toArray
        
    let private runPostInspectors inspectors inspectionContext =
        Seq.iter (fun i -> i inspectionContext) inspectors

    let private createPreMethodEventDetails instance methodInfo parameters =
        {
            Method = methodInfo 
            Instance = instance
            Parameters = parameters 
            Result = None
        }
    
    let private createPostMethodEventDetails preMethodEventDetails result =
        { preMethodEventDetails  with Result = Some result }

    let private createPreInspectionContext inspectionEvent state =
            {
                InspectionStage = InspectionStage.Pre
                InspectionEvent = inspectionEvent
                Time = DateTime.Now
                EvaluationState =  state
            }
            
    let private createPostInspectionContext inspectionEvent state =
            {
                InspectionStage = InspectionStage.Post
                InspectionEvent = inspectionEvent
                Time = DateTime.Now
                EvaluationState =  state
            }

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
        let preInspectionContext = createPreInspectionContext <| ExprEvent expr <| state
        let postInspectors = runPreInspectors preInspectionContext
        
        let postState = state.EvalFunc expr state
        
        let postInspectionContext = createPostInspectionContext <| ExprEvent expr <| postState
        runPostInspectors postInspectors postInspectionContext
        postState

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

    let invokeMethod instance methodInfo parameters state =
        let preMethodEventDetails = createPreMethodEventDetails instance methodInfo parameters
        let preInspectionContext = createPreInspectionContext <| MethodEvent(preMethodEventDetails) <| state
        let postInspectors = runPreInspectors preInspectionContext
        
        let result = Reflection.invokeMethod instance methodInfo parameters
        
        let postMethodEventDetails = createPostMethodEventDetails preMethodEventDetails result
        let postInspectionContext = createPostInspectionContext <| MethodEvent(postMethodEventDetails) <| state
        runPostInspectors postInspectors postInspectionContext
        
        result